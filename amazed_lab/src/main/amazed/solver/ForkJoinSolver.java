package amazed.solver;

import amazed.maze.Maze;
import amazed.maze.Player;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ForkJoinPool;

import javax.swing.plaf.ColorUIResource;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    private int player;
    
    private static boolean found = false;
    private ConcurrentSkipListSet<Integer> safeVisit;
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        this.safeVisit = new ConcurrentSkipListSet<>();
        //create a player in the start tile and push the start tile to the
        this.player = maze.newPlayer(start);
        frontier.push(start);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */



 
    public ForkJoinSolver(Maze maze, int forkAfter){
        this(maze);
        this.forkAfter = forkAfter;

    }


    //private constructor for initialize when forking
    private ForkJoinSolver(Maze maze, int forkAfter,int tile,ConcurrentSkipListSet<Integer> safeVisit) {
        super(maze);
        this.forkAfter = forkAfter;
        this.start = tile;
        this.safeVisit = safeVisit;
        this.player = maze.newPlayer(start);
        frontier.push(start);
        
    }



    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }
    
  

    private List<Integer> parallelSearch()
    {   
        //We will do sequential search n steps, where n = forkAfter

        for(int i = 0; i<forkAfter; i++){
            if(frontier.empty() || found) continue;
            int current = frontier.pop();
            if (!safeVisit.add(current)) { //if it's already visited continue
                continue;
            }

            maze.move(player, current);
    
        	// if current node has a goal
            if (maze.hasGoal(current)) {
                // search finished: reconstruct and return path
                found = true;
                joinSolvers();
                return pathFromTo(start, current);
            
            }
            // if current node has not been visited yet
            for (int nb: maze.neighbors(current)) {       
                if (!safeVisit.contains(nb)){
                    frontier.push(nb);
                    predecessor.put(nb, current);
                } 
            }
        }
    
      
        /*If the goal isn't found in those first tiles we will start adding forks at appropriate 
        * times. If ForkAfter<1 or not set we will start here directly
        */
        //we create this variable beacause the process that doesn't fork has to continue with the forkAfter procedure
        
        int steps = 0;
        while (!frontier.empty() && !found) {
            
            int current = frontier.pop();
            	//grab a starting tile for your search
        	
            if (!safeVisit.add(current)) { //if it's already visited continue
                continue;
            }

            maze.move(player, current);
        	

        	//Check if the current tile has a goal
        	if (maze.hasGoal(current)) {
                found = true;
                // search finished: reconstruct and return path
                joinSolvers();
                return pathFromTo(start, current);
        	}

            ArrayList<Integer> unvisitedNeighbors = new ArrayList<>();

            //add every unvisited neighbor to unvisitedNeighbors
            for(Integer nb: maze.neighbors(current)){
                if(!safeVisit.contains(nb)) unvisitedNeighbors.add(nb);
            }

            
            for (int i = 0; i < unvisitedNeighbors.size(); i++) {
                int nb = unvisitedNeighbors.get(i);
                predecessor.put(nb, current);
                //we won't fork if we only have 1 neighbour or we still have steps of the forkAfter avaliable
                if (i == 0 || steps>0) {
                    
                    frontier.push(nb);
                    steps--;
                } 
                //if we have more than one neighbor and steps == 0 
                //we will fork and restore the steps to the forkAfter value
                else {
                    steps = this.forkAfter;
                    //we create a new solver object, add it to the pool and fork
                    ForkJoinSolver solver = new ForkJoinSolver(maze, this.forkAfter, nb, safeVisit);
                    solversPool.add(solver); //add the new process to the pool;
                    solver.fork();
                }
            } 
        }

        //join solvers and recreate path
        List<Integer> pathToGoal = joinSolvers();

        //if the path to goal is null return null
        if (pathToGoal == null) return null;
        int mid = pathToGoal.remove(0);
        List<Integer> pathFromStart = pathFromTo(start, mid);
        pathFromStart.addAll(pathToGoal);
        return pathFromStart;
    }
    
    private List<ForkJoinSolver> solversPool = new ArrayList<>();


    private List<Integer> joinSolvers() {
        List<Integer> result = null;
        //for every solver of the pool, we will join and return the result
        for (ForkJoinSolver solver : solversPool) {
            if(solver.join() == null) continue;
            result = solver.join();
            
        }
        return result;
    }
    
}

package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;

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
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
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
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
        this.safeVisit = new ConcurrentSkipListSet<>();
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
    
    private ConcurrentSkipListSet safeVisit;


    private List<Integer> parallelSearch()
    {
   
        int current = frontier.pop();
        int player = maze.newPlayer(current);
        

        if(maze.hasGoal(current)){
            //do something
            join();
            //join/terminate all threads
        }
        if(maze.neighbors(current)==null){
            //do something
            join();
        }
        else if(maze.neighbors(current).equals(1)){
            //Player move
        }
        else{
            fork();
        }
     
    //create pool

	//Go sequential for forkafter nr of steps

	//Fork after forkafter nr of steps, if more than 1 neighbour, create new players for each thread, call parallellsearch recursivly?

	//join when no more neighbors?

	

	//if you find goal, return path, 

	

	//if no goal can be found, return null
        return null;
    }


    //private List<Integer> parallelSearch(int nextNode)
}

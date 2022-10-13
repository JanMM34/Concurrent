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
import java.util.concurrent.ForkJoinPool;

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
        this.safeVisit = new ConcurrentSkipListSet<>();	//Threadsafe storage for visited tiles
        frontier.push(start);	//save your starting tile in the frontier stack. Java stacks are threadsafe
        ForkJoinPool pool = ForkJoinPool.commonPool(); //Create pool
    
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
        /*If forkAfter is > 0 then check the first forkAfter numbers of tiles sequentially. */
        try {
            int current = frontier.pop();	//grab a starting tile for your search
            int player = maze.newPlayer(current);	//start a new player at your starting tile
        	for (int i = forkAfter ; i>0 ; i++) {  
        		
            	
            	// if current node has a goal
                if (maze.hasGoal(current)) {
                    // move player to goal
                    maze.move(player, current);
                    // search finished: reconstruct and return path
                    return pathFromTo(start, current);
                }
                
                // if current node has not been visited yet
                if (!safeVisit.contains(current)) {
                    // move player to current node
                    
                    // mark node as visited
                    safeVisit.add(current);
                    //Check if current have more neighbors and if so add them to frontier
                    if (!maze.neighbors(current).isEmpty()) {
                    	// for every node nb adjacent to current
                        for (int nb: maze.neighbors(current)) {
                            // add nb to the nodes to be processed
                            frontier.push(nb);
                            // if nb has not been already visited,
                            // nb can be reached from current (i.e., current is nb's predecessor)
                            if (!safeVisit.contains(nb))
                                predecessor.put(nb, current);
                        }
                    }
                    //If there are no more neighbors, check if frontier is empty
                    else if (frontier.isEmpty()) {
                    	return null;
                    } 
                }
                current = frontier.pop();	//grab a starting tile for your search
                maze.move(player, current);
            }
        	//"kill" player here? Or leave player standing? Reuse?
        }
        catch (Exception e){
        	System.out.println("Something went wrong with the first sequental steps.");
        }
        
       
     /*If the goal isn't found in those first tiles we will start adding forks at appropriate 
         * times. If ForkAfter<1 or not set we will start here directly
        while (!frontier.empty()) {
        	int current = frontier.pop();	//grab a starting tile for your search
        	int player = maze.newPlayer(current);	//start a new player at your starting tile
        	
        	//Check if the current tile has a goal
        	if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                return pathFromTo(start, current);
                
              //join/terminate all threads?
        	}
        	
        	/*If the current tile only have 1 neighbor, it's predecessor, we're
        	 * at a dead-end
        	if(maze.neighbors(current)==1 ){ 
	            //do something add current thread path 
	            join();
	        }
        	

            /*if the current tile have 2 neighbors, including it's predecessor, 
             * check if that neighbor is not visited. If not 
            visited move forward and add tile as visited. If not, return null. 
            else if(maze.neighbors(current).equals(2)){
            	if() {	 
            		//neighbor visited, we can go no further here
            	}
            		//Player move
            		//add neighbor to visited
            	}
            	// The only other neighbor is already visited
            	else { 
            		return null; //or just join?
            	}
     
        			
        }
        //If the current tile have more than 2 neighbors, create a new instance of ForkJoinSolver and fork
        else{
        	pool.invoke(new ForkJoinSolver(this.maze, this.forkAfter);
        }
     
   

*/

	

	//if no goal can be found, return null
        return null;
    }


    //private List<Integer> parallelSearch(int nextNode)
}

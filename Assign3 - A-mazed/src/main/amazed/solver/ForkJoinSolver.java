package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Arrays;
import java.util.AbstractMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Stack;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.BlockingDeque;
import java.util.LinkedHashSet;


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
    // Thread Safe version of HashSet
    protected ConcurrentSkipListSet<Integer> visitedd = new ConcurrentSkipListSet<Integer>();

    // Thread Safe versin of HashMap
    protected ConcurrentHashMap<Integer, Integer> predecessorz = new  ConcurrentHashMap<>();

    // Starting position for the new threads
    protected int forkStart = start;

    // Fork Frontier
    protected BlockingDeque<Integer> frontier = new LinkedBlockingDeque<Integer>();



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
     * @param start       Starting point for fork
     */

    public ForkJoinSolver(Maze maze, int forkAfter, int start, ConcurrentSkipListSet<Integer> vis)
    {
        super(maze);
        this.forkAfter = forkAfter;
        this.forkStart = start;
		this.visitedd = vis;
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
        return parallelDepthFirstSearch();
    }

  private List<Integer> parallelDepthFirstSearch() {
      // Array that hold the threads, so we can join
      ArrayList<ForkJoinSolver> forkArray = new  ArrayList<ForkJoinSolver>();
      // List that will hold the result
      List<Integer> result = null;
      // id of the player
      int player;
      // Flag for root thread
      boolean root = false;
      
      
      // Checking if its a fork or the initial process
      if (forkStart != start) {
        player = maze.newPlayer(forkStart);
        System.out.println("New fork at "+ forkStart);
        frontier.push(forkStart);
      }
      else {
        System.out.println("New player at "+ start);
        
        player = maze.newPlayer(start);
        frontier.push(start);
        root = true;
      }

      while (frontier.size() > 0) {
          // get the new node to process
          int current = frontier.removeFirst();
          //System.out.println("POP!" +" from " +forkStart );
          if (maze.hasGoal(current)) {
              // move player to goal
              maze.move(player, current);
              System.out.println("FOUND IT!! @ " + current + "from "+forkStart ); 
              // search finished: reconstruct and return path 
              return pathFromTo(forkStart, current);
          }
          // if current node has not been visited yet
          if (!visitedd.contains(current)) {
              // move player to current node
              maze.move(player, current);
              // mark node as visited
              visitedd.add(current);
              //Get current neighbors
              Set<Integer> neigh =  maze.neighbors(current);
              // Remove visited nodes from neighbors, so we dont spawn more forks that we need
              boolean a = neigh.removeAll(visitedd);
              Integer[] neighbors =  neigh.toArray(new Integer[0]);
              
              if (neighbors.length > 1) {
                  int size = forkArray.size();
                  for (int i = 0; i < neighbors.length-1; i++) {
                    // Create a new object for every neighbor, keep 1 for the current thread
                    forkArray.add(new ForkJoinSolver(maze, forkAfter, neighbors[i], visitedd));
                    predecessorz.put(neighbors[i], current);
                    forkArray.get(size+i).fork();
                  }
                  //System.out.println("neighbors are  " + Arrays.toString(neighbors) +" from " +forkStart);
                  frontier.addFirst(neighbors[neighbors.length-1]);
                  predecessorz.put(neighbors[neighbors.length-1], current);
              }
              else {
                  // If we have 1 neighbor then no need to spawn a new thread, we continue
                  if (neighbors.length > 0 && !visitedd.contains(neighbors[0])) {
                    frontier.addFirst(neighbors[0]);
                    predecessorz.put(neighbors[0], current);
                 }
              }
          }
      }
      // Joining Threads before returning value, we expect to find 1 goal
      for (int i = 0; i < forkArray.size(); i++) {
          List<Integer> temp = forkArray.get(i).join();
          if (temp != null){
            result = pathFromTo(forkStart, forkArray.get(i).forkStart);
			if (result != null) {
			result.addAll(temp);
			result = new LinkedList<Integer>(new LinkedHashSet<Integer>(result));
			System.out.println("Result is" +result);
			}
			return result;
          }
      }
        // If root process, then you calculate the path from start to root, after joining all threads.
        /*if (root){
            Integer[] sollutions =  result.toArray(new Integer[0]);
            //System.out.println("Goal is at: " + Arrays.toString(sollutions) + "from start" + start);
            System.out.println("result is" + result);
            return pathFromTo(start, sollutions[0]);
            //return result;
        }*/
        return result;
}

   /**
     * Returns the connected path, as a list of node identifiers, that
     * goes from node <code>from</code> to node <code>to</code>
     * following the inverse of relation <code>predecessor</code>. If
     * such a path cannot be reconstructed from
     * <code>predecessor</code>, the method returns <code>null</code>.
     *
     * @param from   the identifier of the initial node on the path
     * @param to     the identifier of the final node on the path
     * @return       the list of node identifiers from <code>from</code> to
     *               <code>to</code> if such a path can be reconstructed from
     *               <code>predecessor</code>; <code>null</code> otherwise
     */
    protected List<Integer> pathFromTo(int from, int to) {
        List<Integer> path = new LinkedList<>();
        Integer curr = to;
        while (curr != from) {
            path.add(curr);
            curr = predecessorz.get(curr);
            if (curr == null) { /// Not having brackets in this if created a really wierd bug
                System.out.println("OH no NULL");
                return null;
            }
        }
        path.add(from);
        Collections.reverse(path);
		System.out.println("current is "+ to +"Path is: " + path);
        return path;
    }
}

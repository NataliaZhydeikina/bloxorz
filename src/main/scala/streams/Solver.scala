package streams

import common._

/**
  * This component implements the solver for the Bloxorz game
  */
trait Solver extends GameDef {

  /**
    * Returns `true` if the block `b` is at the final position
    */
  def done(b: Block): Boolean = (b.isStanding) && (b.b1==goal)

  /**
    * This function takes two arguments: the current block `b` and
    * a list of moves `history` that was required to reach the
    * position of `b`.
    *
    * The `head` element of the `history` list is the latest move
    * that was executed, i.e. the last move that was performed for
    * the block to end up at position `b`.
    *
    * The function returns a stream of pairs: the first element of
    * the each pair is a neighboring block, and the second element
    * is the augmented history of moves required to reach this block.
    *
    * It should only return valid neighbors, i.e. block positions
    * that are inside the terrain.
    */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {


    /**
      *
      * assuming we have a neighbor block nBlock
      * nBlock=b.legalNeighbors(0)._1 //first neighbor, block
      * nMove=b. legalNeighbors(0)._2 //first neighbor move
      * this result:
      * the history for getting to this neighbor block is
      * nHistory = nMove::cHistory
      *
      * return (nBlock, nHistory)
      * stream => return (nBlock, nHistory)#::(cBlock, cHistory)
      *
      * so, current block's neighbor = neighbor + neighbor's neighbor

      */

    def getHistory(x: (Block, Move)): (Block, List[Move]) = {
      return (x._1, x._2::history)
    }

    b.legalNeighbors.toStream.map(getHistory)

  }

  /**
    * This function returns the list of neighbors without the block
    * positions that have already been explored. We will use it to
    * make sure that we don't explore circular paths.
    */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {

    neighbors.filter((x: (Block, List[Move]))=> !explored.contains(x._1))
  }

  /**
    * The function `from` returns the stream of all possible paths
    * that can be followed, starting at the `head` of the `initial`
    * stream.
    *
    * The blocks in the stream `initial` are sorted by ascending path
    * length: the block positions with the shortest paths (length of
    * move list) are at the head of the stream.
    *
    * The parameter `explored` is a set of block positions that have
    * been visited before, on the path to any of the blocks in the
    * stream `initial`. When search reaches a block that has already
    * been explored before, that position should not be included a
    * second time to avoid cycles.
    *
    * The resulting stream should be sorted by ascending path length,
    * i.e. the block positions that can be reached with the fewest
    * amount of moves should appear first in the stream.
    *
    * Note: the solution should not look at or compare the lengths
    * of different paths - the implementation should naturally
    * construct the correctly sorted stream.
    */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {

    //current set = initial
    //find next set
    // recurvive:  current#::from(next)

    var newExplored = explored
    if (!initial.isEmpty){
      //not functional, lmited number
      val initialBlock = initial.unzip._1
      for (x <- initialBlock) {
        newExplored += x
      }
    }


    def getNewNeighbors (x: (Block, List[Move])): Stream[(Block, List[Move])] = {
      newNeighborsOnly(neighborsWithHistory(x._1, x._2), newExplored)
    }

    //generating the next set

    if (initial.isEmpty) {return Stream.empty} else {

      lazy val next = for {
        blk <- initial
        nbr <- getNewNeighbors(blk)
      } yield nbr

      return initial #::: (from(next, newExplored))
    }

  }




  /**
    * The stream of all paths that begin at the starting block.
    */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {


    val initial = Stream((startBlock, List[Move]()))
    val explored = Set[Block]()

    from(initial, explored)

  }

  /**
    * Returns a stream of all possible pairs of the goal block along
    * with the history how it was reached.
    */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {

    def f(x:(Block, List[Move])): Boolean = {done(x._1)}
    pathsFromStart.filter(f)

  }

  /**
    * The (or one of the) shortest sequence(s) of moves to reach the
    * goal. If the goal cannot be reached, the empty list is returned.
    *
    * Note: the `head` element of the returned list should represent
    * the first move that the player should perform from the starting
    * position.
    */
  lazy val solution: List[Move] = {

    val paths = pathsToGoal

    if(paths.isEmpty) {List.empty} else {

      paths(0)._2.reverse
    }

  }
}

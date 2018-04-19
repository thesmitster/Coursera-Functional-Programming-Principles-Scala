package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.getLeftBlock
          case Right => block.getRightBlock
          case Up => block.getUpBlock
          case Down => block.getDownBlock
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level: String =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait DevChecker extends Solver with InfiniteTerrain {
    val startPos = Position(0, 0)
    val goal = Position(1, 1)

  }

  test("testing infinite level") {
    new DevChecker {
      // check `done` functionality
      assert(!done(startBlock))
      val goalBlock = Block(goal, goal)
      assert(done(goalBlock))

      // check we can compare moves
      assert(Left == Left)
      assert(Right == Right)
      assert(Up == Up)
      assert(Down == Down)

      // check we can compare list of moves
      assert(List(Left) == List(Left))

      // check we can compare positions
      assert(Position(0, 1) == Position(0, 1))
      assert(List(Position(0, 1)) == List(Position(0, 1)))

      // check we can compare blocks
      val b1 = Block(Position(0, 0), Position(0, 0))
      val b2 = Block(Position(0, 0), Position(0, 0))
      assert(b1 == b2)

      // check we can compare blockstates
      val bs1 = BlockState(b1, List(Left))
      val bs2 = BlockState(b2, List(Left))
      assert(bs1 == bs2)

      // checking functionality of `neighborsWithHistory`
      val startState = BlockState(startBlock, Nil)
      val ns: Set[BlockState] = neighborsWithHistory(startState).toSet
      val nsCheckResults = Set(
        BlockState(Block(Position(0, -2), Position(0, -1)), List(Left)),
        BlockState(Block(Position(0, 1),Position(0, 2)), List(Right)),
        BlockState(Block(Position(1, 0),Position(2, 0)), List(Down)),
        BlockState(Block(Position(-2, 0),Position(-1, 0)), List(Up))
      )
      assert(nsCheckResults == ns)

      // see below for newNeighborsOnly checking

      // check functionality of `from`
    }
  }

  test("testing basic functionality") {
    new Level1 {
      // continuing from the above DevChecker

      // check functionality of newNeighborsOnly
      val startState = BlockState(startBlock, Nil)
      val neighbors: Stream[BlockState] = neighborsWithHistory(startState)
      val newNeighbors: Stream[BlockState] = newNeighborsOnly(neighbors, Set())
      assert(neighbors.toSet == newNeighbors.toSet)

      // check functionality of newNeighbors when there is an explored block already
      val exploredBlocks = Set(Block(Position(1, 2), Position(1, 3)))
      val newNeighbors2: Stream[BlockState] = newNeighborsOnly(neighbors, exploredBlocks)
      val expectedNewNeighbors2 = Set(BlockState(Block(Position(2, 1),Position(3, 1)), List(Down)))
      assert(expectedNewNeighbors2 == newNeighbors2.toSet)

    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(isInsideMap(Position(0, 0)), "0,0")
      assert(isInsideMap(Position(1, 1)), "1,1") // start
      assert(isInsideMap(Position(4, 7)), "4,7") // goal
      assert(isInsideMap(Position(5, 8)), "5,8")
      assert(!isInsideMap(Position(5, 9)), "5,9")
      assert(isInsideMap(Position(4, 9)), "4,9")
      assert(!isInsideMap(Position(6, 8)), "6,8")
      assert(!isInsideMap(Position(4, 11)), "4,11")
      assert(!isInsideMap(Position(-1, 0)), "-1,0")
      assert(!isInsideMap(Position(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Position(1, 1))
      assert(goal == Position(4, 7))
    }
  }
/*
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }*/
}

package streams

import common._

/**
 * This trait represents the layout and building blocks of the game
 */
trait GameDef {

  /**
   * The case class `Pos` encodes positions in the terrain.
   *
   * IMPORTANT NOTE
   *  - The `row` coordinate denotes the position on the vertical axis
   *  - The `col` coordinate is used for the horizontal axis
   *  - The coordinates increase when moving down and right
   *
   * Illustration:
   *
   *     0 1 2 3   <- col axis
   *   0 o o o o
   *   1 o o o o
   *   2 o # o o    # is at position Pos(2, 1)
   *   3 o o o o
   *
   *   ^
   *   |
   *
   *   row axis
   */
  case class Position(row: Int, col: Int) {
    /** The position obtained by changing the `row` coordinate by `d` */
    def deltaRow(d: Int): Position = copy(row = row + d)

    /** The position obtained by changing the `col` coordinate by `d` */
    def deltaCol(d: Int): Position = copy(col = col + d)
  }

  /**
   * The position where the block is located initially.
   *
   * This value is left abstract, it will be defined in concrete
   * instances of the game.
   */
  val startPos: Position

  /**
   * The target position where the block has to go.
   * This value is left abstract.
   */
  val goal: Position

  /**
   * The terrain is represented as a function from positions to
   * booleans. The function returns `true` for every position that
   * is inside the terrain.
   *
   * As explained in the documentation of class `Pos`, the `row` axis
   * is the vertical one and increases from top to bottom.
   */
  type Terrain = Position => Boolean


  /**
   * The terrain of this game. This value is left abstract.
   */
  val isInsideMap: Terrain


  /**
   * In Bloxorz, we can move left, right, Up or down.
   * These moves are encoded as case objects.
   */
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  case class BlockMove(block: Block, move: Move)

  /**
   * This function returns the block at the start position of
   * the game.
   */
  def startBlock: Block = Block(startPos, startPos)


  /**
   * A block is represented by the position of the two cubes that
   * it consists of. We make sure that `cube1` is lexicographically
   * smaller than `cube2`.
   */
  case class Block(cube1: Position, cube2: Position) {

    // checks the requirement mentioned above
    require(cube1.row <= cube2.row && cube1.col <= cube2.col, "Invalid block position: cube1=" + cube1 + ", cube2=" + cube2)

    /**
     * Returns a block where the `row` coordinates of `cube1` and `cube2` are
     * changed by `d1` and `d2`, respectively.
     */
    private def deltaRow(delta1: Int, delta2: Int) = Block(cube1.deltaRow(delta1), cube2.deltaRow(delta2))

    /**
     * Returns a block where the `col` coordinates of `cube1` and `cube2` are
     * changed by `d1` and `d2`, respectively.
     */
    private def deltaCol(delta1: Int, delta2: Int) = Block(cube1.deltaCol(delta1), cube2.deltaCol(delta2))

    /** The block obtained by moving left */
    def getLeftBlock: Block =
      if (isStanding) deltaCol(-2, -1)
      else if (cube1.row == cube2.row) deltaCol(-1, -2)
      else deltaCol(-1, -1)

    /** The block obtained by moving right */
    def getRightBlock: Block =
      if (isStanding) deltaCol(1, 2)
      else if (cube1.row == cube2.row) deltaCol(2, 1)
      else deltaCol(1, 1)

    /** The block obtained by moving up */
    def getUpBlock: Block =
      if (isStanding) deltaRow(-2, -1)
      else if (cube1.row == cube2.row) deltaRow(-1, -1)
      else deltaRow(-1, -2)

    /** The block obtained by moving down */
    def getDownBlock: Block =
      if (isStanding) deltaRow(1, 2)
      else if (cube1.row == cube2.row) deltaRow(1, 1)
      else deltaRow(2, 1)

    /**
     * Returns the list of blocks that can be obtained by moving
     * the current block, together with the corresponding move.
     */
    def neighbors: List[BlockMove] = List(
      BlockMove(this.getLeftBlock, Left),
      BlockMove(this.getRightBlock, Right),
      BlockMove(this.getDownBlock, Down),
      BlockMove(this.getUpBlock, Up)
    )

    /**
     * Returns the list of positions reachable from the current block
     * which are inside the terrain.
     */
    def legalNeighbors: List[BlockMove] =
      for {
        neighbor ← neighbors
        if neighbor.block.isLegal
      } yield neighbor

    /**
     * Returns `true` if the block is standing.
     * This is true if the two cubes of the block occupy the same position.
     */
    def isStanding: Boolean = this.cube1 == this.cube2

    /**
     * Returns `true` if the block is entirely inside the terrain.
     */
    def isLegal: Boolean = isInsideMap(cube1) && isInsideMap(cube2)
  }
}

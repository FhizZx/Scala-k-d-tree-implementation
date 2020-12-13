import scala.collection.mutable.{ArrayBuffer, HashMap}

/** Space partitioning data structure designed for efficient nearest neighbour searches in a space of arbitrary dimension*/
class KDTree private(val dimension: Int) {
  private var root: KDTree.Tree = null

  /** Returns the closest point to coords*/
  def nearestNeighbour(coords: KDTree.Point): KDTree.Point = {
    var bestNode: KDTree.Tree = null
    var bestDistance = Float.MaxValue
    val stack = new scala.collection.mutable.Stack[KDTree.Tree]
    stack.push(root)
    while (!stack.isEmpty) {
      val currNode = stack.pop()
      val currDistance = currNode.point sqDistance coords
      val axis = currNode.axis
      if (currDistance < bestDistance) { //Check if current node is closer to coords than bestNode
        bestNode = currNode; bestDistance = currDistance
      }
      /** Based on the position of coords in relation to the hyperplane parallel to axis that goes through currNode, we know we should explore the points that are in the same semi-space, but only explore those on the other side of the hyperplane if the hypersphere centered at coords and of radius bestDistance intersects said hyperplane. (i.e. if the hyperplane is too far away from the coords, it is guaranteed that we won't find a closer neighbour on the other side of the hyperplane)*/
      if (coords(axis) < currNode.point(axis)) {
        if (currNode.left != null)
          stack.push(currNode.left)
        if (currNode.right != null && bestDistance > currNode.point(axis) - coords(axis))
          stack.push(currNode.right)
      }
      else {
        if (currNode.right != null)
          stack.push(currNode.right)
        if (currNode.left != null && bestDistance > coords(axis) - currNode.point(axis))
          stack.push(currNode.left)
      }
    }
    bestNode.point
  }

  def draw = root.recPrint(0)
  override def toString = root.toString

}
object KDTree {
  private class Tree(val point: Point, val axis: Int, val left: Tree, val right: Tree) {
    override def toString : String = {

      var s = ""
      if (left != null)
        s += left.toString + " "
      s += point.toString
      if (right != null)
        s += " " + right.toString
      s
    }
    def recPrint(depth: Int): Unit = {
      var w = point
      println(" . " * depth + w)
      if (this.left != null)
        this.left.recPrint(depth + 1)
      if (this.right != null)
        this.right.recPrint(depth + 1)
    }
  }

  class Point(val key: String, val dimension: Int, private val data: Array[Float])  {
      require(data.size == dimension)
      def apply(axis: Int) = data(axis)
      def sqDistance(other: Point) = {
        require(this.dimension == other.dimension)
        var sum = 0: Float
        for (i <- 0 to dimension - 1)
          sum += (this(i) - other(i)) * (this(i) -  other(i))
        sum
      }
      override def toString = key + " " + data.mkString(", ")
  }

  def apply(dimension: Int, elements: Array[Point]) = {
    val res = new KDTree(dimension)
    res.root = buildTree(elements, 0, dimension)
    res
  }

  /** Construction can be optimized but this implementation is good enough for its purposes*/
  private def buildTree(elements: Array[Point], axis: Int, dimension: Int): Tree = {
    if (elements.size == 0)
      return null
    
    val sorted = elements.sortBy(a => a(axis))
    val m = sorted(sorted.size / 2) //median of points according to the current axis
                                 
    return new Tree(m, //point at root
                    (1 + axis) % dimension, //axis of splitting
                    buildTree(sorted.take(m),      (1 + axis) % dimension, dimension), //left branch
                    buildTree(sorted.drop(m + 1),  (1 + axis) % dimension, dimension) //right branch
                   )
  }
}

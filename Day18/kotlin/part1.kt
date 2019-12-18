import java.io.File
import java.util.*

class Point(val x : Int, val y : Int) {
    fun neighbours(): ArrayList<Point> =
        arrayListOf(
            Point(x + 1, y),
            Point(x - 1, y),
            Point(x, y + 1),
            Point(x, y - 1))
    override fun toString(): String = "Point($x, $y)"
    override fun hashCode(): Int = x + 1000
    override fun equals(other: Any?): Boolean = when (other) {
        is Point -> this.x == other.x && this.y == other.y
        else -> false
    }
}
class Search(val pos: Point, val keys: HashSet<Char>, val cost: Int) {
    override fun toString(): String = "Search({${pos.x}, ${pos.y}}, $keys, $cost)"
}
fun main() {
    val input = File("input18.txt").readLines();
    val map = HashMap<Point, Char>()
    var player = Point(40, 40)

    input.forEachIndexed { y, line -> line.forEachIndexed{ x, c ->
        if(c == '@'){map[Point(x, y)] = '.'; player = Point(x, y);}
        else map[Point(x, y)] = c
    }}
    val queue = ArrayDeque<Search>()
    queue.add(Search(player, HashSet(),0))
    val seen = HashSet<Pair<Point, HashSet<Char>>>()
    var progress = 0;
    while (queue.count() > 0){
        val op = queue.pop()
        if (op.cost > progress) { progress = op.cost; println(op) }
        if (op.keys.size == 26) { println(op); break; }
        for (n in op.pos.neighbours()) {
            if(seen.contains(Pair(n,op.keys)))
                continue;
            seen.add(Pair(n,op.keys))
            val c = map[n] ?: '#'
            if (c == '.')
                queue.addLast(Search(n,op.keys,op.cost+1))
            else if (c.isLowerCase()) {
                queue.addLast(Search(n,op.keys.plus(c).toHashSet(),op.cost+1))
            } else if (c.isUpperCase() && op.keys.contains(c.toLowerCase()))
                queue.addLast(Search(n,op.keys,op.cost+1))
        }
    }
}


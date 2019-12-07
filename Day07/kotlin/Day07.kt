import java.io.File
import java.util.stream.Stream
import kotlin.math.max
import kotlin.streams.toList

sealed class IntCode {
    abstract val steps: Int

    object Halt : IntCode() {
        override val steps = 1
        override fun toString() = "Halt"
    }

    data class Add(val a: Loc, val b: Loc, val out: Loc.Ref) : IntCode() {
        override val steps = 4
        override fun toString() = "Add($a, $b, $out)"
    }

    data class Mul(val a: Loc, val b: Loc, val out: Loc.Ref) : IntCode() {
        override val steps = 4
        override fun toString() = "Mul($a, $b, $out)"
    }

    data class Input(val out: Loc.Ref) : IntCode() {
        override val steps = 2
        override fun toString() = "Input($out)"
    }

    data class Output(val out: Loc) : IntCode() {
        override val steps = 2
        override fun toString() = "Output($out)"
    }

    data class JumpIfTrue(val bool: Loc, val jmpLoc: Loc) : IntCode() {
        override val steps = 3
        override fun toString() = "JumpIfTrue($bool, $jmpLoc)"
    }

    data class JumpIfFalse(val bool: Loc, val jmpLoc: Loc) : IntCode() {
        override val steps = 3
        override fun toString() = "JumpIfFalse($bool, $jmpLoc)"
    }

    data class Less(val a: Loc, val b: Loc, val out: Loc.Ref) : IntCode() {
        override val steps = 4
        override fun toString() = "Less($a, $b, $out)"
    }

    data class Equals(val a: Loc, val b: Loc, val out: Loc.Ref) : IntCode() {
        override val steps = 4
        override fun toString() = "Equals($a, $b, $out)"
    }
}

sealed class Loc {
    data class Im(val value: Int) : Loc()
    data class Ref(val ref: Int) : Loc()

    fun get(memory: IntArray) = when (this) {
        is Im -> this.value
        is Ref -> memory[this.ref]
    }
}

fun Int.digits(take: Long) =
    Stream.iterate(this) { it.div(10) }
        .limit(take)
        .toList()
        .reversed()
        .map { it.rem(10) }

fun getCode(ip: Int, memory: IntArray): IntCode {
    val raw = memory[ip]
    val (c,b,a,d,e) = raw.digits(5)
    fun lda(im: Int, offset: Int) = when (im) {
        0 -> Loc.Ref(memory[ip + offset])
        1 -> Loc.Im(memory[ip + offset])
        else -> error("???")
    }
    val inst = d * 10 + e
    return when (inst) {
        1 -> IntCode.Add(lda(a, 1), lda(b, 2), Loc.Ref(memory[ip + 3]))
        2 -> IntCode.Mul(lda(a, 1), lda(b, 2), Loc.Ref(memory[ip + 3]))
        3 -> IntCode.Input(Loc.Ref(memory[ip + 1]))
        4 -> IntCode.Output(lda(a, 1))
        5 -> IntCode.JumpIfTrue(lda(a, 1), lda(b, 2))
        6 -> IntCode.JumpIfFalse(lda(a, 1), lda(b, 2))
        7 -> IntCode.Less(lda(a, 1), lda(b, 2), Loc.Ref(memory[ip + 3]))
        8 -> IntCode.Equals(lda(a, 1), lda(b, 2), Loc.Ref(memory[ip + 3]))
        99 -> IntCode.Halt
        else -> error("Unknown opcode $inst")
    }
}

enum class RunningState {
    Suspended,
    Halted
}

class Machine(var input: ArrayList<Int>, var memory: IntArray, var output: ArrayList<Int>) {
    var ip = 0
    var inputPtr = 0
    fun stepAwait(): RunningState {
        loop@ while (true) {
            val opcode = getCode(ip, this.memory)
            when (opcode) {
                is IntCode.Add -> memory[opcode.out.ref] = opcode.a.get(memory) + opcode.b.get(memory)
                is IntCode.Mul -> memory[opcode.out.ref] = opcode.a.get(memory) * opcode.b.get(memory)
                is IntCode.Input -> if (input.size <= inputPtr) {
                    return RunningState.Suspended
                } else {
                    memory[opcode.out.ref] = input[inputPtr++]
                }
                is IntCode.Output -> {
                    output.add(opcode.out.get(memory))
                }
                is IntCode.JumpIfTrue -> if (opcode.bool.get(memory) != 0) ip = opcode.jmpLoc.get(memory) - opcode.steps else Unit
                is IntCode.JumpIfFalse -> if (opcode.bool.get(memory) == 0) ip = opcode.jmpLoc.get(memory) - opcode.steps else Unit
                is IntCode.Less -> memory[opcode.out.ref] = if (opcode.a.get(memory) < opcode.b.get(memory)) 1 else 0
                is IntCode.Equals -> memory[opcode.out.ref] = if (opcode.a.get(memory) == opcode.b.get(memory)) 1 else 0
                IntCode.Halt -> return RunningState.Halted
            }
            ip += opcode.steps
        }
    }
}

fun permute(input: List<Int>): List<List<Int>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<Int>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun chain(setting: List<Int>, memory: IntArray): Int {
    var bufs: Array<ArrayList<Int>> = arrayOf(
        arrayListOf(setting[0], 0), arrayListOf(setting[1]), arrayListOf(setting[2]), arrayListOf(setting[3]), arrayListOf(setting[4]))

    var machines: Array<Machine> = arrayOf(
        Machine(bufs[0], memory.clone(), bufs[1]),
        Machine(bufs[1], memory.clone(), bufs[2]),
        Machine(bufs[2], memory.clone(), bufs[3]),
        Machine(bufs[3], memory.clone(), bufs[4]),
        Machine(bufs[4], memory.clone(), bufs[0])
    )

    var anySuspended = true
    while (anySuspended) {
        anySuspended = false
        for (machine in machines) {
            val runningState = machine.stepAwait()
            if (runningState.equals(RunningState.Suspended))
                anySuspended = true
        }
    }
    return machines[4].output.last()
}

fun main() {
    val memory = File("Day07.in").readLines().first().split(",").map { it.toInt() }.toIntArray()

    var best = 0
    permute(listOf(0,1,2,3,4)).map { perm ->
        val out = chain(perm, memory.clone())
        best = max(best, out)
    }
    println("Part 1 $best")
    best = 0
    permute(listOf(5,6,7,8,9)).map { perm ->
        val out = chain(perm, memory.clone())
        best = max(best, out)
    }
    println("Part 2 $best")
}

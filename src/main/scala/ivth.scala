package ivth2

import spire.math._
import spire.syntax._

import scala.collection.mutable

class DataUnderflowError extends Exception
class ReturnUnderflowError extends Exception

final class Stack {
  private val buf = mutable.ArrayBuffer.empty[Int]
  def isEmpty: Boolean = buf.isEmpty
  def length: Int = buf.length
  def push(n: Int): Unit = buf.append(n)
  def get(i: Int): Int =
    if (i < buf.length) buf(i) else throw new DataUnderflowError()
  def top(): Int = buf(buf.length - 1)
  def pop(): Int = {
    if (buf.isEmpty) throw new DataUnderflowError()
    val i = buf.length - 1
    val n = buf(i)
    buf.remove(i)
    n
  }
  override def toString() = buf.mkString("Stack(", ", ", ")")
}

sealed trait Lookup
case class Literal(n: Int) extends Lookup
case class Word(addr: Int) extends Lookup
case object NotFound extends Lookup

object Flags {
  @inline final def FAST = 0x80000000
  @inline final def NONE = 0x00000000
}

import Flags._

final case class Runtime(memlen: Int, words: Array[String]) {

  // 0 is pc (program counter, 0 -> exit)
  // 1 is compiling (zero -> interpreting, non-zero -> compiling)
  // 2 is mfree (starts at 100)
  // 3 is w (index into words array)
  // 4-9 are tmp variables
  val memory: Array[Int] = new Array[Int](memlen)

  def pc: Int = memory(0)
  def pc_=(n: Int): Unit = memory(0) = n
  pc = 0

  def compiling: Boolean = memory(1) != 0
  def compiling_=(b: Boolean): Unit = memory(1) = if (b) 1 else 0
  compiling = false

  def mfree: Int = memory(2)
  def mfree_=(n: Int): Unit = memory(2) = n
  mfree = 40

  def w: Int = memory(3)
  def w_=(n: Int): Unit = memory(3) = n

  // TODO: put stacks in memory? upside is that we can save words by building
  // things like r> and >r rather than building them in?
  // we could also implement .s and .r this way.
  var rs = new Stack
  var ds = new Stack

  def save(n: Int) {
    log("mfree=%s" format mfree)
    memory(mfree) = n
    mfree += 1
  }

  val dict = mutable.Map.empty[String, Int]
  val builtins = new Array[Function1[Runtime, Unit]](100)

  def lookup(s: String): Lookup = dict.get(s) match {
    case Some(addr) =>
      Word(addr)
    case None => if (s.matches("-?[1-9][0-9]*|0"))
      Literal(s.toInt)
    else if (s.matches("0x[0-9A-Fa-f]+"))
      Literal(java.lang.Long.parseLong(s.substring(2), 16).toInt)
    else
      NotFound
  }

  def jump(addr: Int): Unit =
    if (addr < 0) builtins(-addr)(this) else { rs.push(pc); pc = addr }

  def goto(addr: Int): Unit =
    if (addr < 0) builtins(-addr)(this) else pc = addr

  def log(msg: String) = ()
  //def log(msg: String) = println(msg)

  var i = 0
  def run(): Unit = {
    log("start")
    pc = 0
    jump(11)
    while (pc != 0) {
      i += 1
      //if (i >= 8000) sys.error("exhausted")

      //println("pc=%s c=%s mfree=%s w=%s ds=%s rs=%s" format (pc, compiling, mfree, w, ds, rs))
      val addr = memory(pc)
      // println("-1 = %s" format memory(pc-1))
      // println("+0 = %s" format memory(pc))
      // println("+1 = %s" format memory(pc+1))
      log("  jumping to %s" format addr)
      pc += 1
      jump(addr)
      log("  pc=%s ds=%s rs=%s" format (pc, ds, rs))
    }
  }

  def die(msg: String): Unit = sys.error(msg)

  def slow(idx: Int, name: String, f: Function1[Runtime, Unit]) {
    builtins(idx) = f
    dict(name) = -idx
  }

  def fast(idx: Int, name: String, f: Function1[Runtime, Unit]) {
    builtins(idx) = f
    dict(name) = mfree
    save(FAST)
    save(-idx)
    save(-3)
  }

  def byhand(idx: Int, n: Int): Unit = memory(idx) = n
  def byhand(idx: Int, name: String): Unit = lookup(name) match {
    case Word(addr) =>
      memory(idx) = addr
    case Literal(n) =>
      memory(idx) = n
    case NotFound =>
      sys.error("can't find %s" format name)
  }

  // 1-100 are reserved
  def initBuiltins() {
    builtins(1) = { r => r.jump(r.ds.pop()) }
    builtins(2) = { r => r.ds.push(memory(r.pc)); r.pc += 1 }
    builtins(3) = { r => r.pc = r.rs.pop() }

    slow(4, "!", { r =>
      val addr = r.ds.pop()
      val n = r.ds.pop()
      r.memory(addr) = n
    })

    slow(5, "@", { r =>
      val addr = r.ds.pop()
      r.ds.push(r.memory(addr))
    })

    builtins(6) = { r =>
      if (r.w >= r.words.length) {
        r.pc = 0
      } else {
        val word = r.words(r.w)
        r.w += 1
        r.lookup(word) match {
          case Word(addr) =>
            if (addr < 0) {
              if (r.compiling) r.save(addr) else r.jump(addr)
            } else {
              val flags = if (addr < 0) 0 else r.memory(addr)
              if (r.compiling && (flags & FAST) == 0)
                r.save(addr + 1)
              else {
                r.jump(addr + 1)
              }
            }
          case Literal(n) =>
            if (r.compiling) {
              r.save(-2)
              r.save(n)
            } else {
              r.ds.push(n)
            }
          case NotFound =>
            println(dict)
            die("%s was not found" format word)
        }
      }
    }

    slow(9, "0br", { r =>
      log("0br")
      val offset = r.ds.pop()
      val test = r.ds.pop()
      if (test == 0) {
        r.pc += offset
        log("%s == 0 so using offset %s to set pc=%s" format (test, offset, r.pc))
      } else {
        log("%s != 0 so not branching" format test)
      }
    })

    slow(10, "nor", {
      log("nor")
      r => r.ds.push(~(r.ds.pop() | r.ds.pop()))
    })

    slow(11, "+", { r =>
      val x = r.ds.pop()
      val y = r.ds.pop()
      r.ds.push(x + y)
    })

    slow(12, ".c", r => System.out.print(r.ds.pop().toChar))
    slow(13, ".n", r => System.out.print(r.ds.pop()))
    slow(14, ".s", r => System.out.print(r.ds))
    // slow(15, ".r", r => System.out.print(r.rs))

    slow(16, ":", { r =>
      log(":")
      r.compiling = true
      val name = r.words(r.w)
      r.w += 1
      dict(name) = r.mfree
      log("defining %s at %s" format (name, r.mfree))
      r.save(NONE)
      log("saved %s: %s" format (name, dict(name)))
    })

    fast(17, ";", { r =>
      log(";;;;")
      r.save(-3)
      r.compiling = false
    })

    // not clear how to automate without adding more words
    fast(18, "(", { r =>
      while (r.w < r.words.length && r.words(r.w) != ")") r.w += 1
      r.w += 1
    })

    // possible to replace but annoying
    fast(20, "[", { r => r.compiling = false })

    // slow(22, "r>", { r =>
    //   r.ds.push(r.rs.pop())
    // })

    // slow(23, ">r", {
    //   r => r.rs.push(r.ds.pop())
    // })

    // run : loop interpret 0 @ -5 0br ;
    memory(10) = 0 // flags
    byhand(11, -6) // interpret current word
    byhand(12, -2)
    byhand(13, 0)
    byhand(14, "@") // push memory(0)
    byhand(15, -2)
    byhand(16, -5) // push offset 5
    byhand(17, "0br") // if memory(0)==0, branch offset 5
    byhand(18, -2)
    byhand(19, 0) // push 0
    byhand(20, -2)
    byhand(21, -12) // push offset -12
    byhand(22, "0br") // branch offset -12
    byhand(23, -3) // exit function
  }

  initBuiltins()
}

import scala.io.Source
object Runtime {
  final val size = 512 * 1024

  def parseString(s: String): Array[String] =
    s.replace("\n", " ").split(" ").filter(""!=)

  def parse(path: String): Array[String] =
    parseString(Source.fromFile(path).mkString)

  def runfile(path: String): Unit =
    Runtime(size, parse(path)).run()

  def main(args: Array[String]): Unit = {
    val prog = """
: ] 1 1 ! ;
: immed 0x80000000 2 @ -1 + ! ;
: immediate [ immed ] immed ;

: nl 10 .c ;
: sp 32 .c ;

: drop 4 ! ;
: dup 4 ! 4 @ 4 @ ;
: swap 4 ! 5 ! 4 @ 5 @ ;
: over 4 ! 5 ! 4 @ 5 @ 4 @ ;
: rot 4 ! 5 ! 6 ! 5 @ 4 @ 6 @ ;
: -rot 4 ! 5 ! 6 ! 4 @ 6 @ 5 @ ;
: nip 4 ! 5 ! 4 @ ;
: tuck 4 ! 5 ! 5 @ 4 @ 5 @ ;

: 2drop 4 ! 4 ! ;
: 2dup 4 ! 5 ! 5 @ 4 @ 5 @ 4 @ ;

(
1 2 3 rot .s nl drop drop drop
1 2 3 -rot .s nl drop drop drop
1 2 3 2drop .s nl drop
1 2 3 2dup .s nl drop drop drop drop drop
)

: ~ dup nor ;
: | nor ~ ;
: & ~ swap ~ nor ;
: nand & ~ ;
: ^ dup rot dup -rot & -rot nor nor ;

: != ^ 7 0br 1 0 2 0br 0 ;
: == ^ 7 0br 0 0 2 0br 1 ;
: bool 0 != ;
: ! 0 == ;
: ==0 0 == ;
: !=0 0 != ;

: 0< -2147483648 & 0 != ;
: 0>= -2147483648 & 0 == ;
: 0> 2147483647 & 0 != ;
: 0<= 2147483647 & 0 == ;
: cmp0 dup 0>= 18 0br ( if < 0, jump NEG )
  0> 7 0br            ( jump ZER )
  1 0 10 0br          ( result 1, jump END )
  0 0 3 0br           ( ZER: result 0, jump END )
drop -1 ;             ( NEG: return -1 )

: negate ~ 1 + ;
: - negate + ;
: -- 1 - ;
: ++ 1 + ;

: cmp swap - cmp0 ;
: < cmp -1 == ;
: <= cmp 1 != ;
: > cmp 1 == ;
: >= cmp 1 != ;

: *helper -rot -- -rot dup -rot + -rot swap rot ;
: * 0 -rot dup 6 0br rot *helper 0 -12 0br ;

: % 2dup >= 9 0br dup -rot - swap 0 -14 0br drop ;

: /helper rot ++ -rot dup -rot - swap ;
: / 0 -rot 2dup >= 6 0br /helper 0 -11 0br 2drop ;

: /test .s / .s nl drop ;

: # .c ; 10 68 76 82 79 87 32 79 76 76 69 72 # # # # # # # # # # # #

"""

    val parsed = parseString(prog)
    val r = Runtime(size, parsed)
    try {
      r.run()
    } catch {
      case e: Exception =>
        println(e)
        e.printStackTrace
        println("pc=%s ds=%s rs=%s" format (r.pc, r.ds, r.rs))
    }
  }
}

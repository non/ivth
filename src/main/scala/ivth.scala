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
      val ow = dict.toList.filter(_._2 == (addr.abs - 1)).headOption
      if (ow.isDefined) {
        log("  jumping to %s (%s)" format (addr, ow.get._1))
      } else {
        log("  jumping to %s" format addr)
      }
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

    slow(7, "find", { r => 
      val name = r.words(r.w)
      r.w += 1
      val n = r.lookup(name) match {
        case Word(addr) => addr
        case _ => 0
      }
      r.ds.push(n)
    })

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
    slow(15, ".r", r => System.out.print(r.rs))
    slow(25, ".x", r => System.out.print(r.memory.slice(776, 806).toList))
    slow(26, ".see", { r => 
      val name = r.words(r.w)
      r.w += 1
      val n = r.lookup(name) match {
        case Word(addr) => addr
        case _ => sys.error("%s not found" format name)
      }
      var i = n
      while (i < n + 80 && memory(i) != -3) i += 1
      System.out.println(memory.slice(n, i + 1).mkString(name + ": ", " ", ""))
    })

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

    slow(22, "r>", { r =>
      r.ds.push(r.rs.pop())
    })

    slow(23, ">r", {
      r => r.rs.push(r.ds.pop())
    })

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
( machinery to support immediate functions and [] )
: ] 1 1 ! ;
: immed 0x80000000 2 @ -1 + ! ;
: immediate [ immed ] immed ;

( character constants )
: nl 10 .c ;
: sp 32 .c ;
: asc-0 60 ;
: asc-A 101 ;
: asc-a 141 ;

( numeric register constants )
: PC 0 ;
: COMPILING 1 ;
: MFREE 2 ;
: W 3 ;
: T1 4 ;
: T2 5 ;
: T3 6 ;

( boolean constants )
: FALSE 0 ;
: TRUE 1 ;

( basic stack manipulation )
: drop 4 ! ;
: dup 4 ! 4 @ 4 @ ;
: swap 4 ! 5 ! 4 @ 5 @ ;
: over 4 ! 5 ! 5 @ 4 @ 5 @ ;
: rot 4 ! 5 ! 6 ! 5 @ 4 @ 6 @ ;
: -rot 4 ! 5 ! 6 ! 4 @ 6 @ 5 @ ;
: nip 4 ! 5 ! 4 @ ;
: tuck 4 ! 5 ! 5 @ 4 @ 5 @ ;

( top-2 stack manipulation )
: 2drop 4 ! 4 ! ;
: 2dup 4 ! 5 ! 5 @ 4 @ 5 @ 4 @ ;

( find 2dup .n nl
.see 2dup nl )

( get/set the program counter )
: pc@ r> dup >r ;
: pc! 0 ! ;

( get/set the next free memory cell -- useful for building functions )
: m@ 2 @ ;
: m! 2 @ dup -rot ! 1 + 2 ! ;

( copy from return stack )
: r@ r> dup >r ;

( bitwise operators )
: ~ dup nor ;
: | nor ~ ;
: & ~ swap ~ nor ;
: nand & ~ ;
: ^ dup rot dup -rot & -rot nor nor ;

( logical/boolean/equality operators )
: != ^ 7 0br 1 0 2 0br 0 ;
: == ^ 7 0br 0 0 2 0br 1 ;
: bool 0 != ;
: not 0 == ; ( TODO: it'd be nice to use ! but we'd need to break with forth )
: ==0 0 == ;
: !=0 0 != ;

( sign tests )
: 0< -2147483648 & 0 != ;
: 0>= -2147483648 & 0 == ;
: 0> 2147483647 & 0 != ;
: 0<= 2147483647 & 0 == ;
: cmp0 dup 0>= 18 0br ( if < 0, jump NEG )
  0> 7 0br            ( jump ZER )
  1 0 10 0br          ( result 1, jump END )
  0 0 3 0br           ( ZER: result 0, jump END )
drop -1 ;             ( NEG: return -1 )

( addition/subtraction/negation )
: negate ~ 1 + ;
: - negate + ;
: -- 1 - ;
: ++ 1 + ;

( comparisons )
: cmp swap - cmp0 ;
: < cmp -1 == ;
: <= cmp 1 != ;
: > cmp 1 == ;
: >= cmp 1 != ;

( inlines the top value of the stack into the current function )
: inline -2 m! m! ;
: resolve immediate -2 m! find dup 0 >= 3 0br 1 + m! ;

( multiplication/modulo/division )
: *helper -rot -- -rot dup -rot + -rot swap rot ;
: * 0 -rot dup 6 0br rot *helper 0 -12 0br ;
: % 2dup >= 9 0br dup -rot - swap 0 -14 0br drop ;
: /helper rot ++ -rot dup -rot - swap ;
: / 0 -rot 2dup >= 6 0br /helper 0 -11 0br 2drop ;

( if/else branching constructions )
( these are a little bit subtle so they have more comments )

( IF )
( before: TEST if A endif B )
( after:  TEST -2 {b-a} 0br [a] A [b] B )
(   backpatch: a-2 )

( IF-ELSE )
( before: TEST if A else B endif C )
( midway: TEST -2 {b-a} 0br [a] A -2 0 -2 9999 0br [b] B C
(   backpatch: a-2 )
( after:  TEST -2 {b-a} 0br [a] A -2 0 -2 {c-b} 0br [b] B [c] C )
(   backpatch: b-2 )

: const! -2 m! m! ;

: if immediate
  -2 m!          ( write const )
  9999 m!        ( write dummy memory offset )
  resolve 0br m! ( write 0br )
  m@       ( a )        ( r: caller )
  r> swap  ( caller a ) ( r: )
  >r >r    ( )          ( r: a caller )
  ;

: else immediate
  -2 m!           ( write const )
  0 m!            ( write 0 )
  -2 m!           ( write const )
  9999 m!         ( write dummy memory offset )
  resolve 0br m!  ( write 0br )
  m@              ( b )          ( r: a caller )
  r> r>           ( b caller a ) ( r: )
  rot dup >r      ( caller a b ) ( r: b )
  rot >r          ( a b )        ( r: b caller )
  over -          ( a b-a )      ( r: b caller )
  swap 2 -        ( b-a a-2 )    ( r: b caller )
  !               ( backpatch memory offset at a-2 with offset b-a )
  ;
  
: endif immediate
  m@         ( c )          ( r: b caller )
  r> r>      ( c caller b ) ( r: )
  -rot >r    ( b c )        ( r: caller )
  over -     ( b c-b )      ( r: caller )
  swap 2 -   ( c-b b-2 )    ( r: caller )
  !          ( backpatch memory offset at b-2 with offset c-b )
  ;

( DO )
( LIMIT START do A loop )
( LIMIT START >r >r [a] A [z] r> r> -2 1 + 2dup < -2 {c-b} 0br [b] >r >r -2 0 -2 {a-c} 0br [c] 2drop )
( LIMIT START >r >r A r> r> -2 1 + 2dup < -2 {c-b} 0br >r >r -2 0 -2 {a-c} 0br 2drop )
( b=z+10 c=z+16 )
( a-c -> a-z-16 )
( c-b -> 6 )

: do immediate
  resolve >r m!
  resolve >r m!
  m@            ( a        ) ( r: caller ) 
  r> swap       ( caller a ) ( r: )
  >r >r         ( )          ( r: a caller )
  ;

: loop immediate
  r> r>           ( caller a ) ( r: )
  swap >r         ( a )        ( r: caller )
  m@ - 17 -       ( a-z-16 )   ( r: caller )
  resolve r> m!
  resolve r> m!
  -2 m! 1 m!      ( a-z-16 )   ( r: caller )
  resolve + m!
  resolve 2dup m!
  resolve < m!
  -2 m! 6 m!      ( a-z-16 )   ( r: caller )
  resolve 0br m!
  resolve >r m!
  resolve >r m!
  -2 m! 0 m!      ( a-z-16 )   ( r: caller )
  -2 m! m!        ( )          ( r: caller )
  resolve 0br m!
  resolve .s m!
  resolve .s m!
  ( resolve 2drop m! )
  ;


( hello world function )
: # .c ; : hello 10 68 76 82 79 87 32 79 76 76 69 72 # # # # # # # # # # # # ;

hello

: looper 2 0 do 80 .c nl loop ;
( .see looper )
looper
.s .r nl

hello

( positive if test )
: xyz1 67 .c 1 if 68 .c endif 69 .c nl ; xyz1
( negative if test )
: xyz2 67 .c 0 if 68 .c endif 69 .c nl ; xyz2


( BROKEN )
( positive if/else test )
: xyz3 67 .c 1 if 68 .c else 69 .c endif 70 .c nl ; xyz3
( positive if/else test )
: xyz4 67 .c 0 if 68 .c else 69 .c endif 70 .c nl ; xyz4

hello
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

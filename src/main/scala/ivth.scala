package ivth

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

final case class Runtime(memlen: Int, words: Array[String]) {

  // program count
  //
  // if rstack is empty, refers to a word index.
  // otherwise refers to a memory index.
  var pc: Int = 0

  // memory
  //
  // used to read and write data. eventually, pc may be moved into memory.
  // for now this mostly stores built-in and compiled words
  val memory: Array[Int] = new Array[Int](memlen)

  // mfree
  //
  // pointer the next free integer of memory. once allocated memory will never
  // be freed so this counter just goes up and up (maybe exploding eventually).
  var mfree: Int = 1

  // rstack
  //
  // stack of memory address to return to after function calls
  var rstack = new Stack

  // dstack
  //
  // stack of integer values, the main way to manipulate data
  var dstack = new Stack

  // compiling
  //
  // whether the interpreter is currently compiling a word or not. changes the
  // semantics of non-fast words and literals.
  var compiling: Boolean = false

  // read and write locations in 'memory'
  def write(n: Int): Unit = { memory(mfree) = n; mfree += 1 }
  def read(i: Int): Int = memory(i)

  // push and pop values to/from 'dstack'
  def push(n: Int) = dstack.push(n)
  def pop(): Int = dstack.pop()

  // dictionary used to map words to memory addresses
  val dict = mutable.Map.empty[String, (Boolean, Int)]

  // array of builtin words, and variable to track next free slot
  val builtins = new Array[Function1[Runtime, Unit]](16)
  var bfree: Int = 1

  def addBuiltin(name: String, fast: Boolean, f: Function1[Runtime, Unit]) {
    dict(name) = (fast, -bfree)
    builtins(bfree) = f
    bfree += 1
  }

  def initBuiltins() {
    addBuiltin("(*", true, { r =>
      while (r.words(r.pc) != "*)") r.pc += 1
    })

    addBuiltin("copy", false, { r =>
      val n = r.pop()
      val x = r.pop()
      for (i <- 0 until n) r.push(x)
    })

    addBuiltin("alt", false, { r =>
      val n = r.pop()
      val xs = new Array[Int](n)
      for (i <- 0 until n) xs(n - i - 1) = r.pop()
      for (i <- 1 until n) r.push(xs(i))
      r.push(xs(0))
    })

    addBuiltin("0br", false, { r =>
      val t = r.pop()
      val offset = if (t == 0) r.words(r.pc).toInt else 0
      r.pc += 1 + offset
    })

    addBuiltin("nor", false, r => r.push(~(r.pop() | r.pop())))

    addBuiltin("+", false, { r =>
      //println("plus: %s %s" format (r.dstack, r.rstack))
      val x = r.pop()
      val y = r.pop()
      r.push(x + y)
    })

    addBuiltin(".c", false, r => System.out.print(r.pop().toChar))

    addBuiltin(":", false, { r =>
      r.compiling = true
      val name = r.words(r.pc)
      dict(name) = (false, r.mfree)
      r.pc += 1
    })

    addBuiltin(";", true, { r =>
      r.compiling = false
    })
  }

  initBuiltins()

  def lookup(s: String): Lookup = dict.get(s) match {
    case Some((immed, addr)) => Word(addr)
    case None => if (s.matches("-?[1-9][0-9]*|0"))
      Literal(s.toInt)
    else if (s.matches("0x[0-9A-Fa-f]+"))
      Literal(java.lang.Integer.parseInt(s.substring(2), 16))
    else
      NotFound
  }

  def run(): Unit = {
    var done = false
    while (!done) {
      if (rstack.isEmpty) {
        if (pc >= words.length) return ()

        val s = words(pc)
        if (compiling) {
          lookup(s) match {
            case Word(addr) =>
              if (false /* word is fast */) {
              } else {
                memory(mfree) = addr
                mfree += 1
              }
            case Literal(n) =>
              memory(mfree) = -1
              memory(mfree + 1) = n
              mfree += 2
            case NotFound =>
              die("could not compile %s" format s)
          }
          pc += 1
        } else {
          lookup(s) match {
            case Word(addr) =>
              rstack.push(pc + 1)
              pc = addr
            case Literal(n) =>
              push(n)
              pc += 1
            case NotFound =>
              die("could not interpret %s" format s)
          }
        }
      } else if (pc < 0) {
        //println("starting builtin pc=%s rstack=%s" format (pc, rstack))
        builtins(-pc)(this)
        pc = rstack.pop()
        //println("after builtin pc=%s rstack=%s" format (pc, rstack))
      } else {
        rstack.push(pc)
        val addr = memory(pc)
        if (addr == -1) {
          val n = memory(pc + 1)
          push(n)
          pc += 2
        } else {
          pc = addr
        }
      }
    }
  }

  def die(msg: String): Unit = sys.error(msg)
}

import scala.io.Source
object Runtime {
  def parse(path: String): Array[String] =
    Source.fromFile(path).mkString.replace("\n", " ").split(" ").filter(""!=)

  def runfile(path: String): Unit =
    Runtime(parse(path)).run()

  def main(args: Array[String]): Unit = {
    val r = Runtime(512 * 1024, Array("48", "9", "+", ".c", "10", ".c"))
    try {
      r.run()
    } catch {
      case e: Exception =>
        println(e)
        println("pc=%s d=%s r=%s" format (r.pc, r.dstack, r.rstack))
    }
  }
}

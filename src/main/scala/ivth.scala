package ivth

import spire.math._
import spire.syntax._

import scala.collection.mutable

sealed trait Symbol { def fast: Boolean }
case class Literal(n: Int) extends Symbol { def fast = false }
case class Builtin(name: String, fast: Boolean, f: Runtime => Unit) extends Symbol
case class Compiled(name: String, fast: Boolean, symbols: Array[String]) extends Symbol {
  override def toString: String =
    "Compiled(%s, %s, Array(%s))" format (name, fast, symbols.mkString(", "))
}

case class Frame(var w: Int, words: Array[String]) {
  def next(): Unit = w += 1
  def done(): Boolean = w >= words.length
  def apply(): String = words(w)
  def apply(n: Int): String = words(n)
  val saved = mutable.ArrayBuffer.empty[String]
  def storeWord(word: String): Unit = saved.append(word)
  def loadWords(): Array[String] = {
    val words = saved.toArray
    saved.clear()
    words
  }
}

case class Runtime(words: Array[String]) {
  var wstack = mutable.ArrayBuffer(Frame(0, words))
  var nstack = mutable.ArrayBuffer.empty[Int]
  var symbols = mutable.Map.empty[String, Symbol]
  var compiling: Boolean = false

  initBuiltins()

  def push(n: Int): Unit = nstack.append(n)

  def pop(): Int = {
    val i = nstack.length - 1
    val n = nstack(i)
    nstack.remove(i)
    n
  }

  def pushFrame(frame: Frame): Unit = wstack.append(frame)
  def popFrame(): Unit = wstack.remove(wstack.length - 1)

  def stackLength(): Int = nstack.length

  def getNumWords(): Int = wstack.last.words.length
  def getWord(n: Int) = wstack.last.apply(n)
  def getWordPtr(): Int = wstack.last.w
  def setWordPtr(n: Int) = wstack.last.w = n

  def setInterpreting(): Unit = compiling = false
  def setCompiling(): Unit = compiling = true

  def addSymbol(s: String, sym: Symbol): Unit =
    symbols(s) = sym

  def addBuiltin(s: String, fast: Boolean, f: Runtime => Unit): Unit =
    symbols(s) = Builtin(s, fast, f)

  def initBuiltins() {
    addBuiltin("(*", true, { r =>
      var w = r.getWordPtr()
      val n = r.getNumWords()
      while (w < n && r.getWord(w) != "*)") w += 1
      setWordPtr(w + 1)
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
      val n = getWordPtr()
      val offset = if (t == 0) getWord(n).toInt else 0
      setWordPtr(n + 1 + offset)
    })

    // addBuiltin("w>", false, r => r.push(r.getWordPtr()))
    // addBuiltin(">w", false, r => r.setWordPtr(r.pop()))
    addBuiltin("nor", false, r => r.push(~(r.pop() | r.pop())))
    addBuiltin("+", false, r => r.push(r.pop() + r.pop()))

    addBuiltin(".c", false, r => System.out.print(r.pop().toChar))
    //addBuiltin(".n", false, r => System.out.print(r.pop().toString))
    //addBuiltin(".s", false, r => System.out.print(nstack.mkString("stack<", " ", ">")))

    // addBuiltin("_", true, { r =>
    //   val n = r.getWordPtr()
    //   val word = r.getWord(n)
    //   r.setWordPtr(n + 1)
    //   r.lookup(word) match {
    //     case Some(sym) => r.start(sym)
    //     case None => r.die("no symbol found for %s" format word)
    //   }
    // })

    addBuiltin(":", false, { r =>
      setCompiling()
      push(getWordPtr())
      setWordPtr(getWordPtr() + 1)
    })

    addBuiltin(";", true, { r =>
      val name = getWord(pop())
      addSymbol(name, Compiled(name, false, frame.loadWords()))
      setInterpreting()
    })
  }

  def lookup(s: String): Option[Symbol] = symbols.get(s).orElse {
    if (s.matches("-?[1-9][0-9]*|0"))
      Some(Literal(s.toInt))
    else if (s.matches("0x[0-9A-Fa-f]+"))
      Some(Literal(java.lang.Integer.parseInt(s.substring(2), 16)))
    else
      None
  }

  def frame: Frame = wstack.last

  def start(sym: Symbol) = sym match {
    case Builtin(_, _, f) => f(this)
    case Compiled(_, _, words) => pushFrame(Frame(0, words))
    case Literal(n) => push(n)
  }

  def die(msg: String) {
    symbols.foreach {
      case (k, v) => println("symbol %s -> %s" format (k, v))
    }
    println(nstack)
    sys.error(msg)
  }

  def prestart(word: String) = lookup(word) match {
    case Some(sym) => if (!compiling || sym.fast) start(sym) else frame.storeWord(word)
    case None => die("no symbol found for %s" format word)
  }

  def run() {
    while (!wstack.isEmpty) {
      while (!frame.done) {
        val word = frame()
        frame.next()
        prestart(word)
      }
      wstack.remove(wstack.length - 1)
    }
  }
}

import scala.io.Source
object Runtime {
  def parse(path: String): Array[String] =
    Source.fromFile(path).mkString.replace("\n", " ").split(" ").filter(""!=)

  def runfile(path: String): Unit =
    Runtime(parse(path)).run()

  def main(args: Array[String]): Unit =
    args.foreach(runfile)
}

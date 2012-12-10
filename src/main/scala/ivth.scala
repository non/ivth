package ivth

import spire.math._
import spire.syntax._

import scala.collection.mutable

sealed trait Symbol {
  def fast: Boolean
}

case class Literal(n: Int) extends Symbol {
  def fast = false
}

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
    addBuiltin(".n", false, r => System.out.print(r.pop().toString))
    addBuiltin(".s", false, r => System.out.print(nstack.mkString("stack<", " ", ">")))

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
    else if (s.matches("0x[0-9A-Fa-F]+"))
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

object Runtime {
  def main(args: Array[String]) {
    val prog = """

72 .c 69 .c 76 .c 76 .c 79 .c 32 .c 87 .c 79 .c 82 .c 76 .c 68 .c 10 .c
0 10 68 76 82 79 87 32 79 76 76 69 72 2 copy 0br 4 .c 0 0br -8 0 copy

(* convenient printing stuff *)
: nl 10 .c ;
: sp 32 .c ;

(* some stack manipulation *)
: drop 0 copy ;
: dup 2 copy ;
: swap 2 alt ;
: rot 3 alt ;
: -rot rot rot ;
: 2drop drop drop ;
: 2dup swap dup rot dup -rot ;

(*
0 1 2 .s swap .s nl drop drop drop

0 1 2 3 .s rot .s nl drop drop drop drop
0 1 2 3 .s -rot .s nl drop drop drop drop
*)

(* bitwise ops *)
: ~ dup nor ;
: | nor ~ ;
: & ~ swap ~ nor ;
: nand & ~ ;
: ^ dup rot dup -rot & -rot nor nor ;

(* logical ops *)
: != ^ 0br 4 1 0 0br 1 0 ;
: == != 0br 4 0 0 0br 1 1 ;
: bool 0 != ;
: ! 0 == ;
: ==0 0 == ;
: !=0 0 != ;

(* sign checks *)
: 0< -2147483648 & 0 != ;
: 0>= -2147483648 & 0 == ;
: 0> 2147483647 & 0 != ;
: 0<= 2147483647 & 0 == ;
: cmp0 dup 0>= 0br 11
  0> 0br 4
  1 0 0br 6
  0 0 0br 2
drop -1 ;

(* addition/subtraction *)
: negate ~ 1 + ;
: - negate + ;
: -- 1 - ;
: ++ 1 + ;

(* comparisons *)
: cmp swap - cmp0 ;
: < cmp -1 == ;
: <= cmp 1 != ;
: > cmp 1 == ;
: >= cmp 1 != ;

(* testing compare *)
: cmptest 90 .c .s sp 2dup .n sp .n sp cmp .n nl ;
(*
1 0 cmptest
0 1 cmptest
1 1 cmptest
-1 -1 cmptest
9 10 cmptest
9 -10 cmptest
29921 2444 cmptest
24 9929 cmptest
93292 0 cmptest
0 0 cmptest
0 124828 cmptest
-1191 0 cmptest
-1 -1 cmptest
*)

(* multiplication/division *)
: *helper -rot -- -rot dup -rot + -rot swap rot ;
: * 0 -rot dup 0br 5 rot *helper 0 0br -9 drop drop ;
: % 2dup >= 0br 7 dup -rot - swap 0 0br -11 drop ;
: /helper rot ++ -rot dup -rot - swap ;
: / 0 -rot 2dup cmp 0<= 0br 4 /helper 0 0br -9 2drop ;

(*
10 10 cmp 0<= .n nl
11 10 cmp 0<= .n nl
100 10 cmp 0<= .n nl
*)

100 10 % .n nl
11 10 % .n nl
10 10 % .n nl

(* broken number printing *)
: . dup 0< 0br 3 45 .c negate
-1 swap
  dup 10 % swap 10 / dup 0br 3 0 0br -12
drop dup -1 != 0br 6
  48 + .c 0 0br -11
drop ;

(* number printing *)
: .. dup 0< 0br 3 45 .c negate
-1 swap
  dup 10 % swap 10 / .s dup 0br 3 0 0br -13
drop dup 0>= 0br 6
  48 + .c 0 0br -10
drop ;

(* testing number printing *)
: ..test dup .n sp .. nl ;

-999 ..test
-10 ..test
0 ..test
1 ..test
9 ..test
10 ..test
11 ..test
100 ..test
101 ..test
999 ..test

: cats dup .n sp 67 .c 65 .c 84 .c 83 .c nl -- dup 0<= 0br -17 drop ;

(*
.s nl

5 cats
11 cats

.s nl
*)
"""

    //val prog = ": blah _ 69 _ .c ; 72 .c blah 10 .c"

    val words = prog.replace("\n", " ").split(" ").filter(""!=)
    val r = Runtime(words)
    r.run()
  }
}

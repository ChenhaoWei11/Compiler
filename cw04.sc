// Chenhao Wei  Q20024299
//=====
//
// The main idea is to extend the parser form the lectures 
// (which processes strings) to a parser that processes
// tokens. For this you need to use the lexer from CW2 and
// possibly adjust the lexing regular expressions accordingly.

import scala.io.StdIn.readInt



// Rexp
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class RECD(s: String, r: Rexp) extends Rexp
case class RANGE(cs: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

// Values, you might have to extend them
// according to which values you want to create
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val
//case class Plus(vs: List[Val]) extends Val
//case class Opt(v: Val) extends Val
//case class Ntimes(vs: List[Val]) extends Val


// Convenience for typing
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s : String) : Rexp =
  charlist2rexp(s.toList)

extension (r: Rexp) {
  def ~ (s: Rexp) = SEQ(r, s)
  def % = STAR(r)
  def | (s: Rexp) = ALT(r, s)
}

extension (s: String) {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def $ (r: Rexp) = RECD(s, r)
}

// nullable
def nullable(r: Rexp): Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(r) => true
    case RANGE(cs) => false
    case PLUS(r) => nullable(r)
    case OPTIONAL(r) => true
    case NTIMES(r, n) => if (n == 0) true else nullable(r)
    case RECD(_, r) => nullable(r)
  }

// der
def der(c: Char, r: Rexp): Rexp = r match {
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2) =>
      if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
      else SEQ(der(c, r1), r2)
    case STAR(r) => SEQ(der(c, r), STAR(r))
    case RANGE(cs) => if (cs.contains(c)) ONE else ZERO
    case PLUS(r) => SEQ(der(c, r), STAR(r))
    case OPTIONAL(r) => der(c, r)
    case NTIMES(r, n) => if (n == 0) ZERO else SEQ(der(c, r), NTIMES(r, n - 1))
    case RECD(_, r1) => der(c, r1)
  }

// Flatten
def flatten(v: Val): String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) + flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
    case Rec(_, v) => flatten(v)
    //case Plus(vs) => vs.map(flatten).mkString
    //case Opt(v) => flatten(v)
    //case Ntimes(vs) => vs.map(flatten).mkString
  }

// Token types 
type Token = (String, String)
type Tokens = List[Token]

// Env
def env(v: Val): Tokens = v match {
    case Empty => Nil
    case Chr(c) => Nil
    case Left(v) => env(v)
    case Right(v) => env(v)
    case Sequ(v1, v2) => env(v1) ::: env(v2)
    case Stars(vs) => vs.flatMap(env)
    case Rec(x, v) => (x, flatten(v)) :: env(v)
    //case Plus(vs) => vs.flatMap(env)
    //case Opt(v) => env(v)
    //case Ntimes(vs) => vs.flatMap(env)
  }

  
// Mkeps
def mkeps(r: Rexp): Val = r match {
    case ONE => Empty
    case ALT(r1, r2) =>
      if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
    case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
    case STAR(r) => Stars(Nil)
    case RECD(x, r) => Rec(x, mkeps(r))
    case PLUS(r) => mkeps(r)
    case OPTIONAL(r) => Empty
    case NTIMES(r, n) => if (n == 0) Stars(Nil) else Stars(List(mkeps(r)))
  }

// Inj
def inj(r: Rexp, c: Char, v: Val): Val = (r, v) match {
    case (STAR(r), Sequ(v, Stars(vs))) => Stars(inj(r, c, v) :: vs)
    case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
    case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CHAR(d), Empty) => Chr(c)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
    case (RANGE(cs), Empty) => Chr(c)
    case (PLUS(r), Sequ(v, Stars(vs))) => Stars(inj(r, c, v) :: vs)
    case (OPTIONAL(r), v) => inj(r, c, v)
    case (NTIMES(r, 0), _) => Empty
    case (NTIMES(r, n), Sequ(v, Stars(vs))) => Stars(inj(r, c, v) :: vs)
  }

// Rectification functions
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v: Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v: Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v: Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v)  => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v: Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
  (v: Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
  (v: Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v: Val) => v match {
      case Rec(x, v) => Rec(x, f(v))
}
def F_ERROR(v: Val): Val = throw new Exception("error")

//Simp
def simp(r: Rexp): (Rexp, Val => Val) = r match {
    case ALT(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (r2s, F_RIGHT(f2s))
        case (_, ZERO) => (r1s, F_LEFT(f1s))
        case _ =>
          if (r1s == r2s) (r1s, F_LEFT(f1s))
          else (ALT(r1s, r2s), F_ALT(f1s, f2s))
      }
    }
    case SEQ(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (ZERO, _) => (ZERO, F_ERROR)
        case (_, ZERO) => (ZERO, F_ERROR)
        case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
        case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
        case _ => (SEQ(r1s, r2s), F_SEQ(f1s, f2s))
      }
    }
    case r => (r, F_ID)
  }

//Lex
def lex_simp(r: Rexp, s: List[Char]): Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else 
    { throw new Exception("lexing error") }
  case c :: cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def ders_simp(cs: List[Char], r: Rexp) : Rexp = cs match {
  case Nil => r
  case c::cs => ders_simp(cs, simp(der(c, r))._1)
}

def lexing_simp(r: Rexp, s: String) =
  env(lex_simp(r, s.toList)) 



// Language specific code
val EOL : Rexp = "\n" | "\r\n"
val KEYWORD: Rexp =
  "while" | "if" | "then" | "else" | "do" | "for" | "to" | "true" | "false" | "read" | "write" | "skip" | "break"
val OP: Rexp =
  "+" | "-" | "*" | "%" | "/" | "==" | "!=" | ">" | "<" | "<=" | ">=" | ":=" | "&&" | "||"
val LET: Rexp = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toSet)
val SYM: Rexp = RANGE("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ._><=;,:\\".toSet)
val PARENS: Rexp = RANGE("(){}".toSet)
val DIGIT: Rexp = RANGE("0123456789".toSet)
val NONZERODIGIT: Rexp = RANGE("123456789".toSet)
val SEMI: Rexp = ";"
val WHITESPACE: Rexp = PLUS(" " | "\n" | "\t" |"\r")
val ID: Rexp = LET ~ ("_" | LET | DIGIT).%
val NUMBER: Rexp = CHAR('0') | NONZERODIGIT ~ DIGIT.%
val STRING: Rexp = "\"" ~ (SYM | DIGIT | PARENS | WHITESPACE | "\n").% ~ "\""
val COMMENT: Rexp = "//" ~ (DIGIT | SYM | PARENS | " ").% ~ EOL


val WHILE_REGS: Rexp = (("eol" $ EOL) |
                        ("keyword" $ KEYWORD) |
                        ("operator" $ OP) |
                        ("parenthesis" $ PARENS) |
                        ("semicolon" $ SEMI) |
                        ("whitespace" $ WHITESPACE) |
                        ("identifier" $ ID) |
                        ("number" $ NUMBER) |
                        ("string" $ STRING) |
                        ("comment" $ COMMENT)
                        ).%

def esc(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def escape(tks: Tokens) =
  tks.map{ case (s1, s2) => (s1, esc(s2))}

//filter out whitespace and comment
def tokenfilter(tks: Tokens) = tks.filter(_._1 != "whitespace").filter(_._1 != "comment")


// more convenience for the map parsers later on;
// it allows writing nested patterns as
// case x ~ y ~ z => ...

case class ~[+A, +B](x: A, y: B)

type IsSeq[A] = A => Seq[_]

abstract class Parser[I, T](using is: I => Seq[_])  {
  def parse(in: I): Set[(T, I)]  

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if is(tl).isEmpty) yield hd
}

// parser combinators

// alternative Parser
class AltParser[I: IsSeq, T](p: => Parser[I, T], 
                             q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)
}

// sequence Parser
class SeqParser[I: IsSeq, T, S](p: => Parser[I, T], 
                                q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) =
    for ((hd1, tl1) <- p.parse(in);
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}

// map Parser
class MapParser[I: IsSeq, T, S](p: => Parser[I, T], 
                                f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}


// token parser 
case class TokenParser(s: String) extends Parser[Tokens, String] {
  def parse(in: Tokens) = {
    if (in.nonEmpty && in.head._2 == s) Set((s, in.tail)) else Set()
  }
}

// atomic parser for (particular) strings
case object StrParser extends Parser[Tokens, String] {
  def parse(in: Tokens) = {
    if (in.nonEmpty && in.head._1 == "string") Set((in.head._2, in.tail))
    else Set()
  }
}

// atomic parser for identifiers (variable names)
case object IdParser extends Parser[Tokens, String] {
  def parse(in: Tokens) = {
    if (in.nonEmpty && in.head._1 == "identifier") Set((in.head._2, in.tail))
    else Set()
  }
}

// atomic parser for numbers (transformed into Ints)
case object NumParser extends Parser[Tokens, Int] {
  def parse(in: Tokens) = {
    if (in.nonEmpty && in.head._1 == "number") Set((in.head._2.toInt, in.tail))
    else Set()
  }
}


// the following token interpolation allows us to write 
// TokenParser(_some_string_) more conveniently as 
//
// p"<_some_string_>" 

extension(sc: StringContext) 
    def p(args: Any*) = TokenParser(sc.s(args: _*))

// More convenient syntax for parser combinators
extension[I: IsSeq, T](p: Parser[I, T]) {
    def ||(q: => Parser[I, T]) = new AltParser[I, T](p, q)
    def ~[S](q: => Parser[I, S]) = new SeqParser[I, T, S](p, q)
    def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
  }



// AST For the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp

type Block = List[Stmt]

case object Skip extends Stmt
case object Break extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class For(x: String, a: AExp, b: AExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Read(s: String) extends Stmt
case class WriteId(s: String) extends Stmt
case class WriteString(s: String) extends Stmt

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class Lop(o: String, b1: BExp, b2: BExp) extends BExp

// arithmetic expressions
lazy val AExp: Parser[Tokens, AExp] =
  (Te ~ p"+" ~ AExp).map[AExp] { case x ~ _ ~ z => Aop("+", x, z) } ||
  (Te ~ p"-" ~ AExp).map[AExp] { case x ~ _ ~ z => Aop("-", x, z) } || Te
lazy val Te: Parser[Tokens, AExp] =
  (Fa ~ p"*" ~ Te).map[AExp] { case x ~ _ ~ z => Aop("*", x, z) } ||
  (Fa ~ p"/" ~ Te).map[AExp] { case x ~ _ ~ z => Aop("/", x, z) } ||
  (Fa ~ p"%" ~ Te).map[AExp] { case x ~ _ ~ z => Aop("%", x, z) } || Fa
lazy val Fa: Parser[Tokens, AExp] =
  (p"(" ~ AExp ~ p")").map[AExp] { case _ ~ y ~ _ => y } ||
  IdParser.map(Var(_)) ||
  NumParser.map(Num(_))

// Boolean Expressions
lazy val BExp: Parser[Tokens, BExp] =
  (CExp ~ p"&&" ~ BExp).map[BExp] { case x ~ _ ~ z => Lop("&&", x, z) } ||
  (CExp ~ p"||" ~ BExp).map[BExp] { case x ~ _ ~ z => Lop("||", x, z) } || CExp

lazy val CExp: Parser[Tokens, BExp] =
  (AExp ~ p"==" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("==", x, z); } ||
  (AExp ~ p"!=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("!=", x, z); } ||
  (AExp ~ p">" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop(">", x, z); } ||
  (AExp ~ p">=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop(">=", x, z); } ||
  (AExp ~ p"<" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("<", x, z); } ||
  (AExp ~ p"<=" ~ AExp).map[BExp] { case x ~ _ ~ z => Bop("<=", x, z);} || Boolean

lazy val Boolean: Parser[Tokens, BExp] =
  (p"true".map[BExp] { _ => True }) ||
  (p"false".map[BExp] { _ => False }) ||
  (p"(" ~ BExp ~ p")").map[BExp] { case _ ~ y ~ _ => y }

// a single statement
lazy val Stmt: Parser[Tokens, Stmt] =
  (p"skip".map[Stmt] { _ => Skip }) ||
  (p"break".map[Stmt] { _ => Break }) ||
  (IdParser ~ p":=" ~ AExp).map[Stmt] { case x ~ _ ~ z => Assign(x, z) } ||
  (p"write" ~ p"(" ~ IdParser ~ p")").map[Stmt] { case _ ~ _ ~ z ~ _ => WriteId(z) } ||
  (p"write" ~ p"(" ~ StrParser ~ p")").map[Stmt] { case _ ~ _ ~ z ~ _ => WriteString(z)} ||
  (p"write" ~ IdParser).map[Stmt] { case _ ~ y => WriteId(y) } ||
  (p"write" ~ StrParser).map[Stmt] { case _ ~ y => WriteString(y) } ||
  (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block).map[Stmt] { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
  (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt] { case _ ~ y ~ _ ~ w => While(y, w) } ||
  (p"for" ~ IdParser ~ p":=" ~ AExp ~ p"upto" ~ AExp ~ p"do" ~ Block).map[Stmt] {case _ ~ x ~ _ ~ a ~ _ ~ b ~ _ ~ w => For(x, a, b, w) } ||
  (p"read" ~ p"(" ~ IdParser ~ p")").map[Stmt] { case _ ~ _ ~ z ~ _ => Read(z) } ||
  (p"read" ~ IdParser).map[Stmt] { case _ ~ y => Read(y) } 

// statements
lazy val Stmts: Parser[Tokens, Block] =
  (Stmt ~ p";" ~ Stmts).map[Block] { case x ~ _ ~ z => x :: z } ||
  (Stmt.map[Block] { s => List(s) })

// blocks (enclosed in curly braces)
lazy val Block: Parser[Tokens, Block] =
  ((p"{" ~ Stmts ~ p"}").map { case _ ~ y ~ _ => y } ||
  (Stmt.map(s => List(s))))



//CW04
// compiler headers needed for the JVM
// (contains a method for write)
val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/print(I)V 
    return 
.end method

.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
    return
.end method

.method public static read()I
    .limit locals 10
    .limit stack 10

    ldc 0
    istore 1    ; this will hold our final integer
Label1:
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    istore 2
    iload 2
    ldc 10    ; test for the newline delimiter for Unix
    isub
    ifeq Label2
    iload 2
    ldc 13    ; test for the carriage-return in Windows
    isub
    ifeq Label2
    iload 2
    ldc 32    ; the space delimiter
    isub
    ifeq Label2
    iload 2
    ldc 48 ; we have our digit in AsCIl, have to subtract it from 48
    isub
    ldc 10
    iload 1
    imul
    iadd
    istore 1
    goto Label1
Label2:
    ; when we come here we have our integer computed
    ; in local variable 1
    iload 1
    ireturn
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

"""

val ending = """
; COMPILED CODE ENDS
   return

.end method
"""

// Compiler functions


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// convenient string interpolations 
// for instructions and labels

extension (sc: StringContext) {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}

// this allows us to write things like
// i"iadd" and l"Label"


// environments 
type Env = Map[String, Int]


def compile_op(op: String) = op match {
  case "+" => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
  case "/" => i"idiv"
  case "%" => i"irem"
}

// arithmetic expression compilation
def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)} \t\t; $s"
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
}

// boolean expression compilation
//  - the jump-label is for where to jump if the condition is not true
def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
  case True => ""
  case False => i"goto $jmp"
  case Lop("&&", b1, b2) =>
    compile_bexp(b1, env, jmp) ++ compile_bexp(b2, env, jmp)
  case Lop("||", b1, b2) => {
    val label = Fresh("label")
    compile_bexp(b1, env, label) ++ l"$label" ++ compile_bexp(b2, env, jmp)
  }
  case Bop("==", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
  case Bop("!=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
  case Bop("<", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"
  case Bop("<=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpgt $jmp"
  case Bop(">", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmple $jmp"
  case Bop(">=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmplt $jmp"
}

// statement compilation
def compile_stmt(s: Stmt, env: Env, break_label: String) : (String, Env) = s match {
  case Skip => ("", env)
  case Break => (i"goto $break_label", env)
  case Assign(x, a) => {
    val index = env.getOrElse(x, env.keys.size)
    (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
  } 
  case If(b, bl1, bl2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If_end")
    val (instrs1, env1) = compile_block(bl1, env, break_label)
    val (instrs2, env2) = compile_block(bl2, env1, break_label)
    (compile_bexp(b, env, if_else) ++
     instrs1 ++
     i"goto $if_end" ++
     l"$if_else" ++
     instrs2 ++
     l"$if_end", env2)
  }
  case While(b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val (instrs1, env1) = compile_block(bl, env, loop_end)
    (l"$loop_begin" ++
     compile_bexp(b, env, loop_end) ++
     instrs1 ++
     i"goto $loop_begin" ++
     l"$loop_end", env1)
  }
  case For(x, a, b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val tmp = Fresh("tmp")
    val x_tmp = Fresh(x)
    val x_index = env.keys.size
    val env1 = if env.contains(x) then
        env + (x_tmp -> x_index) + (x -> x_index)
      else
        env + (x -> x_index)
    val tmp_index = env1.keys.size
    val env2 = env1 + (tmp -> tmp_index)

    val init_insts = compile_aexp(a, env) ++ i"istore $x_index \t\t; $x" ++
                  compile_aexp(b, env) ++ i"istore $tmp_index \t\t; $tmp"

    val cond_insts = i"iload $x_index \t\t; $x" ++
                     i"iload $tmp_index \t\t; $tmp" ++
                     i"if_icmpgt $loop_end"

    val (body_insts, env3) = compile_block(bl, env2, loop_end)
    val x_inc_insts = i"iinc $x_index 1 \t\t; ++$x"
    (init_insts ++
     l"$loop_begin" ++
     cond_insts ++
     body_insts ++
     x_inc_insts ++
     i"goto $loop_begin" ++
     l"$loop_end", env)
  }
  case WriteId(x) => {
    (i"iload ${env(x)} \t\t; $x" ++ 
     i"invokestatic XXX/XXX/write(I)V", env)
  }
  case WriteString(s) =>
    (i"ldc ${s}" ++
      i"invokestatic XXX/XXX/writes(Ljava/lang/String;)V", env)
  case Read(x) => {
    val index = env.getOrElse(x, env.keys.size)
    (i"invokestatic XXX/XXX/read()I" ++
      i"istore $index \t\t; $x", env + (x -> index))
  }
}

// compilation of a block (i.e. list of instructions)
def compile_block(bl: Block, env: Env, break_label: String) : (String, Env) = bl match {
  case Nil => ("", env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env, break_label)
    val (instrs2, env2) = compile_block(bl, env1, break_label)
    (instrs1 ++ instrs2, env2)
  }
}

// main compilation function for blocks
def compile(bl: Block, class_name: String) : String = {
  val break_label = Fresh("exit")
  val instructions = compile_block(bl, Map.empty, break_label)._1
  (beginning ++ instructions ++ l"$break_label" ++ ending).replaceAllLiterally("XXX", class_name)
}


// Fibonacci numbers as a bare-bone test-case
// val fib_test = 
//   List(Assign("n", Num(9)),            //  n := 9;                     
//        Assign("minus1",Num(0)),         //  minus1 := 0;
//        Assign("minus2",Num(1)),         //  minus2 := 1;
//        Assign("temp",Num(0)),           //  temp := 0;
//        While(Bop("<",Num(0),Var("n")),  //  while 0 < n do  {
//           List(Assign("temp",Var("minus2")), //  temp := minus2;
//                Assign("minus2",Aop("+",Var("minus1"),Var("minus2"))), 
//                                         //  minus2 := minus1 + minus2;
//                Assign("minus1",Var("temp")), //  minus1 := temp;
//                Assign("n",Aop("-",Var("n"),Num(1))))), //  n := n - 1 };
//        Write("minus1"))                 //  write minus1



// prints out the JVM instructions
// @main
// def test() = 
//   println(compile(fib_test, "fib"))


def run(src: String, class_name: String, show_asm: Boolean=false) = {
    val bl: Block = Stmts.parse_all(tokenfilter(lexing_simp(WHILE_REGS, src))).head

    val code = compile(bl, class_name)
    if show_asm then {
      println("assembler code:")
      println("--------------------------")
      println(code)
      println("--------------------------")
    }
    os.write.over(os.pwd / s"$class_name.j", code)
    os.proc("java", "-jar", "jasmin.jar", s"$class_name.j").call()
    os.proc("java", s"$class_name/$class_name").call(stdout = os.Inherit, stdin = os.Inherit)
    ()
}

@main
def primes_test() = {
  println("Primes Test:")
  val primes =
    """
    // prints out prime numbers from
    // 2 to 100 (end)

    end := 100;
    n := 2;
    while (n < end) do {
      f := 2;
      tmp := 0;
      while ((f < n / 2 + 1) && (tmp == 0)) do {
        if ((n / f) * f == n) then  { tmp := 1 } else { skip };
        f := f + 1
      };
      if (tmp == 0) then { write(n); write("\n") } else { skip };
      n  := n + 1
    }
  """
  run(primes, "Primes")
}


@main
def factors_test() = {
  println("Factors Test:")
  val factors =
    """
  // Find all factors of a given input number
  // by J.R. Cordy August 2005

  write "Input n please: ";
  read n;
  write "The factors of n are:\n";
  f := 2;
  while (f < n / 2 + 1) do {
    if ((n / f) * f == n) then  { write(f); write "\n" } else { skip };
    f := f + 1
  }
  """
  run(factors, "Factors")
}


@main
def fib_test() = {
  println("Fib Test:")
  val fib =
    """
    write "Fib: ";
    read n;  
    minus1 := 1;
    minus2 := 0;
    while n > 0 do {
          temp := minus2;
          minus2 := minus1 + minus2;
          minus1 := temp;
          n := n - 1
    };
    write "Result: ";
    write minus2;
    write "\n"
  """
  run(fib, "Fib")
}


@main
def collatz_test() = {
  println("Collatz Test:")
  val collatz =
    """
    write "Input a number ";
    read n;
    while n > 1 do {
      if n % 2 == 0 
      then n := n/2 
      else n := 3*n+1
    };
    write "Yes\n" 
  """
  run(collatz, "Collatz")
}


@main
def collatz2_test() = {
  println("Collatz2 Test")
  val collatz2 =
    """
    // Collatz series
    //
    // needs writing of strings and numbers; comments

    bnd := 1;
    while bnd < 101 do {
      write bnd;
      write ": ";
      n := bnd;
      cnt := 0;

      while n > 1 do {
        write n;
        write ",";
        
        if n % 2 == 0 
        then n := n / 2 
        else n := 3 * n+1;

        cnt := cnt + 1
      };

      write " => ";
      write cnt;
      write "\n";
      bnd := bnd + 1
    }
  """
  run(collatz2, "Collatz2")
}

@main
def for_test1() = {
  println("For Test:")
  val loop =
  """
  for i := 2 upto 10 do {
    write i
  }
  """
  run(loop, "ForLoop1")
}

@main
def nested_for_test() = {
  println("Nested For Test:")
  val loop =
  """
  for i := 1 upto 2 do {
    for j := 1 upto 3 do {
      write "i = ";
      write i;
      write ", j = ";
      write j;
      write "\n"
    }
  }
  """
  run(loop, "ForLoop1")
}

//@main
def nested_for_shadow_var_test() = {
  println("Nested Shadow Variable Test:")
  val code =
  """
  for i := 1 upto 10 do {
    for i := 1 upto 10 do {
      write i
    }
  }
  """
  run(code, "ForLoop1", show_asm = true)
}

//@main
def break_test() = {
  println("Break test:")
  val code =
  """
  // should print 0 .. 10
  write "should print 0 .. 10\n";
  for i := 0 upto 10 do {
    write i;
    write "\n"
  };

  // should print 0 .. 4
  write "should print 0 .. 4\n";
  for i := 0 upto 10 do {
    if i > 4 then break else skip;
    write i;
    write "\n"
  };
  write "Should print\n";
  break;
  write "Should not print\n"
  """
  run(code, "ForLoop1")
}

@main
def loop_test() = {
  println("Loop Test:")
  val loop =
    """
    start := 1000; 
    x := start;
    y := start;
    z := start;
    while 0 < x do {
    while 0 < y do {
      while 0 < z do { z := z - 1 };
      z := start;
      y := y - 1
    };     
    y := start;
    x := x - 1
    }
  """
  run(loop, "Loop")
}

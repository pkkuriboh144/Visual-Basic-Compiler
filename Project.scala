package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Syntax
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Expr
  case class CstI (n : Int)                                           extends Expr
  case class Var (nm : String)                                        extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr)                 extends Expr
  case class Call (nm : String, es : List[Expr])					  extends Expr
  
  sealed trait Stmt
  case class Block (ss : List[Stmt])                              					extends Stmt
  case class PrintExpr (e : Expr)                                  					extends Stmt
  case class PrintStr (str : String)												extends Stmt
  case class Decl (nm : String)														extends Stmt
  case class Asgn (nm : String, e : Expr)                         					extends Stmt
  case class If (e : Expr, s1 : Stmt, s2 : Stmt)                  					extends Stmt
  case class For (nm1 : String, low : Expr, high : Expr, s : Stmt, nm2 : String)	extends Stmt
  case class While (e : Expr, s : Stmt)                           					extends Stmt
  case class Return (e : Expr)														extends Stmt
  
  
  
  case class Func (nm : String, params : List[String], body : Stmt)
  
  case class Program (funs : List[Func], main : Stmt)
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Parsing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil)              => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = P (digits.map (n => CstI (n)))
	val string : Parser[String] = P("\"" ~ (CharIn(' ' to '!') | CharIn('#' to '~')).rep().! ~ "\"")
    val keywords : List[String] = List ("if", "else", "while", "for", "print", "dim", "as", "then", "end if",
	"to", "next", "wend", "function")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = P (ident.map (s => Var (s)))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (CharIn (" \n\t\r").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val atExpr : Parser[Expr] = P (
      integer | 
      (ident ~ ("(" ~ expr.rep (sep = ",").map (s => s.toList) ~ ")").?).map {
        case (nm, None)      => Var (nm)
        case (nm, Some (es)) => Call (nm, es)
      }| 
      ("(" ~/ expr ~ ")")
    )
    val multDiv : Parser[Expr] = P (
      (atExpr ~ (("*" | "/").! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val addSub : Parser[Expr] = P (
      (multDiv ~ (("+" | "-" | "%").! ~ multDiv).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val gtLtGeLeExpr : Parser[Expr] = P (
      (addSub ~ ((">=" | "<=" | ">" | "<").! ~ addSub).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val eqNeExpr : Parser[Expr] = P (
      (gtLtGeLeExpr ~ (("=" | "<>" | "||").! ~ gtLtGeLeExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val expr : Parser[Expr] = P (eqNeExpr)

	val block : Parser [Stmt] = P(("[" ~ stmt.rep ~ "]").map { case ss => Block (ss.toList) })
	
    val stmt : Parser[Stmt] = P (
	("dim" ~ ident ~ "as integer").map {case (nm) => Decl (nm) } |
	(ident ~ "=" ~ expr).map { case (nm, e) => Asgn (nm, e) } |
	("if" ~ "(" ~ expr ~ ")" ~ "then" ~ stmt ~ "else" ~ stmt ~ "end if").map { case (e, s1, s2) => If (e, s1, s2) } |
	("for" ~ ident ~ "=" ~ expr ~ "to" ~ expr ~ stmt ~ "next" ~ ident).map { case (nm1, e1, e2, s, nm2) => For (nm1, e1, e2, s, nm2) } |
	("while" ~ "(" ~ expr ~ ")" ~ stmt ~ "wend").map { case (e, s) => While (e, s) } |
	("printexpr" ~ expr).map { case (e) => PrintExpr (e) } |
	("printstr" ~ "(" ~ string ~ ")").map {case (str) => PrintStr (str) } |
	("return" ~ expr).map {case(e) => Return(e)} |
	block
    )

	val func : Parser[Func] = P ("function" ~ ident ~ "(" ~ ident.rep (sep=",").map (s => s.toList) ~ "as integer" ~ ")" ~ "as integer" ~ block ~ "end function").map{case (nm, params, body) => Func (nm, params, body)}
	
	val program : Parser [Program] = P (func.rep.map(s => s.toList) ~ block).map {case (funcs, body) => Program(funcs, body)}
    
	val start : Parser[Program] = P (program ~ End)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Pretty printing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def ppExpr (e : Expr) : String = {
    e match {
      case CstI (i)                     => i.toString
      case Var (x)                      => x
      case Prim (op, e1, e2)            => "(%s %s %s)".format (ppExpr (e1), op, ppExpr (e2))
	  case Call (nm, es)				=> "(%s (%s))".format(nm, es.map(ppExpr)mkString(", "))
    }
  }

  def ppBlock (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Block (ss) => {
        val sb = new StringBuilder
        for (s <- ss) {
          sb.append (ppStmt (newIndent, s))
        }
        sb.toString
      }
      case _ => {
        "%s".format (ppStmt (newIndent, s))
      }
    }
  }

  def ppStmt (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
	  case Decl (nm)	=>
		"%sdim %s as integer".format(indent, nm)
      case Asgn (nm, e)           => 
        "%sdim %s as integer = %s\n".format (indent, nm, ppExpr (e))
      case If (e, s1, s2)         => 
        "%sif (%s) then {\n%s%s} else {\n%s%s}\n end if\n".format (indent, ppExpr (e), ppBlock (indent, s1), indent, ppBlock (indent, s2), indent)
      case Block (ss) => {
        "%s%s%s\n".format (indent, ppBlock (indent, s), indent)
      }
      case For (nm1, low, high, s, nm2) => {
        "%sfor %s = %s to %s \n%s%s\n%snext %s\n".format (indent, nm1, ppExpr (low), ppExpr (high), ppBlock (indent, s), indent, indent, nm2)
      }
      case While (e, s)           => 
        "%swhile (%s) {\n%s%s}\n wend\n".format (indent, ppExpr (e), ppBlock (indent, s), indent)
      case PrintExpr (e)              => 
        "%sprint (%s)\n".format (indent, ppExpr (e))
	  case PrintStr (str)		=>
		"%sprint \"%s\"\n".format (indent, str)
	  case Return(e)		=>
		"%sreturn (%s)\n".format(indent, ppExpr(e))
    }
  }
  
  def ppFunc (f : Func) : String = {
	"%sfunction %s (%s as integer)\n%send function\n".format (f.nm, f.params.mkString(", "), ppStmt ("", f.body))
  }

  def ppProgram (p : Program) : String = {
    p.funs.map (f => ppFunc (f)).mkString ("\n") + ppStmt ("", p.main)
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Code Generation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  type Env = Map[String,String]
  type FuncEnv = Map[String,Func]

  val emptyEnv : Env = Map.empty

  var labelCounter : Int = 0
  def newLabel () : String = {
    labelCounter = labelCounter + 1
    "lab%03d".format (labelCounter)
  }

  // Generate x86-64 assembly to evaluate e.
  // Result is at the top of the stack.
  // The following registers may be changed by the generated assembly language: %rax, %rbx, %rsp, %rip
  def compileExpr (e : Expr, env : Env, fenv : FuncEnv) : String = {
    e match {
      case CstI (i)           => 
        "\tpushq\t$%d\n".format (i)
      case Var (x)            => 
        env.get (x) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (x))
          case Some (lab) => 
            "\tpushq\t%s\n".format (lab)
        }
      case Prim (op, e1, e2) => {
        val insts1 = compileExpr (e1, env, fenv) 
        val insts2 = compileExpr (e2, env, fenv)
        val push = "\tpushq\t%rax\n"
        def pop (reg : String) = "\tpopq\t%%%s\n".format (reg)
        val instsOp : String = op match {
          case  "+" => "\taddq\t%rbx, %rax\n"
          case  "-" => "\tsubq\t%rbx, %rax\n"
          case  "*" => "\timulq\t%rbx, %rax\n"
		  case "%" => {
			"\tcltd\t\n" +
			"\tdivq\t%rbx\n" +
			"\tmovq\t%rdx, %rax\n"
		  }
          case  "=" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets ZF if ((rax-rbx) = 0) as signed, i.e., (rax = rbx)
            "\tsete\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if ZF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          // case "<>" => b2i (i1 != i2) 
          case  "<" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets SF if ((rax-rbx) < 0) as signed, i.e., (rax < rbx)
            "\tsets\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if SF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          // case  ">" => b2i (i1 > i2) 
		  case ">" => {
			"\tcmpq\t%rax, % rbx\n" +
			"\tsets\t%al\n" +           
            "\tmovzbl\t%al, %eax\n"   
		  }
          // case "<=" => b2i (i1 <= i2) 
		  case "<=" => {
			"\tcmpq\t%rbx, %rax\n" +
			"\tsetge\t%al\n" +
			"\tmovzbl\t%al, %eax\n"
		  }
          // case ">=" => b2i (i1 >= i2) 
		  case ">=" => {
			"\tcmpq\t%rax, %rbx\n" +
			"\tsetge\t%al\n" +
			"\tmovzbl\t%al, %eax\n"
		  }
          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
        insts1 +
        insts2 +
        pop ("rbx") +
        pop ("rax") +
        instsOp + 
        push
      }
      case Call (nm, es) => {
        es.reverse.map (e => compileExpr (e, env, fenv)).mkString +
        "\tcall\t%s\n".format (nm) + 
        "\taddq\t$%d, %%rsp\n".format (es.length * 8) +
        "\tpushq\t%rax\n"
      }
    }
  }

  def compileAll (prog : Program, env : Env, fenv : FuncEnv) : String = {
    header () + 
    compileFunc (Func ("main", Nil, prog.main), env, fenv) + 
    "\n" +
    prog.funs.map (fd => compileFunc (fd, env, fenv)).mkString ("\n") + 
    footer (env)
  }

  def header () : String = {
    ""
  }

  def footer (env : Env) : String = {
    "\n" +
    "\t.section .rodata\n" + 
    ".output:\n" + 
    "\t.string \"%d\\n\"\n" +
    "\n" +
    (for ((nm1, _) <- env) yield {
      "\t.globl\t%s\n".format (nm1) +
      "\t.data\n".format (nm1) +
      "\t.align\t8\n" +
      "\t.size\t%s, 8\n".format (nm1.replace(" ", "")) +
      "%s:\n".format (nm1.replace(" ", "")) +
      "\t.quad\t0\n" +
      "\n"
    }).mkString
  }

  def compileFunc (func : Func, env : Env, fenv : FuncEnv) : String = {
    val header = {
      "\t.text\n" +
      "\t.globl\t%s\n".format (func.nm) +
      "\t.type\t%s, @function\n".format (func.nm) +
      "%s:\n".format (func.nm) + 
      "\tpushq\t%rbp\n" + 
      "\tmovq\t%rsp, %rbp\n" 
    }
    val footer = {
      "\tpopq\t%rbp\n" + 
      "\tret\n"
    }
    var env2 : Env = env
    for ((param, i) <- func.params.zipWithIndex) {
      env2 = env2 + ( (param, "%d(%%rbp)".format ((i + 2) * 8)) ) 
    }
    header + 
    compileStmt (func.body, env2, fenv) + 
    footer
  }

  def compileStmt (s : Stmt, env : Env, fenv : FuncEnv) : String = {
    s match {
	  case Decl (nm)	=> {
		ppStmt("// ", s)
	  }
      case Asgn (nm, e)            => {
        env.get (nm) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (nm))
          case Some (lab) => 
            ppStmt ("// ", s) + 
            compileExpr (e, env, fenv) + 
            "\tpopq\t%rax\n" +
            "\tmovq\t%%rax, %s\n".format (lab)
        }
      }
      case If (e, s1, s2)          => 
        val label1 = newLabel ()
        val label2 = newLabel ()
        val label3 = newLabel ()
        "// %s\n".format (ppExpr (e)) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" + 
        "\ttestq\t%rax, %rax\n" + 
        "\tjne\t%s\n".format (label1) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s1, env, fenv) +
        "\tjmp\t%s\n".format (label3) +
        "%s:\n".format (label2) +
        compileStmt (s2, env, fenv) +
        "%s:\n".format (label3) 
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : String = {
          ss2 match {
            case Nil       => ""
            case s2 :: ss3 => compileStmt (s2, env, fenv) + loop (ss3)
          }
        }
        loop (ss)
      }
      case For (nm, low, high, s, nm2)  => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "// for %s = %s to %s\n".format (nm, ppExpr (low), ppExpr (high)) +
        compileExpr (low, env, fenv) + 
        "\tpopq\t%rax\n" + 
        "\tmovq\t%%rax, (%s)\n".format (nm) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "\tmovq\t(%s), %%rax\n".format (nm) +
        "\taddq\t$1, %rax\n" +
        "\tmovq\t%%rax, (%s)\n".format (nm) +
        "%s:\n".format (label2) +
        compileExpr (high, env, fenv) + 
        "\tpopq\t%rbx\n" + 
        "\tmovq\t(%s), %%rax\n".format (nm) +
        "\tcmpq\t%rbx, %rax\n" + 
        "\tjle\t%s\n".format (label1)
      }
      case While (e, s)            => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "// while (%s)\n".format (ppExpr (e)) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "%s:\n".format (label2) +
        compileExpr (e, env, fenv) + 
        "\tpopq\t%rax\n" + 
        "\ttestq\t%rax, %rax\n" + 
        "\tjne\t%s\n".format (label1)
      }
      case PrintExpr (e)               => {
        ppStmt ("// ", s) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rsi\n" +
        "\tmovl\t$.output, %edi\n" + 
        "\tmovl\t$0, %eax\n" +
        "\tcall\tprintf\n"
      }
	  case PrintStr(str)	=> {
		val label = newLabel()
		ppStmt ("// ", s) +
		"\t.section\t.rodata\n" +
		"%s:\n".format(label) +
		"\t.string\t\"%s\"\n".format(str) +
		"\t.text\n" +
		"\tmovl\t$%s, %%edi\n".format(label) +
		"\tcall\tputs\n"
	  }
      case Return (e)               => {
        ppStmt ("// ", s) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\tpopq\t%rbp\n" + 
        "\tret\n"
      }
    }
  }

  def findVarsExpr (e : Expr) : List[String] = {
    e match {
      case CstI (i)           => Nil
      case Var (x)            => List (x)
      case Prim (op, e1, e2)  => findVarsExpr (e1) ::: findVarsExpr (e2)
      case Call (nm, es)      => es.flatMap (findVarsExpr)
    }
  }

  def findVarsStmt (s : Stmt) : List[String] = {
    s match {
	  case Decl (nm)			   => List(nm)
      case Asgn (nm, e)            => nm :: findVarsExpr (e)
      case If (e, s1, s2)          => findVarsExpr (e) ::: findVarsStmt (s1) ::: findVarsStmt (s2)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : List[String] = {
          ss2 match {
            case Nil       => Nil
            case s2 :: ss3 => findVarsStmt (s2) ::: loop (ss3)
          }
        }
        loop (ss)
      }
      case For (nm, low, high, s, nm2)  => {
        nm :: findVarsExpr (low) ::: findVarsExpr (high) ::: findVarsStmt (s)
      }
      case While (e, s)            => {
        findVarsExpr (e) ::: findVarsStmt (s)
      }
      case PrintExpr (e)               => {
        findVarsExpr (e)
      }
	  case PrintStr(str) => {
		List()
	  }
      case Return (e)              => {
        findVarsExpr (e)
      }
    }
  }

  def findVars (s : Stmt) : List[String] = {
    findVarsStmt (s).toSet.toList.sortWith ((s1,s2) => s1 < s2)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Testing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // (* Example programs *)

  val ex1 : Stmt = {
    Block (
      List (
        Asgn (
          "sum", 
          CstI (0)),
        For (
          "i", 
          CstI (0), 
          CstI (100), 
          Asgn (
            "sum", 
            Prim ("+", Var ("sum"), Var ("i"))
          ),
		  "i"
		  ),
        PrintExpr (Var ("sum"))
      )
    )
  }

  val ex2 : Stmt = {
	Block (
	List (
    PrintStr ("Hello World!")
	)
	)
	}

  import fastparse.all.{Parsed,Parser}

  def readFile (filename : String) : String = {
    val source : scala.io.BufferedSource = io.Source.fromFile (filename)
    try source.getLines.mkString ("\n") finally source.close ()
  }

  def invokeAssemblerLinker (asmFilename : String) : Unit = {
    import scala.sys.process.{Process}
    val pb = Process (List ("gcc", "-o", asmFilename.replace (".s", ""), asmFilename))
    import scala.language.postfixOps
    val result : String = (pb !!)
    println ("Running assembler: %s".format (result))
  }

  def compile (prog : Program, filename: String) : Unit = {
    val fenv : FuncEnv = (for (fd <- prog.funs) yield (fd.nm, fd)).toMap
    val vars : List[String] = for (stmt <- (prog.main :: prog.funs.map (f => f.body)); v <- findVars (stmt)) yield v
    val env : Env = (for (v <- vars) yield (v, "(%s)".format (v))).toMap
    println ("Variables: %s".format (env.mkString (", ")))
    println ("Compiling:")
    val asm : String = compileAll (prog, env, fenv)
    val asmFilename = filename.replace (".bas", ".s")
    val fw = new java.io.FileWriter (asmFilename)
    fw.write (asm)
    fw.close
    println ("Wrote to %s".format (asmFilename))
    //invokeAssemblerLinker (asmFilename)
    // println (asm)
  }

  def test (p : Parser[Program], filename : String) : Unit = {
    val input : String = readFile (filename)
    val result : fastparse.core.Parsed[Program, Char, String] = p.parse (input) 
    result match {
      case Parsed.Success (prog, successIndex) => {
        println ("Successfully parsed file \"%s\".\nResult is %s.\nIndex is %d.".format (filename, prog, successIndex))
         println ("Pretty printing:")
        // print (ppStmt ("  ", stmt))
        compile (prog, filename)
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse file \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (filename, lastParser, index, extra))
      }
    }
  }


  def main (args : Array[String]) {
    println ("=" * 80)
    
    import java.io.File
    //for (f <- new File ("./ch03/examples").listFiles.toList.sortWith ((f1, f2) => f1.getName < f2.getName);
      //   if (f.getName.endsWith (".bas"))) {
      test (MyParsers.start, "src/main/scala/csp/ch03/input/onetohundred.bas")
      println ("=" * 80)
	  test (MyParsers.start, "src/main/scala/csp/ch03/input/factorial.bas")
	  println ("=" * 80)
	  test (MyParsers.start, "src/main/scala/csp/ch03/input/mod.bas")
	  println ("=" * 80)
	  test (MyParsers.start, "src/main/scala/csp/ch03/input/helloworld.bas")
	  println ("=" * 80)
    //}
  }
}
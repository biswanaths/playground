import scala.util.parsing.combinator._

trait Expr
case class Prop(value:Char) extends Expr
case class Not(expr:Expr) extends Expr
case class And(left:Expr, right:Expr) extends Expr
case class Or(left:Expr, right:Expr) extends Expr
case class Paren(value:Expr) extends Expr

object  IsTautology extends RegexParsers { 

    def prop: Parser[Expr] = 
        """[a-z]""".r ^^ { x => Prop(x.charAt(0)) } |
        "(" ~> expr <~ ")"                          

    def elem: Parser[Expr] = "!" ~> elem ^^ { Not(_) } | prop 

    def expr: Parser[Expr] = 
        (elem <~ "&" ) ~ elem ^^ { case x ~ y => And(x,y) } |
        (elem <~ "|" ) ~ elem ^^ { case x ~ y => Or (x,y) } |
        elem

    def parse(s:String) = parseAll(expr,s) match { 
        case Success(result,_) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def variables(e:Expr):Set[Char] = e match { 
        case Prop (x)   => Set(x)
        case Paren(x)   => variables(x)
        case Not  (x)   => variables(x)
        case And  (x,y) => (variables(x) ++ variables(y))
        case Or   (x,y) => (variables(x) ++ variables(y))
    }

    def evaluate(e:Expr,truths:Set[Char]):Boolean = e match { 
        case Prop (x)   => truths.contains(x)
        case Paren(x)   => evaluate(x,truths)
        case Not  (x)   => !evaluate(x,truths)
        case And  (x,y) => evaluate(x,truths) & evaluate(y,truths) 
        case Or   (x,y) => evaluate(x,truths) | evaluate(y,truths)
    }

    def apply(s:String): Boolean =  { 
        val tree = parse(s)
        variables(tree).subsets
            .map((x:Set[Char]) => evaluate(tree,x))
            .fold(true)((ans,curr) => ans & curr ) 
    }
}

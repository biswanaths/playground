import scala.util.parsing.combinator._

trait Node 
case class Empty extends Node
case class PropsValue(value:String) extends Node
case class Props(propsValue:Node) extends Node
case class Names(value:String) extends Node
case class CssClass(name:Node, prop:Node) extends Node
case class AllClasses(name:Node, prop:Node) extends Node

//case class Prop(value:Char) extends Expr
//case class Not(expr:Expr) extends Expr
//case class And(left:Expr, right:Expr) extends Expr
//case class Or(left:Expr, right:Expr) extends Expr
//case class Paren(value:Expr) extends Expr

object  CssParse extends RegexParsers { 

    def propsValue: Parser[Node] = 
        """[^\}]*""".r ^^ { x => PropsValue(x) }

    def props: Parser[Node] = 
        "{" ~> propsValue <~ "}" ^^ { case x => Props(x) }

    def names : Parser[Node] = 
        """[^\{]*""".r ^^ { x=> Names(x) }

    def cssClass: Parser[Node] =  
        names ~ props ^^ { case x ~ y => CssClass(x,y) }

    def allClasses: Parser[Node] = 
        cssClass ~  allClasses ^^ { case x ~ y => AllClasses(x,y) } | 
        cssClass ^^ { case x => AllClasses(x,Empty()) }  

    def parse(s:String) = parseAll(allClasses,s) match { 
        case Success(result,_) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def apply(s:String): Node =  { 
        parse(s)
    }
}

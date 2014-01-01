/*
 SXLexer: A lexer for the programming language Scheme

 Author: Matthew Might
 Site:   http://matt.might.net/

 */
//it Token.scala
//import LiveStream.scala
import RegularLanguageImplicits._

class SXLexer extends NonblockingLexer[Char, Token] {
  
  //import RegularLanguageImplicits._

  implicit def charsToString(l : List[Char]) : String = l mkString 

  // Abbreviations:
  private val ch = "#\\" ~ AnyChar
  private val id = (('A' thru 'Z') || ('a' thru 'z') || ('0' thru '9') || oneOf("-+/*_?%$&^=!@<>:"))+
  private val int = ("-"?) ~ ('0' thru '9')+
  private val ws = oneOf(" \r\t\n")+ // whitespace
  private val com = ";" ~ ((!oneOf("\r\n"))*) // single-line comment

  // States:
  protected val MAIN      = State()
  private val BANGCOMMENT = State(0)
  private val STRING      = State[List[Char]](List())

  // Rules:

  // State switching rules
  MAIN switchesOn ("#!") to { BANGCOMMENT(1) }
  MAIN switchesOn ("\"") to { STRING(List()) }

  // Regular tokens
  MAIN (",@")  { emit(PunctToken("comma-unsplice")) }
  MAIN ("(")   { emit(PunctToken("left-parenthesis")) }
  MAIN (")")   { emit(PunctToken("right-parenthesis")) } 
  MAIN ("[")   { emit(PunctToken("left-Bracket")) }
  MAIN ("]")   { emit(PunctToken("Right-Bracket")) } 
  MAIN ("#t")  { emit(BooleanToken(true)) }
  MAIN ("#f")  { emit(BooleanToken(false)) }
  MAIN("`")     {emit(PunctToken("backtick"))}
  MAIN("'")     {emit(PunctToken("tick"))}
  MAIN(",")     {emit(PunctToken("comma"))}
  MAIN("\"")   {emit(PunctToken("doublequote"))}
  MAIN (END)   {terminate() }
  MAIN (ws)     { }
  MAIN (com)    { }
  MAIN (ch)    over { chars => emit(CharToken(chars(2))) }
  MAIN (int)   over { chars => emit(IntToken(Integer.parseInt(chars))) }
  MAIN (id)    over { chars => emit(SymbolToken(chars)) }
  MAIN("#\\newline") {emit(LongCharToken("newline"))}
  MAIN("#\\tab")     {emit(LongCharToken("tab"))}
  MAIN("#\\space")   {emit(LongCharToken("space"))}
  MAIN("#\\,")        {emit(LongCharToken("comma"))}
  // Strings
  STRING ("\"")    = { (string,_)     => { emit(StringToken(string.reverse.mkString)) ; MAIN } }
  STRING ("\\\"")  = { (string,chars) => STRING('"' :: string) }
  STRING ("\\n")   = { (string,chars) => STRING('\n' :: string) }
  STRING ("\\\\")  = { (string,chars) => STRING('\\' :: string) }
  STRING (AnyChar) = { (string,chars) => STRING(chars.reverse ++ string)} 
  
  // #! ... !# comments
  BANGCOMMENT ("#!")    = { (n,chars) => BANGCOMMENT(n+1) }
  BANGCOMMENT (AnyChar)   { }
  BANGCOMMENT ("!#")    = { case (1,chars) => MAIN 
                            case (n,chars) => BANGCOMMENT(n-1) }

}


/**
 Punctuation tokens.
 */
case class PunctToken(s : String) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case PunctToken(thatS) => s compare thatS
  }

  val tag = s

  override lazy val hashCode = s.hashCode
  override lazy val toString = "(" + s + ")"
}

/**
 Symbol tokens.
 */
case class SymbolToken(s : String) extends Token {
  def isParsingMarker = false 

   def stringconvert (s:String) :String = {
      
      var t = ""
      for(c <- s){
        

        t += "(char" + " " + c + ")"
      }
      return "("+t+")"
   }
  protected def localCompare(that : Token) = that match {
    case SymbolToken(thatS) => s compare thatS
  }

  val tag = "Symbol"

  override lazy val hashCode = s.hashCode
  override lazy val toString = "(symbol" +  stringconvert(s) + ")" 
}
   


/**
 String literal tokens.
 */
case class StringToken(s : String) extends Token {
  def isParsingMarker = false 
    var t = " "
    def convert(d :String ):String = {
      for(c <-  d){
         /*if (c == ' ')
           t = "(char space)"

         else
           t += "(char" + " " + c + ")" */

        t += (c match {
          case ' ' => "(char space)"
          case '\n' => "(char newline)"
          case '\t' => "(char tab)"

          case _ => "(char" + " " + c + ")"
        })
      }
       return  " (" + t + ")"
    } 

  protected def localCompare(that : Token) = that match {
    case StringToken(thatS) => s compare thatS
  }

  val tag = "String"

  override lazy val hashCode = s.hashCode
  override lazy val toString = "(text" + convert(s) + ")"
}

/**
 Integer tokens.
 */
case class IntToken(n : Int) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case IntToken(thatN) => this.n compare thatN
  }

  val tag = "Int"

  override lazy val hashCode = n.hashCode
  override lazy val toString = "(integer" +" " + n.toString + ")" 
}

/**
 Boolean literal tokens.
 */
case class BooleanToken(b : Boolean) extends Token {
  def isParsingMarker = false 

  protected def localCompare(that : Token) = that match {
    case BooleanToken(thatB) => this.b compare thatB
  }

  val tag = "Boolean"

  override lazy val hashCode = b.hashCode
  override lazy val toString = if (b) "(boolean true)" else "(boolean false)"
}

/**
 Character tokens.
 */
case class CharToken(c : Char) extends Token {
  def isParsingMarker = false 

   
  protected def localCompare(that : Token) = that match {
    case CharToken(thatC) => this.c compare thatC
  }

  val tag = "Char"

  override lazy val hashCode = c.hashCode
  override lazy val toString = "(char" + " " + c + ")"
}

 
/**
 Long Character tokens.
 */
case class LongCharToken(s : String) extends Token {
  def isParsingMarker = false 
   
  protected def localCompare(that : Token) = that match {
    case LongCharToken(thatC) => this.s compare thatC
  }

  val tag = "Char"

  override lazy val hashCode = s.hashCode
  override lazy val toString = "(char" + " " + s + ")"
}



/**
 A simple program for testing the s-expression lexer.
 */
private object SXLexerDoit{
  def main (args : Array[String]) {
    val in = scala.io.Source.stdin
    val sourcestring =  in.getLines mkString "\n"
    val instream = LiveStream(sourcestring)
    val lexer = new SXLexer
    in.close()
    lexer.lex(instream)
    println(lexer.output) 
  }
}


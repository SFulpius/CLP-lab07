package amyc
package parsing

import utils._
import scala.io.Source
import java.io.File
import scala.annotation.tailrec

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a stream of (Char, Position),
// then uses a functional approach to consume the stream.
object Lexer extends Pipeline[List[File], Stream[Token]] {
  import Tokens._

  /**
   * Maps a string s to the corresponding keyword,
   * or None if it corresponds to no keyword
   */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    case "Int"      => Some(INT())
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
    case _          => None
  }

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {
    import ctx.reporter._

    // Special character which represents the end of an input file
    val EndOfFile: Char = scala.Char.MaxValue
    def isEndOfLine(c: Character): Boolean = c == '\n' || c == '\r'

    val source = Source.fromFile(f)

    // Useful type alias:
    // The input to the lexer will be a stream of characters,
    // along with their positions in the files
    type Input = (Char, Position)

    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

    /**
     * Gets rid of whitespaces and comments and calls readToken to get the next token.
     * Returns the first token and the remaining input that did not get consumed
     */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      if (Character.isWhitespace(currentChar)) {
        nextToken(stream.dropWhile { case (c, _) => Character.isWhitespace(c) })
      } else if (currentChar == '/' && nextChar == '/') {
        // Single-line comment
        nextToken(stream.dropWhile { case (c, _) => !isEndOfLine(c) && c != EndOfFile })
      } else if (currentChar == '/' && nextChar == '*') {
        // Multi-line comment
        var s = rest.tail
        do {
          val st = s.dropWhile { case (c, _) => c != '*' }
          s = if (st.nonEmpty) st.tail else st
        } while (s.nonEmpty && s.head._1 != '/')

        if (s.nonEmpty) nextToken(s.tail)
        else {
          ctx.reporter.error("multilines comment not closed", currentPos)
          (BAD().setPos(currentPos), s)
        }
      } else {
        readToken(stream)
      }
    }

    /**
     * Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
     * Returns the first token and the remaining input that did not get consumed.
     */
    def readToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token) = (t.setPos(currentPos), rest)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token) = (t.setPos(currentPos), rest.tail)

      currentChar match {
        case `EndOfFile` => useOne(EOF())

        // Reserved word or Identifier
        case _ if Character.isLetter(currentChar) =>
          val (wordLetters, afterWord) = stream.span {
            case (ch, _) =>
              Character.isLetterOrDigit(ch) || ch == '_'
          }
          val word = wordLetters.map(_._1).mkString

          // Hint: Decide if it's a letter or reserved word (use our infrastructure!),
          // and return the correct token, along with the remaining input stream.
          // Make sure you set the correct position for the token.
          val token = keywords(word) match {
            case Some(t) => t
            case None    => ID(word)
          }
          (token.setPos(currentPos), afterWord)

        // Int literal
        case _ if Character.isDigit(currentChar) =>
          // Hint: Use a strategy similar to the previous example.
          // Make sure you fail for integers that do not fit 32 bits.
          val (wordDigit, afterWord) = stream.span {
            case (c, _) =>
              Character.isDigit(c)
          }

          val stringDigit = wordDigit.map(_._1).mkString
          val token: Token = try {
            INTLIT(stringDigit.toInt)
          } catch {
            case e: Throwable =>
              ctx.reporter.error(stringDigit + "is too big", currentPos)
              BAD()
          }

          (token.setPos(currentPos), afterWord)

        // String literal
        case '"' =>
          val (wordString, afterWord) = rest.span {
            case (c, _) =>
              c != '"' && c != EndOfFile && !isEndOfLine(c)
          }
          if (afterWord.nonEmpty && afterWord.head._1 != EndOfFile && !isEndOfLine(afterWord.head._1))
            (STRINGLIT(wordString.map(_._1).mkString).setPos(currentPos), afterWord.tail)
          else { //no string terminator
            ctx.reporter.error("The string " + wordString.map(_._1).mkString + "is an invalid string", currentPos)
            (BAD().setPos(currentPos), afterWord)
          }
        //Operators
        case ';' => useOne(SEMICOLON())
        case '+' => if (nextChar == '+') useTwo(CONCAT()) else useOne(PLUS())
        case '-' => useOne(MINUS())
        case '*' => useOne(TIMES())
        case '/' => useOne(DIV())
        case '%' => useOne(MOD())
        case '<' => if (nextChar == '=') useTwo(LESSEQUALS()) else useOne(LESSTHAN())
        case '&' => if (nextChar == '&') useTwo(AND()) else {
          ctx.reporter.error("& is invalid character", currentPos)
          useOne(BAD())
        }
        case '|' => if (nextChar == '|') useTwo(OR()) else {
          ctx.reporter.error("| is invalid character", currentPos)
          useOne(BAD())
        }
        case '=' => if (nextChar == '=') useTwo(EQUALS())
        else if (nextChar == '>') useTwo(RARROW())
        else useOne(EQSIGN())
        case '!' => useOne(BANG())
        //Delimiters and wildcard
        case '{' => useOne(LBRACE())
        case '}' => useOne(RBRACE())
        case '(' => useOne(LPAREN())
        case ')' => useOne(RPAREN())
        case ',' => useOne(COMMA())
        case ':' => useOne(COLON())
        case '.' => useOne(DOT())
        case '_' => useOne(UNDERSCORE())
        case '[' => useOne(LBRACKET())
        case ']' => useOne(RBRACKET())
        case _ =>
          ctx.reporter.error(currentChar + "is a invalid character", currentPos)
          useOne(BAD())
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input]): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest) = nextToken(s)
        token #:: tokenStream(rest)
      }
    }

    tokenStream(inputStream)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): Stream[Token] = {
    files.toStream flatMap lexFile(ctx)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}

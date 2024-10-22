package domain.lexer

import scala.annotation.tailrec


sealed trait Token
object Token {
  case object Dollar extends Token
  case object Asterisk extends Token
  case object Plus extends Token
  case object Colon extends Token
  case object Hyphen extends Token
  case class Number(value: Int) extends Token
  case class Text(value: String) extends Token
  case object NewLine extends Token
  case class UnrecognisedChar(char: Char) extends Token
}

object Tokenizer {

  @tailrec
  private def tokenizeRec(
    chars: List[Char],
    currentToken: Option[Token] = None,
    acc: List[Token] = Nil
  ): List[Token] = {
    chars match {
      case Nil => currentToken.fold(acc)(t => t :: acc).reverse
      case '$' :: tail => commitToken(tail, Token.Dollar, currentToken, acc)
      case '*' :: tail => commitToken(tail, Token.Asterisk, currentToken, acc)
      case '+' :: tail => commitToken(tail, Token.Plus, currentToken, acc)
      case ':' :: tail => commitToken(tail, Token.Colon, currentToken, acc)
      case '-' :: tail => commitToken(tail, Token.Hyphen, currentToken, acc)
      case '\r' :: '\n' :: tail => commitToken(tail, Token.NewLine, currentToken, acc)
      case c :: tail if c.isDigit =>
        currentToken match {
          case Some(Token.Number(value)) => tokenizeRec(tail, Some(Token.Number((value * 10) + c.asDigit)), acc)
          case Some(_) => commitToken(tail, Token.Number(c.asDigit), currentToken, acc)
          case None => tokenizeRec(tail, Some(Token.Number(c.asDigit)), acc)
        }
      case c :: tail if c.isLetter || c.isWhitespace =>
        currentToken match {
          case Some(Token.Text(value)) => tokenizeRec(tail, Some(Token.Text(s"$value$c")), acc)
          case Some(_) => commitToken(tail, Token.Text(s"$c"), currentToken, acc)
          case None => tokenizeRec(tail, Some(Token.Text(s"$c")), acc)
        }
      case unknown :: tail => commitToken(tail, Token.UnrecognisedChar(unknown), currentToken, acc)
    }
  }

  private def commitToken(
    chars: List[Char],
    newToken: Token,
    currentToken: Option[Token],
    acc: List[Token]
  ) : List[Token] = {
    val newAcc = currentToken.fold(acc)(_ :: acc)
    tokenizeRec(chars, Some(newToken), newAcc)
  }


  def tokenize(input: String): List[Token] = {
    val chars = input.toList
    tokenizeRec(chars: List[Char])
  }
}

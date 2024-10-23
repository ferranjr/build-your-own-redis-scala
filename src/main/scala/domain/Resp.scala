package domain

import domain.lexer.{Token, Tokenizer}

sealed trait Resp {
  def serialize: String = this match {
    case Resp.SimpleString(value) => s"+$value\r\n"
    case Resp.Error(value) => s"-$value\r\n"
    case Resp.Integer(value) => s":$value\r\n"
    case Resp.BulkString(value) => s"$$\r\n${value.length}$value\r\n"
    case Resp.Arrays(values) => s"*${values.length}\r\n${values.mkString("\r\n")}"
    case Resp.Null => "$-1\r\n"
  }
}

object Resp {
  case class SimpleString(value: String) extends Resp
  case class Error(value: String) extends Resp
  case class Integer(value: Int) extends Resp
  case class BulkString(value: String) extends Resp
  case class Arrays(values: List[Resp]) extends Resp
  case object Null extends Resp
}

sealed trait RespError
object RespError {
  case object UnexpectedEndOfTokens extends RespError
  case object WrongSizedBulkString extends RespError
  case object WrongSizedArray extends RespError
  case object UnsupportedCombination extends RespError
  case class UnexpectedToken(token: Token) extends RespError
}

object Parser {
  type ParserResult[A] = Either[RespError, A]

  private def parseArray(
    tokens: List[Token],
    arraySize: Int,
    acc: List[Resp]
  ): ParserResult[Resp] =
    tokens match {
      case Nil if arraySize == 0 => Right(Resp.Arrays(acc.reverse))
      case Nil if arraySize != 0 => Left(RespError.WrongSizedArray)
      case Token.Dollar :: Token.Number(size) :: Token.NewLine :: tail =>
        parseBulkString(tail, size).flatMap { case (bulkString, after) =>
          parseArray(after, arraySize - 1, bulkString :: acc)
        }
    }

  private def parseBulkString(
    tokens: List[Token],
    size: Int
  ): ParserResult[(Resp, List[Token])] =
    tokens match {
      case Token.Text(value) :: Token.NewLine :: tail =>
        if(value.length != size) Left(RespError.WrongSizedBulkString)
        else Right((Resp.BulkString(value), tail))
      case Token.NewLine :: tail =>
        if(size != 0) Left(RespError.WrongSizedBulkString)
        else Right((Resp.BulkString(""), tail))
    }

  private def parseNull(tokens: List[Token]): ParserResult[Resp] =
    tokens match {
      case Nil => Left(RespError.UnexpectedEndOfTokens)
      case Token.NewLine :: Nil => Right(Resp.Null)
    }

  private def parseSimpleString(tokens: List[Token]): ParserResult[Resp] =
    tokens match {
      case Token.Text(value) :: Token.NewLine :: Nil => Right(Resp.SimpleString(value))
      case Token.NewLine :: Nil => Right(Resp.SimpleString(""))
      case Token.Text(_) :: Nil => Left(RespError.UnexpectedEndOfTokens)
      case token :: _ => Left(RespError.UnexpectedToken(token))
      case Nil => Left(RespError.UnexpectedEndOfTokens)
    }

  private def parseError(tokens: List[Token]): ParserResult[Resp] =
    tokens match {
      case Token.Text(value) :: Token.NewLine :: Nil => Right(Resp.Error(value))
      case Token.Text(_) :: Nil | Nil=> Left(RespError.UnexpectedEndOfTokens)
      case token :: _ => Left(RespError.UnexpectedToken(token))
    }

  private def parseNumber(tokens: List[Token]): ParserResult[Resp] =
    tokens match {
      case Token.Number(value) :: Token.NewLine :: Nil => Right(Resp.Integer(value))
      case Token.Number(_) :: Nil | Nil => Left(RespError.UnexpectedEndOfTokens)
      case token :: _ => Left(RespError.UnexpectedToken(token))
    }

  private def parseTokens(tokens: List[Token]): ParserResult[Resp] =
    tokens match {
      case Nil => Left(RespError.UnexpectedEndOfTokens)
      case Token.Dollar :: Token.Hyphen :: Token.Number(1) :: tail => parseNull(tail)
      case Token.Asterisk :: Token.Hyphen :: Token.Number(1) :: tail => parseNull(tail)
      case Token.Plus :: tail => parseSimpleString(tail)
      case Token.Hyphen :: tail => parseError(tail)
      case Token.Colon :: tail => parseNumber(tail)
      case Token.Asterisk :: Token.Number(size) :: Token.NewLine :: tail => parseArray(tail, size, Nil)
      case Token.Dollar :: Token.Number(size) :: Token.NewLine :: tail => parseBulkString(tail, size).map(_._1)
      case _ => Left(RespError.UnsupportedCombination)
    }

  def parse(input: String): ParserResult[Resp] =
    parseTokens(Tokenizer.tokenize(input))
}
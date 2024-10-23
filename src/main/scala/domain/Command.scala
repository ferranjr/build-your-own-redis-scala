package domain

import domain.ParserCommandError.UnsupportedCommand

sealed trait Command

object Command {
  case object Ping extends Command
  case class Echo(text: String) extends Command
  case class Set(key: String, value: String) extends Command
  case class Get(key: String) extends Command
}


sealed trait ParserCommandError
object ParserCommandError {
  case class UnsupportedCommand(resp: Resp) extends ParserCommandError
}

object ParserCommand {
  def parserInput: Resp => Either[ParserCommandError, Command] = {
    case Resp.Arrays(Resp.BulkString(cmd) :: Nil)
      if cmd.toUpperCase() == "PING" =>
      Right(Command.Ping)
    case Resp.Arrays(Resp.BulkString(cmd) :: Resp.BulkString(text) :: Nil)
      if cmd.toUpperCase() == "ECHO" =>
      Right(Command.Echo(text))
    case Resp.Arrays(Resp.BulkString(cmd) :: Resp.BulkString(key) :: Resp.BulkString(value) :: Nil)
      if cmd.toUpperCase() == "SET" =>
      Right(Command.Set(key, value))
    case Resp.Arrays(Resp.BulkString(cmd) :: Resp.BulkString(key) :: Nil)
      if cmd.toUpperCase() == "GET" =>
      Right(Command.Get(key))
    case other => Left(UnsupportedCommand(other))
  }
}
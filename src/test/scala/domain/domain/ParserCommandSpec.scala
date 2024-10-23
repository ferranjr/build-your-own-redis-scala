package domain.domain

import domain.{Command, ParserCommand, ParserCommandError, Resp}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserCommandSpec
  extends AnyFlatSpec
    with Matchers
    with EitherValues {

  behavior of "ParserCommand"

  it should "parse command ECHO as expected" in {
    val resp = Resp.Arrays(List(
      Resp.BulkString("ECHO"),
      Resp.BulkString("Hello World!")
    ))
    val result = ParserCommand.parserInput(resp).value
    result shouldBe Command.Echo("Hello World!")
  }

  it should "parse command PING as expected" in {
    val resp = Resp.Arrays(List(Resp.BulkString("PING")))
    val result = ParserCommand.parserInput(resp).value
    result shouldBe Command.Ping
  }

  it should "parse command SET as expected" in {
    val resp = Resp.Arrays(List(Resp.BulkString("SET"), Resp.BulkString("key"), Resp.BulkString("value")))
    val result = ParserCommand.parserInput(resp).value
    result shouldBe Command.Set("key", "value")
  }

  it should "parse command GET as expected" in {
    val resp = Resp.Arrays(List(Resp.BulkString("GET"), Resp.BulkString("key")))
    val result = ParserCommand.parserInput(resp).value
    result shouldBe Command.Get("key")
  }

  it should "fail parsing badly provided PING" in {
    val resp = Resp.Arrays(List(Resp.BulkString("PIN")))
    val error = ParserCommand.parserInput(resp).left.value
    error shouldBe ParserCommandError.UnsupportedCommand(resp)
  }

  it should "fail parsing badly provided ECHO" in {
    val resp = Resp.Arrays(List(Resp.BulkString("ECHO")))
    val error = ParserCommand.parserInput(resp).left.value
    error shouldBe ParserCommandError.UnsupportedCommand(resp)
  }

  it should "fail parsing badly provided SET" in {
    val resp = Resp.Arrays(List(Resp.BulkString("SET"), Resp.BulkString("key")))
    val error = ParserCommand.parserInput(resp).left.value
    error shouldBe ParserCommandError.UnsupportedCommand(resp)
  }

  it should "fail parsing badly provided GET" in {
    val resp = Resp.Arrays(List(Resp.BulkString("GET"), Resp.BulkString("key"), Resp.BulkString("value")))
    val error = ParserCommand.parserInput(resp).left.value
    error shouldBe ParserCommandError.UnsupportedCommand(resp)
  }
}

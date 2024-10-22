package domain

import org.scalatest.EitherValues
import org.scalatest.Inspectors.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RespSpec
  extends AnyFlatSpec
    with Matchers
    with EitherValues {

  "Parser" should "parse simple string successfully" in {
    val result = Parser.parse("+hello world\r\n").value
    result shouldBe Resp.SimpleString("hello world")
  }

  it should "parse null: `$-1\\r\\n`" in {
    val result = Parser.parse("$-1\r\n").value
    result shouldBe Resp.Null
  }

  it should "parse null: `*-1\\r\\n`" in {
    val result = Parser.parse("*-1\r\n").value
    result shouldBe Resp.Null
  }

  it should "parse error" in {
    val result = Parser.parse("-Error message\r\n").value
    result shouldBe Resp.Error("Error message")
  }

  it should "parse integer" in {
    val result = Parser.parse(":12345\r\n").value
    result shouldBe Resp.Integer(12345)
  }

  it should "parse bulk strings" in {
    val result = Parser.parse("$3\r\nget\r\n").value
    result shouldBe Resp.BulkStrings("get")
  }

  it should "parse empty bulk strings" in {
    val result = Parser.parse("$0\r\n\r\n").value
    result shouldBe Resp.BulkStrings("")
  }

  it should "parser array successfully" in {
    val result = Parser.parse("*2\r\n$4\r\necho\r\n$11\r\nhello world\r\n").value
    result shouldBe Resp.Arrays(List(
      Resp.BulkStrings("echo"),
      Resp.BulkStrings("hello world"),
    ))
  }

  it should "parser array successfully example 2" in {
    val result = Parser.parse("*1\r\n$4\r\nping\r\n").value
    result shouldBe Resp.Arrays(List(
      Resp.BulkStrings("ping"),
    ))
  }

  List(
    "$-1",
    "*-1",
    "-Error message",
    "-",
    "+Simple String",
    "+",
  ).foreach { input =>
    it should s"fail to parse if not ending with NewLine: `$input`" in {
      Parser.parse(input).left.value shouldBe RespError.UnexpectedEndOfTokens
    }
  }
}

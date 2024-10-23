package cache

import domain.{Command, Resp}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import runner.CommandRunner

class CommandRunnerSpec
  extends AnyFlatSpec
    with Matchers
    with EitherValues {

  private val storage = new InLocalMemoryStorage
  private val commandRunnerUnderTest = CommandRunner(storage)

  behavior of "CommandRunner"

  it should "answer PING with PONG" in {
    val result = commandRunnerUnderTest.run(Command.Ping).value
    result shouldBe Resp.SimpleString("PONG")
  }

  it should "respond `ECHO` with the text provided to echo" in {
    val result = commandRunnerUnderTest.run(Command.Echo("Hello World")).value
    result shouldBe Resp.SimpleString("Hello World")
  }

  it should "respond `OK` and store value when using SET" in {
    val result = commandRunnerUnderTest.run(Command.Set("foo", "barbaz")).value
    result shouldBe Resp.SimpleString("OK")
    storage.get("foo") shouldBe Some("barbaz")
  }

  it should "respond with value stored previously if using GET" in {
    commandRunnerUnderTest.run(Command.Set("foo", "bar"))
    val result = commandRunnerUnderTest.run(Command.Get("foo")).value
    result shouldBe Resp.SimpleString("bar")
  }

  it should "respond with null when not found key" in {
    val result = commandRunnerUnderTest.run(Command.Get("unknown")).value
    result shouldBe Resp.Null
  }
}

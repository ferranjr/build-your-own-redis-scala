package runner

import cache.Storage
import domain.{Command, Resp}

trait CommandRunner {
  def run(cmd: Command): Either[RuntimeException, Resp]
}

class CommandRunnerImpl(
  storage: Storage
) extends CommandRunner {

  override def run(cmd: Command): Either[RuntimeException, Resp] = cmd match {
    case Command.Ping => Right(Resp.SimpleString("PONG"))
    case Command.Echo(text) => Right(Resp.SimpleString(text))
    case Command.Set(key, value) =>
      storage.set(key, value)
      Right(Resp.SimpleString("OK"))
    case Command.Get(key) =>
      storage.get(key).fold[Either[RuntimeException, Resp]](Right(Resp.Null)){ value =>
        Right(Resp.SimpleString(value))
      }
  }
}

object CommandRunner {
  def apply(storage: Storage): CommandRunner = new CommandRunnerImpl(storage)
}
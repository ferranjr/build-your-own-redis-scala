import cache.InLocalMemoryStorage
import runner.CommandRunner
import server.RedisLite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object Main
  extends App {

  private val port = 6379
  private val timeout = 1.second

  private val storage = new InLocalMemoryStorage
  private val commandRunner = CommandRunner(storage)
  private val redisLite = new RedisLite(port, timeout, commandRunner)
  redisLite.start()
}

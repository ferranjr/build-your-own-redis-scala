package server

import com.typesafe.scalalogging.LazyLogging
import domain.{Parser, ParserCommand, Resp}
import runner.CommandRunner

import java.io.{BufferedInputStream, BufferedOutputStream, PrintStream}
import java.net.{ServerSocket, Socket}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException, blocking}
import scala.util.{Try, Using}

class RedisLite(
  port: Int,
  timeout: Duration,
  commandRunner: CommandRunner
)(implicit ec: ExecutionContext)
  extends LazyLogging {

  def start(): Unit = {
    val result = Using(new ServerSocket(port)) { serverSocket =>
      logger.info(s"Redis Lite Server listening on port $port")
      while (true) {
        Using(serverSocket.accept()) { socket =>
          logger.info("Connection accepted")
          val result = handleConnection(socket)
          Try(Await.result(result, timeout)).getOrElse(new TimeoutException)
        }
        logger.info("Client disconnected")
      }
    }

    result.fold(
      error => logger.error(s"Something went wrong: ${error.getMessage}", error),
      _ => logger.info(s"Cmd Executed successfully")
    )
  }

  private def handleConnection(socket: Socket): Future[Unit] = Future {
    blocking {
      Using(new BufferedInputStream((socket.getInputStream))) { is =>
        Using(new PrintStream(new BufferedOutputStream(socket.getOutputStream))) { os =>
          val buf = new Array[Byte](is.available())
          is.read(buf)
          (
            for {
              resp <- Parser.parse(new String(buf))
              cmd <- ParserCommand.parserInput(resp)
              result <- commandRunner.run(cmd)
            } yield result
          ).fold(
            error => os.println(Resp.Error(s"Error Parsing Command: $error").serialize),
            resp => os.println(resp.serialize)
          )

          os.flush()
        }
      }
    }
  }
}

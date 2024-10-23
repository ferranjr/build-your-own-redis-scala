package cache

import java.nio.charset.StandardCharsets


trait Storage {
  def set(key: String, value: String): Unit
  def get(key: String): Option[String]
}

class InLocalMemoryStorage
  extends Storage {
  private val myMap = collection.mutable.Map.empty[String, Array[Byte]]

  override def set(key: String, value: String): Unit = {
    synchronized {
      val encodedValue = value.getBytes(StandardCharsets.UTF_8)
      myMap(key) = encodedValue
    }
  }

  override def get(key: String): Option[String] = {
    synchronized {
      myMap.get(key).map { encodedValue =>
        new String(encodedValue, StandardCharsets.UTF_8)
      }
    }
  }
}
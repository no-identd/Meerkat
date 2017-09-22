package org.meerkat.parsers.neo4jgraph

import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import org.meerkat.util.{HttpClient, JsonUtil}



/**
  * Created by sofysmo on 24.03.17.
  */
object Example1 {
  def main(args: Array[String]): Unit = {
    case class Relationship(start: String, end: String, `type`: String)
    val url = "http://localhost:7474/db/data/node/1/relationships/out"
    val client = new HttpClient("neo4j","karamell111")
    implicit val formats = Serialization.formats(NoTypeHints)
    println(JsonUtil.fromJson[Seq[Relationship]](client.get(url)))
  }
}

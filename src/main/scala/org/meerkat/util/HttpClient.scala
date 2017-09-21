package org.meerkat.util

import java.net.URL
import java.nio.charset.StandardCharsets
import java.util.Base64

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import org.json4s.{Formats, NoTypeHints}
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.read

import scala.io.Source
import scala.reflect.ClassTag

import org.json4s._
import org.json4s.jackson.JsonMethods._

/**
  * Created by sofysmo on 24.03.17.
  */
class HttpClient(username: String, password: String) {

  def get(url: String): String = {
    println(url)
    val connection = new URL(url).openConnection
    connection.setRequestProperty(HttpBasicAuth.AUTHORIZATION, HttpBasicAuth.getHeader(username, password))
    connection.setRequestProperty("Accept", "application/json; charset=UTF-8")
    Source.fromInputStream(connection.getInputStream).mkString
  }

}

object HttpBasicAuth {
  val BASIC = "Basic"
  val AUTHORIZATION = "Authorization"

  def encodeCredentials(username: String, password: String): String = {
    Base64.getEncoder().encodeToString(
      s"$username:$password".getBytes(StandardCharsets.UTF_8))
  }

  def getHeader(username: String, password: String): String =
    BASIC + " " + encodeCredentials(username, password)
}

object JsonUtil {

  implicit val formats = Serialization.formats(NoTypeHints)

  def fromJson[T](json: String)(implicit formats: Formats, manifest: Manifest[T]): T = {
    read[T](json)
  }
}
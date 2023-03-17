package types

import com.google.gson.GsonBuilder

import scala.io.Source

case class ServiceConfig(
    liliumFeeAddress: String,
    liliumFeeNanoErg: Long,
    minerFeeNanoErg: Long,
    dataBaseURL: String
)

object serviceConfigHelper {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): ServiceConfig = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[ServiceConfig])
  }

  def readJsonString(jsonString: String): ServiceConfig = {
    gson
      .fromJson(jsonString, classOf[ServiceConfig])
  }

  def toJsonString(json: ServiceConfig): String = {
    this.gson.toJson(json)
  }

}

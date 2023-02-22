package configs

import com.google.gson.reflect.TypeToken
import com.google.gson.{Gson, GsonBuilder, TypeAdapter}
import com.google.gson.stream.JsonReader
import com.google.gson.stream.JsonWriter

import java.io.{FileWriter, Writer}
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.io.Source

class ListTypeAdapter extends TypeAdapter[List[Any]] {
  override def read(jsonReader: JsonReader): List[Any] = List.empty
  override def write(jsonWriter: JsonWriter, list: List[Any]): Unit =
    jsonWriter.nullValue()
}

case class ApiMessages(
    response: String
)

case class FrontendFile(
    transactionId: String,
    userPK: String,
    collectionDetails: Collection,
    nft: Array[Data]
)

case class CollectionEncoder(
    collectionInfo: String,
    socials: String
)

case class MasterMetadata(
    items: Data
)

case class Data(
    name: String,
    description: String,
    image: String,
    imageSHA256: String,
    dna: String,
    edition: Int,
    attributes: Array[Attribute],
    levels: Array[Level],
    stats: Array[Stats]
)
case class Attribute(trait_type: String, value: String)

case class Level(trait_type: String, max_value: Int, value: Int)

case class Stats(trait_type: String, max_value: Int, value: Int)

case class Collection(
    collectionInfo: CollectionInfo,
    socialMedia: java.util.Map[String, String],
    royalty: java.util.Map[String, Double],
    saleStartTimestamp: Long,
    saleEndTimestamp: Long,
    mintingExpiry: Long,
    collectionMaxSize: Long,
    priceOfNFTNanoErg: Long,
    returnCollectionTokensToArtist: Boolean
)

case class CollectionInfo(
    collectionName: String,
    collectionDescription: String,
    collectionLogoURL: String,
    collectionFeaturedImageURL: String,
    collectionBannerImageURL: String,
    collectionCategory: String
)

case class SocialMediaEntry(name: String, url: String)

//class metadata(
//    name: String,
//    description: String,
//    image: String,
//    imageSHA256: String,
//    dna: String,
//    edition: Int,
//    attributes: List[Attribute],
//    levels: mutable.Map[String, (Int, Int)],
//    stats: mutable.Map[String, (Int, Int)]
//) {
//  private val gson = new GsonBuilder()
//    .setPrettyPrinting()
//    .registerTypeAdapter(classOf[List[Any]], new ListTypeAdapter)
//    .create()
//  private val conf = MetaDataqw(
//    name,
//    description,
//    image,
//    imageSHA256,
//    dna,
//    edition,
//    attributes,
//    levels,
//    stats
//  )
//
//  def read(filePath: String): ServiceOwnerConfig = {
//    val jsonString: String = Source.fromFile(filePath).mkString
//    gson.fromJson(jsonString, classOf[Array[Map[String, Any]]])
//  }
//}

class apiResp(message: String) {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  private val newMessage: ApiMessages = ApiMessages(message)

  def toJsonString: String = {
    gson.toJson(newMessage)
  }

}

object masterMeta {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): Array[Data] = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson
      .fromJson(jsonString, classOf[Array[Data]])
  }

  def readJsonString(jsonString: String): Array[Data] = {
    gson
      .fromJson(jsonString, classOf[Array[Data]])
  }
}

object metadata {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): Data = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[Data])
  }
}

object collectionParser {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): Collection = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[Collection])
  }

  def readJsonString(jsonString: String): Collection = {
    gson
      .fromJson(jsonString, classOf[Collection])
  }

}

object frontendRespParser {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): FrontendFile = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[FrontendFile])
  }

  def readJsonString(jsonString: String): FrontendFile = {
    gson
      .fromJson(jsonString, classOf[FrontendFile])
  }

  def toJsonString(json: FrontendFile): String = {
    this.gson.toJson(json)
  }

}

object collectionEncoderHelper {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): CollectionEncoder = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[CollectionEncoder])
  }

  def readJsonString(jsonString: String): CollectionEncoder = {
    gson
      .fromJson(jsonString, classOf[CollectionEncoder])
  }

  def toJsonString(json: CollectionEncoder): String = {
    this.gson.toJson(json)
  }

}

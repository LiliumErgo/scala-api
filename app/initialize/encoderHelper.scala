package initialize

import configs.Collection
import utils.MetadataTranscoder
import java.util.{Map => JMap}
import scala.collection.JavaConverters._

import scala.collection.mutable

class encoderHelper(collectionFile: Collection) {
  private val metadataTranscoder = new MetadataTranscoder
  private val encoder = new metadataTranscoder.Encoder

  private def convertToMutableMap(
      jmap: JMap[String, String]
  ): mutable.LinkedHashMap[String, String] = {
    mutable.LinkedHashMap(jmap.asScala.toSeq: _*)
  }

  def encodeCollectionInfo: String = {

    val collectionInfo = Array(
      collectionFile.collectionInfo.collectionLogoURL,
      collectionFile.collectionInfo.collectionFeaturedImageURL,
      collectionFile.collectionInfo.collectionBannerImageURL,
      collectionFile.collectionInfo.collectionCategory
    )

    encoder.encodeCollectionInfo(collectionInfo).toHex
  }

  def encodeSocials: String = {

    encoder
      .encodeSocialMedaInfo(
        convertToMutableMap(collectionFile.socialMedia)
      )
      .toHex
  }

}

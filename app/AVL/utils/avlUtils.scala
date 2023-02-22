package AVL.utils

import AVL.IssuerBox.{IssuerHelpersAVL, IssuerValue}
import AVL.NFT.{IndexKey, IssuanceAVLHelpers, IssuanceValueAVL}
import configs.{AVLJsonHelper, AvlJson, Data}
import io.getblok.getblok_plasma.PlasmaParameters
import io.getblok.getblok_plasma.collections.{PlasmaMap, ProvenResult}
import sigmastate.AvlTreeFlags
import utils.MetadataTranscoder

import scala.collection.mutable

object avlUtils {
  private val metadataTranscoder = new MetadataTranscoder
  private val encoder = new metadataTranscoder.Encoder

  def prepareAVL(
      metadataFromJson: Array[Data],
      issuerTree: IssuerHelpersAVL,
      issuanceTree: IssuanceAVLHelpers
  ): Unit = {
    //create AVL trees
    var index = 0
    for (res: Data <- metadataFromJson) {
      val attributesMap = mutable.Map(
        res.attributes.map(a => a.trait_type -> a.value): _*
      )

      val levelsMap = mutable.Map(
        res.levels.map(a => a.trait_type -> (a.value, a.max_value)): _*
      )
      val statsMap = mutable.Map(
        res.stats.map(a => a.trait_type -> (a.value, a.max_value)): _*
      )

      val issuanceDataToInsert = IssuanceValueAVL.createMetadata(
        res.name,
        res.description,
        "picture",
        res.imageSHA256,
        res.image
      )

      val issuerDataToInsert = IssuerValue.createMetadata(
        encoder
          .encodeMetaData(
            attributesMap,
            levelsMap,
            statsMap
          )
          .getValue
      )

      val key = new IndexKey(index.toLong)

      issuanceTree.insertMetaData(key, issuanceDataToInsert)
      issuerTree.insertMetaData(key, issuerDataToInsert)

      index += 1
    }
  }

  def exportAVL[K, V](map: PlasmaMap[K, V]): AVLJsonHelper = {

    val manifest: io.getblok.getblok_plasma.collections.Manifest =
      map.getManifest(255)

    val manifestHex: String = manifest.toHexStrings._1

    val manifestDigestHex: String =
      manifest.digest.map("%02x".format(_)).mkString
    val manifestSubTreeHex: Seq[String] =
      manifest.toHexStrings._2

    new AVLJsonHelper(
      manifestHex,
      manifestDigestHex,
      manifestSubTreeHex.toArray
    )
  }

  def AVLFromExport[K, V](
      jsonData: AvlJson,
      map: PlasmaMap[K, V]
  ): Unit = {

    val manifest: io.getblok.getblok_plasma.collections.Manifest =
      io.getblok.getblok_plasma.collections.Manifest.fromHexStrings(
        jsonData.digestHex,
        jsonData.manifestHex,
        jsonData.subTreeHex.toSeq
      )

    map.loadManifest(manifest)

  }

}

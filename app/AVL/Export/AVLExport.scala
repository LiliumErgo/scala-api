package AVL.Export

import AVL.NFT.{IndexKey, IssuanceAVLHelpers, IssuanceValueAVL}
import configs.AVLJsonHelper
import io.getblok.getblok_plasma.PlasmaParameters
import io.getblok.getblok_plasma.collections.PlasmaMap
import sigmastate.AvlTreeFlags

object AVLExport {}

object AVLExportTester extends App {
  val issuanceTree = new IssuanceAVLHelpers

  val issuanceDataToInsert = IssuanceValueAVL.createMetadata(
    "name",
    "desc",
    "picture",
    "b207faec43d6120eba770ef9b375c72d1429dd50b031d4623dddd417f2c0bd7e",
    "link"
  )

  issuanceTree.insertMetaData(IndexKey(0), issuanceDataToInsert)

  val manifest: io.getblok.getblok_plasma.collections.Manifest =
    issuanceTree.getMap.getManifest(255)

  val maniBytes: String = manifest.toHexStrings._1
  val digestByes: String = manifest.digest.map("%02x".format(_)).mkString
  val subTreeBytes: Seq[String] = manifest.toHexStrings._2

  val helper = new AVLJsonHelper(maniBytes, digestByes, subTreeBytes.toArray)

  val json = helper.getJsonString

  println(json)

}

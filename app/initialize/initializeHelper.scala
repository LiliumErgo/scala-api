package initialize
import AVL.IssuerBox.IssuerHelpersAVL
import AVL.NFT.IssuanceAVLHelpers
import AVL.utils.avlUtils
import configs.{
  Collection,
  ContractsConfig,
  Data,
  collectionParser,
  conf,
  masterMeta,
  serviceOwnerConf
}
import contracts.{LiliumContracts}
import org.ergoplatform.appkit.{Address, BlockchainContext}
import utils.{
  Client,
  DatabaseAPI,
  DefaultNodeInfo,
  MetadataTranscoder,
  createCollection
}

import scala.collection.JavaConverters._
import scala.collection.mutable

object initializeHelper {

  def main(
      txFromArtist: String,
      userPK: String,
      collectionData: Collection,
      avlData: Array[Data]
  ): (Int, String) = {
    val client: Client = new Client()
    client.setClient
    val ctx: BlockchainContext = client.getContext
    val serviceFilePath = "serviceOwner.json"
    lazy val serviceConf = serviceOwnerConf.read(serviceFilePath)

    val liliumTxOperatorMnemonic: String =
      serviceConf.liliumTxOperatorMnemonic
    val liliumTxOperatorMnemonicPw: String =
      serviceConf.liliumTxOperatorMnemonicPw

    val contracts = LiliumContracts
    val collectionFromJson = collectionData
    val metadataTranscoder = new MetadataTranscoder
    val encoder = new metadataTranscoder.Encoder
    val decoder = new metadataTranscoder.Decoder

    val royaltyMap: mutable.Map[Address, Int] = mutable.Map()

    collectionFromJson.royalty.asScala.foreach { case (key, value: Double) =>
      royaltyMap += (Address.create(key) -> value.round.toInt)
    }

    val encodedRoyalty =
      encoder.encodeRoyalty(royaltyMap)

    val hashedRoyalty = decoder.hashRoyalty(encodedRoyalty.toHex)

    val issuanceTree = new IssuanceAVLHelpers
    val issuerTree = new IssuerHelpersAVL

    avlUtils.prepareAVL(avlData, issuerTree, issuanceTree)

    val res: ContractsConfig = createCollection.main(
      ctx,
      liliumTxOperatorMnemonic,
      liliumTxOperatorMnemonicPw,
      contracts.StateContract.contractScript,
      contracts.IssuerContract.contractScript,
      contracts.ProxyContract.contractScript,
      contracts.CollectionIssuance.contractScript,
      contracts.SingletonIssuance.contractScript,
      Address.create(userPK),
      hashedRoyalty,
      collectionFromJson.collectionInfo.collectionName,
      collectionFromJson.collectionInfo.collectionDescription,
      collectionFromJson.collectionMaxSize,
      collectionFromJson.saleStartTimestamp,
      collectionFromJson.saleEndTimestamp,
      collectionFromJson.returnCollectionTokensToArtist,
      issuanceTree.getMap,
      issuerTree.getMap,
      collectionFromJson.priceOfNFTNanoErg,
      Address.create(serviceConf.liliumFeeAddress),
      serviceConf.liliumFeeNanoErg,
      serviceConf.minTxOperatorFeeNanoErg,
      serviceConf.minerFeeNanoErg,
      txFromArtist
    )

    val statusCode: Int = DatabaseAPI.createArtistEntry(
      res.Contracts.stateContract.singleton,
      res.Contracts.collectionToken,
      collectionFromJson.saleStartTimestamp,
      collectionFromJson.saleEndTimestamp,
      userPK,
      avlUtils.exportAVL(issuerTree.getMap).getJsonString,
      avlUtils.exportAVL(issuanceTree.getMap).getJsonString,
      encodedRoyalty.toHex
    )

    println("Database Write Status: " + statusCode)
    (statusCode, conf.toJsonString(res))
  }

}

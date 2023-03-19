package initialize
import AVL.IssuerBox.IssuerHelpersAVL
import AVL.NFT.IssuanceAVLHelpers
import AVL.utils.avlUtils
import com.google.gson.Gson
import configs.{
  Collection,
  ContractsConfig,
  Data,
  apiResp,
  collectionParser,
  conf,
  masterMeta,
  serviceOwnerConf
}
import contracts.LiliumContracts
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import org.ergoplatform.appkit.{Address, BlockchainContext, Parameters}
import utils.{
  Client,
  CoinGekoAPIError,
  DatabaseAPI,
  DefaultNodeInfo,
  InvalidCollectionJsonFormat,
  InvalidCollectionSize,
  InvalidNftFee,
  MetadataTranscoder,
  createCollection
}

import scala.collection.JavaConverters._
import scala.collection.mutable

case class Ergo(usd: Double)
case class CoinGekoFormat(
    ergo: Ergo
)

object initializeHelper {

  def getERGUSD: Double = {
    try {
      val ERGUSD = new HttpGet(
        s"https://api.coingecko.com/api/v3/simple/price?ids=ergo&vs_currencies=USD"
      )
      val client = HttpClients.custom().build()
      val response = client.execute(ERGUSD)
      val resp = EntityUtils.toString(response.getEntity)
      val gson = new Gson()
      gson.fromJson(resp, classOf[CoinGekoFormat]).ergo.usd
    } catch {
      case e: Exception => throw new CoinGekoAPIError("api error")
    }
  }

  def calulateLiliumFee(amountNFTs: Long): Long = {
    val alpha = 54.10
    val beta = 0.03
    val feeUSD =
      math.floor(alpha * math.log((beta * amountNFTs) + 1)).asInstanceOf[Int]

    val ERGUSD = getERGUSD
    val feeNanoERGs = (BigDecimal(feeUSD / ERGUSD)
      .setScale(3, BigDecimal.RoundingMode.HALF_UP)
      .toDouble * Parameters.OneErg).toLong

    feeNanoERGs
  }

  def validateCollectionJson(collectionFromJson: Collection): Boolean = {
    collectionFromJson.collectionInfo.collectionName != null &&
    collectionFromJson.collectionInfo.collectionLogoURL != null &&
    collectionFromJson.collectionInfo.collectionFeaturedImageURL != null &&
    collectionFromJson.collectionInfo.collectionBannerImageURL != null &&
    collectionFromJson.collectionInfo.collectionCategory != null &&
    collectionFromJson.saleStartTimestamp != 0 &&
    collectionFromJson.saleEndTimestamp != 0 &&
    collectionFromJson.mintingExpiry != 0 &&
    collectionFromJson.collectionMaxSize != 0 &&
    collectionFromJson.priceOfNFTNanoErg != 0
  }

  def main(
      txFromArtist: String,
      userPK: String,
      collectionData: Collection,
      avlData: Array[Data]
  ): (Int, String) = {

    if (collectionData.priceOfNFTNanoErg < 100000000L) {
      println(collectionData.priceOfNFTNanoErg)
      throw new InvalidNftFee("invalid nft fee")
    }

    if (!validateCollectionJson(collectionData)) {
      throw new InvalidCollectionJsonFormat("invalid format")
    }

    if (avlData.length != collectionData.collectionMaxSize) {
      throw new InvalidCollectionSize("invalid collection size")
    }

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

    val royaltyMap: mutable.LinkedHashMap[Address, Int] =
      mutable.LinkedHashMap()

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
      serviceConf.liliumFeePercent,
      calulateLiliumFee(collectionFromJson.collectionMaxSize),
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

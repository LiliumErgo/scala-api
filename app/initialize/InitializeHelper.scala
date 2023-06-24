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
  InvalidAddress,
  InvalidCollectionJsonFormat,
  InvalidCollectionSize,
  InvalidMetadata,
  InvalidNftFee,
  InvalidPaymentToken,
  InvalidPremintSetting,
  InvalidRoyalty,
  InvalidTimeStamp,
  InvalidTotalFee,
  InvalidTransactionB64,
  InvalidWhitelistSetting,
  MetadataTranscoder,
  SpectrumAPI,
  createCollection,
  explorerApi
}

import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

case class Ergo(usd: Double)
case class CoinGekoFormat(
    ergo: Ergo
)

object InitializeHelper {

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

  def validPaymentToken(exp: explorerApi, tokenID: String): Boolean = {
    try {
      if (tokenID == "") {
        true
      } else if (exp.getTokenInfo(tokenID) == null) {
        false
      } else {
        true
      }
    } catch {
      case _: Exception => false
    }
  }

  def calulateLiliumFee(amountNFTs: Long, extraFeatures: Double): Long = {
    val alpha = 54.10
    val beta = 0.03
    val feeUSD =
      math
        .floor((alpha * math.log((beta * amountNFTs) + 1)) * extraFeatures)
        .asInstanceOf[Int]

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
    collectionFromJson.saleStartTimestamp >= 1 &&
    collectionFromJson.saleEndTimestamp != 0 &&
    collectionFromJson.mintingExpiry != 0 &&
    collectionFromJson.collectionMaxSize >= 1 &&
    collectionFromJson.priceOfNFTNanoErg != 0 &&
    collectionFromJson.paymentTokenID != null &&
    collectionFromJson.paymentTokenAmount > 0 &&
    collectionFromJson.premintTokenAmount != 0
  }

  def main(
      txFromArtist: String,
      userPK: String,
      collectionData: Collection,
      avlData: Array[Data]
  ): (Int, String) = {

    val transactionFromArtist: String = {
      try {
        val decoded: Array[Byte] =
          scorex.util.encode.Base64.decode(txFromArtist).get
        new String(decoded, StandardCharsets.UTF_8)
      } catch {
        case e: Exception =>
          throw new InvalidTransactionB64(
            "Transaction JSON must be serialized to Base 64"
          )
      }
    }

    if (
      collectionData.priceOfNFTNanoErg < 100000000L || Math.floor(
        collectionData.priceOfNFTNanoErg
      ) != collectionData.priceOfNFTNanoErg
    ) {
      throw new InvalidNftFee("invalid nft fee")
    }
    if (!validateCollectionJson(collectionData)) {
      throw new InvalidCollectionJsonFormat("invalid format")
    }

    if (avlData.length != collectionData.collectionMaxSize) {
      throw new InvalidCollectionSize(
        "invalid collection size specified, please sure the size matches the length in file"
      )
    }

    if (collectionData.amountLP > collectionData.collectionMaxSize) {
      throw new InvalidCollectionSize(
        "Amount LP cannot be greater than collection size"
      )
    }

    if (
      collectionData.saleEndTimestamp <= collectionData.saleStartTimestamp && collectionData.saleEndTimestamp != -1L
    ) {
      throw new InvalidTimeStamp(
        "End Timestamp needs to be greater than start Timestamp"
      )
    }

    if (collectionData.premintAccepted) {
      if (collectionData.premintTokenAmount < 1) {
        // throw error
        throw new InvalidPremintSetting("Token amount must at least be 1")
      }
    }

    if (collectionData.whitelistAccepted) {
      if (collectionData.whitelistTokenAmount < 1) {
        // throw error
        throw new InvalidWhitelistSetting("Token amount must at least be 1")
      }
    }

    if (collectionData.whitelistBypass && !collectionData.whitelistAccepted) {
      // throw error
      throw new InvalidWhitelistSetting(
        "Whitelist Bypass cannot be selected while whitelist is not accepted"
      )
    }

    if (!collectionData.ergAccepted) {
      if (!collectionData.paymentTokenAccepted) {
        throw new InvalidPaymentToken(
          "Payment token must be accepted if ERG is not"
        )
      }
    }

    val client: Client = new Client()
    client.setClient
    val ctx: BlockchainContext = client.getContext
    val serviceFilePath = "serviceOwner.json"
    lazy val serviceConf = serviceOwnerConf.read(serviceFilePath)

    val exp = new explorerApi(
      DefaultNodeInfo(ctx.getNetworkType).explorerUrl
    )

    val liliumTxOperatorMnemonic: String =
      serviceConf.liliumTxOperatorMnemonic
    val liliumTxOperatorMnemonicPw: String =
      serviceConf.liliumTxOperatorMnemonicPw

    val contracts = LiliumContracts
    val metadataTranscoder = new MetadataTranscoder
    val encoder = new metadataTranscoder.Encoder
    val decoder = new metadataTranscoder.Decoder

    val royaltyMap: mutable.LinkedHashMap[Address, Int] =
      mutable.LinkedHashMap()

    breakable {
      for (entry <- collectionData.royalty) {
        if (entry.address == "" || entry.amount == 0) {
          break
        }
        royaltyMap += (Address.create(
          entry.address
        ) -> entry.amount.round.toInt)
      }
    }

    val encodedRoyalty =
      encoder.encodeRoyalty(royaltyMap)

    val hashedRoyalty = decoder.hashRoyalty(encodedRoyalty.toHex)

    val issuanceTree = new IssuanceAVLHelpers
    val issuerTree = new IssuerHelpersAVL

    avlUtils.prepareAVL(avlData, issuerTree, issuanceTree)

    val priceOfNFTNanoErg: Long = {
      if (!collectionData.ergAccepted) {
        val ergPrice =
          SpectrumAPI.getERGPrice(collectionData.paymentTokenID)
        if (ergPrice < 0) {
          100000000
        } else {
          val price = (ergPrice * collectionData.paymentTokenAmount)
            .setScale(0, BigDecimal.RoundingMode.FLOOR)
            .toLong
          val nanoErgPrice = math.floor(price * math.pow(10, 9))
          if (nanoErgPrice < 100000000) 100000000 else nanoErgPrice.toLong
        }
      } else {
        collectionData.priceOfNFTNanoErg
      }
    }

    val validNanoErgPrice = Math.abs(
      priceOfNFTNanoErg - collectionData.priceOfNFTNanoErg
    ) <= (0.05 * collectionData.priceOfNFTNanoErg)

    if (!validNanoErgPrice) {
      throw new InvalidNftFee(
        "Mint Price is not properly set, try increasing it"
      )
    }

    if (!validPaymentToken(exp, collectionData.paymentTokenID)) {
      throw new InvalidPaymentToken("Invalid Payment Token")
    }

    val paymentTokenAmount = {
      if (!collectionData.paymentTokenAccepted) {
        -1L
      } else {
        math
          .floor(
            collectionData.paymentTokenAmount * math.pow(
              10,
              exp
                .getTokenInfo(collectionData.paymentTokenID)
                .getDecimals
                .toDouble
            )
          )
          .toLong
      }
    }

    val whitelistTokenAmount = {
      if (!collectionData.whitelistAccepted) {
        -1L
      } else {
        collectionData.whitelistTokenAmount
      }
    }

    val premintTokenAmount = {
      if (!collectionData.premintAccepted) {
        -1L
      } else {
        collectionData.premintTokenAmount
      }
    }

    val liliumFee = serviceConf.liliumFeePercent / 100.0
    val totalFees: Long = math
      .ceil(
        collectionData.amountLP * ((liliumFee * priceOfNFTNanoErg) + serviceConf.minBoxValueNanoErg + serviceConf.minerFeeNanoErg + serviceConf.minerFeeNanoErg + serviceConf.minTxOperatorFeeNanoErg)
      )
      .toLong

    val extraFeatures: Double = {
      val extraFeatureFee = serviceConf.extraFeaturePercent / 100.0
      var percent = 1.0
      percent += (if (collectionData.usePool) extraFeatureFee else 0.0)
      percent += (if (collectionData.whitelistAccepted) extraFeatureFee else 0.0)
      percent += (if (collectionData.premintAccepted) extraFeatureFee else 0.0)
      percent += (if (collectionData.paymentTokenAccepted) extraFeatureFee else 0.0)
      percent
    }

    val res: ContractsConfig = createCollection.main(
      ctx,
      liliumTxOperatorMnemonic,
      liliumTxOperatorMnemonicPw,
      contracts.StateContract.contractScript,
      contracts.IssuerContract.contractScript,
      contracts.ProxyContract.contractScript,
      contracts.CollectionIssuance.contractScript,
      contracts.SingletonIssuance.contractScript,
      contracts.CollectionIssuer.contractScript,
      contracts.SingletonIssuer.contractScript,
      contracts.PreMintIssuer.contractScript,
      contracts.WhitelistIssuer.contractScript,
      contracts.SaleLP.contractScript,
      Address.create(userPK),
      hashedRoyalty,
      collectionData.collectionInfo.collectionName,
      collectionData.collectionInfo.collectionDescription,
      collectionData.collectionMaxSize,
      collectionData.saleStartTimestamp,
      collectionData.saleEndTimestamp,
      collectionData.returnCollectionTokensToArtist,
      collectionData.whitelistAccepted,
      collectionData.whitelistBypass,
      collectionData.premintAccepted,
      collectionData.paymentTokenAccepted,
      collectionData.ergAccepted,
      whitelistTokenAmount,
      premintTokenAmount,
      collectionData.usePool,
      totalFees,
      collectionData.amountLP,
      issuanceTree.getMap,
      issuerTree.getMap,
      collectionData.priceOfNFTNanoErg,
      paymentTokenAmount,
      collectionData.paymentTokenID,
      Address.create(serviceConf.liliumFeeAddress),
      serviceConf.liliumFeePercent,
      calulateLiliumFee(collectionData.collectionMaxSize, extraFeatures),
      serviceConf.minTxOperatorFeeNanoErg,
      serviceConf.minerFeeNanoErg,
      serviceConf.minBoxValueNanoErg,
      transactionFromArtist
    )

    val statusCode: Int = DatabaseAPI.createArtistEntry(
      res.Contracts.stateContract.singleton.tokenID,
      res.Contracts.collectionToken.tokenID,
      collectionData.saleStartTimestamp,
      collectionData.saleEndTimestamp,
      userPK,
      avlUtils.exportAVL(issuerTree.getMap).getJsonString,
      avlUtils.exportAVL(issuanceTree.getMap).getJsonString,
      encodedRoyalty.toHex
    )

    println("Database Write Status: " + statusCode)
    (statusCode, conf.toJsonString(res))
  }

  def validate(
      txFromArtist: String,
      userPK: String,
      collectionData: Collection,
      avlData: Array[Data]
  ): Boolean = {

    val client: Client = new Client()
    client.setClient
    val ctx: BlockchainContext = client.getContext
    val serviceFilePath = "serviceOwner.json"

    val exp = new explorerApi(
      DefaultNodeInfo(ctx.getNetworkType).explorerUrl
    )

    if (
      collectionData.priceOfNFTNanoErg < 100000000L || Math.floor(
        collectionData.priceOfNFTNanoErg
      ) != collectionData.priceOfNFTNanoErg
    ) {
      throw new InvalidNftFee("invalid nft fee")
    }

    if (!validateCollectionJson(collectionData)) {
      throw new InvalidCollectionJsonFormat("invalid format")
    }

    if (avlData.length != collectionData.collectionMaxSize) {
      throw new InvalidCollectionSize(
        "invalid collection size specified, please sure the size matches the length in file"
      )
    }

    if (collectionData.amountLP > collectionData.collectionMaxSize) {
      throw new InvalidCollectionSize(
        "Amount LP cannot be greater than collection size"
      )
    }

    val metadataTranscoder = new MetadataTranscoder
    val encoder = new metadataTranscoder.Encoder
    val decoder = new metadataTranscoder.Decoder

    val royaltyMap: mutable.LinkedHashMap[Address, Int] =
      mutable.LinkedHashMap()

    try {

      breakable {
        for (entry <- collectionData.royalty) {

          if (entry.address == "" || entry.amount == 0) {
            break
          }
          royaltyMap += (Address.create(
            entry.address
          ) -> entry.amount.round.toInt)
        }
      }

      val encodedRoyalty =
        encoder.encodeRoyalty(royaltyMap)

      decoder.hashRoyalty(encodedRoyalty.toHex)
    } catch {
      case e: Exception =>
        throw new InvalidRoyalty("Invalid Royalty")
    }

    val issuanceTree = new IssuanceAVLHelpers
    val issuerTree = new IssuerHelpersAVL

    try {
      avlUtils.prepareAVL(avlData, issuerTree, issuanceTree)
    } catch {
      case e: Exception => throw new InvalidMetadata("Invalid Metadata")
    }

    try {
      Address.create(userPK)
    } catch {
      case e: Exception => throw new InvalidAddress("Invalid Address Format")
    }

    if (
      collectionData.saleEndTimestamp <= collectionData.saleStartTimestamp && collectionData.saleEndTimestamp != -1L
    ) {
      throw new InvalidTimeStamp(
        "End Timestamp needs to be greater than start Timestamp"
      )
    }

    if (collectionData.premintAccepted) {
      if (collectionData.premintTokenAmount < 1) {
        // throw error
        throw new InvalidPremintSetting("Token amount must at least be 1")
      }
    }

    if (collectionData.whitelistAccepted) {
      if (collectionData.whitelistTokenAmount < 1) {
        // throw error
        throw new InvalidWhitelistSetting("Token amount must at least be 1")
      }
    }

    if (collectionData.whitelistBypass && !collectionData.whitelistAccepted) {
      // throw error
      throw new InvalidWhitelistSetting(
        "Whitelist Bypass cannot be selected while whitelist is not accepted"
      )
    }

    if (!collectionData.ergAccepted) {
      if (!collectionData.paymentTokenAccepted) {
        throw new InvalidPaymentToken(
          "Payment token must be accepted if ERG is not"
        )
      }
    }

    if (!validPaymentToken(exp, collectionData.paymentTokenID)) {
      throw new InvalidPaymentToken("Invalid Payment Token")
    }

    val priceOfNFTNanoErg: Long = {
      if (!collectionData.ergAccepted) {
        val ergPrice =
          SpectrumAPI.getERGPrice(collectionData.paymentTokenID)
        if (ergPrice < 0) {
          100000000
        } else {
          val price = (ergPrice * collectionData.paymentTokenAmount)
            .setScale(0, BigDecimal.RoundingMode.FLOOR)
            .toLong
          val nanoErgPrice = math.floor(price * math.pow(10, 9))
          if (nanoErgPrice < 100000000) 100000000 else nanoErgPrice.toLong
        }
      } else {
        collectionData.priceOfNFTNanoErg
      }
    }

    val validNanoErgPrice = Math.abs(
      priceOfNFTNanoErg - collectionData.priceOfNFTNanoErg
    ) <= (0.05 * collectionData.priceOfNFTNanoErg)

    if (!validNanoErgPrice) {
      throw new InvalidNftFee(
        "Mint Price is not properly set, try increasing it"
      )
    }

    true
  }

}

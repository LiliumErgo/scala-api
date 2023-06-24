package utils

import AVL.IssuerBox.IssuerValue
import AVL.NFT.{IndexKey, IssuanceValueAVL}
import io.getblok.getblok_plasma.collections.{LocalPlasmaMap, PlasmaMap}
import org.ergoplatform.appkit.impl.{Eip4TokenBuilder, ErgoTreeContract}
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.scalaapi.ErgoValueBuilder
import sigmastate.eval.Colls
import java.util
import scala.collection.mutable.ListBuffer

class OutBoxes(ctx: BlockchainContext) {

  private def getAmount(amount: Double): Long = {
    (amount * Parameters.OneErg).toLong
  }
  private val txBuilder = this.ctx.newTxBuilder()
  private val minAmount = this.getAmount(0.001)
  def tokenHelper(
      inputBox: InputBox,
      name: String,
      description: String,
      tokenAmount: Long,
      tokenDecimals: Int
  ): Eip4Token = {
    new Eip4Token(
      inputBox.getId.toString,
      tokenAmount,
      name,
      description,
      tokenDecimals
    )
  }

  def collectionTokenHelper(
      inputBox: InputBox,
      name: String,
      description: String,
      tokenAmount: Long,
      tokenDecimals: Int
  ): Eip4Token = {

    Eip4TokenBuilder.buildNftArtworkCollectionToken(
      inputBox.getId.toString,
      tokenAmount,
      name,
      description,
      tokenDecimals
    )
  }

  def tokenOutBox(
      token: Eip4Token,
      receiver: Address,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .mintToken(token)
      .contract(
        new ErgoTreeContract(
          receiver.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def tokenSendOutBox(
      receiver: List[Address],
      amountList: List[Double],
      tokens: List[List[String]],
      amountTokens: List[List[Long]] = null
  ): List[OutBox] = {
    val outbx = new ListBuffer[OutBox]()
    var amountCounter = 0
    var tokenList1 = new ListBuffer[ListBuffer[ErgoToken]]()
    var tokenList2 = new util.ArrayList[ErgoToken]()
    var tokenAmountCounter = 0
    val tokenList = new ListBuffer[ErgoToken]()
    var tList = new ListBuffer[ErgoToken]()
    if (amountTokens == null) {
      for (token <- tokens) {
        for (x <- token) {
          val t: ErgoToken = new ErgoToken(x, 1)
          tokenList.append(t)
        }
      }
    } else {
      for (token <- tokens) {
        var tokenAmountCounterLocal = 0
        var tokenAmountList = amountTokens.apply(tokenAmountCounter)
        for (x <- token) {
          var tokenAmm = tokenAmountList.apply(tokenAmountCounterLocal)
          tList.append(new ErgoToken(x, tokenAmm))
          tokenAmountCounterLocal = tokenAmountCounterLocal + 1
        }
        tokenAmountCounter = tokenAmountCounter + 1
        tokenList1.append(tList)
        tList = new ListBuffer[ErgoToken]()
      }
    }
    for (address: Address <- receiver) {
      var erg = getAmount(amountList.apply(amountCounter))
      var box = this.ctx
        .newTxBuilder()
        .outBoxBuilder()
        .value(erg)
        .tokens(tokenList1.apply(amountCounter).toArray: _*)
        .contract(
          new ErgoTreeContract(
            address.getErgoAddress.script,
            this.ctx.getNetworkType
          )
        )
        .build()
      outbx.append(box)
      amountCounter += 1
    }
    outbx.toList
  }

  def initTicketContractBox(
      singleTon: ErgoToken,
      contract: ErgoContract,
      distributionAddress: Address,
      timeStamp: Long,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(singleTon)
      .registers(
        ErgoValue.of(1L),
        ErgoValue.of(1L),
        ErgoValue.of(distributionAddress.getPublicKey),
        ErgoValue.of(timeStamp),
        ErgoValue.of(1L)
      )
      .contract(contract)
      .build()
  }

  def payoutBox(
      receiver: Address,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .contract(
        new ErgoTreeContract(
          receiver.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def genericContractBox(
      contract: ErgoContract,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .contract(contract)
      .build()
  }

  def whiteListIssuerBox(
      contract: ErgoContract,
      tokenAmount: Long,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(ErgoValue.of(tokenAmount))
      .contract(contract)
      .build()
  }

  def preMintIssuerBox(
      contract: ErgoContract,
      tokenAmount: Long,
      amount: Double = 0.001
  ): OutBox = {
    whiteListIssuerBox(contract, tokenAmount, amount)
  }

  def initNFTCollection(
      registers: Array[ErgoValue[_]],
      contract: ErgoContract,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(registers: _*)
      .contract(contract)
      .build()
  }

  def buildIssuerBox(
      contract: ErgoContract,
      registers: Array[ErgoValue[_]],
      collectionToken: ErgoToken,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(registers: _*)
      .tokens(collectionToken)
      .contract(contract)
      .build()
  }

  def buildStateBox(
      contract: ErgoContract,
      issuanceMetaDataMap: PlasmaMap[IndexKey, IssuanceValueAVL],
      issuerMetaDataMap: PlasmaMap[IndexKey, IssuerValue],
      singletonToken: ErgoToken,
      collectionToken: ErgoToken,
      index: Long,
      startingTime: Long,
      expiryTime: Long,
      r8BooleanArr: Array[Boolean],
      preMintToken: ErgoToken,
      whitelistToken: ErgoToken,
      paymentToken: ErgoToken,
      amount: Double = 0.001
  ): OutBox = {

    val r9 = {
      var preMintTokenBytes: Array[Byte] = Array()
      var whiteListTokenBytes: Array[Byte] = Array()
      var paymentTokenBytes: Array[Byte] = Array()

      if (preMintToken != null) {
        preMintTokenBytes = preMintToken.getId.getBytes
      }
      if (whitelistToken != null) {
        whiteListTokenBytes = whitelistToken.getId.getBytes
      }
      if (paymentToken != null) {
        paymentTokenBytes = paymentToken.getId.getBytes
      }

      Colls.fromArray(
        Array(
          Colls.fromArray(whiteListTokenBytes),
          Colls.fromArray(preMintTokenBytes),
          Colls.fromArray(paymentTokenBytes)
        )
      )
    }

    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(
        issuanceMetaDataMap.ergoValue,
        issuerMetaDataMap.ergoValue,
        ErgoValue.of(index),
        ErgoValueBuilder.buildFor(
          (startingTime, expiryTime)
        ),
        ErgoValueBuilder.buildFor(Colls.fromArray(r8BooleanArr)),
        ErgoValueBuilder.buildFor(r9)
      )
      .tokens(singletonToken, collectionToken)
      .contract(contract)
      .build()
  }

  def buildStateBoxWithLocalPlasmaMap(
      contract: ErgoContract,
      issuanceMetaDataMap: LocalPlasmaMap[IndexKey, IssuanceValueAVL],
      issuerMetaDataMap: LocalPlasmaMap[IndexKey, IssuerValue],
      singletonToken: ErgoToken,
      collectionToken: ErgoToken,
      index: Long,
      startingTime: Long,
      expiryTime: Long,
      r8BooleanArr: Array[Boolean],
      preMintToken: ErgoToken,
      whitelistToken: ErgoToken,
      paymentToken: ErgoToken,
      amount: Double = 0.001
  ): OutBox = {

    val r9 = {
      var preMintTokenBytes: Array[Byte] = Array()
      var whiteListTokenBytes: Array[Byte] = Array()
      var paymentTokenBytes: Array[Byte] = Array()

      if (preMintToken != null) {
        preMintTokenBytes = preMintToken.getId.getBytes
      }
      if (whitelistToken != null) {
        whiteListTokenBytes = whitelistToken.getId.getBytes
      }
      if (paymentToken != null) {
        paymentTokenBytes = paymentToken.getId.getBytes
      }

      Colls.fromArray(
        Array(
          Colls.fromArray(whiteListTokenBytes),
          Colls.fromArray(preMintTokenBytes),
          Colls.fromArray(paymentTokenBytes)
        )
      )
    }

    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(
        issuanceMetaDataMap.ergoValue,
        issuerMetaDataMap.ergoValue,
        ErgoValue.of(index),
        ErgoValueBuilder.buildFor(
          (startingTime, expiryTime)
        ),
        ErgoValueBuilder.buildFor(Colls.fromArray(r8BooleanArr)),
        ErgoValueBuilder.buildFor(r9)
      )
      .tokens(singletonToken, collectionToken)
      .contract(contract)
      .build()
  }

  def lastStateBox(
      contract: ErgoContract,
      issuanceMetaDataMap: PlasmaMap[IndexKey, IssuanceValueAVL],
      issuerMetaDataMap: PlasmaMap[IndexKey, IssuerValue],
      singletonToken: ErgoToken,
      index: Long,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(
        issuanceMetaDataMap.ergoValue,
        issuerMetaDataMap.ergoValue,
        ErgoValue.of(index)
      )
      .tokens(singletonToken)
      .contract(contract)
      .build()
  }

  def lastStateBoxWithLocalPlasmaMap(
      contract: ErgoContract,
      issuanceMetaDataMap: LocalPlasmaMap[IndexKey, IssuanceValueAVL],
      issuerMetaDataMap: LocalPlasmaMap[IndexKey, IssuerValue],
      singletonToken: ErgoToken,
      index: Long,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(
        issuanceMetaDataMap.ergoValue,
        issuerMetaDataMap.ergoValue,
        ErgoValue.of(index)
      )
      .tokens(singletonToken)
      .contract(contract)
      .build()
  }

  def saleExpiryOutbox(
      artistAddress: Address,
      collectionToken: ErgoToken,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(collectionToken)
      .contract(
        new ErgoTreeContract(
          artistAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def burnSaleExpiryOutbox(
      artistAddress: Address,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .contract(
        new ErgoTreeContract(
          artistAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def refundBox(
      singleTon: ErgoToken,
      senderAddress: Address,
      amount: Double
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(singleTon)
      .contract(
        new ErgoTreeContract(
          senderAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def simpleOutBox(
      senderAddress: Address,
      amount: Double
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .contract(
        new ErgoTreeContract(
          senderAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def artistTokenPayoutBox(
      token: ErgoToken,
      artistAddress: Address,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(token)
      .contract(
        new ErgoTreeContract(
          artistAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def artistPayoutBox(
      artistAddress: Address,
      amount: Double
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .contract(
        new ErgoTreeContract(
          artistAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
  }

  def newLPBox(
      LPContract: ErgoContract,
      singleton: ErgoToken,
      artistAddress: Address,
      newAmountLP: Long,
      amount: Double
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(singleton)
      .contract(LPContract)
      .registers(ErgoValue.of(artistAddress.getPublicKey), ErgoValue.of(newAmountLP))
      .build()
  }

  def nftOutBox(
      buyerAddress: Address,
      nft: Eip4Token,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .contract(
        new ErgoTreeContract(
          buyerAddress.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .mintToken(nft)
      .build()
  }

  def mintBuyerBox(
      senderAddress: Address,
      proxyContract: ErgoContract,
      amount: Double = 0.012
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(ErgoValue.of(senderAddress.getPublicKey))
      .contract(proxyContract)
      .build()
  }

}

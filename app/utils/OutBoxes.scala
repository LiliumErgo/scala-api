package utils

import AVL.IssuerBox.IssuerValue
import AVL.NFT.{IndexKey, IssuanceAVLHelpers, IssuanceValueAVL}

import io.getblok.getblok_plasma.collections.{LocalPlasmaMap, PlasmaMap}
import json.Register.Register
import org.ergoplatform.{ErgoBox, appkit}
import org.ergoplatform.appkit.JavaHelpers.JLongRType
import org.ergoplatform.appkit.impl.{Eip4TokenBuilder, ErgoTreeContract}
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.scalaapi.ErgoValueBuilder
import special.collection.Coll

import java.nio.charset.StandardCharsets
import java.util
import scala.collection.mutable.ListBuffer

class OutBoxes(ctx: BlockchainContext) {

  private def getAmount(amount: Double): Long = {
    (amount * Parameters.OneErg).toLong
  }
  private val txBuilder = this.ctx.newTxBuilder()
  private val minAmount = this.getAmount(0.001)

  def pictureNFTHelper(
      inputBox: InputBox,
      name: String,
      description: String,
      imageLink: String,
      sha256: Array[Byte]
  ): Eip4Token = {
    val tokenID = inputBox.getId.toString
    Eip4TokenBuilder.buildNftPictureToken(
      tokenID,
      1,
      name,
      description,
      0,
      sha256,
      imageLink
    )

  }

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

    Eip4TokenBuilder.buildNftPictureToken(
      inputBox.getId.toString,
      tokenAmount,
      name,
      description,
      tokenDecimals,
      Array(0.toByte),
      "link"
    )
  }

  def NFToutBox(
      nft: Eip4Token,
      receiver: Address,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .mintToken(nft)
      .contract(
        new ErgoTreeContract(
          receiver.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
      .build()
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

  def initErgoSapiensContract(
      contract: ErgoContract,
      metaDataMap: String,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(
        ErgoValue.fromHex(metaDataMap),
        ErgoValue.of(0L)
      )
      .contract(contract)
      .build()
  }

  def buyerNFTOutBox(
      nft: Eip4Token,
      receiver: Address,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .mintToken(nft)
      .contract(
        new ErgoTreeContract(
          receiver.getErgoAddress.script,
          this.ctx.getNetworkType
        )
      )
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

  def newErgoSapiensContractBox(
      contract: ErgoContract,
      metaDataMap: String,
      index: Long,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .registers(
        ErgoValue.fromHex(metaDataMap),
        ErgoValue.of(index)
      )
      .contract(contract)
      .build()
  }

  def newIssuerBox(
      royalty: Long,
      senderAddress: Address,
      amount: Double = 0.001
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
      .registers(ErgoValue.of(royalty * 10))
      .build()
  }

  def initCollectionContractBox(
      comet: ErgoToken,
      contract: ErgoContract,
      timeStamp: Long,
      amount: Double = 0.001
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(comet)
      .registers(ErgoValue.of(1L), ErgoValue.of(1L), ErgoValue.of(timeStamp))
      .contract(contract)
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
      returnCollectionTokensToArtist: Boolean,
      amount: Double = 0.001
  ): OutBox = {
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
        ErgoValue.of(returnCollectionTokensToArtist)
      )
      .tokens(singletonToken, collectionToken)
      .contract(contract)
      .build()
  }

  def lastStateBox(
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

  def newTicketBox(
      singleTon: ErgoToken,
      contract: ErgoContract,
      version: Long,
      index: Long,
      timeStamp: Long,
      cometAmount: Long,
      distributionAddress: Address,
      amount: Double
  ): OutBox = {
    this.txBuilder
      .outBoxBuilder()
      .value(getAmount(amount))
      .tokens(singleTon)
      .registers(
        ErgoValue.of(version),
        ErgoValue.of(index),
        ErgoValue.of(distributionAddress.getPublicKey),
        ErgoValue.of(timeStamp),
        ErgoValue.of(cometAmount)
      )
      .contract(contract)
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

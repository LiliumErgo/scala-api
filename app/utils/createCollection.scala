package utils

import AVL.IssuerBox.IssuerValue
import AVL.NFT.{IndexKey, IssuanceValueAVL}
import configs.{
  ContractsConfig,
  SignedTransactionJson,
  SignedTransactionJsonParser,
  conf
}
import contracts.LiliumContracts
import io.getblok.getblok_plasma.collections.{LocalPlasmaMap, PlasmaMap}
import org.ergoplatform.appkit.scalaapi.ErgoValueBuilder
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.SignedTransactionImpl
import org.ergoplatform.explorer.client.model.TransactionInfo
import sigmastate.eval.Colls
import special.collection.Coll

import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class InvalidArtistTransaction(message: String) extends Exception(message)
class TxSubmissionError(message: String) extends Exception(message)
class DataBaseError(message: String) extends Exception(message)
class InvalidCollectionJsonFormat(message: String) extends Exception(message)
class InvalidTransactionB64(message: String) extends Exception(message)
class InvalidCollectionSize(message: String) extends Exception(message)
class InvalidNftFee(message: String) extends Exception(message)
class InvalidTotalFee(message: String) extends Exception(message)
class CoinGekoAPIError(message: String) extends Exception(message)
class InvalidAddress(message: String) extends Exception(message)
class InvalidRoyalty(message: String) extends Exception(message)
class InvalidMetadata(message: String) extends Exception(message)
class InvalidTimeStamp(message: String) extends Exception(message)
class InvalidPremintSetting(message: String) extends Exception(message)
class InvalidWhitelistSetting(message: String) extends Exception(message)
class InvalidPaymentToken(message: String) extends Exception(message)
class ArtistTxError(message: String) extends Exception(message)
object createCollection {

  private def convertERGLongToDouble(num: Long): Double = {
    val value = num * math.pow(10, -9)
    val x =
      (math floor value * math.pow(10, num.toString.length + 2)) / math.pow(
        10,
        num.toString.length + 2
      )
    val bNum = math.BigDecimal(x)
    val finalNum = bNum.underlying().stripTrailingZeros()
    finalNum.toString.toDouble
  }

  def main(
      ctx: BlockchainContext,
      liliumTxOperatorMnemonic: String,
      liliumTxOperatorMnemonicPw: String,
      stateContractString: String,
      issuerContractString: String,
      proxyContractString: String,
      collectionIssuanceContractString: String,
      singletonIssuanceContractString: String,
      collectionIssuerContractString: String,
      singletonIssuerContractString: String,
      premintIssuerContractString: String,
      whitelistIssuerContractString: String,
      LPContractString: String,
      artistAddress: Address,
      royaltyBlakeHash: Array[Byte],
      collectionName: String,
      collectionDescription: String,
      collectionTokensAmount: Long,
      startingTimestamp: Long,
      expiryTimestamp: Long,
      returnCollectionTokensToArtist: Boolean,
      whitelistAccepted: Boolean,
      whitelistBypass: Boolean,
      premintAccepted: Boolean,
      paymentTokenAccepted: Boolean,
      ergAccepted: Boolean,
      whitelistTokenAmount: Long,
      premintTokenAmount: Long,
      usePool: Boolean,
      totalFees: Long,
      amountLP: Long,
      issuanceMetaDataMap: PlasmaMap[IndexKey, IssuanceValueAVL],
      issuerMetaDataMap: PlasmaMap[IndexKey, IssuerValue],
      priceOfNFTNanoErg: Long,
      paymentTokenAmount: Long,
      paymentTokenID: String,
      liliumFeeAddress: Address,
      liliumFeePercent: Long,
      liliumFeeNanoErg: Long,
      minTxOperatorFeeNanoErg: Long,
      minerFee: Long,
      minBoxValue: Long,
      transactionFromArtist: String
  ): ContractsConfig = {

    val txHelper = new TransactionHelper(
      ctx,
      liliumTxOperatorMnemonic,
      liliumTxOperatorMnemonicPw
    )

    def printAndSend(tx: SignedTransaction, description: String): String = {
      val txId = txHelper.sendTx(tx)
      println(s"$description: $txId")
      txId
    }

    def createTokenWithCondition(
        initTxnBox: InputBox,
        collectionName: String,
        condition: String,
        tokenAmount: Long,
        conditionAccepted: Boolean
    ): SignedTransaction = {
      txHelper.createToken(
        artistAddress,
        List(0.001),
        List(initTxnBox).asJava,
        name = s"$collectionName $condition Token",
        description =
          s"Allows ${if (conditionAccepted) "free mint" else "mint"} for $collectionName $condition",
        tokenAmount = tokenAmount,
        tokenDecimals = 0
      )
    }

    val exp = new explorerApi(DefaultNodeInfo(ctx.getNetworkType).explorerUrl)
    val outBoxObj = new OutBoxes(ctx)
    val compiler = new ContractCompile(ctx)
    val issuerContract =
      compiler.compileIssuerContract(issuerContractString, minerFee)
    val LPContract =
      compiler.compileSaleLP(
        LPContractString,
        minBoxValue,
        minerFee,
        minTxOperatorFeeNanoErg
      )

    val initTxnBoxes: ListBuffer[configs.Output] =
      new ListBuffer[configs.Output]

    if (
      !validateBoxes(
        ctx,
        collectionIssuerContractString: String,
        singletonIssuerContractString: String,
        premintIssuerContractString: String,
        whitelistIssuerContractString: String,
        txHelper.senderAddress,
        liliumFeeAddress,
        liliumFeeNanoErg,
        minerFee,
        premintAccepted,
        whitelistAccepted,
        usePool,
        totalFees,
        SignedTransactionJsonParser.readJsonString(transactionFromArtist),
        initTxnBoxes
      )
    ) {
      throw new InvalidArtistTransaction("Improper Transaction From artist")
    }

    var preMintToken: ErgoToken = new ErgoToken("", 1)
    var whitelistToken: ErgoToken = new ErgoToken("", 1)
    val paymentToken: ErgoToken = {
      if (paymentTokenAccepted) {
        new ErgoToken(paymentTokenID, paymentTokenAmount)
      } else {
        null
      }
    }

    preMintToken = null
    whitelistToken = null

    val txIdFromArtist = {
      try {
        exp.sendTx(transactionFromArtist)
      } catch {
        case e: Exception =>
          throw new TxSubmissionError(
            "Issue Submitting Transaction To Node"
          )
      }
    }

    Thread.sleep(1000)

    val stateBoxSingletonIssuerOutbox: InputBox =
      exp.getUnspentBoxFromMempool(initTxnBoxes.head.boxId)
    val collectionIssuerOutbox: InputBox =
      exp.getUnspentBoxFromMempool(initTxnBoxes(1).boxId)
    val singletonIssuanceContract = compiler.compileSingletonIssuanceContract(
      singletonIssuanceContractString,
      new ErgoToken(stateBoxSingletonIssuerOutbox.getId.toString, 1),
      txHelper.senderAddress,
      usePool,
      totalFees
    )

    val proxyContract: ErgoContract =
      compiler.compileProxyContract(proxyContractString, minerFee)
    val stateContract: ErgoContract = compiler.compileStateContract(
      stateContractString,
      proxyContract,
      issuerContract,
      artistAddress,
      royaltyBlakeHash,
      new ErgoToken(collectionIssuerOutbox.getId.toString, 1L),
      new ErgoToken(stateBoxSingletonIssuerOutbox.getId.toString, 1L),
      priceOfNFTNanoErg,
      paymentTokenAmount,
      liliumFeeAddress,
      liliumFeePercent,
      minTxOperatorFeeNanoErg,
      minerFee,
      minBoxValue
    )

    val singletonTokenAmount = {
      if (usePool) {
        2L
      } else {
        1L
      }
    }

    val singletonTokenTx: SignedTransaction = txHelper.createToken(
      singletonIssuanceContract.toAddress,
      List(
        convertERGLongToDouble(
          stateBoxSingletonIssuerOutbox.getValue - minerFee
        )
      ),
      List(
        stateBoxSingletonIssuerOutbox.withContextVars(
          ContextVar.of(
            0.toByte,
            singletonIssuanceContract.toAddress.asP2S().scriptBytes
          )
        )
      ).asJava,
      singletonIssuanceContract.toAddress,
      name = "State Box Singleton",
      description = "State Box Singleton",
      tokenAmount = singletonTokenAmount,
      tokenDecimals = 0,
      fee = convertERGLongToDouble(minerFee)
    )

    printAndSend(singletonTokenTx, "Singleton Creation Tx")

    val collectionIssuanceContract = compiler.compileCollectionIssuanceContract(
      collectionIssuanceContractString,
      txHelper.senderAddress
    )
    val collectionTokenTx: SignedTransaction = txHelper.createToken(
      collectionIssuanceContract.toAddress,
      List(0.002 + convertERGLongToDouble(minerFee)),
      List(
        collectionIssuerOutbox.withContextVars(
          ContextVar.of(
            0.toByte,
            collectionIssuanceContract.toAddress.asP2S().scriptBytes
          )
        )
      ).asJava,
      collectionIssuanceContract.toAddress,
      isCollection = true,
      name = collectionName,
      description = collectionDescription,
      tokenAmount = collectionTokensAmount,
      tokenDecimals = 0
    )

    printAndSend(collectionTokenTx, "Collection Creation Tx")

    if (premintAccepted && premintTokenAmount != -1) {
      val preMintTokenTx: SignedTransaction = createTokenWithCondition(
        exp.getUnspentBoxFromMempool(initTxnBoxes(3).boxId),
        collectionName,
        "Premint",
        premintTokenAmount,
        premintAccepted
      )

      printAndSend(preMintTokenTx, "Premint Token Mint Tx")
      preMintToken = preMintTokenTx.getOutputsToSpend.get(0).getTokens.get(0)

      if (whitelistAccepted && whitelistTokenAmount != -1) {
        val whitelistTokenTx: SignedTransaction = createTokenWithCondition(
          exp.getUnspentBoxFromMempool(initTxnBoxes(4).boxId),
          collectionName,
          "Whitelist",
          whitelistTokenAmount,
          whitelistAccepted
        )
        printAndSend(whitelistTokenTx, "Whitelist Token Mint Tx")
        whitelistToken =
          whitelistTokenTx.getOutputsToSpend.get(0).getTokens.get(0)
      }
    } else if (whitelistAccepted && whitelistTokenAmount != -1) {
      val whitelistTokenTx: SignedTransaction = createTokenWithCondition(
        exp.getUnspentBoxFromMempool(initTxnBoxes(3).boxId),
        collectionName,
        "Whitelist",
        whitelistTokenAmount,
        whitelistAccepted
      )
      printAndSend(whitelistTokenTx, "Whitelist Token Mint Tx")
      whitelistToken =
        whitelistTokenTx.getOutputsToSpend.get(0).getTokens.get(0)
    }

    val stateBox: OutBox = outBoxObj.buildStateBox(
      stateContract,
      issuanceMetaDataMap,
      issuerMetaDataMap,
      new ErgoToken(
        singletonTokenTx.getOutputsToSpend
          .get(0)
          .getTokens
          .get(0)
          .getId
          .toString,
        1
      ),
      collectionTokenTx.getOutputsToSpend.get(0).getTokens.get(0),
      0,
      startingTimestamp,
      expiryTimestamp,
      Array(
        returnCollectionTokensToArtist,
        whitelistAccepted,
        whitelistBypass,
        premintAccepted,
        paymentTokenAccepted,
        usePool,
        ergAccepted
      ),
      preMintToken,
      whitelistToken,
      paymentToken,
      0.001 + convertERGLongToDouble(minerFee) + convertERGLongToDouble(
        minTxOperatorFeeNanoErg
      )
    )

    val stateBoxInput = new util.ArrayList[InputBox]()
    stateBoxInput.add(
      singletonTokenTx.getOutputsToSpend
        .get(0)
        .withContextVars(
          ContextVar.of(0.toByte, stateContract.toAddress.asP2S().scriptBytes),
          ContextVar.of(1.toByte, LPContract.toAddress.asP2S().scriptBytes)
        )
    )
    stateBoxInput.add(
      collectionTokenTx.getOutputsToSpend
        .get(0)
        .withContextVars(
          ContextVar.of(0.toByte, stateContract.toAddress.asP2S().scriptBytes),
          ContextVar.of(
            1.toByte,
            exp.getErgoBoxfromID(collectionIssuerOutbox.getId.toString)
          )
        )
    )

    val stateBoxAndMaybeLPOut = {
      if (usePool) {
        val LPBox = outBoxObj.newLPBox(
          LPContract,
          new ErgoToken(
            singletonTokenTx.getOutputsToSpend
              .get(0)
              .getTokens
              .get(0)
              .getId
              .toString,
            1
          ),
          artistAddress,
          amountLP,
          convertERGLongToDouble(totalFees)
        )
        List(stateBox, LPBox)
      } else {
        List(stateBox)
      }
    }

    val firstStateBoxTx: SignedTransaction = txHelper.signTransaction(
      txHelper.buildUnsignedTransaction(stateBoxInput, stateBoxAndMaybeLPOut)
    )

    printAndSend(firstStateBoxTx, "State Box Tx")

    val preMintTokenID = {
      try {
        preMintToken.getId.toString
      } catch {
        case e: Exception => ""
      }
    }

    val whitelistTokenID = {
      try {
        whitelistToken.getId.toString
      } catch {
        case e: Exception => ""
      }
    }

    val paymentTokenIDString = {
      try {
        paymentToken.getId.toString
      } catch {
        case e: Exception => ""
      }
    }

    val saleLPContract = {
      if (usePool) {
        LPContract.toAddress.toString
      } else {
        ""
      }
    }

    val saleLPSingleton = {
      if (usePool) {
        singletonTokenTx.getOutputsToSpend
          .get(0)
          .getTokens
          .get(0)
          .getId
          .toString
      } else {
        ""
      }
    }

    val saleLPSingletonAmount = {
      if (usePool) {
        singletonTokenTx.getOutputsToSpend.get(0).getTokens.get(0).getValue
      } else {
        -1
      }
    }

    new conf(
      txIdFromArtist,
      stateContract.toAddress.toString,
      singletonTokenTx.getOutputsToSpend.get(0).getTokens.get(0).getId.toString,
      singletonTokenTx.getOutputsToSpend.get(0).getTokens.get(0).getValue,
      issuerContract.toAddress.toString,
      proxyContract.toAddress.toString,
      saleLPContract,
      saleLPSingleton,
      saleLPSingletonAmount,
      collectionIssuerOutbox.getId.toString,
      collectionTokensAmount,
      preMintTokenID,
      premintTokenAmount,
      whitelistTokenID,
      whitelistTokenAmount,
      paymentTokenIDString,
      paymentTokenAmount
    ).read
  }

  def validateBoxes(
      ctx: BlockchainContext,
      collectionIssuerContractString: String,
      singletonIssuerContractString: String,
      premintIssuerContractString: String,
      whitelistIssuerContractString: String,
      liliumTxOperator: Address,
      liliumFeeAddress: Address,
      liliumFeeNanoERG: Long,
      minerFee: Long,
      preMint: Boolean,
      whitelist: Boolean,
      usePool: Boolean,
      totalFees: Long,
      artistTx: SignedTransactionJson,
      initTxnBoxes: ListBuffer[configs.Output]
  ): Boolean = {

    val compilerObj = new ContractCompile(ctx)

    def addressMatchesCompiledScript(
        compiledFunc: (String, Address) => ErgoContract,
        contractScript: String,
        ergotree: String
    ): Boolean = {
      ergotree == compiledFunc(
        contractScript,
        liliumTxOperator
      ).getErgoTree.bytesHex
    }

    def createFilter(
        compiledFunc: (String, Address) => ErgoContract,
        script: String,
        minThreshold: Long
    ): configs.Output => Boolean = box => {
      addressMatchesCompiledScript(
        compiledFunc,
        script,
        box.ergoTree
      ) &&
        box.value.toLong >= minThreshold
    }

    val singletonIssuerFilter = (box: configs.Output) => {
      val minThreshold: Long = {
        if (usePool) {
          1000000L + minerFee + totalFees
        } else {
          1000000L + minerFee
        }
      }

      (box.ergoTree == compilerObj
        .compileSingletonIssuerContract(
          singletonIssuerContractString,
          liliumTxOperator,
          usePool,
          minerFee
        )
        .getErgoTree
        .bytesHex) && box.value.toLong >= minThreshold
    }

    val collectionIssuerFilter = createFilter(
      compilerObj.compileCollectionIssuerContract,
      collectionIssuerContractString,
      2000000L + 1000000L + minerFee
    )

    val liliumFeeFilter: configs.Output => Boolean = box => {
      box.ergoTree == liliumFeeAddress.asP2PK().script.bytesHex &&
        box.value.toLong >= (liliumFeeNanoERG * 0.95)
    }

    val preMintFilter = createFilter(
      compilerObj.compilePreMintIssuerContract,
      premintIssuerContractString,
      2000000L
    )

    val whitelistFilter = createFilter(
      compilerObj.compileWhitelistIssuerContract,
      whitelistIssuerContractString,
      2000000L
    )

    val singletonIssuerElem = artistTx.outputs.filter(singletonIssuerFilter)
    val collectionIssuerElem = artistTx.outputs.filter(collectionIssuerFilter)
    val liliumFeeBox = artistTx.outputs.filter(liliumFeeFilter)

    if (
      singletonIssuerElem.length != 1 || collectionIssuerElem.length != 1 || liliumFeeBox.length != 1
    ) {
      throw new ArtistTxError("Issuer or Lilium fee box error")
    }

    initTxnBoxes.append(singletonIssuerElem.head)
    initTxnBoxes.append(collectionIssuerElem.head)
    initTxnBoxes.append(liliumFeeBox.head)

    val preMintBox: Array[configs.Output] =
      if (preMint) artistTx.outputs.filter(preMintFilter) else Array()
    val whitelistBox: Array[configs.Output] =
      if (whitelist) artistTx.outputs.filter(whitelistFilter) else Array()

    if (preMint) {
      if (preMintBox.length < 1) {
        throw new ArtistTxError("Premint Box Error")
      }
      initTxnBoxes.append(preMintBox.head)
      if (whitelist) {
        if (whitelistBox.length < 1) {
          throw new ArtistTxError("Whitelist Box Error")
        }
        initTxnBoxes.append(whitelistBox(1))
      }
    } else if (whitelist) {
      if (whitelistBox.length < 1) {
        throw new ArtistTxError("Whitelist Box Error")
      }
      initTxnBoxes.append(whitelistBox.head)
    }

    true
  }

}

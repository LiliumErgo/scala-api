package utils

import AVL.IssuerBox.IssuerValue
import AVL.NFT.{IndexKey, IssuanceValueAVL}
import configs.{ContractsConfig, conf}
import contracts.LiliumContracts
import io.getblok.getblok_plasma.collections.{LocalPlasmaMap, PlasmaMap}
import org.ergoplatform.appkit.scalaapi.ErgoValueBuilder
import org.ergoplatform.appkit._
import org.ergoplatform.explorer.client.model.TransactionInfo
import sigmastate.eval.Colls

import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class InvalidArtistTransaction(message: String) extends Exception(message)
class DataBaseError(message: String) extends Exception(message)
class InvalidCollectionJsonFormat(message: String) extends Exception(message)
class InvalidCollectionSize(message: String) extends Exception(message)
class CoinGekoAPIError(message: String) extends Exception(message)

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
      artistAddress: Address,
      royaltyBlakeHash: Array[Byte],
      collectionName: String,
      collectionDescription: String,
      collectionTokensAmount: Long,
      startingTimestamp: Long,
      expiryTimestamp: Long,
      returnCollectionTokensToArtist: Boolean,
      issuanceMetaDataMap: PlasmaMap[IndexKey, IssuanceValueAVL],
      issuerMetaDataMap: PlasmaMap[IndexKey, IssuerValue],
      priceOfNFTNanoErg: Long,
      liliumFeeAddress: Address,
      liliumFeeNanoErg: Long,
      minTxOperatorFeeNanoErg: Long,
      minerFee: Long,
      transactionIdFromArtist: String
  ): ContractsConfig = {
    val exp = new explorerApi(
      DefaultNodeInfo(ctx.getNetworkType).explorerUrl
    )
    val txHelper =
      new TransactionHelper(
        ctx,
        liliumTxOperatorMnemonic,
        liliumTxOperatorMnemonicPw
      )
    val outBoxObj = new OutBoxes(ctx)
    val compiler = new ContractCompile(ctx)
    val issuerContract =
      compiler.compileIssuerContract(issuerContractString)

    // the above txns should come from the other party

    val initTransactionP1: List[List[String]] = exp
      .getOutputsFromTransaction(transactionIdFromArtist)

    if (
      !this.validateBoxes(
        ctx,
        txHelper.senderAddress,
        liliumFeeAddress,
        liliumFeeNanoErg,
        minerFee,
        initTransactionP1,
        exp
      )
    ) {
      throw new InvalidArtistTransaction("Improper Transaction From artist")
    }

    val stateBoxSingletonIssuerOutbox: InputBox =
      exp.getUnspentBoxFromMempool(initTransactionP1.head.head)

    val collectionIssuerOutbox: InputBox =
      exp.getUnspentBoxFromMempool(initTransactionP1(1).head)

    val singletonIssuanceContract =
      compiler.compileSingletonIssuanceContract(
        singletonIssuanceContractString,
        new ErgoToken(
          stateBoxSingletonIssuerOutbox.getId.toString,
          1
        ),
        txHelper.senderAddress
      )

    val proxyContract: ErgoContract =
      compiler.compileProxyContract(proxyContractString, minerFee)

    val stateContract: ErgoContract = compiler.compileStateContract(
      stateContractString,
      proxyContract,
      issuerContract,
      artistAddress,
      royaltyBlakeHash,
      new ErgoToken(
        collectionIssuerOutbox.getId.toString,
        1L
      ),
      new ErgoToken(
        stateBoxSingletonIssuerOutbox.getId.toString,
        1L
      ),
      priceOfNFTNanoErg,
      liliumFeeAddress,
      5000000,
      minTxOperatorFeeNanoErg,
      minerFee
    )

    val singletonTokenTx: SignedTransaction = txHelper.createToken(
      singletonIssuanceContract.toAddress,
      List(0.001),
      List(
        stateBoxSingletonIssuerOutbox
          .withContextVars(
            ContextVar.of(
              0.toByte,
              singletonIssuanceContract.toAddress.asP2S().scriptBytes
            )
          )
      ).asJava,
      singletonIssuanceContract.toAddress,
      name = "State Box Singleton",
      description = "State Box Singleton",
      tokenAmount = 1L,
      tokenDecimals = 0
    )

    println("Singleton Creation Tx: " + txHelper.sendTx(singletonTokenTx))

    val collectionIssuanceContract =
      compiler.compileCollectionIssuanceContract(
        collectionIssuanceContractString,
        txHelper.senderAddress
      )

    val collectionTokenTx: SignedTransaction = txHelper.createToken(
      collectionIssuanceContract.toAddress,
      List(0.002 + convertERGLongToDouble(minerFee)),
      List(
        collectionIssuerOutbox
          .withContextVars(
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

    println("Collection Creation Tx: " + txHelper.sendTx(collectionTokenTx))

    val stateBox: OutBox = outBoxObj.buildStateBox(
      stateContract,
      issuanceMetaDataMap,
      issuerMetaDataMap,
      singletonTokenTx.getOutputsToSpend.get(0).getTokens.get(0),
      collectionTokenTx.getOutputsToSpend.get(0).getTokens.get(0),
      0,
      startingTimestamp,
      expiryTimestamp,
      returnCollectionTokensToArtist,
      0.001 + convertERGLongToDouble(minerFee) + convertERGLongToDouble(
        minTxOperatorFeeNanoErg
      )
    )

    val stateBoxInput = new util.ArrayList[InputBox]()

    stateBoxInput.add(
      singletonTokenTx.getOutputsToSpend
        .get(0)
        .withContextVars(
          ContextVar.of(0.toByte, stateContract.toAddress.asP2S().scriptBytes)
        )
    )

    stateBoxInput.add(
      collectionTokenTx.getOutputsToSpend
        .get(0)
        .withContextVars(
          ContextVar.of(0.toByte, stateContract.toAddress.asP2S().scriptBytes),
          ContextVar.of(
            1.toByte,
            exp.getErgoBoxfromID(
              collectionIssuerOutbox.getId.toString
            )
          )
        )
    )

    val firstStateBoxTx: SignedTransaction = txHelper.signTransaction(
      txHelper.buildUnsignedTransaction(stateBoxInput, List(stateBox))
    )

    val stateBoxTx: String = txHelper.sendTx(firstStateBoxTx)
    println("State Box Tx: " + stateBoxTx)

    new conf(
      stateContract.toAddress.toString,
      singletonTokenTx.getOutputsToSpend.get(0).getTokens.get(0).getId.toString,
      issuerContract.toAddress.toString,
      proxyContract.toAddress.toString,
      collectionIssuerOutbox.getId.toString
    ).read
  }

  def validateBoxes(
      ctx: BlockchainContext,
      liliumTxOperator: Address,
      liliumFeeAddress: Address,
      liliumFeeNanoERG: Long,
      minerFee: Long,
      boxes: List[List[String]],
      exp: explorerApi
  ): Boolean = {

    val boxesMapped = new ListBuffer[(InputBox, String)]()

    val boxesInput: List[InputBox] =
      boxes.map(obj => exp.getUnspentBoxFromMempool(obj.head))

    boxes.foreach(obj =>
      boxesMapped.append((exp.getUnspentBoxFromMempool(obj.head), obj(1)))
    )

    boxesInput.head.toErgoValue.getValue.propositionBytes

    val compilerObj = new ContractCompile(ctx)

    val singletonIssuerContractFromArtist = Address
      .fromPropositionBytes(
        ctx.getNetworkType,
        boxesInput.head.toErgoValue.getValue.propositionBytes.toArray
      )

    val collectionIssuerContractFromArtist = Address
      .fromPropositionBytes(
        ctx.getNetworkType,
        boxesInput(1).toErgoValue.getValue.propositionBytes.toArray
      )

    val liliumFeeAddressFromArtist = Address
      .fromPropositionBytes(
        ctx.getNetworkType,
        boxesInput(2).toErgoValue.getValue.propositionBytes.toArray
      )

    if ( //singleton issuer must have correct ergoTree and at least 0.002 ERG
      singletonIssuerContractFromArtist.toString == compilerObj
        .compileSingletonIssuerContract(
          LiliumContracts.SingletonIssuer.contractScript,
          liliumTxOperator
        )
        .toAddress
        .toString && boxesInput.head.getValue >= 2000000L
    ) {
      if (
        collectionIssuerContractFromArtist.toString == compilerObj
          .compileCollectionIssuerContract(
            LiliumContracts.CollectionIssuer.contractScript,
            liliumTxOperator
          )
          .toAddress
          .toString && convertERGLongToDouble(
          boxesInput(1).getValue
        ) >= 0.002 + 0.001 + convertERGLongToDouble(minerFee)
      ) {
        val correctFeeAddress: Boolean =
          liliumFeeAddressFromArtist.toString == liliumFeeAddress.toString
        val correctFeeAmount: Boolean =
          boxesInput(2).getValue >= (liliumFeeNanoERG * 0.95)

        return correctFeeAddress && correctFeeAmount
      }
    }
    false

  }

}

package utils

import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit._

import java.util
import scala.collection.JavaConverters
import scala.collection.mutable.ListBuffer
import scala.math._

class TransactionHelper(
    ctx: BlockchainContext,
    walletMnemonic: String,
    mnemonicPassword: String = ""
) {
  private val mnemonic = Mnemonic.create(
    SecretString.create(walletMnemonic),
    SecretString.create(mnemonicPassword)
  )
  private val txBuilder = this.ctx.newTxBuilder()

  val senderAddress: Address = Address.createEip3Address(
    0,
    ctx.getNetworkType,
    SecretString.create(walletMnemonic),
    SecretString.create(mnemonicPassword),
    false
  )

  def buildUnsignedTransaction(
      inputBox: util.List[InputBox],
      outBox: List[OutBox],
      fee: Double = 0.001
  ): UnsignedTransaction = {
    this.ctx
      .newTxBuilder()
      .boxesToSpend(inputBox)
      .outputs(outBox.toArray: _*)
      .fee(this.getAmount(fee))
      .sendChangeTo(this.senderAddress.asP2PK())
      .build()
  }

  def buildUnsignedTransactionWithDataInputs(
      inputBox: util.List[InputBox],
      outBox: List[OutBox],
      dataInputs: util.List[InputBox],
      fee: Double = 0.001
  ): UnsignedTransaction = {
    this.ctx
      .newTxBuilder()
      .boxesToSpend(inputBox)
      .outputs(outBox.toArray: _*)
      .withDataInputs(dataInputs)
      .fee(this.getAmount(fee))
      .sendChangeTo(this.senderAddress.asP2PK())
      .build()
  }

  def buildUnsignedTransactionWithDataInputsWithTokensToBurn(
      inputBox: util.List[InputBox],
      outBox: List[OutBox],
      dataInputs: util.List[InputBox],
      tokensToBurn: List[ErgoToken],
      fee: Double = 0.001
  ): UnsignedTransaction = {
    this.ctx
      .newTxBuilder()
      .boxesToSpend(inputBox)
      .outputs(outBox.toArray: _*)
      .withDataInputs(dataInputs)
      .tokensToBurn(tokensToBurn.toArray: _*)
      .fee(this.getAmount(fee))
      .sendChangeTo(this.senderAddress.asP2PK())
      .build()
  }

  private def buildUnsignedTransactionChained(
      inputBox: util.List[InputBox],
      senderAddress: Address,
      amountList: List[Double],
      outBox: List[OutBox],
      tokens: Boolean = false,
      fee: Double = 0.001
  ): UnsignedTransaction = {
    val tb = this.ctx.newTxBuilder()
    val inputBox1: InputBox = inputBox.get(0)
    val erg: Long = abs(
      inputBox1.getValue - (Parameters.OneErg * (amountList.sum + 0.001)).toLong
    )
    val tList = new ListBuffer[ErgoToken]()
    var box: OutBox = null
    val tokenFromInput: Seq[ErgoToken] = JavaConverters
      .collectionAsScalaIterableConverter(inputBox1.getTokens)
      .asScala
      .toSeq
    if (tokens) {
      for (token <- tokenFromInput) {
        tList.append(token: ErgoToken)
      }
      box = tb
        .outBoxBuilder()
        .value(erg)
        .tokens(tList.toArray: _*)
        .contract(
          new ErgoTreeContract(
            senderAddress.getErgoAddress.script,
            this.ctx.getNetworkType
          )
        )
        .build()
    } else {
      box = tb
        .outBoxBuilder()
        .value(erg)
        .contract(
          new ErgoTreeContract(
            senderAddress.getErgoAddress.script,
            this.ctx.getNetworkType
          )
        )
        .build()

    }
    val finalOut = box :: outBox
    tb.boxesToSpend(inputBox)
      .outputs(finalOut.toArray: _*)
      .fee(this.getAmount(fee))
      .sendChangeTo(senderAddress.asP2PK())
      .build()
  }

  def signTransaction(
      unsignedTransaction: UnsignedTransaction,
      proverIndex: Int = 0
  ): SignedTransaction = {
    val prover = this.ctx
      .newProverBuilder()
      .withMnemonic(mnemonic, false)
      .withEip3Secret(proverIndex)
      .build()
    prover.sign(unsignedTransaction)
  }
  def sendTx(signedTransaction: SignedTransaction): String = {
    this.ctx.sendTransaction(signedTransaction)
  }

  private def getAmount(amount: Double): Long = {
    val bigAmount = BigDecimal(amount)
    val result = (bigAmount * BigDecimal(Parameters.OneErg))
      .setScale(0, BigDecimal.RoundingMode.HALF_UP)
    result.toLong
  }
  def sendToken(
      receiver: List[Address],
      amountList: List[Double],
      tokens: List[List[String]],
      amountTokens: List[List[Long]] = null,
      inputBox: util.List[InputBox] = null,
      sender: Address = this.senderAddress,
      isChained: Boolean = false
  ): SignedTransaction = {
    var inBox: util.List[InputBox] = null;
    if (inputBox == null) {
      inBox =
        new InputBoxes(ctx).getInputs(amountList, sender, tokens, amountTokens)
    } else {
      inBox = inputBox
    }
    var unsignedTransaction: UnsignedTransaction = null
    val outBox = new OutBoxes(this.ctx)
      .tokenSendOutBox(receiver, amountList, tokens, amountTokens)
    if (isChained) {
      unsignedTransaction = this.buildUnsignedTransactionChained(
        inBox,
        sender,
        amountList,
        outBox,
        tokens = true
      )
    } else {
      unsignedTransaction = this.buildUnsignedTransaction(inBox, outBox)
    }
    this.signTransaction(unsignedTransaction)
  }

  def simpleSend(
      receiver: List[Address],
      amountList: List[Double],
      inputBox: util.List[InputBox] = null,
      sender: Address = this.senderAddress,
      isChained: Boolean = false
  ): SignedTransaction = {
    var inBox: util.List[InputBox] = null;
    if (inputBox == null) {
      inBox = new InputBoxes(ctx).getInputs(amountList, sender)
    } else {
      inBox = inputBox
    }
    var unsignedTransaction: UnsignedTransaction = null
    val outBox =
      new OutBoxes(this.ctx).simpleOutBox(receiver.head, amountList.head)
    if (isChained) {
      unsignedTransaction = this.buildUnsignedTransactionChained(
        inBox,
        sender,
        amountList,
        List(outBox),
        tokens = true
      )
    } else {
      unsignedTransaction = this.buildUnsignedTransaction(inBox, List(outBox))
    }
    this.signTransaction(unsignedTransaction)
  }

  def createToken(
      receiver: Address,
      amountList: List[Double],
      inputBox: util.List[InputBox] = null,
      sender: Address = this.senderAddress,
      isChained: Boolean = false,
      isCollection: Boolean = false,
      name: String,
      description: String,
      tokenAmount: Long,
      tokenDecimals: Int,
      fee: Double = 0.001
  ): SignedTransaction = {
    var inBox: util.List[InputBox] = null;
    if (inputBox == null) {
      inBox = new InputBoxes(ctx).getInputs(amountList, sender)
    } else {
      inBox = inputBox
    }
    var unsignedTransaction: UnsignedTransaction = null
    val outBoxObj = new OutBoxes(this.ctx)
    var token = outBoxObj.tokenHelper(
      inBox.get(0),
      name,
      description,
      tokenAmount,
      tokenDecimals
    )

    if (isCollection) {
      token = outBoxObj.collectionTokenHelper(
        inBox.get(0),
        name,
        description,
        tokenAmount,
        tokenDecimals
      )
    }

    val outBox =
      outBoxObj.tokenOutBox(token, receiver, amount = amountList.head)

    if (isChained) {
      unsignedTransaction = this.buildUnsignedTransactionChained(
        inBox,
        sender,
        amountList,
        List(outBox),
        tokens = true,
        fee
      )
    } else {
      unsignedTransaction =
        this.buildUnsignedTransaction(inBox, List(outBox), fee)
    }

    this.signTransaction(unsignedTransaction)
  }
}

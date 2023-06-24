package utils

import configs.serviceOwnerConf
import explorer.Explorer
import org.apache.http.{HttpHeaders, HttpStatus, NameValuePair}
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair
import org.ergoplatform.ErgoBox
import org.ergoplatform.appkit.{ErgoToken, InputBox}
import org.ergoplatform.appkit.impl.{InputBoxImpl, ScalaBridge}
import org.ergoplatform.explorer.client.model.{
  InputInfo,
  OutputInfo,
  TokenInfo,
  TransactionInfo
}
import org.ergoplatform.explorer.client.{DefaultApi, ExplorerApiClient}
import org.ergoplatform.restapi.client._

import java.io.{BufferedReader, InputStreamReader}
import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class explorerApi(
    apiUrl: String = serviceOwnerConf.read("serviceOwner.json").apiUrl,
    nodeUrl: String = serviceOwnerConf.read("serviceOwner.json").nodeUrl
) extends Explorer(
      nodeInfo = DefaultNodeInfo(
        new network(
          serviceOwnerConf.read("serviceOwner.json").nodeUrl
        ).getNetworkType
      )
    ) {

  def getExplorerApi(apiUrl: String): DefaultApi = {
    new ExplorerApiClient(apiUrl).createService(classOf[DefaultApi])
  }

  def buildNodeService(nodeUrl: String): ApiClient = {
    new ApiClient(nodeUrl)
  }

  def getUnspentBoxFromTokenID(tokenId: String): OutputInfo = {
    val api = this.getExplorerApi(this.apiUrl)
    val res =
      api.getApiV1BoxesUnspentBytokenidP1(tokenId, 0, 1).execute().body()
    try {
      res.getItems.get(0)
    } catch {
      case e: Exception => null
    }
  }

  def getBoxesFromTokenID(tokenId: String): OutputInfo = { //returns latest box the token has been in
    val api = this.getExplorerApi(this.apiUrl)
    var res = api.getApiV1BoxesBytokenidP1(tokenId, 0, 1).execute().body()
    val offset = res.getTotal - 1
    res = api.getApiV1BoxesBytokenidP1(tokenId, offset, 1).execute().body()
    try {
      res.getItems.get(0)
    } catch {
      case e: Exception => println(e); null
    }
  }

  def getBoxesfromTransaction(txId: String): TransactionInfo = {
    val api = this.getExplorerApi(this.apiUrl)
    api.getApiV1TransactionsP1(txId).execute().body()
  }

  def sendTx(
      tx: String
  ): String = {
    val requestEntity = new StringEntity(
      tx,
      ContentType.APPLICATION_JSON
    )
    val post = new HttpPost(
      s"${this.nodeUrl}/transactions"
    )

    val nameValuePairs = new util.ArrayList[NameValuePair]()

    nameValuePairs.add(new BasicNameValuePair("JSON", tx))
    post.setEntity(requestEntity)
    post.setHeader(HttpHeaders.CONTENT_TYPE, "application/json")
    val client = HttpClients.custom().build()
    val response = client.execute(post)

    if (response.getStatusLine.getStatusCode == HttpStatus.SC_OK) {
      val entity = response.getEntity
      if (entity != null) {
        val content = new BufferedReader(
          new InputStreamReader(entity.getContent)
        )
        val responseBody = Iterator
          .continually(content.readLine())
          .takeWhile(_ != null)
          .mkString("\n")
        content.close() // Close the content stream to release its resources
        responseBody.replace("\"", "")
      } else {
        throw new RuntimeException("No response body received")
      }
    } else {
      throw new RuntimeException(
        s"Request failed with status code: ${response.getStatusLine.getStatusCode}"
      )
    }

  }

  def getTokenInfo(tokenID: String): TokenInfo = {
    val api = this.getExplorerApi(this.apiUrl)
    api.getApiV1TokensP1(tokenID).execute().body()
  }

  def getAddressInfo(address: String): util.List[OutputInfo] = {
    val api = this.getExplorerApi(this.apiUrl)
    api.getApiV1BoxesByaddressP1(address, 0, 100).execute().body().getItems
  }

  def getBoxesfromUnconfirmedTransaction(
      txId: String
  ): ErgoTransaction = {
    val nodeService = this
      .buildNodeService(this.nodeUrl)
      .createService(classOf[TransactionsApi])

    nodeService.getUnconfirmedTransactionById(txId).execute().body()
  }

  def getOutputsFromTransaction(
      txId: String
  ): List[List[String]] = {

    val outputsList = new ListBuffer[List[String]]()
    val nodeService = this
      .buildNodeService(this.nodeUrl)
      .createService(classOf[TransactionsApi])

    val res = nodeService.getUnconfirmedTransactionById(txId).execute()
    if (res.code() == 404) {
      val boxes = this.getBoxesfromTransaction(txId).getOutputs
      boxes.foreach(o => outputsList.append(List(o.getBoxId, o.getErgoTree)))
      return outputsList.toList
    }
    val boxes = res.body().getOutputs
    boxes.foreach(o => outputsList.append(List(o.getBoxId, o.getErgoTree)))
    outputsList.toList
  }

  def getUnspentBoxFromMempool(boxId: String): InputBox = {
    val nodeService =
      this.buildNodeService(this.nodeUrl).createService(classOf[UtxoApi])
    val response = nodeService.getBoxWithPoolById(boxId).execute().body()
    if (response == null) {
      return new InputBoxImpl(this.getErgoBoxfromID(boxId))
        .asInstanceOf[InputBox]
    }
    new InputBoxImpl(response).asInstanceOf[InputBox]
  }

  def getMem(boxId: String): Boolean = {
    val nodeService =
      this.buildNodeService(this.nodeUrl).createService(classOf[UtxoApi])
    val response = nodeService.getBoxWithPoolById(boxId).execute().body()
    if (response == null) {
      return false
    }
    true
  }

  def getBoxbyIDfromExplorer(boxID: String): OutputInfo = {
    val api = this.getExplorerApi(this.apiUrl)
    api.getApiV1BoxesP1(boxID).execute().body()
  }

  def getErgoBoxfromID(boxID: String): ErgoBox = {
    val nodeService =
      this.buildNodeService(this.nodeUrl).createService(classOf[UtxoApi])
    val response: ErgoTransactionOutput =
      nodeService.getBoxWithPoolById(boxID).execute().body()

    if (response == null) {
      val box = this.getBoxbyIDfromExplorer(boxID)
      val tokens = new util.ArrayList[Asset](box.getAssets.size)
      for (asset <- box.getAssets) {
        tokens.add(
          new Asset().tokenId(asset.getTokenId).amount(asset.getAmount)
        )
      }
      val registers = new Registers
      for (registerEntry <- box.getAdditionalRegisters.entrySet) {
        registers.put(
          registerEntry.getKey,
          registerEntry.getValue.serializedValue
        )
      }
      val boxConversion: ErgoTransactionOutput = new ErgoTransactionOutput()
        .ergoTree(box.getErgoTree)
        .boxId(box.getBoxId)
        .index(box.getIndex)
        .value(box.getValue)
        .transactionId(box.getTransactionId)
        .creationHeight(box.getCreationHeight)
        .assets(tokens)
        .additionalRegisters(registers)
      return ScalaBridge.isoErgoTransactionOutput.to(boxConversion)
    }
    val tokens = new util.ArrayList[Asset](response.getAssets.size)
    for (asset <- response.getAssets) {
      tokens.add(new Asset().tokenId(asset.getTokenId).amount(asset.getAmount))
    }
    val registers = new Registers
    for (registerEntry <- response.getAdditionalRegisters.entrySet()) {
      registers.put(registerEntry.getKey, registerEntry.getValue)
    }
    val boxConversion: ErgoTransactionOutput = new ErgoTransactionOutput()
      .ergoTree(response.getErgoTree)
      .boxId(response.getBoxId)
      .index(response.getIndex)
      .value(response.getValue)
      .transactionId(response.getTransactionId)
      .creationHeight(response.getCreationHeight)
      .assets(tokens)
      .additionalRegisters(registers)
    ScalaBridge.isoErgoTransactionOutput.to(boxConversion)
  }

  def getErgoBoxfromIDNoApi(box: InputInfo): ErgoBox = {

    val tokens = new util.ArrayList[Asset](box.getAssets.size)
    for (asset <- box.getAssets) {
      tokens.add(new Asset().tokenId(asset.getTokenId).amount(asset.getAmount))
    }
    val registers = new Registers
    for (registerEntry <- box.getAdditionalRegisters.entrySet) {
      registers.put(
        registerEntry.getKey,
        registerEntry.getValue.serializedValue
      )
    }
    val boxConversion: ErgoTransactionOutput = new ErgoTransactionOutput()
      .ergoTree(box.getErgoTree)
      .boxId(box.getBoxId)
      .index(box.getIndex)
      .value(box.getValue)
      .transactionId(null)
      .creationHeight(null)
      .assets(tokens)
      .additionalRegisters(registers)
    return ScalaBridge.isoErgoTransactionOutput.to(boxConversion)

  }

}

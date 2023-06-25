package types

import com.google.gson.GsonBuilder

import scala.io.Source

case class ContractErgoTree(
    singletonIssuerContractUsePool: String,
    singletonIssuerContractNoPool: String,
    collectionIssuerContract: String,
    preMintIssuerContract: String,
    whitelistIssuerContract: String,
    proxyContract: String,
    minerFeeNanoErg: Long
)

object contractErgoTreeHelper {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): ContractErgoTree = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[ContractErgoTree])
  }

  def readJsonString(jsonString: String): ContractErgoTree = {
    gson
      .fromJson(jsonString, classOf[ContractErgoTree])
  }

  def toJsonString(json: ContractErgoTree): String = {
    this.gson.toJson(json)
  }

}

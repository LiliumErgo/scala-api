package configs

import com.google.gson.{Gson, GsonBuilder, JsonElement}

import java.io.{FileWriter, Writer}
import scala.io.Source

case class ContractsConfig(
    Contracts: Config
)

case class Config(
    stateContract: StateContract,
    issuerContract: IssuerContract,
    proxyContract: ProxyContract,
    collectionToken: String
)
case class StateContract(
    contract: String,
    singleton: String
)
case class IssuerContract(
    contract: String
)
case class ProxyContract(
    contract: String
)

class conf(
    stateContract: String,
    singleton: String,
    issuerContract: String,
    proxyContract: String,
    collectionToken: String
) {
  val stateContractInstance: StateContract =
    StateContract(stateContract, singleton)
  val issuerContractInstance: IssuerContract =
    IssuerContract(issuerContract)
  val proxyContractInstance: ProxyContract = ProxyContract(proxyContract)

  val conf = Config(
    stateContractInstance,
    issuerContractInstance,
    proxyContractInstance,
    collectionToken
  )
  val newConfig: ContractsConfig = ContractsConfig(conf)
  private val gson = new GsonBuilder().setPrettyPrinting().create()

  def write(filePath: String): Unit = {
    val writer: Writer = new FileWriter(filePath)
    writer.write(this.gson.toJson(this.newConfig))
    writer.close()
  }

  def read(filePath: String): ContractsConfig = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[ContractsConfig])
  }

  def read: ContractsConfig = {
    gson.fromJson(this.gson.toJson(this.newConfig), classOf[ContractsConfig])
  }

  def print: String = {
    this.gson.toJson(this.newConfig)
  }

}

object conf {
  private val gson = new GsonBuilder().setPrettyPrinting().create()

  def read(filePath: String): ContractsConfig = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[ContractsConfig])
  }

  def toJsonString(json: ContractsConfig): String = {
    this.gson.toJson(json)
  }

  def write(filePath: String, newConfig: ContractsConfig): Unit = {
    val writer: Writer = new FileWriter(filePath)
    writer.write(this.gson.toJson(newConfig))
    writer.close()
  }

}

case class ServiceOwnerConfig(
    liliumTxOperatorMnemonic: String,
    liliumTxOperatorMnemonicPw: String,
    txOperatorMnemonic: String,
    txOperatorMnemonicPw: String,
    liliumFeeAddress: String,
    liliumFeePercent: Long,
    minTxOperatorFeeNanoErg: Long,
    minerFeeNanoErg: Long,
    nodeUrl: String,
    apiUrl: String,
    dataBaseKey: String,
    dataBaseURL: String
)

object serviceOwnerConf {
  private val gson = new GsonBuilder().setPrettyPrinting().create()

  def read(filePath: String): ServiceOwnerConfig = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[ServiceOwnerConfig])
  }
}

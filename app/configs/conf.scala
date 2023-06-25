package configs

import com.google.gson.{Gson, GsonBuilder, JsonElement}

import java.io.{FileWriter, Writer}
import scala.io.Source

case class ContractsConfig(
    Contracts: Config
)

case class Config(
    artistTransaction: String,
    stateContract: StateContract,
    issuerContract: IssuerContract,
    proxyContract: ProxyContract,
    saleLPContract: SaleLPContract,
    collectionToken: Token,
    preMintToken: Token,
    whitelistToken: Token,
    paymentToken: Token
)

case class Token(
    tokenID: String,
    tokenAmount: Long
)
case class StateContract(
    contract: String,
    singleton: Token
)
case class IssuerContract(
    contract: String
)
case class ProxyContract(
    contract: String
)

case class SaleLPContract(
    contract: String,
    singleton: Token
)

class conf(
    artistTransaction: String,
    stateContract: String,
    singleton: String,
    singletonAmount: Long,
    issuerContract: String,
    proxyContract: String,
    saleLPContract: String,
    saleLPSingleton: String,
    saleLPSingletonAmount: Long,
    collectionToken: String,
    collectionTokenAmount: Long,
    preMintToken: String,
    premintTokenAmount: Long,
    whitelistToken: String,
    whitelistTokenAmount: Long,
    paymentToken: String,
    paymentTokenAmount: Long
) {
  private val stateContractInstance: StateContract =
    StateContract(stateContract, Token(singleton, singletonAmount))
  private val issuerContractInstance: IssuerContract =
    IssuerContract(issuerContract)
  private val proxyContractInstance: ProxyContract = ProxyContract(
    proxyContract
  )
  private val saleLPContractInstance: SaleLPContract =
    SaleLPContract(saleLPContract, Token(saleLPSingleton, saleLPSingletonAmount))

  val conf: Config = Config(
    artistTransaction,
    stateContractInstance,
    issuerContractInstance,
    proxyContractInstance,
    saleLPContractInstance,
    Token(collectionToken, collectionTokenAmount),
    Token(preMintToken, premintTokenAmount),
    Token(whitelistToken, whitelistTokenAmount),
    Token(paymentToken, paymentTokenAmount)
  )
  private val newConfig: ContractsConfig = ContractsConfig(conf)
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
    extraFeaturePercent: Long,
    minTxOperatorFeeNanoErg: Long,
    minerFeeNanoErg: Long,
    minBoxValueNanoErg: Long,
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

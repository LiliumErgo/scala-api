package utils

import org.ergoplatform.appkit._
import scorex.crypto.hash

class ContractCompile(ctx: BlockchainContext) {

  def compileSingletonIssuerContract(
      contract: String,
      operator: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_TxOperatorPK", operator.getPublicKey)
        .build(),
      contract
    )
  }

  def compileSingletonIssuanceContract(
      contract: String,
      singleton: ErgoToken,
      operator: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_SingletonToken", singleton.getId.getBytes)
        .item("_TxOperatorPK", operator.getPublicKey)
        .build(),
      contract
    )
  }

  def compileCollectionIssuerContract(
      contract: String,
      operator: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_TxOperatorPK", operator.getPublicKey)
        .build(),
      contract
    )
  }

  def compileCollectionIssuanceContract(
      contract: String,
      operator: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_TxOperatorPK", operator.getPublicKey)
        .build(),
      contract
    )
  }

  def compileProxyContract(
      contract: String,
      minerFee: Long
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_minerFee", minerFee)
        .build(),
      contract
    )
  }

  def compileStateContract(
      contract: String,
      proxyContract: ErgoContract,
      issuerContract: ErgoContract,
      artistAddress: Address,
      royaltyBlakeHash: Array[Byte],
      collectionToken: ErgoToken,
      singletonToken: ErgoToken,
      priceOfNFTNanoErg: Long,
      liliumFeeAddress: Address,
      liliumFeeNanoErg: Long,
      minTxOperatorFeeNanoErg: Long,
      minerFee: Long
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_artistSigmaProp", artistAddress.getPublicKey)
        .item(
          "_proxyContractBytes",
          proxyContract.toAddress.asP2S().scriptBytes
        )
        .item(
          "_issuerContractBytes",
          issuerContract.toAddress.asP2S().scriptBytes
        )
        .item("_royaltyBlakeHash", royaltyBlakeHash)
        .item("_collectionToken", collectionToken.getId.getBytes)
        .item("_singletonToken", singletonToken.getId.getBytes)
        .item("_priceOfNFT", priceOfNFTNanoErg)
        .item("_liliumSigmaProp", liliumFeeAddress.getPublicKey)
        .item("_liliumFee", liliumFeeNanoErg)
        .item("_txOperatorFee", minTxOperatorFeeNanoErg)
        .item("_minerFee", minerFee)
        .build(),
      contract
    )
  }

  def compileIssuerContract(contract: String): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .build(),
      contract
    )
  }
}

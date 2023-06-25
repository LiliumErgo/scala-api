package utils

import org.ergoplatform.appkit._

class ContractCompile(ctx: BlockchainContext) {

  def compileSingletonIssuerContract(
      contract: String,
      operator: Address,
      usePool: Boolean,
      minerFee: Long
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_txOperatorPK", operator.getPublicKey)
        .item("_usePool", usePool)
        .item("_minerFee", minerFee)
        .build(),
      contract
    )
  }

  def compileSingletonIssuanceContract(
      contract: String,
      singleton: ErgoToken,
      operator: Address,
      usePool: Boolean,
      totalFees: Long
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_singletonToken", singleton.getId.getBytes)
        .item("_usePool", usePool)
        .item("_totalFees", totalFees)
        .item("_txOperatorPK", operator.getPublicKey)
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
        .item("_txOperatorPK", operator.getPublicKey)
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
        .item("_txOperatorPK", operator.getPublicKey)
        .build(),
      contract
    )
  }

  def compileWhitelistIssuerContract(
      contract: String,
      operator: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_txOperatorPK", operator.getPublicKey)
        .build(),
      contract
    )
  }

  def compilePreMintIssuerContract(
      contract: String,
      operator: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_txOperatorPK", operator.getPublicKey)
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
      paymentTokenAmount: Long,
      liliumFeeAddress: Address,
      liliumFeePercent: Long,
      minTxOperatorFeeNanoErg: Long,
      minerFee: Long,
      minBoxValue: Long
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
        .item("_paymentTokenAmount", paymentTokenAmount)
        .item("_liliumSigmaProp", liliumFeeAddress.getPublicKey)
        .item("_liliumFeeNum", liliumFeePercent)
        .item("_liliumFeeDenom", 100)
        .item("_txOperatorFee", minTxOperatorFeeNanoErg)
        .item("_minerFee", minerFee)
        .item("_minBoxValue", minBoxValue)
        .build(),
      contract
    )
  }

  def compileSaleLP(
      contract: String,
      minBoxValue: Long,
      minerFee: Long,
      minTxOperatorFeeNanoErg: Long,
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_minBoxValue", minBoxValue)
        .item("_minerFee", minerFee)
        .item("_txOperatorFee", minTxOperatorFeeNanoErg)
        .build(),
      contract
    )
  }

  def compileIssuerContract(contract: String, minerFee: Long): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("_minerFee", minerFee)
        .build(),
      contract
    )
  }
}

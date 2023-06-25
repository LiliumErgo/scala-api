{

    // ===== Contract Description ===== //
    // Name: Singleton Issuer Contract
    // Description: Although an issuer contract is not required for a singleton token, this proxy required so all transactions are processed with only one transaction from the client
    // Version: 1.0.0
    // Author: mgpai22@github.com
    // Auditor: lucagdangelo@github.com

    // ===== Compile Time Constants ===== //
    // _usePool: Boolean
    // _txOperatorPK: SigmaProp
    // _minerFee

    // ===== Context Extension Variables ===== //
    val singletonIssuanceContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val nftBox = if (_usePool) (OUTPUTS(0).tokens(0) == (SELF.id, 2L)) else (OUTPUTS(0).tokens(0) == (SELF.id, 1L))
    val properOutput = OUTPUTS(0).propositionBytes == singletonIssuanceContractBytes
    val properValueTransfer = OUTPUTS(0).value == SELF.value - _minerFee

   sigmaProp(nftBox && properOutput && properValueTransfer) && _txOperatorPK

}

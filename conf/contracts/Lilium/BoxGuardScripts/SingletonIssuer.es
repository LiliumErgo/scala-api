{

    // ===== Contract Description ===== //
    // Name: Singleton Issuer Contract
    // Description: Although an issuer contract is not required for a singleton token, this proxy required so all transactions are processed with only one transaction from the client
    // Version: 1.0.0
    // Author: mgpai22@github.com
    // Auditor: lucagdangelo@github.com


    // ===== Compile Time Constants ===== //
    // _TxOperatorPK: SigmaProp

    // ===== Context Extension Variables ===== //
   val SingletonIssuanceContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get


   val nftBox = OUTPUTS(0).tokens(0) == (SELF.id, 1L)
   val properOutput = OUTPUTS(0).propositionBytes == SingletonIssuanceContractBytes

   sigmaProp(nftBox && properOutput) && _TxOperatorPK

}

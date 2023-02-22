{

    // ===== Contract Description ===== //
    // Name: Collection Issuer Contract
    // Description: In order to create collection tokens, it needs metadata written in the issuer box. Once the box is created, it needs to be guarded by a contract that
    //              enforces the proper amount of collection tokens will be minted when the issuer box is spent.
    // Version: 1.0.0
    // Author: mgpai22@github.com
    // Auditor: lucagdangelo@github.com


    // ===== Box Registers ===== //
    // R4: Int => Collection Standard Version
    // R5: Coll[Coll[Byte]] => Collection Info
    // R6: Coll[(Coll[Byte], Coll[Byte]) => Collection Socials
    // R7: Long => Minting Expiry Timestamp in MS
    // R8: Coll[(Coll[Byte], Coll[Byte])] => Additional information
    // R9: Long => Amount Collection Tokens

    // ===== Compile Time Constants ===== //
    // _TxOperatorPK: SigmaProp

    // ===== Context Extension Variables ===== //
   val CollectionIssuanceContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get


   val nftBox = OUTPUTS(0).tokens(0) == (SELF.id, SELF.R9[Long].get)
   val properOutput = OUTPUTS(0).propositionBytes == CollectionIssuanceContractBytes

   sigmaProp(nftBox && properOutput) && _TxOperatorPK

}

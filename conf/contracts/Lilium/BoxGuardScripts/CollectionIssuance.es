{

    // ===== Contract Description ===== //
    // Name: Collection Issuance Contract
    // Description: Collection Tokens are required . In order to create the state box, the state box needs the collection token ID as a constant. Therefore, the
    //                tokens cannot be directly minted to the state box. This contract acts as a proxy to hold the collection tokens before it is sent to the newly created state box.
    // Version: 1.0.0
    // Author: mgpai22@github.com
    // Auditor: lucagdangelo@github.com


    // ===== Box Registers ===== //
    // Tokens: Collection Token

    // ===== Compile Time Constants ===== //
    // _TxOperatorPK: SigmaProp

    // ===== Context Extension Variables ===== //
    val StateBoxContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val CollectionIssuerBox: Box = getVar[Box](1).get

    val properOutput = OUTPUTS(0).propositionBytes == StateBoxContractBytes
    val properTokenTransfer = (OUTPUTS(0).tokens(1) == (CollectionIssuerBox.id, CollectionIssuerBox.R9[Long].get)) && (SELF.tokens(0)._1  == CollectionIssuerBox.id)

    sigmaProp(properOutput && properTokenTransfer) && _TxOperatorPK

}

{

    // ===== Contract Description ===== //
    // Name: Singleton Issuance Contract
    // Description: A singleton token is required to track the state box. In order to create the state box, the state box needs the singleton token ID as a constant. Therefore, the
    //                singleton cannot be directly minted to the state box. This contract acts as a proxy to hold the singleton before it is sent to the newly created state box.
    // Version: 1.0.0
    // Author: mgpai22@github.com
    // Auditor: lucagdangelo@github.com

    // ===== Box Registers ===== //
    // Tokens: Singleton Token

    // ===== Compile Time Constants ===== //
    // _SingletonToken: Coll[Byte]
    // _TxOperatorPK: SigmaProp

    // ===== Context Extension Variables ===== //
    val StateBoxContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get

    val properOutput = OUTPUTS(0).propositionBytes == StateBoxContractBytes
    val properTokenTransfer = (OUTPUTS(0).tokens(0) == (_SingletonToken, 1L)) && (SELF.tokens(0)._1  == _SingletonToken)

    sigmaProp(properOutput && properTokenTransfer) && _TxOperatorPK

}

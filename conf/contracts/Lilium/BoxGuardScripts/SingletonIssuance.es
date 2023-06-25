{

    // ===== Contract Description ===== //
    // Name: Singleton Issuance Contract
    // Description: A singleton token is required to track the state box. In order to create the state box, the state box needs the singleton token ID as a constant. Therefore, the
    //                singleton cannot be directly minted to the state box. This contract acts as a proxy to hold the singleton before it is sent to the newly created state box.
    // Version: 1.0.0
    // Author: mgpai22@github.com
    // Auditor: lucagdangelo@github.com

    // ===== Box Contents ===== //
    // Tokens
    // 1. (SingletonTokenId, 1 | 2)

    // ===== Compile Time Constants ===== //
    // _singletonToken: Coll[Byte]
    // _usePool: Boolean
    // _totalFees: Long
    // _txOperatorPK: SigmaProp

    // ===== Context Extension Variables ===== //
    val singletonIssuanceContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val properOutput: Boolean = OUTPUTS(0).propositionBytes == singletonIssuanceContractBytes

    val properTokenTransfer: Boolean = {

        if (_usePool) {

            val saleLPContractBytes: Coll[Byte] = getVar[Coll[Byte]](1).get
            val properSaleLP: Boolean = OUTPUTS(1).propositionBytes == saleLPContractBytes

            allOf(Coll(
                (OUTPUTS(0).tokens(0) == (_singletonToken, 1L)), // state box
                (OUTPUTS(1).value == _totalFees),
                properSaleLP,
                (OUTPUTS(1).tokens(0) == (_singletonToken, 1L)), // sale lp box
                (SELF.tokens(0)._1  == _singletonToken)
            ))

        } else {

            allOf(Coll(
                (OUTPUTS(0).tokens(0) == (_singletonToken, 1L)),
                (SELF.tokens(0)._1  == _singletonToken)
            ))

        }

    }

    sigmaProp(properOutput && properTokenTransfer) && _txOperatorPK

}

{
    // ===== Contract Description ===== //
    // Name: NFT Buyer Proxy Contract
    // Description: This contract is a proxy contract and ensures funds are used for NFTs or are refunded.
    // Version: 1.0.0
    // Author: mgpai22@github.com

    // ===== Box Contents ===== //
    // Tokens
    // 1. (PaymentToken | PreMintToken | WhitelistToken, PaymentTokenAmount | 1 | 1) // only present if the sale requires a custom token, premint token, or whitelist token
    // Registers
    // R4: SigmaProp => Buyer SigmaProp
    // R5: Coll[Byte] => State Box Singleton

    // ===== Compile Time Constants ===== //
    // _minerFee: Long

    // ===== Context Extension Variables ===== //
    // None

    val buyerPK: SigmaProp = SELF.R4[SigmaProp].get
    val stateBoxSingleton: Coll[Byte] = SELF.R5[Coll[Byte]].get
    val validStateBox: Boolean = INPUTS(0).tokens.size > 0 && INPUTS(0).tokens(0)._1 == stateBoxSingleton

    if (validStateBox) {

        val validNFTSaleTx: Boolean = {

            // outputs
            val issuerBoxOUT: Box = OUTPUTS(0)

            val validIssuerBox: Boolean = {
                (issuerBoxOUT.R9[(SigmaProp, Long)].get._1 == buyerPK) // check that issuer box has the buyer sigmaprop
            }

            validIssuerBox

        }

        sigmaProp(validNFTSaleTx)

    } else {

        val validRefundTx: Boolean = {

            val validRefundBox: Boolean = {

                //ensures buyer receives total value of box
                val validValueTransfer: Boolean = OUTPUTS.map { (o: Box) =>
                    if (o.propositionBytes == buyerPK.propBytes) o.value else 0L
                }.fold(0L, { (a: Long, b: Long) => a + b }) >= SELF.value

                // if box has tokens it must go to buyer
                val validTokenTransfer: Boolean = {
                    if(SELF.tokens.size > 0){
                        OUTPUTS.map { (o: Box) =>
                            if ((o.tokens == SELF.tokens) && (o.propositionBytes == buyerPK.propBytes)) 1L else 0L
                        }.fold(0L, { (a: Long, b: Long) => a + b }) >= 1L
                    } else{
                      true
                    }
                }


                allOf(Coll(
                    validValueTransfer,
                    validTokenTransfer
                ))
            }

            val validMinerFee: Boolean = OUTPUTS.map { (o: Box) =>
                if (blake2b256(o.propositionBytes) == fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")) o.value else 0L
            }.fold(0L, { (a: Long, b: Long) => a + b }) == _minerFee

            allOf(Coll(
                validRefundBox,
                validMinerFee
            ))

        }

        sigmaProp(validRefundTx) && buyerPK // buyer must sign tx themself as well

    }

}

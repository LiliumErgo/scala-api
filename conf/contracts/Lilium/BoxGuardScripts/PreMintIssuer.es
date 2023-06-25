{
    // ===== Contract Description ===== //
    // Name: PreMint Issuer Contract
    // Description: Used to guard the issuer box of minting the pre-mint tokens.
    // Version: 1.0.0
    // Author: lucagdangelo@github.com
    // Auditor: mgpai22@github.com

    // ===== Box Contents ===== //
    // Registers
    // R4: Long PreMintTokenAmount

    // ===== Compile Time Constants ===== //
    // _txOperatorPK: SigmaProp

    // ===== Context Extension Variables ===== //
    // None

    val validPreMintMintingTx: Boolean = {

        val validPreMintIssuanceBox: Boolean = {

            val preMintTokenAmount = SELF.R4[Long].get
            val userPk = SELF.R5[SigmaProp].get
            val validTokens: Boolean = (OUTPUTS(0).tokens(0) == (SELF.id, preMintTokenAmount))
            val validUser: Boolean = (OUTPUTS(0).propositionBytes == userPk.propBytes)

            allOf(Coll(
                validTokens,
                validUser
            ))

        }

        validPreMintIssuanceBox
    }

    sigmaProp(validPreMintMintingTx) && _txOperatorPK

}

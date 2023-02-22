{
      // ===== Contract Description ===== //
      // Name: NFT Issuer Contract
      // Description: This contract guards the issuer box. This box can only be spent to create an NFT if the NFT has the proper data in the issuance box.
      // Version: 1.0.0
      // Author: mgpai22@github.com
      // Auditor: lucagdangelo@github.com

      // ===== Box Registers ===== //
      // R4: Int => Artwork Standard Version
      // R5: Coll[(Coll[Byte], Int)] => Royalty
      // R6: (Coll[(Coll[Byte]Coll[Byte])],(Coll[(Coll[Byte],(Int, Int))],Coll[(Coll[Byte],(Int, Int))])) => metadata
      // R7: Coll[Byte] => Collection Token ID
      // R8: Coll[(Coll[Byte], Coll[Byte])] => Additional Information
      // R9: Coll[(SigmaProp, Long)] => Buyer SigmaProp and Index



      // ===== Compile Time Constants ===== //
      // None


      // ===== Context Extension Variables ===== //
    val proof = getVar[Coll[Byte]](0).get


    val buyerPropBytes: SigmaProp = SELF.R9[(SigmaProp, Long)].get._1.propBytes
    val index: Long = SELF.R9[(SigmaProp, Long)].get._2

    val salesContractInput = CONTEXT.dataInputs(0) //ensure to check this is the legit stateContract with a singleton or something
    val metadataTree = salesContractInput.R4[AvlTree].get
    val key = longToByteArray(index)
    val keyHash = blake2b256( key )
    val valueFromAvlTree: Coll[Byte] = metadataTree.get(keyHash, proof).get

    val nftBox = OUTPUTS(0).tokens(0)._1 == SELF.id && OUTPUTS(0).tokens.size == 1L

    val R4 = OUTPUTS(0).R4[Coll[Byte]].get //name
    val R5 = OUTPUTS(0).R5[Coll[Byte]].get //desc
    val R6 = OUTPUTS(0).R6[Coll[Byte]].get // #decimals
    val R7 = OUTPUTS(0).R7[Coll[Byte]].get // asset type
    val R8 = OUTPUTS(0).R8[Coll[Byte]].get // SHA of media
    val R9 = OUTPUTS(0).R9[Coll[Byte]].get // media link


    val properRecipient = OUTPUTS(0).propositionBytes == buyerPropBytes


    val validNFT = blake2b256( longToByteArray(R4.size.toLong) ++ R4 ++ longToByteArray(R5.size.toLong) ++ R5 ++  longToByteArray(R7.size.toLong) ++ R7 ++  longToByteArray(R8.size.toLong) ++ R8 ++  longToByteArray(R9.size.toLong) ++ R9 ) == blake2b256( valueFromAvlTree )


    sigmaProp( nftBox && validNFT && (R6 == fromBase64("MA==")) && properRecipient )

}




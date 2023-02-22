{
   // ===== Contract Description ===== //
   // Name: NFT State Contract
   // Description: An NFT requires the bulk of its metadata to come from an issuer box. In order to ensure the proper metadata is placed into the issuer box, this contract is used.
   //              For more information, refer to https://github.com/anon-real/eips/blob/master/eip-0024.md
   // Version: 1.0.0
   // Author: mgpai22@github.com
   // Auditor: lucagdangelo@github.com

   // ===== Box Registers ===== //
   // R4: AvlTree => Issuance Box AVL
   // R5: AvlTree => Issuer Box AVL
   // R6: Long => Index
   // R7: (Long, Long) => Sale Starting and Ending Timestamps
   // R8: Boolean => If true, collection tokens will be returned to artist after expiry if there are NFTs left

   // ===== Compile Time Constants ===== //
   // _artistSigmaProp: SigmaProp
   // _proxyContractBytes: Coll[Byte]
   // _issuerContractBytes: Coll[Byte]
   // _royaltyBlakeHash: Coll[Byte]
   // _collectionToken: Coll[Byte]
   // _singletonToken: Coll[Byte]
   // _priceOfNFT: Long
   // _liliumSigmaProp: SigmaProp
   // _liliumFee: Long
   // _minerFee: Long
   // _txOperatorFee: Long

   // ===== Relevant Variables ===== //
   val hasSaleStarted: Boolean = SELF.R7[(Long, Long)].get._1 <= CONTEXT.headers(0).timestamp
   val hasSaleEnded: Boolean = SELF.R7[(Long, Long)].get._2 <= CONTEXT.headers(0).timestamp
   val isInfiniteSale: Boolean = (SELF.R7[(Long, Long)].get._2 == -1L)
   val isReturn: Boolean = SELF.R8[Boolean].get

   if (hasSaleStarted) {

      if (!hasSaleEnded || isInfiniteSale) {

      // ===== Relevant Variables ===== //
      val proof: Coll[Byte] = getVar[Coll[Byte]](0).get
      val collectionIssuerBox: Box = getVar[Box](1).get
      val tree: AvlTree = SELF.R5[AvlTree].get
      val index: Long = SELF.R6[Long].get // metadata index, the value for index in the init tx must be 1L, AVLTREE(0) does not exist, it starts at 1
      val key: Coll[Byte] = longToByteArray(index)
      val keyHash: Coll[Byte] = blake2b256(key)
      val avlData: Coll[Byte] = tree.get(keyHash, proof).get
      val delimiter: Coll[Byte] = fromBase16("7f000fbaf55fab7e")

      val nftSupplyCap: Long = collectionIssuerBox.R9[Long].get
      val isLastSale: Boolean = (index + 1 == nftSupplyCap)

      val validSaleTx: Boolean = {

         // inputs
         val buyerProxyBox: Box = INPUTS(1)

         // outputs
         val issuerBoxOUT: Box = OUTPUTS(0)
         val stateBoxOUT: Box = OUTPUTS(1)
         val userBoxOUT: Box = OUTPUTS(2)
         val liliumBoxOUT: Box = OUTPUTS(3)
         val minerBoxOUT: Box = OUTPUTS(4)
         val txOperatorBoxOUT: Box = OUTPUTS(5)

         val validCollection: Boolean = (collectionIssuerBox.id == _collectionToken)

         val validSupplyCap: Boolean = (index + 1 <= nftSupplyCap)

         val validProxyBox: Boolean = (buyerProxyBox.propositionBytes == _proxyContractBytes)

         val validIssuerBox: Boolean = {

            val royalty = issuerBoxOUT.R5[Coll[(Coll[Byte], Int)]].get
            val metadata = issuerBoxOUT.R6[(Coll[(Coll[Byte],Coll[Byte])],(Coll[(Coll[Byte],(Int,Int))],Coll[(Coll[Byte],(Int,Int))]))].get
            val traits: Coll[(Coll[Byte],Coll[Byte])] = metadata._1
            val levels: Coll[(Coll[Byte],(Int,Int))] = metadata._2._1
            val stats: Coll[(Coll[Byte],(Int,Int))] = metadata._2._2

            val traitsBytes = (traits.fold(longToByteArray(0L), { (a: Coll[Byte], b: (Coll[Byte],Coll[Byte])) => a ++ longToByteArray(b._1.size.toLong) ++ b._1 ++ longToByteArray(b._2.size.toLong) ++ b._2 })) ++ delimiter
            val levelsBytes = (levels.fold(traitsBytes, { (a: Coll[Byte], b: (Coll[Byte],(Int,Int))) => a ++ longToByteArray(b._1.size.toLong) ++ b._1 ++ longToByteArray(b._2._1.toLong) ++ longToByteArray(b._2._2.toLong) })) ++ delimiter
            val statsBytes = stats.fold(levelsBytes, { (a: Coll[Byte], b: (Coll[Byte],(Int,Int))) => a ++ longToByteArray(b._1.size.toLong) ++ b._1 ++ longToByteArray(b._2._1.toLong) ++ longToByteArray(b._2._2.toLong) })
            val royaltyBytes = royalty.fold(longToByteArray(0L), { (a: Coll[Byte], b: (Coll[Byte], Int)) => a ++ b._1 ++ longToByteArray(b._2.toLong) })

            allOf(Coll(
               (issuerBoxOUT.propositionBytes == _issuerContractBytes), // correct contract
               (issuerBoxOUT.tokens(0) == (_collectionToken, 1L)), // transfer one collection token to the issuer box
               (issuerBoxOUT.R4[Int].get == 2), // correct standard version
               (blake2b256(royaltyBytes) == _royaltyBlakeHash), // correct royalty
               (blake2b256(statsBytes) == blake2b256(avlData)), // correct data from avl tree
               (issuerBoxOUT.R7[Coll[Byte]].get == _collectionToken), // collection token id stored in register of issuer box
               (issuerBoxOUT.R9[(SigmaProp, Long)].get == (buyerProxyBox.R4[SigmaProp].get, index)) // correct buyer SigmaProp and NFT index
            ))

         }

         val validStateBox: Boolean = {

            val validTokens: Boolean = {

               if (!isLastSale) {

                  allOf(Coll(
                     (stateBoxOUT.tokens(0) == (SELF.tokens(0)._1, 1L)), // transfer the state box singleton token
                     (stateBoxOUT.tokens(0)._1 == _singletonToken), // check that the state box singleton token is the correct one
                     (stateBoxOUT.tokens(1) == (SELF.tokens(1)._1, SELF.tokens(1)._2 - 1L)), // amount of collection tokens reduced by 1
                     (stateBoxOUT.tokens(1)._1 == _collectionToken) // check that the collection token is correct the correct one

                  ))


               } else {

                  allOf(Coll(
                     (stateBoxOUT.tokens(0) == (SELF.tokens(0)._1, 1L)), // transfer the state box singleton token
                     (stateBoxOUT.tokens(0)._1 == _singletonToken), // check that the state box singleton token is the correct one
                     (stateBoxOUT.tokens.size == 1) // check that the singleton token is the only token left in the box
                  ))

               }

            }

            allOf(Coll(
               (stateBoxOUT.value == SELF.value), // transfer the box value
               (stateBoxOUT.propositionBytes == SELF.propositionBytes), // correct contract
               validTokens, // correct tokens
               (SELF.R4[AvlTree].get.digest == stateBoxOUT.R4[AvlTree].get.digest), // correct issuer avl tree
               (SELF.R5[AvlTree].get.digest == stateBoxOUT.R5[AvlTree].get.digest), // correct issuance avl tree
               (stateBoxOUT.R6[Long].get == index + 1L) // increment nft index
            ))

         }

         val validUserBox: Boolean = {

            allOf(Coll(
               (userBoxOUT.value == _priceOfNFT),
               (userBoxOUT.propositionBytes == _artistSigmaProp.propBytes)
            ))

         }

         val validLiliumBox: Boolean = {

            allOf(Coll(
               (liliumBoxOUT.value == _liliumFee),
               (liliumBoxOUT.propositionBytes == _liliumSigmaProp.propBytes)
            ))

         }

         val validMinerFee: Boolean = (minerBoxOUT.value == _minerFee)

         val validTxOperatorFee: Boolean = (txOperatorBoxOUT.value >= _txOperatorFee)

         allOf(Coll(
            validCollection,
            validSupplyCap,
            validProxyBox,
            validIssuerBox,
            validStateBox,
            validUserBox,
            validLiliumBox,
            validMinerFee,
            validTxOperatorFee,
            (OUTPUTS.size == 6)
         ))

      }

      sigmaProp(validSaleTx)

      } else {
// create box if sale has expired, do this by setting timestamp to like 5 minutes from now
            val validReturnOrBurnTx: Boolean = {

               // outputs
               val userBoxOUT: Box = OUTPUTS(0)
               val minerBoxOUT: Box = OUTPUTS(1)
               val txOperatorBoxOUT: Box = OUTPUTS(2)

               val validUserBox: Boolean = {

                val validTokenTransfer: Boolean = {

                    if (isReturn) {

                        allOf(Coll(
                            (userBoxOUT.tokens(0) == SELF.tokens(1)),
                            (userBoxOUT.tokens(0)._1 == _collectionToken)
                        ))

                    } else {
                    // maybe add condition here to ensure collection tokens get burned?
                        OUTPUTS.forall({(output: Box) => (output.tokens.size == 0)})
                    }

                }

                allOf(Coll(
                    (userBoxOUT.value == SELF.value - _minerFee - _txOperatorFee), //state box should have more than 0.001, it should have 0.001 + minerFee + operatorFee
                    (userBoxOUT.propositionBytes == _artistSigmaProp.propBytes),
                    validTokenTransfer
                ))

               }

               val validMinerFee: Boolean = (minerBoxOUT.value == _minerFee)

               val validTxOperatorFee: Boolean = (txOperatorBoxOUT.value >= _txOperatorFee)

               allOf(Coll(
                  validUserBox,
                  validMinerFee,
                  validTxOperatorFee,
                  (OUTPUTS.size == 3)
               ))

            }

            sigmaProp(validReturnOrBurnTx)

      }

   } else {
      sigmaProp(false)
   }
}

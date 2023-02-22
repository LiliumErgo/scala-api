package AVL.Export

import configs.frontendRespParser
import contracts.LiliumContracts
import initialize.initializeHelper

object avlTest extends App {
  val contract = LiliumContracts.CollectionIssuer.contractScript

  println(contract)
}

object metaTest extends App {
  val res = frontendRespParser.read("./frontendData.json")
  println(res.nft(0))
}

object txTest extends App {

  val jsonBody = frontendRespParser.read("frontendData.json")
  initializeHelper.main(
    jsonBody.transactionId,
    jsonBody.userPK,
    jsonBody.collectionDetails,
    jsonBody.nft
  )
}

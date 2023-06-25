package AVL.Export

import configs.frontendRespParser
import contracts.LiliumContracts
import initialize.InitializeHelper
import org.ergoplatform.appkit.BlockchainContext
import utils.{Client, ContractCompile}

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
  InitializeHelper.main(
    jsonBody.transactionId,
    jsonBody.userPK,
    jsonBody.collectionDetails,
    jsonBody.nft
  )
}

object compileProxy extends App{
  val client: Client = new Client()
  client.setClient
  val ctx: BlockchainContext = client.getContext

  val compilerObj = new ContractCompile(ctx)
  val contract = compilerObj.compileProxyContract(LiliumContracts.ProxyContract.contractScript, 1523000).toAddress

  println(contract.toString)

}

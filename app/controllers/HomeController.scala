package controllers

import AVL.Export.AVLExportTester.issuanceTree
import AVL.IssuerBox.IssuerHelpersAVL
import AVL.NFT.IssuanceAVLHelpers
import AVL.utils.avlUtils
import configs.{
  AVLJsonHelper,
  Collection,
  CollectionEncoder,
  Data,
  FrontendFile,
  SignedTransactionJson,
  SignedTransactionJsonParser,
  apiResp,
  collectionEncoderHelper,
  collectionParser,
  conf,
  frontendRespParser,
  masterMeta,
  serviceOwnerConf
}
import initialize.{InitializeHelper, encoderHelper}

import javax.inject._
import play.api.mvc._
import io.circe.Json
import play.api.libs.circe.Circe
import io.circe.syntax.EncoderOps
import play.api.libs.json.JsValue
import utils.{
  ArtistTxError,
  CoinGekoAPIError,
  DataBaseError,
  InvalidAddress,
  InvalidArtistTransaction,
  InvalidCollectionJsonFormat,
  InvalidCollectionSize,
  InvalidMetadata,
  InvalidNftFee,
  InvalidPaymentToken,
  InvalidPremintSetting,
  InvalidRoyalty,
  InvalidTimeStamp,
  InvalidTransactionB64,
  InvalidWhitelistSetting,
  explorerApi
}

import scala.concurrent.Future
import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import types.{ServiceConfig, contractErgoTreeHelper, serviceConfigHelper}

import scala.concurrent.{ExecutionContext, Future}

/** This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject() (cc: ControllerComponents)(implicit
    ec: ExecutionContext
) extends AbstractController(cc) {

  private lazy val contracts: types.ContractErgoTree =
    InitializeHelper.getContracts

  /** Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def postJson(): Action[JsValue] = Action(parse.json) {
    request: Request[JsValue] =>
      val jsonBody: Array[Data] =
        masterMeta.readJsonString(request.body.toString())
      Ok("JSON received")

  }

  def getServiceConf: Action[AnyContent] = Action {

    val serviceFilePath = "serviceOwner.json"
    val serviceConf = serviceOwnerConf.read(serviceFilePath)
    val serviceConfToOutput = ServiceConfig(
      serviceConf.liliumFeeAddress,
      serviceConf.liliumFeePercent,
      serviceConf.extraFeaturePercent,
      serviceConf.minerFeeNanoErg,
      serviceConf.minTxOperatorFeeNanoErg,
      serviceConf.dataBaseURL
    )
    Ok(serviceConfigHelper.toJsonString(serviceConfToOutput))
      .as("application/json")
  }

  def getContracts: Action[AnyContent] = Action {
    Ok(contractErgoTreeHelper.toJsonString(contracts)).as("application/json")
  }

  def generateCollectionIssuerHex(): Action[JsValue] = Action(parse.json) {
    request: Request[JsValue] =>
      val jsonBody: Collection =
        collectionParser.readJsonString(request.body.toString())
      val encoderHelper = new encoderHelper(jsonBody)

      val response = CollectionEncoder(
        encoderHelper.encodeCollectionInfo,
        encoderHelper.encodeSocials
      )

      Ok(collectionEncoderHelper.toJsonString(response)).as("application/json")
  }

  def submitSignedTransaction(): Action[JsValue] = Action(parse.json) {
    request: Request[JsValue] =>
      val exp = new explorerApi()

      val response = exp.sendTx(request.body.toString())

      Ok(
        new apiResp(
          response
        ).toJsonString
      ).as("application/json")
  }

  def submitCollection(): Action[JsValue] = Action.async(parse.json) {
    request =>
      val jsonBody: FrontendFile =
        frontendRespParser.readJsonString(request.body.toString())

      val futureResult = Future {
        InitializeHelper.main(
          jsonBody.transactionId,
          jsonBody.userPK,
          jsonBody.collectionDetails,
          jsonBody.nft
        )
      }
      futureResult
        .flatMap { res =>
          if (res._1 != 201) {
            Future.failed(new DataBaseError("Database Error"))
          } else {
            Future.successful(Ok(res._2).as("application/json"))
          }
        }
        .recover {
          case e: InvalidArtistTransaction =>
            BadRequest(
              new apiResp(
                "improper artist transaction submitted, please try again or contract support"
              ).toJsonString
            ).as("application/json")
          case e: DataBaseError =>
            BadRequest(
              new apiResp(
                "database error, please contract support"
              ).toJsonString
            ).as("application/json")
          case e: InvalidCollectionJsonFormat =>
            BadRequest(
              new apiResp(
                "The collection json format seems to be invalid. Please try again"
              ).toJsonString
            ).as("application/json")
          case e: InvalidTransactionB64 =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")
          case e: InvalidCollectionSize =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: InvalidNftFee =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: CoinGekoAPIError =>
            BadRequest(
              new apiResp(
                "coingeko api error please try again in a few minutes"
              ).toJsonString
            ).as("application/json")

          case e: InvalidAddress =>
            BadRequest(
              new apiResp(
                "Inputted Address Invalid"
              ).toJsonString
            ).as("application/json")

          case e: InvalidTimeStamp =>
            BadRequest(
              new apiResp(
                "Ensure that ending timestamp is greater than starting"
              ).toJsonString
            ).as("application/json")

          case e: InvalidRoyalty =>
            BadRequest(
              new apiResp(
                "Ensure that royalty address are correct"
              ).toJsonString
            ).as("application/json")

          case e: InvalidMetadata =>
            BadRequest(
              new apiResp(
                "Ensure that the metadata follows the proper format"
              ).toJsonString
            ).as("application/json")

          case e: InvalidPremintSetting =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: InvalidWhitelistSetting =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: InvalidPaymentToken =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: ArtistTxError =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: Exception =>
            println(e)
            BadRequest(
              new apiResp(
                "Something went wrong, please contract support"
              ).toJsonString
            ).as("application/json")
        }
  }

  def validateCollection(): Action[JsValue] = Action.async(parse.json) {
    request =>
      val jsonBody: FrontendFile =
        frontendRespParser.readJsonString(request.body.toString())

      val futureResult = Future {
        InitializeHelper.validate(
          jsonBody.transactionId,
          jsonBody.userPK,
          jsonBody.collectionDetails,
          jsonBody.nft
        )
      }
      futureResult
        .flatMap { res =>
          Future.successful(
            Ok(
              new apiResp(
                "Successful"
              ).toJsonString
            ).as("application/json")
          )
        }
        .recover {
          case e: InvalidArtistTransaction =>
            BadRequest(
              new apiResp(
                "improper artist transaction submitted, please try again or contract support"
              ).toJsonString
            ).as("application/json")
          case e: DataBaseError =>
            BadRequest(
              new apiResp(
                "database error, please contract support"
              ).toJsonString
            ).as("application/json")
          case e: InvalidCollectionJsonFormat =>
            BadRequest(
              new apiResp(
                "The collection json format invalid"
              ).toJsonString
            ).as("application/json")

          case e: InvalidCollectionSize =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: InvalidNftFee =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: CoinGekoAPIError =>
            BadRequest(
              new apiResp(
                "coingeko api error please try again in a few minutes"
              ).toJsonString
            ).as("application/json")

          case e: InvalidAddress =>
            BadRequest(
              new apiResp(
                "Inputted Address Invalid"
              ).toJsonString
            ).as("application/json")

          case e: InvalidTimeStamp =>
            BadRequest(
              new apiResp(
                "Ensure that ending timestamp is greater than starting"
              ).toJsonString
            ).as("application/json")

          case e: InvalidRoyalty =>
            BadRequest(
              new apiResp(
                "Ensure that royalty address are correct"
              ).toJsonString
            ).as("application/json")

          case e: InvalidMetadata =>
            BadRequest(
              new apiResp(
                "Ensure that the metadata follows the proper format"
              ).toJsonString
            ).as("application/json")

          case e: InvalidPremintSetting =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: InvalidWhitelistSetting =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: InvalidPaymentToken =>
            BadRequest(
              new apiResp(
                e.getMessage
              ).toJsonString
            ).as("application/json")

          case e: Exception =>
            println(e)
            BadRequest(
              new apiResp(
                "Oh no something went wrong, please contract support"
              ).toJsonString
            ).as("application/json")
        }
  }

}

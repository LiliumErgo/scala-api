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
  apiResp,
  collectionEncoderHelper,
  collectionParser,
  conf,
  frontendRespParser,
  masterMeta,
  serviceOwnerConf
}
import initialize.{encoderHelper, initializeHelper}

import javax.inject._
import play.api.mvc._
import io.circe.Json
import play.api.libs.circe.Circe
import io.circe.syntax.EncoderOps
import play.api.libs.json.JsValue
import utils.{
  DataBaseError,
  InvalidArtistTransaction,
  InvalidCollectionJsonFormat,
  InvalidCollectionSize
}

import scala.concurrent.Future
import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import types.{ServiceConfig, serviceConfigHelper}

import scala.concurrent.{ExecutionContext, Future}

/** This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject() (cc: ControllerComponents)(implicit
    ec: ExecutionContext
) extends AbstractController(cc) {

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
      serviceConf.liliumFeeNanoErg,
      serviceConf.minerFeeNanoErg,
      serviceConf.dataBaseURL
    )
    Ok(serviceConfigHelper.toJsonString(serviceConfToOutput))
      .as("application/json")
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

  def submitCollection(): Action[JsValue] = Action.async(parse.json) {
    request =>
      val jsonBody: FrontendFile =
        frontendRespParser.readJsonString(request.body.toString())

      val futureResult = Future {
        initializeHelper.main(
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
            Ok(
              new apiResp(
                "improper artist transaction submitted, please try again or contract support"
              ).toJsonString
            ).as("application/json")
          case e: DataBaseError =>
            Ok(
              new apiResp(
                "database error, please contract support"
              ).toJsonString
            ).as("application/json")
          case e: InvalidCollectionJsonFormat =>
            Ok(
              new apiResp(
                "The collection json format seems to be invalid. Please send again with the same txid."
              ).toJsonString
            ).as("application/json")

          case e: InvalidCollectionSize =>
            Ok(
              new apiResp(
                "collectionMaxSize does not match the length of the nft array"
              ).toJsonString
            ).as("application/json")

          case e: Exception =>
            println(e)
            Ok(
              new apiResp(
                "Oh no something went wrong, please contract support"
              ).toJsonString
            ).as("application/json")
        }
  }

}

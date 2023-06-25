package utils

import org.apache.http._
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair
import com.google.gson.Gson
import configs.serviceOwnerConf
import org.apache.http.entity.{ContentType, StringEntity}

import java.util

case class LiliumEntry(
    state_box_singleton_id: String,
    collection_id: String,
    start_timestamp: String,
    end_timestamp: String,
    user_pk: String,
    issuer_avl_bytes: String,
    issuance_avl_bytes: String,
    royalty_bytes: String
)

object DatabaseAPI {

  private val serviceFilePath = "serviceOwner.json"
  private lazy val serviceConf = serviceOwnerConf.read(serviceFilePath)

  private val apiKey = serviceConf.dataBaseKey
  private val tableEndpointURI = serviceConf.dataBaseURL

  def createArtistEntry(
      stateBoxSingleton: String,
      collectionId: String,
      startTimestamp: Long,
      endTimeStamp: Long,
      userPK: String,
      issuerAVL: String,
      issuanceAVL: String,
      royaltyHex: String
  ): Int = {
    val dbEntry = new LiliumEntry(
      stateBoxSingleton,
      collectionId,
      startTimestamp.toString,
      endTimeStamp.toString,
      userPK,
      issuerAVL,
      issuanceAVL,
      royaltyHex
    )
    val entryAsJson: String = new Gson().toJson(dbEntry)
    val requestEntity = new StringEntity(
      entryAsJson,
      ContentType.APPLICATION_JSON
    )

    val post = new HttpPost(
      tableEndpointURI
    )
    val nameValuePairs = new util.ArrayList[NameValuePair]()
    nameValuePairs.add(new BasicNameValuePair("JSON", entryAsJson))
    post.setEntity(requestEntity)

    post.setHeader("apikey", apiKey)
    post.setHeader(HttpHeaders.AUTHORIZATION, s"Bearer ${apiKey}")
    post.setHeader(HttpHeaders.CONTENT_TYPE, "application/json")

    // send the post request
    val client = HttpClients.custom().build()
    val response = client.execute(post)

    response.getStatusLine.getStatusCode

  }
}

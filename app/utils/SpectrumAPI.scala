package utils

import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import com.google.gson.GsonBuilder
import org.apache.http.util.EntityUtils

case class Pool(
    id: String,
    baseId: String,
    baseSymbol: String,
    quoteId: String,
    quoteSymbol: String,
    lastPrice: Double,
    baseVolume: Volume,
    quoteVolume: Volume
)

case class Volume(
    value: Long,
    units: SpectrumAsset,
    window: Window
)

case class SpectrumAsset(
    tokenId: String,
    ticker: String,
    decimals: Int
)

case class Window(
    from: String,
    to: String
)

object SpectrumAPI {

  def getAllPools: Map[String, Map[String, BigDecimal]] = {

    val currentTimestamp: Long = System.currentTimeMillis()
    val timestamp30daysAgo: Long = currentTimestamp - 2592000000L

    val spectrumURI =
      s"https://api.spectrum.fi/v1/price-tracking/markets?from=${timestamp30daysAgo}&to=${currentTimestamp}"
    val ERG_TOKEN_ID =
      "0000000000000000000000000000000000000000000000000000000000000000"

    val get = new HttpGet(spectrumURI)

    // send the get request
    val client = HttpClients.custom().build()
    val response = client.execute(get)

    // parse the response
    val entity = response.getEntity
    val content = EntityUtils.toString(entity)

    // convert the content to Array[Tickers]
    val gson = new GsonBuilder()
      .setPrettyPrinting()
      .create()

    val poolArray = gson.fromJson(content, classOf[Array[Pool]])

    def distinctBy[T, U](list: List[T], f: T => U): List[T] = {
      list
        .foldLeft(List.empty[T]) { (acc, x) =>
          if (acc.exists(y => f(x) == f(y))) acc
          else x :: acc
        }
        .reverse
    }

    val filtered = distinctBy(
      poolArray.filter(_.baseId == ERG_TOKEN_ID).toList,
      (p: Pool) => p.quoteId
    )

    filtered.map { r =>
      r.quoteId -> Map("erg" -> (BigDecimal(1) / r.lastPrice))
    }.toMap
  }

  def getERGPrice(tokenID: String): BigDecimal = {
    try {
      val tokenQuote = getAllPools.get(tokenID)
      if (tokenQuote.isEmpty) {
        -1
      } else {
        val ergPrice = tokenQuote.get("erg")
        if (ergPrice == None) {
          -1
        } else {
          ergPrice
        }
      }
    } catch {
      case _: Exception => -1
    }
  }
}

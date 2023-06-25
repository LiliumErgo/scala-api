package AVL.IssuerBox

import com.google.common.primitives.Longs
import io.getblok.getblok_plasma.ByteConversion
import org.bouncycastle.util.Strings
import org.bouncycastle.util.encoders.Hex

import scala.util.control.Breaks._
import org.ergoplatform.appkit.{ErgoValue, Iso}
import scorex.crypto.hash
import sigmastate.eval.Colls
import special.collection.Coll

import scala.collection.JavaConverters._
import scorex.crypto.hash
import utils.MetadataTranscoder

import scala.util.control.Breaks._
import java.nio.charset.StandardCharsets
import java.util
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class IssuerValue(
    explicit: Boolean,
    metaData: (
        Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])],
        (
            Coll[(Coll[java.lang.Byte], (Integer, Integer))],
            Coll[(Coll[java.lang.Byte], (Integer, Integer))]
        )
    )
) {
  def toBytes: Array[Byte] = {
    val traits = metaData._1
    val levels = metaData._2._1
    val stats = metaData._2._2

    val delimiter: Array[Byte] =
      Array[Byte](
        0x7f.toByte,
        0x00.toByte,
        0x0f.toByte,
        0xba.toByte,
        0xf5.toByte,
        0x5f.toByte,
        0xab.toByte,
        0x7e.toByte
      )

    //    val hexString = delimiter.map("%02x".format(_)).mkString

    var traitsBytes: Array[Byte] = {
      if (explicit) {
        Longs.toByteArray(1)
      } else {
        Longs.toByteArray(0)
      }
    }

    for (element <- traits.toArray) {
      val key: Array[Byte] =
        element._1.map(_.byteValue()).toArray
      val value: Array[Byte] =
        element._2.map(_.byteValue()).toArray

      traitsBytes = traitsBytes ++ Longs.toByteArray(key.length) ++ key ++ Longs
        .toByteArray(value.length) ++ value
    }
    traitsBytes = traitsBytes ++ delimiter

    for (element <- levels.toArray) {
      val key = element._1.map(_.toByte).toArray
      val value = element._2
      val intOne: Long = value._1.toLong
      val intTwo: Long = value._2.toLong

      traitsBytes = traitsBytes ++ Longs.toByteArray(key.length) ++ key ++ Longs
        .toByteArray(
          intOne
        ) ++ Longs.toByteArray(
        intTwo
      )
    }
    traitsBytes = traitsBytes ++ delimiter

    for (element <- stats.toArray) {
      val key = element._1.map(_.toByte).toArray
      val value = element._2
      val intOne: Long = value._1.toLong
      val intTwo: Long = value._2.toLong

      traitsBytes = traitsBytes ++ Longs.toByteArray(key.length) ++ key ++ Longs
        .toByteArray(
          intOne
        ) ++ Longs.toByteArray(
        intTwo
      )
    }
    traitsBytes
  }

}

object IssuerValue {
  implicit val IssuerValueConversion: ByteConversion[IssuerValue] =
    new ByteConversion[IssuerValue] {
      override def convertToBytes(t: IssuerValue): Array[Byte] = t.toBytes
      override def convertFromBytes(bytes: Array[Byte]): IssuerValue = {

        val textualTraitsMap: mutable.LinkedHashMap[String, String] =
          mutable.LinkedHashMap()

        val levelsMap: mutable.LinkedHashMap[String, (Int, Int)] =
          mutable.LinkedHashMap()
        val statsMap: mutable.LinkedHashMap[String, (Int, Int)] =
          mutable.LinkedHashMap()

        val delimiter: Array[Byte] =
          Array[Byte](
            0x7f.toByte,
            0x00.toByte,
            0x0f.toByte,
            0xba.toByte,
            0xf5.toByte,
            0x5f.toByte,
            0xab.toByte,
            0x7e.toByte
          )

        val initialBytes = Longs.toByteArray(0L)
        val explicit: Boolean =
          bytes.slice(0, initialBytes.length).exists(_ != 0.toByte)
        val data = bytes.slice(initialBytes.length, bytes.length)

        var index = 0

        var found = true
        while (index < data.length && found) {

          if (data.slice(index, index + 8) sameElements delimiter) {
            index = index + 8
            found = false
          } else {
            val keyLength =
              Longs.fromByteArray(data.slice(index, index + 8)).toInt
            val keyBytes: Array[Byte] =
              data.slice(index + 8, keyLength + index + 8)

            val valueLength = Longs
              .fromByteArray(
                data.slice(keyLength + index + 8, keyLength + index + 16)
              )
              .toInt
            val valueBytes: Array[Byte] = data.slice(
              keyLength + index + 16,
              keyLength + index + valueLength + 16
            )

            textualTraitsMap += (Strings.fromUTF8ByteArray(keyBytes) -> Strings
              .fromUTF8ByteArray(valueBytes))

            index = keyLength + valueLength + index + 16
          }
        }

        found = true
        while (index < data.length && found) {

          if (data.slice(index, index + 8) sameElements delimiter) {
            index = index + 8
            found = false
          } else {
            val keyLength =
              Longs.fromByteArray(data.slice(index, index + 8)).toInt
            val keyBytes: Array[Byte] =
              data.slice(index + 8, keyLength + index + 8)

            val intOne = Longs.fromByteArray(
              data.slice(keyLength + index + 8, keyLength + index + 16)
            )
            val intTwo = Longs.fromByteArray(
              data.slice(keyLength + index + 16, keyLength + index + 24)
            )

            levelsMap += (Strings.fromUTF8ByteArray(
              keyBytes
            ) -> (intOne.toInt, intTwo.toInt))

            index = keyLength + index + 24
          }
        }

        found = true
        while (index < data.length && found) {

          if (data.slice(index, index + 8) sameElements delimiter) {
            index = index + 8
            found = false
          } else {
            val keyLength =
              Longs.fromByteArray(data.slice(index, index + 8)).toInt
            val keyBytes: Array[Byte] =
              data.slice(index + 8, keyLength + index + 8)

            val intOne = Longs.fromByteArray(
              data.slice(keyLength + index + 8, keyLength + index + 16)
            )
            val intTwo = Longs.fromByteArray(
              data.slice(keyLength + index + 16, keyLength + index + 24)
            )

            statsMap += (Strings.fromUTF8ByteArray(
              keyBytes
            ) -> (intOne.toInt, intTwo.toInt))

            index = keyLength + index + 24
          }
        }

        val metadataTranscoder = new MetadataTranscoder
        val encoder = new metadataTranscoder.Encoder

        //        println(textualTraitsMap)
        //        println(levelsMap)
        //        println(statsMap)

        IssuerValue(
          explicit,
          encoder
            .encodeMetaData(textualTraitsMap, levelsMap, statsMap)
            .getValue
        )

      }

    }
  def createMetadata(
      explicit: Boolean,
      metadata: (
          Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])],
          (
              Coll[(Coll[java.lang.Byte], (Integer, Integer))],
              Coll[(Coll[java.lang.Byte], (Integer, Integer))]
          )
      )
  ): IssuerValue = {
    IssuerValue(
      explicit,
      metadata
    )
  }

}

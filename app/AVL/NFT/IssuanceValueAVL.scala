package AVL.NFT

import com.google.common.primitives.Longs
import io.getblok.getblok_plasma.ByteConversion
import org.bouncycastle.util.Strings
import org.bouncycastle.util.encoders.Hex
import org.ergoplatform.appkit.Iso
import scorex.crypto.hash

import java.nio.charset.{MalformedInputException, StandardCharsets}

case class IssuanceValueAVL(
    name: String,
    description: String,
    assetType: String,
    sha256: Array[Byte],
    link: String
) {
  var _assetType: Array[Byte] = _;
  def toBytes: Array[Byte] = {

    val assetTypeMap = Map(
      "picture" -> Array(1.toByte, 1.toByte),
      "audio" -> Array(1.toByte, 2.toByte),
      "video" -> Array(1.toByte, 3.toByte),
      "attachment" -> Array(1.toByte, 0x0f.toByte),
      "membership" -> Array(2.toByte, 1.toByte)
    )

    val nameBytes: Array[Byte] = name.getBytes(StandardCharsets.UTF_8)
    val nameLengthBytes: Array[Byte] =
      Longs.toByteArray(nameBytes.length.toLong)

    val descriptionBytes: Array[Byte] =
      description.getBytes(StandardCharsets.UTF_8)
    val descriptionLengthBytes: Array[Byte] =
      Longs.toByteArray(descriptionBytes.length.toLong)

    val assetTypeBytes: Array[Byte] =
      assetTypeMap.getOrElse(assetType.toLowerCase, Array(1.toByte, 1.toByte))
    val assetTypeLengthBytes: Array[Byte] =
      Longs.toByteArray(assetTypeBytes.length.toLong)

    val sha256TypeLengthBytes: Array[Byte] =
      Longs.toByteArray(sha256.length.toLong)

    val linkBytes: Array[Byte] = Strings.toUTF8ByteArray(link)
    val linkLengthBytes: Array[Byte] =
      Longs.toByteArray(linkBytes.length.toLong)

    nameLengthBytes ++ nameBytes ++ descriptionLengthBytes ++ descriptionBytes ++ assetTypeLengthBytes ++ assetTypeBytes ++
      sha256TypeLengthBytes ++ sha256 ++ linkLengthBytes ++ linkBytes

  }
}

object IssuanceValueAVL {
  var _assetBytes: String = _;
  implicit val ErgoValueMetaConversionV2: ByteConversion[IssuanceValueAVL] =
    new ByteConversion[IssuanceValueAVL] {
      override def convertToBytes(t: IssuanceValueAVL): Array[Byte] = t.toBytes
      override def convertFromBytes(bytes: Array[Byte]): IssuanceValueAVL = {

        val nameLength: Int = Longs.fromByteArray(bytes.slice(0, 8)).toInt
        val nameBytes: Array[Byte] = bytes.slice(8, nameLength + 8)
        val descriptionLength: Int = Longs
          .fromByteArray(bytes.slice(nameLength + 8, nameLength + 16))
          .toInt
        val descriptionBytes: Array[Byte] =
          bytes.slice(nameLength + 16, descriptionLength + nameLength + 16)
        val assetTypeLength: Int = Longs
          .fromByteArray(
            bytes.slice(
              descriptionLength + nameLength + 16,
              descriptionLength + nameLength + 24
            )
          )
          .toInt
        val assetBytes: Array[Byte] = bytes.slice(
          descriptionLength + nameLength + 24,
          descriptionLength + nameLength + assetTypeLength + 24
        )
        val sha256TypeLength: Int = Longs
          .fromByteArray(
            bytes.slice(
              descriptionLength + nameLength + assetTypeLength + 24,
              descriptionLength + nameLength + assetTypeLength + 32
            )
          )
          .toInt
        val sha256Bytes: Array[Byte] = bytes.slice(
          descriptionLength + nameLength + assetTypeLength + 32,
          descriptionLength + nameLength + assetTypeLength + sha256TypeLength + 32
        )
        val linkLength: Int = Longs
          .fromByteArray(
            bytes.slice(
              descriptionLength + nameLength + assetTypeLength + sha256TypeLength + 32,
              descriptionLength + nameLength + assetTypeLength + sha256TypeLength + 40
            )
          )
          .toInt
        val linkByte: Array[Byte] = bytes.slice(
          descriptionLength + nameLength + assetTypeLength + sha256TypeLength + 40,
          descriptionLength + nameLength + assetTypeLength + sha256TypeLength + linkLength + 40
        )
        if (assetBytes sameElements Array(1.toByte, 1.toByte)) {
          _assetBytes = "picture"
        } else if (assetBytes sameElements Array(1.toByte, 2.toByte)) {
          _assetBytes = "audio"
        } else if (assetBytes sameElements Array(1.toByte, 3.toByte)) {
          _assetBytes = "video"
        } else if (assetBytes sameElements Array(1.toByte, 0x0f.toByte)) {
          _assetBytes = "attachment"
        } else if (assetBytes sameElements Array(2.toByte, 1.toByte)) {
          _assetBytes = "membership"
        } else {
          _assetBytes = "picture"
        }

        IssuanceValueAVL(
          Strings.fromUTF8ByteArray(nameBytes),
          Strings.fromUTF8ByteArray(descriptionBytes),
          _assetBytes,
          sha256Bytes,
          Strings.fromUTF8ByteArray(linkByte)
        )
      }
    }

  def createMetadata(
      name: String,
      description: String,
      assetType: String,
      sha256Hash: String,
      contentLink: String
  ): IssuanceValueAVL = {
    val metaData: IssuanceValueAVL = IssuanceValueAVL(
      name,
      description,
      assetType,
      Hex.decode(sha256Hash),
      contentLink
    )
    metaData
  }

}

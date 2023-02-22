package AVL.NFT

import com.google.common.primitives.Longs
import io.getblok.getblok_plasma.ByteConversion
import scorex.crypto.hash.Blake2b256

case class IndexKey(value: Long) {
  def toBytes: Array[Byte] = {
//    println(
//      "IndexKey: ",
//      Hex.toHexString(Blake2b256.hash(Longs.toByteArray(value)))
//    )
    Blake2b256.hash(Longs.toByteArray(value))
  }
}

object IndexKey {
  implicit val IndexKeyConversion: ByteConversion[IndexKey] =
    new ByteConversion[IndexKey] {
      override def convertToBytes(t: IndexKey): Array[Byte] = t.toBytes
      override def convertFromBytes(bytes: Array[Byte]): IndexKey = {
        IndexKey(0L)
      }
    }
}

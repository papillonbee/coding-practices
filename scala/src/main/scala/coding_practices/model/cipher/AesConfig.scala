package coding_practices.model.cipher

case class AesConfig(
  name: String,
  secretKeyAlgorithm: String,
  encryptionAlgorithm: String,
  salt: Array[Int],
  iterationCount: Int,
  keyLength: Int,
  initializationVector: Array[Int],
  key: String,
)

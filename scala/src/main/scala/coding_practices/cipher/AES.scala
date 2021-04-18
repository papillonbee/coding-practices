package coding_practices.cipher

import coding_practices.model.cipher.AesConfig

import java.nio.charset.StandardCharsets
import java.util.Base64
import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}

class AES(config: AesConfig) extends SimplifiedCipher {
  private val ivParameterSpec: IvParameterSpec = buildIvParameterSpec(
    initializationVector = config.initializationVector.map(_.toByte),
  )
  private val secretKey: SecretKeySpec = buildKey(
    key = config.key,
    secretKeyAlgorithm = config.secretKeyAlgorithm,
    salt = config.salt,
    iterationCount = config.iterationCount,
    keyLength = config.keyLength,
    name = config.name,
  )

  private def buildIvParameterSpec(initializationVector: Array[Byte]): IvParameterSpec = {
    val ivParameterSpec: IvParameterSpec = new IvParameterSpec(initializationVector)

    ivParameterSpec
  }

  private def buildKey(
    key: String,
    secretKeyAlgorithm: String,
    salt: Array[Int],
    iterationCount: Int,
    keyLength: Int,
    name: String,
  ): SecretKeySpec = {
    val secretKeyFactory: SecretKeyFactory = SecretKeyFactory.getInstance(secretKeyAlgorithm)
    val pbeKeySpec: PBEKeySpec = new PBEKeySpec(
      key.toCharArray,
      salt.map(_.toByte),
      iterationCount,
      keyLength
    )
    val secretKey: SecretKey = secretKeyFactory.generateSecret(pbeKeySpec)
    val secretKeySpec: SecretKeySpec = new SecretKeySpec(secretKey.getEncoded, name)

    secretKeySpec
  }

  override def encrypt(string: String): String = {
    val cipher: Cipher = Cipher.getInstance(config.encryptionAlgorithm)
    cipher.init(Cipher.ENCRYPT_MODE, this.secretKey, this.ivParameterSpec)
    val encryptedBytes: Array[Byte] = cipher.doFinal(string.getBytes(StandardCharsets.UTF_8))
    val encryptedString: String = Base64.getEncoder.encodeToString(encryptedBytes)

    encryptedString
  }

  override def decrypt(string: String): String = {
    val cipher: Cipher = Cipher.getInstance(config.encryptionAlgorithm)
    cipher.init(Cipher.DECRYPT_MODE, this.secretKey, this.ivParameterSpec)
    val decryptedBytes: Array[Byte] = cipher.doFinal(Base64.getDecoder.decode(string.getBytes(StandardCharsets.UTF_8)))
    val decryptedString: String = new String(decryptedBytes, StandardCharsets.UTF_8)

    decryptedString
  }
}

package coding_practices.cipher

import coding_practices.model.Dinosaur
import coding_practices.model.cipher.AesConfig
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}

class AESTest extends AnyFunSpec
  with Matchers {

  describe("AES") {
    val config: Config = ConfigFactory.load()
    val aesConfig: AesConfig = config.as[AesConfig]("aes-config")
    val aes: SimplifiedCipher = new AES(aesConfig)

    it("should encrypt/decrypt arbitrary string correctly") {
      val string: String = "My previous manager was a trained sniper!"

      val encryptedString: String = aes.encrypt(string)
      val decryptedString: String = aes.decrypt(encryptedString)

      decryptedString shouldEqual string
    }

    it("should encrypt/decrypt dinosaur correctly") {
      val dinosaur: Dinosaur = Dinosaur(name = "T-Rex", age = 67000000)

      val dinosaurJsValue: JsValue = Json.toJson(dinosaur)
      val dinosaurJsString: String = Json.stringify(dinosaurJsValue)

      val encryptedString: String = aes.encrypt(dinosaurJsString)
      val decryptedString: String = aes.decrypt(encryptedString)

      val decryptedJsValue: JsValue = Json.parse(decryptedString)
      val decryptedDinosaur: Dinosaur = decryptedJsValue.as[Dinosaur]

      decryptedString shouldEqual dinosaurJsString
      decryptedJsValue shouldEqual dinosaurJsValue
      decryptedDinosaur shouldEqual dinosaur
    }
  }
}

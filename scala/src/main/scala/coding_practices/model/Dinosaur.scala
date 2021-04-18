package coding_practices.model

import play.api.libs.json.{Json, OFormat}

case class Dinosaur(
  name: String,
  age: Int,
)

object Dinosaur {
  implicit val format: OFormat[Dinosaur] = Json.format[Dinosaur]
}

import io.circe.Encoder
import io.circe.syntax.EncoderOps

import scala.reflect.io.File


/*
 * To pseudonomize the data we need to remove the ability to infer
 * the number (id) and name of pokemon. Therefore we would have to
 * scramble the order of the pokemon and encrypt the name
 * and id in the output file. The key can be sent separately
 */
object PokeEncoder {
  def encodeToCsvFile(pokemonList: List[Pokemon], fileName: String): Unit = {
    val csvLines = for (pokemon <- pokemonList) yield {
      pokemon match {
        case Pokemon(name, id, base_experience, weight, height,
          bmi, order, types, _, sprite_link) =>
          val typesCsv = s"${types.head},${if (types.length > 1) types(1) else null}"
          s"$name,$id,$base_experience,$weight,$height,$bmi,$order,$typesCsv,$sprite_link"
      }
    }
    val writer = File(fileName).printWriter()
    writer.println("name,id,base_experience,weight,height,bmi,order,type_1,type_2,sprite")
    csvLines.foreach(line => writer.println(line))
    writer.close()
  }

  implicit val encodeUser: Encoder[Pokemon] =
    Encoder.forProduct10(
      "name","id","base_experience","weight","height","bmi",
      "order","type_1","type_2","sprite")(pkmn =>
      (pkmn.name, pkmn.id, pkmn.base_experience, pkmn.weight, pkmn.height, pkmn.bmi,
        pkmn.order, pkmn.types.head, if(pkmn.types.length > 1) pkmn.types(1) else "null", pkmn.sprite_link))
  def encodeToJsonFile(pokemonList: List[Pokemon], fileName: String): Unit = {
    val jsonString = pokemonList.asJson.toString()
    File(fileName).writeAll(jsonString)
  }
}

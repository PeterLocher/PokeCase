import io.circe.Encoder
import io.circe.syntax.EncoderOps

import scala.reflect.io.File
import scala.util.Random
object PokeEncoder {

  // Value used for type_2 in output files when a pokemon is single type
  private val noSecondType = ""

  // Encode a list of Pokemon to a headed csv file.
  def encodeToCsvFile(pokemonList: List[Pokemon], fileName: String): Unit = {
    val csvLines = for (pokemon <- pokemonList) yield {
      pokemon match {
        case Pokemon(name, id, base_experience, weight, height,
          bmi, order, types, _, sprite_link) =>
          val typesCsv = s"${types.head},${if (types.length > 1) types(1) else noSecondType}"
          s"$name,$id,$base_experience,$weight,$height,$bmi,$order,$typesCsv,$sprite_link"
      }
    }
    val csvHeader = "name,id,base_experience,weight,height,bmi,order,type_1,type_2,sprite"
    writeCsvFile(fileName, csvHeader :: csvLines)
  }

  //Uses a PrintWriter to write input lines to a new csv file
  private def writeCsvFile(fileName: String, lines: List[String]): Unit = {
    val dataWriter = File(fileName).printWriter()
    lines.foreach(header => dataWriter.println(header))
    dataWriter.close()
  }

  // Define Circe encoder for the Model.scala pokemon case class
  implicit val encodePokemon: Encoder[Pokemon] =
    Encoder.forProduct10(
      "name","id","base_experience", "weight","height","bmi", "order","type_1","type_2","sprite"
    )(
      pkmn => (pkmn.name, pkmn.id, pkmn.base_experience, pkmn.weight, pkmn.height, pkmn.bmi, pkmn.order,
        pkmn.types.head, if(pkmn.types.length > 1) pkmn.types(1) else noSecondType, pkmn.sprite_link)
    )

  def encodeToJsonFile(pokemonList: List[Pokemon], fileName: String): Unit = {
    val jsonString = pokemonList.asJson.toString()
    File(fileName).writeAll(jsonString)
  }

  /*
   * To pseudonomize the data we need to remove the ability to infer
   * the number (id), name or sprite of pokemon when having only the
   * data file.
   * Therefore we have to scramble the order of the pokemon.
   *
   * This method produces 3 files, a header, data and ordering file.
   * The ordering file can be used to realign the lines of the data
   * file to their indices. For pseudonymisation, the ordering file
   * should be sent and stored separately.
   */
  def encodePseudonomizedCsvFiles(pokemonList: List[Pokemon], fileNameHeader: String,
                                  fileNameData: String, fileNameOrdering: String): Unit = {
    val csvLines = for (pokemon <- pokemonList) yield {
      pokemon match {
        case Pokemon(name, id, base_experience, weight, height,
        bmi, order, types, _, sprite_link) =>
          val header = s"$name,$id,$order,$sprite_link"
          val typesCsv = s"${types.head},${if (types.length > 1) types(1) else noSecondType}"
          val data = s"$base_experience,$weight,$height,$bmi,$typesCsv"
          (header, data)
      }
    }

    val (headerLines, dataLines) = csvLines.unzip
    val permutation = Random.shuffle(List.range(0, csvLines.length))
    val permutedData = dataLines.zip(permutation).sortBy(_._2).map(_._1)
    val permutationCsvLine = permutation.map(_.toString).reduce(_ + "," + _)

    writeCsvFile(fileNameHeader, "name,id,order,sprite" :: headerLines)
    writeCsvFile(fileNameData, "base_experience,weight,height,bmi,type_1,type_2" :: permutedData)
    writeCsvFile(fileNameOrdering, List(permutationCsvLine))
  }
}

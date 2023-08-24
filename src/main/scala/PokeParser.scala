import io.circe._
import io.circe.generic.semiauto.deriveDecoder
import org.apache.spark.sql.SparkSession

import scala.io.Source

object PokeParser {

  implicit val typeSlotDecoder: Decoder[PokemonType] =
    (c: HCursor) => for {
      slot <- c.get[Int]("slot")
      type_name <- c.downField("type").get[String]("name")
    } yield {
      PokemonType(slot, type_name)
    }

  implicit val gameDecoder: Decoder[Game] =
    (c: HCursor) => for {
      name <- c.downField("version").get[String]("name")
    } yield {
      Game(name)
    }

  //implicit val pokemonDecoder: Decoder[Pokemon] = deriveDecoder
  implicit val pokemonDecoder: Decoder[Pokemon] =
    (c: HCursor) =>
      for {
      name <- c.get[String]("name")
      id <- c.get[Int]("id")
      base_experience <- c.get[Float]("base_experience")
      weight <- c.get[Float]("weight")
      height <- c.get[Float]("height")
      order <- c.get[Int]("order")
      first_type <- c.downField("types").downArray.downField("type").get[String]("name")
      second_type <- if (c.downField("types").values.get.count(_ => true) > 2)
          c.downField("types").downArray.left.downField("type").get[String]("name") else c.get[String]("name")
      games <- c.downField("game_indices").downArray.downField("version").get[String]("name")
      sprite_link <- c.downField("sprites").get[String]("front_default")
    } yield {
      val bmi = (weight / 10) / Math.pow(height / 10, 2).toFloat
      Pokemon(name.capitalize, id, base_experience, weight, height, bmi, order, List(first_type, second_type), List(games), sprite_link)
    }
  implicit val linkDecoder: Decoder[Link] = deriveDecoder[Link]
  implicit val varietyDecoder: Decoder[Variety] = deriveDecoder[Variety]
  implicit val speciesDecoder: Decoder[PokemonSpecies] = deriveDecoder[PokemonSpecies]
  implicit val entryDecoder: Decoder[PokedexEntry] = deriveDecoder[PokedexEntry]
  implicit val pokedexDecoder: Decoder[Pokedex] = deriveDecoder[Pokedex]

  def main(args: Array[String]): Unit = {
    val time = System.currentTimeMillis()
    //val pokemon = getPokemonOfPokedexes(List(2, 3, 8))
    val pokemon = List.range(1, 5/*650*/).map(parsePokemon)
    println(s"Time consumed: ${System.currentTimeMillis() - time}")
    val legalGameNames = List("red", "blue", "leafgreen", "white")
    val filteredPokemon = pokemon.filter(pkmn => {
      //val gameNames = pkmn.game_indices.map(game => game)
      pkmn.game_indices.exists(legalGameNames.contains)
    })
    println(s"Time consumed: ${System.currentTimeMillis() - time}")
    filteredPokemon.foreach(println)
  }

  private def getPokemonOfPokedexes(pokedexesOfLegalGames: List[Int]): List[Pokemon] = {
    val uniqueEndpoints = pokedexesOfLegalGames.map(pokemonEndpointsOfPokedex)
      .map(list => list.toSet)
      .reduce((s1, s2) => s1 concat s2)
    println("Downloading pokemon from endpoints")
    uniqueEndpoints.toList.map(parsePokemon)
  }

  private def pokemonEndpointsOfPokedex(pokedex_no: Int): List[String] = {
    println(s"Getting endpoints for pokemon in pokedex no $pokedex_no")
    val pokedex = parsePokedex(pokedex_no)
    val urls = pokedex.pokemon_entries.map(entry => entry.pokemon_species.url)
    val species = urls.map(parsePokemonSpecies)
    val pokemonURLs = species
      .map(species => species.varieties)
      .reduce((l1, l2) => l1.concat(l2))
      .map(variety => variety.pokemon.url)
    pokemonURLs
  }

  def parsePokemon(pokemon_no: Int): Pokemon = {
    val endpoint = s"https://pokeapi.co/api/v2/pokemon/$pokemon_no"
    parsePokemon(endpoint)
  }

  def parsePokemon(endpoint: String): Pokemon = {
    val jsonString = downloadToString(endpoint)
    val decodeResult = parser.decode[Pokemon](jsonString)
    unpackParseResult(decodeResult)
  }

  def parsePokemonSpecies(endpoint: String): PokemonSpecies = {
    val jsonString = downloadToString(endpoint)
    val decodeResult = parser.decode[PokemonSpecies](jsonString)
    unpackParseResult(decodeResult)
  }

  def parsePokedex(pokedex_no: Int): Pokedex = {
    val endpoint = s"https://pokeapi.co/api/v2/pokedex/$pokedex_no"
    val jsonString = downloadToString(endpoint)
    val decodeResult = parser.decode[Pokedex](jsonString)
    unpackParseResult(decodeResult)
  }

  private def downloadToString(endpoint: String): String = {
    val response = Source.fromURL(endpoint)
    response.mkString
  }

  private def unpackParseResult[T](decodeResult: Either[Error, T]) = {
    val output = decodeResult match {
      case Right(value) => value
      case Left(msg) =>
        throw new IllegalArgumentException(s"Couldn't parse JSON: $msg")
    }
    output
  }


  private def simple_spark_app(): Unit = {
    val logFile = "files/README.md" // Should be some file on your system
    val spark = SparkSession.builder().appName("Simple").getOrCreate()
    val logData = spark.read.textFile(logFile).cache()
    val numAs = logData.filter(line => line.contains("a")).count()
    val numBs = logData.filter(line => line.contains("b")).count()
    println(s"Lines with a: $numAs, Lines with b: $numBs")
    spark.stop()
  }
}

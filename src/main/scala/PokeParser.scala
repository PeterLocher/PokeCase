import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, _}
import org.apache.spark.sql.SparkSession

import scala.io.Source

object PokeParser {

  implicit val typeDecoder: Decoder[PokemonType] =
    (c: HCursor) => c.downField("type").get[String]("name")
      .map(t => PokemonType(t))

  implicit val gameDecoder: Decoder[Game] =
    (c: HCursor) => c.downField("version").get[String]("name")
      .map(name => Game(name))

  implicit val pokemonDecoder: Decoder[Pokemon] = {
    (c: HCursor) =>
      for {
        name <- c.get[String]("name")
        id <- c.get[Int]("id")
        base_exp <- c.get[Float]("base_experience")
        weight <- c.get[Float]("weight")
        height <- c.get[Float]("height")
        order <- c.get[Int]("order")
        wrapped_types <- c.downField("types").as[Seq[PokemonType]]
        wrapped_games <- c.downField("game_indices").as[Seq[Game]]
        sprite_link <- c.downField("sprites").getOrElse[String]("front_default")("")
    } yield {
      val cName = name.capitalize
      val bmi = ((weight / 10) / Math.pow(height / 10, 2)).floatValue()
      val types = wrapped_types.toList.map(t => t.name)
      val games = wrapped_games.toList.map(game => game.name)
      Pokemon(cName, id, base_exp, weight, height, bmi,
        order, types, games, sprite_link)
    }
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

    /*
     * The following methods are used for finding valid pokemon
     * (of games Red, Blue, Leaf Green and White)
     * by looking up all entries of the relevant pokedexes
     */

  implicit val linkDecoder: Decoder[Link] = deriveDecoder[Link]
  implicit val varietyDecoder: Decoder[Variety] = deriveDecoder[Variety]
  implicit val speciesDecoder: Decoder[PokemonSpecies] = deriveDecoder[PokemonSpecies]
  implicit val entryDecoder: Decoder[PokedexEntry] = deriveDecoder[PokedexEntry]
  implicit val pokedexDecoder: Decoder[Pokedex] = deriveDecoder[Pokedex]

  def getPokemonOfPokedexes(pokedexes: List[Int]): List[Pokemon] = {
    val endpoints = pokedexes.map(pokemonEndpointsOfPokedex)
      .reduce((s1, s2) => s1 concat s2)
    val uniqueEndpoints = endpoints.toSet.toList
    println("Downloading pokemon from endpoints")
    val pokemon = uniqueEndpoints.map(parsePokemon)
    pokemon.sortBy(pokemon => pokemon.id)
  }

  def pokemonEndpointsOfPokedex(pokedex_no: Int): List[String] = {
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
}

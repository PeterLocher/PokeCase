
object Main {

  def main(args: Array[String]): Unit = {
    // Download pokemon from API
    val pokemon = List.range(1, 650).map(PokeParser.parsePokemon)
    val legalGames = List("red", "blue", "leafgreen", "white")

    // Keep pokemon appearing in the wanted games only
    val filteredPokemon = pokemon.filter(pkmn => pkmn.games.exists(legalGames.contains))

    // Export to csv and json files
    PokeEncoder.encodeToCsvFile(filteredPokemon, "files/pokemon.csv")
    PokeEncoder.encodeToJsonFile(filteredPokemon, "files/pokemon.json")
    PokeEncoder.encodePseudonomizedCsvFiles(filteredPokemon,
      "files/pseudonymous_data/pokemon_header.csv",
      "files/pseudonymous_data/pokemon_data.csv",
      "files/pseudonymous_data/ordering.csv"
    )
  }

}

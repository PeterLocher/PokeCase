case class Pokemon(name: String,
                   id: Int,
                   base_experience: Float,
                   weight: Float,
                   height: Float,
                   bmi: Float,
                   order: Int,
                   types: List[String],
                   games: List[String],
                   sprite_link: String)
case class Game(name: String)
case class PokemonType(name: String)

case class PokemonSpecies(varieties: List[Variety])
case class Variety(pokemon: Link)

case class Pokedex(pokemon_entries: List[PokedexEntry])
case class PokedexEntry(entry_number: Int, pokemon_species: Link)

case class Link(url:String)

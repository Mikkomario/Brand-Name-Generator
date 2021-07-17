package vf.brand.model

object Variable
{
	/**
	 * @param location Variable insert location
	 * @param word Word to insert
	 * @return A new word variable
	 */
	def apply(location: Int, word: String): Variable = apply(location, Set(word))
	/**
	 * @param location Variable insert location
	 * @param first Word to insert
	 * @param second Alternative word to insert
	 * @param more More alternatives
	 * @return A new word variable
	 */
	def apply(location: Int, first: String, second: String, more: String*): Variable =
		apply(location, Set(first, second) ++ more)
}

/**
 * Variables are words that may or may not be inserted to certain places
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
case class Variable(location: Int, alternatives: Set[String])

package vf.brand.controller

import utopia.flow.parse.CsvReader
import vf.brand.model.{Word, WordGroup}
import vf.brand.model.enumeration.WordRole.{Adjective, Noun, Verb}

import java.nio.file.Path

/**
 * Used for reading words from a .csv file
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
object ReadWords
{
	// ATTRIBUTES   -------------------------
	
	private val headerRoles = Map(
		"Adjective" -> Adjective.Passive,
		"Noun Single" -> Noun.Singular,
		"Noun Plural" -> Noun.Plural,
		"Verb" -> Verb,
		"Verb Adjective" -> Adjective.Active
	)
	
	
	// OTHER    -----------------------------
	
	/**
	 * Reads word data from the specified path
	 * @param path A path to the file to read
	 * @return Word groups read from the file. Failure if the file couldn't be read or if there were format problems.
	 */
	def apply(path: Path) =
	{
		CsvReader.iterateLinesIn(path, ",", ignoreEmptyStringValues = true) { iterator =>
			iterator.map { model =>
				// Forms a word group from each line
				WordGroup(model.attributesWithValue.map { att =>
					// NB: Throws if one of the headers is invalid
					// (this is on purpose in order to find mistakes early)
					Word(att.value.getString, headerRoles(att.name))
				}.toSet)
			}.filter { _.nonEmpty }.toVector
		}
	}
}

package vf.brand.model

import utopia.flow.util.CollectionExtensions._
import vf.brand.model.enumeration.WordRole.{Adjective, Noun, Verb}
import vf.brand.model.enumeration.{SpecificWordRole, WordRole}

import scala.util.Random

/**
 * Represents a word in its different forms
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
case class WordGroup(words: Set[Word])
{
	// COMPUTED ---------------------------
	
	def isEmpty = words.isEmpty
	def nonEmpty = !isEmpty
	
	/**
	 * @return Verbs within this word group
	 */
	def verbs = apply(Verb: WordRole)
	/**
	 * @return Adjectives within this word group
	 */
	def adjectives = apply(Adjective)
	/**
	 * @return Nouns within this word group
	 */
	def nouns = apply(Noun)
	
	/**
	 * @return Words within this group that imply action
	 */
	def active = filter { _.role.isActive }
	/**
	 * @return Words within this group that don't imply action
	 */
	def passive = filterNot { _.role.isActive }
	/**
	 * @return A subgroup of these words where the words don't imply action, then those which do
	 */
	def passiveAndActive = divideBy { _.role.isActive }
	
	/**
	 * @return Words within this group that imply many items
	 */
	def plural = filter { _.role.isPlural }
	/**
	 * @return Words within this group that don't imply many items
	 */
	def singular = filterNot { _.role.isPlural }
	/**
	 * @return A subgroup of these words where the words don't imply many items, then those which do
	 */
	def singularAndPlural = divideBy { _.role.isPlural }
	
	
	// OTHER    ---------------------------
	
	/**
	 * @return A random word from this word group
	 */
	def nextRandom() = if (isEmpty) None else Some(words.toVector(Random.nextInt(words.size)))
	
	/**
	 * @param role A (generic) word role
	 * @return All words in this group that belong to the specified role
	 */
	def apply(role: WordRole) = filter { _.genericRole == role }
	/**
	 * @param role A (specific) word role
	 * @return All words in this group that belong to the specified role
	 */
	def apply(role: SpecificWordRole) = filter { _.role == role }
	
	/**
	 * @param f A filter function
	 * @return A subgroup of this word group with only filtered words
	 */
	def filter(f: Word => Boolean) = WordGroup(words.filter(f))
	/**
	 * @param f A filter function
	 * @return A subgroup of these words containing only words NOT accepted by the filter
	 */
	def filterNot(f: Word => Boolean) = filter { !f(_) }
	
	/**
	 * @param f A dividing function that returns either true or false
	 * @return The false words group, then the true words group
	 */
	def divideBy(f: Word => Boolean) =
	{
		val (l, r) = words.divideBy(f)
		WordGroup(l) -> WordGroup(r)
	}
}

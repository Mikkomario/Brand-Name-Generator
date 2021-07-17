package vf.brand.controller

import utopia.flow.datastructure.mutable.Pointer
import vf.brand.model.enumeration.{SpecificWordRole, WordRole}
import vf.brand.model.{BrandName, Word, WordGroup}

import scala.util.Random

/**
 * This class offers new words for brand names. It receives feedback and alters the frequency of the words based on it.
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
class WordOfferer(words: Vector[WordGroup])
{
	// ATTRIBUTES   --------------------------------
	
	private val allWords = words.flatMap { group =>
		val ratingPointer = new Pointer(0)
		group.words.map { new RatedWord(_, ratingPointer) }
	}
	private val groupedWords = WordRole.specificValues.map { role =>
		role -> allWords.filter { _.word.role == role }
	}.filter { _._2.nonEmpty }.map { case (role, words) => role -> new RatedWordGroup(words) }.toMap
	
	
	// OTHER    ------------------------------------
	
	/**
	 * @param role A (specific) word role
	 * @return A random word from that word role, prioritizing popular words
	 */
	def nextRandomOf(role: SpecificWordRole) = groupedWords(role).nextRandom()
	/**
	 * @param role A (generic) word role
	 * @return A random word from that word role, prioritizing popular words
	 */
	def nextRandomOf(role: WordRole): Word =
	{
		val roleOptions = role.variations
		nextRandomOf(roleOptions(Random.nextInt(roleOptions.size)))
	}
	
	/**
	 * Rates a word
	 * @param word Word to rate
	 * @param rating Rating to apply for the word where positive values increase status and negatives decrease
	 */
	def rate(word: Word, rating: Int) = groupedWords(word.role).rate(word, rating)
	/**
	 * Likes the specified word, increasing its frequency
	 * @param word Word to like
	 */
	def like(word: Word, modifier: Int = 1) = rate(word, 4 * modifier)
	/**
	 * Dislikes the specified word, decreasing its frequency
	 * @param word Word to dislike
	 */
	def dislike(word: Word, modifier: Int = 1) = rate(word, -1 * modifier)
	
	/**
	 * @param word A word
	 * @return Current rating of that word (int)
	 */
	def ratingOf(word: Word) = groupedWords(word.role).ratingOf(word)
	/**
	 * @param name A brand name
	 * @return Current rating of that brand name (int)
	 */
	def ratingOf(name: BrandName): Int =
	{
		val ratings = name.words.map(ratingOf)
		ratings.sum / ratings.size
	}
	
	
	// NESTED   ------------------------------------
	
	private class RatedWordGroup(words: Vector[RatedWord])
	{
		// ATTRIBUTES   ----------------------------
		
		private var minRating = 0
		private var maxRating = 0
		
		
		// OTHER    --------------------------------
		
		def ratingOf(word: Word) = words.find { _.word == word }.map { _.rating }.getOrElse(0)
		
		def nextRandom() =
		{
			if (minRating >= maxRating - 1)
				words(Random.nextInt(words.size)).word
			else
			{
				val ratingThreshold = minRating + Random.nextInt((maxRating - minRating) / 2)
				val filtered = words.filter { _.rating >= ratingThreshold }
				// Word group ratings may have decreased, causing the maximum to lower
				if (filtered.nonEmpty)
					filtered(Random.nextInt(filtered.size)).word
				else
					words(Random.nextInt(words.size)).word
			}
		}
		
		def rate(word: Word, rating: Int) = words.find { _.word == word }.foreach { word =>
			word.rate(rating)
			val newRating = word.rating
			if (newRating < minRating)
				minRating = newRating
			else if (newRating > maxRating)
				maxRating = newRating
		}
	}
	
	private class RatedWord(val word: Word, groupRatingPointer: Pointer[Int])
	{
		// ATTRIBUTES   ----------------------------
		
		private var _rating = 0
		
		
		// COMPUTED --------------------------------
		
		def rating = groupRatingPointer.value + _rating
		
		
		// OTHER    --------------------------------
		
		def rate(rating: Int) =
		{
			groupRatingPointer.update { _ + rating }
			_rating += rating * 3
		}
	}
}

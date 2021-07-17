package vf.brand.model

import utopia.flow.util.CollectionExtensions._

/**
 * Represents a possible brand name, which is created by combining certain words
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
case class BrandName(words: Vector[Word], variables: Set[Variable] = Set())
{
	// COMPUTED ------------------------------
	
	/**
	 * @return All possible variations of this brand name (versions with or without extra words)
	 */
	def variations =
	{
		if (variables.isEmpty)
			Vector(toString)
		else
		{
			val variableCombinations = Set[(String, Int)]() +:
				combinationsOf(variables.toVector).toVector.sortBy { _.size }
			variableCombinations.map(toStringWithVariables)
		}
	}
	
	
	// IMPLEMENTED  --------------------------
	
	override def toString = words.mkString(" ")
	
	
	// OTHER    ------------------------------
	
	private def toStringWithVariables(variables: Set[(String, Int)]) =
	{
		// Corrects the variable indices (because the previous inserted words affect the latter insert positions)
		val correctedIndexVariables = variables.toVector.sortBy { _._2 }.zipWithIndex
			.map { case ((word, rawIdx), varIdx) => word -> (rawIdx + varIdx) }
		// Inserts the words, one at a time, then combines them
		correctedIndexVariables.foldLeft(words.map { _.text }) { (words, variable) =>
			words.inserted(variable._1, variable._2)
		}.mkString(" ")
	}
	
	private def combinationsOf(variables: Vector[Variable]): Set[Set[(String, Int)]] =
	{
		val head = variables.head
		val headAlternatives = head.alternatives.map { _ -> head.location }
		val headAlternativesIndividually = headAlternatives.map { Set(_) }
		
		// Case: Only one item to combine => Can produce only that one combination
		if (variables.size == 1)
			headAlternativesIndividually
		// Case: Multiple combinations available => Uses recursion
		else
		{
			val tail = variables.tail
			val tailCombinations = combinationsOf(tail)
			
			// Result = Combinations without the item + combinations with the item + item individually
			val extendedTailCombinations = tailCombinations
				.flatMap { combination => headAlternatives.map { combination + _ } }
			tailCombinations ++ extendedTailCombinations ++ headAlternativesIndividually
		}
	}
}

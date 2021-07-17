package vf.brand.model.enumeration

import vf.brand.model.Variable
import vf.brand.model.enumeration.WordRole.{Adjective, Noun}

/**
 * An enumeration for different rules / algorithms for producing brand names
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
sealed trait NameRule
{
	// ABSTRACT ---------------------------------
	
	/**
	 * @return Specifies the amount, order and type of words taken as input.
	 *         Left means a generic, less strict requirement. Right means a strict, specific requirement.
	 */
	def inputRequirements: Vector[Either[WordRole, SpecificWordRole]]
	/**
	 * @return Variable words that may be applied to the final brand name.
	 *         Each item contains the inserted word and the index where the word would be inserted
	 *         (to the base version)
	 */
	def variables: Set[Variable]
}

object NameRule
{
	// ATTRIBUTES   ----------------------------
	
	private val startingThe = Variable(0, "the")
	/**
	 * All available name creation rules
	 */
	lazy val values = Vector[NameRule](SingleWord, DescribedNoun, Combo, DescribedCombo,
		/*ThreeWords, ActionTarget, Declaration*/)
	
	/**
	 * Presents a single noun only. E.g. "Apple" or "the Apples"
	 */
	case object SingleWord extends NameRule
	{
		override val inputRequirements = Vector(Left(Noun))
		override val variables = Set(startingThe)
	}
	
	/**
	 * Presents a combination of two nouns. E.g. "Apple Cider" or "Apple and the Oranges"
	 */
	case object Combo extends NameRule
	{
		override val inputRequirements = Vector(Right(Noun.Singular), Left(Noun))
		override val variables = Set(startingThe, Variable(1, "with", "of", "with the", "of the", "and", "and the"))
	}
	
	/**
	 * Presents a noun described with a single adjective. E.g. "Tasty Apples" or "the Tasty Oranges"
	 */
	case object DescribedNoun extends NameRule
	{
		override val inputRequirements = Vector(Left(Adjective), Left(Noun))
		override val variables = Set(startingThe)
	}
	
	/*
	 * Describes a verb with a target. E.g. "Eat the Apple" or "Buy Oranges"
	 *//*
	case object ActionTarget extends NameRule
	{
		override val inputRequirements = Vector(Right(Verb), Left(Noun))
		override val variables = Set(Variable(1, "with", "the", "with the"))
	}*/
	
	/*
	 * Combines three nouns. E.g. "the Lord of the Ring Polishers"
	 */
	/*
	case object ThreeWords extends NameRule
	{
		override val inputRequirements = Vector(Right(Noun.Singular), Right(Noun.Singular), Left(Noun))
		override val variables = Set(startingThe, Variable(1, "of the", "and the"))
	}*/
	
	/**
	 * Describes a two-word noun combination with an adjective. E.g. "the Last Lord of the Rings"
	 */
	case object DescribedCombo extends NameRule
	{
		override val inputRequirements = Vector(Left(Adjective), Right(Noun.Singular), Left(Noun))
		override val variables = Set(startingThe, Variable(2, "of", "of the"))
	}
	
	/*
	 * Declares an action between two nouns, where the first one is plural. E.g. "Lords Eat Apples"
	 */
	/*
	case object Declaration extends NameRule
	{
		override val inputRequirements = Vector(Right(Noun.Plural), Right(Verb), Left(Noun))
		override val variables = Set(Variable(2, "the"))
	}*/
}
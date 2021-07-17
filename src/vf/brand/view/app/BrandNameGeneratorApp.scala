package vf.brand.view.app

import utopia.flow.generic.DataType
import utopia.flow.generic.ValueConversions._
import utopia.flow.util.FileExtensions._
import vf.brand.controller.{ReadWords, WordOfferer}
import vf.brand.model.BrandName
import vf.brand.model.enumeration.NameRule

import scala.io.StdIn
import scala.util.Random

/**
 * The main application of this project
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
object BrandNameGeneratorApp extends App
{
	DataType.setup()
	val words = ReadWords("data/brand-words.csv").get
	val wordRater = new WordOfferer(words)
	var likedBrandNames = Vector[BrandName]()
	var otherBrandNames = Vector[BrandName]()
	
	println("Welcome to brand names generator (prototype). Press enter to move forward. " +
		"Type exit and press enter to quit.")
	var input = ""
	do {
		val rule = NameRule.values(Random.nextInt(NameRule.values.size))
		val nameWords = rule.inputRequirements.map
		{
			case Left(generic) => wordRater.nextRandomOf(generic)
			case Right(specific) => wordRater.nextRandomOf(specific)
		}
		val name = BrandName(nameWords, rule.variables)
		if (!likedBrandNames.contains(name) && !otherBrandNames.contains(name))
		{
			name.variations.foreach(println)
			input = StdIn.readLine()
			
			input.trim.int.foreach { rating =>
				val sizeMod = (4 - name.words.size) max 1
				if (rating > 3)
				{
					val modifier = sizeMod * (rating - 3)
					name.words.foreach { wordRater.like(_, modifier) }
					likedBrandNames :+= name
				}
				else
				{
					if (rating < 3)
					{
						val modifier = sizeMod * (3 - rating)
						name.words.foreach { wordRater.dislike(_, modifier) }
					}
					otherBrandNames :+= name
				}
			}
		}
	
	} while (input.toLowerCase != "exit")
	
	likedBrandNames.sortBy(wordRater.ratingOf).foreach { name =>
		println()
		name.variations.foreach(println)
	}
}

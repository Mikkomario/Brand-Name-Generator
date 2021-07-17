package vf.brand.model

import vf.brand.model.enumeration.SpecificWordRole

/**
 * Represents a single word
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
case class Word(text: String, role: SpecificWordRole)
{
	// COMPUTED ------------------------
	
	/**
	 * @return The generic word role this word belongs to
	 */
	def genericRole = role.parent
	
	
	// IMPLEMENTED  -------------------
	
	override def toString = text
}
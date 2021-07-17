package vf.brand.model.enumeration

/**
 * An enumration for different roles a word can play in a sentence
 * @author Mikko Hilpinen
 * @since 17.7.2021, v0.1
 */
sealed trait WordRole
{
	/**
	 * @return Specific variations of this word role
	 */
	def variations: Vector[SpecificWordRole]
}

sealed trait SpecificWordRole
{
	/**
	 * @return The category of this word role
	 */
	def parent: WordRole
	
	/**
	 * @return Whether this word role implies action
	 */
	def isActive: Boolean
	/**
	 * @return Whether this word role implies multiple items
	 */
	def isPlural: Boolean
}

object WordRole
{
	// COMPUTED   ----------------------------
	
	/**
	 * @return All available high level word roles
	 */
	def genericValues = Vector[WordRole](Verb, Noun, Adjective)
	/**
	 * All available specific / lowest level word roles
	 */
	def specificValues: Vector[SpecificWordRole] = Verb +: (Noun.variations ++ Adjective.variations)
	
	
	// NESTED   -----------------------------
	
	/**
	 * Verbs describe action
	 */
	case object Verb extends WordRole with SpecificWordRole
	{
		override def variations = Vector()
		
		override def parent = this
		
		override def isActive = true
		override def isPlural = false
	}
	
	/**
	 * Nouns describe items and they are separated into singular and plural forms
	 */
	case object Noun extends WordRole
	{
		// IMPLEMENTED   ----------------------------
		
		override def variations = Vector(Singular, Plural)
		
		
		// NESTED   ------------------------------
		
		/**
		 * Describes a singular item or an actor
		 */
		case object Singular extends SpecificWordRole
		{
			override def parent = Noun
			
			override def isActive = false
			override def isPlural = false
		}
		/**
		 * Describes a group of items or actors
		 */
		case object Plural extends SpecificWordRole
		{
			override def parent = Noun
			
			override def isActive = false
			override def isPlural = true
		}
	}
	
	/**
	 * Adjectives describe items or actions. These are divided to adjectives
	 * implying action (active) and those that don't (passive)
	 */
	case object Adjective extends WordRole
	{
		// IMPLEMENTED ----------------------
		
		override def variations = Vector(Passive, Active)
		
		
		// NESTED   -------------------------
		
		/**
		 * Describes an item without necessarily implying action
		 */
		case object Passive extends SpecificWordRole
		{
			override def parent = Adjective
			
			override def isActive = false
			override def isPlural = false
		}
		/**
		 * Describes an item by implying a certain verb / action
		 */
		case object Active extends SpecificWordRole
		{
			override def parent = Adjective
			
			override def isActive = true
			override def isPlural = false
		}
	}
}
package com.gravitydev.vanity

object Model {
	
	case class CssProperty [T] (name:String)
	
	trait CssValue [T <: CssValue[T]] {
		self: T => 
		def toCSS : String
		def apply (s:String) = this
		var isImportant = false

		def important ():T = {
			isImportant = true
			this
		}
	}
	case class CssColor(color:Int) extends CssValue[CssColor] {
		def toCSS = "#" + "%06x".format(color)
	}
	case class CssScalar (pixels:Int, unit:String) extends CssValue[CssScalar] {
		def toCSS = pixels.toString + unit
	}
	case class CssString (text:String) extends CssValue[CssString] {
		def toCSS = text
	}
	
	trait Rule {
		def toCSS:String
	}
	
	class NumberWrapper(num:Int) {
		private def wrap (unit:String) = new CssScalar(num, unit)
		def px = wrap("px")
	}
	
	trait Selector {
		def toCSS : String
	}
	class SingleSelector (tag:String) extends Selector {
		var predicate = ""
		def apply (pred:String) = {
			val s = new SingleSelector(tag)
			s.predicate = pred
			s
		}
		def toCSS =  tag + predicate
	}
	class MultipleSelector (selectors: List[SingleSelector]) extends Selector {
		def toCSS = selectors.map(_.toCSS).mkString(", ")
	}
	
	case class RuleSet (parent:Css, selector:Selector) extends Css with Rule {
		override def current_=(cur:Css) = parent.current = cur
		override def current = parent.current

		def apply [T](r: => T) = {
			var old = current;
			current = this
			r;
			current = old;
			this
		}
		
		def toCSS = {
			val (subRuleSets, declarations) = rules.toList.partition(_.isInstanceOf[RuleSet])
			selector.toCSS + " {\n" +
			Util.indent( declarations.map(_.toCSS).mkString ) + "\n" +
			"}\n" +
			ruleSets.map(rs =>
				selector.toCSS + " " + rs.toCSS
			).mkString
		}
	}
	
	class Declaration[T <: CssValue[T]] (val prop:CssProperty[T]) extends Rule {
		var value:T = _
		def toCSS = {
			prop.name + ": " + value.toCSS + (if (value.isImportant) " !important" else "") + ";\n"
		}
		
		def := (value:T) = {
			this.value = value
			this
		}
		
		def important = this
	}
	
	class Css {
		import scala.collection.mutable.ListBuffer
		
		val ruleSets = new ListBuffer[RuleSet]
		val rules = new ListBuffer[Rule]
		private var _current:Css = this
		
		def current = _current
		def current_=(cur:Css) = _current = cur

		implicit def toRuleSet (selector:SingleSelector) :RuleSet = {
			val r = RuleSet(this, selector)
			current.ruleSets.append(r)
			r
		}
		implicit def toRuleSet (selectors:List[SingleSelector]) = {
			val r = RuleSet(this, new MultipleSelector(selectors))
			current.ruleSets.append(r)
			r
		}
		implicit def toDeclaration [T <: CssValue[T]] (x:CssProperty[T]) :Declaration[T] = {
			var dec = new Declaration[T](x)
			current.rules.append( dec )
			dec
		}
	}
	
}

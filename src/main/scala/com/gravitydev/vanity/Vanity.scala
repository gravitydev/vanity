package com.gravitydev.vanity

object Vanity {
	case class CssProperty [T] (name:String)
	object fontFamily extends CssProperty[CssString]("font-family")
	object color extends CssProperty[CssColor]("color")
	object width extends CssProperty[CssScalar]("width")
	object height extends CssProperty[CssScalar]("height")
	object display extends CssProperty[CssString]("display")
	
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
	
	
	
	class NumberWrapper(num:Int) {
		private def wrap (unit:String) = new CssScalar(num, unit)
		def px = wrap("px")
	}
	
	implicit def toColorValue (number:Int) = new CssColor(number)
	implicit def toScalarValue (number:Int) = new NumberWrapper(number)
	implicit def toStringValue (s:String) = new CssString(s)
		
	def indent (s:String) = s.split("\n").map("  "+_).mkString("\n")
	
	trait Rule {
		def toCSS:String
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
			indent( declarations.map(_.toCSS).mkString ) + "\n" +
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

	object h1 extends SingleSelector("h1")
	object h2 extends SingleSelector("h2")
	object p extends SingleSelector("p")
	
	class Style extends Css {
		def toCSS = ruleSets.map(_.toCSS).mkString("\n")
	}
	
}
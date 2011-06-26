package com.gravitydev.vanity

object Model {
	
	case class CssProperty [T] (name:String)
	
	trait CssValue [T <: CssValue[T]] {
		self: T => 
		def toCSS : String
		def apply (s:String) = this
	}
	case class CssColor(color:Int) extends CssValue[CssColor] {
		def toCSS = "#" + "%06x".format(color)
	}
	case class CssScalar (pixels:Int, unit:String) extends CssValue[CssScalar] {
		def toCSS = pixels.toString + unit
		
		/* for conversion to border */
		def solid (color:CssColor) = CssBorder(this, CssString("solid"), color)
		def dotted (color:CssColor) = CssBorder(this, CssString("dotted"), color)
		def dashed (color:CssColor) = CssBorder(this, CssString("dashed"), color)
	}
	case class CssString (text:String) extends CssValue[CssString] {
		def toCSS = text
	}
	case class CssBorder (width:CssScalar, style:CssString, color:CssColor) extends CssValue[CssBorder] {
		def toCSS = width.toCSS + " " + style.toCSS + " " + color.toCSS
	}
	case class CssBox (top:CssScalar, right:CssScalar, bottom:CssScalar, left:CssScalar) extends CssValue[CssBox] {
		def toCSS = top.toCSS+" "+right.toCSS+" "+bottom.toCSS+" "+left.toCSS
	}
	object CssBox {
		def apply (vertical:CssScalar, horizontal:CssScalar) :CssBox = CssBox(vertical, horizontal, vertical, horizontal)
		def apply (all:CssScalar) :CssBox = CssBox(all, all, all, all)
	}
	
	trait CssBackground extends CssValue[CssBackground]
	case class SingleColorBackground(color:CssColor) extends CssBackground {
		def toCSS = color.toCSS
	}
	
	case class LinearGradient (start:String, color1:CssColor, color2:CssColor) extends CssBackground {
		def toCSS = "linear-gradient("+start+", "+color1.toCSS+", "+color2.toCSS+")"
	}
	
	trait Rule {
		def toCSS:String
	}
	
	class NumberWrapper(num:Int) {
		private def wrap (unit:String) = CssScalar(num, unit)
		def px() = wrap("px")
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
		
		var isImportant = false
		
		def toCSS = prop match {
			case x if x.name == "background" => value match {
				case g:LinearGradient => {
					"background: "+CssColor( (g.color1.color + g.color2.color) / 2 ).toCSS+ maybeImportant +";\n" +
					"background: -moz-linear-gradient("+g.start+", "+g.color1.toCSS+", "+g.color2.toCSS+")" + maybeImportant + ";\n" +
					"background: -webkit-gradient(linear, left top, left bottom, from("+g.color1.toCSS+"), to("+g.color2.toCSS+"))" + maybeImportant + ";\n" +
					"background: linear-gradient("+g.start+", "+g.color1.toCSS+", "+g.color2.toCSS+")" + maybeImportant + ";\n" +
					"filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='"+g.color1.toCSS+"', endColorstr='"+g.color2.toCSS+"')" + maybeImportant + ";\n"
				}
				case _ => prop.name + ": " + value.toCSS + maybeImportant + ";\n"
			}
			case _ => prop.name + ": " + value.toCSS + maybeImportant + ";\n"
		}
		
		private def maybeImportant = if (isImportant) " !important" else ""
		
		def := (value:T) = {
			this.value = value
			this
		}
		
		def :=! (value:T) = {
			this.value = value
			this.isImportant = true
			this
		}
		
		def := (v1:CssScalar, v2:CssScalar, v3:CssScalar, v4:CssScalar) = {
			this.value = CssBox(v1, v2, v3, v4).asInstanceOf[T]
			this
		}

		def ! = {
			isImportant = true
			this
		}
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

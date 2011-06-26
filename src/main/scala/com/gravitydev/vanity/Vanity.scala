package com.gravitydev.vanity

import Model._

object Vanity {
	object background extends CssProperty[CssBackground]("background")
	object color extends CssProperty[CssColor]("color")
	object fontWeight extends CssProperty[CssString]("font-weight")
	object height extends CssProperty[CssScalar]("height")
	object display extends CssProperty[CssString]("display")
	object border extends CssProperty[CssBorder]("border")
	object borderWidth extends CssProperty[CssBox]("border")
	object borderCollapse extends CssProperty[CssString]("border-collapse")
	object fontFamily extends CssProperty[CssString]("font-family")
	object fontSize extends CssProperty[CssScalar]("font-size")
	object maxHeight extends CssProperty[CssScalar]("max-height")
	object overflow extends CssProperty[CssString]("overflow")
	object padding extends CssProperty[CssBox]("padding")
	object margin extends CssProperty[CssBox]("margin")
	object width extends CssProperty[CssScalar]("width")
	
	implicit def toColorValue (number:Int) = CssColor(number)
	implicit def toScalarValue (number:Int) = new NumberWrapper(number)
	implicit def toStringValue (s:String) = CssString(s)
	implicit def toBoxValue (s:CssScalar) = CssBox(s)
	implicit def toBackground (number:Int) = SingleColorBackground(CssColor(number))
	
	def box (v:CssScalar) = CssBox(v)
	def box (v1:CssScalar, v2:CssScalar) = CssBox(v1, v2)
	def box (v1:CssScalar, v2:CssScalar, v3:CssScalar, v4:CssScalar) = CssBox(v1, v2, v3, v4)
	
	def linearGradient (start:String, color1:CssColor, color2:CssColor) = LinearGradient(start, color1, color2)
	
	// position
	val top = "top"
	val bottom = "bottom"
	val left = "left"
	val right = "right"
		
	// font-weight
	val bold = "bold"
	val normal = "normal"

	object $ extends SingleSelector("")
	object h1 extends SingleSelector("h1")
	object h2 extends SingleSelector("h2")
	object p extends SingleSelector("p")
	object td extends SingleSelector("td")
	object th extends SingleSelector("th")
	object div extends SingleSelector("div")
	
	class Style extends Css {
		def toCSS = ruleSets.map(_.toCSS).mkString("\n")
	}
}

package com.gravitydev.vanity

import Model._

object Vanity {
	object fontFamily extends CssProperty[CssString]("font-family")
	object color extends CssProperty[CssColor]("color")
	object width extends CssProperty[CssScalar]("width")
	object height extends CssProperty[CssScalar]("height")
	object display extends CssProperty[CssString]("display")
	
	implicit def toColorValue (number:Int) = new CssColor(number)
	implicit def toScalarValue (number:Int) = new NumberWrapper(number)
	implicit def toStringValue (s:String) = new CssString(s)

	object h1 extends SingleSelector("h1")
	object h2 extends SingleSelector("h2")
	object p extends SingleSelector("p")
	
	class Style extends Css {
		def toCSS = ruleSets.map(_.toCSS).mkString("\n")
	}
}
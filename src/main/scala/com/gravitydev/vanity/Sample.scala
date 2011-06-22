package com.gravitydev.vanity

import Vanity._

object Sample {
	def main (args:Array[String]){
		val test = 24
				
		val s = new Style {
			h1(".test") {
				fontFamily 	:= "sans-serif"
				color 		:= 234 important()
				color 		:= 242
				display 	:= "none"
				width		:= 24.px important()
				height		:= 100.px
			}
			
			List( p, h1, h2 ) {
				fontFamily	:= "sans-serif"
			}
			
			h1("#testblah") {
				color := 0x23444
				
				h2(".para") {
					color := 0x234244	
				}
			}
		}

		println( s.toCSS )
	}
}
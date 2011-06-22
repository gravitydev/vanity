package com.gravitydev.vanity

import Vanity._

object Sample {
	def main (args:Array[String]){
				
		val s = new Style {
			h1(".test") {
				fontFamily 	:= "sans-serif"
				color 		:= 0x00ff88 important()
				display 	:= "none"
				width		:= 24.px important()
				height		:= 100.px
			}
			
			List( p, h1, h2 ) {
				fontFamily	:= "sans-serif"
			}
			
			h1("#testblah") {
				color := 0x224444
				
				h2(".para") {
					color := 0x234244	
				}
			}
		}

		println( s.toCSS )
	}
}
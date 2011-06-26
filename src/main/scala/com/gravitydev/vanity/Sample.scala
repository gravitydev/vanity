package com.gravitydev.vanity

import Vanity._

object Sample {
	def main (args:Array[String]){
				
		val s = new Style {
			h1(".test") {
				fontFamily 	:= "sans-serif"
				background	:=! 0x000000
				color 		:=! 0x00ff88
				display 	:= "none"
				width 		:=! 24.px
				height		:= 100.px
				border		:= 2.px solid 0x444444
			}
			
			List( p, h1, h2 ) {
				fontFamily	:= "sans-serif"
				( margin := box( 2.px, 3.px ) ) !;
				padding := 10.px
			}
			
			h1("#testblah") {
				color := 0x224444
				
				h2(".para") {
					color := 0x234244
					background := linearGradient(top, 0xFFFFFF, 0x000000)
				}
			}
		}

		println( s.toCSS )
	}
}
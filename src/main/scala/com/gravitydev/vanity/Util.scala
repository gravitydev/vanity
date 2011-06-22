package com.gravitydev.vanity

object Util {
	def indent (s:String) = s.split("\n").map("  "+_).mkString("\n")
}
package com.pvteks.mvstuff

import org.scalatest.junit.{ JUnitSuite, ShouldMatchersForJUnit }
import org.scalatest.prop.{ Checkers }
import org.junit.Test
import org.scalacheck.{Test => SCTest,_}

import java.io.{ Console => _, _ }


class CoreTests extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  
  import mvstuff._

  def sha1_s2bv(sha1: String): Vector[Byte] = {
    
    def hex2byte(hex: String):Byte = {
      require(hex.length == 2)
      java.lang.Integer.parseInt(hex, 16).asInstanceOf[Byte]
    }
    Vector(sha1.sliding(2, 2).map(hex2byte(_)).toSeq:_*)
  }
  
  @Test def digest() {
    
    val a = new File("src/test/resources/a.txt")
    mkDigest(a) should equal (sha1_s2bv("4bae196a7ed6a46e68ca300d5a24f05f48bf7f10"))     // from sha1sum

    val aCopy = new File("src/test/resources/a_copy.txt")
    mkDigest(aCopy) should equal (sha1_s2bv("4bae196a7ed6a46e68ca300d5a24f05f48bf7f10"))
    
    val b = new File("src/test/resources/b.txt")
    mkDigest(b) should equal (sha1_s2bv("8ca6e3991eede2d4bdf6996870f3843e9de17996"))     // from sha1sum
  }
  @Test def sc() {
//    check { (n: Int) => Main.hw(n) == Main.hw }
  }
}
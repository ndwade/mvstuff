/*
    mvstuff License
    ==============================
    
    This software is released under BSD license, adapted from
    <http://opensource.org/licenses/bsd-license.php>
    
    ---
    
    Copyright (c) 2011, Panavista Technologies LLC.
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:
    
    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    
    * Neither the names "Panavista Technologies LLC", "pvteks.com", "mvstuff", nor the names
      of its contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE. 

*/
package com.pvteks.mvstuff

import org.scalatest.junit.{ JUnitSuite, ShouldMatchersForJUnit }
import org.scalatest.prop.{ Checkers }
import org.junit.{Test, Before, After}
import org.scalacheck.{Test => SCTest,_}

import java.io.{ Console => _, _ }

/**
 * Core tests for mvstuff.
 * 
 * NOTE: all the tests assume that the working directory is the path src/test/resources
 * In Eclipse, this can be effected by setting the working directory of the Run Configuration.
 * In SBT, no way to accomplish directly (see https://github.com/harrah/xsbt/issues/36)
 */
class CoreTests extends JUnitSuite with ShouldMatchersForJUnit with Checkers {
  
  import mvstuff._

  def sha1_s2bv(sha1: String): Vector[Byte] = {
    
    def hex2byte(hex: String):Byte = {
      require(hex.length == 2)
      java.lang.Integer.parseInt(hex, 16).asInstanceOf[Byte]
    }
    Vector(sha1.sliding(2, 2).map(hex2byte(_)).toSeq:_*)
  }
  
  @Before def cleanup() {
    rmRfd("temp")
  }
  @Test def digest() {

    println(sys.props("user.dir"))

    val a = new File("files" +/+ "a.txt")
    mkDigest(a) should equal (sha1_s2bv("4bae196a7ed6a46e68ca300d5a24f05f48bf7f10"))     // from sha1sum

    val aCopy = new File("files" +/+ "x" +/+ "y" +/+ "a_copy.txt")
    mkDigest(aCopy) should equal (sha1_s2bv("4bae196a7ed6a46e68ca300d5a24f05f48bf7f10"))
    
    val b = new File("files" +/+ "x" +/+ "b.txt")
    mkDigest(b) should equal (sha1_s2bv("8ca6e3991eede2d4bdf6996870f3843e9de17996"))     // from sha1sum
  }
  
  @Test def cpMkdirRmRfd() {
    
    mkdir("temp")
    val fDir = new File("temp")
    (fDir.exists && fDir.isDirectory) should be (true) 
    
    val fOrg = new File("files" +/+ "blah.jnk")
    fOrg.exists should be (true)
    
    val fCopy = new File("temp" +/+ "blah.jnk")
    fCopy.exists should be (false)
    
    cp(fOrg, fCopy)
    fCopy.exists should be (true)
    
    mkDigest(fOrg) should equal (mkDigest(fCopy))
    fOrg.digest should equal (fCopy.digest)
    fOrg.lastModified should equal (fCopy.lastModified)
    fOrg.length should equal (fCopy.length)
    
    rmRfd(fDir) should be (true)
    fDir.exists should not be (true)
  }
  
  val jnks = Set[String](
          "blah.jnk",
          "foo_copy.jnk",
          "x" +/+ "blah_copy.jnk",
          "x" +/+ "foo.jnk",
          "x" +/+ "y" +/+ "bar.jnk"
      ) map ("files" +/+ _)
      
  val txts = Set[String](
          "a.txt",
          "x" +/+ "b.txt",
          "x" +/+ "y" +/+ "a_copy.txt"
      ) map ("files" +/+ _)
  
  val xfiles = Set[String](
          "x" +/+ "b.txt",
          "x" +/+ "y" +/+ "a_copy.txt",
          "x" +/+ "blah_copy.jnk",
          "x" +/+ "foo.jnk",
          "x" +/+ "y" +/+ "bar.jnk"
      ) map ("files" +/+ _)
      
  val allDups = List(
          "foo_copy.jnk",
          "x" +/+ "blah_copy.jnk",
          "x" +/+ "y" +/+ "a_copy.txt"
      ) map ("files" +/+ _)

  val allFiles = txts ++ jnks 

  private def walk(ff: FileFilter, dir: String = "files") = {
    var files = List[File]()
    def walk(dir: File) {
      require (dir.exists && dir.isDirectory && dir.canRead)
      for (file <- dir.listFiles(ff)) files ::= file
      for (file <- dir.listFiles; if (file.isDirectory)) walk(file)
    }
    walk(new File(dir))
    val set = files.map(_.getPath).toSet
    files.size should equal (set.size)        // no dups! path names enforce this.
    set
  }

  @Test def extensionFileFilter() {
    walk(new ExtensionFileFilter("txt")) should equal (txts)
    walk(new ExtensionFileFilter("TxT")) should equal (txts)
    walk(new ExtensionFileFilter("jnk")) should equal (jnks)
    walk(new ExtensionFileFilter("tXt", "jnK")) should equal (txts ++ jnks)
  }
  
  val xfileRx = (".*" +/+ "x" +/+ ".*\\.(?i:jnk|TXT)").fixupR.r
  val rxff = new RegexFileFilter(xfileRx)
  
  @Test def regexFileFilter() {
    walk(rxff) should equal (xfiles)
  }
  
  def resultFiles = {
    val files = for (file <- (new File("temp")).listFiles; if (!file.isHidden)) yield file
    files.map(_.getPath drop ("temp".length + 1)).toSet 
  }
  @Test def indexedDestDirNoDups() {
    
    val xpected = (xfiles) map { dateString + '-' + _.replace(File.separator, "-") }
    
    _testabilityDate = Some(new java.util.Date)
    mkdir("temp")

    val idd = IndexedDestDir("temp")
    idd cpstuff xfileRx
    resultFiles should equal (xpected)
    idd.dups should equal (0)
    idd.inspectIndex()
    rmRfd("temp") should be (true)
  }
  
  @Test def indexedDestDirAll() {
    
    val expected = (allFiles -- allDups) map { dateString + '-' + _.replace(File.separator, "-") }
    
    _testabilityDate = Some(new java.util.Date)
    mkdir("temp")
    
    val idd = IndexedDestDir("temp")
    idd cpstuff (".*\\.(?i:jnk|TXT)").fixupR.r
    resultFiles should equal (expected)
    idd.dups should equal (3)
    idd.inspectIndex()
    idd.flush(); idd.close()
    
    val idd2 = IndexedDestDir("temp")
    println("idd2-pre")
    idd2.inspectIndex
    idd2 cpstuff (".*_copy\\.(?i:jnk|TXT)").fixupR.r
    resultFiles should equal (expected)
    idd2.dups should equal (3)
    println("idd2-post")
    idd2.inspectIndex()
    idd2.flush(); idd2.close()
    
    rmRfd("temp") should be (true)
    
    
  }
  
}

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
package com.pvteks.mvstuff.test

// import com.pvteks.mvstuff._
// import com.pvteks.mvstuff._
import com.pvteks.mvstuff.MvStuff._

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
  
  import SourceFileImplicits._
  
  def sha1_s2bv(sha1: String): Vector[Byte] = {
    
    def hex2byte(hex: String):Byte = {
      require(hex.length == 2)
      java.lang.Integer.parseInt(hex, 16).asInstanceOf[Byte]
    }
    Vector(sha1.sliding(2, 2).map(hex2byte(_)).toSeq:_*)
  }
  
  val testHome = "src"/"test"/"resources"

  def cpAndSetLastModified(src: String, dest: String) {
    
    val sf = new File(testHome/"files"/src)
    val df = new File(testHome/"files"/dest)
    cp(sf, df)
    sf.setLastModified(df.lastModified - 1)
    assert(sf.lastModified() < df.lastModified())
  }
  
  @Before def setup() {
    /*
     * files/
     *     a.txt
     *     blah.jnk
     *     foo_copy.jnk
     *     x/
     *         b.txt
     *         blah_copy.jnk
     *         foo.jnk
     *         y/
     *             a_copy.jnk
     *             bar.jnk
     *             
     */
    rmRfd(testHome/"temp")
    rmRfd(testHome/"files")
    mkdir(testHome/"temp")
    mkdir(testHome/"files")
    cpTree(testHome/"golden", testHome/"files")
    cpAndSetLastModified("x"/"foo.jnk", "foo_copy.jnk")
    cpAndSetLastModified("blah.jnk", "x" / "blah_copy.jnk")
    cpAndSetLastModified("a.txt", "x" / "y" / "a_copy.txt")
  }
  
  @Test def digest() {

    val a = new File(testHome/"files" / "a.txt")
    mkDigest(a) should equal (sha1_s2bv("4bae196a7ed6a46e68ca300d5a24f05f48bf7f10"))     // from sha1sum

    val aCopy = new File(testHome/"files" / "x" / "y" / "a_copy.txt")
    mkDigest(aCopy) should equal (sha1_s2bv("4bae196a7ed6a46e68ca300d5a24f05f48bf7f10"))
    
    val b = new File(testHome/"files" / "x" / "b.txt")
    mkDigest(b) should equal (sha1_s2bv("8ca6e3991eede2d4bdf6996870f3843e9de17996"))     // from sha1sum
  }
  
  @Test def cpMkdirRmRfd() {
        
    val fDir = new File(testHome/"temp")
    (fDir.exists && fDir.isDirectory) should be (true) 
    
    val fOrg = new File(testHome/"files" / "blah.jnk")
    fOrg.exists should be (true)
    
    val fCopy = new File(testHome/"temp" / "blah.jnk")
    fCopy.exists should be (false)
    
    cp(fOrg, fCopy)
    fCopy.exists should be (true)
    
    mkDigest(fOrg) should equal (mkDigest(fCopy))
    fOrg.lastModified should equal (fCopy.lastModified)
    fOrg.length should equal (fCopy.length)
    
    rmRfd(fDir) should be (true)
    fDir.exists should not be (true)
  }
  
  val jnks = Set[String](
          "blah.jnk",
          "foo_copy.jnk",
          "x" / "blah_copy.jnk",
          "x" / "foo.jnk",
          "x" / "y" / "bar.jnk"
      ) map ("files" / _)
      
  val txts = Set[String](
          "a.txt",
          "x" / "b.txt",
          "x" / "y" / "a_copy.txt"
      ) map ("files" / _)
  
  val xfiles = Set[String](
          "x" / "b.txt",
          "x" / "y" / "a_copy.txt",
          "x" / "blah_copy.jnk",
          "x" / "foo.jnk",
          "x" / "y" / "bar.jnk"
      ) map ("files" / _)
      
  val allDups = List(
          "foo_copy.jnk",
          "x" / "blah_copy.jnk",
          "x" / "y" / "a_copy.txt"
      ) map ("files" / _)

  val allFiles = txts ++ jnks 

  private def pathSet(path: String, ff: FileFilter): Set[String] = {
    val files = ls(new File(testHome/path), ff, r=true)
    val set = files.map(_.getPath.stripPrefix(testHome/)).toSet
    files.size should equal (set.size)        // no dups! path names enforce this.
    set
  }

  @Test def extensionFileFilter() {
    pathSet("files", new ExtensionFileFilter("txt")) should equal (txts)
    pathSet("files", new ExtensionFileFilter("TxT")) should equal (txts)
    pathSet("files", new ExtensionFileFilter("jnk")) should equal (jnks)
    pathSet("files", new ExtensionFileFilter("tXt", "jnK")) should equal (txts ++ jnks)
  }
  
  val xfileRx = ("files" / "x" / ".*\\.(?i:jnk|TXT)").escFileSep.r
  val rxff = new RegexFileFilter(xfileRx)(pwd/testHome)
  
  @Test def regexFileFilter() {
    pathSet("files", rxff) should equal (xfiles)
  }
 
  val srcPath = testHome/"files" 
  val destPath = testHome/"temp"
  val noHidden = new FileFilter { def accept(f: File): Boolean = !f.isHidden }

  def resultFiles = ls(new File(destPath), noHidden, r=true)
                    .map(_.getPath.stripPrefix(destPath/))
                    .toSet
  
  @Test def indexedDestDirNoDups() {
    
    testabilityDate = new java.util.Date
    val xpected = (xfiles) map { dateString + '-' + _.replace(File.separator, "-") }

    val idd = (testHome/"temp").asIDD.verbose 
    val sd = testHome.recursive.flatten.verbose.filter(xfileRx)
    sd copyTo idd
    resultFiles should equal (xpected)
    sd.dups should equal (0)
    idd.inspectIndex()
    rmRfd(testHome/"temp") should be (true)
  }
  
  @Test def indexedDestDirAll() {
    
    testabilityDate = new java.util.Date
    val expected = (allFiles -- allDups) map { dateString + '-' + _.replace(File.separator, "-") }

    val sd = testHome.recursive.flatten.verbose.filter(("files" / ".*\\.(?i:jnk|TXT)").escFileSep.r)
    val idd = (testHome/"temp").asIDD.verbose
    sd copyTo idd
      
    resultFiles should equal (expected)
    sd.dups should equal (3)
    idd.inspectIndex()
    idd.flush(); idd.close()
    
    println("idd2-pre")
    val sd2 = testHome.recursive.flatten.verbose
                      .filter(("files" / ".*_copy\\.(?i:jnk|TXT)").escFileSep.r)
    val idd2 = IndexedDestDir(testHome/"temp").verbose
    idd2.inspectIndex
    sd2 copyTo idd2
      
    resultFiles should equal (expected)
    sd2.dups should equal (3)
    println("idd2-post")
    idd2.inspectIndex()
    idd2.flush(); idd2.close()
    
    rmRfd(testHome/"temp") should be (true)
     
  }
  
  @Test def indexedDestDirAllMove() {
    
    testabilityDate = new java.util.Date
    val expected = (allFiles -- allDups) map { dateString + '-' + _.replace(File.separator, "-") }

    val sd = testHome.recursive.flatten.verbose.filter(("files" / ".*\\.(?i:jnk|TXT)").escFileSep.r)
    val idd = (testHome/"temp").asIDD.verbose
    sd moveTo idd
      
    resultFiles should equal (expected)
    sd.dups should equal (3)
    idd.inspectIndex()
    idd.flush(); idd.close()
    
    rmEmptyDirs(new File(testHome/"files")) should equal (true)

    rmRfd(testHome/"temp") should be (true)
     
  }
  
  @Test def indexedDestDirHierarchicalMove() {

    testabilityDate = new java.util.Date
    val expected = (allFiles map(_.stripPrefix("files"/))) -- 
      Seq("x"/"blah_copy.jnk", "x"/"foo.jnk", "x"/"y"/"a_copy.txt")

    val sd = (srcPath).recursive.verbose.filter((".*\\.(?i:jnk|TXT)").escFileSep.r)
    val idd = (destPath).asIDD.verbose
    sd moveTo idd
          
    resultFiles should equal (expected)
    sd.dups should equal (3)
    idd.inspectIndex()
    idd.flush(); idd.close()
    
    rmEmptyDirs(new File(testHome/"files")) should equal (true)
    
    rmRfd(testHome/"temp") should be (true)
  }
  
  @Test def indexedDestDirHierarchical() {

    testabilityDate = new java.util.Date
    val expected = (allFiles map(_.stripPrefix("files"/))) -- 
      Seq("x"/"blah_copy.jnk", "x"/"foo.jnk", "x"/"y"/"a_copy.txt")

    val srcPath  = testHome/"files"
    val destPath = testHome/"temp"
    
    val sd = (srcPath).recursive.verbose.filter((".*\\.(?i:jnk|TXT)").escFileSep.r)
    val idd = (destPath).asIDD.verbose
    sd copyTo idd
      
    resultFiles should equal (expected)
    sd.dups should equal (3)
    sd.copied should equal (5)
    idd.inspectIndex()
    idd.flush(); idd.close()
    
    /*
     * blow away the index, start over and see if dups eliminated
     */
    val indexFile = new File(testHome/"temp"/indexName)
    rm(indexFile) should be (true)
    
    val idd2 = (testHome/"temp").asIDD.verbose
    val sd2 = (srcPath).recursive.verbose.filter((".*\\.(?i:jnk|TXT)").escFileSep.r)
    sd2 moveTo idd2
    
    resultFiles should equal (expected)
    sd2.dups should equal (8)
    sd2.copied should equal (0)
    idd2.flush(); idd2.close()
    
    rmRfd(testHome/"temp") should be (true)

  }
  
  @Test def indexedDestDirNoRecursion() {
    /*
     * copy these files only:
     *     a.txt
     *     blah.jnk
     *     foo_copy.jnk
     */
    val srcPath  = testHome/"files"
    val destPath = testHome/"temp"
    val sd = srcPath.asSD.verbose
    val idd = (destPath).asIDD.verbose
    sd copyTo idd
      
    resultFiles should equal (Set("a.txt", "blah.jnk", "foo_copy.jnk"))
    sd.dups should equal (0)
    sd.copied should equal (3)
    idd.inspectIndex()
    idd.flush(); idd.close()
    
    rmRfd(testHome/"temp") should be (true)
  }
  
}

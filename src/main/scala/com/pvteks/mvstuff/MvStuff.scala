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

/**
 * A focussed set of primitives to shove large quantities of files in random directory structures
 * into a single, managed directory, eliminating duplicates, and preserving time order.
 * 
 * Appropriate for "stuff" which is non-critical but which is worth hanging on to.
 * 
 * Rather than create an all singing, all dancing utility with lots of options, a set of 
 * methods is exposed in a package object which may be used to construct interpreted scala scripts 
 * for specific purposes.
 */

/*
 * TODO:
 * 
 * wtf is up with the ./ stuff
 * 
 */
package com.pvteks.mvstuff

// object `package` extends `package`
/*
 * note: package object stuff doesn't quite work out
 * 
 *  since breakpoints cannot be set within objects, would like to put functionality 
 *  in a companion class
 * 
 *  but then nested classes and the implicit conversions to nested classes aren't "seen" 
 *  unless the clients use
 *  
 *      import com.blah.`package`._
 * 
 *  further, the Eclipse IDE is unhappy with the arrangement; scala plugin generates spurious
 *  syntax errors involving non-existent circular references to `package` class - these disappear
 *  when project is cleaned, and do not appear from scalac command line invocation.
 *  (and yes, I tried using a file "package.scala")
 * 
 *  attempts to work around these limitations are postponed until basic functionality 
 *  is more stable.
 *  
 *  suggested approach is a small dedicated test project which can be used as example in mailing
 *  list posting.  
 */

object MvStuff extends MvStuff

/**
 * Since we can't set breakpoints within objects, this is how we roll. Yo. 
 */
class MvStuff private[mvstuff] {

  import scala.collection.mutable
  import scala.Console._
  import java.io.{ Console => _, _ }
  import java.security.{ MessageDigest, DigestInputStream }

  /*
   * private crap
   */
  private val indexName = ".mvstuff-index"

  private val md = MessageDigest.getInstance("SHA")

  private def esc(s: String): String = """([ (){}\\$&#;])""".r.replaceAllIn(s, """\""" + _)
  
  case class PathString(string: String) {
    def +/ = string + File.separatorChar
    def +/+ (that: String) = string.+/ + that   
    def fixupR = if (File.separatorChar == '\\') string.replace("\\", "\\\\") else string
  }
  implicit def string2pathstring(string: String) = PathString(string)

  type Closeable = { def close(): Unit } 
  def withCloseable[C <: Closeable](c: C)(body: => Unit) {
    try { body }
    finally { c.close() }
  }
  
  type Flushable = { def flush(): Unit } 
  def withFlushable[F <: Flushable with Closeable](f: F)(body: => Unit) {
    try { body }
    finally { withCloseable(f) { f.flush() } }
  }
  
  def insist(ok: Boolean) { if (!ok) throw new IllegalStateException("not OK!") }
  
  /**
   * Make a sha1 digest for a <code>File</code>
   */
  def mkDigest(file: File): Vector[Byte] = {
      md.reset()
      val is = new DigestInputStream(new BufferedInputStream(new FileInputStream(file)), md)
      val buffer = new Array[Byte](4096)
      withCloseable(is) { while (is.read(buffer) != -1) {} }
      Vector(md.digest(): _*)
  }
  
  case class Digest(digest: Vector[Byte]) {
    require (digest.length == 20)
    def toHexString: String = digest map (_ formatted "%02x") reduce (_ + _)
  }
  implicit def bv2digest(bv: Vector[Byte]) = Digest(bv)

  private def now = new java.util.Date
  private lazy val ymd_HM = "ymd_HM" flatMap (c => if (c == '_') "" + c else "%1$t" + c)
  
  private[mvstuff] def testabilityDate = _testabilityDate
  private[mvstuff] def testabilityDate_=(date: java.util.Date) { _testabilityDate = Some(date) }
  private[this] var _testabilityDate: Option[java.util.Date] = None
  /**
   * Date as yymmdd_HHMM.
   */
  def dateString: String = testabilityDate getOrElse now formatted ymd_HM
  

  /**
   * A pimped <code>File</code>, with digest and destination name.
   */
  class SourceFile(val file: File) {
    require(file.exists)
    val digest = mkDigest(file)
    val outFileName = 
            dateString + '-' + {
              if (file.getParent == ".") "" 
              else (file.getParent stripPrefix ("." +/)).replace(File.separator, "-") + '-' 
            } +
            esc(file.getName)
  }
  
  object SourceFile {
      def apply(file: File) = new SourceFile(file)
      def apply(name: String) = new SourceFile(new File(name))
  }
  
  implicit def file2sourceFile(file: File) = new SourceFile(file)
  implicit def sourceFile2file(srcf: SourceFile) = srcf.file
 
  /** 
   * A list of file extensions as a <code>FileFilter</code>.
   */
  class ExtensionFileFilter(exts: String*) extends FileFilter {
    
    private val extensions = Set[String]() ++ exts map ("." + _.toLowerCase) 
    
    def accept(f: File): Boolean = {
      val dot = f.getName lastIndexOf "."
      if (dot != -1) extensions contains ((f.getName substring dot).toLowerCase) else false
    }
  }
  
  // import scala.util.matching._
  /**
   * A generalized <code>Regex FileFilter</code>.
   */
  import util.matching._
  class RegexFileFilter(regex: Regex) extends FileFilter {
    def accept(f: File): Boolean = {
      val path = f.getPath stripPrefix ("." +/)
      regex.findPrefixOf(path) map { _.length == path.length } getOrElse false
    }
  }

  def mkdir(name: String) { mkdir (new File(name)) }
  
  def mkdir(dir: File) {
    if (!dir.mkdir()) throw new RuntimeException("can't make " + dir)  
  }

  def rm(path: String):Boolean = rm(new File(path))
  
  def rm(file: File): Boolean = {
    val ok= file.delete() 
    if (!ok) err.println("could not delete file " + file)
    ok
  }
  
  /**
   * Equivalent to <code>$ rm -rfd</code>
   */
  def rmRfd(dirPath: String): Boolean = {
    val dir = new File(dirPath);
    dir.exists && dir.isDirectory && dir.canRead && rmRfd(dir)
  }
  
  def rmRfd(dir: File): Boolean = {
    require (dir.exists && dir.isDirectory && dir.canRead)
    var ok = true
    for (file <- dir.listFiles) {
      if (file.isDirectory) ok &= rmRfd(file)
      else ok &= file.delete()
    }
    ok &= dir.delete()
    if (!ok) err.println("could not delete dir " + dir)
    ok
  }
  
  /**
   * Only remove empty(ish) directories.
   */
  def rmEmptyDirs(dir: File) {
    for (d <- dir.listFiles; if (d.isDirectory)) rmEmptyDirs(d)
    val dss = new File(dir, ".DS_Store")        // Finder love!
    if (dss.exists) rm(dss)
    if (dir.list.isEmpty) rm(dir) else println("NotEmpty? : " + dir)
  }
  
  /**
   * Copy manually - rename doesn't work across volumes, etc...
   */
  def cp(from: String, to: String): Boolean = cp(new File(from), new File(to))

  def cp(from: SourceFile, to: File): Boolean = {
    val is = new FileInputStream(from)
    val os = new FileOutputStream(to)
    val buffer = new Array[Byte](4096 * 16)
    var bytesRead = 0
    def readBytes() : Boolean = {
      bytesRead = is.read(buffer)
      bytesRead != -1
    }
    def writeBytes() = os.write(buffer, 0, bytesRead)
    withFlushable(os) { while (readBytes()) writeBytes() }
    to setLastModified from.lastModified
    from.digest == to.digest
  }  
  
  /**
   * copy a whole tree. TODO: reconsider the whole naming thing...  
   */
  def cpTree(from: String, to: String) { cpTree(new File(from), new File(to)) }
  
  def cpTree(from: File, to: File) {
    require (from.exists && from.isDirectory && from.canRead)
    require (to.exists && to.isDirectory && to.canRead && to.canWrite)
    for (file <- from.listFiles) {
      if (file.isDirectory) { 
        val subTo = new File(to, file.getName); 
        mkdir(subTo); 
        cpTree(file, subTo) 
      } else {
        cp(file, new File(to, file.getName))
      }
    }
  }
  
  def lsR(
      path: String = ".",
      ff: FileFilter = new FileFilter { def accept(f: File) = true }
    ): List[File] = lsR(new File(path), ff)
    
  def lsR(maybeDir: File, ff: FileFilter): List[File] = {
    require (maybeDir.exists && maybeDir.canRead)
    if (!maybeDir.isDirectory) {
      if (ff accept maybeDir) List(maybeDir) else Nil
    } else {
      maybeDir.listFiles.toList flatMap (lsR(_, ff))
    }    
  }
  
  /**
   * Represents and indexed directory. Files are stored flat in the directory (we rely on Spotlight
   * to find stuff), and duplicates are elminated by checking against an index of sha1 digests.
   */
  class IndexedDestDir private (val dir: File, private[this] var _verbose: Boolean) {

    require (dir.exists && dir.isDirectory && dir.canRead && dir.canWrite)
    
    def verbose = _verbose
    def verbose_=(v: Boolean) { _verbose = v }
    
    val dm = mutable.Map[Vector[Byte],  String]()
    val index = new File(dir, indexName)

    if (!index.exists) {
      println(dir.getPath + ": creating index...")
      insist(index.createNewFile())                     // ensures index file is in index as entry
      val dos = new DataOutputStream(new FileOutputStream(index))
      withFlushable(dos) {
        for (file <- dir.listFiles; if (file.isFile && !file.isHidden)) {
          writeEntry(dos, file.digest, file.getName)
          dm(file.digest) = file.getName
        }
      }
      println(dir.getPath + ": wrote digest db for " + dm.size + " files")
    }
    // sic
    {
      println(dir.getPath + ": reading index...")
      val dis = new DataInputStream(new FileInputStream(index))
      val digestBuffer = new Array[Byte](20)
      def readEntry() = {
        val n = dis.read(digestBuffer)
        if (n == -1) throw new java.io.EOFException     // intentional flow control
        val digest = Vector(digestBuffer: _*) 
        val name = dis.readUTF()
        if (verbose) println("  read entry " + digest.toHexString + " -> " + name)
        (digest, name)
      }
      withCloseable(dis) { 
        try { while (true) dm += readEntry() } catch { case _:  EOFException => () }
      }
      println(dir.getPath + ": read " + dm.size + " index entries")
    }

    private[this] val dos = new DataOutputStream(new FileOutputStream(index, true))
    
    private[this] var _copied = 0
    def copied: Int = _copied
    
    private[this] var _dups = 0;
    def dups: Int = _dups
    
    def indexAndMoveFrom(src: SourceFile) {
      indexAndCopyFrom(src, true)
    }
    
    def indexAndCopyFrom(src: SourceFile, deleteSource: Boolean = false) {
      if (dm contains src.digest) {
        _dups += 1
        println("dup: " + src.getPath + " is identical to " + dm(src.digest))
      } else {
        val dest = new File(dir, src.outFileName)
        val ok = deleteSource && (src renameTo dest) || cp(src, dest)
        if (!ok) { 
          err.println("failed to copy " + src.getPath + " to " + dest.getPath)
          return
        } else {
          println("copied " + src.getPath + " to " + dest.getPath)
          writeEntry(dos, src.digest, src.outFileName)
          dm(src.digest) = src.outFileName
          _copied += 1
        }
      }
      if (deleteSource && src.exists && !rm(src)) 
        err.println("failed to delete " + src.getPath)
    }
    
    private def dostuff(ff: FileFilter, deleteSource: Boolean) {
      withFlushable(this) {
        lsR(".", ff) sortWith (_.file.lastModified < _.file.lastModified) map { indexAndCopyFrom(_, deleteSource) }
      } 
      println("copied " + copied + " files to " + dir)
    }
      
    def inspectIndex() { for ((digest, name) <- dm) println(digest.toHexString + " -> " + name) }
    
    def flush() { dos.flush() }
    def close() { dos.close() }
    
    private[this] def writeEntry(dos: DataOutputStream, digest: Vector[Byte], name: String) {
      require (digest.size == 20)
      dos.write(digest.toArray, 0, 20)
      dos.writeUTF(name)
      if (verbose) println("  writing entry " + digest.toHexString + " -> " + name)
    }
    
    @inline def !+>>: (exts: String*)   { mvstuff(exts:_*) }
    @inline def !+>>: (regex: Regex)    { mvstuff(regex) }
    @inline def !+>>: (ff: FileFilter)  { mvstuff(ff) }
    
    @inline def <<+!  (exts: String*)   { mvstuff(exts:_*) }
    @inline def <<+!  (regex: Regex)    { mvstuff(regex) }
    @inline def <<+!  (ff: FileFilter)  { mvstuff(ff) }
    
    @inline def ++>>: (exts: String*)   { cpstuff(exts:_*) }
    @inline def ++>>: (regex: Regex)    { cpstuff(regex) }
    @inline def ++>>: (ff: FileFilter)  { cpstuff(ff) }
    
    @inline def <<++  (exts: String*)   { cpstuff(exts:_*) }
    @inline def <<++  (regex: Regex)    { cpstuff(regex) }
    @inline def <<++  (ff: FileFilter)  { cpstuff(ff) }
    
    @inline def mvstuff(exts: String*)  { mvstuff(new ExtensionFileFilter(exts:_*)) }
    @inline def mvstuff(regex: Regex)   { mvstuff(new RegexFileFilter(regex)) }
    @inline def mvstuff(ff: FileFilter) { dostuff(ff, true) }
    
    @inline def cpstuff(exts: String*)  { cpstuff(new ExtensionFileFilter(exts:_*)) }
    @inline def cpstuff(regex: Regex)   { cpstuff(new RegexFileFilter(regex)) }
    @inline def cpstuff(ff: FileFilter) { dostuff(ff, false) }    
  }
    
  object IndexedDestDir {
    def apply(name: String, verbose: Boolean = false) = 
      new IndexedDestDir(new File(name), verbose)
  }
}

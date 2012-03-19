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

  private def esc(s: String): String = """([ (){}\\$&#;])""".r.replaceAllIn(s, """\""" + _)
  
  class PathString(string: String) {
    def / : String = string + File.separatorChar
    def / (that: String): String = string./ + that   
    def escFileSep = if (File.separatorChar == '\\') string.replace("""\""", """\\""") else string
  }
  implicit def string2pathstring(string: String) = new PathString(string)

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
  private val md = MessageDigest.getInstance("SHA")

  def mkDigest(file: File): Vector[Byte] = {
      md.reset()
      val is = new DigestInputStream(new BufferedInputStream(new FileInputStream(file)), md)
      val buffer = new Array[Byte](4096)
      withCloseable(is) { while (is.read(buffer) != -1) {} }
      Vector(md.digest(): _*)
  }
  
  implicit def bv2digest(bv: Vector[Byte]) = new {	// monkey pimping
    require (bv.length == 20)
    def toHexString: String = bv map (_ formatted "%02x") reduce (_ + _)
  }

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
   * A pimped <code>File</code>, with destination name.
   */
  class SourceFile(private[mvstuff] val _file: File, val rootPath: String) {
    require(_file.exists && _file.canRead)
    lazy val canonicalFile = _file.getCanonicalFile
    lazy val relativePath = canonicalFile.getPath.stripPrefix(rootPath/)
    lazy val flatName = dateString + '-' + {
      canonicalFile.getParent.stripPrefix(rootPath/).replace(File.separator, "-") + '-'
    } + esc(canonicalFile.getName)
  }
  
  object SourceFileImplicits {
    implicit def file2sourceFile(file: File)(implicit evrp: String) = 
      new SourceFile(file, evrp)
    implicit def sourceFile2file(srcf: SourceFile) = srcf._file
  }
  import SourceFileImplicits._
 
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
  
  // implicit def exts2ff(exts: String*) = new ExtensionFileFilter(exts: _*)

  /**
   * A generalized <code>Regex FileFilter</code>.
   */
  def pwd = (new File(".")).getCanonicalPath
  
  class RegexFileFilter(regex: util.matching.Regex)(implicit evrp: String) extends FileFilter {
    def accept(f: File): Boolean = {
      val path = f.relativePath
      regex.findPrefixOf(path) map (_.length == path.length) getOrElse false
    }
  }

  // implicit def regex2ff(regex: util.matching.Regex)(implicit evrp: String) = new RegexFileFilter(regex)
  
  val passAllFF = new FileFilter { def accept(f: File) = true }

  /*
   * directory and file CRUD
   */
  def mkdir(name: String): File = { mkdir(new File(name)) }
  
  def mkdir(dir: File) = {
    if (!dir.mkdir()) throw new RuntimeException("can't make " + dir)
    dir
  }

  def rm(path: String): Boolean = rm(new File(path))
  
  def rm(file: File): Boolean = {
    val kind = if (file.isDirectory) "dir " else "file "
    val ok= file.delete() 
    if (!ok) err.println("could not delete " + kind + file)
    ok
  }
  
  /**
   * Equivalent to <code>$ rm -rfd</code>
   */
  def rmRfd(dirPath: String): Boolean = {
    val dir = new File(dirPath);
    dir.exists && dir.isDirectory && dir.canRead && rmRfd(dir)
  }

  def rmRfd(file: File): Boolean = {
    require (file.exists && file.canRead)
    val ok = if (file.isDirectory) file.listFiles.map(rmRfd(_)).fold(true){ _ & _ } else true
    ok & rm(file)
  }
  
  /**
   * Only remove empty(ish) directories.
   */
  def rmEmptyDirs(dir: File): Boolean = {
    require (dir.exists && dir.isDirectory && dir.canRead)
    val ok = dir.listFiles.filter(_.isDirectory).map(rmEmptyDirs(_)).fold(true){ _ & _ }
    val dss = new File(dir, ".DS_Store")        // Finder love!
    if (dss.exists) rm(dss)
    ok & (if (dir.list.isEmpty) rm(dir) else { err.println("NotEmpty? : " + dir); false })
  }
  
  /**
   * Copy manually - rename doesn't work across volumes, etc...
   */
  def cp(from: File, to: File, checkDigest: Boolean = true): Boolean = {
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
    if (checkDigest) mkDigest(from) == mkDigest(to) else true
  }  
    
  /**
   * copy a whole tree. 
   */
  def cpTree(from: String, to: String): Boolean = cpTree(new File(from), new File(to))
  
  def cpTree(src: File, 
             dest: File, 
             ff: FileFilter = passAllFF, 
             cpf: (File, File) => Boolean = cp(_, _)): Boolean = {
    require (src.exists && src.isDirectory && src.canRead)
    require (dest.exists && dest.isDirectory && dest.canRead && dest.canWrite)
    def _cpt(src: File, dest: File): Boolean = {
      require (src.exists && src.canRead)
      require (dest.exists && dest.isDirectory && dest.canRead && dest.canWrite)
      if (!src.isDirectory) {
        cpf(src, new File(dest, src.getName))
      } else {
        val subDest = mkdir(new File(dest, src.getName))
        src.listFiles.map(_cpt(_, subDest)).fold(true)(_ & _) 
      }
    }
   src.listFiles.map(_cpt(_, dest)).fold(true)(_ & _) 
  }
  
  def ls(file: File = new File("."), 
         ff: FileFilter = passAllFF, 
         r: Boolean = false): List[File] = {
    require (file.exists && file.canRead)
    if (!file.isDirectory) {
      if (ff accept file) List(file) else Nil
    } else if (r) {
      file.listFiles.toList flatMap (ls(_, ff, r))
    } else Nil
  }
  
  /**
   * Represents and indexed directory. Files are stored flat in the directory (we rely on Spotlight
   * to find stuff), and duplicates are elminated by checking against an index of sha1 digests.
   *
   * new features:
   * - allow specification of source dir (removes need for running in same dir as source)
   * - make recursive traversal of source directories optional
   * - make flattening of dest tree optional.
   * 
   * "sub/stuff"
   */
  class IndexedDestDir private (val dir: File) {

    require (dir.exists && dir.isDirectory && dir.canRead && dir.canWrite)
    
    val srcDir = new File(".").getCanonicalFile
    implicit val rp = srcDir.getPath

    def asIDD = this
    private[this] var _verbose = false
    def verbose: IndexedDestDir = { _verbose = true; this }
    
    val dm = mutable.Map[Vector[Byte],  String]()
    val index = new File(dir, indexName)

    if (!index.exists) {
      println(dir.getPath + ": creating index...")
      insist(index.createNewFile())                     // ensures index file is in index as entry
      val dos = new DataOutputStream(new FileOutputStream(index))
      withFlushable(dos) {
        for (file <- dir.listFiles; if (file.isFile && !file.isHidden)) {
          val digest = mkDigest(file)
          writeEntry(dos, digest, file.getName)
          dm(digest) = file.getName
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
        if (_verbose) println("  read entry " + digest.toHexString + " -> " + name)
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
    
    def indexAndCopyFrom(src: SourceFile, deleteSource: Boolean = false) {
      // err.println("file: " + src.file + " rp: " + src.rootPath)
      val digest = mkDigest(src)
      if (dm contains digest) {
        _dups += 1
        println("dup: " + src.getPath + " is identical to " + dm(digest))
      } else {
        val dest = new File(dir, src.flatName)
        val ok = deleteSource && (src renameTo dest) || cp(src, dest)
        if (!ok) { 
          err.println("failed to copy " + src.getPath + " to " + dest.getPath)
          return
        } else {
          println("copied " + src.getPath + " to " + dest.getPath)
          writeEntry(dos, digest, src.flatName)
          dm(digest) = src.flatName
          _copied += 1
        }
      }
      if (deleteSource && src.exists && !rm(src)) 
        err.println("failed to delete " + src.getPath)
    }
    
    private def dostuff(ff: FileFilter, deleteSource: Boolean) {
      withFlushable(this) {
                
        ls(file=srcDir, ff=ff, r=true) sortWith (_.canonicalFile.lastModified < _.canonicalFile.lastModified) map { 
          indexAndCopyFrom(_, deleteSource) 
        }
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
      if (_verbose) println("  writing entry " + digest.toHexString + " -> " + name)
    }
    
    @inline def mvstuff(exts: String*)  { mvstuff(new ExtensionFileFilter(exts:_*)) }
    @inline def mvstuff(regex: util.matching.Regex)   { mvstuff(new RegexFileFilter(regex)) }
    @inline def mvstuff(ff: FileFilter) { dostuff(ff, true) }
    
    @inline def cpstuff(exts: String*)  { cpstuff(new ExtensionFileFilter(exts:_*)) }
    @inline def cpstuff(regex: util.matching.Regex)   { cpstuff(new RegexFileFilter(regex)) }
    @inline def cpstuff(ff: FileFilter) { dostuff(ff, false) }    
    
    /* 
     * new stuff
     */
    
    private[mvstuff] def dupeName(digest: Vector[Byte]): Option[String] = dm.get(digest)
    
    private[mvstuff] def writeIndexEntry(digest: Vector[Byte], path: String) {
      writeEntry(dos, digest, path)
      dm(digest) = path
      dos.flush();	// ?!
    }
  }
    
  object IndexedDestDir {
    def apply(name: String, verbose: Boolean = false) = 
      new IndexedDestDir(new File(name))
  }
  
  implicit def string2idd(dir: String): IndexedDestDir = IndexedDestDir(dir)

  final class SourceDir(path: String) {
    
    val dir = new File(path).getCanonicalFile
    require (dir.exists && dir.isDirectory && dir.canRead)
    
    implicit val rp = dir.getPath
    
    private[this] var _copied = 0
    private[this] var _dups = 0

    private var _verbose = false
    private var _flatten = false
    private var _recursive = false
    private var _rmSrc = false
    private var _ff = passAllFF
    
    private def dostuff(idd: IndexedDestDir): Boolean = {
      
      def _cp(src: File, dest: File): Boolean = {
        val digest = mkDigest(src)
        val ok = idd.dupeName(digest) match {
          case Some(name) => {
            println("dup: " + src.relativePath + " is identical to " + name) 
            _dups += 1
            true // considered success
          }
          case None => {
            if (_rmSrc && (src renameTo dest) || cp(src, dest, true)) {
              val name = if (_flatten) src.flatName else src.relativePath
              idd.writeIndexEntry(digest, name)
              _copied += 1
              if (_verbose) println("copied " + src.getPath + " to " + dest.getPath)
              true
            } else { 
              err.println("failed to copy " + src.relativePath + " to " + dest.getPath)
              false
            }
          }
        }
        if (ok && _rmSrc && src.exists && !rm(src)) err.println("failed to delete " + src.getPath)
        if (!ok) err.println("failed to copy " + src.relativePath + " to " + dest.getPath)
        ok
      }
      val ok = if (_flatten) {
        ls(file=dir, ff=_ff, r=_recursive)
          .sortWith(_.canonicalFile.lastModified < _.canonicalFile.lastModified)
          .map(src => _cp(src, new File(idd.dir, src.flatName)))
          .fold(true)(_ & _)
      } else {
        cpTree(src=dir, dest=idd.dir, ff=_ff, cpf=_cp(_, _))
      }
      println("copied " + copied + " files")
      ok
    }
    
    /*
     * public API
     */
    def asSD = this
    def verbose: SourceDir = { _verbose = true; this }
    /** when flattened, earlier timestamps are preferred. 
      * When hierarchical, files higher in the hierarchy are preferred 
      * - within level preference is undefined*/
    def flatten: SourceDir = { _flatten = true; this }
    def recursive: SourceDir = { _recursive = true; this }
    
    def filter(ff: FileFilter): SourceDir = { _ff = ff; this}
    def filter(regex: util.matching.Regex): SourceDir = { _ff = new RegexFileFilter(regex); this}
    def filter(exts: String*): SourceDir = { _ff = new ExtensionFileFilter(exts: _*); this}
    
    def copyTo(dest: IndexedDestDir): Boolean = dostuff(dest) 
    def moveTo(dest: IndexedDestDir): Boolean = { _rmSrc = true; dostuff(dest) }

    def copied: Int = _copied    
    def dups: Int = _dups
  }

  object SourceDir {
    def apply(dir: String = ".") = new SourceDir(dir)
  }

  implicit def string2sourceDir(dir: String) = SourceDir(dir)
  
}


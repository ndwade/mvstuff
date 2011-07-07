package com.pvteks.mvstuff


/**
 * 
 */
object `mvstuff` extends `mvstuff` 

class `mvstuff` private[mvstuff] {

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
    def +/+ (that: String): String = string + File.separatorChar + that   
    def +/ = string + File.separatorChar
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
  
  /**
   * Make a sha1 digest for a <code>File</code>
   */
  def mkDigest(file: File): Vector[Byte] = {
      md.reset()
      val is = new DigestInputStream(new BufferedInputStream(new FileInputStream(file)), md)
      val buffer = new Array[Byte](4096)
      withCloseable(is) { while (is.read(buffer) != -1) {} }
      Vector(md.digest():_*)
  }
  
  case class Digest(digest: Vector[Byte]) {
    require (digest.length == 20)
    def toHexString: String = digest map (_ formatted "%02x") reduce (_ + _)
  }
  implicit def bv2digest(bv: Vector[Byte]) = Digest(bv)

  private def now = new java.util.Date
  private lazy val ymd_HM = "ymd_HM".flatMap(c => if (c == '_') "" + c else "%1$t" + c)
  var _testabilityDate: Option[java.util.Date] = None      // for testability
  /**
   * Date as yymmdd_HHMM.
   */
  def dateString: String = {
    val date = _testabilityDate match { case Some(testingDate) => testingDate; case None => now }
    date formatted ymd_HM
  }

  /**
   * A pimped <code>File</code>, with digest and destination name.
   */
  class SourceFile(val file: File) {
    require(file.exists)
    val digest = mkDigest(file)
    val path = {
      val parentPath = file.getParent
      if (parentPath.substring(0, 2) == ("." +/)) parentPath.substring(2) else parentPath
    }
    val outFileName = 
            dateString + '-' + 
            path.replace(File.separator, "-") + '-' + 
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
    def accept(f: File): Boolean = regex.findPrefixOf(f.getPath) match {
      case Some(s) if (s.length == f.getPath.length) => true    // full match
      case _ => false
    }
  }

  def mkdir(name: String) {
    val dir = new File(name)
    if (!dir.mkdir()) throw new RuntimeException("can't make " + dir)  
  }


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
    if (dir.list.isEmpty) rm(dir) else err.println("NotEmpty? : " + dir)
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
   * Represents and indexed directory. Files are stored flat in the directory (we rely on Spotlight
   * to find stuff), and duplicates are elminated by checking against an index of sha1 digests.
   */
  class IndexedDestDir private (val dir: File) {
    
    require (dir.exists && dir.isDirectory && dir.canRead && dir.canWrite)
    def this(name: String) = this(new File(name))
    
    val dm = mutable.Map[Vector[Byte],  String]()
    val index = new File(dir, indexName)
    
    if (!index.exists) {
      err.println("creating index for directory " + this)
      val oos = new ObjectOutputStream(new FileOutputStream(index))
      withFlushable(oos) {
        for (file <- dir.listFiles; if (file.isFile)) {
          oos.writeObject((file.digest, oos.writeObject(file.getName))) 
        }
      }
      println(dir.getName + ": wrote digest db for " + dm.size + " files")
    }   // sic
    
    {
      val ios = new ObjectInputStream(new FileInputStream(index))
      withCloseable(ios) {
        @annotation.tailrec
        def loop() {
          val entry = ios.readObject().asInstanceOf[Tuple2[Vector[Byte], String]]
          if (entry != null) {
            dm += entry
            loop()
          } 
        }
      }
    }
    
    private[this] val oos = new ObjectOutputStream(new FileOutputStream(index, true))   // append
    
    private[this] var _dups = 0;
    def dups: Int = _dups
    
    def indexAndMoveFrom(src: SourceFile) {
      indexAndCopyFrom(src, true)
    }
    
    def indexAndCopyFrom(src: SourceFile, deleteSource: Boolean = false) {
      if (dm contains src.digest) {
        _dups += 1
        err.println("dup: " + src.getPath + " is identical to " + dm(src.digest))
      } else {
        val dest = new File(dir, src.outFileName)
        val ok = deleteSource && (src renameTo dest) || cp(src, dest)
        if (!ok) { 
          err.println("failed to copy " + src.getPath + " to " + dest.getPath)
          return
        } else {
          val entry = (src.digest, src.outFileName)
          dm += entry
          oos.writeObject(entry)
        }
      }
      if (deleteSource && src.exists && !rm(src)) 
        err.println("failed to remove " + src.getPath)
    }
    
    @inline def mvstuff(exts: String*)  { mvstuff(new ExtensionFileFilter(exts:_*)) }
    @inline def mvstuff(regex: Regex)   { mvstuff(new RegexFileFilter(regex)) }
    @inline def mvstuff(ff: FileFilter) { dostuff(ff, true) }
    
    @inline def cpstuff(exts: String*)  { cpstuff(new ExtensionFileFilter(exts:_*)) }
    @inline def cpstuff(regex: Regex)   { cpstuff(new RegexFileFilter(regex)) }
    @inline def cpstuff(ff: FileFilter) { dostuff(ff, false) }
    
    private def dostuff(ff: FileFilter, deleteSource: Boolean) {
      var files = List[File]()
      def processDir(dir: File) {
        require(dir.exists && dir.isDirectory && dir.canRead)
        for (file <- dir.listFiles(ff)) files ::= file
        for (sub <- dir.listFiles; if (sub.isDirectory)) processDir(sub)
      }
      processDir(new File("."))
      for (file <- files.sortWith(_.file.lastModified < _.file.lastModified)) { 
        indexAndCopyFrom(file, deleteSource) 
      }
    }
      
    def inspectIndex() { for ((digest, name) <- dm) println(digest.toHexString + " -> " + name) }
    
    def flush() { oos.flush() }
    def close() { oos.close() }
  }
    
  object IndexedDestDir {
    def apply(name: String) = new IndexedDestDir(new File(name))
  }
}

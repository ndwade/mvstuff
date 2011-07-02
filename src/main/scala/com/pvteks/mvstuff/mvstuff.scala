package com.pvteks.mvstuff

/*
 * TODO: new version.
 * 
 * in the target directory, create a .mvstuff/ hidden folder, with a file "index" which is the 
 * database. Make it ASCII, so it can be edited. (Or maybe just a .mvstuff-index file).
 * maybe have conversion between ASCII and binary? See how slow ASCII is. Maybe OK.
 * 
 * mvstuff --init dir
 *      computes a new index based on existing directory contents. (Warns if there
 * are subdirs)
 * 
 * mvstuff fromdir todir
 *      
 * 
 * 
 * 
 * 
 */
object `mvstuff` {

  import scala.collection.mutable
  import scala.Console._
  import java.io.{ Console => _, _ }
  import java.security.{ MessageDigest, DigestInputStream }

  private[mvstuff] val dbName = "uniqueDigestDB"

  private[mvstuff] val md = MessageDigest.getInstance("SHA")

  private[mvstuff] def esc(s: String): String = """([ (){}\\$&#;])""".r.replaceAllIn(s, """\""" + _)
  
  def mkDigest(file: File): Vector[Byte] = {
      md.reset()
      val is = new DigestInputStream(new BufferedInputStream(new FileInputStream(file)), md)
      val buffer = new Array[Byte](4096)
      while (is.read(buffer) != -1) {}
      is.close() // FIXME: this should really be exception wrapped
      Vector(md.digest():_*)
  }
  
  private[mvstuff] case class FileAttributes(file: File, outFileName: String) {
    val digest = mkDigest(file)
  }
  
  private[mvstuff] final class ExtensionFileFilter(exts: String*) extends FileFilter {
    
    private val extensions = Set[String]() ++ exts flatMap (
        s => List("." + s.toLowerCase, "." + s.toUpperCase))
    
    def accept(f: File): Boolean = {
      val dot = f.getName lastIndexOf "."
      if (dot != -1) extensions contains (f.getName substring dot) else false
    }
  }

  private[mvstuff] case class MediaAttributes(dir: File, fileFilter: FileFilter) {
    val db = new File(dir, dbName)
  }

  private[mvstuff] val mas = mutable.ListBuffer[MediaAttributes]()
  
  def addMedia(destDir: File, exts: String*) {
    mas += new MediaAttributes(destDir, new ExtensionFileFilter(exts:_*))
  }

  private[mvstuff] val datePrefix = {
    val ymd_HM = "ymd-HM".flatMap(c => if (c == '-') "" + c else "%1$t" + c)
    (new java.util.Date) formatted ymd_HM
  }
  
  private[mvstuff] def doDir(dir: File, xff: FileFilter): List[FileAttributes] = {
    
    var fas: List[FileAttributes] = Nil
    
    def doDir(stack: List[File]) {

      val dir = stack.head
      require(dir.canRead && dir.isDirectory)
      
      def pathName(file: File): String = {
        datePrefix + 
        (stack.reverse map(f => esc(f.getName)) mkString("-", "-", "-")) + 
        esc(file.getName)
      }
      for (f <- dir.listFiles(xff)) fas ::= new FileAttributes(f, pathName(f))
      for (d <- dir.listFiles; if (d.isDirectory)) doDir(d :: stack)
    }
    doDir(dir :: Nil)
    fas
  }
  
  private[mvstuff] def digestMap(db: File): mutable.Map[Vector[Byte], String] = {
    val ios = new ObjectInputStream(new FileInputStream(db))
    ios.readObject().asInstanceOf[mutable.Map[Vector[Byte], String]]
  }
  
  def rmFile(file: File): Boolean = {
    val ok= file.delete() 
    if (!ok) err.println("could not delete file " + file.getName)
    ok
  }
  
  def rmEmptyDirs(dir: File) {
    for (d <- dir.listFiles; if (d.isDirectory)) rmEmptyDirs(d)
    val dss = new File(dir, ".DS_Store")        // Finder love!
    if (dss.exists) rmFile(dss)
    if (dir.list.isEmpty) rmFile(dir) else err.println("NotEmpty? : " + dir)
  }
  
  private[mvstuff] def mkDb(ma: MediaAttributes) {
    val label = ma.dir.getName
    var fas = doDir(ma.dir, ma.fileFilter)
    println("\n\n" + label + ": total files: " + fas.size)
    fas = fas.sortWith(_.file.lastModified < _.file.lastModified)
    println("sorted by date")
    val dm = mutable.Map[Vector[Byte], String]()
    for (fa <- fas) {
      if (dm contains fa.digest) {
        println("dup: " + fa.file.getName + " is identical to " + dm(fa.digest))
        rmFile(fa.file)
      } else {
        dm(fa.digest) = fa.outFileName
      }
    }
    val oos = new ObjectOutputStream(new FileOutputStream(ma.db))
    oos.writeObject(dm); oos.flush()
    println(label + ": wrote digest db for " + dm.size + " files")
  }
  
  def mvFile(from: File, to: File, digest: Option[Vector[Byte]] = None): Boolean = {
    // try the easy way first
    if (from renameTo to) return true
    // ok - need to copy and then delete - 'to' file must be on remote volume or something
    if (!cpFile(from, to, digest)) return false
    rmFile(from)
  }
  
  def cpFile(from: File, to: File, digest: Option[Vector[Byte]] = None): Boolean = {
    val is = new FileInputStream(from)
    val os = new FileOutputStream(to)
    val buffer = new Array[Byte](4096 * 16)
    var bytesRead = 0
    def readBytes() : Boolean = {
      bytesRead = is.read(buffer)
      bytesRead != -1
    }
    def writeBytes() = os.write(buffer, 0, bytesRead)
    while (readBytes()) writeBytes()
    mkDigest(to) == (digest match {
      case Some(d) => d
      case None => mkDigest(from)
    })
  }
  
  private[mvstuff] def mvFile(fa: FileAttributes, toDir: File): Boolean = {
    mvFile(fa.file, new File(toDir, fa.outFileName), Some(fa.digest))
  }

  private[mvstuff] def mvMedia(ma: MediaAttributes) {
    val label = ma.dir.getName
    val userDir = new File(scala.util.Properties.userDir)
    var fas = doDir(userDir, ma.fileFilter)
    println("\n\n" + label + " total files: " + fas.size)
    fas = fas.sortWith(_.file.lastModified < _.file.lastModified)
    println("sorted by date")
    val dm = digestMap(ma.db)
    var dups = 0
    for (fa <- fas) {
      if (dm contains fa.digest) {
        dups += 1
        println("dup: " + fa.file.getAbsolutePath + " is identical to " + dm(fa.digest))
        rmFile(fa.file)
      } else {
        mvFile(fa, ma.dir)
        dm(fa.digest) = fa.outFileName
      }
    }
    rmEmptyDirs(userDir)
  }
  
  def mvAllMedia() { mas map (mvMedia _) }
  def mkAllDbs() { mas map (mkDb _) }
  
}


/*
 * var k: K = _
 * val foo: = ListBuffer[String]()
 * lazy val zed = opts.get("zed")
 * opts('k', "kludge", "an elegant option of type K") = { k = _ }
 * opts('f', "foo", "a list of Strings") = { foo += _ }
 * 
 * opts parse args
 */

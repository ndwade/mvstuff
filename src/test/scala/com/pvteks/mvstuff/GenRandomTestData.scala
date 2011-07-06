package com.pvteks.mvstuff

import mvstuff._
import java.io._

/*
 * Data for testing - binary "Ipsum Lorem", if you will... 
 */
object GenRandomTestData extends App {
  
  /*
   * each arg is a path to the prospective data file to be created
   */
  val root = "src" +/+ "test" +/+ "resources" +/+ "files"
  val specs = Vector(
          ("blah.jnk",                  271), 
          ("x" +/+ "foo.jnk",           618), 
          ("x" +/+ "y" +/+ "bar.jnk",   314)
      ) map { case (path, size) => (root +/+ path, size) }
          
  val bbuf = new Array[Byte](1024)
  val rnd = new util.Random(0xBADF82BF00D4AF00L)

  for ((path, size) <- specs) {  
    val dos = new DataOutputStream(new FileOutputStream(path))
    withFlushable(dos) {
      for (i <- 0 until size) {
        rnd.nextBytes(bbuf)
        dos.write(bbuf)
      }
    }
  }
  var ok = cp(specs(0)._1, root +/+ "x" +/+ "blah_copy.jnk"); assert(ok)
  ok = cp(specs(1)._1, root +/+ "foo_copy.jnk"); assert(ok)
}
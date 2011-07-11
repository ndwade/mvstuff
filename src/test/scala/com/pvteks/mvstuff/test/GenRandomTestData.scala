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
import com.pvteks.mvstuff.MvStuff._

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
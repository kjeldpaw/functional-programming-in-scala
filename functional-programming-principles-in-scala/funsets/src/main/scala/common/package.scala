// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
import java.io.File

package object common {

  /** An alias for the `Nothing` type.
    * Denotes that the type should be filled in.
    */
  type ??? = Nothing

  /** An alias for the `Any` type.
    * Denotes that the type should be filled in.
    */
  type *** = Any


  /**
    * Get a child of a file. For example,
    *
    * subFile(homeDir, "b", "c")
    *
    * corresponds to ~/b/c
    */
  def subFile(file: File, children: String*): File = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }

  /**
    * Get a resource from the `src/main/resources` directory. Eclipse does not copy
    * resources to the output directory, then the class loader cannot find them.
    */
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists) {
      Some(new java.io.FileInputStream(resourceFile))
    } else {
      None
    }
  }
}

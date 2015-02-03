/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

/** PackageMap provides an information map of a package containing item content.
  * It locates scheme resources via package-relative file names.
  *
  * @author richardrodgers
  */

case class PackageMap(id: Int, pkgmapId: String, description: String, swordUrl: Option[String])

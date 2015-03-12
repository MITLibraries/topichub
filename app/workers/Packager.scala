/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import scala.collection.immutable.HashMap

import java.io.{ByteArrayInputStream, FileInputStream, InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.net.URL
import java.util.zip.{ZipEntry, ZipOutputStream}

import akka.actor.Actor

import play.api.Play.current
import play.api.libs.ws._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.xml.pull._

import models.{Item, PackageMap}

/** Packager is a worker that assembles content packages according
  * to the specifications in a packageMap
  *
  * @author richardrodgers
  */

class PackageWorker extends Actor {
  def receive = {
    case item: Item => Packager.packageItem(item)
    case _ => println("Unknown package command")
  }
}

class Packager {

  def packageItem(item: Item): InputStream = {
    // check the package cache - if already created, skip build
    if (! Packager.inCache(item.objKey)) {
      // build a new package & cache it
      Packager.toCache(item.objKey, buildPackage(item))
    }
    Packager.fromCache(item.objKey)
  }

  def buildPackage(item: Item) = {
      val pkgName = Packager.pkgDir.resolve(item.objKey)
      val zout = new ZipOutputStream(Files.newOutputStream(pkgName))
      // mets file is an entry
      writeEntry("mets.xml", new ByteArrayInputStream(item.toMets.toString.getBytes), zout)
      // each item resource is a also an entry
      item.metadataValues("accessUri").foreach { uri =>
        writeEntry(item.filename(uri), new URL(uri).openStream, zout)
      }
      zout.close
      pkgName
  }

  private def writeEntry(name: String, in: InputStream, zout: ZipOutputStream) = {
    zout.putNextEntry(new ZipEntry(name))
    streamCopy(in, zout)
    in.close
    zout.closeEntry
  }

  private def streamCopy(in: InputStream, out: OutputStream) = {
    Iterator.continually(in.read).takeWhile(-1 !=).foreach(out.write)
  }
}

object Packager {

  // temporary cache - maybe move to a persistent cache?
  val pkgDir = Files.createTempDirectory("SWORDPKGS")

  var pkgCache: Map[String, Path] = new HashMap

  def inCache(key: String) = pkgCache.contains(key)

  def toCache(key: String, pkg: Path) = pkgCache += key -> pkg

  def fromCache(key: String): InputStream = {
    Files.newInputStream(pkgCache.get(key).get)
  }

  def packageItem(item: Item) = {
    new Packager().packageItem(item)
  }
}

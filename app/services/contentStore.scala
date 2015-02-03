/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package services

import java.io.{File, FileInputStream, FileOutputStream, InputStream}
import java.net.URL
import java.security.{DigestInputStream, MessageDigest, NoSuchAlgorithmException}
import java.util.zip.{ZipFile, ZipEntry, ZipInputStream}

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.{AmazonS3, AmazonS3Client}
import com.amazonaws.services.s3.model.{S3Object, GetObjectRequest}

import play.api._
import play.api.libs.Files.TemporaryFile
import play.api.Play.current

import models.Item
//import controllers.SwordError

/** Traits, classes and objects to abstract details of item content storage.
  * Storage may be package-based or individual resources.
  * Current implementations for package-based: local file system or
  * Amazon S3 storage; for unpackaged, a remote HTTP-based read-only store
  * which is essentially the publisher's site/service
  * TODO: refactor for real plug-in modularity
  *
  * @author richardrodgers
  */

trait ContentStore {
  def contains(oid: String): Boolean
  def store(file: File, oid: String): Unit
  def retrieve(oid: String): StoredContent
}

trait StoredContent {
  def content: InputStream
  def resource(name: String): InputStream
  def oid: String
  def mimetype: String = "application/zip"
}

/**
  * Storage in a managed directory in the local file system.
  * Files are stored in a shallow directory tree, with naming convention
  *     xx/xxyyyyyyy.zip
  * where xx represents the first two characters of the MD5 of the file,
  * and xxyyyyy is the entire checksum
  */
object LocalStore extends ContentStore {
  // Assumes format 'file:///directory/path'
  val rootDir = Play.configuration.getString("hub.item.store").get.substring(7)

  override def contains(oid: String) = storedFile(oid).exists

  // optimization - rename, rather than copy, file
  override def store(file: File, oid: String) = file.renameTo(storedFile(oid, true))

  override def retrieve(oid: String) = LocalZipFile(storedFile(oid), oid)

  private def storedFile(oid: String, make: Boolean = false) = {
    val subDir = new File(rootDir, oid.substring(0, 2))
    if (make) subDir.mkdirs
    new File(subDir, oid + ".zip")
  }
}

case class LocalZipFile(pkg: File, oid: String) extends StoredContent {

  val zip = new ZipFile(pkg)

  override def content = new FileInputStream(pkg)

  override def resource(name: String): InputStream  = {
    val entry = zip.getEntry(name)
    if (entry != null) zip.getInputStream(entry) else null
  }
}

/**
  * Amazon S3 storage.
  * Files are all stored with object IDs = file md5s in a single bucket
  * configured via the URL in the 'hub.item.store' property.
  * Bucket must be created before use.
  */
object S3Store extends ContentStore {
  // Assumes format 'http://bucketname.s3.amazonaws.com'
  val url = Play.configuration.getString("hub.item.store").get
  val bucket = url.substring(7, url.indexOf(".s3.amazonaws.com"))
  val accessKey = Play.configuration.getString("hub.aws.accesskey").get
  val secretKey = Play.configuration.getString("hub.aws.secretkey").get
  val s3 = new AmazonS3Client(new BasicAWSCredentials(accessKey, secretKey))

  override def contains(oid: String) = {
    try {
      s3.getObjectMetadata(bucket, oid)
      true
    } catch {
      case _ : Throwable => false
    }
  }

  override def store(file: File, oid: String) {
    s3.putObject(bucket, oid, file)
    // remove local temp file
    file.delete
  }

  override def retrieve(oid: String) = {
    // defer actual retrieval until needed - to ensure http connections to S3 are conserved
    S3ZipFile(s3, bucket, oid)
  }
}

case class S3ZipFile(s3: AmazonS3, bucket: String, oid: String) extends StoredContent {

  override def content = s3.getObject(new GetObjectRequest(bucket, oid)).getObjectContent

  override def resource(name: String): InputStream = {
    val s3o = s3.getObject(new GetObjectRequest(bucket, oid))
    // filter entries until name found
    val zin = new ZipInputStream(s3o.getObjectContent)
    var entry: ZipEntry = null
    while({entry = zin.getNextEntry; entry != null}) {
      if (entry.getName.equals(name)) {
        return zin
      }
    }
    null
  }
}

case class RemoteStore(baseUrl: String) extends ContentStore {
    override def contains(oid: String) = false
    // NB: this store does not implement storage, only retrieval
    override def store(file: File, oid: String) {}
    override def retrieve(oid: String) = new RemoteContent(baseUrl, oid)
}

case class RemoteContent(baseUrl: String, oid: String) extends StoredContent {
  // NB - packaging not yet implemented
  def content: InputStream = ???
  def resource(name: String): InputStream = new URL(baseUrl + name).openConnection.getInputStream
}

object Store {
  //val storeCfg = Play.configuration.getString("hub.item.store").get
  // crude test for now, only 2 backends supported
  //val cstore = if (storeCfg.startsWith("file")) LocalStore else S3Store

/*
  def upload(tempFile: TemporaryFile, maxSize: Int, md5: String): Either[SwordError.Value, StoredContent] = {
    // create temp upload file location
    var tmpFile = File.createTempFile("collId", ".zip")
    val fout = new FileOutputStream(tmpFile)
    var count = 0
    var buf = new Array[Byte](4096)
    val din = new DigestInputStream(new FileInputStream(tempFile.file), MessageDigest.getInstance("MD5"))
    var read = 0
    var quit = false
    while (read != -1 && ! quit) {
      read = din.read(buf)
      if (read > 0)  {
        count += read
        if (maxSize > -1 && count > maxSize)  {
          // bail out
          quit = true
        }
        fout.write(buf, 0, read)
      }
    }
    val ulMd5 = toHex(din.getMessageDigest().digest())
    din.close
    fout.close
    //tempFile.close
    if (quit) {
      // blow away partial upload, and return error
      tmpFile.delete()
      Left(SwordError.MaxUploadSizeExceeded)
    } else if (md5 != null && ! md5.equals(ulMd5)) {
      // now make sure checksums agree
      tmpFile.delete()
      Left(SwordError.ErrorChecksumMismatch)
    } else if (cstore.contains(ulMd5)) {
      // reject as duplicate
      tmpFile.delete()
      Left(SwordError.DuplicateContent)
    } else {
      // save file
      cstore.store(tmpFile, ulMd5)
      Right(cstore.retrieve(ulMd5))
    }
  }
  */

  // return stored content associated with item
  def content(item: Item) = {
    val loc = item.location
    val cstore = if (loc.startsWith("remote:"))
      new RemoteStore(loc.substring(7))
      else if (loc.startsWith("s3:")) S3Store else LocalStore
    cstore.retrieve(item.objKey)
  }

  // TODO
  // def listContents: List[StoredContent]

  private def toHex(bytes: Array[Byte]): String =
     bytes.map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}

}

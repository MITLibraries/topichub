/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date

import play.api._
import play.api.Play.current

/** HubUtils contains common view rendering methods - it is not a model and only appears
  * in this package because the latter is automatically in scope in view templates.
  *
  * @author richardrodgers
  */

object HubUtils {

  import java.time.ZoneId
  import java.time.format.DateTimeFormatter

  val iso8601 = DateTimeFormatter.ISO_LOCAL_DATE

  def fmtDate(date: Date) = {
    date.toInstant.atZone(ZoneId.systemDefault).toLocalDate.format(iso8601)
  }

  def fmtPreciseDateTime(date: Date) = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss:SSS");
    date.toInstant.atZone(ZoneId.systemDefault).toLocalDateTime.format(formatter)
  }

  def advanceDate(curDate: Date, days: Int): Date = {
    // clumsily convert to Java8 date for easy calculation of daily advance,
    // but convernt back to old Date for JDBC ease of use
    val newDt = curDate.toInstant.atZone(ZoneId.systemDefault).toLocalDateTime.plusDays(days)
    Date.from(newDt.atZone(ZoneId.systemDefault).toInstant)
  }

  def pluralize(amt: Int, word: String): String = pluralize(amt.toLong, word)

  def pluralize(amt: Long, word: String) = {
    amt match {
      case 0L => "No " + word + "s"
      case 1L => "One " + word
      case _ => amt + " " + word + "s"
    }
  }

  def interpolate(token: String, full: Boolean = false) = {
    val start = token.indexOf("${")
    if (start >= 0) {
      val end = token.indexOf("}", start + 2)
      if (full) {
        token.substring(0, start) + Play.configuration.getString(token.substring(start + 2, end)).get + token.substring(end + 1)
      } else Play.configuration.getString(token.substring(start + 2, end)).get
    } else
      token
  }

  def isAnalyst(user: Option[User]) = {
    if (user.isEmpty) false
    else user.get.role.indexOf("analyst") >= 0 || user.get.role.indexOf("admin") >= 0
  }

  def extractCredentials(x: String, url: String): String = {
    val Array(indexUsername, indexPassword, _) = url.stripPrefix("https://").split(":|@")
    if (x == "username") {
      indexUsername
    } else {
      indexPassword
    }
  }
}

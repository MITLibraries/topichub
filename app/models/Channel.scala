/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date

/** Channel contains coordinates and credentials to effect a content transfer
  * or notification between a hub and a specified address or endpoint of a subscriber
  * or publisher.
  *
  * @author richardrodgers
  */

case class Channel(id: Int, protocol: String, mode: String, direction: String, description: String,
                  userId: String, password: String, channelUrl: String, created: Date, updated: Date, transfers: Int)

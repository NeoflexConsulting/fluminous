package org.fluminous

/**
 * External environment settings
 * servers is the Map between OpenApi documents and servers URL, specific for environment */
case class Settings(servers: Map[String, String])

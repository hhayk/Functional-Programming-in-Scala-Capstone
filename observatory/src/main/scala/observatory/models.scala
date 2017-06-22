package observatory

import math._

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(stn: Option[String], wban: Option[String], lat: Option[Double], lon: Option[Double])

case class Temperature(stn: Option[String], wban: Option[String], month: Option[Int], day: Option[Int], temperature: Option[Double])

case class Tile(x: Double, y: Double, z: Int, w: Double, h: Double) {
  def toLatLon = LatLonPoint(
    toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << z))))),
    x.toDouble / (1 << z) * w - h,
    z, w, h)

  def toLocation = {
    Location(toLatLon.lat, toLatLon.lon)
  }

  def toURI = new java.net.URI("http://tile.openstreetmap.org/" + z + "/" + x + "/" + y + ".png")
}

case class LatLonPoint(lat: Double, lon: Double, z: Int, w: Double, h: Double) {
  def toTile = Tile(
    ((lon + h) / w * (1 << z)).toInt,
    ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / Pi) / 2.0 * (1 << z)).toInt,
    z, w, h)
}

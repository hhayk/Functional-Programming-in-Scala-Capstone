package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(stn: Option[String], wban: Option[String], lat: Option[Double], lon: Option[Double])

case class Temperature(stn: Option[String], wban: Option[String], month: Option[Int], day: Option[Int], temperature: Option[Double])


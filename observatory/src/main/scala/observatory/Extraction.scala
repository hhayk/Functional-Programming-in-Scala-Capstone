package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.SparkSession

/**
  * 1st milestone: data extraction
  */
object Extraction {
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsRDD = spark.sparkContext.textFile(fsPath(stationsFile)).map(_.split(",", 4)).map(line => Station(
      stn = if (line(0) == "") None else Some(line(0)),
      wban = if (line(1) == "") None else Some(line(1)),
      lat = if (line(2) == "") None else Some(line(2).toDouble),
      lon = if (line(3) == "") None else Some(line(3).toDouble)
    )).filter(st => st.lat.isDefined && st.lat.isDefined)
      .map(p => ((p.stn, p.wban), p))

    val temperaturesRDD = spark.sparkContext.textFile(fsPath(temperaturesFile)).map(_.split(",", 5)).map(line => Temperature(
      stn = if (line(0) == "") None else Some(line(0)),
      wban = if (line(1) == "") None else Some(line(1)),
      month = if (line(2) == "") None else Some(line(2).toInt),
      day = if (line(3) == "") None else Some(line(3).toInt),
      temperature = if (line(4) == "") None else Some(line(4).toDouble)
    )).map(p => ((p.stn, p.wban), p))

    stationsRDD.join(temperaturesRDD).mapValues(p => (
      LocalDate.of(year, p._2.month.getOrElse(0), p._2.day.getOrElse(0)),
      Location(p._1.lat.get, p._1.lon.get),
      (p._2.temperature.getOrElse(9999.9) - 32) * 5 / 9
    )).values.collect()
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.par.groupBy(_._2).mapValues(p => p.map(_._3).sum / p.size).seq
  }
}

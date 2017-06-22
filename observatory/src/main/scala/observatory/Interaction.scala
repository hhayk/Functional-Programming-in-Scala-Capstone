package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = Tile(x, y, zoom, 360, 180).toLocation

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val s = 256
    val pixels: IndexedSeq[Pixel] = for {
      j <- 0 until s
      i <- 0 until s
      l = Tile(x, y, zoom, s, s).toLocation
      pt = predictTemperature(temperatures, l)
      c = interpolateColor(colors, pt)
    } yield Pixel(c.red, c.green, c.blue, 127)

    Image.apply(s, s, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    yearlyData.foreach {
      case (year, data) => {
        for {
          z <- 0 to 3
          zz = 1 << z
          x <- 0 until zz
          y <- 0 until zz
        } generateImage(year, z, x, y, data)
      }
    }
  }

}

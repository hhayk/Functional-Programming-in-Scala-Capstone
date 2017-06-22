package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    def gcd(loc1: Location, loc2: Location): Double = {
      import math._
      val loc1Rad = (toRadians(loc1.lat), toRadians(loc1.lon))
      val loc2Rad = (toRadians(loc2.lat), toRadians(loc2.lon))
      val latDelt = abs(loc1Rad._1 - loc2Rad._1)
      val lonDelt = abs(loc1Rad._2 - loc2Rad._2)
      val k = pow(sin(latDelt / 2), 2) + cos(loc1Rad._1) * cos(loc2Rad._1) * pow(sin(lonDelt / 2), 2)
      val d = 2 * atan2(sqrt(k), sqrt(1 - k))
      val radius = 6372.8
      radius * d
    }

    val tmp = temperatures.map(t2 => (gcd(t2._1, location), t2._2))
    tmp.find(_._1 == 0) match {
      case Some((d, t)) => t
      case None => {
        val (ws, iws) = tmp.foldLeft((0.0, 0.0))((acc, t2) => {
          val w = 1.0 / math.pow(t2._1, 3)
          (acc._1 + t2._2 * w, acc._2 + w)
        })
        ws / iws
      }
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    def lerp(a: Int, b: Int, f: Double): Int = math.round(a + f * (b - a)).toInt

    val tmp = points.toList.sortBy(_._1)
    tmp.find(_._1 == value) match {
      case Some(p) => p._2
      case None => {
        (tmp.takeWhile(_._1 < value).lastOption, tmp.dropWhile(_._1 < value).headOption) match {
          case (Some(p1), Some(p2)) => {
            val f = (value - p1._1) / (p2._1 - p1._1)
            val r = lerp(p1._2.red, p2._2.red, f)
            val g = lerp(p1._2.green, p2._2.green, f)
            val b = lerp(p1._2.blue, p2._2.blue, f)
            Color(r, g, b)
          }
          case (Some(p1), None) => p1._2
          case (None, Some(p2)) => p2._2
          case _ => Color(0, 0, 0)
        }
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    //    import scala.language.implicitConversions
    //    implicit def colorToColor(color: Color): com.sksamuel.scrimage.Color = com.sksamuel.scrimage.Color(color.red, color.green, color.blue)

    //    //       90
    //    // -180  0  180
    //    //      -90
    //    val w = 360
    //    val h = 180
    //    val pixels: IndexedSeq[Pixel] = for {
    //      j <- 0 until h
    //      i <- 0 until w
    //      l = Location(i - w / 2, h / 2 - j)
    //      pt = predictTemperature(temperatures, l)
    //      c = interpolateColor(colors, pt)
    //    } yield Pixel(c)
    //
    //    Image.apply(w, h, pixels.toArray)

    val w = 360
    val h = 180
    val pixels: IndexedSeq[Pixel] = for {
      j <- 0 until h
      i <- 0 until w
      l = Tile(0, 0, 0, w, h).toLocation
      pt = predictTemperature(temperatures, l)
      c = interpolateColor(colors, pt)
    } yield Pixel(c.red, c.green, c.blue, 127)

    Image.apply(w, h, pixels.toArray)
  }

}


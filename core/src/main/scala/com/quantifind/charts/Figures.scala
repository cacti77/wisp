package com.quantifind.charts

import com.quantifind.charts.highcharts.BoxplotData
import com.quantifind.charts.highcharts.Highchart
import com.quantifind.charts.highcharts.Histogram
import com.quantifind.charts.highcharts.LeastSquareRegression
import com.quantifind.charts.highcharts.Series
import com.quantifind.charts.highcharts.SeriesType
import com.quantifind.charts.repl.BinnedData
import com.quantifind.charts.repl.IterablePair

object Figures {
  private val figures = scala.collection.mutable.Map[Int, Highcharts]()

  // A value of 0 indicates no figure exists yet.
  private var currentFigure = 0

  // Mirrors Matlab's 'gcf' command: get current figure (number)
  def gcf = currentFigure

  def newFigure(): Unit = {
    currentFigure = figures.size + 1
    figures += (currentFigure -> new Highcharts())
  }

  def getNumFigures = figures.size

  /**
   * Gets the current figure itself. If no figure exists then one is created.
   */
  private def getFigure(): Highcharts = {
    if (figures.isEmpty) {
      newFigure()
    }
    figures(currentFigure)
  }

  /**
   * Mirrors Matlab's 'close' command to close the specified figure.
   */
  def close(figure: Int): Unit = {
    figures(figure).deleteAll()
    figures.remove(figure)
    if (figure == currentFigure) {
      // Find the largest key value remaining and set currentFigure to that.
      // We could maintain a list of figures the user has worked with but that's
      // more work!
      var max = 0
      figures.foreach{ case(key, value) => if (key > max) max = key }
      currentFigure = max
    }
  }

  /**
   * Closes (deletes) all the figures.
   */
  def closeAll(): Unit = {
    figures.foreach{ case(key, value) => figures(key).deleteAll() }
    figures.clear()
    currentFigure = 0
  }

  /**
   * Switches focus to the specified figure.
   */
  def figure(figure: Int) = {
    if (figures contains figure) {
      currentFigure = figure
    } else {
      throw new NoSuchElementException("Unknown figure: " + figure)
    }
    currentFigure
  }

  private def isInZeppelin = sys.env.get("ZEPPELIN_HOME").isDefined

  def zPlot(figure: Int = currentFigure): Unit = {
    if (isInZeppelin) {
      print("%html " + figures(figure).getHtml)
    }
  }

  // The following methods mirror (and call) the corresponding ones in the
  // Highcharts class...

  def area[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.area)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def areaspline[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.areaspline)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def bar[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.bar)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def boxplot[T: Numeric](data: Iterable[BoxplotData[T]]) = {
    val fig = getFigure()
    fig.plot(Highchart(series = Seq(Series(data = data, chart = Some(SeriesType.boxplot)))))
    zPlot()
  }

  def column[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.column)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def histogram[A: Numeric](data: Iterable[A], numBins: Int) = {
    val fig = getFigure()
    val binCounts = fig.binIterableNumBins(data, numBins).toBinned().toSeq
    fig.plot(Histogram.histogram(binCounts))
    zPlot()
  }

  def histogram(data: BinnedData) = {
    val fig = getFigure()
    val binCounts = data.toBinned().toSeq
    fig.plot(Histogram.histogram(binCounts))
    zPlot()
  }

  def line[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.line)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def pie[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.pie)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def regression[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    def numericToDouble[X](x: X)(implicit ev: Numeric[X]): Double = ev.toDouble(x)
    val (xr, yr) = xy.toIterables
    LeastSquareRegression.leastSquareRegression(fig, xr.toSeq.map(numericToDouble(_)), yr.toSeq.map(numericToDouble(_)))
    zPlot()
  }

  def scatter[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.scatter)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def spline[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.spline)
    fig.plot(fig.addStyle(hc, xy))
    zPlot()
  }

  def help(): Unit = {
    // TODO Add help specific for Figures once we're happy with its methods and usage.
    val fig = getFigure()
    fig.help()
  }
}

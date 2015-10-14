package com.quantifind.charts

import com.quantifind.charts.highcharts.BoxplotData
import com.quantifind.charts.highcharts.Highchart
import com.quantifind.charts.highcharts.Histogram
import com.quantifind.charts.highcharts.LeastSquareRegression
import com.quantifind.charts.highcharts.Series
import com.quantifind.charts.highcharts.SeriesType
import com.quantifind.charts.repl.BinnedData
import com.quantifind.charts.repl.IterablePair

import org.apache.zeppelin.interpreter.InterpreterContext

/**
 * Wisp plotting embedded directly in Apache Zeppelin paragraphs.
 */
object ZeppelinFigures {
  // In Zeppelin there appears to be a different InterpreterContext object for
  // each paragraph in a notebook.
  private var ic: InterpreterContext = _
  private var figure: Highcharts = _

  // See http://mail-archives.apache.org/mod_mbox/incubator-zeppelin-users/201510.mbox/%3CCALf24sZBqvyspE1EGuXDa29HGi2Udk=OcCvm7YmBVCMN=ad7sA@mail.gmail.com%3E
  def newFigure(ic: InterpreterContext) = {
    if (ic == null) {
      throw new NullPointerException("Null InterpreterContext!")
    }
    if (this.ic != null) {
      figure.deleteAll()
    }
    this.ic = ic
    figure = new Highcharts()
  }

  private def checkInitialised: Unit = {
    if (ic == null) {
      throw new IllegalStateException("Call this before you issue any commands:\nnewFigure(z.getInterpreterContext())")
    }
  }

  private def getFigure(): Highcharts = {
    checkInitialised
    figure
  }

  //private def isInZeppelin = sys.env.get("ZEPPELIN_HOME").isDefined

  def show(): Unit = {
    checkInitialised
    print("%html " + figure.getHtml)
  }

  // The following methods mirror (and call) the corresponding ones in the
  // Highcharts class...

  def area[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.area)
    fig.plot(fig.addStyle(hc, xy))
  }

  def areaspline[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.areaspline)
    fig.plot(fig.addStyle(hc, xy))
  }

  def bar[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.bar)
    fig.plot(fig.addStyle(hc, xy))
  }

  def boxplot[T: Numeric](data: Iterable[BoxplotData[T]]) = {
    val fig = getFigure()
    fig.plot(Highchart(series = Seq(Series(data = data, chart = Some(SeriesType.boxplot)))))
  }

  def column[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.column)
    fig.plot(fig.addStyle(hc, xy))
  }

  def histogram[A: Numeric](data: Iterable[A], numBins: Int) = {
    val fig = getFigure()
    val binCounts = fig.binIterableNumBins(data, numBins).toBinned().toSeq
    fig.plot(Histogram.histogram(binCounts))
  }

  def histogram(data: BinnedData) = {
    val fig = getFigure()
    val binCounts = data.toBinned().toSeq
    fig.plot(Histogram.histogram(binCounts))
  }

  def line[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.line)
    fig.plot(fig.addStyle(hc, xy))
  }

  def pie[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.pie)
    fig.plot(fig.addStyle(hc, xy))
  }

  def regression[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    def numericToDouble[X](x: X)(implicit ev: Numeric[X]): Double = ev.toDouble(x)
    val (xr, yr) = xy.toIterables
    LeastSquareRegression.leastSquareRegression(fig, xr.toSeq.map(numericToDouble(_)), yr.toSeq.map(numericToDouble(_)))
  }

  def scatter[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.scatter)
    fig.plot(fig.addStyle(hc, xy))
  }

  def spline[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val fig = getFigure()
    val (xr, yr) = xy.toIterables
    val hc = fig.xyToSeries(xr, yr, SeriesType.spline)
    fig.plot(fig.addStyle(hc, xy))
  }

  def help(): Unit = {
    // TODO Add help specific to ZeppelinFigures once we're happy with its methods and usage.
    val fig = getFigure()
    fig.help()
  }
}

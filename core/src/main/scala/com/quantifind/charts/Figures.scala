package com.quantifind.charts

/**
 * Wrapper object to allow client to get and manipulate multiple instances of
 * Highcharts. The client is responsible for managing the objects.
 */
object Figures {

  def newFigure() = {
    new Highcharts()
  }

  def help(): Unit = {
    // TODO Add help specific to Figures once we're happy with its methods and usage.
    newFigure().help()
  }
}

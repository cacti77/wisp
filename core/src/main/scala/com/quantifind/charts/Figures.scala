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
    println("\nFigure commands:\n")
    Map(
      "newFigure()" -> "Creates and returns a new charting object"
    )
      .map{case(plot, description) =>"\t%-35s%s".format(plot, description)}
      .foreach(println)

    println("\nThe following commands are available for each charting object...")
    newFigure().help()
  }
}

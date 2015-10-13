package com.qf.charts.repl

import com.quantifind.charts.Highcharts
import org.scalatest.Matchers
import org.scalatest.FunSuite

/**
 * User: austin
 * Date: 12/15/14
 */
class HighchartsReplTest extends FunSuite with Matchers  {

  test("Pie repl") {
    val highcharts = new Highcharts()
    highcharts.disableOpenWindow // prevents server from starting
    highcharts.pie(1 to 4).toJson should be(
        """{"series":[""" +
        """{"data":[{"x":0,"y":1},{"x":1,"y":2},{"x":2,"y":3},{"x":3,"y":4}],"type":"pie"}],""" +
        """"exporting":{"filename":"chart"},""" +
        """"yAxis":[{"title":{"text":""}}],""" +
        """"plotOptions":{},""" +
        """"credits":{"href":"","text":""},""" +
        """"chart":{"zoomType":"xy"},""" +
        """"title":{"text":""},""" +
        """"xAxis":[{"title":{"text":""}}]}"""
    )
  }

}

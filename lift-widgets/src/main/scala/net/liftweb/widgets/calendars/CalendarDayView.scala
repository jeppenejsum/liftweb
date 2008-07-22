/*
* Copyright 2007-2008 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions
* and limitations under the License.
*/

package net.liftweb.widgets.calendars;

import scala.xml._
import java.util.{Calendar, Locale}
import java.util.Calendar._
import java.text.SimpleDateFormat
import net.liftweb.util.Helpers._
import net.liftweb.util.{Can, Full, Empty}
import net.liftweb.http.js._
import net.liftweb.http.SHtml._
import JsCmds._
import JE._

object CalendarDayView {

  /**
   * Call this function typically in boot
   */
  def init() {
    import net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "calendars" :: _ => true
      case "common" :: _ => true
    })
  }

  def apply(when: Calendar,
      calendars: List[CalendarItem],
      itemClick: Can[AnonFunc]) = new CalendarDayView(when).render(calendars, itemClick)

  def apply(when: Calendar,
      meta: DayViewMeta,
      calendars: List[CalendarItem],
      itemClick: Can[AnonFunc]) = new CalendarDayView(when, meta) render(calendars, itemClick)

}

class CalendarDayView(val when: Calendar, val meta: DayViewMeta) {

  def this(when: Calendar) = this(when, DayViewMeta(Locale getDefault))

  def makeHead(headCal: Calendar) = <tr><td></td>{
    (0 to 6) map(x => <td width="14%">{
      try{
        meta.weekDaysFormatter format(headCal getTime)
      } finally {
        headCal add(DAY_OF_MONTH, 1)
      }
    }</td>)
  }</tr>


  def render(calendars: List[CalendarItem], itemClick: Can[AnonFunc]): NodeSeq = {

    val cal = when.clone().asInstanceOf[Calendar]

    <head>
      <link rel="stylesheet" href="/classpath/calendars/dayview/style.css" type="text/css"/>
      <script type="text/javascript" src="/classpath/common/jquery.dimensions.js"></script>
      <script type="text/javascript" src="/classpath/calendars/dayview/dayviewcalendars.js"></script>
      <script type="text/javascript" src="/classpath/common/jquery.bgiframe.js"></script>
      <script type="text/javascript" src="/classpath/common/jquery.tooltip.js"></script>
      <script type="text/javascript" charset="utf-8">{
        Unparsed("\nvar itemClick = " + (itemClick openOr JsRaw("function(param){}")).toJsCmd) ++
        Unparsed("\nvar calendars = " + CalendarUtils.toJSON(calendars filter (c => CalendarUtils.sameDay(c.start, when))).toJsCmd) ++
        Unparsed("""
         jQuery(document).ready(function() {
            buildDayViewCalendars();
          })
         """)
       }
      </script>
    </head>

    <div class="dayView">
    <div class="dayHead">
      <table cellspacing="0" cellpading="0" style="width: 100%;">
       <tr>
         <td  class="dayHour"><div></div></td>
            <td class="dayHeadCell">{
                val time = cal.getTime
                meta.weekDaysFormatter.format(time) + " " + dateFormatter.format(time)
            }</td>
        </tr>
      </table>
    </div>
    <div class="dayViewBody">
      <table cellspacing="0" cellpading="0" style="width: 100%;">
      {
        val cal = Calendar getInstance;
        cal set(HOUR_OF_DAY, 0)
        cal set(MINUTE, 0)
        for (val i <- 0 to 23) yield
        try{
          <tr>
            <td class="dayHour"><div>{Unparsed(meta.timeFormatter format(cal getTime))}</div></td>
            {
              <td id={Unparsed("didx_" + (i*2 toString))} class="dayCell borderDashed"></td>
            }
          </tr>
          <tr>
            <td class="dayHour borderSolid"></td>
            {
              <td id={Unparsed("didx_" + ((i*2+1) toString))} class="dayCell borderSolid"></td>
            }
          </tr>
        } finally {
          cal add(HOUR_OF_DAY, 1)
        }
      }</table>
    </div>
    </div>
  }
}


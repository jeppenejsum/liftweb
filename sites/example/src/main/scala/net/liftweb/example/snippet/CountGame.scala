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
package net.liftweb.example.snippet

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group, Node}
import net.liftweb.http._
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util._


class CountGame extends StatefulSnippet {
  val dispatch: DispatchIt = {
    case "run" if lastGuess == number =>
    xhtml => win(chooseTemplate("choose", "win", xhtml))

    case "run" =>
    xhtml => nextGuess(chooseTemplate("choose", "guess", xhtml))

    case "count_down" =>
    xhtml => countDown(attr("from").map(Helpers.toInt).openOr(0))
  }
<<<<<<< HEAD:sites/example/src/main/scala/net/liftweb/example/snippet/CountGame.scala
  
  def win(xhtml: NodeSeq) = bind("count", xhtml, "number" -> number,
  "count" -> count) ++ <p>Counting backward: {countDown(number)}</p>
  
=======

  def win(xhtml: NodeSeq) = bind("count", xhtml, "number" --> number,
  "count" --> count) ++ <p>Counting backward: {countDown(number)}</p>

>>>>>>> master:sites/example/src/main/scala/net/liftweb/example/snippet/CountGame.scala
  def countDown(number: Int): Node = if (number <= 0) Text("")
  else <xml:group>{number} <lift:count_game.count_down from={(number - 1).toString} /></xml:group>

  def nextGuess(xhtml: NodeSeq) =  bind("count", xhtml,
  "input" -> text("", guess _),
  "last" ->
  lastGuess.map(v =>
  if (v < number) v+" is low"
  else v+" is high").
  openOr("Make first Guess"))

  private def guess(in: String) {
    count += 1
    lastGuess = Full(toInt(in))
  }

  private val number = 1 + randomInt(100)
  private var lastGuess: Can[Int] = Empty
  private var count = 0
}

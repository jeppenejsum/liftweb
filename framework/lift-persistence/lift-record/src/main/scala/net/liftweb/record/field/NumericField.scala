/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package record {
package field {

import net.liftweb.http.{S}
import net.liftweb.http.js._
import net.liftweb.util._
import net.liftweb.common._
import scala.xml._
import S._
import Helpers._
import JE._

trait NumericFieldMixin {
  self: Field[_, _] =>

  protected def elem: Elem

  /**
   * Returns form input of this field
   */
  def toForm = {
    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{elem % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{elem}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }
}

trait NumericField[MyType, OwnerType <: Record[OwnerType]] extends Field[MyType, OwnerType] with NumericFieldMixin {

  protected def elem = S.fmapFunc{s: List[String] => {
      this.setFromAny(s) match {
        case Empty => valueCouldNotBeSet = true
        case _ =>
      }}}{funcName => <input type="text" name={funcName} value={value.toString}
      tabindex={tabIndex toString}/>}

  def asJs = JsRaw(String.valueOf(value))

  override def noValueErrorMessage = S.??("number.required")
}


trait OptionalNumericField[MyType, OwnerType <: Record[OwnerType]] extends Field[Option[MyType], OwnerType] with NumericFieldMixin {
  protected def elem = {
    def handleInput(s: List[String]): Unit = s match {
      case s::_ if s.trim == "" => set(None)
      case other => setFromAny(other) match {
        case Empty => valueCouldNotBeSet = true
        case _ => ()
      }
    }

    S.fmapFunc(handleInput _) {
      funcName => <input type="text" name={funcName} value={value.toString} tabindex={tabIndex toString}/>
    }
  }

  def asJs = value.map(v => JsRaw(String.valueOf(v))) getOrElse JsNull

  def defaultValue = None

  override def noValueErrorMessage = S.??("number.required")
}

}
}
}

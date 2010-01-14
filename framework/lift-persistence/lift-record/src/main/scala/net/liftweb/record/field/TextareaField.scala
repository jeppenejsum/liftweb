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

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.{S}
import S._
import Helpers._

trait TextareaFieldMixin {
  self: Field[_, _] =>

  def valueOption: Option[String]

  protected def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <textarea name={funcName}
      rows={textareaRows.toString}
      cols={textareaCols.toString}
      tabindex={tabIndex toString}>{valueOption match {case Some(null)|None => "" case Some(s) => s.toString}}</textarea>
  }

  protected def _toForm = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{el % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{el}</div>
    }

  }

  protected def _asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }

  override def toString = valueOption match {
    case Some(s) if s == null || s.length < 100 => super.toString
    case None => super.toString
    case Some(longValue) => longValue.substring(0,40) + " ... " + longValue.substring(longValue.length - 40)
  }

  def textareaRows  = 8

  def textareaCols = 20
}

class TextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends StringField(rec, maxLength) with TextareaFieldMixin {
  def valueOption = Some(value)
  override def asXHtml = _asXHtml
  override def toForm = _toForm
}


class OptionalTextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int) extends OptionalStringField(rec, maxLength) with TextareaFieldMixin {
  def valueOption = value
  override def asXHtml = _asXHtml
  override def toForm = _toForm
}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * A string field holding DB related logic
 */
abstract class DBTextareaField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType, maxLength: Int) extends
TextareaField[OwnerType](rec, maxLength) with JDBCFieldFlavor[String]{

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+maxLength+")"

  def jdbcFriendly(field : String) : String = value
}

}
}
}

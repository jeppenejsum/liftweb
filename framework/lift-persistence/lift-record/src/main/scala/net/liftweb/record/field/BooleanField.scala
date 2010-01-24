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

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S, SHtml}
import _root_.net.liftweb.http.js._
import Helpers._
import S._
import JE._

trait BooleanFieldMixin {
  self: Field[_, _] =>

  protected def valueOption: Option[Boolean]

  // FIXME? Right now the control is only emitted as a plain checkbox and so can never represent None (null)
  private def elem(id: Option[String]): NodeSeq = {
    val attrs = ("tabIndex" -> tabIndex.toString) :: (id.map(s => ("id" -> s)::Nil) getOrElse Nil)
    SHtml.checkbox(valueOption getOrElse false, (b: Boolean) => this.setFromAny(Some(b)), attrs: _*)
  }

  def toForm = {
    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label
              for={id+"_field"}>{displayName}</label></div>{ elem(Some(id+"_field")) }<lift:msg id={id}/></div>
      case _ => <div>{ elem(None) }</div>
    }

  }

  def asXHtml: NodeSeq = elem(uniqueFieldId.map(_ + "_field"))
}

class BooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[Boolean, OwnerType] with BooleanFieldMixin {
  def owner = rec

  def this(rec: OwnerType, value: Boolean) = {
    this(rec)
    set(value)
  }

  /**
   * Sets the field value from an Any
   */
  def setFromAny(in: Any): Box[Boolean] = {
    in match {
      case b: Boolean => Full(this.set(b))
      case (b: Boolean) :: _ => Full(this.set(b))
      case Some(b: Boolean) => Full(this.set(b))
      case Full(b: Boolean) => Full(this.set(b))
      case Empty | Failure(_, _, _) | None => Full(this.set(false))
      case (s: String) :: _ => Full(this.set(toBoolean(s)))
      case null => Full(this.set(false))
      case s: String => Full(this.set(toBoolean(s)))
      case o => Full(this.set(toBoolean(o)))
    }
  }

  def setFromString(s: String): Box[Boolean] = {
    try{
      Full(set(java.lang.Boolean.parseBoolean(s)));
    } catch {
      case e: Exception => Empty
    }
  }

  protected def valueOption = Some(value)

  def defaultValue = false

  def asJs: JsExp = value

}


class OptionalBooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[Option[Boolean], OwnerType] with BooleanFieldMixin {
  def owner = rec

  def this(rec: OwnerType, value: Option[Boolean]) = {
    this(rec)
    set(value)
  }

  /**
   * Sets the field value from an Any
   */
  def setFromAny(in: Any): Box[Option[Boolean]] = {
    in match {
      case (b: Boolean) => Full(this.set(Some(b)))
      case (b: Boolean) :: _ => Full(this.set(Some(b)))
      case Some(b: Boolean) => Full(this.set(Some(b)))
      case Full(b: Boolean) => Full(this.set(Some(b)))
      case null | Empty | Failure(_, _, _) | None => Full(this.set(None))
      case (s: String) :: _ => Full(this.set(Some(toBoolean(s))))
      case o => Full(this.set(Some(toBoolean(o))))
    }
  }

  def setFromString(s: String): Box[Option[Boolean]] = {
    try{
      Full(set(Some(java.lang.Boolean.parseBoolean(s))))
    } catch {
      case e: Exception => Empty
    }
  }

  protected def valueOption = value

  def defaultValue = None

  def asJs: JsExp = value match {
    case None         => JsNull
    case Some(b) if b => JsTrue
    case _            => JsFalse
  }
}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
class DBBooleanField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends BooleanField[OwnerType](rec) with JDBCFieldFlavor[Boolean] {

  def targetSQLType = _root_.java.sql.Types.BOOLEAN

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Boolean = value

}

}
}
}

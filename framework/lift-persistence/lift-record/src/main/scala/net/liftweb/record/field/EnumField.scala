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
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js._
import S._
import Helpers._
import JE._

private[field] trait EnumFieldMixin[EnumType <: Enumeration] {
  self: Field[_, _] =>

  protected def enum: EnumType
  protected def toInt: Int
  protected def setFromAny(a: Any): Box[_]

  /**
   * Build a list for the select.  Return a tuple of (String, String) where the first string
   * is the id.string of the Value and the second string is the Text name of the Value.
   */
  def buildDisplayList: List[(Int, String)] = enum.elements.toList.map(a => (a.id, a.toString))

  protected def elem = SHtml.selectObj[Int](buildDisplayList, Some(toInt), this.setFromAny(_)) % ("tabindex" -> tabIndex.toString)

  def toForm = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{el % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{el}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) => el % ("id" -> (id+"_field"))
      case _ => el
    }
  }
}


class EnumField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType) extends Field[EnumType#Value, OwnerType] with EnumFieldMixin[EnumType] {

  def owner = rec

  def toInt = value.id

  def fromInt(in: Int): EnumType#Value = enum(in)

  override protected def set_!(value: EnumType#Value): EnumType#Value = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def setFromAny(in: Any): Box[EnumType#Value] = {
    in match {
      case n: Int => Full(set(fromInt(n)))
      case n: Long => Full(set(fromInt(n.toInt)))
      case n: Number => Full(set(fromInt(n.intValue)))
      case (n: Number) :: _ => Full(set(fromInt(n.intValue)))
      case Some(n: Number) => Full(set(fromInt(n.intValue)))
      case Full(n: Number) => Full(set(fromInt(n.intValue)))
      case None | Empty | Failure(_, _, _) => Full(set(defaultValue))
      case (s: String) :: _ => Full(set(fromInt(Helpers.toInt(s))))
      case vs: EnumType#Value => Full(set(vs))
      case null => Full(set(defaultValue))
      case s: String => Full(set(fromInt(Helpers.toInt(s))))
      case o => Full(set(fromInt(Helpers.toInt(o))))
    }
  }

  def setFromString(s: String): Box[EnumType#Value] = setFromAny(s)

  def defaultValue: EnumType#Value = enum.elements.next

  def asJs = Str(toString)

}


class OptionalEnumField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType) extends Field[Option[EnumType#Value], OwnerType] with EnumFieldMixin[EnumType] {

  // FIXME? -1 is a bit of a hack

  def owner = rec

  def toInt = value.map(_.id) getOrElse -1

  def fromInt(in: Int): EnumType#Value = enum(in)

  override def buildDisplayList: List[(Int, String)] = (-1, S.??("no.selection")) :: super.buildDisplayList

  override protected def set_!(value: Option[EnumType#Value]): Option[EnumType#Value] = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  def setFromAny(in: Any): Box[Option[EnumType#Value]] = {
    in match {
      case (n: Number) if n.intValue == -1 => Full(set(None))
      case (n: Int) => Full(set(Some(fromInt(n))))
      case (n: Long) => Full(set(Some(fromInt(n.toInt))))
      case (n: Number) => Full(set(Some(fromInt(n.intValue))))
      case (n: Number) :: _ => Full(set(Some(fromInt(n.intValue))))
      case Some(n: Number) => Full(set(Some(fromInt(n.intValue))))
      case Some(v: EnumType#Value) => Full(set(Some(v)))
      case Full(n: Number) => Full(set(Some(fromInt(n.intValue))))
      case Full(v: EnumType#Value) => Full(set(Some(v)))
      case null | None | Empty | Failure(_, _, _) => Full(set(None))
      case (s: String) :: _ => Full(set(Some(fromInt(Helpers.toInt(s)))))
      case (vs: EnumType#Value) => Full(set(Some(vs)))
      case (s: String) => Full(set(Some(fromInt(Helpers.toInt(s)))))
      case o => Full(set(Some(fromInt(Helpers.toInt(o)))))
    }
  }

  def setFromString(s: String): Box[Option[EnumType#Value]] = setFromAny(s)

  def defaultValue: Option[EnumType#Value] = None

  def asJs = Str(toString)

}


import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An enum field holding DB related logic
 */
abstract class DBEnumField[OwnerType <: DBRecord[OwnerType], EnumType <: Enumeration](rec: OwnerType, enum: EnumType) extends
  EnumField(rec, enum) with JDBCFieldFlavor[Integer] {

  def targetSQLType = Types.VARCHAR

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field: String) = new _root_.java.lang.Integer(toInt)

}

}
}
}

/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package couchdb {

import java.net.ConnectException
import dispatch.{Http, StatusCode}
import net.liftweb.common.{Failure, Full}
import net.liftweb.json.Implicits.{int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JField, JInt, JObject, JString, render}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import net.liftweb.json.Printer.compact
import org.specs._
import org.specs.runner.JUnit4
import DocumentHelpers.{jobjectToJObjectExtension, stripIdAndRev}

class CouchRecordTestSpecsAsTest extends JUnit4(CouchRecordTestSpecs)

class Person extends CouchRecord[Person] {
    def meta = Person

    object name extends JSONStringField(this, 200)
    object age extends JSONIntField(this)
}

object Person extends Person with CouchMetaRecord[Person] { }

class Company extends CouchRecord[Company] {
    def meta = Company

    object name extends JSONStringField(this, 200)
}        

object Company extends Company with CouchMetaRecord[Company] { }

object CouchRecordTestSpecs extends Specification {
    import CouchDB.defaultDatabase

    val design: JObject = 
        ("language" -> "javascript") ~
        ("views" -> ("people_by_age" -> ("map" -> "function(doc) { if (doc.type == 'Person') { emit(doc.age, doc); } }")))

    def setup = {
        val database = new Database("test")
        (try { Http(database delete) } catch { case StatusCode(_, _) => () }) must not(throwAnException[ConnectException]).orSkipExample
        Http(database create)
        Http(database.design("test") put design)
        defaultDatabase = database
    }

    def assertEqualPerson(a: Person, b: Person) = {
        a.name.value must_== b.name.value
        a.age.value must_== b.age.value
    }

    def assertEqualRows(foundRows: Seq[Person], expectedRows: Seq[Person]) = {
        foundRows.length must_== expectedRows.length
        for ((found, expected) <- foundRows.toList zip expectedRows.toList) {
            found.id.value must_== expected.id.value
            found.rev.value must_== expected.rev.value
            assertEqualPerson(found, expected)
        }
    }
        
    "A couch record" should {
        def testRec1: Person = Person.createRecord.name("Alice").age(25)
        val testDoc1: JObject = ("age" -> 25) ~ ("name" -> "Alice") ~ ("type" -> "Person")
        def testRec2: Person = Person.createRecord.name("Bob").age(30)
        val testDoc2: JObject = ("age" -> 30) ~ ("extra1" -> "value1") ~ ("extra2" -> "value2") ~ ("name" -> "Bob") ~ ("type" -> "Person")
        def testRec3: Company = Company.createRecord.name("Acme")

        "give emtpy box on get when nonexistant" in {
            setup must_== ()

            Person.fetch("testdoc") must verify (!_.isDefined)
        }

        "be insertable" in {
            setup
            
            val newRec = testRec1
            newRec save

            assertEqualPerson(newRec, testRec1)
            newRec.saved_? must_== true
            newRec.id.value must verify (_.isDefined)
            newRec.rev.value must verify (_.isDefined)

            val Full(foundRec) = Person.fetch(newRec.id.value.get)
            assertEqualPerson(foundRec, testRec1)
            foundRec.id.value must_== newRec.id.value
            foundRec.rev.value must_== newRec.rev.value
            
        }

        "generate the right JSON" in {
            setup
            val newRec = testRec1
            newRec save

            val foundDoc = Http(defaultDatabase(newRec.id.value.get) fetch)
            compact(render(stripIdAndRev(foundDoc))) must_== compact(render(testDoc1))
        }

        "preserve extra fields from JSON" in {
            setup

            val docBox = Http(defaultDatabase store testDoc2)
            docBox must verify(_.isDefined)
            val Full(doc) = docBox

            val foundRecBox = Person.fetch(doc._id.open_!)
            foundRecBox must verify(_.isDefined)
            val Full(foundRec) = foundRecBox
            
            foundRec.additionalJFields must_== List(JField("extra1", JString("value1")), 
                                                    JField("extra2", JString("value2")),
                                                    JField("type", JString("Person")))

            foundRec.age.set(1)
            foundRec.save

            val foundDoc = Http(defaultDatabase(doc) fetch)
            compact(render(stripIdAndRev(foundDoc))) must_== compact(render(("age" -> 1) ~ testDoc2.remove("age")))
        }

        "be deletable" in {
            setup
            val newRec = testRec1
            newRec.save

            newRec.id.value must verify(_.isDefined)
            val id = newRec.id.value.get

            Person.fetch(id) must verify(_.isDefined)
            newRec.delete_! must verify(_.isDefined)
            Person.fetch(id) must not(verify(_.isDefined))
            newRec.delete_! must not(verify(_.isDefined))

            newRec.save
            Http(defaultDatabase(newRec.id.value.get) @@ newRec.rev.value.get delete)
            newRec.delete_! must not(verify(_.isDefined))
        }

        "be fetchable in bulk" in {
            setup
            val newRec1, newRec2, newRec3 = testRec1

            newRec1.save
            newRec2.save
            newRec3.save

            newRec1.saved_? must_== true
            newRec2.saved_? must_== true
            newRec3.saved_? must_== true

            val expectedRows = newRec1::newRec3::Nil

            Person.fetchMany(newRec1.id.value.get, newRec3.id.value.get).map(_.toList) must beLike {
                case Full(foundRows) => assertEqualRows(foundRows, expectedRows); true
            }
        }

        "support queries" in {
            setup

            val newRec1, newRec3 = testRec1
            val newRec2 = testRec2
            newRec1.save
            newRec2.save
            newRec3.save

            newRec1.saved_? must_== true
            newRec2.saved_? must_== true
            newRec3.saved_? must_== true

            val expectedRows = newRec2::Nil

            Person.queryView("test", "people_by_age", _.key(JInt(30))) must beLike {
                case Full(foundRows) => assertEqualRows(foundRows, expectedRows); true
            }
        }

        "support multiple databases for fetching" in {
            setup
            val database2 = new Database("test2")
            (try { Http(database2 delete) } catch { case StatusCode(_, _) => () }) must not(throwAnException[ConnectException]).orSkipExample
            Http(database2 create)

            val newRec = testRec1
            newRec.database = database2
            newRec.save

            newRec.saved_? must_== true

            val foundRecBox = Person.fetchFrom(database2, newRec.id.value.get)
            foundRecBox must verify(_.isDefined)
            val Full(foundRec) = foundRecBox
            assertEqualPerson(foundRec, testRec1)
            foundRec.id.value must_== newRec.id.value
            foundRec.rev.value must_== newRec.rev.value
            
            Person.fetch(newRec.id.value.get) must not(verify(_.isDefined))
        }

        "support multiple databases for fetching in bulk" in {
            setup
            val database2 = new Database("test2")
            (try { Http(database2 delete) } catch { case StatusCode(_, _) => () }) must not(throwAnException[ConnectException]).orSkipExample
            Http(database2 create)

            val newRec1, newRec2, newRec3 = testRec1
            newRec1.database = database2
            newRec2.database = database2
            newRec3.database = database2
            newRec1.save
            newRec2.save
            newRec3.save

            newRec1.saved_? must_== true
            newRec2.saved_? must_== true
            newRec3.saved_? must_== true

            val expectedRows = newRec1::newRec3::Nil

            Person.fetchManyFrom(database2, newRec1.id.value.get, newRec3.id.value.get).map(_.toList) must beLike {
                case Full(foundRows) => assertEqualRows(foundRows, expectedRows); true
            }

            Person.fetchMany(newRec1.id.value.get, newRec3.id.value.get) must beLike { case Full(seq) if seq.isEmpty => true }
        }

        "support multiple databses for queries" in {
            setup
            val database2 = new Database("test2")
            (try { Http(database2 delete) } catch { case StatusCode(_, _) => () }) must not(throwAnException[ConnectException]).orSkipExample
            Http(database2 create)
            Http(database2.design("test") put design)

            val newRec1, newRec3 = testRec1
            val newRec2 = testRec2
            newRec1.database = database2
            newRec2.database = database2
            newRec3.database = database2
            newRec1.save
            newRec2.save
            newRec3.save

            newRec1.saved_? must_== true
            newRec2.saved_? must_== true
            newRec3.saved_? must_== true

            val expectedRows = newRec2::Nil

            Person.queryViewFrom(database2, "test", "people_by_age", _.key(JInt(30))) must beLike {
                case Full(foundRows) => assertEqualRows(foundRows, expectedRows); true
            }

            Person.queryView("test", "people_by_age", _.key(JInt(30))) must beLike { case Full(seq) if seq.isEmpty => true }
        }
    }
}

}
}

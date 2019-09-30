package com.example

import play.api.libs.json.Json


sealed trait Creature
case class Person(name: String, age: Int) extends Creature
case class Cat(@renamed("кличка") name: String) extends Creature

object Implicits {
  implicit val creatureReads: JsonReads[Creature] = JsonReads.generate[Creature]
  implicit val creatureWrites: JsonWrites[Creature] = JsonWrites.generate[Creature]
}

object Main {

  import Implicits._

  def main(args: Array[String]): Unit = {
    val reads = JsonReads[Creature]
    val writes = JsonWrites[Creature]

    val personJson = writes write Person("John", 30)
    val catJson = writes write Cat("Begemot")

    println(personJson)
    println(catJson)

    println(reads read personJson)
    println(reads read catJson)


    println(reads read Json.parse("""{"type":"com.example.Dog", "value" : { "кличка":"Begemot" } }"""))
  }
}

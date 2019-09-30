package com.example

import scala.language.experimental.macros
import magnolia.{CaseClass, Magnolia, SealedTrait}
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString, JsValue}

trait JsonWrites[A] {
  def write(a: A): JsValue
}

trait PrimitiveWrites {
  implicit val writeInt: JsonWrites[Int] = JsonWrites.create((num: Int) => JsNumber(num))

  implicit val writeLong: JsonWrites[Long] = JsonWrites.create((num: Long) => JsNumber(num))

  implicit val writeString: JsonWrites[String] = JsonWrites.create((str: String) => JsString(str))

  implicit val writeBoolean: JsonWrites[Boolean] = JsonWrites.create((bool: Boolean) => JsBoolean(bool))
}

object JsonWrites extends PrimitiveWrites {
  def apply[A: JsonWrites]: JsonWrites[A] = implicitly[JsonWrites[A]]

  def create[A](write: A => JsValue): JsonWrites[A] = (a: A) => write(a)

  type Typeclass[A] = JsonWrites[A]

  def combine[A](caseClass: CaseClass[JsonWrites, A]): JsonWrites[A] = create[A] { instance =>
    val namesMap = caseClass.parameters.map {param =>
      val renamedOpt = param.annotations.collectFirst {
        case renamed: renamed => renamed.name
      }
      param.label -> renamedOpt.getOrElse(param.label)
    }.toMap

    val fieldsMap = caseClass.parameters.foldLeft(Map.empty[String, JsValue]) {
      case (fields, param) =>
        fields + (namesMap(param.label) -> param.typeclass.write(param.dereference(instance)))
    }

    JsObject(fieldsMap)
  }

  def dispatch[A](sealedTrait: SealedTrait[JsonWrites, A]): JsonWrites[A] = create[A] { instance =>
    sealedTrait.dispatch(instance) { subType =>
      JsObject(
        Map(
          "type" -> JsString(subType.typeName.full),
          "value" -> subType.typeclass.write(subType.cast(instance))
        )
      )
    }
  }

  def generate[A]: JsonWrites[A] = macro Magnolia.gen[A]
}

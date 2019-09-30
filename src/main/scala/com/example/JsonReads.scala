package com.example

import scala.language.experimental.macros
import magnolia.{CaseClass, Magnolia, SealedTrait}
import play.api.libs.json.{JsBoolean, JsNumber, JsObject, JsString, JsValue}

trait JsonReads[A] {
  def read(json: JsValue): ErrorOr[A]
}

trait JsonReadsPrimitives {
  implicit val readInt: JsonReads[Int] = JsonReads.create[Int] {
    case JsNumber(num) if num.isValidInt => ErrorOr.success(num.toInt)
    case JsNumber(num)                   => ErrorOr.singleError(s"Expected Int, got number: $num")
    case other                           => ErrorOr.singleError(s"Expected Int, got: $other")
  }

  implicit val readLong: JsonReads[Long] = JsonReads.create[Long] {
    case JsNumber(num) if num.isValidLong => ErrorOr.success(num.toLong)
    case JsNumber(num)                    => ErrorOr.singleError(s"Expected Long, got number: $num")
    case other                            => ErrorOr.singleError(s"Expected Long, got: $other")
  }

  implicit val readBoolean: JsonReads[Boolean] = JsonReads.create[Boolean] {
    case JsBoolean(bool) => ErrorOr.success(bool)
    case other           => ErrorOr.singleError(s"Expected Boolean, got: $other")
  }

  implicit val readString: JsonReads[String] = JsonReads.create[String] {
    case JsString(str) => ErrorOr.success(str)
    case other         => ErrorOr.singleError(s"Expected String, got: $other")
  }
}

object JsonReads extends JsonReadsPrimitives {
  def apply[A: JsonReads]: JsonReads[A] = implicitly[JsonReads[A]]

  def create[A](read: JsValue => ErrorOr[A]): JsonReads[A] = (json: JsValue) => read(json)

  type Typeclass[A] = JsonReads[A]

  def combine[A](caseClass: CaseClass[JsonReads, A]): JsonReads[A] =
    create[A] {
      case JsObject(json) =>
        val namesMap = caseClass.parameters.map {param =>
          val renamedOpt = param.annotations.collectFirst {
            case renamed: renamed => renamed.name
          }
          param.label -> renamedOpt.getOrElse(param.label)
        }.toMap

        def check: List[String] = {
          caseClass.parameters.collect {
            case param if !json.contains(namesMap(param.label)) =>
              s"field ${namesMap(param.label)} not found in $json"
          }.toList
        }

        check match {
          case Nil =>
            caseClass.constructMonadic[ErrorOr, Any](param =>
              param.typeclass.read(json(namesMap(param.label)))
            )

          case errors => ErrorOr.errors(errors)
        }

      case other => ErrorOr.singleError(s"Expected JsObject, got: $other")
    }

  def dispatch[A](sealedTrait: SealedTrait[JsonReads, A]): JsonReads[A] = create[A] {
    case JsObject(jsonMap) if jsonMap.contains("type") && jsonMap.contains("value") =>
      val typeName = jsonMap("type") match {
        case JsString(value) => value
        case other           => ErrorOr.singleError(s"Expected JsString in type field, got: $other")
      }

      sealedTrait.subtypes.find(_.typeName.full == typeName) match {
        case None          => ErrorOr.singleError(s"${sealedTrait.typeName.full} does not contain a member of type $typeName")
        case Some(subType) => subType.typeclass.read(jsonMap("value"))
      }

    case other => ErrorOr.singleError(s"Expected JsObject with `type` and `value` fields, got: $other")
  }

  def generate[A]: JsonReads[A] = macro Magnolia.gen[A]
}
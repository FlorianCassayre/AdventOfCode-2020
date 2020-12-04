package adventofcode.solutions

import adventofcode.Day

import scala.util.matching.Regex

object Day04 extends Day(4):

  enum Field[T](val name: String,
                regex: Regex = ".*".r,
                map: String => T = identity,
                validation: T => Boolean = (_: T) => true):
    case BirthYear extends Field("byr", "[0-9]{4}".r, _.toInt, (1920 to 2002).contains)
    case IssueYear extends Field("iyr", "[0-9]{4}".r, _.toInt, (2010 to 2020).contains)
    case ExpirationYear extends Field("eyr", "[0-9]{4}".r, _.toInt, (2020 to 2030).contains)
    case Height extends Field("hgt", "[0-9]{2,3}(cm|in)".r,
      s => (s.substring(0, s.length - 2).toInt, s.endsWith("cm")),
      (height, isCm) => (if isCm then (150 to 193) else (59 to 76)).contains(height))
    case HairColor extends Field("hcl", "#[0-9a-f]{6}".r)
    case EyeColor extends Field("ecl", "amb|blu|brn|gry|grn|hzl|oth".r)
    case PassportID extends Field("pid", "[0-9]{9}".r)
    case CountryID extends Field("cid")

    def validate(value: String): Boolean = value match
      case regex(_*) => validation(map(value))
      case _ => false

  object Field:
    private val map = Field.values.map(v => v.name -> v).toMap
    val requiredFields = (values.toSet - CountryID).map(_.name)
    
    def validateSchema(passport: Seq[(String, String)]): Boolean =
      (passport.map(_._1).toSet intersect requiredFields) == requiredFields
    
    def validateValues(passport: Seq[(String, String)]): Boolean = validateSchema(passport) && passport.forall {
      (field, value) => map(field).validate(value)
    }

  val passports = input.split(lineSeparator + lineSeparator).map(_.split(lineSeparator).flatMap(_.split(" ")).map(
    _.split(":") match
      case Array(field, value) => (field, value)
  ).toSeq).toSeq
  
  override def solutionA = passports.count(Field.validateSchema)

  override def solutionB = passports.count(Field.validateValues)

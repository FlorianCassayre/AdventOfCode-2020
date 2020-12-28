package adventofcode.solutions

import adventofcode.Day

object Day21 extends Day(21):

  val regex = "(.*) \\(contains (.*)\\)".r

  val foods = lines.map { case regex(ingredients, allergens) => (ingredients.split(" ").toSet, allergens.split(", ").toSet) }

  val (allIngredients, allAllergens) =
    val (l, r) = foods.unzip
    (l.flatten.toSet, r.flatten.toSet)

  val possible = allAllergens.flatMap(allergen => foods.collect { case (food, allergens) if allergens.contains(allergen) => food }.reduce[Set[String]](_ & _))
  val inert = allIngredients -- possible

  override def solutionA = foods.map((ingredients, _) => ingredients.count(inert.contains)).sum

  val constraints = foods.map((ingredients, allergens) => (ingredients.filter(e => possible.contains(e)), allergens))

  val Some(matching) = possible.toSeq.permutations.map(_.zip(allAllergens.toSeq).toMap).find(map => constraints.forall((ingredients, allergens) => allergens.subsetOf(ingredients.map(map))))

  override def solutionB = matching.toSeq.sortBy((_, allergen) => allergen).map((ingredient, _) => ingredient).mkString(",")

package astrac.dimapn.sbt

import scala.meta._

class ArrowsTupleOpsGenerator(arity: Int) {
  assert(arity >= 2)

  def typeParam(name: String) =
    Type.Param(
      Nil, Type.Name(name), Nil, Type.Bounds(None, None), Nil, Nil
    )

  val typeParameters = Stream
    .from(1, 1)
    .map(i => (typeParam(s"A$i"), typeParam(s"B$i")))

  val typeNames = Stream
    .from(1, 1)
    .map(i => (Type.Name(s"A$i"), Type.Name(s"B$i")))

  val GenericArrowTupleOps = Type.Name(s"GenericArrowTuple${arity}Ops")
  val SameInputArrowTupleOps = Type.Name(s"SameInputArrowTuple${arity}Ops")

  val tNames = typeNames.take(arity).toList

  val tParamsPairs = typeParameters.take(arity).toList

  val inputTParams = tParamsPairs.map(_._1)
  val outputTParams = tParamsPairs.map(_._2)
  val allTParams = tParamsPairs.flatMap(p => List(p._1, p._2))

  val genericTupleTypeArgs = tNames.map(tps => t"F[${tps._1}, ${tps._2}]")
  val sameInputTupleTypeArgs = tNames.map(tps => t"F[A, ${tps._2}]")

  val nestedSplit = (1 to arity).map(i => s"tuple._$i").mkString(" *** ").parse[Term].get
  val nestedMerge = (1 to arity).map(i => s"tuple._$i").mkString(" &&& ").parse[Term].get

  val inputTypes = (1 to arity).map(i => s"A$i".parse[Type].get).toList
  val inputPat = (1 to arity).map(i => s"a$i".parse[Pat].get).toList
  val inputNested = (2 to arity).foldLeft(s"a1")((acc, i) => s"($acc, a$i)").parse[Term].get

  val outputTypes = (1 to arity).map(i => s"B$i".parse[Type].get).toList
  val outputPat = (2 to arity).foldLeft(s"b1")((acc, i) => s"($acc, b$i)").parse[Pat].get
  val outputParams = (1 to arity).map(i => s"b$i".parse[Term].get).toList

  def sameInputTupleOps: Stat =
    q"""
implicit class $SameInputArrowTupleOps[F[_, _], A, ..$outputTParams](
    tuple: (..$sameInputTupleTypeArgs))(
    implicit arrow: cats.arrow.Arrow[F]) {

  def mergeN: F[A, (..$outputTypes)] = $nestedMerge.rmap {
    case ($outputPat) => (..$outputParams)
  }

  def mergeMapN[B](f: (..$outputTypes) => B): F[A, B] = mergeN.rmap(f.tupled)
}
"""

  def genericTupleOps: Stat =
    q"""
implicit class $GenericArrowTupleOps[F[_, _], ..$allTParams](
    tuple: (..$genericTupleTypeArgs))(
    implicit arrow: cats.arrow.Arrow[F]) {

  def splitN: F[(..$inputTypes), (..$outputTypes)] = $nestedSplit.lmap[(..$inputTypes)] {
    case (..$inputPat) => $inputNested
  }.rmap {
    case ($outputPat) => (..$outputParams)
  }

  def lmapN[A](f: A => (..$inputTypes)): F[A, (..$outputTypes)] = splitN.lmap(f)

  def rmapN[A](f: (..$outputTypes) => A): F[(..$inputTypes), A] = splitN.rmap(f.tupled)

  def dimapN[A, B](f: A => (..$inputTypes))(g: (..$outputTypes) => B): F[A, B] = splitN.dimap(f)(g.tupled)
}
"""

  def members: List[Stat] =
    genericTupleOps :: sameInputTupleOps :: Nil
}

object ArrowsTupleOpsGenerator {

  val members = (2 to 22).toList.flatMap(arity => new ArrowsTupleOpsGenerator(arity).members)

  private val program = q"""
package astrac {
  package object dimapn {
    object syntax {
      import cats.syntax.arrow._
      import cats.syntax.profunctor._

      ..$members
    }
  }
}
"""

  val tree = program.toString
}

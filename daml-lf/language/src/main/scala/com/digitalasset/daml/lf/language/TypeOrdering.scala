// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.language

import com.daml.lf.data.{FrontStack, FrontStackCons, ImmArray}

import scala.annotation.tailrec

object TypeOrdering extends Ordering[Ast.Type] {

  @throws[IllegalArgumentException]
  // throws an exception if x or y is not comparable,
  // i.e. it contains variables, quantifier, or synonym.
  def compare(x: Ast.Type, y: Ast.Type): Int =
    compareType(0, FrontStack((x, y)))

  private[this] val builtinTypeRank =
    List(
      Ast.BTUnit,
      Ast.BTBool,
      Ast.BTInt64,
      Ast.BTText,
      Ast.BTNumeric,
      Ast.BTTimestamp,
      Ast.BTDate,
      Ast.BTParty,
      Ast.BTContractId,
      Ast.BTArrow,
      Ast.BTOptional,
      Ast.BTList,
      Ast.BTTextMap,
      Ast.BTGenMap,
      Ast.BTAny,
      Ast.BTTypeRep,
      Ast.BTUpdate,
      Ast.BTScenario
    ).zipWithIndex.toMap

  private[this] def typeRank(typ: Ast.Type): Int =
    typ match {
      case Ast.TBuiltin(_) => 0
      case Ast.TTyCon(_) => 1
      case Ast.TNat(_) => 2
      case Ast.TStruct(_) => 3
      case Ast.TApp(_, _) => 4
      case Ast.TVar(_) | Ast.TForall(_, _) | Ast.TSynApp(_, _) =>
        throw new IllegalArgumentException(s"cannot compare types $typ")
    }

  @tailrec
  // Any two ground types (types without variable nor quantifiers) can be compared.
  private[this] def compareType(diff: Int, stack0: FrontStack[(Ast.Type, Ast.Type)]): Int =
    if (diff != 0) diff
    else
      stack0 match {
        case FrontStackCons(tuple, stack) =>
          tuple match {
            case (Ast.TBuiltin(x), Ast.TBuiltin(y)) =>
              compareType(builtinTypeRank(x) compareTo builtinTypeRank(y), stack)
            case (Ast.TTyCon(x), Ast.TTyCon(y)) =>
              compareType(x compare y, stack)
            case (Ast.TNat(x), Ast.TNat(y)) =>
              compareType(x compareTo y, stack)
            case (Ast.TStruct(xs), Ast.TStruct(ys)) =>
              compareType(
                Ordering
                  .Iterable[String]
                  .compare(xs.iterator.map(_._1).toSeq, ys.iterator.map(_._1).toSeq),
                (xs.iterator.map(_._2) zip ys.iterator.map(_._2)).to[ImmArray] ++: stack
              )
            case (Ast.TApp(x1, x2), Ast.TApp(y1, y2)) =>
              compareType(0, (x1, y1) +: (x2, y2) +: stack)
            case (x, y) =>
              // This only occurs when x and y have different ranks or are not comparable
              typeRank(x) compareTo typeRank(y)
          }
        case FrontStack() =>
          diff
      }

}

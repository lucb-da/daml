// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.language

import com.daml.lf.testing.parser.Implicits._

import scala.util.Random

class TypeOrderingSpec
    extends org.scalatest.WordSpec
    with org.scalatest.Matchers
    with org.scalatest.prop.TableDrivenPropertyChecks {

  private[this] val comparableTypesInOder =
    Vector(
      t"Unit",
      t"Bool",
      t"Int64",
      t"Text",
      t"Numeric",
      t"Timestamp",
      t"Date",
      t"Party",
      t"ContractId",
      t"Arrow",
      t"Option",
      t"List",
      t"TextMap",
      t"GenMap",
      t"Any",
      t"TypeRep",
      t"Update",
      t"Scenario",
      t"a:a",
      t"a:a.a",
      t"a:aa",
      t"a:b",
      t"a.a:a",
      t"aa:a",
      t"b:a",
      t"0",
      t"10",
      t"37",
      t"<a: Unit>",
      t"<a: Any>",
      t"<a: a:a>",
      t"<a: a:b>",
      t"<a: 1>",
      t"<a: 2>",
      t"<a: <a: Unit>>",
      t"<a: (Option Unit)>",
      t"<a: (Option Int64)>",
      t"<a: (List Unit)>",
      t"<a: <a: Unit>, b: Int64>",
      t"<a: <a: Unit>, b: Date>",
      t"<a: <a: Unit>, b: (List Text)>",
      t"<a: (List Unit), b: Date>",
      t"<a: <a: Unit>, b: Date, c: Party>",
      t"<a: (List Unit), c: Date>",
      t"<b: Unit>",
      t"<b: Date>",
      t"<b: Date, c: Party>",
      t"(Numeric Int64)",
      t"(Option Int64)",
      t"(Option a:a)",
      t"(Option b:a)",
      t"(Option <a: Int64>)",
      t"(Option <a: Int64, b: Text>)",
      t"(List Int64)",
      t"(List (List Int64))",
      t"(List (List Text))",
      t"(a:a Int64)",
      t"(a:a b:b)",
      t"((a:a a:a) Int64)",
      t"((a:a a:a) b:b)",
      t"((a:a a:a) (List Int64))",
      t"((a:a a:a) (List Text))",
      t"((a:a a:a) (b:b b:b))",
      t"((a:a (a:a a:a)) Int64)",
    )

  val incomparableTypes = Vector(
    t"x",
    t"y",
    t"forall x. List x",
    t"forall y. List y",
    t"forall x. Option x",
    t"|a:a|",
    t"|b:b|",
  )

  "TypeOrdering.compare" should {
    import TypeOrdering.compare

    "reflexive" in {
      for {
        x <- comparableTypesInOder
      } compare(x, x) shouldBe 0
    }

    "respect the specify order" in {
      for {
        i <- comparableTypesInOder.indices
        x = comparableTypesInOder(i)
        j <- i + 1 until comparableTypesInOder.size
        y = comparableTypesInOder(j)
      } {
        compare(x, y) shouldBe -1
        compare(y, x) shouldBe +1
      }
    }

    "sort as expected" in {
      Random.shuffle(comparableTypesInOder).sorted(TypeOrdering) shouldBe comparableTypesInOder
    }

    "fail when given 1 incomparable Types" in {
      for {
        x <- incomparableTypes
        y <- comparableTypesInOder
      } {
        a[IllegalArgumentException] shouldBe thrownBy(compare(x, y))
        a[IllegalArgumentException] shouldBe thrownBy(compare(x, y))
      }
    }

    "fail when given 2 incomparable Types" in {
      for {
        x <- incomparableTypes
        y <- incomparableTypes
      } {
        a[IllegalArgumentException] shouldBe thrownBy(compare(x, y))
        a[IllegalArgumentException] shouldBe thrownBy(compare(x, y))
      }
    }

  }

}

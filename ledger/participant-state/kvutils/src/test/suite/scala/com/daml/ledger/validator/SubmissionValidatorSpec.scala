// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.validator

import java.time.Clock

import com.codahale.metrics.MetricRegistry
import com.daml.caching.Cache
import com.daml.ledger.participant.state.kvutils.DamlKvutils._
import com.daml.ledger.participant.state.kvutils.MockitoHelpers.captor
import com.daml.ledger.participant.state.kvutils.{Bytes, Envelope, KeyValueCommitting}
import com.daml.ledger.participant.state.v1.ParticipantId
import com.daml.ledger.validator.SubmissionValidator.RawKeyValuePairs
import com.daml.ledger.validator.SubmissionValidatorSpec._
import com.daml.ledger.validator.ValidationFailed.{MissingInputState, ValidationError}
import com.daml.lf.data.Time.Timestamp
import com.daml.lf.engine.Engine
import com.daml.metrics.Metrics
import com.github.ghik.silencer.silent
import com.google.protobuf.{ByteString, Empty}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito.{times, verify, when}
import org.scalatest.mockito.MockitoSugar._
import org.scalatest.{AsyncWordSpec, Inside, Matchers}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@silent("deprecated \\(since v1\\.5\\): To be removed in v1\\.6\\.")
class SubmissionValidatorSpec extends AsyncWordSpec with Matchers with Inside {
  "validate" should {
    "return success in case of no errors during processing of submission" in {
      val mockStateOperations = mock[LedgerStateOperations[Unit]]
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))
      val instance = SubmissionValidator.create(
        new FakeStateAccess(mockStateOperations),
        metrics = new Metrics(new MetricRegistry),
        engine = Engine.DevEngine(),
      )
      instance.validate(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId()).map {
        inside(_) {
          case Right(_) => succeed
          case Left(error: ValidationError) => fail(s"ValidationError: $error")
        }
      }
    }

    "signal missing input in case state cannot be retrieved" in {
      val mockStateOperations = mock[LedgerStateOperations[Unit]]
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(None)))
      val instance = SubmissionValidator.create(
        ledgerStateAccess = new FakeStateAccess(mockStateOperations),
        checkForMissingInputs = true,
        metrics = new Metrics(new MetricRegistry),
        engine = Engine.DevEngine(),
      )
      instance.validate(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId()).map {
        inside(_) {
          case Left(MissingInputState(keys)) => keys should have size 1
        }
      }
    }

    "return invalid submission for invalid envelope" in {
      val mockStateOperations = mock[LedgerStateOperations[Unit]]
      val instance = SubmissionValidator.create(
        new FakeStateAccess(mockStateOperations),
        metrics = new Metrics(new MetricRegistry),
        engine = Engine.DevEngine(),
      )
      instance
        .validate(
          ByteString.copyFrom(Array[Byte](1, 2, 3)),
          "aCorrelationId",
          newRecordTime(),
          aParticipantId())
        .map {
          inside(_) {
            case Left(ValidationError(reason)) => reason should include("Failed to parse")
          }
        }
    }

    "return invalid submission in case exception is thrown during processing of submission" in {
      val mockStateOperations = mock[BatchingLedgerStateOperations[Unit]]
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))

      val failingProcessSubmission: SubmissionValidator.ProcessSubmission =
        (_, _, _, _, _) => throw new IllegalArgumentException("Validation failed")

      val instance = new SubmissionValidator(
        new FakeStateAccess(mockStateOperations),
        failingProcessSubmission,
        allocateLogEntryId = () => aLogEntryId(),
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = new Metrics(new MetricRegistry),
      )
      instance.validate(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId()).map {
        inside(_) {
          case Left(ValidationError(reason)) => reason should include("Validation failed")
        }
      }
    }
  }

  "validateAndCommit" should {
    "write marshalled log entry to ledger" in {
      val mockStateOperations = mock[LedgerStateOperations[Int]]
      val expectedLogResult: Int = 3
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))
      val logEntryValueCaptor = captor[Bytes]
      val logEntryIdCaptor = captor[Bytes]
      when(
        mockStateOperations.appendToLog(logEntryIdCaptor.capture(), logEntryValueCaptor.capture())(
          any[ExecutionContext]()))
        .thenReturn(Future.successful(expectedLogResult))
      val expectedLogEntryId = aLogEntryId()
      val mockLogEntryIdGenerator = mockFunctionReturning(expectedLogEntryId)
      val metrics = new Metrics(new MetricRegistry)
      val instance = new SubmissionValidator(
        ledgerStateAccess = new FakeStateAccess(mockStateOperations),
        processSubmission = SubmissionValidator
          .processSubmission(new KeyValueCommitting(Engine.DevEngine(), metrics)),
        allocateLogEntryId = mockLogEntryIdGenerator,
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = metrics,
      )
      instance
        .validateAndCommit(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId())
        .map {
          inside(_) {
            case Right(actualLogResult) =>
              actualLogResult should be(expectedLogResult)
              verify(mockLogEntryIdGenerator, times(1)).apply()
              verify(mockStateOperations, times(0))
                .writeState(any[RawKeyValuePairs]())(any[ExecutionContext]())
              logEntryValueCaptor.getAllValues should have size 1
              logEntryIdCaptor.getAllValues should have size 1
              logEntryIdCaptor.getValue should be(expectedLogEntryId.toByteString)
              logEntryValueCaptor.getValue should not be logEntryIdCaptor.getValue
          }
        }
    }

    "write marshalled key-value pairs to ledger" in {
      val mockStateOperations = mock[LedgerStateOperations[Int]]
      val expectedLogResult: Int = 7
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))
      val writtenKeyValuesCaptor = captor[RawKeyValuePairs]
      when(
        mockStateOperations.writeState(writtenKeyValuesCaptor.capture())(any[ExecutionContext]()))
        .thenReturn(Future.unit)
      val logEntryCaptor = captor[Bytes]
      when(
        mockStateOperations.appendToLog(any[Bytes](), logEntryCaptor.capture())(
          any[ExecutionContext]()))
        .thenReturn(Future.successful(expectedLogResult))
      val logEntryAndStateResult = (aLogEntry(), someStateUpdates)
      val instance = new SubmissionValidator(
        ledgerStateAccess = new FakeStateAccess(mockStateOperations),
        processSubmission = (_, _, _, _, _) => logEntryAndStateResult,
        allocateLogEntryId = () => aLogEntryId(),
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = new Metrics(new MetricRegistry),
      )
      instance
        .validateAndCommit(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId())
        .map {
          inside(_) {
            case Right(actualLogResult) =>
              actualLogResult should be(expectedLogResult)
              writtenKeyValuesCaptor.getAllValues should have size 1
              val writtenKeyValues = writtenKeyValuesCaptor.getValue
              writtenKeyValues should have size 1
              Try(SubmissionValidator.bytesToStateValue(writtenKeyValues.head._2)).isSuccess shouldBe true
              logEntryCaptor.getAllValues should have size 1
          }
        }
    }

    "support batch with single submission" in {
      val mockStateOperations = mock[LedgerStateOperations[Int]]
      val expectedLogResult: Int = 7
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))
      val writtenKeyValuesCaptor = captor[RawKeyValuePairs]
      when(
        mockStateOperations.writeState(writtenKeyValuesCaptor.capture())(any[ExecutionContext]()))
        .thenReturn(Future.unit)
      val logEntryCaptor = captor[Bytes]
      when(
        mockStateOperations.appendToLog(any[Bytes](), logEntryCaptor.capture())(
          any[ExecutionContext]()))
        .thenReturn(Future.successful(expectedLogResult))
      val logEntryAndStateResult = (aLogEntry(), someStateUpdates)
      val instance = new SubmissionValidator(
        ledgerStateAccess = new FakeStateAccess(mockStateOperations),
        processSubmission = (_, _, _, _, _) => logEntryAndStateResult,
        allocateLogEntryId = () => aLogEntryId(),
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = new Metrics(new MetricRegistry),
      )
      val batchEnvelope =
        Envelope.enclose(
          DamlSubmissionBatch.newBuilder
            .addSubmissions(
              DamlSubmissionBatch.CorrelatedSubmission.newBuilder
                .setCorrelationId("aCorrelationId")
                .setSubmission(anEnvelope()))
            .build)
      instance
        .validateAndCommit(batchEnvelope, "aBatchCorrelationId", newRecordTime(), aParticipantId())
        .map {
          inside(_) {
            case Right(actualLogResult) =>
              actualLogResult should be(expectedLogResult)
              writtenKeyValuesCaptor.getAllValues should have size 1
              val writtenKeyValues = writtenKeyValuesCaptor.getValue
              writtenKeyValues should have size 1
              Try(SubmissionValidator.bytesToStateValue(writtenKeyValues.head._2)).isSuccess shouldBe true
              logEntryCaptor.getAllValues should have size 1
          }
        }
    }

    "fail when batch contains more than one submission" in {
      val mockStateOperations = mock[LedgerStateOperations[Int]]
      val logEntryAndStateResult = (aLogEntry(), someStateUpdates)
      val instance = new SubmissionValidator(
        ledgerStateAccess = new FakeStateAccess(mockStateOperations),
        processSubmission = (_, _, _, _, _) => logEntryAndStateResult,
        allocateLogEntryId = () => aLogEntryId(),
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = new Metrics(new MetricRegistry),
      )
      val batchEnvelope =
        Envelope.enclose(
          DamlSubmissionBatch.newBuilder
            .addSubmissions(DamlSubmissionBatch.CorrelatedSubmission.newBuilder
              .setCorrelationId("aCorrelationId")
              .setSubmission(anEnvelope()))
            .addSubmissions(DamlSubmissionBatch.CorrelatedSubmission.newBuilder
              .setCorrelationId("aCorrelationId2")
              .setSubmission(anEnvelope()))
            .build)
      instance
        .validateAndCommit(batchEnvelope, "aBatchCorrelationId", newRecordTime(), aParticipantId())
        .map {
          inside(_) {
            case Left(ValidationError(reason)) =>
              reason should include("Unsupported batch size")
          }
        }
    }

    "return invalid submission if state cannot be written" in {
      val mockStateOperations = mock[LedgerStateOperations[Int]]
      when(mockStateOperations.writeState(any[RawKeyValuePairs]())(any[ExecutionContext]()))
        .thenThrow(new IllegalArgumentException("Write error"))
      when(mockStateOperations.readState(any[Seq[Bytes]]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))
      when(mockStateOperations.appendToLog(any[Bytes](), any[Bytes]())(any[ExecutionContext]()))
        .thenReturn(Future.successful(99))
      val logEntryAndStateResult = (aLogEntry(), someStateUpdates)
      val instance = new SubmissionValidator(
        ledgerStateAccess = new FakeStateAccess(mockStateOperations),
        processSubmission = (_, _, _, _, _) => logEntryAndStateResult,
        allocateLogEntryId = () => aLogEntryId(),
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = new Metrics(new MetricRegistry),
      )
      instance
        .validateAndCommit(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId())
        .map {
          inside(_) {
            case Left(ValidationError(reason)) => reason should include("Write error")
          }
        }
    }

    "work with v1.4 LedgerStateAccess" in {
      val mockStateOperations = mock[LedgerStateOperations.v1_4[Int]]
      when(mockStateOperations.modernize()).thenCallRealMethod()
      val expectedLogResult: Int = 3
      when(mockStateOperations.readState(any[Seq[Bytes]]()))
        .thenReturn(Future.successful(Seq(Some(aStateValue()))))
      val logEntryValueCaptor = captor[Bytes]
      val logEntryIdCaptor = captor[Bytes]
      when(
        mockStateOperations.appendToLog(logEntryIdCaptor.capture(), logEntryValueCaptor.capture()))
        .thenReturn(Future.successful(expectedLogResult))
      val expectedLogEntryId = aLogEntryId()
      val mockLogEntryIdGenerator = mockFunctionReturning(expectedLogEntryId)
      val metrics = new Metrics(new MetricRegistry)
      val instance = SubmissionValidator.new_v1_4(
        ledgerStateAccess = new FakeStateAccess.v1_4(mockStateOperations),
        processSubmission = SubmissionValidator
          .processSubmission(new KeyValueCommitting(Engine.DevEngine(), metrics)),
        allocateLogEntryId = mockLogEntryIdGenerator,
        checkForMissingInputs = false,
        stateValueCache = Cache.none,
        metrics = metrics,
      )
      instance
        .validateAndCommit(anEnvelope(), "aCorrelationId", newRecordTime(), aParticipantId())
        .map {
          inside(_) {
            case Right(actualLogResult) =>
              actualLogResult should be(expectedLogResult)
              verify(mockLogEntryIdGenerator, times(1)).apply()
              verify(mockStateOperations, times(0))
                .writeState(any[RawKeyValuePairs]())
              logEntryValueCaptor.getAllValues should have size 1
              logEntryIdCaptor.getAllValues should have size 1
              logEntryIdCaptor.getValue should be(expectedLogEntryId.toByteString)
              logEntryValueCaptor.getValue should not be logEntryIdCaptor.getValue
          }
        }
    }
  }
}

object SubmissionValidatorSpec {

  private def aLogEntry(): DamlLogEntry =
    DamlLogEntry
      .newBuilder()
      .setPartyAllocationEntry(
        DamlPartyAllocationEntry.newBuilder().setParty("aParty").setParticipantId("aParticipant"))
      .build()

  private def aLogEntryId(): DamlLogEntryId = SubmissionValidator.allocateRandomLogEntryId()

  private def someStateUpdates: Map[DamlStateKey, DamlStateValue] = {
    val key = DamlStateKey
      .newBuilder()
      .setContractId(1.toString)
      .build
    val value = DamlStateValue.getDefaultInstance
    Map(key -> value)
  }

  private def aStateValue(): Bytes =
    SubmissionValidator.valueToBytes(DamlStateValue.getDefaultInstance)

  private def anEnvelope(): Bytes = {
    val submission = DamlSubmission
      .newBuilder()
      .setConfigurationSubmission(DamlConfigurationSubmission.getDefaultInstance)
      .addInputDamlState(DamlStateKey.newBuilder.setConfiguration(Empty.getDefaultInstance))
      .build
    Envelope.enclose(submission)
  }

  private def aParticipantId(): ParticipantId = ParticipantId.assertFromString("aParticipantId")

  private def newRecordTime(): Timestamp =
    Timestamp.assertFromInstant(Clock.systemUTC().instant())

  private def mockFunctionReturning[A](returnValue: A): () => A = {
    val mockFunction = mock[() => A]
    when(mockFunction.apply).thenReturn(returnValue)
    mockFunction
  }

  private class FakeStateAccess[LogResult](mockStateOperations: LedgerStateOperations[LogResult])
      extends LedgerStateAccess[LogResult] {
    override def inTransaction[T](
        body: LedgerStateOperations[LogResult] => Future[T],
    )(implicit executionContext: ExecutionContext): Future[T] =
      body(mockStateOperations)
  }

  object FakeStateAccess {

    @silent("deprecated \\(since v1\\.5\\): To be removed in v1\\.6\\.")
    private[SubmissionValidatorSpec] class v1_4[LogResult](
        mockStateOperations: LedgerStateOperations.v1_4[LogResult],
    ) extends LedgerStateAccess.v1_4[LogResult] {
      override def inTransaction[T](
          body: LedgerStateOperations.v1_4[LogResult] => Future[T],
      ): Future[T] =
        body(mockStateOperations)
    }

  }

}

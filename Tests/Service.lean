/-
  Integration tests for service code generation.
-/
import Crucible
import Protolean

namespace Tests.Service

open Crucible
open Protolean.Service

-- Import the service test proto file
proto_import "service_test.proto"

-- Test that the message types are generated
def sampleRequest : Test.Service.Request := {
  query := "test query"
}

def sampleResponse : Test.Service.Response := {
  result := "test result"
}

-- Mock implementation of the service for testing
instance mockTestService : Test.Service.TestServiceClient IO where
  search req := pure { result := s!"Echo: {req.query}" }
  streamResults req := pure #[{ result := s!"Result 1 for {req.query}" }, { result := s!"Result 2 for {req.query}" }]

testSuite "Service Tests"

test "ServiceInfo has correct name" := do
  let info := Test.Service.testServiceInfo
  ensure (info.name == "TestService") s!"Expected 'TestService', got '{info.name}'"

test "ServiceInfo has correct fullName" := do
  let info := Test.Service.testServiceInfo
  ensure (info.fullName == "test.service.TestService") s!"Expected 'test.service.TestService', got '{info.fullName}'"

test "ServiceInfo has correct method count" := do
  let info := Test.Service.testServiceInfo
  ensure (info.methods.length == 2) s!"Expected 2 methods, got {info.methods.length}"

test "Search method is unary" := do
  let info := Test.Service.testServiceInfo
  match info.findMethod "Search" with
  | some m =>
    ensure (!m.inputStream && !m.outputStream) "Search should be unary (no streaming)"
    ensure (m.kind == RpcKind.unary) s!"Search kind should be unary, got {m.kind}"
  | none => ensure false "Search method not found"

test "StreamResults method is server streaming" := do
  let info := Test.Service.testServiceInfo
  match info.findMethod "StreamResults" with
  | some m =>
    ensure (!m.inputStream && m.outputStream) "StreamResults should have outputStream=true only"
    ensure (m.kind == RpcKind.serverStream) s!"StreamResults kind should be serverStream, got {m.kind}"
  | none => ensure false "StreamResults method not found"

test "TestServiceClient type class exists" := do
  -- The TestServiceClient type class should exist
  -- This is verified at compile time - if the file compiles, the class exists
  pure ()

test "mock search returns correct result" := do
  let response ← Test.Service.TestServiceClient.search sampleRequest
  ensure (response.result == "Echo: test query") s!"Mock search returned wrong result: {response.result}"

test "mock streamResults returns correct count" := do
  let responses ← Test.Service.TestServiceClient.streamResults sampleRequest
  ensure (responses.size == 2) s!"Mock streamResults returned wrong count: {responses.size}"

#generate_tests

end Tests.Service

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
  info.name ≡ "TestService"

test "ServiceInfo has correct fullName" := do
  let info := Test.Service.testServiceInfo
  info.fullName ≡ "test.service.TestService"

test "ServiceInfo has correct method count" := do
  let info := Test.Service.testServiceInfo
  info.methods.length ≡ 2

test "Search method is unary" := do
  let info := Test.Service.testServiceInfo
  match info.findMethod "Search" with
  | some m => do
    m.inputStream ≡ false
    m.outputStream ≡ false
    m.kind ≡ RpcKind.unary
  | none => ensure false "Search method not found"

test "StreamResults method is server streaming" := do
  let info := Test.Service.testServiceInfo
  match info.findMethod "StreamResults" with
  | some m => do
    m.inputStream ≡ false
    m.outputStream ≡ true
    m.kind ≡ RpcKind.serverStream
  | none => ensure false "StreamResults method not found"

test "TestServiceClient type class exists" := do
  -- The TestServiceClient type class should exist
  -- This is verified at compile time - if the file compiles, the class exists
  pure ()

test "mock search returns correct result" := do
  let response ← Test.Service.TestServiceClient.search sampleRequest
  response.result ≡ "Echo: test query"

test "mock streamResults returns correct count" := do
  let responses ← Test.Service.TestServiceClient.streamResults sampleRequest
  responses.size ≡ 2



end Tests.Service

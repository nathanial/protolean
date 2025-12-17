/-
  Code generation for gRPC service type classes and metadata.
-/
import Lean
import Protolean.Syntax.AST
import Protolean.Codegen.Names
import Protolean.Codegen.Types

namespace Protolean.Codegen

open Lean
open Protolean.Syntax

/-- Generate a method name from proto RPC name (e.g., "GetUser" -> "getUser") -/
def rpcMethodToLean (name : String) : String :=
  name.decapitalize

/-- Get the return type string for an RPC method -/
def rpcReturnType (m : RpcMethod) : String :=
  let outputType := protoFullNameToLeanString m.outputType.parts
  if m.outputStream then
    s!"(Array {outputType})"
  else
    outputType

/-- Get the input type string for an RPC method -/
def rpcInputType (m : RpcMethod) : String :=
  let inputType := protoFullNameToLeanString m.inputType.parts
  if m.inputStream then
    s!"(Array {inputType})"
  else
    inputType

/-- Generate a type class method signature -/
def generateMethodSignature (m : RpcMethod) : String :=
  let methodName := rpcMethodToLean m.name
  let inputType := rpcInputType m
  let returnType := rpcReturnType m
  s!"  {methodName} : {inputType} → m {returnType}"

/-- Generate a type class for a service (client interface) -/
def generateServiceTypeClassString (ctx : CodegenContext) (svc : ServiceDef) : String :=
  let _ := ctx  -- ctx reserved for future use
  let className := s!"{protoTypeToLean svc.name}Client"
  let methods := svc.methods.map generateMethodSignature
  let methodLines := "\n".intercalate methods
  s!"class {className} (m : Type → Type) [Monad m] where\n{methodLines}"

/-- Generate the ServiceInfo constant for runtime reflection -/
def generateServiceInfoString (ctx : CodegenContext) (pkg : Option FullIdent) (svc : ServiceDef) : String :=
  let _ := ctx  -- ctx reserved for future use
  let infoName := s!"{rpcMethodToLean svc.name}Info"
  let fullName := match pkg with
    | some p => s!"{".".intercalate p.parts}.{svc.name}"
    | none => svc.name

  let methodInfos := svc.methods.map fun m =>
    let optionsList := m.options.map fun opt =>
      s!"(\"{opt.name}\", \"{optionValueToString opt.value}\")"
    let optionsStr := if optionsList.isEmpty then "[]" else s!"[{", ".intercalate optionsList}]"
    s!"    \{ name := \"{m.name}\"\n      inputType := \"{".".intercalate m.inputType.parts}\"\n      outputType := \"{".".intercalate m.outputType.parts}\"\n      inputStream := {m.inputStream}\n      outputStream := {m.outputStream}\n      options := {optionsStr} }"

  let methodInfosStr := ",\n".intercalate methodInfos

  let serviceOptions := svc.options.map fun opt =>
    s!"(\"{opt.name}\", \"{optionValueToString opt.value}\")"
  let serviceOptionsStr := if serviceOptions.isEmpty then "[]" else s!"[{", ".intercalate serviceOptions}]"

  s!"def {infoName} : Protolean.Service.ServiceInfo := \{
  name := \"{svc.name}\"
  fullName := \"{fullName}\"
  methods := [\n{methodInfosStr}\n  ]
  options := {serviceOptionsStr}
}"
where
  optionValueToString : OptionValue → String
    | .ident fi => fi.toString
    | .int i => toString i
    | .float f => toString f
    | .string s => s
    | .bool b => toString b

/-- Generate both type class and info for a service -/
def generateServiceString (ctx : CodegenContext) (pkg : Option FullIdent) (svc : ServiceDef) : String × String :=
  let typeClass := generateServiceTypeClassString ctx svc
  let info := generateServiceInfoString ctx pkg svc
  (typeClass, info)

end Protolean.Codegen

namespace Hedgehog.Experimental.Xunit

open System
open System.Threading.Tasks
open Xunit
open Xunit.Sdk
open Xunit.Abstractions
open Hedgehog
open System.Reflection
open Microsoft.FSharp.Reflection
open Xunit.Sdk

module internal PropertyHelper =
  type MarkerRecord = {``_``:int}
  let private genxAutoBox<'T> = GenX.auto<'T> |> Gen.map box
  let private genxAutoBoxMethodInfo =
    typeof<MarkerRecord>.DeclaringType.GetTypeInfo().DeclaredMethods
    |> Seq.find (fun meth -> meth.Name = "genxAutoBox")

  let check (methodinfo:MethodInfo) testClassInstance =
    let gens =
      methodinfo.GetParameters()
      |> Array.map (fun p ->
        genxAutoBoxMethodInfo
          .MakeGenericMethod(p.ParameterType)
          .Invoke(null, null)
        :?> Gen<obj>)
      |> ArrayGen.toGenTuple
    let invoke t =
      methodinfo.Invoke(testClassInstance, FSharpValue.GetTupleFields t)
      |> function
      | :? bool as b -> Property.ofBool b
      | _            -> Property.success ()
    Property.forAll gens invoke |> Property.check

type PropertyTestInvoker  (test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, beforeAfterAttributes, aggregator, cancellationTokenSource) =
  inherit XunitTestInvoker(test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, beforeAfterAttributes, aggregator, cancellationTokenSource)

  override this.CallTestMethod testClassInstance =
    PropertyHelper.check this.TestMethod testClassInstance
    null

type PropertyTestRunner  (test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource) =
  inherit XunitTestRunner(test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource)

  override this.InvokeTestMethodAsync aggregator =
    PropertyTestInvoker(this.Test, this.MessageBus, this.TestClass, this.ConstructorArguments, this.TestMethod, this.TestMethodArguments, this.BeforeAfterAttributes, aggregator, this.CancellationTokenSource)
      .RunAsync()

type PropertyTestCaseRunner(testCase: IXunitTestCase, displayName, skipReason, constructorArguments, testMethodArguments, messageBus, aggregator, cancellationTokenSource) =
  inherit XunitTestCaseRunner(testCase,               displayName, skipReason, constructorArguments, testMethodArguments, messageBus, aggregator, cancellationTokenSource)

  override this.RunTestAsync() =
    let args = this.TestMethod.GetParameters().Length |> Array.zeroCreate // need to pass the right number of args otherwise an exception will be thrown
    PropertyTestRunner(this.CreateTest(this.TestCase, this.DisplayName), this.MessageBus, this.TestClass, this.ConstructorArguments, this.TestMethod, args, this.SkipReason, this.BeforeAfterAttributes, this.Aggregator, this.CancellationTokenSource)
      .RunAsync()

[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("Hedgehog.Experimental.Xunit.PropertyTestCaseDiscoverer", "Hedgehog.Experimental.Xunit")>]
type public PropertyAttribute() =
  inherit FactAttribute()

open System.ComponentModel
type PropertyTestCase  (diagnosticMessageSink, defaultMethodDisplay, testMethodDisplayOptions, testMethod, ?testMethodArguments) =
  inherit XunitTestCase(diagnosticMessageSink, defaultMethodDisplay, testMethodDisplayOptions, testMethod, (testMethodArguments |> Option.defaultValue null))

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  [<Obsolete("Called by the de-serializer; should only be called by deriving classes for de-serialization purposes")>]
  new() = new PropertyTestCase(null, TestMethodDisplay.ClassAndMethod, TestMethodDisplayOptions.All, null)

  override this.RunAsync(_, messageBus, constructorArguments, aggregator, cancellationTokenSource) =
    PropertyTestCaseRunner(this, this.DisplayName, this.SkipReason, constructorArguments, this.TestMethodArguments, messageBus, aggregator, cancellationTokenSource)
      .RunAsync()

type PropertyTestCaseDiscoverer(messageSink) =

  member _.MessageSink = messageSink

  interface IXunitTestCaseDiscoverer with
    override this.Discover(discoveryOptions, testMethod, _) =
      new PropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), discoveryOptions.MethodDisplayOptionsOrDefault(), testMethod)
      :> IXunitTestCase
      |> Seq.singleton

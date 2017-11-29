namespace FsCheck.NUnit

// open NUnit.Core.Extensibility
open FsCheck
//open FsCheck.NUnit.Addin
(*
?  https://github.com/nunit/docs/wiki/Addin-Replacement-in-the-Framework

[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =
  interface IAddin with
    override x.Install host =
      let tcBuilder = new FsCheckTestCaseBuilder()
      host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
      true
*)
module TestHelper =

  let (|NonNullString|) (str: NonNull<string>) = str.Get

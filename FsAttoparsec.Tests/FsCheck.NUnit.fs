namespace FsCheck.NUnit

open FsCheck
open NUnit.Core.Extensibility

open FsCheck.NUnit.Addin

[<NUnitAddin(Description = "FsCheck addin")>]
type FsCheckAddin() =
  interface IAddin with
    override x.Install host =
      let tcBuilder = new FsCheckTestCaseBuider()
      host.GetExtensionPoint("TestCaseBuilders").Install(tcBuilder)
      true

module TestHelper =

  let (|NonNullString|) (str: NonNull<string>) = str.Get

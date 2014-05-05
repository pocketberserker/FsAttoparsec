namespace Attoparsec.Tests

open NUnit.Framework
open FsUnit
open Attoparsec
open Helper
open Attoparsec.String

[<TestFixture>]
module LTSVTest =

  type Record = Map<string, string>
  type LTSV = Record list

  let intToCharStr i = i |> char |> string

  // see http://ltsv.org/
  let label = takeWhile1 (string >> isMatch @"[0-9A-Za-z_\.-]")
  let fieldValue = takeWhile (string >> isMatch "[\x01-\x080\x0B\x0C\x0E-\xFF]")
  let field =
    (fun k v -> (BmpString.toString k, BmpString.toString v))
    <!> label <*> (string_ (intToCharStr 0x3A) *> fieldValue)
  let record: Parser<_, Record> =
    Map.ofList <!> (sepBy field (string_ (intToCharStr 0x09)))
  let nl = opt (string_ (intToCharStr 0x0D)) *> string_ (intToCharStr 0x0A)
  let ltsv: Parser<_, LTSV> = sepBy record nl
  let parseField input = input |> parse field |> ParseResult.feed ""
  let parseRecord input = input |> parse record |> ParseResult.feed ""
  let parseLTSV input = input |> parse ltsv |> ParseResult.feed ""

  [<Test>]
  let ``field test`` () =
    let expected = ("host", "127.0.0.1")
    let input = "host:127.0.0.1"
    match parseField input with
    | Done(_, actual) -> actual |> should equal expected
    | _ -> Assert.Fail()

  [<Test>]
  let ``record test`` () =
    let expected =
      [
        "host", "127.0.0.1";
         "ident", "-";
         "user", "frank";
         "time", "[10/Oct/2000:13:55:36 -0700]";
         "req" ,"GET /apache_pb.gif HTTP/1.0";
         "status", "200";
         "size", "2326";
         "referer", "http://www.example.com/start.html";
         "ua", "Mozilla/4.08 [en] (Win98; I ;Nav)";
      ]
      |> Map.ofList
    let input = "host:127.0.0.1	ident:-	user:frank	time:[10/Oct/2000:13:55:36 -0700]	req:GET /apache_pb.gif HTTP/1.0	status:200	size:2326	referer:http://www.example.com/start.html	ua:Mozilla/4.08 [en] (Win98; I ;Nav)"
    match parseRecord input with
    | Done(_, actual) -> actual |> should equal expected
    | _ -> Assert.Fail()

  [<Test>]
  let ``ltsv test`` () =
    let expected =
      [
        [
          "title", "test1";
          "user", "user1";
        ] |> Map.ofList;
        [
          "title", "test2";
          "user", "user2";
        ] |> Map.ofList;
      ]
    let input = "title:test1	user:user1\ntitle:test2	user:user2"
    match parseLTSV input with
    | Done(_, actual) -> actual |> should equal expected
    | _ -> Assert.Fail()
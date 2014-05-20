module Attoparsec.Json

open Attoparsec
open Attoparsec.String
open System
open Helper
open Ast

let ws = spaces
let str = pstring

let stringLiteral =
  let escape =
    oneOf "\"\\/bfnrt"
    |>> (function
      | 'b' -> "\b"
      | 'f' -> "\u000C"
      | 'n' -> "\n"
      | 'r' -> "\r"
      | 't' -> "\t"
      | c   -> string c)

  let hex = satisfy (inClass "0-9a-fA-F")

  let unicodeEscape =
    let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
    str "u" >>.
    hex >>= fun h3 ->
    hex >>= fun h2 ->
    hex >>= fun h1 ->
    hex |>> fun h0 ->
      (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
      |> char |> string

  between (str "\"") (str "\"")
    (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\')) (str "\\" >>. (escape <|> unicodeEscape)) |>> BmpString.toString)

let jstring = stringLiteral |>> JString

let pfloat = scientific |>> float

let jnumber = pfloat |>> JNumber

let jtrue  = pstring "true" >>% (JBool true)
let jfalse = pstring "false" >>% (JBool false)
let jnull  = pstring "null" >>% JNull

let jvalue, jvalueRef = createParserForwardedToRef()

let listBetweenStrings sOpen sClose pElement f =
  between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

let keyValue =
  stringLiteral >>= (fun x -> (ws >>. str ":" >>. ws >>. jvalue) |>> (fun y -> (x, y)))

let jlist = listBetweenStrings "[" "]" jvalue JList
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do
  jvalueRef :=
    choice [
      jobject
      jlist
      jstring
      jnumber
      jtrue
      jfalse
      jnull
    ]

let json = ws >>. jvalue .>> ws

let parseJsonString str = parse json str

let formatResult result =
  match ParseResult.feed "" result with
  | Partial _
  | Done _ -> "Success"
  | Fail(_, _, e) -> e

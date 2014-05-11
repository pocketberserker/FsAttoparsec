module Attoparsec.Json

open Attoparsec
open Attoparsec.String
open System
open Helper
open Ast

let newline = satisfy (string >> (fun i -> isMatch "\r\n" i || isMatch "\r" i || isMatch "\n" i))
let ws = many (newline <|> satisfy Char.IsWhiteSpace)
let str = string_

let stringLiteral =
  let escape =
    oneOf "\"\\/bfnrt"
    |> map (function
      | 'b' -> "\b"
      | 'f' -> "\u000C"
      | 'n' -> "\n"
      | 'r' -> "\r"
      | 't' -> "\t"
      | c   -> string c)

  let hex = satisfy (inClass "0-9a-fA-F")

  let unicodeEscape =
    str "u" >>. hex >>= (fun h3 -> hex >>= (fun h2 -> hex >>= (fun h1 -> hex |> map (fun h0 ->
      let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
      (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
      |> char |> string
    ))))

  between (str "\"") (str "\"")
    (sepBy (many (satisfy (fun c -> c <> '"' && c <> '\\')) |> map BmpString.ofList)
      (str "\\" >>. (escape <|> unicodeEscape)) |> map (List.fold (fun acc x -> BmpString.append acc x) BmpString.empty >> BmpString.toString))

let jstring = stringLiteral |> map JString

let pfloat = scientific |> map float

let jnumber = pfloat |> map JNumber

let jtrue  = string_ "true" >>. ok  (JBool true)
let jfalse = string_ "false" >>. ok (JBool false)
let jnull  = string_ "null" >>. ok JNull

let jvalue, jvalueRef = createParserForwardedToRef()

let listBetweenStrings sOpen sClose pElement f =
  between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |> map f)

let keyValue =
  stringLiteral >>= (fun x -> (ws >>. str ":" >>. ws >>. jvalue) |> map (fun y -> (x, y)))

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

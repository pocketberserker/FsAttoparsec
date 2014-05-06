module ParsecClone.Json

open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator
open Ast

let ws = ws <|> (many newline |>> List.reduce (+))

let stringLiteral =
  let escape =
    anyOf matchStr [ "\"";"\\";"/";"b";"f";"n";"r";"t" ]
    |>> (Microsoft.FSharp.Core.Operators.char >> function
      | 'b' -> "\b"
      | 'f' -> "\u000C"
      | 'n' -> "\n"
      | 'r' -> "\r"
      | 't' -> "\t"
      | c   -> string c)

  let hex = regexStr "[0-9a-fA-F]" |>> Microsoft.FSharp.Core.Operators.char

  let unicodeEscape =
    matchStr "u" >>. hex >>= (fun h3 -> hex >>= (fun h2 -> hex >>= (fun h1 -> hex |>> (fun h0 ->
      let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
      let hoge = (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
      hoge |> Microsoft.FSharp.Core.Operators.char |> string
    ))))

  between
    (matchStr "\"")
    (sepBy (manySatisfy (fun c -> c <> "\"" && c <> "\\") any |>> List.reduce (+)) (matchStr "\\" >>. (escape <|> unicodeEscape)))
    (matchStr "\"")

let jstring = stringLiteral |>> JString

let pfloat =
  regexStr @"[0-9]+\.[0-9]*"
  >>= (fun x ->
    match System.Double.TryParse x with
    | true, x -> preturn x
    | false,_ -> pzero)

let jnumber = pfloat |>> JNumber

let jtrue  = matchStr "true" |>>%  (JBool true)
let jfalse = matchStr "false" |>>% (JBool false)
let jnull  = matchStr "null" |>>% JNull

let jvalue, jvalueRef = createParserForwardedToRef()

let listBetweenStrings sOpen sClose pElement f =
  between
    (matchStr sOpen)
    (ws >>. many (sepBy (pElement .>> ws) (matchStr "," .>> ws)) |>> f)
    (matchStr sClose)

let keyValue =
  stringLiteral
  >>= (fun x -> (ws >>. matchStr ":" >>. ws >>. jvalue) |>> (fun y -> (x, y)))

let jlist   = listBetweenStrings "[" "]" jvalue JList
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

let json = ws >>. jvalue .>> ws .>> eof

let parseJsonString str =
  let stream = makeStringStream str
  json stream

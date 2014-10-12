module ParsecClone.Json

open ParsecClone.CombinatorBase
open ParsecClone.StringCombinator
open Ast

let ws = allWhiteSpace

let quote  = matchStr "\""

let stringLiteral = quotedStringLiteral |> between2 quote

let jstring = stringLiteral |>> JString

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

let formatResult result =
  match fst result with
  | Some _ -> "Success"
  | None -> "Failure"

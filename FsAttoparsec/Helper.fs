namespace Attoparsec

module Helper =

  open System.Text.RegularExpressions

  let isMatch regex item = Regex.IsMatch(item, regex)

  let private charClass s =
    let rec inner = function
      | a :: '-' :: b :: xs -> List.append [a..b] (inner xs)
      | x :: xs -> x :: inner xs
      | _ -> []
    inner (List.ofSeq s) |> Seq.distinct

  let inClass (s: string) c = charClass s |> Seq.exists ((=) c)

  let (|Bmp|) s = BmpString.ofString s

  let (|Bin|) array = BinaryArray.create array

namespace Attoparsec

module Helper =

  open System.Text.RegularExpressions

  let isMatch regex item = Regex.IsMatch(item, regex)

  let private charClass (s: string) =
    let rec inner acc xs =
      let len = Array.length xs
      if Array.isEmpty xs then acc
      elif Array.length xs >= 3 && xs.[1] = '-' && xs.[0] < xs.[2] then
        inner (Array.append acc [| xs.[0] .. xs.[2] |]) (Array.sub xs 3 (len - 3))
      else inner ([| yield xs.[0]; yield! acc |]) (Array.sub xs 1 (len - 1))
    inner [||] (s.ToCharArray())

  let inClass (s: string) c = charClass s |> Array.exists ((=) c)

  let (|Bmp|) s = BmpString.ofString s

  let (|Bin|) array = BinaryArray.create array

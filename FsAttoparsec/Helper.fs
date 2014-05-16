namespace Attoparsec

module Helper =

  open System.Text.RegularExpressions
  open System

  let isMatch regex item = Regex.IsMatch(item, regex)

  let private append x x' =
    let s = sizeof<char>
    let l = Array.length x
    let l' = Array.length x'
    let buffer = Array.zeroCreate<char> (l + l')
    Buffer.BlockCopy(x, 0, buffer, 0, l * s)
    Buffer.BlockCopy(x', 0, buffer, l * s, l'*s)
    buffer

  let private charClass (s: string) =
    let rec inner acc xs =
      let len = Array.length xs
      if Array.isEmpty xs then acc
      elif Array.length xs >= 3 && xs.[1] = '-' && xs.[0] < xs.[2] then
        inner (append acc [| xs.[0] .. xs.[2] |]) (Array.sub xs 3 (len - 3))
      else inner (append [| xs.[0] |] acc) (Array.sub xs 1 (len - 1))
    inner [||] (s.ToCharArray())

  let inClass (s: string) c = charClass s |> Array.exists ((=) c)

  let (|Bmp|) s = BmpString.ofString s

  let (|Bin|) array = BinaryArray.create array

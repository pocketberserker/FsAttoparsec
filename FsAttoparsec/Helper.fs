namespace Attoparsec

module Helper =

  open System.Text.RegularExpressions

  let isMatch regex item = Regex.IsMatch(item, regex)

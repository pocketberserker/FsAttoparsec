﻿namespace Attoparsec

type Monoid<'T> =
  abstract member Mempty : 'T
  abstract member Mappend : 'T * 'T -> 'T

module List =

  let monoid<'T> = { new Monoid<'T list> with
    member this.Mempty = []
    member this.Mappend(x, y) = List.append x y }

  let cons x xs = x :: xs

module internal Contract=
  let Requires x = 
    if not x then failwith "Requires!" else ()
namespace Attoparsec

// port from http://hackage.haskell.org/package/parsec-3.1.5/docs/src/Text-Parsec-Expr.html

module Expr =
  
  type Assoc =
    | AssocNone
    | AssocLeft
    | AssocRight

  type Operator<'T, 'U> =
    | Infix of Parser<'T, 'U -> 'U -> 'U> * Assoc
    | Prefix of Parser<'T, 'U -> 'U>
    | Postfix of Parser<'T, 'U -> 'U>

  type OperatorTable<'T, 'U> = Operator<'T, 'U> list list

  let buildExpressionParser operators (simpleExpr: Lazy<Parser<_ ,_>>) =
    let splitOp assoc (rassoc,lassoc,nassoc,prefix,postfix) =
      match assoc with
      | Infix(op, assoc) ->
        match assoc with
        | AssocNone  -> (rassoc, lassoc, op :: nassoc, prefix, postfix)
        | AssocLeft  -> (rassoc, op :: lassoc, nassoc, prefix, postfix)
        | AssocRight -> (op :: rassoc, lassoc, nassoc, prefix, postfix)
      | Prefix op -> (rassoc, lassoc, nassoc, op :: prefix, postfix)
      | Postfix op -> (rassoc, lassoc, nassoc, prefix, op :: postfix)
    let makeParser (term: Lazy<Parser<_, _>>) ops =
      let (rassoc,lassoc,nassoc ,prefix ,postfix) =
        List.foldBack splitOp ops ([], [], [], [], [])
      let rassocOp = choice rassoc
      let lassocOp   = choice lassoc
      let nassocOp   = choice nassoc
      let prefixOp   = choice prefix <?> ""
      let postfixOp  = choice postfix <?> ""
      let ambigious assoc op= parser {
        let! _ = op
        return! error ("ambiguous use of a " + assoc + " associative operator")
      }
      let ambigiousRight    = ambigious "right" rassocOp
      let ambigiousLeft     = ambigious "left" lassocOp
      let ambigiousNon      = ambigious "non" nassocOp
      let postfixP = postfixOp <|> ok id
      let prefixP = prefixOp <|> ok id
      let termP = parser {
        let! pre = prefixP
        let! x = term.Value
        let! post = postfixP
        return post (pre x)
      }
      let rec rassocP x =
        parser {
          let! f = rassocOp
          let! y = termP >>= rassocP1
          return f x y
        }
        <|> ambigiousLeft
        <|> ambigiousNon
      and rassocP1 x = rassocP x <|> ok x
      let rec lassocP x  =
        parser {
          let! f = lassocOp
          let! y = termP
          return!  lassocP1 (f x y)
        }
        <|> ambigiousRight
        <|> ambigiousNon
      and lassocP1 x = lassocP x <|> ok x
      let nassocP x = parser {
        let! f = nassocOp
        let! y = termP
        return!
          ambigiousRight
          <|> ambigiousLeft
          <|> ambigiousNon
          <|> ok (f x y)
      }
      lazy (parser {
        let! x = termP
        return!
          rassocP x <|> lassocP  x <|> nassocP x <|> ok x <?> "operator"
      })
    
    (List.fold makeParser simpleExpr operators).Value

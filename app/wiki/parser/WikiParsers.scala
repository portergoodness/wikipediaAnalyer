package wiki.parser

import scala.util.parsing.combinator.Parsers

trait WikiParsers extends Parsers {
  /** `Tokens` is the abstract type of the `Token`s consumed by the parsers in this component. */
  type Tokens <: WikiTokens


  /** The input-type for these parsers*/
  type Elem = WikiTokens#WikiToken

}
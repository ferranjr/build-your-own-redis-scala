package domain.lexer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerSpec
  extends AnyFlatSpec
    with Matchers {

  it should "tokenize as expected" in {
    val result = Tokenizer.tokenize("+Foo bar\r\n")
    result shouldBe List(Token.Plus, Token.Text("Foo bar"), Token.NewLine)
  }

  it should "tokenize as expected: $*+-:\r\nFoo bar\r\n" in {
    val result = Tokenizer.tokenize("$*+-:\r\nFoo bar\r\n")
    result shouldBe List(
      Token.Dollar,
      Token.Asterisk,
      Token.Plus,
      Token.Hyphen,
      Token.Colon,
      Token.NewLine,
      Token.Text("Foo bar"),
      Token.NewLine
    )
  }

}

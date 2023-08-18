return {
    ast = if script then require(script.ast) else require("./ast"),
    token = if script then require(script.lex_token) else require("./lex_token"),
    lexer = if script then require(script.lexer) else require("./lexer"),
    parser = if script then require(script.parser) else require("./parser")
}
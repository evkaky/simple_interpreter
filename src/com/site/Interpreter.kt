package com.site

enum class TokenType {
    INT,
    PLUS,
    MINUS,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    EOF
}

data class Token(val type: TokenType, val lexeme: String)

class Lexer(private val text: String) {
    private var pos = 0

    private val currChar: Char
        get() = text[pos]

    fun getNextToken(): Token {
        if (pos > text.lastIndex) {
            return Token(TokenType.EOF, "")
        }

        skipSpaces()

        if (currChar.isDigit()) {
            return Token(TokenType.INT, recognizeInt())
        }

        val token = when (currChar) {
            '+' -> Token(TokenType.PLUS, "")
            '-' -> Token(TokenType.MINUS, "")
            '*' -> Token(TokenType.MUL, "")
            '/' -> Token(TokenType.DIV, "")
            '(' -> Token(TokenType.LPAREN, "")
            ')' -> Token(TokenType.RPAREN, "")
            else -> throw Exception("unknown symbol")
        }
        ++pos
        return token
    }

    private fun skipSpaces() {
        while (pos <= text.lastIndex && currChar.isWhitespace()) {
            ++pos
        }
    }

    private fun recognizeInt(): String {
        val start = pos
        while (pos <= text.lastIndex && currChar.isDigit()) {
            ++pos
        }
        return text.slice(start until pos)
    }
}

class Parser(private val lexer: Lexer) {
    private var currToken: Token = lexer.getNextToken()

    private fun eat(type: TokenType) {
        if (type == currToken.type) {
            currToken = lexer.getNextToken()
        } else {
            throw Exception("error parsing input")
        }
    }

    private fun factor(): AST {
        if (currToken.type == TokenType.INT) {
            val value = currToken.lexeme.toInt()
            eat(TokenType.INT)
            return Num(value)
        } else {
            eat(TokenType.LPAREN)
            val node = expr()
            eat(TokenType.RPAREN)
            return node
        }
    }

    private fun term(): AST {
        var node = factor()
        while (currToken.type == TokenType.MUL || currToken.type == TokenType.DIV) {
            val op = currToken.type
            eat(op)
            node = BinOp(left = node, op = op, right = factor())
        }
        return node
    }

    private fun expr(): AST {
        var node = term()
        while (currToken.type == TokenType.PLUS || currToken.type == TokenType.MINUS) {
            val op = currToken.type
            eat(op)
            node = BinOp(left = node, op = op, right = term())
        }
        return node
    }

    fun parse(): AST = expr()
}

sealed class AST
class BinOp(val left: AST, val op: TokenType, val right: AST) : AST()
class Num(val value: Int) : AST()

class Interpreter(private val parser: Parser) {

    fun interprete(): Double = visit(parser.parse())

    private fun visit(node: AST): Double = when (node) {
        is Num -> visitNum(node).toDouble()
        is BinOp -> visitBinOp(node)
    }

    private fun visitBinOp(node: BinOp): Double = when (node.op) {
        TokenType.PLUS -> visit(node.left) + visit(node.right)
        TokenType.MINUS -> visit(node.left) - visit(node.right)
        TokenType.MUL -> visit(node.left) * visit(node.right)
        TokenType.DIV -> visit(node.left) / visit(node.right)
        else -> throw Exception("unknown binary operation")
    }

    private fun visitNum(node: Num): Int = node.value
}

fun main(args: Array<String>) {
    val lexer = Lexer("(7 + 5) * 2")
    val parser = Parser(lexer)
    val interpreter = Interpreter(parser)
    val res = interpreter.interprete()
    print(res)
}




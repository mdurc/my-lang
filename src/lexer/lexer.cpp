#include <list>
#include <vector>
#include "token.cpp"


class Lexer {
public:
    std::list<Token> m_tokens;

    // constructor:
    // opens file, lexes the file from the stream, then closes the file
    Lexer(const std::string& filename);

    friend std::ostream& operator<<(std::ostream& out, const Lexer& lex) {
        out << "Tokens (" << lex.m_tokens.size() << "):" << '\n';
        for (const Token& token : lex.m_tokens) {
            out << token << '\n';
        }
    }
}

enum TokenType {
    // keywords:
    FUNC,       // func
    IF,         // if
    ELSE,       // else
    FOR,        // for
    WHILE,      // while
    PRINT,      // print
    RETURN,     // return
    RETURNS,    // returns
    BREAK,      // break
    CONTINUE,   // continue
    TRUE,       // true
    FALSE,      // false
    NULL_,      // null
    AND,        // and
    OR,         // or

    STRUCT,     // struct
    SWITCH,     // switch
    CASE,       // case
    DEFAULT,    // default

    IMM,        // imm (immutable, though only used for pointee types)
    MUT,        // mut (mutable)
    READ,       // read
    TAKE,       // take
    GIVE,       // give

    PTR,        // ptr

    NEW,        // new
    FREE,       // free

    IDENTIFIER,

    // primitive types:
    // Unsigned
    U8, U16, U32, U64,
    // Signed
    I8, I16, I32, I64,
    // Floating
    F64,
    // Special
    BOOL, STRING, U0,

    // literals
    INT_LITERAL,        // 42, 0x1F, 0b1010
    FLOAT_LITERAL,      // 3.14, 2e5
    STRING_LITERAL,     // "text"

    // syntax:
    LPAREN, RPAREN,     // ()
    LBRACE, RBRACE,     // {}
    LBRACK, RBRACK,     // []
    LANGLE, RANGLE,     // <>
    COMMA, COLON, SEMICOLON, TILDE, DOT,

    // operators:
    BANG,           // !
    PLUS,           // +
    MINUS,          // -
    SLASH,          // /
    STAR,           // *
    EQUAL,          // =
    AMPERSAND,      // &
    MODULO,         // %

    // compound operators:
    WALRUS,         // :=
    EQUAL_EQUAL,    // ==
    BANG_EQUAL,     // !=
    LESS_EQUAL,     // <=
    GREATER_EQUAL,  // >=

    EOF_
}

std::string token_type_to_string(TokenType type) {
    switch (type) {
        // keywords
        case FUNC:          return "func";
        case IF:            return "if";
        case ELSE:          return "else";
        case FOR:           return "for";
        case WHILE:         return "while";
        case PRINT:         return "print";
        case RETURN:        return "return";
        case RETURNS:       return "returns";
        case BREAK:         return "break";
        case CONTINUE:      return "continue";
        case TRUE:          return "true";
        case FALSE:         return "false";
        case NULL_:         return "null";
        case AND:           return "and";
        case OR:            return "or";

        case STRUCT:        return "struct";
        case SWITCH:        return "switch";
        case CASE:          return "case";
        case DEFAULT:       return "default";

        case MUT:           return "mut";
        case READ:          return "read";
        case TAKE:          return "take";
        case GIVE:          return "give";

        case PTR:           return "ptr";

        case NEW:           return "new";
        case FREE:          return "free";

        case IDENTIFIER:    return "_identifier_";

        // primitive types
        case U0:            return "u0";
        case U8:            return "u8";
        case U16:           return "u16";
        case U32:           return "u32";
        case U64:           return "u64";
        case I8:            return "i8";
        case I16:           return "i16";
        case I32:           return "i32";
        case I64:           return "i64";
        case F64:           return "f64";
        case BOOL:          return "bool";
        case STRING:        return "String";

        // literals
        case INT_LITERAL:   return "_int_literal_";
        case FLOAT_LITERAL: return "_float_literal_";
        case STRING_LITERAL:return "_string_literal_";

        // syntax
        case LPAREN:        return "(";
        case RPAREN:        return ")";
        case LBRACE:        return "{";
        case RBRACE:        return "}";
        case LBRACK:        return "[";
        case RBRACK:        return "]";
        case LANGLE:        return "<";
        case RANGLE:        return ">";
        case COMMA:         return ",";
        case DOT:           return ".";
        case COLON:         return ":";
        case SEMICOLON:     return ";";
        case TILDE:         return "~";

        // operators
        case BANG:          return "!";
        case PLUS:          return "+";
        case MINUS:         return "-";
        case SLASH:        return "/";
        case STAR:          return "*";
        case EQUAL:         return "=";
        case AMPERSAND:     return "&";
        case MODULO:        return "%";

        // compound operators
        case WALRUS:        return ":=";
        case BANG_EQUAL:    return "!=";
        case LESS_EQUAL:    return "<=";
        case GREATER_EQUAL: return ">=";

        case EOF_:          return "_EOF_";
        default:            return "UNKNOWN";
    }
}

typedef struct {
    const char* keyword;
    TokenType type;
} KeywordEntry;

static KeywordEntry keyword_map[] = {
    {"func",    FUNC},
    {"if",      IF},
    {"else",    ELSE},
    {"for",     FOR},
    {"while",   WHILE},
    {"print",   PRINT},
    {"return",  RETURN},
    {"returns", RETURNS},
    {"struct",  STRUCT_KW},
    {"switch",  SWITCH},
    {"case",    CASE},
    {"default", DEFAULT},
    {"break",   BREAK},
    {"continue",CONTINUE},
    {"mut",     MUT},
    {"read",    READ},
    {"owned",   OWNED},
    {"safeptr", SAFEPTR},
    {"ownedptr",OWNEDPTR},
    {"sharedptr",SHAREDPTR},
    {"rawptr", RAWPTR},
    {"new", NEW},
    {"free", FREE},
    {"true", TRUE}, {"false", FALSE}, {"null", NULL_}, {"and", AND}, {"or", OR},
    {"u0", U0}, {"u8", U8}, {"u16", U16}, {"u32", U32}, {"u64", U64},
    {"i8", I8}, {"i16", I16}, {"i32", I32}, {"i64", I64}, {"f64", F64},
    {"bool",    BOOL},
    {"String",  STRING},
};

#define KEYWORD_COUNT (sizeof(keyword_map) / sizeof(keyword_map[0]))

class Literal {
public:
    auto& get_data() { return 0; }
}

class IntLiteral: public Literal {
public:
    uint64_t m_data;
    auto& get_data() {
        return m_data;
    }
}

class StringLiteral: public Literal {
public:
    std::string m_data;
    auto& get_data() {
        return m_data;
    }
}

class FloatLiteral: public Literal {
public:
    double m_data;
    auto& get_data() {
        return m_data;
    }
}

class Token {
public:
    TokenType m_type;
    std::string m_lexeme;
    int row, col;

    bool m_is_literal;
    Literal m_lit_data; // if this is a literal

    friend std::ostream& operator<<(std::ostream& out, const Token& token) {
        out << "[" << token_type_to_string(token.m_type) <<
        "] : " << token.m_lexeme;
        if (token.m_is_literal) {
            out << " (" << token.m_lit_data.get_data() << ")";
        }
        out << " at (" << row << ", " << col << ")" << std::endl;
    }
}

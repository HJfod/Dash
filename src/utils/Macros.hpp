#pragma once

#define THROW_LEX_ERR(code, msg, hint, note) \
    return Err(LineError { Error::code, msg, \
    hint, note, positionFromIndex(startIndex),\
    positionFromIndex(m_index), m_instance.getSource()\
    })

#define THROW_SYNTAX_ERR(token, msg, hint, note) \
    return Err(LineError {\
        Error::SyntaxError, msg, hint, note,\
        token.start, token.end, token.source })

#define THROW_TYPE_ERR(msg, hint, note) \
    return LineError { Error::TypeError,\
        msg, hint, note, start, end, source }

#define THROW_TYPE_ERR_AT(msg, hint, note, start, end) \
    return LineError { Error::TypeError,\
        msg, hint, note, start, end, source }

#define THROW_COMPILE_ERR(msg, hint, note) \
    return LineError { Error::CompileError,\
        msg, hint, note, start, end, source }

#define PROPAGATE_ERROR(err) \
    {auto err__ = err; if (!err__) return err__.unwrapErr(); }
#define PROPAGATE_VALUE(val) \
    {auto val__ = val; PROPAGATE_ERROR(val__); return val__.unwrap();}
#define PROPAGATE_ASSIGN(to, expr) \
    {auto val__ = expr; PROPAGATE_ERROR(val__); to = val__.unwrap();}

#define TOKEN_STR(id) ("'" + tokenTypeToString(TokenType::id) + "'")
#define TOKEN_STR_V(id) ("'" + tokenTypeToString(id) + "'")

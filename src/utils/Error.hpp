#pragma once

namespace gdml {
    enum class Error {
        OK = 0,
        Unknown,
        CantOpenFile,
        CantSaveFile,
        UnknownToken,
        UnexpectedToken,
        MalformedToken,
        UnknownEmbeddedLanguage,
        NoLiteralTerminator,
        NoComponentTerminator,
        NoEmbedTerminator,
        InvalidEscape,
        InvalidNumber,
        SyntaxError,
        TypeError,
        CompileError,
        BadCodegen,
        ScrollUpAndReadTheConsoleOutput,
        InternalError,
    };

    constexpr const char* errorToString(Error error) {
        switch (error) {
            default:
            case Error::Unknown: return "Unknown";
            case Error::OK: return "OK";
            case Error::CantOpenFile: return "CantOpenFile";
            case Error::CantSaveFile: return "CantSaveFile";
            case Error::UnknownToken: return "UnknownToken";
            case Error::UnexpectedToken: return "UnexpectedToken";
            case Error::MalformedToken: return "MalformedToken";
            case Error::UnknownEmbeddedLanguage: return "UnknownEmbeddedLanguage";
            case Error::NoLiteralTerminator: return "NoLiteralTerminator";
            case Error::NoComponentTerminator: return "NoComponentTerminator";
            case Error::NoEmbedTerminator: return "NoEmbedTerminator";
            case Error::InvalidEscape: return "InvalidEscape";
            case Error::InvalidNumber: return "InvalidNumber";
            case Error::SyntaxError: return "SyntaxError";
            case Error::TypeError: return "TypeError";
            case Error::CompileError: return "CompileError";
            case Error::BadCodegen: return "BadCodegen";
            case Error::InternalError: return "InternalError";
            case Error::ScrollUpAndReadTheConsoleOutput: return "ScrollUpAndReadTheConsoleOutput";
        }
    }
}

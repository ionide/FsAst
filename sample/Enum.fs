module Enum

/// enum uint32
type CXErrorCode =
    | CXError_Success = 0u
    | CXError_Failure = 1u
    | CXError_Crashed = 2u
    | CXError_InvalidArguments = 3u
    | CXError_ASTReadError = 4u
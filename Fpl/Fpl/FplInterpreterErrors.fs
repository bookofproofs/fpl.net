module FplInterpreterErrors

type FplInterpreterErrorCode =
    | NSP000 of string
    | NSP001 of string
    | NSP002 of string * string
    | NSP003 of string

let getErroMsg = function  
    | NSP000 fileNamePattern -> sprintf "%s could not be loaded" fileNamePattern
    | NSP001 fileName -> sprintf "%s not found" fileName
    | NSP002 (url, innerErrMsg) -> sprintf "%s not downloadable: %s" url innerErrMsg
    | NSP003 alias -> sprintf "Alias %s appeared previously in this namespace" alias


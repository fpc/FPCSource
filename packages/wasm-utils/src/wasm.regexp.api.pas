unit wasm.regexp.api;

{$mode ObjFPC}{$H+}

interface

uses wasm.regexp.shared;

function __wasm_regexp_allocate(aExpr : PByte; aExprLen : longint; aFlags : Longint; aID : PWasmRegExpID) : TWasmRegexpResult; external regexpExportName name regexpFN_Allocate;
function __wasm_regexp_deallocate(aExprID : TWasmRegExpID) : TWasmRegexpResult; external regexpExportName name regexpFN_DeAllocate;
function __wasm_regexp_exec(aExprID : TWasmRegExpID; aString : PByte; aStringLen :Longint; aIndex : PLongint; aResultCount : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_Exec;
function __wasm_regexp_test(aExprID : TWasmRegExpID; aString : PByte; aStringLen :Longint; aResult : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_Test;
function __wasm_regexp_get_flags(aExprID : TWasmRegExpID; aFlags : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetFlags;
function __wasm_regexp_get_expression(aExprID : TWasmRegExpID; aExp : PByte; aExpLen : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetExpression;
function __wasm_regexp_get_last_index(aExprID : TWasmRegExpID; aLastIndex : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetLastIndex;
function __wasm_regexp_get_result_match(aExprID : TWasmRegExpID; aIndex : Longint; Res : PByte; ResLen : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetResultMatch;
function __wasm_regexp_get_group_count(aExprID : TWasmRegExpID; aCount: PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetGroupCount;
function __wasm_regexp_get_group_name(aExprID : TWasmRegExpID; aIndex : Longint; aName : PByte; aNameLen : PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetGroupName;
function __wasm_regexp_get_named_group(aExprID : TWasmRegExpID; aName : PByte; aNameLen : Longint; aValue : PByte; aValueLen: PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetNamedGroup;
function __wasm_regexp_get_indexes(aExprID : TWasmRegExpID; aIndex : Longint; aStartIndex : PLongint; aStopIndex: PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetIndexes;
function __wasm_regexp_get_named_group_indexes(aExprID : TWasmRegExpID; aName : PByte; aNameLen : Integer; aStartIndex : PLongint; aStopIndex: PLongint) : TWasmRegexpResult; external regexpExportName name regexpFN_GetNamedGroupIndexes;

implementation

end.


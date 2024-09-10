{
    This file is part of the Free Component Library

    Webassembly RegExp API - Shared constants & functions
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.regexp.shared;

{$mode ObjFPC}{$H+}

interface

Type
  TWasmRegexpID = Longint;
  TWasmRegexpResult = Longint;
  {$IFNDEF PAS2JS}
  TWasmPointer = Pointer;
  PWasmRegexpID = ^TWasmRegexpID;
  {$ELSE}
  TWasmPointer = Longint;
  PWasmRegexpID = TWasmPointer;
  {$ENDIF}

Const

  WASMRE_RESULT_SUCCESS    = 0;
  WASMRE_RESULT_ERROR      = -1;
  WASMRE_RESULT_INVALIDID  = -2;
  WASMRE_RESULT_NO_MEM     = -3;
  WASMRE_RESULT_NO_REGEXP  = -4;
  WASMRE_RESULT_INVALIDIDX = -5;
  WASMRE_RESULT_NOINDEXES  = -6;

  WASMRE_FLAG_DOTALL      = 1;
  WASMRE_FLAG_GLOBAL      = 2;
  WASMRE_FLAG_INDICES     = 4;
  WASMRE_FLAG_IGNORECASE  = 8;
  WASMRE_FLAG_MULTILINE   = 16;
  WASMRE_FLAG_STICKY      = 32;
  WASMRE_FLAG_UNICODE     = 64;
  WASMRE_FLAG_UNICODESETS = 128;

  // Aliases that correspond to the letters used when creating a regexp
  WASMRE_FLAG_S = WASMRE_FLAG_DOTALL;
  WASMRE_FLAG_G = WASMRE_FLAG_GLOBAL;
  WASMRE_FLAG_D = WASMRE_FLAG_INDICES;
  WASMRE_FLAG_I = WASMRE_FLAG_IGNORECASE;
  WASMRE_FLAG_M = WASMRE_FLAG_MULTILINE;
  WASMRE_FLAG_Y = WASMRE_FLAG_STICKY;
  WASMRE_FLAG_U = WASMRE_FLAG_UNICODE;
  WASMRE_FLAG_V = WASMRE_FLAG_UNICODESETS;

  regexpExportName = 'regexp';
  regexpFN_Allocate = 'allocate';
  regexpFN_DeAllocate = 'deallocate';
  regexpFN_Exec = 'exec';
  regexpFN_Test = 'test';
  regexpFN_GetFlags = 'get_flags';
  regexpFN_GetExpression = 'get_expression';
  regexpFN_GetLastIndex = 'get_last_index';
  regexpFN_SetLastIndex = 'set_last_index';
  regexpFN_GetResultMatch = 'get_result_match';
  regexpFN_GetGroupCount = 'get_group_count';
  regexpFN_GetGroupName = 'get_group_name';
  regexpFN_GetNamedGroup = 'get_named_group';
  regexpFN_GetIndexes = 'get_indexes';
  regexpFN_GetNamedGroupIndexes = 'get_named_group_indexes';

Function StringToRegexpFlags(const S : String; IgnoreUnknown : Boolean = True) : Longint;
Function RegexpFlagsToString(S : Longint) : String;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}


Function StringToRegexpFlags(const S : String; IgnoreUnknown : Boolean = True) : Longint;

var
  C : Char;
  Flag : Longint;

begin
  Result:=0;
  for C in S do
   begin
   case C of
     's': Flag:=WASMRE_FLAG_S;
     'g': Flag:=WASMRE_FLAG_G;
     'd': Flag:=WASMRE_FLAG_D;
     'i': Flag:=WASMRE_FLAG_I;
     'm': Flag:=WASMRE_FLAG_M;
     'y': Flag:=WASMRE_FLAG_Y;
     'u': Flag:=WASMRE_FLAG_U;
     'v': Flag:=WASMRE_FLAG_V;
   else
     if not IgnoreUnknown then
       Raise EConvertError.CreateFmt('Unknown regexp flag: %s',[C]);
     Flag:=0;
   end;
   Result:=Result or Flag;
   end;
end;

Function RegexpFlagsToString(S : Longint) : String;

var
  C,I : Longint;
  Flag : Char;

begin
  Result:='';
  for I:=0 to 7 do
    begin
    C:=S and (1 shl i);
    case C of
      WASMRE_FLAG_S : Flag:='s';
      WASMRE_FLAG_G : Flag:='g';
      WASMRE_FLAG_D : Flag:='d';
      WASMRE_FLAG_I : Flag:='i';
      WASMRE_FLAG_M : Flag:='m';
      WASMRE_FLAG_Y : Flag:='y';
      WASMRE_FLAG_U : Flag:='u';
      WASMRE_FLAG_V : Flag:='v';
    else
      Flag:='0';
    end;
    if Flag<>'0' then
      Result:=Result + Flag;
    end;
end;


end.


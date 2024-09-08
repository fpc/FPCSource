{
    This file is part of the Free Component Library

    Webassembly RegExp API - Object form
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.regexp.objects;

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  wasm.regexp.shared,
  wasm.regexp.api;

Type
  EWasmRegExp = class(Exception);

  TRegexpFlag = (rfUnknown,rfDotAll,rfGlobal,rfIndices,rfIgnoreCase,rfMultiLine,rfSticky,rfUnicode,rfUnicodeSets);
  TRegexpFlags = Set of TRegExpFlag;

  { TRegexpFlagHelper }

  TRegexpFlagHelper = type helper for TRegexpFlag
  Public
    function ToString: String;
    Function AsFlag : Longint;
    Property AsString : String Read ToString;
  end;

  { TRegexpFlagsHelper }

  TRegexpFlagsHelper = type helper for TRegexpFlags
  private
    procedure SetAsFlags(const aValue: Longint);
  public
    function ToString: String;
    Function ToFlags : Longint;
    class function FromFlags(aFlags : Longint) : TRegExpFlags; static;
    Property AsString : String Read ToString;
    Property AsFlags : Longint Read ToFlags Write SetAsFlags;
  end;

  TRegExpMatch = record
    Value : UTF8String;
    StartIndex, StopIndex : Integer;
  end;
  TRegExpMatchArray = array of TRegExpMatch;

  TRegExpGroup = record
    Name,Value : UTF8String;
    StartIndex, StopIndex : Integer;
  end;
  TRegExpGroupArray = array of TRegExpGroup;

  TRegExpResult = record
    Matches : TRegExpMatchArray;
    Input : UTF8String;
    Index : Integer;
    Groups : TRegExpGroupArray;
  end;

  { TWasmRegExp }

  TWasmRegExp = Class(TObject)
  private
    FRegexpID: TWasmRegexpID;
    FFlags : Longint;
    function GetFlags: TRegexpFlags;
    function GetGroups(aCount: Integer): TRegExpGroupArray;
    function GetLastIndex: Longint;
    function GetMatches(aCount: Integer): TRegExpMatchArray;
  protected
    function CheckRegExpResult(Res : TWasmRegexpResult; const aOperation : String; aRaise : Boolean = true) : Boolean;
  Public
    Constructor Create(const aExpression,aFlags : String); overload;
    Constructor Create(const aExpression : String; aFlags : Longint); overload;
    Constructor Create(const aExpression : String; aFlags : TRegexpFlags); overload;
    destructor Destroy; override;
    Function Exec(const aString : String) : TRegExpResult;
    Function Test(const aString : String) : Boolean;
    Property LastIndex : Longint Read GetLastIndex;
    Property RegexpID : TWasmRegExpID Read FRegexpID;
    Property FlagsAsInteger : Integer Read FFlags;
    Property Flags : TRegexpFlags Read GetFlags;
  end;

implementation

{ TRegexpFlagHelper }

function TRegexpFlagHelper.ToString: String;
begin

end;

function TRegexpFlagHelper.AsFlag: Longint;

Const
  FlagValues : Array[TRegexpFlag] of longint
             = (0,
                WASMRE_FLAG_DOTALL,
                WASMRE_FLAG_GLOBAL,
                WASMRE_FLAG_INDICES,
                WASMRE_FLAG_IGNORECASE,
                WASMRE_FLAG_MULTILINE,
                WASMRE_FLAG_STICKY,
                WASMRE_FLAG_UNICODE,
                WASMRE_FLAG_UNICODESETS);



begin
  Result:=FlagValues[Self];
end;

{ TRegexpFlagsHelper }

procedure TRegexpFlagsHelper.SetAsFlags(const aValue: Longint);

var
  F : TRegexpFlag;
  Res : TRegexpFlags;

begin
  Res:=[];
  for F in TRegexpFlag do
    if (F.AsFlag and aValue)<>0 then
      Include(Res,F);
  Self:=Res;
end;

function TRegexpFlagsHelper.ToString: String;

begin
  Result:=RegexpFlagsToString(AsFlags);
end;

function TRegexpFlagsHelper.ToFlags: Longint;
var
  F : TRegexpFlag;
begin
  Result:=0;
  For F in Self do
    Result:=Result or F.AsFlag;
end;

class function TRegexpFlagsHelper.FromFlags(aFlags: Longint): TRegExpFlags;
begin
  Result.AsFlags:=aFlags;
end;


{ TWasmRegExp }


function TWasmRegExp.GetLastIndex: Longint;
begin
  CheckRegExpResult(__wasm_regexp_get_last_index(FRegexpID,@Result),'get_last_index');
end;

function TWasmRegExp.GetFlags: TRegexpFlags;
begin
  Result:=TRegExpFlags.FromFlags(FFlags)
end;

function TWasmRegExp.CheckRegExpResult(Res: TWasmRegexpResult; const aOperation: String; aRaise: Boolean): Boolean;
begin
  Result:=Res=WASMRE_RESULT_SUCCESS;
  if (not Result) and aRaise then
    Raise EWasmRegExp.CreateFmt('Error %d occured during "%s"',[Res,aOperation]);
end;

constructor TWasmRegExp.Create(const aExpression, aFlags: String);
begin
  Create(aExpression,StringToRegexpFlags(aFlags,False));
end;

constructor TWasmRegExp.Create(const aExpression: String; aFlags: Longint);

var
  R : RawByteString;

begin
  R:=UTF8Encode(aExpression);
  FFlags:=aFlags;
  CheckRegexpResult(__wasm_regexp_allocate(PByte(R),Length(R),aFlags,@FRegexpID),regexpFN_Allocate);
end;

constructor TWasmRegExp.Create(const aExpression: String; aFlags: TRegexpFlags);
begin
  Create(aExpression,aFlags.AsFlags);
end;

destructor TWasmRegExp.Destroy;
begin
  CheckRegExpResult(__wasm_regexp_deallocate(FRegexpID),regexpFN_Allocate,false);
  inherited Destroy;
end;

function TWasmRegExp.GetMatches(aCount: Integer): TRegExpMatchArray;

var
  I : Integer;
  Len,lStart,lStop : Longint;
  Res : TWasmRegexpResult;
  S : RawByteString;
  lGetIndexes : Boolean;

begin
  SetLength(Result,aCount);
  lGetindexes:=rfIndices in Flags;
  For I:=0 to aCount-1 do
    begin
    Len:=256;
    Repeat
      SetLength(S,Len);
      Res:=__wasm_regexp_get_result_match(FRegexpID,I,Pbyte(S),@Len);
    Until (Res<>WASMRE_RESULT_NO_MEM);
    SetLength(S,Len);
    CheckRegExpResult(Res,regexpFN_GetResultMatch);
    Result[i].Value:=S;
    S:='';
    if lGetIndexes then
      CheckRegExpResult(__wasm_regexp_get_Indexes(FRegexpID,I,@lStart,@lStop),regexpFN_GetIndexes);
    Result[i].StartIndex:=lStart+1;
    Result[i].StopIndex:=lStop+1;
    end;
end;

function TWasmRegExp.GetGroups(aCount: Integer): TRegExpGroupArray;

var
  I : Integer;
  Len,lStart,lStop : Longint;
  Res : TWasmRegexpResult;
  N,V : RawByteString;
  lGetIndexes : Boolean;

begin
  N:='';
  V:='';
  SetLength(Result,aCount);
  lGetindexes:=rfIndices in Flags;
  For I:=0 to aCount-1 do
    begin
    Len:=256;
    Repeat
      SetLength(N,Len);
      Res:=__wasm_regexp_get_group_name(FRegexpID,I,Pbyte(N),@Len);
    Until (Res<>WASMRE_RESULT_NO_MEM);
    CheckRegExpResult(Res,regexpFN_GetGroupName);
    SetLength(N,Len);
    Result[i].Name:=N;
    Len:=256;
    Repeat
      SetLength(V,Len);
      Res:=__wasm_regexp_get_named_group(FRegexpID,PByte(N),Length(N),Pbyte(V),@Len);
    Until (Res<>WASMRE_RESULT_NO_MEM);
    CheckRegExpResult(Res,regexpFN_GetNamedGroup);
    SetLength(V,Len);
    Result[I].Value:=V;
    if lGetIndexes then
      CheckRegExpResult(__wasm_regexp_get_named_group_indexes(FRegexpID,PByte(N),Length(N),@lStart,@lStop),regexpFN_GetNamedGroup);
    Result[i].StartIndex:=lStart+1;
    Result[i].StopIndex:=lStop+1;
    end;
end;


function TWasmRegExp.Exec(const aString: String): TRegExpResult;

var
  lGroupCount, lIndex, lMatchCount : longint;
  R : RawByteString;

begin
  Result:=Default(TRegExpResult);
  R:=UTF8Encode(aString);
  Result.Input:=R;
  CheckRegexpResult(__wasm_regexp_exec(FRegexpID,Pbyte(R),Length(R),@lIndex,@lMatchCount),regexpFN_exec);
  If lMatchCount=0 then
    exit;
  Result.Index:=lIndex+1;
  Result.Matches:=GetMatches(lMatchCount);
  CheckRegExpResult(__wasm_regexp_get_group_count(FRegexpID,@lGroupCount),regexpFN_GetGroupCount);
  if lGroupCount>0 then
    Result.Groups:=GetGroups(lGroupCount);
end;


function TWasmRegExp.Test(const aString: String): Boolean;

var
  R : RawByteString;
  lRes : Longint;

begin
  R:=UTF8Encode(aString);
  CheckRegexpResult(__wasm_regexp_test(FRegexpID,Pbyte(R),Length(R),@lRes),regexpFN_test);
  Result:=(lRes<>0);
end;

end.


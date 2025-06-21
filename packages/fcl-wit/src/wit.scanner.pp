{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    Tokenizer for WIT documents.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WIT.Scanner;

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}
interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, Fcl.Streams.Extra;
  {$ELSE}
  SysUtils, Classes, Streamex;
  {$ENDIF}


type
  EWITScanner = Class(Exception);

  TTokenType = (
    ttUnknown,
    ttWhitespace,
    ttComment,
    ttEOF,
    // Symbols
    ttOpenRoundBrace,
    ttCloseRoundBrace,
    ttOpenCurlyBrace,
    ttCloseCurlyBrace,
    ttOpenSquareBrace,
    ttCLoseSquareBrace,
    ttColon,
    ttSemicolon,
    ttComma,
    ttEqual,
    ttDot,
    ttPlus,
    ttMinus,
    ttStar,
    ttDiv,
    ttLessThan,
    ttGreaterThan,
    ttAt,
    ttArrow,
    // General
    ttNumber,
    ttStringLiteral,
    ttIdentifier,
    // Keywords
    ttAs,
    ttAsync,
    ttBool,
    ttBorrow,
    ttChar,
    ttConstructor,
    ttEnum,
    ttExport,
    ttF32,
    ttF64,
    ttFlags,
    ttFrom,
    ttFunc,
    ttFuture,
    ttImport,
    ttInclude,
    ttInterface,
    ttList,
    ttOption,
    ttOwn,
    ttPackage,
    ttRecord,
    ttResource,
    ttResult,
    ttS8,
    ttS16,
    ttS32,
    ttS64,
    ttStatic,
    ttStream,
    ttStringType,
    ttTuple,
    ttType,
    ttU8,
    ttU16,
    ttU32,
    ttU64,
    ttUse,
    ttVariant,
    ttWith,
    ttWorld


  );
  TTokenTypes = Set of TTokenType;
  TKeyWordType = ttAs..ttWorld;
  TSymbolType = ttOpenRoundBrace..ttArrow;

  TToken = record
    TokenType: TTokenType;
    Value: string;
    LineNumber: Integer;
    ColumnNumber: Integer;
  end;

  { TWITScanner }

  TWITScanner = class
  private
    FReader: TTextReader;
    FCurrentLine : AnsiString;
    FLineNumber: Integer;
    FColumnNumber: Integer;
    FCurrentChar: AnsiChar;
    FSkipWhitespace: Boolean;
    function GetNewToken(aType: TTokenType): TToken;
    function GetNextLine: boolean;
    function HandleIdentifier: TToken;
    function HandleNumber: TToken;
    function HandleString: TToken;
    function HandleWhiteSpace: TToken;
    function HandleComment: TToken;

    function IsKeyWord(const aValue: string; out aKeyWord: TTokenType): Boolean;
    function IsSymbol(const aValue: char; out aSymbol: TTokenType): Boolean;
  protected
    function GetTokenInternal: TToken;virtual;
    function GetNextChar: boolean;
    function PeekNextChar: AnsiChar;
  public
    // Scanner always owns the reader
    constructor Create(Reader: TTextReader);
    // Scanner does not own the stream;
    constructor Create(Stream : TStream);
    // Pass WIT content.
    constructor Create(WIT : String);
    destructor Destroy; override;
    function GetToken: TToken;
    property LineNumber: Integer read FLineNumber;
    property ColumnNumber: Integer read FColumnNumber;
    property SkipWhitespace : Boolean read FSkipWhitespace Write FSkipWhitespace;
  end;

  { TTokenTypeHelper }

  TTokenTypeHelper = type helper for TTokenType
    function tostring : string;
  end;

  { TTokenTypesHelper }

  TTokenTypesHelper = type helper for TTokenTypes
    function tostring : string;
  end;

implementation

Resourcestring
  SErrInvalidCommentChar = 'Invalid comment character: ';

const
  Keywords: array[TKeyWordType] of string =
    ( 'as',
      'async',
      'bool',
      'borrow',
      'char',
      'constructor',
      'enum',
      'export',
      'f32',
      'f64',
      'flags',
      'from',
      'func',
      'future',
      'import',
      'include',
      'interface',
      'list',
      'option',
      'own',
      'package',
      'record',
      'resource',
      'result',
      's8',
      's16',
      's32',
      's64',
      'static',
      'stream',
      'string',
      'tuple',
      'type',
      'u8',
      'u16',
      'u32',
      'u64',
      'use',
      'variant',
      'with',
      'world');
  Symbols: array[TSymbolType] of string = ('(', ')', '{', '}', '[', ']', ':', ';', ',', '=', '.', '+', '-', '*', '/', '<', '>', '@','->');
  WhitespaceChars: set of char = [' ', #9, #10, #13];

function IsIdentifierStart(c: char): boolean;
begin
  IsIdentifierStart := (c in ['a'..'z', 'A'..'Z', '_','%']);
end;

function IsIdentifierChar(c: char): boolean;
begin
  IsIdentifierChar := (IsIdentifierStart(c) or (c in ['0'..'9','-']));
end;

function IsDigit(c: char): boolean;
begin
  IsDigit := (c in ['0'..'9']);
end;

function IsWhitespace(c: char): boolean;
begin
  IsWhitespace := (c in WhitespaceChars);
end;

{ TWITScanner }

constructor TWITScanner.Create(Reader: TTextReader);
begin
  FReader := Reader;
  FLineNumber := 0;
  FColumnNumber := 0;
  GetNextChar;
end;

constructor TWITScanner.Create(Stream: TStream);

begin
  Create(TStreamReader.Create(Stream));
end;

constructor TWITScanner.Create(WIT: String);
var
  S: TStringStream;

begin
  S:=TStringStream.Create(WIT);
  try
    Create(S);
  finally
    S.Free;
  end;
end;

function TWITScanner.GetNextLine : boolean;
begin
  FCurrentLine:='';
  While (FCurrentLine='') and not FReader.Eof do
    begin
    inc(FLineNumber);
    FCurrentLine:=FReader.ReadLine;
    end;
  Result:=(FCurrentLine<>'');
  FColumnNumber:=0;
end;

function TWITScanner.GetNextChar : boolean;
begin
  Result:=FColumnNumber<Length(FCurrentLine);
  if not Result then
    Result:=GetNextLine;
  if not Result then
    exit;
  inc(FColumnNumber);
  FCurrentChar:=FCurrentLine[FColumnNumber];
end;

function TWITScanner.PeekNextChar: AnsiChar;
begin
  if (FColumnNumber<Length(FCurrentLine)) then
    Result:=FCurrentLine[FColumnNumber+1]
  else
    Result:=#0;
end;

destructor TWITScanner.Destroy;
begin
  FReader.Free;
  inherited Destroy;
end;

function TWITScanner.IsKeyWord(const aValue : string; out aKeyWord : TTokenType) : Boolean;
var
  KW : TKeywordType;
begin
  for kw:=Low(TKeyWordType) to High(TKeywordType) do
    begin
    Result:=KeyWords[kw]=aValue;
    if Result then
      begin
      aKeyword:=kw;
      break;
      end;
    end;
end;

function TWITScanner.IsSymbol(const aValue: char; out aSymbol: TTokenType): Boolean;
var
  Sym : TSymbolType;
begin
  for Sym:=Low(TSymbolType) to High(TSymbolType) do
    begin
    Result:=Symbols[Sym]=aValue;
    if Result then
      begin
      aSymbol:=sym;
      Break;
      end;
    end;
  if (aSymbol=ttMinus) and (PeekNextChar='>') then
    begin
    GetNextChar;
    aSymbol:=ttArrow;
    end;
end;


function TWITScanner.GetNewToken(aType : TTokenType) : TToken;

begin
  Result.TokenType:=aType;
  Result.ColumnNumber:=FColumnNumber;
  Result.LineNumber:=FLineNumber;
  Result.Value:='';
end;

function TWITScanner.HandleWhiteSpace: TToken;

var
  lTokenValue : string;
begin
  lTokenValue:='';
  Result:=GetNewToken(ttWhitespace);
  while IsWhitespace(FCurrentChar) do
    begin
    lTokenValue := lTokenValue + FCurrentChar;
    if Not GetNextChar then
      FCurrentChar := #0;
    end;
  Result.Value := lTokenValue;
end;


function TWITScanner.HandleComment: TToken;

var
  Level : Integer;

begin
  Result:=GetNewToken(ttComment);
  if not GetNextChar then
    Raise EWITScanner.Create(SErrInvalidCommentChar+'EOF');
  if (FCurrentChar='/') then
    begin
    Result.Value:=Copy(FCurrentLine,FColumnNumber+1,Length(FCurrentLine)-FColumnNumber);
    FColumnNumber:=Length(FCurrentLine);
    if not GetNextChar then
      FCurrentChar:=#0;
    end
  else if (FCurrentChar<>'*') then
    Raise EWITScanner.Create(SErrInvalidCommentChar+FCurrentChar)
  else
    begin
    Level:=1;
    While Level<>0 do
      begin
      While (GetNextChar) and not ((FCurrentChar='*') and (PeekNextChar='/')) do
        begin
        if (FCurrentChar='/') and (PeekNextChar='*') then
          Inc(Level);
        if FColumnNumber=1 then
          Result.Value:=Result.Value+#10;
        Result.Value:=Result.Value+FCurrentChar;
        end;
      if Level>1 then
        Result.Value:=Result.Value+'*'; // the / will be added in the next loop
      Dec(Level);
      end;
    GetNextChar;
    if (FCurrentChar<>'/') then
      Raise EWITScanner.Create(SErrInvalidCommentChar+FCurrentChar);
    if not GetNextChar then
      FCurrentChar:=#0;

    end;

end;

function TWITScanner.HandleIdentifier: TToken;

var
  lTokenValue : string;
begin
  Result:=GetNewToken(ttIdentifier);
  lTokenValue:='';
  while IsIdentifierChar(FCurrentChar) do
  begin
    lTokenValue := lTokenValue + FCurrentChar;
    if not GetNextChar then
      FCurrentChar := #0;
  end;
  if not IsKeyWord(lTokenValue,Result.TokenType) then
    begin
    Result.TokenType := ttIdentifier;
    if lTokenValue[1]='%' then
      Delete(lTokenValue,1,1);
    end;
  Result.Value := lTokenValue;
end;

function TWITScanner.HandleNumber: TToken;

var
  lTokenValue : string;
  lDotCount : integer;

begin
  lDotCount:=0;
  Result:=GetNewToken(ttNumber);
  lTokenValue:='';
  while IsDigit(FCurrentChar) or ((FCurrentChar='.') and (lDotCount<2)) do
    begin
    if (FCurrentChar='.') then
      Inc(lDotCount);
    lTokenValue := lTokenValue + FCurrentChar;
    if not GetNextChar then
     FCurrentChar := #0;
    end;
  Result.Value := lTokenValue;
  if lDotCount>1 then
    Result.TokenType:=ttIdentifier;
end;

function TWITScanner.HandleString : TToken;

var
  lTokenValue : string;
begin
  lTokenValue:='';
  Result:=GetNewToken(ttStringLiteral);
  if not GetNextChar then
    FCurrentChar := #0;

  while (FCurrentChar <> '"') and (FCurrentChar <> #0) do
    begin
    lTokenValue := lTokenValue + FCurrentChar;
    if not GetNextChar then
      FCurrentChar := #0;
    end;

  if FCurrentChar = '"' then
    If not GetNextChar then
      FCurrentChar:=#0;
  Result.Value := lTokenValue;
end;

function TWITScanner.GetTokenInternal: TToken;
var
  lToken: TTokenType;
begin
  if FCurrentChar = #0 then
    Result:=GetNewToken(ttEOF)
  else if IsWhitespace(FCurrentChar) then
    Result:=HandleWhitespace
  else if IsIdentifierStart(FCurrentChar) then
    Result:=HandleIdentifier
  else if IsDigit(FCurrentChar) then
    Result:=HandleNumber
  else if FCurrentChar = '"' then
    Result:=HandleString
  else if IsSymbol(FCurrentChar,lToken) then
    begin
    if (lToken=ttDiv) and (PeekNextChar in ['/','*']) then
      Result:=HandleComment
    else
      begin
      Result:=GetNewToken(lToken);
      Result.Value:=FCurrentChar;
      if not getNextChar then
        FCurrentChar:=#0;
      end;
    end
  else
    begin
    Result:=GetNewToken(ttUnknown);
    Result.Value := FCurrentChar;
    if not getNextChar then
      FCurrentChar:=#0;
    end;
end;

function TWITScanner.GetToken: TToken;
var
  lToken : TToken;
begin
  Repeat
    lToken:=GetTokenInternal;
  until (Not (lToken.TokenType in [ttWhitespace,ttComment])) or Not SkipWhiteSpace;
  Result:=lToken;
end;

{ TTokenTypeHelper }

function TTokenTypeHelper.tostring: string;
begin
  if (self>=low(TKeyWordType)) and (self<=high(TKeyWordType)) then
    Result:=Keywords[Self]
  else if (self>=low(TSymbolType)) and (self<=high(TSymbolType)) then
    Result:=Symbols[Self]
  else
    case self of
      ttIdentifier : Result:='identifier';
      ttWhitespace : Result:='whitespace';
    else
      Result:='<unknown>';
    end;
end;

{ TTokenTypesHelper }

function TTokenTypesHelper.tostring: string;
var
  T : TTokenType;
begin
  Result:='';
  For T in Self do
    begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+T.ToString;
    end;
end;

end.

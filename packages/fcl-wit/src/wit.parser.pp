{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    Parser for WIT documents.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WIT.Parser;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
  System.Types,
  {$ELSE}
  SysUtils,
  Types,
  {$ENDIF}
  WIT.Model,
  WIT.Scanner;


Type
  EWITParser = class(EWIT);

  { TAnnotationStore }

  TAnnotationStore = Record
    Items : Array of TWITAnnotation;
    Count : Integer;
    class function Create : TAnnotationStore; static;
    procedure Add(aAnnotation : TWITAnnotation);
    procedure Apply(aEl : TWITBaseElement);
    procedure Free;
  end;

  { TWITParser }

  TWITParser = class
  Private
    FOwnsScanner : Boolean;
    FScanner: TWITScanner;
    FLastToken : TToken;
    FUnGet : TToken;
  protected
    procedure Error(ErrNo : Integer; const Msg : String);
    procedure Error(ErrNo : Integer; const Fmt : String; Const Args : Array of const);
    function GetToken : TToken;
    procedure UngetToken(aToken : TToken);
    procedure CheckToken(constref aToken : TToken;aType : TTokenType);
    procedure CheckTokens(constref aToken : TToken;aTypes : TTokenTypes);
    function ExpectToken(aType : TTokenType) : TToken;
    function ExpectToken(aTypes: TTokenTypes): TToken;
    function ParseInterface(const aName : string = ''): TWITInterface;
    function ParsePackage: TWITPackage;
    procedure ParsePackageContent(aPackage : TWITPackage);
    function ParseAnnotation: TWITAnnotation;
    function ParseExchange: TWITExchange;
    function ParseFunctionResult: TWITType;
    function ParseFunctionParam: TWITFuncParam;
    function ParseFunctionType(aAllowFlags: TWITFunctionFlags): TWITFunctionType;
    function ParseFunction(aAllowStatic: boolean=false): TWITFunction;
    function ParseTopLevelUse: TWITTopLevelUse;
    function ParseUse: TWITUse;
    function ParseInclude : TWITInclude;
    function ParseUsePath(aUse: TWITUsePath; allowAs: boolean; aExtraTerminator: TTokenType=ttEOF): String;
    function ParseWorld: TWITWorld;
    // Types
    function ParseType: TWITType;
    function ParseHandle(aOwned: Boolean): TWITType;
    function ParseEnumType: TWITTypeDef;
    function ParseFlagsType: TWITTypeDef;
    function ParseListType: TWITListType;
    function ParseOptionType: TWITOptionType;
    function ParseFutureType: TWITFutureType;
    function ParseStreamType: TWITStreamType;
    function ParseRecordType: TWITTypeDef;
    function ParseResourceType: TWITTypeDef;
    function ParseResultType: TWITResultType;
    function ParseTupleType: TWITTupleType;
    function ParseVariantType: TWITTypeDef;
    function ParseTypeDef: TWITTypeDef;
  protected
    Property Scanner : TWITScanner read FScanner;
    property LastToken : TToken Read FLastToken;
  Public
    constructor create(aScanner: TWITScanner; aOwnsScanner: Boolean=false);
    function ParseDocument: TWITDocument;
    procedure ParseDocument(aDocument: TWITDocument);
    Destructor destroy; override;
  end;

const
  WITERR_EXPECTEDTOKEN  = 1001;
  WITERR_EXPECTEDTOKENS = 1002;
  WITERR_EXPECTEDDIV = 1003;
  WITERR_DOUBLEUNGET = 1004;
  WITERR_UNEXPECTEDTOKEN = 1005;
  WITERR_ASNOTALLOWED = 1006;
  WITERR_NOGATE = 1007;

Resourcestring
  SErrDoubleUnget = 'UngetToken can be called only once.';
  SErrUnexpected = 'Unexpected token: %s';
  SErrExpectedTokenGot = 'Expected token "%s", got: "%s"';
  SErrExpectedOneOfTokensGot = 'Expected one token of "%s", got: "%s"';
  SErrorAt = 'Error at %s(%d,%d): %s';
  SErrExpectedDiv    = 'Expected /, got @';
  SErrAsNotAllowed = 'As not allowed in this use-path';
  SErrNoGate = 'A gate is not allowed (double gate)';

implementation

{ TAnnotationStore }

class function TAnnotationStore.Create: TAnnotationStore;
begin
  Result.Count:=0;
  Result.Items:=[];
end;

procedure TAnnotationStore.Add(aAnnotation: TWITAnnotation);
begin
  if Count=Length(Items) then
    SetLength(Items,Count+3);
  Items[Count]:=aAnnotation;
  inc(Count);
end;

procedure TAnnotationStore.Apply(aEl: TWITBaseElement);
var
  I : Integer;
begin
  for I:=0 to Count-1 do
    begin
    aEl.AddAnnotation(Items[i]);
    Items[i]:=Nil;
    end;
  Count:=0;
end;

procedure TAnnotationStore.Free;
var
  I : Integer;
begin
  for I:=0 to Count-1 do
    FreeAndNil(Items[i]);
  Count:=0;
end;

function TWITParser.ParseRecordType: TWITTypeDef;
var
  RecordType: TWITRecordType;
  Field: TWITRecordField;
  lFieldType : TWITType;
  lToken: TToken;
  lName,lFieldName : String;

begin
  CheckToken(lAstToken,ttRecord);
  lToken:=ExpectToken(ttIdentifier);
  lName:=lToken.Value;
  lToken:=ExpectToken(ttOpenCurlyBrace);
  RecordType:=TWITRecordType.Create;
  try
    lToken:=GetToken;
    while not (lToken.TokenType in [ttEOF,ttCloseCurlyBrace]) do
      begin
      CheckToken(lToken,ttIdentifier);
      lFieldName:=lToken.Value;
      ExpectToken(ttColon);
      GetToken; // Position on first type token
      lFieldType:=ParseType;
      Field:=TWITRecordField.Create(lFieldName,lFieldType);
      RecordType.AddField(Field);
      lToken:=ExpectToken([ttComma,ttCloseCurlyBrace]);
      if lToken.TokenType=ttComma then
        lToken:=GetToken;
      end;
    CheckToken(lastToken,ttCloseCurlyBrace);
  except
    RecordType.Free;
    Raise;
  end;
  Result:=TWitTypeDef.Create(lName,RecordType);
end;

function TWITParser.ParseFlagsType: TWITTypeDef;
(*
  on entry: flags
  on exit: }
*)
var
  FlagsType: TWITFlagsType;
  lToken : TToken;
  lName : string;
begin
  CheckToken(FLastToken,ttFlags);
  lToken:=ExpectToken(ttIdentifier);
  lName:=lToken.Value;
  FlagsType:=TWITFlagsType.Create;
  try
    ExpectToken(ttOpenCurlyBrace);
    lToken:=GetToken;
    while not (lToken.TokenType in [ttEOF,ttCloseCurlyBrace]) do
      begin
      CheckToken(lToken,ttIdentifier);
      FlagsType.AddFlag(lToken.value);
      lToken:=GetToken;
      if (lToken.TokenType=ttComma) then
        lToken:=GetToken;
      end;
  Except
    FlagsType.Free;
    Raise;
  end;
  Result:=TWitTypeDef.Create(lName,FlagsType);
end;

function TWITParser.ParseEnumType: TWITTypeDef;
(*
  on entry: enum
  on exit: }
*)
var
  EnumType: TWITEnumType;
  lToken : TToken;
  lName : string;

begin
  Result:=Nil;
  CheckToken(LastToken,ttEnum);
  lName:=ExpectToken(ttIdentifier).Value;
  EnumType:=TWITEnumType.Create();
  try
    expectToken(ttOpenCurlyBrace);
    lToken:=GetToken;
    while not (lToken.TokenType in [ttEOF,ttCloseCurlyBrace]) do
      begin
      CheckToken(lToken,ttIdentifier);
      EnumType.AddCase(lToken.Value);
      lToken:=ExpectToken([ttComma,ttCloseCurlyBrace]);
      if lToken.TokenType=ttComma then
        lToken:=GetToken;
      end;
    CheckToken(lastToken,ttCloseCurlyBrace);
  except
    EnumType.Free;
    Raise;
  end;
  Result:=TWITTypeDef.Create(lName,EnumType);
end;

function TWITParser.ParseTupleType: TWITTupleType;
{
  on entry: tuple
  on exit: >
}
var
  TupleType: TWITTupleType;
  lToken : TToken;
begin
  CheckToken(lastToken,ttTuple);
  ExpectToken(ttLessThan);
  TupleType:=TWITTupleType.Create;
  try
    lToken:=GetToken;
    while lToken.TokenType<>ttGreaterThan do
      begin
      TupleType.AddItem(ParseType);
      lToken:=ExpectToken([ttComma,ttGreaterThan]);
      if lToken.TokenType=ttComma then
        lToken:=GetToken;
      end;
  except
    TupleType.Free;
    Raise;
  end;
  Result:=TupleType;
end;

function TWITParser.ParseListType: TWITListType;
{
  on entry : list
  on exit >
}
var
  lToken: TToken;
begin
  ExpectToken(ttLessThan);
  GetToken;
  Result:=TWITListType.Create(ParseType);
  lToken:=ExpectToken([ttGreaterThan,ttComma]);
  if lToken.TokenType=ttComma then
    begin
    lToken:=ExpectToken(ttNumber);
    Result.ItemCount:=StrToInt(lToken.Value);
    ExpectToken(ttGreaterThan);
    end;
end;

function TWITParser.ParseOptionType: TWITOptionType;
{
  on entry: Option
  on exit: >
}
var
  lType : TWITType;
begin
  CheckToken(LastToken,ttOption);
  ExpectToken(ttLessThan);
  GetToken; // position on first token
  lType:=ParseType;
  try
    ExpectToken(ttGreaterThan);
    Result:=TWITOptionType.Create(lType);
    lType:=nil;
  finally
    FreeAndNil(lType);
  end;
end;

function TWITParser.ParseFutureType: TWITFutureType;
{
  on entry: Future
  on exit: >
}
var
  lType : TWITType;
  lToken : TToken;
begin
  lType:=Nil;
  CheckToken(LastToken,ttFuture);
  lToken:=GetToken;
  if lToken.TokenType<>ttLessThan then
    begin
    Result:=TWITFutureType.Create(TWITType.Create(wtVoid));
    UngetToken(LastToken);
    end
  else
    begin
    GetToken; // position on first token of type
    lType:=ParseType;
    try
      ExpectToken(ttGreaterThan);
      Result:=TWITFutureType.Create(lType);
    except
      FreeAndNil(lType);
      Raise;
    end;
    end;
end;

function TWITParser.ParseStreamType: TWITStreamType;
{
  on entry: stream
  on exit: > or stream
}
var
  lType : TWITType;
  lToken : TToken;
begin
  CheckToken(LastToken,ttStream);
  lToken:=GetToken; // position on first token
  if lToken.TokenType<>ttLessThan then
    begin
    Result:=TWITStreamType.Create(TWITType.Create(wtVoid));
    UnGetToken(lToken);
    end
  else
    begin
    GetToken; // Position on first token of type
    lType:=ParseType;
    try
      ExpectToken(ttGreaterThan);
      Result:=TWITStreamType.Create(lType);
      lType:=nil;
    finally
      FreeAndNil(lType);
    end;
    end;
end;

function TWITParser.ParseResultType: TWITResultType;
{
  On entry: result
  on exit: >
}
var
  lToken : TToken;
  lOK,lError : TWITType;

begin
  CheckToken(LastToken,ttResult);
  lOK:=Nil;
  lError:=Nil;
  lToken:=GetToken;
  if lToken.TokenType=ttLessThan then
    begin
    lToken:=GetToken;
    if not ((lToken.TokenType=ttIdentifier) and (ltoken.Value='_')) then
      lOK:=ParseType;
    lToken:=ExpectToken([ttGreaterThan,ttComma]);
    if lToken.TokenType=ttComma then
      begin
      lToken:=GetToken;
      if not ((lToken.TokenType=ttIdentifier) and (ltoken.Value='_')) then
        lError:=ParseType;
      lToken:=ExpectToken(ttGreaterThan);
      end;
    end
  else
    UnGetToken(lToken);
  Result:=TWITResultType.Create(lOK,lError);
end;

function TWITParser.ParseVariantType: TWITTypeDef;
(*
  on entry : variant
  on exit : }
*)
var
  lVariant: TWITVariantType;
  lCase: TWITVariantCase;
  lCaseType : TWITType;
  lToken : TToken;
  lName,lCaseName : string;

begin
  CheckToken(LastToken,ttVariant);
  lToken:=ExpectToken(ttIdentifier);
  lName:=lToken.Value;
  lToken:=ExpectToken(ttOpenCurlyBrace);
  lCaseType:=Nil;
  lVariant:=TWITVariantType.Create;
  try
    lToken:=GetToken;
    while not (lToken.TokenType in [ttEOF,ttCloseCurlyBrace]) do
      begin
      CheckToken(lToken,ttIdentifier);
      lCaseName:=lToken.Value;
      lCaseType:=Nil;
      lToken:=ExpectToken([ttComma,ttCloseCurlyBrace,ttOpenRoundBrace]);
      if lToken.TokenType=ttOpenRoundBrace then
        begin
        GetToken; // Position on first token of type
        lCaseType:=ParseType;
        ExpectToken([ttCloseRoundBrace]);
        lToken:=ExpectToken([ttComma,ttCloseCurlyBrace]);
        end;
      lCase:=TWITVariantCase.Create(lCaseName,lCaseType);
      lCaseType:=Nil;
      lVariant.AddCase(lCase);
      if lToken.TokenType=ttComma then
        lToken:=GetToken;
      end;
    CheckToken(lastToken,ttCloseCurlyBrace);
  except
    lCaseType.Free;
    lVariant.Free;
    Raise;
  end;
  Result:=TWITTypeDef.Create(lName,lVariant);
end;

function TWITParser.ParseHandle(aOwned : Boolean): TWITType;
{
  on entry: borrow/own
  on exit : >
}

var
  lToken : TToken;

begin
  CheckTokens(lastToken,[ttBorrow,ttOwn]);
  ExpectToken(ttLessThan);
  lToken:=ExpectToken(ttIdentifier);
  ExpectToken(ttGreaterThan);
  Result:=TWITHandleType.Create(lToken.Value,Not aOwned);
end;

function TWITParser.ParseType: TWITType;
{
  On Entry: first token of type
  On exit : last token of type
}
var
  lToken: TToken;
begin
  lToken:=LastToken;
  Case lToken.TokenType of
  ttlist:
    Result:=ParseListType;
  ttOwn,
  ttBorrow:
    Result:=ParseHandle(lToken.TokenType=ttOwn);
  ttOption:
    Result:=ParseOptionType;
  ttFuture:
    Result:=ParseFutureType;
  ttStream:
    Result:=ParseStreamType;
  ttResult:
    Result:=ParseResultType;
  ttResource:
    Result:=ParseResourceType;
  ttTuple :
    Result:= ParseTupleType;
  ttChar:
    Result:=TWITType.Create(wtChar);
  ttBool:
    Result:=TWITType.Create(wtBool);
  ttStringType:
    Result:=TWITType.Create(wtString);
  ttU8:
    Result:=TWITType.Create(wtU8);
  ttU16:
    Result:=TWITType.Create(wtU16);
  ttU32:
    Result:=TWITType.Create(wtU32);
  ttU64:
    Result:=TWITType.Create(wtU64);
  ttS8:
    Result:=TWITType.Create(wtS8);
  ttS16:
    Result:=TWITType.Create(wtS16);
  ttS32:
    Result:=TWITType.Create(wtS32);
  tts64:
    Result:=TWITType.Create(wtS64);
  ttf32:
    Result:=TWITType.Create(wtFloat32);
  ttf64:
    Result:=TWITType.Create(wtFloat64);
  ttIdentifier:
    Result:=TWITIdentifierType.Create(lToken.Value);
  else
    raise Exception.Create('Unexpected Token: ' + lToken.Value);
  end;
end;



function TWITParser.ParsePackage: TWITPackage;
(*
  On entry: package
  Exit : ; or }
*)
var
  lToken: TToken;
  Package: TWITPackage;
  PackageName: string;
  Parts: TStringDynArray;
begin
  // Parse 'package' packageName
  lToken:=LastToken;
  CheckToken(lToken,ttPackage);
  PackageName:='';
  Package:=TWITPackage.Create;
  try
    Result:=Package;
    lToken:=GetToken;
    while not (lToken.TokenType in [ttEOF,ttSemicolon,ttOpenCurlyBrace]) do
    begin
      PackageName:=PackageName + lToken.Value;
      lToken:=GetToken;
    end;
    Parts:=PackageName.Split([':', '@']);

    Case Length(Parts) of
    1:
    begin
      Package.Namespace:='';
      Package.PackageName:=Parts[0];
      Package.Version:='';
    end;
    2:
    begin
      Package.Namespace:=Parts[0];
      Package.PackageName:=Parts[1];
      Package.Version:='';
    end;
    3:
    begin
      Package.Namespace:=Parts[0];
      Package.PackageName:=Parts[1];
      Package.Version:=Parts[2];
    end;
    else
      raise Exception.Create('Invalid package name format: ' + PackageName);
    end;

    CheckTokens(lToken,[ttEOF,ttSemicolon,ttOpenCurlyBrace]);
    if (lToken.TokenType in [ttSemicolon,ttEOF]) then
      exit;
    ParsePackageContent(Package);

    CheckToken(LastToken,ttCloseCurlyBrace);
  except
    Package.Free;
    Raise;
  end;
end;


function TWITParser.ParseResourceType: TWITTypeDef;
(*
  on entry: resource
  on exit: ; or }
*)
var
  lToken: TToken;
  lResource: TWITResourceType;
  lName : string;
  lFunctionType : TWITFunctionType;
  lFunction : TWITFunction;
  lAnn : TAnnotationStore;

begin
  lAnn:=TAnnotationStore.Create;
  Checktoken(LastToken,ttResource);
  lToken:=ExpectToken(ttIdentifier);
  lName:=lToken.Value;
  lResource:=TWITResourceType.Create(lName);
  try
    lToken:=ExpectToken([ttSemicolon,ttOpenCurlyBrace]);
    if lToken.TokenType=ttOpenCurlyBrace then
      begin
      lToken:=GetToken;
      while not (lToken.TokenType in [ttEOF,ttCloseCurlyBrace]) do
        begin
        lFunction:=Nil;
        lFunctionType:=Nil;
        CheckTokens(lToken,[ttIdentifier,ttConstructor,ttAt]);
        Case lToken.TokenType of
          ttAt:
            begin
            lAnn.Add(ParseAnnotation);
            lToken:=GetToken;
            end;
          ttIdentifier:
            begin
            lFunction:=ParseFunction(True);
            lResource.AddFunction(lFunction);
            lAnn.Apply(lFunction);
            lFunction:=Nil;
            end;
          ttConstructor:
            begin
            lFunctionType:=ParseFunctionType([ffConstructor]);
            lFunction:=TWITFunction.Create(lName,lFunctionType);
            lFunctionType:=Nil;
            lResource.AddFunction(lFunction);
            lAnn.Apply(lFunction);
            lFunction:=Nil;
            end;
        end;
        if LastToken.TokenType=ttSemicolon then
          lToken:=GetToken;
        end;
      CheckToken(lastToken,ttCloseCurlyBrace);
      end;
    Result:=TWITTypeDef.Create(lName,lResource);
  except
    lAnn.Free;
    lFunctionType.Free;
    lFunction.Free;
    lResource.Free;
    Raise;
  end;
end;



function TWITParser.ParseFunctionParam: TWITFuncParam;
{
  on entry: identifier
  on exit; last token of type
}
var
  lName: String;
  lType : TWITType;
begin
  CheckToken(LastToken,ttIdentifier);
  lName:=LastToken.Value;
  ExpectToken(ttColon);
  GetToken; // Position on first token of type
  lType:=ParseType;
  Result:=TWITFuncParam.Create(lName, lType);
end;

procedure TWITParser.Error(ErrNo: Integer; const Msg: String);
var
  E : EWITParser;
begin
  E:=EWITParser.CreateFmt(SErrorAt,['',FLastToken.LineNumber,FLastToken.ColumnNumber,Msg]);
  E.errno:=Errno;
  Raise E;
end;

procedure TWITParser.Error(ErrNo: Integer; const Fmt: String;
  const Args: array of const);
begin
  Error(Errno,Format(Fmt,Args));
end;


function TWITParser.ParseFunctionResult: TWITType;
begin
  Result:=ParseType;
end;

function TWITParser.ParseFunctionType(aAllowFlags: TWITFunctionFlags): TWITFunctionType;
{
  on entry: func, async, static, constructor
  on exit : semicolon
}
const
  FlagTokens : Array[TWITFunctionFlag] of TTokenType = (ttAsync, ttStatic, ttConstructor);
var
  lTokens : Set of TTokenType;
  lToken : TToken;
  lType : TWITFunctionType;
  lFlag : TWITFunctionFlag;
  lFlags : TWITFunctionFlags;
begin
  lTokens:=[ttFunc];
  for lFlag in aAllowFlags do
    Include(lTokens,FLagTokens[lFlag]);
  lToken:=LastToken;
  CheckTokens(lToken,lTokens);
  Exclude(lTokens,ttStatic);
  lFlags:=[];
  if (lToken.TokenType=ttStatic) then
    Include(lFlags,ffStatic);
  if ffStatic in lFlags then
    lToken:=ExpectToken(lTokens);
  Exclude(lTokens,ttAsync);
  if lToken.TokenType=ttAsync then
    begin
    Include(lFlags,ffAsync);
    lToken:=ExpectToken(lTokens);
    end;
  if lToken.TokenType=ttConstructor then
    Include(lFlags,ffConstructor);
  ExpectToken(ttOpenRoundBrace);
  lType:=TWITFunctionType.Create;
  try
    lType.Flags:=lFlags;
    lToken:=GetToken;
    while lToken.TokenType <> ttCloseRoundBrace do
    begin
      lType.Parameters.Add(ParseFunctionParam);
      lToken:=ExpectToken([ttComma,ttCloseRoundBrace]);
      if lToken.TokenType = ttComma then
        lToken:=GetToken;
    end;
    lToken:=ExpectToken([ttArrow,ttSemicolon]);
    if lToken.TokenType = ttArrow then
      begin
      GetToken; // position on first token of type
      lType.ResultType:=ParseType;
      ExpectToken([ttSemicolon]);
      end;
     Result:=lType;
  except
    lType.Free;
    Raise;
  end;
  // Parse results (optional)
end;

function TWITParser.ParseFunction(aAllowStatic : boolean = false): TWITFunction;
{
  On Entry: identifier
  on exit: semicolon
}
var
  lName : string;
  lFunction: TWITFunction;
  lFunctionType : TWITFunctionType;
begin
  CheckToken(LastToken,ttIdentifier);
  lName:=LastToken.Value;
  ExpectToken(ttColon);
  GetToken;
  lFunctionType:=ParseFunctionType([ffStatic,ffAsync,ffConstructor]);
  try
    lFunction:=TWITFunction.Create(lName,lFunctionType);
    lFunctionType:=Nil;
    Result:=lFunction;
    lFunction:=Nil;
  except
    lFunctionType.Free;
    lFUnction.Free;
    Raise;
  end;
end;

function TWITParser.GetToken: TToken;
begin
  if FUnget.TokenType<>ttUnknown then
    begin
    FLastToken:=FUnGet;
    FUnGet:=Default(TToken);
    end
  else
    FLastToken:=Scanner.GetToken;
  Result:=FLastToken;
end;

procedure TWITParser.UngetToken(aToken: TToken);
begin
  if FUnGet.TokenType<>ttUnknown then
    Error(WITERR_DOUBLEUNGET,SErrDoubleUnget);
  FUnGet:=aToken;
end;

procedure TWITParser.CheckToken(constref aToken: TToken; aType: TTokenType);
begin
  if aToken.TokenType<>aType then
    Error(WITERR_EXPECTEDTOKEN,SErrExpectedTokenGot,[aType.ToString,aToken.Value]);
end;

procedure TWITParser.CheckTokens(constref aToken: TToken; aTypes: TTokenTypes);
begin
  if not (aToken.TokenType in aTypes) then
    Error(WITERR_EXPECTEDTOKENS,SErrExpectedOneOfTokensGot,[aTypes.ToString,aToken.Value]);
end;

function TWITParser.ExpectToken(aType: TTokenType): TToken;
begin
  Result:=GetToken;
  CheckToken(Result,aType);
end;

function TWITParser.ExpectToken(aTypes: TTokenTypes): TToken;
begin
  Result:=GetToken;
  CheckTokens(Result,aTypes);
end;

constructor TWITParser.create(aScanner: TWITScanner; aOwnsScanner : Boolean = false);
begin
  FOwnsScanner:=aOwnsScanner;
  FScanner:=aScanner;
end;

destructor TWITParser.destroy;
begin
  if FOwnsScanner then
    FreeAndNil(FScanner);
  inherited destroy;
end;


function TWITParser.ParseAnnotation: TWITAnnotation;
{
  On Entry : @
  On Exit : )
}
var
  lToken: TToken;
  Annotation: TWITAnnotation;
  lArgName,lArgValue : string;
begin
  CheckToken(LastToken,ttAt);
  lToken:=ExpectToken(ttIdentifier);
  Annotation:=TWITAnnotation.Create(lToken.Value);
  try
    ExpectToken(ttOpenRoundBrace);
    lToken:=GetToken;
    while (lToken.TokenType = ttIdentifier) do
        begin
        lArgName:=lToken.Value;
        lToken:=GetToken;
        if lToken.TokenType = ttEqual then
          begin
          lToken:=GetToken;
          if (lToken.TokenType in [ttStringLiteral, ttIdentifier, ttNumber]) then
            lArgValue:=lToken.Value;
          end;
        Annotation.Arguments.Add(TWITAnnotationArgument.Create(lArgName, lArgValue));
        lToken:=GetToken;
        if lToken.TokenType = ttComma then
          lToken:=GetToken
        else if lToken.TokenType = ttCloseRoundBrace then
          break
        else
          raise Exception.Create('Expected "," or ")", found: ' + lToken.Value);
        end;
  except
    Annotation.Free;
    Raise;
  end;
  Result:=Annotation;
end;

function TWITParser.ParseTypeDef : TWITTypeDef;
{
  first token: Type
  last token: semicolon
}
var
  lToken : TToken;
  aType : TTokenType;
begin
  CheckTokens(LastToken,[ttType,ttFlags,ttVariant,ttResource, ttRecord,ttEnum]);
  aType:=lastToken.TokenType;
  case aType of
    ttEnum:
      Result:=ParseEnumType;
    ttRecord:
      Result:=ParseRecordType;
    ttFlags:
      Result:=ParseFlagsType;
    ttVariant:
      Result:=ParseVariantType;
    ttResource:
      Result:=ParseResourceType;
    ttType:
      begin
      lToken:=ExpectToken(ttIdentifier);
      ExpectToken(ttEqual);
      GetToken;// Position on first token of type.
      Result:=TWITTypeDef.Create(lToken.Value,ParseType);
      ExpectToken(ttSemicolon);
      end;
  end;
end;

function TWITParser.ParseInterface(const aName : string = ''): TWITInterface;
(*
  On entry: interface
  On exit: }
*)
const
  TypeDefTokens = [ttEnum, ttResource, ttVariant, ttRecord, ttFlags, ttType, ttUse];
  InterfaceTokens = [ttAt,ttIdentifier] + TypeDefTokens;
var
  lToken: TToken;
  lInterface: TWITInterface;
  lAnnotation : TAnnotationStore;
  lFunc : TWITFunction;
  lType : TWITTypeDef;
  lUse : TWITUse;
  lName : string;
begin
  lInterface:=Nil;
  lFunc:=nil;
  lType:=nil;
  lUse:=nil;
  lAnnotation:=TAnnotationStore.Create;
  lToken:=LastToken;
  CheckToken(lToken,ttInterface);
  lName:=aName;
  if lName='' then
    begin
    lToken:=ExpectToken(ttIdentifier);
    lName:=lToken.Value;
    end;
  lInterface:=TWITInterface.Create(lName);
  try
    ExpectToken(ttOpenCurlyBrace);
    lToken:=GetToken;
    while (lToken.TokenType in InterfaceTokens) do
    begin
      Case lToken.TokenType of
      ttAt:
        begin
        lAnnotation.Add(ParseAnnotation);
        lToken:=GetToken;
        end;
      ttType,ttFlags,ttEnum,ttRecord,ttVariant,ttResource:
        begin
        lType:=ParseTypeDef;
        lInterface.AddType(lType);
        lAnnotation.Apply(lType);
        lToken:=GetToken;
        end;
      ttUse :
        begin
        lUse:=ParseUse;
        lAnnotation.Apply(lUse);
        lInterface.AddUses(lUse);
        lUse:=Nil;
        lToken:=GetToken;
        end;
      ttIdentifier:
        begin
        lFunc:=ParseFunction;
        lAnnotation.Apply(lFunc);
        lInterface.AddFunction(lFunc);
        lToken:=GetToken;
        end;
      end;
      lToken:=FLastToken;
    end;
    Result:=lInterface;
  except
    LInterface.Free;
    lAnnotation.Free;
    Raise;
  end;
  CheckToken(lToken,ttCloseCurlyBrace);
end;

function TWITParser.ParseDocument: TWITDocument;
var
  lDocument: TWITDocument;
begin
  lDocument:=TWITDocument.Create;
  try
    ParseDocument(lDocument);
    Result:=lDocument;
    lDocument:=Nil;
  finally
    lDocument.Free;
  end;
end;

procedure TWITParser.ParseDocument(aDocument: TWITDocument);
var
  Token: TToken;
  lInterface : TWITInterface;
  lAnn: TAnnotationStore;
  lWorld : TWITWorld;
  lPackage : TWITPackage;
  lUse : TWITTopLevelUse;
begin
  lAnn:=TAnnotationStore.Create;
  Scanner.SkipWhitespace:=True;
  Token:=GetToken;
  while Token.TokenType <> ttEOF do
    begin
      if Token.TokenType=ttAt then
        begin
        lAnn.Add(ParseAnnotation);
        Token:=GetToken;
        end;
      Case Token.TokenType of
      ttPackage:
        begin
        lPackage:=ParsePackage;
        lAnn.Apply(lPackage);
        aDocument.Packages.add(lPackage);
        if not lPackage.IsNested then
          aDocument.DefaultPackage:=lPackage;
        end;
      ttUse:
        begin
        lUse:=ParseTopLevelUse;
        lAnn.Apply(lUse);
        aDocument.AddUse(lUse);
        end;
      ttInterface:
        begin
        lInterface:=ParseInterface;
        aDocument.AddInterface(lInterface);
        lAnn.Apply(lInterface);
        end;
      ttWorld:
        begin
        lWorld:=ParseWorld;
        aDocument.AddWorld(lWorld);
        lAnn.Apply(lWorld);
        end;
      else
        raise Exception.Create('Unexpected token: ' + Token.Value);
      end;
      Token:=GetToken;
    end;
end;

function TWITParser.ParseUsePath(aUse: TWITUsePath; allowAs: boolean; aExtraTerminator : TTokenType = ttEOF ): String;
{
  on entry: first ID
  on exit: semicolon
}
type
  TUsePart = (upUnknown,upNamespace,upIdentifier,UpVersion,upAlias);

var
  lNameToken,lToken: TToken;
  lPart : TUsePart;
  lTerminators : Set of TTokenType;

  procedure addlastpart;
  begin
    Case lPart of
      upUnknown,
      upIdentifier : aUse.Identifier:=lNameToken.Value;
      upVersion : aUse.Version:=lNameToken.Value;
      upAlias : Result:=lNameToken.Value;
    end;
  end;

begin
  lTerminators:=[ttSemicolon,ttEOF];
  include(lTerminators,aExtraTerminator);
  lPart:=upUnknown;
  lNameToken:=LastToken;
  lToken:=GetToken;
  While not (lToken.TokenType in lTerminators) do
    begin
    Case lToken.TokenType of
     ttColon:
       begin
       aUse.AddNamespace(lNameToken.Value);
       end;
     ttDiv :
       begin
       aUse.PackageName:=lNameToken.Value;
       lPart:=upIdentifier;
       end;
     ttAt:
       begin
       if lPart<>upIdentifier then
         Error(WITERR_EXPECTEDDIV,SErrExpectedDiv);
       aUse.Identifier:=lNameToken.Value;
       lpart:=upVersion;
       end;
     ttAs:
       begin
       if not AllowAs then
         Error(WITERR_ASNOTALLOWED,SERRAsNotAllowed);
       Case lPart of
         upUnknown,
         upIdentifier,
         UpVersion : AddLastPart;
       else
         Error(WITERR_EXPECTEDDIV,SErrExpectedDiv);
       end;
       lpart:=upAlias;
       end;
    end;
    lNameToken:=ExpectToken([ttIdentifier,ttDiv,ttAt,ttColon,ttSemicolon,ttNumber]);
    if lNameToken.TokenType in [ttIdentifier,ttNumber] then
      lToken:=GetToken
    else
      lToken:=lNameToken;
    end;
  AddLastpart;
end;


function TWITParser.ParseTopLevelUse: TWITTopLevelUse;
{
  on entry: use
  on exit: semicolon
}
var
  UseStatement: TWITTopLevelUse;
  lRename : string;
begin
  CheckToken(LastToken,ttUse);
  UseStatement:=TWITTopLevelUse.Create;
  try
    ExpectToken(ttIdentifier);
    lRename:=ParseUsePath(UseStatement.Path,True);
    UseStatement.Rename:=lRename;
  except
    UseStatement.Free;
    Raise;
  end;
  Result:=UseStatement;
end;

function TWITParser.ParseUse: TWITUse;
{
  on entry: use
  on exit: semicolon
}
var
  UseStatement: TWITUse;
  lToken : TToken;
  lName,lAlias : string;

begin
  CheckToken(LastToken,ttUse);
  UseStatement:=TWITUse.Create;
  try
    ExpectToken(ttIdentifier);
    ParseUsePath(UseStatement.Path,false,ttDot);
    CheckToken(LastToken,ttDot);
    ExpectToken(ttOpenCurlyBrace);
    lToken:=GetToken;
    While not (lToken.TokenType in [ttEOF,ttCloseCurlyBrace]) do
      begin
      CheckToken(lToken,ttIdentifier);
      lAlias:='';
      lName:=lToken.Value;
      lToken:=GetToken;
      if lToken.TokenType=ttAs then
        begin
        lToken:=ExpectToken(ttIdentifier);
        lAlias:=lToken.Value;
        lToken:=GetToken;
        end;
      UseStatement.AddItem(lName,lAlias);
      CheckTokens(lToken,[ttComma,ttCloseCurlyBrace]);
      if (lToken.TokenType=ttComma) then
        lToken:=GetToken;
      end;
    CheckToken(LastToken,ttCloseCurlyBrace);
    ExpectToken(ttSemicolon);
  except
    UseStatement.Free;
    Raise;
  end;
  Result:=UseStatement;
end;

function TWITParser.ParseInclude: TWITInclude;
var
  lToken : TToken;
  lName : string;
begin
  CheckToken(LastToken,ttInclude);
  ExpectToken(ttIdentifier);
  Result:=TWITInclude.Create;
  try
    ParseUsePath(Result.Path,False,ttWith);
    if (LastToken.TokenType=ttWith) then
      begin
      ExpectToken(ttOpenCurlyBrace);
      lToken:=GetToken;
      While not (lToken.TokenType in [ttCloseCurlyBrace,ttEOF]) do
        begin
        CheckToken(lToken,ttIdentifier);
        lName:=lToken.Value;
        ExpectToken(ttAs);
        lToken:=ExpectToken(ttIdentifier);
        Result.AddItem(lName,lToken.Value);
        lToken:=ExpectToken([ttComma,ttCloseCurlyBrace]);
        if (lToken.TokenType=ttComma) then
          lToken:=GetToken;
        end;
      CheckToken(LastToken,ttCloseCurlyBrace);
      end;
  except
    Result.Free;
    Raise;
  end;
end;

function TWITParser.ParseExchange: TWITExchange;
var
  lToken : TToken;
  lName : String;
  lFunc : TWITFunctionType;
  lIntf : TWITInterface;
  LUse : TWITUsePath;
  XT : TExchangeType;
begin
  lFunc:=Nil;
  lIntf:=Nil;
  try
    CheckTokens(LastToken,[ttExport,ttImport]);
    if LastToken.TOkenType=ttExport then
      XT:=xtExport
    else
      XT:=xtImport;
    lToken:=ExpectToken(ttIdentifier);
    lName:=lToken.Value;
    lToken:=ExpectToken([ttColon,ttSemicolon]);
    if lToken.TokenType=ttSemicolon then
      begin
      Result:=TWITExchangeIdentifier.Create(XT,LName);
      end
    else
      begin
      lToken:=GetToken;
      case lToken.TokenType of
        ttIdentifier :
          begin
          lUse:=TWitUsePath.Create;
          lUse.AddNamespace(lName);
          ParseUsePath(lUse,False);
          Result:=TWITExchangeIdentifier.Create(XT,lUse);
          lUse:=nil;
          end;
        ttStatic, ttAsync, ttFunc :
          begin
          lFunc:=ParseFunctionType([ffStatic,ffAsync]);
          Result:=TWITExchangeFunc.Create(XT,lName,lFunc);
          lFunc:=nil;
          end;
        ttInterface :
          begin
          lIntf:=ParseInterface(lName);
          Result:=TWITExchangeInterface.Create(XT,lName,lIntf);
          lIntf:=nil;
          end;
      end;
      end;
  except
    lFunc.Free;
    lIntf.Free;
    lUse.Free;
    Raise;
  end;
end;

function TWITParser.ParseWorld: TWITWorld;

var
  Token: TToken;
  lWorld: TWITWorld;
  lUse : TWitUse;
  LAnn : TAnnotationStore;
  lType : TWITTypeDef;
  LExchange : TWITExchange;
  lInclude : TWITInclude;

begin
  CheckToken(LastToken,ttWorld);
  Token:=ExpectToken(ttIdentifier);
  lWorld:=TWITWorld.Create(Token.Value);
  LAnn:=TAnnotationStore.Create;
  lUse:=Nil;
  lExchange:=Nil;
  lInclude:=nil;
  try
    Token:=ExpectToken(ttOpenCurlyBrace);
    Token:=GetToken;
    while Not (Token.TokenType in [ttEOF,ttCloseCurlyBrace]) do
      begin
      case Token.TokenType of
        ttAt:
          begin
          lAnn.Add(ParseAnnotation);
          end;
        ttInclude:
          begin
          lInclude:=ParseInclude;
          lAnn.Apply(lInclude);
          lWorld.AddInclude(lInclude);
          lInclude:=Nil;
         end;
        ttImport:
          begin
          lExchange:=ParseExchange;
          lAnn.Apply(lExchange);
          lWorld.AddImport(lExchange);
          lExchange:=Nil;
          end;
        ttExport:
          begin
          lExchange:=ParseExchange;
          lAnn.Apply(lExchange);
          lWorld.AddExport(lExchange);
          lExchange:=Nil;
          end;
{        ttInterface:
          begin
          lInterface:=ParseInterface;
          MaybeGate(lInterface);
          lWorld.AddInterface(lInterface);
          end;
        ttIdentifier:
          begin
          lFunction:=ParseFunction;
          MaybeGate(lFunction);
          lWorld.AddFunction(lFunction);
          end;}
        ttUse:
          begin
          lUse:=ParseUse;
          lAnn.Apply(lUse);
          lWorld.AddUses(lUse);
          lUse:=Nil;
          end;
        ttType,ttFlags,ttEnum,ttRecord,ttVariant,ttResource:
          begin
          lType:=ParseTypeDef;
          lAnn.Apply(lType);
          lWorld.AddTypeDef(lType);
          lType:=Nil;
          end;
      else
        Error(WITERR_UNEXPECTEDTOKEN,Format(SErrUnexpected,[Token.Value]));
      end;
      Token:=GetToken;
      end;
    CheckToken(lastToken,ttCloseCurlyBrace);
  except
    lUse.Free;
    lExchange.Free;
    lAnn.Free;
    lType.Free;
    lWorld.Free;
    Raise;
  end;
  Result:=lWorld;
end;

procedure TWITParser.ParsePackageContent(aPackage: TWITPackage);
var
  Token: TToken;
  lInterface : TWITInterface;
  Gate: TWITAnnotation;
  lWorld : TWITWorld;
  lUse : TWITTopLevelUse;
begin
  aPackage.IsNested:=True;
  Token:=GetToken;
  while not (Token.TokenType in [ttEOF,ttCloseCurlyBrace]) do
    begin
      Gate:=nil;
      if Token.TokenType=ttAt then
        begin
        Gate:=ParseAnnotation;
        Token:=GetToken;
        end;
      Case Token.TokenType of
      ttUse:
        begin
        lUse:=ParseTopLevelUse;
        aPackage.UseStatements.Add(luse);
        if Gate <> nil then
          begin
          Gate.Free;
          Raise EWITParser.Create('Use statement cannot be gated');
          end;
        end;
      ttInterface:
        begin
        lInterface:=ParseInterface;
        aPackage.Interfaces.Add(lInterface);
        if Gate <> nil then
          lInterface.AddAnnotation(Gate);
        end;
      ttWorld:
        begin
        lWorld:=ParseWorld;
        aPackage.Worlds.Add(lWorld);
        if Gate <> nil then
          lWorld.AddAnnotation(Gate);
        end;
      else
        raise Exception.Create('Unexpected token: ' + Token.Value);
      end;
      Token:=GetToken;
    end;
end;

end.



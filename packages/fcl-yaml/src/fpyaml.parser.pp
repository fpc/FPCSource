{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML Parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpyaml.parser;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.StrUtils, System.DateUtils, System.TypInfo, System.Classes, System.Contnrs,
  {$ELSE}
  sysutils, strutils, dateutils, typinfo, classes, contnrs,
  {$ENDIF}
  fpyaml.scanner, fpyaml.data, fpyaml.types;

Type
  EYAMLParser = class(EYAML)
    Pos : TYAMLPos;
  end;

  { TYAMLParser }

  TYAMLParser = class(TObject)
  private
    FLastVersion : TYAMLVersion;
    FLastAnchor : TYAMLString;
    FScanner : TYAMLScanner;
    FOwnsScanner : Boolean;
    FStream : TStream;
    FMap : TFPObjectHashTable;
  protected
    procedure Error(const aMsg : String);
    procedure Error(const aFmt : String; const aArgs: array of const);
    Function Peek : TYAMLTokenData;
    Procedure ConsumeToken;
    function ConsumeAnchor: TYAMLString;
    function ConsumeVersion: TYAMLVersion;
    class function TokenToScalarKind(aToken: TYAMLToken): TYAMLScalarKind;
    function GuessTagFromScalarValue(const aValue: TYAMLString; aKind: TYAMLScalarKind): TYAMLTagType; virtual;
    Function CreateScanner(aInput : TStream) : TYAMLScanner; virtual;
    function CreateYAMLData(aClass: TYAMLDataClass; aTag : TYAMLTagType = yttCustom): TYAMLData; virtual;
    function CreateDocument : TYAMLDocument;
    function CreateTagData : TYAMLTagData;
    function CreateStream: TYAMLStream;
    function CreateSequence(aKind : TYAMLCollectionKind) : TYAMLSequence;
    function CreateMapping(aKind : TYAMLCollectionKind) : TYAMLMapping;
    function CreateScalar(const aValue : TYAMLString; aTag : TYAMLTagType): TYAMLScalar; virtual;
    procedure ParseAnchor; virtual;
    procedure ParseVersion; virtual;
    function ParseAlias: TYAMLData; virtual;
    function ParseBlockMapping: TYAMLMapping; virtual;
    function ParseFlowMapping: TYAMLMapping; virtual;
    function ParseScalar: TYAMLScalar; virtual;
    function ParseBlockSequence(SkipStart: boolean=false): TYAMLSequence; virtual;
    function ParseFlowSequence: TYAMLSequence; virtual;
    function ParseValue(aAllowBlockEntry: Boolean=false): TYAMLData; virtual;
    function ParseDocument: TYAMLDocument; virtual;
    function ParseTagDirective: TYAMLTagData; virtual;
    Property Scanner : TYAMLScanner Read FScanner;
  public
    Constructor Create(aScanner : TYAMLScanner; aOwnsScanner : Boolean = False);
    Constructor Create(aStream: TStream);
    Constructor Create(aInput: TStrings);
    Constructor Create(const aInput : array of string);
    Constructor Create(const aFileName : string);
    Destructor Destroy; override;
    Function Parse : TYAMLStream;
  end;

implementation

uses fpyaml.strings;

{ TYAMLParser }

function TYAMLParser.ConsumeAnchor : TYAMLString;

begin
  Result:=FLastAnchor;
  FLastAnchor:='';
end;


function TYAMLParser.ConsumeVersion: TYAMLVersion;

begin
  Result:=FLastVersion;
  FLastVersion:=Default(TYAMLVersion);
end;


function TYAMLParser.CreateYAMLData(aClass : TYAMLDataClass; aTag : TYAMLTagType) : TYAMLData;

var
  lAnchor:String;

begin
  lAnchor:=ConsumeAnchor;
  Result:=aClass.Create(lAnchor,YAMLTagNames[aTag]);
  if lAnchor<>'' then
    begin
    if (Result.Anchor=lAnchor) then
      FMap.Add(lAnchor,Result)
    else
      FLastAnchor:=lAnchor;
    end;
end;


function TYAMLParser.CreateScanner(aInput: TStream): TYAMLScanner;

begin
  Result:=TYAMLScanner.Create(aInput);
end;


constructor TYAMLParser.create(aScanner: TYAMLScanner; aOwnsScanner: Boolean);

begin
  FScanner:=aScanner;
  FOwnsScanner:=aOwnsScanner;
  FMap:=TFPObjectHashTable.Create(False);
end;


constructor TYAMLParser.create(aStream: TStream);

begin
  Create(CreateScanner(aStream),True);
end;


constructor TYAMLParser.create(aInput: TStrings);

begin
  FStream:=TMemoryStream.Create;
  aInput.SaveToStream(FStream);
  FStream.Position:=0;
  Create(FStream);
end;


constructor TYAMLParser.create(const aInput: array of string);

var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    L.AddStrings(aInput);
    Create(L);
  finally
    L.Free;
  end;
end;


constructor TYAMLParser.create(const aFileName: string);

begin
  FStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  Create(FStream);
end;


function TYAMLParser.GuessTagFromScalarValue(const aValue : TYAMLString; aKind : TYAMLScalarKind): TYAMLTagType;

var
  i64 : Int64;
  D : Double;

begin
  if (aValue='') then
    begin
    if aKind=yskPlain then
      Exit(yttNull)
    else
      Exit(yttString);
    end;
  if aValue[1] in ['0'..'9'] then
    begin
    if TryStrToInt64(aValue,i64) then
      Exit(yttInteger)
    else if TryISOStrToDate(aValue,D) then
      Exit(yttTimeStamp)
    else if TryStrToFloat(aValue,D,YAMLFormatSettings) then
      Exit(yttFloat)
    end;
  if IndexStr(aValue,['true','false'])<>-1 then
    Exit(yttBoolean);
  Result:=yttString;
end;


class function TYAMLParser.TokenToScalarKind(aToken : TYAMLToken) : TYAMLScalarKind;

begin
  Case aToken of
    ytScalarPlain : Result:=yskPlain;
    ytScalarSingle : Result:=yskSingle;
    ytScalarDouble : Result:=yskDouble;
    ytScalarFolded : Result:=yskFolded;
    ytScalarLiteral : Result:=yskLiteral;
  end;
end;


function TYAMLParser.ParseScalar : TYAMLScalar;

var
  lToken : TYAMLTokenData;
  lValue : TYAMLString;
  lTag : TYAMLTagType;
  lKind : TYAMLScalarKind;

begin
  lToken:=Peek;
  lValue:=lToken.value;
  lKind:=TokenToScalarKind(lToken.Token);
  lTag:=GuessTagFromScalarValue(lValue,lKind);
  Result:=TYAMLScalar(CreateYAMLData(TYAMLScalar,lTag));
  Result.Kind:=lKind;
  Result.Value:=lValue;
  ConsumeToken;
end;


function TYAMLParser.ParseValue(aAllowBlockEntry : Boolean = false) : TYAMLData;
// On entry, we're on the first token of the value
// On exit, were on EOF or the first token after the value
var
  lToken : TYAMLTokenData;

begin
  lToken:=Peek;
  case lToken.Token of
    ytAnchor : ParseAnchor;
    ytAlias : Result:=ParseAlias;
    ytBlockEntry :
      {
        key:
        - item 1
        - item 2
      }
      if aAllowBlockEntry then
        Result:=ParseBlockSequence(True)
      else
        Error(SErrUnexpectedToken,[lToken.Token.ToString,lToken.Value]);
    ytBlockSequenceStart : Result:=ParseBlockSequence;
    ytFlowSequenceStart : Result:=ParseFlowSequence;
    ytBlockMappingStart : Result:=ParseBlockMapping;
    ytFlowMappingStart : Result:=ParseFlowMapping;
    ytScalarPlain,
    ytScalarSingle,
    ytScalarDouble,
    ytScalarFolded,
    ytScalarLiteral : Result:=ParseScalar;
  else
    Error(SErrUnexpectedToken,[lToken.Token.ToString,lToken.Value])
  end;
end;


function TYAMLParser.ParseFlowSequence : TYAMLSequence;
// On entry, we're on the first token of the sequence: ytFlowSequenceStart.
// On exit, were on EOF or the first token after the sequence end: ytFlowSequenceEnd;
var
  lToken : TYAMLTokenData;

begin
  Result:=CreateSequence(yckFlow);
  try
    ConsumeToken;
    lToken:=Peek;
    While not (Ltoken.token in [ytEOF,ytFlowSequenceEnd]) do
      begin
      Result.Add(ParseValue);
      lToken:=Peek;
      if lToken.token=ytFlowEntry then
        begin
        ConsumeToken;
        lToken:=Peek;
        end;
      end;
    If Ltoken.token=ytFlowSequenceEnd then
      ConsumeToken;
  except
    Result.Free;
    Raise;
  end;
end;


function TYAMLParser.ParseBlockSequence(SkipStart : boolean = false) : TYAMLSequence;
// On entry, we're on the first token of the sequence: ytBlockSequenceStart.
// On exit, were on EOF or the first token after the sequence end: yt(Block|Flow)SequenceEnd;

var
  lToken : TYAMLTokenData;

begin
  Result:=CreateSequence(yckBlock);
  try
    if not SkipStart then
      ConsumeToken;
    lToken:=Peek;
    While (Ltoken.token=ytBlockEntry) do
      begin
      ConsumeToken;
      Result.Add(ParseValue);
      lToken:=Peek;
      end;
    // When we're indentless, encountering a block end means we finish the enclosing block, so do not consume it...
    If (Ltoken.token=ytBlockEnd) and Not SkipStart then
      ConsumeToken;
  except
    Result.Free;
    Raise;
  end;
end;


function TYAMLParser.ParseBlockMapping: TYAMLMapping;
// On entry, we're on the first token of the mapping: ytBlockMappingStart.
// On exit, were on EOF or the first token after the mapping end: ytBlockEnd;

var
  lToken : TYAMLTokenData;
  lKey,lValue: TYAMLData;
  lCount : Integer;

begin
  lCount:=0;
  Result:=CreateMapping(yckBlock);
  try
    ConsumeToken;
    lToken:=Peek;
    While (Ltoken.token=ytKey) do
      begin
      ConsumeToken;
      lKey:=ParseValue;
      lToken:=Peek;
      if lToken.Token<>ytValue then
        Error(SErrUnexpectedToken,[lToken.token.ToString,lToken.Value]);
      consumeToken;
      lValue:=ParseValue(true);
      Result.Add(lKey,lValue);
      lToken:=Peek;
      Inc(lCount);
      end;
    If Ltoken.token=ytBlockEnd then
      ConsumeToken
    else If Ltoken.token<>ytEOF then
      Error(SErrUnexpectedToken,[lToken.token.ToString,lToken.Value]);
  except
    Result.Free;
    Raise;
  end;
end;


function TYAMLParser.ParseFlowMapping: TYAMLMapping;
// On entry, we're on the first token of the mapping: ytBlockMappingStart.
// On exit, were on EOF or the first token after the mapping end: ytFlowMappingEnd

var
  lToken : TYAMLTokenData;
  lKey,lValue: TYAMLData;

begin
  Result:=CreateMapping(yckFlow);
  try
    ConsumeToken;
    lToken:=Peek;
    While (Ltoken.token=ytKey) do
      begin
      ConsumeToken;
      lKey:=ParseValue;
      lToken:=Peek;
      if lToken.Token<>ytValue then
        Error(SErrUnexpectedToken,[lToken.token.ToString,lToken.Value]);
      consumeToken;
      lValue:=ParseValue;
      Result.Add(lKey,lValue);
      lToken:=Peek;
      if lToken.Token=ytFlowEntry then
        begin
        ConsumeToken;
        lToken:=Peek;
        end;
      end;
    If Ltoken.token=ytFlowMappingEnd then
      ConsumeToken
    else If Ltoken.token<>ytEOF then
      Error(SErrUnexpectedToken,[lToken.token.ToString,lToken.Value]);
  except
    Result.Free;
    Raise;
  end;
end;


function TYAMLParser.ParseAlias : TYAMLData;

begin
  //
end;


function TYAMLParser.ParseDocument : TYAMLDocument;

// On entry, we're on the first token of the document: this may be the document start token or a value.
// On exit, were on EOF or the first token after the document end.
var
  lToken : TYAMLTokenData;

begin
  Result:=CreateDocument;
  try
    lToken:=Peek;
    if lToken.Token=ytDocumentStart then
      begin
      ConsumeToken;
      lToken:=Peek;
      end;
    While not (lToken.token in [ytEOF,ytDocumentEnd]) do
      begin
      case lToken.Token of
        ytAnchor : ParseAnchor;
        ytAlias : Result.Add(ParseAlias);
      else
        Result.Add(ParseValue);
      end;
      lToken:=Peek;
      end;
    if lToken.token=ytDocumentEnd then
      ConsumeToken;
  except
    Result.Free;
    Raise;
  end;
end;


function TYAMLParser.CreateDocument : TYAMLDocument;

begin
  Result:=TYAMLDocument(CreateYAMLData(TYAMLDocument));
  Result.Version:=ConsumeVersion;
end;


function TYAMLParser.CreateTagData: TYAMLTagData;
begin
  Result:=TYAMLTagData(CreateYAMLData(TYAMLTagData));
end;


function TYAMLParser.CreateStream : TYAMLStream;

begin
  Result:=TYAMLStream(CreateYAMLData(TYAMLStream));
end;


function TYAMLParser.CreateSequence(aKind: TYAMLCollectionKind): TYAMLSequence;

begin
  Result:=TYAMLSequence(CreateYAMLData(TYAMLSequence));
  Result.Kind:=aKind;
end;


function TYAMLParser.CreateMapping(aKind: TYAMLCollectionKind): TYAMLMapping;

begin
  Result:=TYAMLMapping(CreateYAMLData(TYAMLMapping));
  Result.Kind:=aKind;
end;


function TYAMLParser.CreateScalar(const aValue: TYAMLString; aTag: TYAMLTagType): TYAMLScalar;

begin
  Result:=TYAMLScalar(CreateYAMLData(TYAMLScalar,aTag));
  Result.Value:=aValue;
end;


procedure TYAMLParser.ParseAnchor;
// On entry, we're on the anchor token.
// On exit, we're on the token after the anchor
begin
  if FLastAnchor<>'' then
    Error(SErrDoubleAnchor,[Peek.value,FLastAnchor]);
  FLastAnchor:=Peek.value;
  ConsumeToken;
end;


procedure TYAMLParser.ParseVersion;
// On entry, we're on the version token.
// On exit, we're on the token after the directive
var
  lNew : TYAMLVersion;

begin
  lNew.Major:=StrToIntDef(Peek.value,0);
  lNew.Minor:=StrToIntDef(Peek.value2,0);
  if (FLastVersion.ToString<>'0.0') then
    Error(SErrDoubleVersion,[lNew.ToString,FLastVersion.ToString]);
  FLastVersion:=lNew;
  ConsumeToken;
end;


function TYAMLParser.ParseTagDirective : TYAMLTagData;
// On entry, we're on the directive token.
// On exit, we're on the token after the directive
begin
  Result:=TYAMLTagData(CreateYAMLData(TYAMLTagData));
  ConsumeToken;
end;


procedure TYAMLParser.Error(const aMsg: String);

var
  Err : EYAMLParser;

begin
  Err:=EYAMLParser.Create(aMsg);
  Err.Pos:=Peek.beginpos;
  Raise Err;
end;


procedure TYAMLParser.Error(const aFmt: String; const aArgs: array of const);

begin
  Error(Format(aFmt,aArgs));
end;


function TYAMLParser.Peek: TYAMLTokenData;

begin
  Result:=Scanner.Peek;
end;


procedure TYAMLParser.ConsumeToken;
begin
  Scanner.ConsumeToken;
end;


function TYAMLParser.Parse: TYAMLStream;

var
  lToken : TYAMLTokenData;
  lDone : Boolean;

begin
  lDone:=False;
  Result:=CreateStream;
  try
    Repeat
      lToken:=Peek;
      Case lToken.token of
        ytAnchor : ParseAnchor;
        ytAlias : Error(SErrAliasNotAllowed);
        ytScalarDouble,
        ytScalarSingle,
        ytScalarFolded,
        ytScalarLiteral,
        ytScalarPlain,
        ytBlockMappingStart,
        ytBlockSequenceStart,
        ytFlowSequenceStart,
        ytFlowMappingStart,
        ytDocumentStart : Result.Add(ParseDocument);
        ytVersionDirective : ParseVersion;
        ytTagDirective : Result.Add(ParseTagDirective);
        ytEOF: lDone:=True;
      else
        Error(SErrUnexpectedToken,[lToken.Token.ToString,lToken.value]);
      end;
    until lDone;
  except
    Result.Free;
    Raise;
  end;
end;


destructor TYAMLParser.Destroy;

begin
  FreeAndNil(FMap);
  FreeAndNil(FStream);
  if FOwnsScanner then
    FreeAndNil(FScanner);
  inherited Destroy;
end;

end.


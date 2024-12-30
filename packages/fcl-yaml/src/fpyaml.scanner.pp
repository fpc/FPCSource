{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML scanner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit takes some ideas from the libYAML C scanner:
  https://github.com/yaml/libyaml/
}
unit fpyaml.scanner;

{$mode ObjFPC}
{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.TypInfo, System.Classes, Fcl.Streams.Extra, System.Types, fpyaml.types;
{$ELSE}
uses sysutils, TypInfo, classes, streamex, types, fpyaml.types;
{$ENDIF}



type
  TYAMLString = AnsiString;

  TYAMLPos = record
    Line: Cardinal;
    Column: Cardinal;
    Constructor create(const aLine, aColumn: Cardinal);
  end;
  { TYAMLKey }

  TYAMLKey = record
    Possible: Boolean;
    Required: Boolean;
    TokenNumber: Cardinal;
    Position : TYAMLPos;
    constructor Create (aPossible, aRequired : Boolean; aTokenNumber : Cardinal);
    function CheckPossibleAt(at : TYAMLPos) : Boolean;
  end;
  PYAMLKey = ^TYAMLKey;

  TYAMLKeyArray = Array of TYAMLKey;

  TYAMLToken = (
    ytNoToken,
    ytEOF,

    ytVersionDirective,
    ytTagDirective,
    ytDocumentStart,
    ytDocumentEnd,

    ytAlias,
    ytAnchor,
    ytTag,

    ytScalarPlain,
    ytScalarLiteral,
    ytScalarFolded,
    ytScalarSingle,
    ytScalarDouble,

    ytBlockSequenceStart,
    ytBlockMappingStart,
    ytBlockEntry,
    ytBlockEnd,

    ytFlowSequenceStart,
    ytFlowSequenceEnd,
    ytFlowMappingStart,
    ytFlowMappingEnd,
    ytFlowEntry,

    ytKey,
    ytValue);

  { TYAMLTokenHelper }

  TYAMLTokenHelper = type helper for TYAMLToken
    function ToString : String;
  end;

  { TYAMLTokenData }

  TYAMLTokenData = record
    token : TYAMLToken;
    beginpos,
    endpos : TYAMLPos;
    value : TYAMLString;
    value2 : TYAMLString;
    constructor Create(aToken : TYAMLToken; aBegin,aEnd : TYAMLPos; aValue : TYAMLString=''; aValue2 : TYAMLString='');
    constructor Create(aToken : TYAMLToken; aPos : TYAMLPos);
  end;
  TYAMLTokenDataArray = Array of TYAMLTokenData;

  { EYAMLScannerError }

  EYAMLScanner = class(EYAML)
    FPos: TYAMLPos;
  public
    constructor Create(const aPos: TYAMLPos; const aMessage: TYAMLString); overload;
    property Position : TYAMLPos Read FPos;
  end;


  { TYAMLScanner }

  TYAMLScanner = class
  private
    FCurrRow : Integer;
    FCurrCol : Integer;
    FCurrLine : TYAMLString;
    FCurrLineLen : Integer;
    FCurrIndent: Integer;
    FAllowKey: Boolean;
    FSkipAllowKeyCheck : Boolean;
    FFlowLevel: Integer;
    FConsumedTokens: Integer;
    FReader: TStreamReader;
    // Queue
    FTokens: TYAMLTokenDataArray;
    FTokenCount : Integer;
    // Stack
    FKeys: TYAMLKeyArray;
    FKeyCount : Integer;
    // Stack
    FIndents: TIntegerDynArray;
    FIndentCount: Integer;
  Protected
    // Error handling
    procedure Error(const aPos: TYAMLPos; const aMessage: TYAMLString); virtual;
    procedure Error(const aPos: TYAMLPos; const aFmt: TYAMLString; const aArgs : Array of const); virtual;
    // Character stream handling
    function GetEscapeChar: TYAMLString;
    function ReadLine : Boolean;
    function SkipWhiteSpace : boolean;
    Function CurrPos : TYAMLPos;
    function GetCurrChar: AnsiChar;
    function GetNextChar: AnsiChar;
    function IsDocumentStart: Boolean;
    function IsDocumentEnd: Boolean;
    function IsCurrAlpha: Boolean;
    function IsCurrDigit: Boolean;
    function IsCurrBlank(AllowEOF: boolean=True): Boolean;
    function IsCurrSpace: Boolean;
    function IsNextBlank: Boolean;
    Function IsEOL : Boolean;
    // Actual sacanning
    function ScanDirective: TYAMLTokenData;
    function ScanAnchorOrAlias(ATokenType: TYAMLToken): TYAMLTokenData;
    function ScanTag: TYAMLTokenData;
    function ScanDirectiveName(aPos: TYAMLPos): TYAMLString;
    procedure ScanVersionDirectiveValue(aPos: TYAMLPos; out AMajor: Integer; out AMinor: Integer);
    procedure ScanTagDirectiveValue(aPos: TYAMLPos; out AHandle: TYAMLString; out APrefix: TYAMLString);
    function ScanTagHandle(aIsDirective: Boolean; aPos: TYAMLPos): TYAMLString;
    function ScanTagUri(AIsVerbatim: Boolean; AIsDirective: Boolean; AHead: TYAMLString; aPos: TYAMLPos): TYAMLString;
    function ScanUriEscapes(aIsDirective: Boolean; aPos: TYAMLPos): TYAMLString;
    function ScanBlockScalar(aFolded: Boolean): TYAMLTokenData;
    function ScanFlowScalar(AIsSingle: Boolean): TYAMLTokenData;
    function ScanPlainScalar: TYAMLTokenData;
    procedure HandleBlockScalarWhiteSpace(var AIndent: Integer; var ABreaks: TYAMLString; aPos: TYAMLPos; out aPos2: TYAMLPos);
    procedure DoFold(var aValue, aLeading, aTrailing: TYAMLString);
    // Indent stack
    function PopIndent: Integer;
    procedure PushIndent(aIndent: Integer);
    procedure Indent(AColumn: Integer; ANumber: Integer; ATokenType: TYAMLToken; aPos: TYAMLPos);
    procedure Undent(AColumn: Integer);
    // Key stack
    procedure PushKey(aKey: TYAMLKey);
    function PeekKey: PYAMLKey;
    procedure PopKey;
    function CheckMoreTokensNeeded: Boolean;
    procedure CheckPossibleKeys;
    procedure RemoveSimpleKey;
    procedure SaveSimpleKey;
    // Token handling
    procedure MaybeGrowTokens;
    function FirstToken: TYAMLTokenData;
    procedure QueueToken(aToken: TYAMLTokenData);
    procedure InsertToken(aAtPos : Integer; aToken: TYAMLTokenData);
    procedure FetchMoreTokens;
    function FetchNextToken : Boolean;
    function RemoveFirstToken: TYAMLTokenData;
    procedure IncreaseFlowLevel;
    procedure DecreaseFlowLevel;
    procedure HandleStreamStart;
    procedure HandleStreamEnd;
    procedure HandleDirective;
    procedure HandleDocumentIndicator(ATokenType: TYAMLToken);
    procedure HandleFlowCollectionStart(ATokenType: TYAMLToken);
    procedure HandleFlowCollectionEnd(ATokenType: TYAMLToken);
    procedure HandleFlowEntry;
    procedure HandleBlockEntry;
    procedure HandleKey;
    procedure HandleValue;
    procedure HandleAnchorOrAlias(ATokenType: TYAMLToken);
    procedure HandleTag;
    procedure HandleBlockScalar(aFolded: Boolean);
    procedure HandleFlowScalar(aSingle: Boolean);
    procedure HandlePlainScalar;
  public
    constructor Create(aInput: TStream);
    destructor Destroy; override;
    procedure ConsumeToken;
    function Peek: TYAMLTokenData;
    Function GetNextToken : TYAMLTokenData;
    function InFlowContext : Boolean;
  end;

implementation

uses fpyaml.strings;

const
  MaxVersionDigitLength = 9;
  TabChar = #9;
  WhiteSpace = [#32, #9];
  AlphaChars = ['A'..'Z', 'a'..'z', '0'..'9'];
  DigitChars = ['0'..'9'];
  NotScalarStartChars = ['-', '?', ':', ',', '[', ']', '{', '}', '#', '&', '*', '!', '|', '>', '''', '"', '%', '@', '`' ];
  TagURIChars = [';', '/', '?', ':', '@', '&', '=', '+', '$', '.', '%', '!', '~', '*', '''', '(', ')'];
  HexChars = ['A'..'F','a'..'f','0'..'9'];
  FlowIndicatorChars = [',', '[', ']', '{', '}'];
  DirectiveNames : Array[boolean] of string = ('tag','directive');

{ TYAMLPos }

constructor TYAMLPos.create(const aLine, aColumn : Cardinal);

begin
  Line:=aLine;
  Column:=aColumn;
end;

{ TYAMLKey }

constructor TYAMLKey.Create(aPossible, aRequired: Boolean; aTokenNumber: Cardinal);

begin
  Possible:=aPossible;
  Required:=aRequired;
  TokenNumber:=aTokenNumber;
end;


function TYAMLKey.CheckPossibleAt(at: TYAMLPos): Boolean;

begin
  Result:=(Position.Line=at.Line) and ((at.Column-Position.Column) <= 1024);
end;

{ TYAMLTokenHelper }

function TYAMLTokenHelper.ToString: String;
begin
  Result:=GetEnumName(TypeInfo(TYAMLToken),Ord(Self))
end;


{ TYAMLTokenData }

constructor TYAMLTokenData.Create(aToken: TYAMLToken; aBegin, aEnd: TYAMLPos; aValue: TYAMLString; aValue2 : TYAMLString='');

begin
  Token:=aToken;
  BeginPos:=aBegin;
  EndPos:=aEnd;
  Value:=aValue;
  Value2:=aValue2;
end;


constructor TYAMLTokenData.Create(aToken: TYAMLToken; aPos: TYAMLPos);

begin
  Token:=aToken;
  BeginPos:=aPos;
  EndPos:=aPos;
  Value:='';
end;

{ EYAMLScannerError }

constructor EYAMLScanner.Create(const aPos: TYAMLPos; const aMessage: TYAMLString);

begin
  inherited Create(AMessage);
  FPos:=aPos;
end;


{ TYAMLScanner }

constructor TYAMLScanner.Create(aInput: TStream);

begin
  FReader:=TStreamReader.Create(aInput,4096,False);
  SetLength(FTokens,100);
  SetLength(FKeys,100);
  SetLength(FIndents,100);

  FTokenCount:=0;
  FConsumedTokens:=0;
  FFlowLevel:=0;
  FCurrIndent:=0;
  HandleStreamStart;
end;

destructor TYAMLScanner.Destroy;

begin
  FReader.Free;
  inherited Destroy;
end;


function TYAMLScanner.CurrPos: TYAMLPos;

begin
  Result:=TYAMLPos.Create(FCurrRow,FCurrCol);
end;


function TYAMLScanner.IsNextBlank : Boolean;

begin
  Result:=(FCurrCol=FCurrLineLen) or (FCurrLine[FCurrCol+1] in WhiteSpace);
end;


function TYAMLScanner.IsEOL: Boolean;

begin
  Result:=FCurrCol>FCurrLineLen;
end;


function TYAMLScanner.IsCurrBlank(AllowEOF: boolean=True) : Boolean;

begin
  if AllowEOF and (FCurrCol>FCurrLineLen) then
    Result:=True
  else
    Result:=(FCurrCol<=FCurrLineLen) and (FCurrLine[FCurrCol] in WhiteSpace);
end;


function TYAMLScanner.IsCurrSpace: Boolean;

begin
  Result:=(FCurrCol<=FCurrLineLen) and (FCurrLine[FCurrCol]=' ');
end;


function TYAMLScanner.IsDocumentStart : Boolean;

begin
  Result:=(FCurrLineLen>=3)
          and (FCurrLine[1]='-')
          and (FCurrLine[2]='-')
          and (FCurrLine[3]='-')
          And ((FCurrLineLen=3) or (FCurrLine[4]=' '));
end;


function TYAMLScanner.IsDocumentEnd : Boolean;

begin
  Result:=(FCurrLineLen>=3)
          and (FCurrLine[1]='.')
          and (FCurrLine[2]='.')
          and (FCurrLine[3]='.')
          and ((FCurrLineLen=3) or (FCurrLine[4]=' '));
end;


procedure TYAMLScanner.IncreaseFlowLevel;

var
  lKey: TYAMLKey;

begin
  lKey:=Default(TYAMLKey);
  PushKey(lKey);
  Inc(FFlowLevel);
end;


procedure TYAMLScanner.DecreaseFlowLevel;

begin
  if (FFlowLevel > 0) then
    begin
    Dec(FFlowLevel);
    PopKey;
    end;
end;


procedure TYAMLScanner.Error(const aPos: TYAMLPos; const aMessage: TYAMLString);

begin
  raise EYAMLScanner.Create(aPos,aMessage);
end;


procedure TYAMLScanner.Error(const aPos: TYAMLPos; const aFmt: TYAMLString; const aArgs: array of const);

begin
  Error(aPos,Format(aFmt,aArgs));
end;


function TYAMLScanner.RemoveFirstToken: TYAMLTokenData;
var
  i : integer;
begin
  Result:=FTokens[0];
  Dec(FTokenCount);
  For I:=0 to FTokenCount-1 do
   FTokens[i]:=FTokens[I+1];
end;


function TYAMLScanner.FirstToken: TYAMLTokenData;

begin
  if FTokenCount=0 then
    Result:=TYAMLTokenData.Create(ytEOF,CurrPos)
  else
    Result:=FTokens[0];
end;


function TYAMLScanner.Peek: TYAMLTokenData;

begin
  FetchMoreTokens;
  Result:=FirstToken;
end;


function TYAMLScanner.GetNextToken: TYAMLTokenData;

begin
  Result:=Peek;
  ConsumeToken;
end;

function TYAMLScanner.InFlowContext: Boolean;
begin
  Result:=(FFlowLevel>0);
end;


procedure TYAMLScanner.ConsumeToken;

begin
  Inc(FConsumedTokens);
  RemoveFirstToken;
end;


procedure TYAMLScanner.FetchMoreTokens;

var
  lMore: Boolean;
  L : Integer;
begin
  l:=0;
  Repeat
    inc(l);
    // Check if we really need to get more tokens.
    lMore:=FTokenCount=0;
    if not lMore then
      begin
      CheckPossibleKeys;
      lMore:=CheckMoreTokensNeeded;
      end;
    if lMore then
      lMore:=FetchNextToken;
  until not lMore;
end;

function TYAMLScanner.CheckMoreTokensNeeded: Boolean;
// Check if we need more tokens to determine a key
var
  lKey: PYAMLKey;
  I : Integer;

begin
  Result:=False;
  I:=0;
  While (Not Result) and (i<FKeyCount) do
    begin
    lKey:=@FKeys[i];
    With lKey^ do
      begin
      Result:=(Possible and (TokenNumber=FConsumedTokens));
      end;
    inc(I);
    end;
end;

procedure TYAMLScanner.CheckPossibleKeys;

var
  i : Integer;
  lKey: PYAMLKey;

begin
  For I:=0 to FKeyCount-1 do
    begin
    lKey:=@FKeys[i];
    { a simple key is limited to a single line and has at most 1024 chars }
    if lKey^.Possible and Not lKey^.CheckPossibleAt(CurrPos) then
      begin
      if (lKey^.Required) then
        Error(lkey^.Position,SErrMissingColonInKey);
      lKey^.Possible:=False;
      end;
    end;
end;


function TYAMLScanner.FetchNextToken: Boolean;

var
  lDone : Boolean;
  C : AnsiChar;

begin
  Result:=True;
  if not ReadLine then
    begin
    HandleStreamEnd;
    exit(False);
    end;
  if not SkipWhiteSpace then
    begin
    HandleStreamEnd;
    exit;
    end;
  CheckPossibleKeys;
  Undent(FCurrCol);
  lDone:=FCurrCol=1;
  if lDone then
    begin
    C:=FCurrLine[FCurrCol];
    Case C of
    '%' : HandleDirective;
    '-' : begin
          lDone:=IsDocumentStart;
          if lDone then
            HandleDocumentIndicator(ytDocumentStart)
          end;
    '.' : begin
          lDone:=IsDocumentEnd;
          if lDone then
            HandleDocumentIndicator(ytDocumentEnd);
          end;
    else
      lDone:=False;
    end;
    end;
  if lDone then
    exit;
  lDone:=True;
  C:=FCurrLine[FCurrCol];
  Case C of
  '[': HandleFlowCollectionStart(ytFlowSequenceStart);
  '{': HandleFlowCollectionStart(ytFlowMappingStart);
  ']': HandleFlowCollectionEnd(ytFlowSequenceEnd);
  '}': HandleFlowCollectionEnd(ytFlowMappingEnd);
  ',': HandleFlowEntry;
  '*': HandleAnchorOrAlias(ytAlias);
  '&': HandleAnchorOrAlias(ytAnchor);
  '!': HandleTag;
  '''': HandleFlowScalar(True);
  '"' : HandleFlowScalar(False);
  '-': begin
       if IsNextBlank then
         HandleBlockEntry
       else
         HandlePlainScalar
       end;
  '?': begin
       if (FFlowLevel>0) or IsNextBlank then
         HandleKey
       else
         HandlePlainScalar;
       end;
  ':': begin
       if (FFlowLevel>0) or IsNextBlank then
         HandleValue
       else
         HandlePlainScalar;
       end;
  '|': begin
       lDone:=(FFlowLevel=0);
       if lDone then
          HandleBlockScalar(False);
       end;
  '>': begin
       lDone:=(FFlowLevel=0);
       if lDone then
         HandleBlockScalar(True);
       end;
  else
    lDone:=False;
  end;
  if lDone then
    exit;
  if (not IsCurrBlank) and not (FCurrLine[FCurrCol] in NotScalarStartChars) then
    begin
    HandlePlainScalar;
    exit;
    end;
  Error(CurrPos, SErrUnknownCharacter);
end;


function TYAMLScanner.SkipWhiteSpace : boolean;

begin
  // Skip whitespace.
  // Handle tabs: they are not allowed in block mode at start of line or after '-' '?' ':'
  Result:=False;
  // if we're past the end of the line, it means we start a new line, and a simple key is allowed.
  if (FFlowLevel=0) and (FCurrCol>FCurrLineLen) then
    FAllowKey:=True;
  while ReadLine do
    begin
    While (FCurrCol<=FCurrLineLen) do
      Case FCurrLine[FCurrCol] of
      ' ':
         Inc(FCurrCol);
      #9:
        if (FFlowLevel > 0) or (not FAllowKey) then
          Inc(FCurrCol)
        else
          Exit(True);
      '#':
        FCurrCol:=FCurrLineLen+1;
      else
        Exit(True);
      end;
     // If we get here, we treated the whole line. In block mode, we can have a key now.
    if (FFlowLevel=0) then
      FAllowKey:=True;
    end;
end;


procedure TYAMLScanner.MaybeGrowTokens;

Var
  lLen: Integer;

begin
  lLen:=Length(FTokens);
  if (FTokenCount=lLen) then
    SetLength(FTokens,lLen+10);
end;


procedure TYAMLScanner.QueueToken(aToken : TYAMLTokenData);

begin
  MaybeGrowTokens;
  FTokens[FTokenCount]:=aToken;
  Inc(FTokenCount);
end;


procedure TYAMLScanner.InsertToken(aAtPos: Integer; aToken: TYAMLTokenData);

var
  I : Integer;

begin
  MaybeGrowTokens;
  For I:=FTokenCount downto aAtpos+1 do
    FTokens[i]:=FTokens[i-1];
  FTokens[aAtPos]:=aToken;
  Inc(FTokenCount);
end;


procedure TYAMLScanner.Undent(AColumn: Integer);

begin
  if (FFlowLevel > 0) then
    Exit;
  while {(FCurrIndent<>-1) and} (FCurrIndent > AColumn-1) do
    begin
    QueueToken(TYAMLTokenData.Create(ytBlockEnd,CurrPos));
    FCurrIndent:=PopIndent;
    end;
end;


function TYAMLScanner.PopIndent: Integer;

begin
  if FIndentCount=0 then
    Result:=-1
  else
    begin
    Dec(FIndentCount);
    Result:=FIndents[FIndentCount];
    end;
end;

procedure TYAMLScanner.PushIndent(aIndent : Integer);

var
  lLen : Integer;

begin
  lLen:=Length(FIndents);
  if (FIndentCount=lLen) then
    SetLength(FIndents,lLen+10);
  FIndents[FIndentCount]:=aIndent;
  Inc(FIndentCount);
end;


procedure TYAMLScanner.Indent(AColumn: Integer; ANumber: Integer; ATokenType: TYAMLToken; aPos: TYAMLPos);

var
  lToken: TYAMLTokenData;

begin
  if (FFlowLevel > 0) then
    Exit;
  if (FCurrIndent>=AColumn-1) then
    Exit;
  PushIndent(FCurrIndent);
  FCurrIndent:=aColumn-1;
  //* Create a token and insert it into the queue. */
  Case ATokenType of
  ytBlockMappingStart,
  ytBlockSequenceStart:
     lToken:=TYAMLTokenData.Create(aTokenType, aPos);
  else
    raise EYAMLScanner.Create(aPos,'Unexpected token for indent');
  end;
  if (ANumber=-1) then
    QueueToken(lToken)
  else
    InsertToken(ANumber-FConsumedTokens,lToken);
end;


function TYAMLScanner.PeekKey: PYAMLKey;

begin
  if FKeyCount>0 then
    Result:=@FKeys[FKeyCount-1]
  else
    raise EYAMLScanner.Create(SErrNoSimpleKeyAvailable);
end;


procedure TYAMLScanner.RemoveSimpleKey;

var
  lKey: PYAMLKey;

begin
  lKey:=PeekKey;
  if (lKey^.Possible) and (lKey^.Required) then
    Error(lKey^.Position,SErrMissingColonInKey);
  lKey^.Possible:= False;
end;


procedure TYAMLScanner.SaveSimpleKey;

var
  lKey: PYAMLKey;
  lIsRequired: Boolean;

begin
  // in blockcontext, a key is required if the curr column is the indent level.
  // Note that indent is 0 based, CurrCol is 1 based.
  lIsRequired:=(FFlowLevel=0) and (FCurrIndent=(FCurrCol-1));
  if (FAllowKey) then
    begin
    RemoveSimpleKey;
    lKey:=PeekKey;
    lKey^.Possible:=True;
    lKey^.Required:=lIsRequired;
    lKey^.TokenNumber:=FConsumedTokens + FTokenCount;
    lKey^.Position:=CurrPos;
    end;
end;


procedure TYAMLScanner.PushKey(aKey : TYAMLKey);

var
  lLen : Integer;

begin
  lLen:=Length(FKeys);
  if FKeyCount=lLen then
    SetLength(FKeys,lLen+10);
  FKeys[FKeyCount]:=aKey;
  Inc(FKeyCount);
end;


procedure TYAMLScanner.PopKey;

begin
  If FKeyCount>0 then
    Dec(FKeyCount);
end;


procedure TYAMLScanner.HandleStreamStart;

var
  lKey: TYAMLKey;

begin
  lKey:=Default(TYAMLKey);
  FCurrIndent:=-1;
  PushKey(lKey);
  FAllowKey:=True;
end;


procedure TYAMLScanner.HandleStreamEnd;

begin
  Undent(0);
  RemoveSimpleKey;
  FAllowKey:=False;
  QueueToken(TYAMLTokenData.Create(ytEOF,CurrPos));
end;


procedure TYAMLScanner.HandleDirective;

begin
  Undent(0);
  RemoveSimpleKey;
  FAllowKey:=False;
  QueueToken(ScanDirective);
end;


procedure TYAMLScanner.HandleDocumentIndicator(ATokenType: TYAMLToken);

var
  lPos1,lPos2: TYAMLPos;

begin
  Undent(0);
  RemoveSimpleKey;
  FAllowKey:=False;
  lPos1:=CurrPos;
  Inc(FCurrCol,3);
  lPos2:=CurrPos;
  Case aTokenType of
    ytDocumentStart,
    ytDocumentEnd:
      QueueToken(TYAMLtokenData.Create(aTokenType,lPos1,lPos2));
  else
    raise EYAMLScanner.Create(lPos1,'unexpected token type');
  end;
end;


procedure TYAMLScanner.HandleFlowCollectionStart(ATokenType: TYAMLToken);

var
  lPos1,lPos2: TYAMLPos;

begin
  SaveSimpleKey;
  IncreaseFlowLevel;
  FAllowKey:=True;
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lPos2:=CurrPos;
  Case ATokenType of
  ytFlowSequenceStart,
  ytFlowMappingStart:
    QueueToken(TYAMLTokenData.Create(aTokenType,lPos1,lPos2))
  else
    raise EYAMLScanner.Create(lPos1,'unexpected token type');
  end;
end;


procedure TYAMLScanner.HandleFlowCollectionEnd(ATokenType: TYAMLToken);

var
  lPos1,lPos2: TYAMLPos;

begin
  RemoveSimpleKey;
  DecreaseFlowLevel;
  FAllowKey:=False;
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lPos2:=CurrPos;
  Case ATokenType of
  ytFlowSequenceEnd,
  ytFlowMappingEnd:
    QueueToken(TYAMLTokenData.Create(aTokenType,lPos1,lPos2));
  else
    raise EYAMLScanner.Create(lPos1,'unexpected token type');
  end;
end;


procedure TYAMLScanner.HandleFlowEntry;

var
  lPos1,lPos2: TYAMLPos;

begin
  RemoveSimpleKey;
  FAllowKey:=True;
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lPos2:=CurrPos;
  QueueToken(TYAMLTokenData.Create(ytFlowEntry,lPos1,lPos2));
end;


procedure TYAMLScanner.HandleBlockEntry;

var
  lPos1,lPos2 : TYAMLPos;

begin
  if (FFlowLevel > 0) then
    Error(CurrPos,SErrUnexpectedBlockEntry);
  if (not FAllowKey) then
    Error(CurrPos,SErrUnexpectedBlockEntry);
  Indent(FCurrCol,-1,ytBlockSequenceStart,CurrPos);
  RemoveSimpleKey;
  FAllowKey:=True;
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lPos2:=CurrPos;
  QueueToken(TYAMLTokenData.Create(ytBlockEntry,lPos1,lpos2));
end;


procedure TYAMLScanner.HandleKey;

var
  lPos1,lPos2: TYAMLPos;

begin
  if (FFlowLevel=0) then
    begin
    //* Check if we are allowed to start a new key (not necessary simple). */
    if (not FAllowKey) then
      Error(CurrPos, SErrMappingKeysAreNotAllowedInBlockContext);
    Indent(FCurrCol, -1, ytBlockMappingStart, CurrPos);
    end;
  RemoveSimpleKey;
  FAllowKey:=(FFlowLevel=0);
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lPos2:=CurrPos;
  QueueToken(TYAMLTokenData.Create(ytKey,lPos1,lPos2));
end;


procedure TYAMLScanner.HandleValue;

var
  lPos1,lPos2 : TYAMLPos;
  lKey: PYAMLKey;

begin
  lKey:=PeekKey;
  if (lKey^.Possible) then
    begin
    With lKey^ do
      begin
      InsertToken(TokenNumber - FConsumedTokens, TYAMLTokenData.Create(ytKey,Position));
      Indent(Position.Column, TokenNumber,ytBlockMappingStart, Position);
      end;
    lKey^.Possible:=False;
    FAllowKey:=(FFlowLevel=0) and (FCurrCol>=FCurrLineLen);
    end
  else
    begin
    if (FFlowLevel=0) then
      begin
      if (not FAllowKey) then
        Error(CurrPos, SErrMappingValuesNotAllowedInThisContext);
      Indent(FCurrCol, -1, ytBlockMappingStart,CurrPos);
      end;
    FAllowKey:=(FFlowLevel=0);
    end;
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lPos2:=CurrPos;
  QueueToken(TYAMLTokenData.Create(ytValue,lPos1,lPos2));
end;


procedure TYAMLScanner.HandleAnchorOrAlias(ATokenType: TYAMLToken);

begin
  SaveSimpleKey;
  FAllowKey:=False;
  QueueToken(ScanAnchorOrAlias(ATokenType));
end;


procedure TYAMLScanner.HandleTag;

begin
  SaveSimpleKey;
  FAllowKey:=False;
  QueueToken(ScanTag);
end;


procedure TYAMLScanner.HandleBlockScalar(aFolded: Boolean);

begin
  RemoveSimpleKey;
  FAllowKey:=True;
  QueueToken(ScanBlockScalar(aFolded));
end;


procedure TYAMLScanner.HandleFlowScalar(aSingle: Boolean);

begin
  SaveSimpleKey;
  FAllowKey:=False;
  QueueToken(ScanFlowScalar(aSingle));
end;


procedure TYAMLScanner.HandlePlainScalar;

begin
  SaveSimpleKey;
  FAllowKey:=False;
  QueueToken(ScanPlainScalar);
end;


function TYAMLScanner.ReadLine: Boolean;

begin
  if FCurrCol>FCurrLineLen then
    FCurrCol:=0;
  Result:=(FCurrCol>0);
  if not (Result or FReader.EOF) then
    begin
    FCurrLine:=FReader.ReadLine;
    FCurrLineLen:=Length(FCurrLine);
    Inc(FCurrRow);
    Result:=True;
    if not (FSkipAllowKeyCheck or InFlowContext) then
      FAllowKey:=True;
    FCurrCol:=1
    end;
end;


function TYAMLScanner.ScanDirective: TYAMLTokenData;

var
  lPos1,lPos2 : TYAMLPos;
  lmajor, lMinor: Integer;
  lName,lPrefix, lHandle : TYAMLString;

begin
  lPos1:=CurrPos;
  Inc(FCurrCol);
  lName:=ScanDirectiveName(lPos1);
  Case lName of
  'YAML':
    begin
    ScanVersionDirectiveValue(lPos1, lMajor, lMinor);
    lPos2:=CurrPos;
    Result:=TYAMLTokenData.Create(ytVersionDirective,lPos1,lPos2,IntToStr(lMajor),IntToStr(lMinor));
    end;
  'TAG':
    begin
    ScanTagDirectiveValue(lPos1, lHandle, lPrefix);
    lPos2:=CurrPos;
    Result:=TYAMLTokenData.Create(ytTagDirective, lPos1,lPos2, lHandle, lPrefix);
    end
  else
    Error(CurrPos, SErrUnknownDirective, [lName]);
  end;
  Inc(FCurrCol);
  while (FCurrCol<=FCurrLineLen) and IsCurrBlank do
    Inc(FCurrCol);

  if (FCurrLine[FCurrCol]='#') then
    FCurrCol:=FCurrLineLen+1;
  if (FCurrCol<=FCurrLineLen) then
    Error(CurrPos, SErrUnexpectedTrailingData);
  ReadLine;
end;


function TYAMLScanner.IsCurrAlpha: Boolean;

begin
  Result:=(FCurrCol<=FCurrLineLen) and (FCurrLine[FCurrCol] in AlphaChars);
end;


function TYAMLScanner.IsCurrDigit: Boolean;

begin
  Result:=(FCurrCol<=FCurrLineLen) and (FCurrLine[FCurrCol] in DigitChars);
end;


function TYAMLScanner.ScanDirectiveName(aPos: TYAMLPos): TYAMLString;

var
  lStart : Integer;

begin
  Result:='';
  lStart:=FCurrCol;
  Inc(FCurrCol);
  while IsCurrAlpha do
    Inc(FCurrCol);
  Result:=Copy(FCurrLine,lStart,FCurrCol-lStart);
  if (Result='') then
    Error(aPos, SErrMissingDirectiveName);
  if Not (IsCurrBlank) then
    Error(aPos,SErrUnexpectedCharacter, [FCurrLine[FCurrCol]]);
end;


procedure TYAMLScanner.ScanVersionDirectiveValue(aPos: TYAMLPos; out AMajor: Integer; out AMinor: Integer);

  Function ScanDigit : Integer;

  var
    lStart: Integer;
    lNr: TYAMLString;

  begin
    Result:=0;
    lStart:=FCurrCol;
    while IsCurrDigit do
      Inc(FCurrCol);
    if (FCurrCol-lStart>MaxVersionDigitLength) then
        Error(CurrPos, SErrVersionNumberTooLong);
    lNr:=Copy(FCurrLine,lStart,FCurrCol-lStart);
    Result:=StrToIntDef(lNr, -1);
    if (Result=-1) then
      Error(aPos, SErrInvalidVersionNumber, [lNr]);
  end;

begin
  While (FCurrCol<=FCurrLineLen) and IsCurrBlank do
    Inc(FCurrCol);
  AMajor:=ScanDigit;
  if ((FCurrCol>FCurrLineLen) or (FCurrLine[FCurrCol]<>'.')) then
    Error(CurrPos, SErrInvalidCharacterInVersion, [FCurrLine[FCurrCol]]);
  Inc(FCurrCol);
  AMinor:=ScanDigit;
end;


procedure TYAMLScanner.ScanTagDirectiveValue(aPos: TYAMLPos; out AHandle: TYAMLString; out APrefix: TYAMLString);

begin
  While IsCurrBlank(False) do
    Inc(FCurrCol);
  aHandle:=ScanTagHandle(True,aPos);
  if not IsCurrBlank(False) then
    Error(CurrPos, SErrUnexpectedCharInDirectiveValue);
  While IsCurrBlank(False) do
    Inc(FCurrCol);
  aPrefix:=ScanTagUri(True,True,'', CurrPos);
  if (not IsCurrBlank) then
    Error(CurrPos, SErrNoWhiteSpaceAtDirectiveEnd);
end;


function TYAMLScanner.GetCurrChar: AnsiChar;

begin
  if FCurrCol>FCurrLineLen then
    Result:=#0
  else
    Result:=FCurrLine[FCurrCol];
end;


function TYAMLScanner.GetNextChar: AnsiChar;

begin
  if FCurrCol>=FCurrLineLen then
    Result:=#0
  else
    Result:=FCurrLine[FCurrCol+1];
end;


function TYAMLScanner.ScanTagHandle(aIsDirective: Boolean; aPos: TYAMLPos): TYAMLString;

var
  lStart : Integer;

begin
  if GetCurrChar <> '!' then
    Error(CurrPos, SErrUnexpectedCharacterInTag, [DirectiveNames[aIsDirective], GetCurrChar]);
  LStart:=FCurrCol;
  Inc(FCurrCol);
  while (IsCurrAlpha) do
    Inc(FCurrCol);
  if (GetCurrChar<>'!') then
    Error(CurrPos,SErrUnexpectedCharacterInTag, [DirectiveNames[aIsDirective], GetCurrChar]);
  inc(FCurrCol);
  Result:=Copy(FCurrLine,lStart,FCurrCol-lStart);
end;


function TYAMLScanner.ScanTagUri(AIsVerbatim: Boolean; AIsDirective: Boolean; AHead: TYAMLString; aPos: TYAMLPos): TYAMLString;

var
  C : AnsiChar;

begin
  if Length(AHead)> 1 then
    Result:=Copy(AHead,2,Length(AHead) - 1)
  else
    Result:='';
  C:=GetCurrChar;
  while IsCurrAlpha
        or (C in TagURIChars)
        or (AIsVerbatim and (C in [',','[',']'])) do
    begin
    if (C='%') then
      Result:= Result+ScanUriEscapes(AIsDirective, CurrPos)
    else
      Result:=Result + C;
    Inc(FCurrCol);
    C:=GetCurrChar;
    end;
  if (Result='') then
    Error(CurrPos, SErrMissingTagURI);
end;


function TYAMLScanner.ScanUriEscapes(aIsDirective: Boolean; aPos: TYAMLPos): TYAMLString;

var
  lWidth: Integer;
  lByte: Byte;
  C1,C2 : AnsiChar;

begin
  Result:='';
  lWidth:=0;
  repeat
    lByte:=0;
    Inc(FCurrCol); // Skip %
    C1:=GetCurrChar;
    Inc(FCurrCol);
    C2:=GetCurrChar;
    if Not ((C1 in HexChars) and (C2 in HexChars)) then
      Error(CurrPos, SErrInvalidURIEscapeChar, [DirectiveNames[aIsDirective]]);
    lByte:=StrToInt('$'+C1+C2);
    if (lWidth=0) then
      begin
      if (lByte and $80)=$00 then
        lWidth:=1
      else if (lByte and $E0)=$C0 then
        lWidth:=2
      else if (lByte and $F0)=$E0 then
        lWidth:=3
      else if (lByte and $F8)=$F0 then
        lWidth:=4
      else
        Error(aPos,SErrInvalidUTF8Char,[DirectiveNames[aIsDirective]]);
      end
    else if ((lByte and $C0) <> $80) then
      Error(aPos, Format(SErrInvalidUTF8Char, [DirectiveNames[aIsDirective]]));
    Result:=Result+AnsiChar(lByte);
    Dec(lWidth);
  until (lWidth=0);
end;


function TYAMLScanner.ScanAnchorOrAlias(ATokenType: TYAMLToken): TYAMLTokenData;

const
  AnchorAliasNames : Array[Boolean] of string = ('alias','anchor');

var
  lPos1,lPos2: TYAMLPos;
  lValue: TYAMLString;

begin
  lValue:='';
  inc(FCurrCol);
  lPos1:=CurrPos;
  while IsCurrAlpha do
    Inc(FCurrCol);
  lPos2:=CurrPos;
  if (LPos2.Column-LPos1.Column)=0 then
    Error(CurrPos, SErrInvalidAnchorAliasName,[AnchorAliasNames[aTokenType=ytAnchor]]);
  if not (IsCurrBlank(False) or (GetCurrChar in ['?', ':', ',', ']', '}', '%', '@', '`'])) then
    Error(CurrPos, SErrInvalidCharacterInAnchor, [GetCurrChar]);
  lValue:=Copy(FCurrLine,LPos1.Column,LPos2.Column-LPos1.Column);
  Result:=TYAMLTokenData.Create(aTokenType,lPos1,lPos2,lValue);
end;


function TYAMLScanner.ScanTag: TYAMLTokenData;

var
  lPos1,lPos2: TYAMLPos;
  lSuffix,lHandle: TYAMLString;

begin
  lHandle:='';
  lSuffix:='';
  lPos1:=CurrPos;
  inc(FCurrCol);
  if (GetCurrChar='<') then
    begin
    Inc(FCurrCol);
    lSuffix:=ScanTagUri(True, False,'',lPos1);
    if (GetCurrChar <> '>') then
      Error(CurrPos, SErrInvalidTagEnd);
    Inc(FCurrCol);
    end
  else
    begin
    lhandle:=ScanTagHandle(False,lPos1);
    if (Length(lHandle)>1) and (lHandle[1]='!') and (lHandle[Length(LHandle)]='!') then
      lSuffix:=ScanTagUri(False, False, '', lPos1)
    else
      begin
      lSuffix:=ScanTagUri(False, False,lHandle, lPos1);
      lHandle:='!';
      if (lSuffix='') then
        begin
        lHandle:='';
        lSuffix:='!';
        end;
      end;
    end;
  if (not IsCurrBlank) then
    if (FFlowLevel=0) or (GetCurrChar<>',') then
      Error(CurrPos, SErrInvalidWhitespace, [GetCurrChar]);
  lPos2:=CurrPos;
  Result:=TYAMLTokenData.Create(ytTag,lPos1,lPos2,lHandle,lSuffix);
end;


procedure TYAMLScanner.HandleBlockScalarWhiteSpace(var AIndent: Integer; var ABreaks: TYAMLString; aPos: TYAMLPos; out  aPos2: TYAMLPos);

var
  maxIndent: Integer;

begin
  maxIndent:=0;
  aPos2:=CurrPos;
  while (True) do
    begin
    // IsCurrBlank will return false if CurrCol>CurrLineLen, no need to check again
    while IsCurrBlank(False) and ((aIndent=0) or (FCurrCol < (aIndent+1))) do
      Inc(FCurrCol);
    if (FCurrCol-1>maxIndent) then
      maxIndent:=FCurrCol-1;
    if ((AIndent=0) or (FCurrCol < (AIndent+1))) and (GetCurrChar=TabChar) then
      Error(CurrPos, SErrInvalidIndentChar);
    // have chars ?
    if FCurrCol<=FCurrLineLen then
      Break;
    ReadLine;
    aBreaks:= aBreaks + #10;
    aPos2:=CurrPos;
    end;
  if (AIndent>0) then
    Exit;
  AIndent:=maxIndent;
  if (AIndent<=FCurrIndent) then
    AIndent:=FCurrIndent+1;
  if (AIndent<=0) then
    AIndent:=1;
end;


function TYAMLScanner.ScanBlockScalar(aFolded: Boolean): TYAMLTokenData;

Type
  TBlockStart=(bsNeutral,bsPlus,bsMinus);

Const
  Starts : Array[Boolean] of TBlockStart=(bsMinus,bsPlus);
  Scalars : array[Boolean] of TYAMLToken=(ytScalarLiteral,ytScalarFolded);

var
  lPos1,lPos2: TYAMLPos;
  lStart : TBlockStart;
  lLeading: TYAMLString;
  lTrailing: TYAMLString;
  lIncrement: Integer;
  lindent: Integer;
  lBlank: Boolean;
  lTrailingBlank: Boolean;
  lValue: TYAMLString;
  C : AnsiChar;

  procedure AppendValue(const s : string); inline;

  begin
    lValue:=lValue+S;
  end;


begin
  lLeading:='';
  lTrailing:='';
  lStart:=bsNeutral;
  lIncrement:=0;
  lindent:=0;
  lBlank:=False;
  lTrailingBlank:=False;
  lValue:='';
  lPos1:=CurrPos;
  Inc(FCurrCol);
  C:=GetCurrChar;
  if (C in ['+', '-']) then
    begin
    lStart:=Starts[C='+'];
    inc(FCurrCol);
    if (IsCurrDigit) then
      begin
      if (GetCurrChar='0') then
        Error(CurrPos, SErrInvalidIndentationChar);
      lIncrement:=Ord(GetCurrChar)- Ord('0');
      end;
    end
  //* Do the same as above, but in the opposite order. */
  else if (C in DigitChars) then
    begin
    if (GetCurrChar='0') then
      Error(CurrPos,SErrInvalidIndentationChar);
    lIncrement:=Ord(C)- Ord('0');
    inc(FCurrCol);
    C:=GetCurrChar;
    if (C in ['+', '-']) then
      begin
      lStart:=Starts[C='+'];
      Inc(FCurrCol);
      end;
    end;
  while IsCurrBlank(False) do
    Inc(FCurrCol);
  if GetCurrChar='#' then
    FCurrCol:=FCurrLineLen+1;
  if FCurrCol<=FCurrLineLen then
    Error(CurrPos, SErrUnexpectedEndOfLine);
  if FCurrCol>FCurrLineLen then
    ReadLine;
  lPos2:=CurrPos;

  if (lIncrement <> 0) then
    begin
    if FCurrIndent >= 0 then
      lindent:=FCurrIndent + lIncrement
    else
      lindent:=lIncrement;
  end;
  HandleBlockScalarWhiteSpace(lIndent,lTrailing,lPos1,lPos2);
  while ((FCurrCol-1)=lIndent) and ReadLine do
    begin
    lTrailingBlank:=IsCurrBlank(False);
    //* Check if we need to fold the leading line break. */
    if aFolded
        and ((lLeading<>'') and (lLeading[1]=#10))
        and (not lBlank) and (not lTrailingBlank) then
      begin
      if (lTrailing='') then
        AppendValue(' ');
      lLeading:='';
      end
    else
      begin
      AppendValue(lLeading);
      lLeading:='';
      end;
    AppendValue(lTrailing);
    lTrailing:='';
    lBlank:=IsCurrBlank(False);
    AppendValue(Copy(FCurrLine,FCurrCol,FCurrLineLen-FCurrCol+1));
    FCurrCol:=FCurrLineLen+1;
    ReadLine;
    lLeading:=lLeading+#10;
    HandleBlockScalarWhiteSpace(lIndent,lTrailing,lPos1,lPos2);
    end;
  if (lStart<>bsMinus) then
    AppendValue(lLeading);
  if (lStart=bsPlus) then
    AppendValue(lTrailing);
  Result:=TYAMLTokenData.Create(Scalars[aFolded],lPos1,lPos2,lValue);
end;


function TYAMLScanner.GetEscapeChar : TYAMLString;

var
  lSuff : TYAMLString;
  i,lLen : Integer;
  lPoint : Cardinal;
  C : AnsiChar;

begin
  Result:='';
  lLen:=0;
  inc(FCurrCol);
  C:=GetCurrChar;
  //* Check the escape character. */
  case C of
    '0': lSuff:=#00;
    'a': lSuff:=#07;
    'b': lSuff:=#08;
    't': lSuff:=#09;
    'n': lSuff:=#10;
    'v': lSuff:=#11;
    'f': lSuff:=#12;
    'r': lSuff:=#13;
    'e': lSuff:=#$1B;
    ' ': lSuff:=#32;
    '"': lSuff:='"';
    '/': lSuff:='/';
    '\': lSuff:='\';
    'N': lSuff:=#$C2#$85;
    '_': lSuff:=#$C2#$A0;
    'L': lSuff:=#$E2#$80#$A8;
    'P': lSuff:=#$E2#$80#$A9;
    'x': lLen:=2;
    'u': lLen:=4;
    'U': lLen:=8;
  else
     Error(CurrPos, SErrInvalidEscapeChar,[C]);
  end;
  if (lLen=0) then
    Exit(lSuff);
  lPoint:=0;
  for i:=1 to lLen do
    begin
    inc(FCurrCol);
    C:=GetCurrChar;
    if not (C in HexChars) then
      Error(CurrPos, SErrInvalidHexChar, [C]);
    lPoint:=(lPoint shl 4) + StrToInt('$'+C);
    end;
  if (((lPoint >= $D800) and (lPoint <= $DFFF)) or (lPoint > $10FFFF)) then
    Error(CurrPos, SErrInvalidUnicodePoint, [HexStr(Pointer(lPoint))]);
  if (lPoint <= $7F) then
    Result:=AnsiChar(lPoint)
  else if (lPoint <= $07FF) then
    Result:=AnsiChar($C0 + (lPoint shr 6)) + AnsiChar($80 + (lpoint and $3F))
  else if (lPoint <= $FFFF) then
    Result:=AnsiChar($E0 + (lPoint shr 12))
            + AnsiChar($80 + ((lPoint shr 6) and $3F))
            + AnsiChar($80 + (lPoint and $3F))
  else
    Result:=AnsiChar($F0 + (lPoint shr 18))
            + AnsiChar($80 + ((lPoint shr 12) and $3F))
            + AnsiChar($80 + ((lPoint shr 6) and $3F))
            + AnsiChar($80 + (lPoint and $3F));
end;


procedure TYAMLScanner.DoFold(var aValue, aLeading, aTrailing: TYAMLString);

begin
  if (aLeading<>'') and (aLeading[1]=#10) then
    begin
    if (aTrailing='') then
      aValue:=aValue + ' '
    else
      begin
      aValue:=aValue + aTrailing;
      aTrailing:='';
      end;
    aLeading:='';
    end
  else
    begin
    aValue:=aValue + aLeading;
    aValue:=aValue + aTrailing;
    aLeading:='';
    aTrailing:='';
    end;
end;


function TYAMLScanner.ScanFlowScalar(AIsSingle: Boolean): TYAMLTokenData;

const
  Scalars : array[Boolean] of TYAMLToken=(ytScalarDouble,ytScalarSingle);
  QuoteChars : Array[Boolean] of Char=('"','''');

var
  lBlanks: Boolean;
  lPos1,lPos2: TYAMLPos;
  lValue: TYAMLString;
  lLeading: TYAMLString;
  lTrailing: TYAMLString;
  whitespaces: TYAMLString;
  c : ansichar;

  procedure AppendValue(const s : string); inline;

  begin
    lValue:=lValue+S;
  end;

begin
  lBlanks :=False;
  lLeading:='';
  lTrailing:='';
  lValue:='';
  whitespaces:='';
  lPos1:=CurrPos;
  Inc(FCurrCol);
  while (True) do
    begin
    if (FCurrCol=1) and (IsDocumentStart or IsDocumentEnd) then
      Error(CurrPos, SErrInvalidDocumentIndicator);
    //* Check for EOF. */
    if (FCurrCol>FCurrLineLen) and not ReadLine then
      Error(CurrPos, SErrUnexpectedEOS);
    lBlanks:=False;
    while not IsCurrBlank do
      begin
      C:=GetCurrChar;
      // Single quote
      if aIsSingle then
        begin
        if (C<> '''') then
          begin
          AppendValue(C);
          Inc(FCurrCol);
          end
        else if (GetNextChar='''') then
          begin
          AppendValue('''');
          Inc(FCurrCol,2);
          end
        else if (C= '''') then
          Break
        end
      else
        // Double quote
        begin
        if (C='"') then
          Break
        else if (C='\') then
          begin
          if (FCurrCol<FCurrLineLen) then
            begin
            AppendValue(GetEscapeChar);
            Inc(FCurrCol);
            end
          else
            begin
            FCurrCol:=FCurrLineLen+1;
            ReadLine;
            lBlanks:=True;
            Break;
            end
          end
        else
          begin
          AppendValue(C);
          Inc(FCurrCol);
          end;
        end;
      end;
    if C=QuoteChars[aIsSingle] then
      Break;
    while IsCurrBlank(True) do
      begin
      if IsCurrBlank(False) then
        begin
        if not lBlanks then
          whitespaces:=whitespaces + GetCurrChar;
        inc(FCurrCol);
        end
      else
        begin
        if not Readline then
          Error(CurrPos,SErrUnexpectedEOS);
        if lBlanks then
          lTrailing:=lTrailing + #10
        else
          begin
          whitespaces:='';
          Lleading:=lLeading+#10;
          lBlanks:=True;
          end
        end;
      end;

    if (lBlanks) then
      DoFold(lValue,lLeading,lTrailing)
    else
      begin
      AppendValue(whitespaces);
      whitespaces:='';
      end;
    end; // While
  Inc(FCurrCol);
  if (FFlowLevel=0) and (FCurrCol>FCurrLineLen) then
    FAllowKey:=True;
  lPos2:=CurrPos;
  Result:=TYAMLTokenData.Create(Scalars[aIsSingle],lPos1,lPos2,lValue)
end;


function TYAMLScanner.ScanPlainScalar: TYAMLTokenData;

var
  lBlanks: Boolean;
  lPos1, lPos2: TYAMLPos;
  lValue : TYAMLString;
  lLeading: TYAMLString;
  lTrailing: TYAMLString;
  whitespaces: TYAMLString;
  lindent: Integer;
  C,C2 : AnsiChar;

  procedure AppendValue(const s : string); inline;

  begin
    lValue:=lValue+S;
  end;

begin
  lLeading:='';
  lTrailing:='';
  lValue:='';
  whitespaces:='';
  lBlanks:=False;
  lindent:=FCurrIndent + 1;
  lPos1:=CurrPos;
  lPos2:=lPos1;
  FSkipAllowKeyCheck:=True;
  try
    while ReadLine do
      begin
      if (FCurrCol=1) and (IsDocumentStart or isDocumentEnd) then
        Break;
      if (GetCurrChar='#') then
        Break;
      while not IsCurrBlank(True) do
        begin
        C:=GetCurrChar;
        C2:=GetNextChar;
        if (FFlowLevel > 0) and (C=':') and (C2 in FlowIndicatorChars) then
            Error(CurrPos, SErrUnexpectedColon);
        if ((C=':') and IsNextBlank)
           or ((FFlowLevel>0) and (C in FlowIndicatorChars)) then
          Break;
        if lBlanks or (whitespaces <> '') then
          begin
          if (lBlanks) then
            begin
            DoFold(lValue,lLeading,lTrailing);
            lBlanks:=False;
            end
          else
            begin
            AppendValue(whitespaces);
            whitespaces:='';
            end;
          end;
        AppendValue(C);
        Inc(FCurrCol);
        end;
      if not IsCurrBlank(True)  then
        Break;
      while IsCurrBlank(True) do
        begin
        if IsCurrBlank(False) then
          begin
          if lBlanks and ((FCurrCol-1)<lIndent) and (GetCurrChar=TabChar) then
            Error(CurrPos, SErrUnexpectedTab);
          if not lBlanks then
            begin
            lBlanks:=FCurrCol=1;
            whitespaces:=whitespaces + GetCurrChar;
            end;
          Inc(FCurrCol);
          end
        else
          begin
          if lBlanks then
            lTrailing:=lTrailing+#10
          else
            begin
            whitespaces:='';
            lLeading:=lLeading+#10;
            lBlanks:=True;
            end;
          if not ReadLine then break;
          end;
        end;

      if (FFlowLevel=0) and (FCurrCol-1 < lIndent) then
        Break;
      end;
  finally
    FSkipAllowKeyCheck:=False
  end;
  Result:=TYAMLTokenData.Create(ytScalarPlain, lPos1, lPos2,lValue);
  if (lBlanks) or ((FFlowLevel=0) and (FCurrCol>FCurrLineLen))  then
    FAllowKey:=True;
end;


end.


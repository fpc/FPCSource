{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    File name wildcard matching (*, ?, [set]), compatible with Delphi's System.Masks.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPMasks;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes;
{$ENDIF FPC_DOTTEDUNITS}

type
  EMaskException = class(Exception);

  TMaskOption = (
    moCaseSensitive,  // letters must match in case
    moDisableSets,    // '[' is an ordinary character
    moUseEscapes      // the escape character makes the next character literal
  );
  TMaskOptions = set of TMaskOption;

  { TMask }

  TMask = class
  private
    type
      TTokenKind = (tkLiteral, tkAnyChar, tkAnyText, tkSet);
      TCodePointRange = record
        Lo, Hi: Cardinal;
      end;
      TCodePointRanges = array of TCodePointRange;
      TToken = record
        Kind: TTokenKind;
        Value: Cardinal;
        Negate: Boolean;
        Ranges: TCodePointRanges;
        FoldedRanges: TCodePointRanges;
      end;
  private
    FMask: UnicodeString;
    FOptions: TMaskOptions;
    FEscapeChar: WideChar;
    FTokens: array of TToken;
    function GetCaseSensitive: Boolean;
    procedure Compile;
    function SetMatches(const aToken: TToken; aChar, aFolded: Cardinal): Boolean;
  public
    // Create a mask; matching ignores case unless aCaseSensitive is set. Raises EMaskException for an invalid mask
    constructor Create(const aMask: UnicodeString; aCaseSensitive: Boolean = False); overload;
    // Create a mask with options; aEscapeChar is used with moUseEscapes. Raises EMaskException for an invalid mask
    constructor Create(const aMask: UnicodeString; aOptions: TMaskOptions; aEscapeChar: WideChar = '\'); overload;
    // True if aFileName matches the mask
    function Matches(const aFileName: UnicodeString): Boolean;
    // The mask text
    property Mask: UnicodeString read FMask;
    // True if letters must match in case
    property CaseSensitive: Boolean read GetCaseSensitive;
    // Options given to the constructor
    property Options: TMaskOptions read FOptions;
    // Escape character used with moUseEscapes
    property EscapeChar: WideChar read FEscapeChar;
  end;

  { TParseStringList }

  TParseStringList = class(TStringList)
  public
    // Split aText at every character of aSeparators, leaving out empty parts
    constructor Create(const aText, aSeparators: String);
  end;

  { TMaskList }

  TMaskList = class
  private
    FMask: UnicodeString;
    FMasks: array of TMask;
    function GetCount: Integer;
    function GetItem(aIndex: Integer): TMask;
    procedure AddMasks(const aValue: UnicodeString; aSeparator: WideChar; aOptions: TMaskOptions; aEscapeChar: WideChar);
  public
    // Create masks from aValue split at aSeparator; empty parts are left out
    constructor Create(const aValue: UnicodeString; aSeparator: WideChar = ';'; aCaseSensitive: Boolean = False); overload;
    // Create masks with options from aValue split at aSeparator; empty parts are left out
    constructor Create(const aValue: UnicodeString; aSeparator: WideChar; aOptions: TMaskOptions; aEscapeChar: WideChar = '\'); overload;
    destructor Destroy; override;
    // True if aFileName matches any of the masks
    function Matches(const aFileName: UnicodeString): Boolean;
    // Number of masks
    property Count: Integer read GetCount;
    // The masks
    property Items[aIndex: Integer]: TMask read GetItem; default;
    // The text the masks were created from
    property Mask: UnicodeString read FMask;
  end;

// True if aFileName matches aMask, ignoring case
function MatchesMask(const aFileName, aMask: UnicodeString): Boolean; overload;
// True if aFileName matches aMask; letters must match in case if aCaseSensitive is set
function MatchesMask(const aFileName, aMask: UnicodeString; aCaseSensitive: Boolean): Boolean; overload;
// True if aFileName matches one of the masks in aMask, separated by aSeparator
function MatchesMaskList(const aFileName, aMask: UnicodeString; aSeparator: WideChar = ';'; aCaseSensitive: Boolean = False): Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.RtlConsts;
{$ELSE FPC_DOTTEDUNITS}
uses
  RtlConsts;
{$ENDIF FPC_DOTTEDUNITS}

type
  TCodePoints = array of Cardinal;


// True if a widestring manager with case mapping is installed
function HaveCaseMapping: Boolean;

var
  lManager: TUnicodeStringManager;

begin
  GetUnicodeStringManager(lManager);
  Result:=Pointer(lManager.LowerUnicodeStringProc)<>Pointer(lManager.UpperUnicodeStringProc);
end;


// Lower case of code point aChar: ASCII always, other BMP characters when a widestring manager is installed
function FoldCodePoint(aChar: Cardinal; aCaseMapping: Boolean): Cardinal;

var
  lLower: UnicodeString;

begin
  if (aChar>=Ord('A')) and (aChar<=Ord('Z')) then
    Result:=aChar+32
  else if (aChar<128) or (aChar>$FFFF) or not aCaseMapping then
    Result:=aChar
  else
    begin
    lLower:=UnicodeLowerCase(UnicodeString(WideChar(aChar)));
    if Length(lLower)=1 then
      Result:=Ord(lLower[1])
    else
      Result:=aChar;
    end;
end;


// Code points of s; aPositions receives the 1-based index in s where each code point starts
function ToCodePoints(const s: UnicodeString; out aPositions: TCodePoints): TCodePoints;

var
  i, lCount: Integer;

begin
  Result:=Nil;
  aPositions:=Nil;
  SetLength(Result,Length(s));
  SetLength(aPositions,Length(s));
  lCount:=0;
  i:=1;
  while i<=Length(s) do
    begin
    aPositions[lCount]:=i;
    if (s[i]>=#$D800) and (s[i]<=#$DBFF) and (i<Length(s)) and (s[i+1]>=#$DC00) and (s[i+1]<=#$DFFF) then
      begin
      Result[lCount]:=$10000+((Ord(s[i])-$D800) shl 10)+(Ord(s[i+1])-$DC00);
      Inc(i,2);
      end
    else
      begin
      Result[lCount]:=Ord(s[i]);
      Inc(i);
      end;
    Inc(lCount);
    end;
  SetLength(Result,lCount);
  SetLength(aPositions,lCount);
end;


{ TMask }

constructor TMask.Create(const aMask: UnicodeString; aCaseSensitive: Boolean);

begin
  if aCaseSensitive then
    Create(aMask,[moCaseSensitive])
  else
    Create(aMask,[]);
end;


constructor TMask.Create(const aMask: UnicodeString; aOptions: TMaskOptions; aEscapeChar: WideChar);

begin
  inherited Create;
  FMask:=aMask;
  FOptions:=aOptions;
  FEscapeChar:=aEscapeChar;
  Compile;
end;


function TMask.GetCaseSensitive: Boolean;

begin
  Result:=moCaseSensitive in FOptions;
end;


procedure TMask.Compile;

var
  lChars, lPos: TCodePoints;
  lCount, i: Integer;
  lCaseMapping: Boolean;

  procedure InvalidMask(aIndex: Integer);
  var
    lAt: Integer;
  begin
    if aIndex<Length(lPos) then
      lAt:=lPos[aIndex]
    else
      lAt:=Length(FMask)+1;
    raise EMaskException.CreateFmt(SInvalidMask,[String(FMask),lAt]);
  end;

  function NewToken(aKind: TTokenKind): Integer;
  begin
    Result:=lCount;
    if lCount>=Length(FTokens) then
      SetLength(FTokens,lCount*2+4);
    FTokens[lCount]:=Default(TToken);
    FTokens[lCount].Kind:=aKind;
    Inc(lCount);
  end;

  procedure AddLiteral(aChar: Cardinal);
  var
    lToken: Integer;
  begin
    lToken:=NewToken(tkLiteral);
    if moCaseSensitive in FOptions then
      FTokens[lToken].Value:=aChar
    else
      FTokens[lToken].Value:=FoldCodePoint(aChar,lCaseMapping);
  end;

  { Reads one set member character at i, honouring the escape character, and moves i past it }
  function ReadSetChar: Cardinal;
  begin
    if (moUseEscapes in FOptions) and (lChars[i]=Ord(FEscapeChar)) then
      begin
      Inc(i);
      if i>=Length(lChars) then
        InvalidMask(i);
      end;
    Result:=lChars[i];
    Inc(i);
  end;

  procedure AddSet;
  var
    lToken, lStart: Integer;
    lLo, lHi: Cardinal;
    lRanges, lFolded: TCodePointRanges;
    lFoldLo, lFoldHi: Cardinal;
  begin
    lStart:=i;
    Inc(i);
    lToken:=NewToken(tkSet);
    if (i<Length(lChars)) and (lChars[i]=Ord('!')) then
      begin
      FTokens[lToken].Negate:=True;
      Inc(i);
      end;
    lRanges:=Nil;
    lFolded:=Nil;
    while (i<Length(lChars)) and (lChars[i]<>Ord(']')) do
      begin
      lLo:=ReadSetChar;
      lHi:=lLo;
      if (i+1<Length(lChars)) and (lChars[i]=Ord('-')) and (lChars[i+1]<>Ord(']')) then
        begin
        Inc(i);
        lHi:=ReadSetChar;
        if lHi<lLo then
          InvalidMask(i-1);
        end;
      SetLength(lRanges,Length(lRanges)+1);
      lRanges[High(lRanges)].Lo:=lLo;
      lRanges[High(lRanges)].Hi:=lHi;
      if not (moCaseSensitive in FOptions) then
        begin
        lFoldLo:=FoldCodePoint(lLo,lCaseMapping);
        lFoldHi:=FoldCodePoint(lHi,lCaseMapping);
        if lFoldLo<=lFoldHi then
          begin
          SetLength(lFolded,Length(lFolded)+1);
          lFolded[High(lFolded)].Lo:=lFoldLo;
          lFolded[High(lFolded)].Hi:=lFoldHi;
          end;
        end;
      end;
    if i>=Length(lChars) then
      InvalidMask(lStart);
    if lRanges=Nil then
      InvalidMask(i);
    FTokens[lToken].Ranges:=lRanges;
    FTokens[lToken].FoldedRanges:=lFolded;
    Inc(i);
  end;

begin
  FTokens:=Nil;
  lCount:=0;
  lCaseMapping:=not (moCaseSensitive in FOptions) and HaveCaseMapping;
  lChars:=ToCodePoints(FMask,lPos);
  i:=0;
  while i<Length(lChars) do
    case lChars[i] of
      Ord('*'):
        begin
        if (lCount=0) or (FTokens[lCount-1].Kind<>tkAnyText) then
          NewToken(tkAnyText);
        Inc(i);
        end;
      Ord('?'):
        begin
        NewToken(tkAnyChar);
        Inc(i);
        end;
      Ord('['):
        if moDisableSets in FOptions then
          begin
          AddLiteral(lChars[i]);
          Inc(i);
          end
        else
          AddSet;
    else
      if (moUseEscapes in FOptions) and (lChars[i]=Ord(FEscapeChar)) then
        begin
        if i+1>=Length(lChars) then
          InvalidMask(i);
        AddLiteral(lChars[i+1]);
        Inc(i,2);
        end
      else
        begin
        AddLiteral(lChars[i]);
        Inc(i);
        end;
    end;
  SetLength(FTokens,lCount);
end;


function TMask.SetMatches(const aToken: TToken; aChar, aFolded: Cardinal): Boolean;

var
  i: Integer;

begin
  Result:=False;
  for i:=0 to High(aToken.Ranges) do
    if (aChar>=aToken.Ranges[i].Lo) and (aChar<=aToken.Ranges[i].Hi) then
      Exit(not aToken.Negate);
  for i:=0 to High(aToken.FoldedRanges) do
    if (aFolded>=aToken.FoldedRanges[i].Lo) and (aFolded<=aToken.FoldedRanges[i].Hi) then
      Exit(not aToken.Negate);
  Result:=aToken.Negate;
end;


function TMask.Matches(const aFileName: UnicodeString): Boolean;

var
  lChars, lFolded, lPos: TCodePoints;
  lToken, lChar, lStarToken, lStarChar, i: Integer;
  lCaseMapping, lOk: Boolean;

begin
  lChars:=ToCodePoints(aFileName,lPos);
  if moCaseSensitive in FOptions then
    lFolded:=lChars
  else
    begin
    lCaseMapping:=HaveCaseMapping;
    lFolded:=Nil;
    SetLength(lFolded,Length(lChars));
    for i:=0 to High(lChars) do
      lFolded[i]:=FoldCodePoint(lChars[i],lCaseMapping);
    end;
  lToken:=0;
  lChar:=0;
  lStarToken:=-1;
  lStarChar:=0;
  while lChar<Length(lChars) do
    begin
    if (lToken<Length(FTokens)) and (FTokens[lToken].Kind=tkAnyText) then
      begin
      lStarToken:=lToken;
      lStarChar:=lChar;
      Inc(lToken);
      Continue;
      end;
    lOk:=False;
    if lToken<Length(FTokens) then
      case FTokens[lToken].Kind of
        tkLiteral:
          lOk:=lFolded[lChar]=FTokens[lToken].Value;
        tkAnyChar:
          lOk:=True;
        tkSet:
          lOk:=SetMatches(FTokens[lToken],lChars[lChar],lFolded[lChar]);
        tkAnyText:
          ;
      end;
    if lOk then
      begin
      Inc(lToken);
      Inc(lChar);
      end
    else if lStarToken>=0 then
      begin
      lToken:=lStarToken+1;
      Inc(lStarChar);
      lChar:=lStarChar;
      end
    else
      Exit(False);
    end;
  while (lToken<Length(FTokens)) and (FTokens[lToken].Kind=tkAnyText) do
    Inc(lToken);
  Result:=lToken=Length(FTokens);
end;


{ TParseStringList }

constructor TParseStringList.Create(const aText, aSeparators: String);

var
  lStart, i: Integer;

begin
  inherited Create;
  lStart:=1;
  for i:=1 to Length(aText)+1 do
    if (i>Length(aText)) or (Pos(aText[i],aSeparators)>0) then
      begin
      if i>lStart then
        Add(Copy(aText,lStart,i-lStart));
      lStart:=i+1;
      end;
end;


{ TMaskList }

constructor TMaskList.Create(const aValue: UnicodeString; aSeparator: WideChar; aCaseSensitive: Boolean);

begin
  if aCaseSensitive then
    Create(aValue,aSeparator,[moCaseSensitive])
  else
    Create(aValue,aSeparator,[]);
end;


constructor TMaskList.Create(const aValue: UnicodeString; aSeparator: WideChar; aOptions: TMaskOptions; aEscapeChar: WideChar);

begin
  inherited Create;
  FMask:=aValue;
  AddMasks(aValue,aSeparator,aOptions,aEscapeChar);
end;


destructor TMaskList.Destroy;

var
  i: Integer;

begin
  for i:=0 to High(FMasks) do
    FMasks[i].Free;
  inherited Destroy;
end;


procedure TMaskList.AddMasks(const aValue: UnicodeString; aSeparator: WideChar; aOptions: TMaskOptions; aEscapeChar: WideChar);

var
  lStart, i: Integer;

begin
  lStart:=1;
  for i:=1 to Length(aValue)+1 do
    if (i>Length(aValue)) or (aValue[i]=aSeparator) then
      begin
      if i>lStart then
        begin
        SetLength(FMasks,Length(FMasks)+1);
        FMasks[High(FMasks)]:=TMask.Create(Copy(aValue,lStart,i-lStart),aOptions,aEscapeChar);
        end;
      lStart:=i+1;
      end;
end;


function TMaskList.GetCount: Integer;

begin
  Result:=Length(FMasks);
end;


function TMaskList.GetItem(aIndex: Integer): TMask;

begin
  Result:=FMasks[aIndex];
end;


function TMaskList.Matches(const aFileName: UnicodeString): Boolean;

var
  i: Integer;

begin
  Result:=False;
  for i:=0 to High(FMasks) do
    if FMasks[i].Matches(aFileName) then
      Exit(True);
end;


function MatchesMask(const aFileName, aMask: UnicodeString): Boolean;

begin
  Result:=MatchesMask(aFileName,aMask,False);
end;


function MatchesMask(const aFileName, aMask: UnicodeString; aCaseSensitive: Boolean): Boolean;

var
  lMask: TMask;

begin
  lMask:=TMask.Create(aMask,aCaseSensitive);
  try
    Result:=lMask.Matches(aFileName);
  finally
    lMask.Free;
  end;
end;


function MatchesMaskList(const aFileName, aMask: UnicodeString; aSeparator: WideChar; aCaseSensitive: Boolean): Boolean;

var
  lList: TMaskList;

begin
  lList:=TMaskList.Create(aMask,aSeparator,aCaseSensitive);
  try
    Result:=lList.Matches(aFileName);
  finally
    lList.Free;
  end;
end;

end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2023 The Free Pascal team

    Delphi-compatible Record based Regular expressions API unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.RegularExpressions;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.RegularExpressionsCore;
{$ELSE}
  SysUtils, System.RegularExpressionsCore;
{$ENDIF}

const
  MatchGrowDelta = 32;

type
  TRegExOption = (roNone, roIgnoreCase, roMultiLine, roExplicitCapture,
                  roCompiled, roSingleLine, roIgnorePatternSpace, roNotEmpty);
  TRegExOptions = set of TRegExOption;
  TREStringDynArray = Array of TREString;

  IObjectReference = Interface ['{69E6C6D3-764F-4A6F-BC3D-5F4E32A2E4F4}']
    Function GetObject : TObject;
  end;

  { TObjectReference }

  TObjectReference = Class(TInterfacedObject,IObjectReference)
  Private
    FObject : TObject;
  Protected
    Function GetObject : TObject;
  Public
    Constructor Create(aObject : TObject);
    Destructor Destroy; override;
    Property Obj : TObject Read GetObject;
  end;


  { TGroup }

  TGroup = record
  private
    FIndex: Integer;
    FLength: Integer;
    FName: TREString;
    FSuccess: Boolean;
    FValue: TREString;
  public
    constructor Create(const aValue,aName: TREString; aIndex, aLength: Integer; aSuccess: Boolean);
    property Index: Integer read FIndex;
    property Length: Integer read FLength;
    property Success: Boolean read FSuccess;
    property Value: TREString read FValue;
    property Name: TREString read FName;
  end;
  TGroupArray = Array of TGroup;

  TGroupCollectionEnumerator = class;

  { TGroupCollection }

  TGroupCollection = record
  private
    FGroups : TGroupArray;
    function GetCount: Integer;
    function GetItem(const aIndex: Variant): TGroup;
    function IndexOfName(const aName : TREString): Integer;
  public
    constructor Create(const aGroups : TGroupArray);
    function GetEnumerator: TGroupCollectionEnumerator;
    property Count: Integer read GetCount;
    property Item[const Index: Variant]: TGroup read GetItem; default;
  end;

  { TGroupCollectionEnumerator }

  TGroupCollectionEnumerator = class
  private
    FGroups: TGroupCollection;
    FCurrent: Integer;
  public
    constructor Create(const aGroups: TGroupCollection);
    function GetCurrent: TGroup;
    function MoveNext: Boolean;
    property Current: TGroup read GetCurrent;
  end;

  { TMatch }

  PMatch = ^TMatch;
  TMatch = record
  private
    FGroup: TGroup;
    FGroups: TGroupCollection;
    FRegex : IObjectReference;
    FNext: PMatch;
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetSuccess: Boolean;
    function GetValue: TREString;
    procedure SetNext(const aNext: PMatch);
  public
    constructor Create(const aRegex: IObjectReference; const aValue: TREString; aIndex, aLength: Integer; aSuccess: Boolean);
    function NextMatch: TMatch;
    function Result_(const Pattern: TREString): TREString;
    property Groups: TGroupCollection read FGroups;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read GetSuccess;
    property Value: TREString read GetValue;
  end;
  TMatchArray = array of TMatch;

  TMatchCollectionEnumerator = class;

  { TMatchCollection }

  TMatchCollection = record
  private
    FMatches: TMatchArray;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMatch;
  public
    constructor Create(const aRegex : IObjectReference; const aInput: TREString; aOptions: TRegExOptions; aStartPos: Integer);
    function GetEnumerator: TMatchCollectionEnumerator;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TMatch read GetItem; default;
  end;

  { TMatchCollectionEnumerator }

  TMatchCollectionEnumerator = class
  private
    FCollection: TMatchCollection;
    FCurrent: Integer;
  public
    constructor Create(const aCollection: TMatchCollection);
    function GetCurrent: TMatch;
    function MoveNext: Boolean;
    property Current: TMatch read GetCurrent;
  end;

  TMatchEvaluator = function(const Match: TMatch): TREString of object;

  { TRegEx }


  TRegEx = record
  private
    FOptions: TRegExOptions;
    FRef: IObjectReference;
    FRegEx: TPerlRegEx;
  public
    constructor Create(const aPattern: TREString; aOptions: TRegExOptions = [roNotEmpty]);
    function IsMatch(const aInput: TREString): Boolean; overload;
    function IsMatch(const aInput: TREString; aStartPos: Integer): Boolean; overload;
    class function IsMatch(const aInput, aPattern: TREString): Boolean;overload; static;
    class function IsMatch(const aInput, aPattern: TREString; aOptions: TRegExOptions): Boolean; overload; static;

    class function Escape(const aString: TREString; aUseWildCards: Boolean = False): TREString; static;

    function Match(const aInput: TREString): TMatch; overload;
    function Match(const aInput: TREString; aStartPos: Integer): TMatch; overload;
    function Match(const aInput: TREString; aStartPos, aLength: Integer): TMatch; overload;
    class function Match(const aInput, aPattern: TREString): TMatch; overload; static;
    class function Match(const aInput, aPattern: TREString; aOptions: TRegExOptions): TMatch; overload; static;

    function Matches(const aInput: TREString): TMatchCollection; overload;
    function Matches(const aInput: TREString; aStartPos: Integer): TMatchCollection; overload;
    class function Matches(const aInput, aPattern: TREString): TMatchCollection; overload; static;
    class function Matches(const aInput, aPattern: TREString; aOptions: TRegExOptions): TMatchCollection; overload; static;

    function Replace(const aInput, aReplacement: TREString): TREString; overload;
    function Replace(const aInput: TREString; aEvaluator: TMatchEvaluator): TREString; overload;
    function Replace(const aInput, aReplacement: TREString; aCount: Integer): TREString; overload;
    function Replace(const aInput: TREString; aEvaluator: TMatchEvaluator; aCount: Integer): TREString; overload;
    class function Replace(const aInput, aPattern, aReplacement: TREString): TREString; overload; static;
    class function Replace(const aInput, aPattern: TREString; aEvaluator: TMatchEvaluator): TREString; overload; static;
    class function Replace(const aInput, aPattern, aReplacement: TREString; aOptions: TRegExOptions): TREString; overload; static;
    class function Replace(const aInput, aPattern: TREString; aEvaluator: TMatchEvaluator; aOptions: TRegExOptions): TREString; overload; static;

    function Split(const aInput: TREString): TREStringDynArray; overload; inline;
    function Split(const aInput: TREString; aCount: Integer): TREStringDynArray; overload; inline;
    function Split(const aInput: TREString; aCount, aStartPos: Integer): TREStringDynArray; overload;
    class function Split(const aInput, aPattern: TREString): TREStringDynArray; overload; static;
    class function Split(const aInput, aPattern: TREString; aOptions: TRegExOptions): TREStringDynArray; overload; static;
  end;

                                                                     
  { TRegExHelper }

  TRegExHelper = record helper for TRegEx
  public
    procedure Study(Options: TRegExStudyOptions = []);
    procedure AddRawOptions(const Value: Integer);
  end;

function RegExOptionsToPCREOptions(Value: TRegExOptions): TPerlRegExOptions;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.Variants, System.RegularExpressionsConsts;
{$ELSE}
  Classes, Variants, System.RegularExpressionsConsts;
{$ENDIF}

Type

  { TMatcher }

  TMatcher = Class(TObject)
    FMatchEvaluator: TMatchEvaluator;
    FRef: IObjectReference;
    Constructor Create(AMatchEvaluator: TMatchEvaluator; aRef: IObjectReference);
    procedure ReplaceEvent(Sender: TObject; var ReplaceWith: TREString);
  end;

Function GetRegEx(aRef : IObjectReference) : TPerlRegEx; inline;

begin
  Result:=aRef.GetObject as TPerlRegex;
end;

function RegExOptionsToPCREOptions(Value: TRegExOptions): TPerlRegExOptions;

 Procedure Add(aFlag : TRegExOption; aOpt : TPerlRegExOption);

 begin
   if aFlag in Value then
     Include(Result,aOpt);
 end;

begin
  Result := [];
  Add(roIgnoreCase,preCaseLess);
  Add(roMultiLine,preMultiLine);
  Add(roExplicitCapture,preNoAutoCapture);
  Add(roSingleLine,preSingleLine);
  Add(roIgnorePatternSpace,preExtended);
end;

{ TMatcher }

constructor TMatcher.Create(AMatchEvaluator: TMatchEvaluator; aRef: IObjectReference);
begin
  FMatchEvaluator:=AMatchEvaluator;
  FRef:=aRef;
end;

procedure TMatcher.ReplaceEvent(Sender: TObject; var ReplaceWith: TREString);

var
  M: TMatch;
  RE : TPerlRegEx;

begin
  if Assigned(FMatchEvaluator) then
    begin
    RE:=Sender as TPerlRegEx;
    M:=TMatch.Create(FRef,RE.MatchedText,RE.MatchedOffset,Re.MatchedLength,True);
    ReplaceWith:=FMatchEvaluator(M);
    end;
end;

{ TObjectReference }

function TObjectReference.GetObject: TObject;
begin
  Result:=FObject;
end;

constructor TObjectReference.Create(aObject: TObject);
begin
  FObject:=aObject;
end;

destructor TObjectReference.Destroy;
begin
  FreeAndNil(FObject);
  inherited Destroy;
end;



{ TGroup }

constructor TGroup.Create(const aValue, aName: TREString; aIndex, aLength: Integer; aSuccess: Boolean);
begin
  FValue:=aValue;
  FIndex:=aIndex;
  FLength:=aLength;
  FSuccess:=aSuccess;
  FName:=aName;
end;

{ TGroupCollectionEnumerator }

constructor TGroupCollectionEnumerator.Create(const aGroups: TGroupCollection);
begin
  FGroups:=aGroups;
  FCurrent:=-1;
end;

function TGroupCollectionEnumerator.GetCurrent: TGroup;
begin
  Result:=FGroups.FGroups[FCurrent];
end;

function TGroupCollectionEnumerator.MoveNext: Boolean;
begin
  Result:=FCurrent<Length(FGroups.FGroups)-1;
  if Result then
    Inc(FCurrent);
end;

{ TMatch }

function TMatch.GetIndex: Integer;
begin
  Result:=FGroup.Index;
end;


function TMatch.GetLength: Integer;
begin
  Result:=FGroup.Length;
end;

function TMatch.GetSuccess: Boolean;
begin
  Result:=FGroup.Success;
end;

function TMatch.GetValue: TREString;
begin
  Result:=FGRoup.Value;
end;

procedure TMatch.SetNext(const aNext: PMatch);
begin
  FRegex:=Nil;
  FNext:=aNext;
end;

constructor TMatch.Create(const aRegex: IObjectReference; const aValue: TREString; aIndex, aLength: Integer; aSuccess: Boolean);

var
  N : TREStringDynArray;
  G : TGroupArray;
  RE : TPerlRegEx;
  i,idx : Integer;
  GN : TREString;

begin
  G:=Default(TGroupArray);
  N:=Default(TREStringDynArray);
  FRegex:=aRegex;
  FGroup:=TGroup.Create(aValue,'',aIndex,aLength,aSuccess);
  if Success then
    begin
    RE:=GetRegEx(FRegEx);
    SetLength(N,RE.GroupCount+1);
    For I:=0 to RE.NameCount-1 do
      begin
      GN:=RE.Names[i];
      Idx:=RE.NamedGroup(GN);
      if Idx<>-1 then
        N[Idx]:=GN;
      end;
    SetLength(G,RE.GroupCount+1);
    For I:=0 to RE.GroupCount do
      G[i]:=TGroup.Create(RE.Groups[I],N[i],RE.GroupOffsets[i],RE.GroupLengths[I],aSuccess);
    end;
  FGroups:=TGroupCollection.Create(G);
end;

function TMatch.NextMatch: TMatch;

var
  RE : TPerlRegEx;

begin
  if Assigned(FRegex) then
    begin
    RE:=GetRegEx(FRegex);
    if RE.MatchAgain then
      Result:=TMatch.Create(FRegex,RE.MatchedText,RE.MatchedOffset,RE.MatchedLength,True)
    else
      Result:=TMatch.Create(FRegex,'',0,0,False);
    end
  else if Assigned(FNext) then
    Result:=FNext^
  else
    Result:=TMatch.Create(FRegex,'',0,0,False);
end;

function TMatch.Result_(const Pattern: TREString): TREString;

var
  RE: TPerlRegEx;

begin
  RE:=GetRegEx(FRegex);
  RE.Replacement:=Pattern;
  Result:=RE.ComputeReplacement;
end;

{ TMatchCollection }

constructor TMatchCollection.Create(const aRegex: IObjectReference; const aInput: TREString; aOptions: TRegExOptions;
  aStartPos: Integer);

var
  Found: Boolean;
  Len : Integer;
  RE: TPerlRegEx;
begin
  RE:=GetRegEx(aRegex);
  RE.Subject:=aInput;
  RE.Options:=RegExOptionsToPCREOptions(AOptions);
  RE.Start:=aStartPos;
  Len:=0;
  SetLength(FMatches,0);
  Found:=RE.Match;
  while Found do
    begin
    if Len>=Length(FMatches) then
      SetLength(FMatches,Length(FMatches)+MatchGrowDelta);
    FMatches[Len]:=TMatch.Create(aRegex,RE.MatchedText,RE.MatchedOffset,RE.MatchedLength,Found);
    if Len>0 then
      FMatches[Len-1].SetNext(@FMatches[Len]);
    Found:=RE.MatchAgain;
    Inc(Len);
    end;
  FMatches[Len-1].SetNext(Nil);
  if Len<Length(FMatches) then
    SetLength(FMatches,Len);
end;

function TMatchCollection.GetCount: Integer;
begin
  Result:=Length(FMatches);
end;

function TMatchCollection.GetItem(Index: Integer): TMatch;
begin
  Result:=FMatches[Index];
end;

function TMatchCollection.GetEnumerator: TMatchCollectionEnumerator;
begin
  Result:=TMatchCollectionEnumerator.Create(Self);
end;

{ TMatchCollectionEnumerator }

constructor TMatchCollectionEnumerator.Create(const aCollection: TMatchCollection);
begin
  FCollection:=aCollection;
  FCurrent:=-1;
end;

function TMatchCollectionEnumerator.GetCurrent: TMatch;
begin
  Result:=FCollection.FMatches[FCurrent];
end;

function TMatchCollectionEnumerator.MoveNext: Boolean;
begin
  Result:=FCurrent<Length(FCollection.FMatches)-1;
  If Result then
    Inc(FCurrent);
end;

{ TRegEx }

constructor TRegEx.Create(const aPattern: TREString; aOptions: TRegExOptions);
begin
  FRegEx:=TPerlRegEx.Create;
  Foptions:=aOPtions;
  FRegex.Options:=RegExOptionsToPCREOptions(aOptions);
  FRegex.RegEx:=aPattern;
  FRef:=TObjectReference.Create(FRegex);
end;

function TRegEx.IsMatch(const aInput: TREString): Boolean;
begin
  Result:=IsMatch(aInput,1);
end;

function TRegEx.IsMatch(const aInput: TREString; aStartPos: Integer): Boolean;
begin
  FRegex.Subject:=aInput;
  FRegex.Start:=aStartPos;
  Result:=FRegex.Match;
end;

class function TRegEx.IsMatch(const aInput, aPattern: TREString): Boolean;

begin
  Result:=IsMatch(aInput,aPattern,[roNotEmpty]);
end;

class function TRegEx.IsMatch(const aInput, aPattern: TREString; aOptions: TRegExOptions): Boolean;

var
  RE : TRegEx;
begin
  RE:=TRegex.Create(aPattern,aOptions);
  Result:=RE.IsMatch(aInput);
end;

class function TRegEx.Escape(const aString: TREString; aUseWildCards: Boolean): TREString;

  function esc(const s : TREString; c : char; rep : string) : string;
  begin
    Result:=StringReplace(s,'\'+c,rep,[rfReplaceAll]);
    Result:=StringReplace(Result,Rep+rep,'\'+c,[rfReplaceAll]);
  end;

begin
  Result:=TPerlRegEx.EscapeRegExChars(aString);
  Result:=StringReplace(Result,#13#10,'\r\n',[rfReplaceAll]);
  if Not aUseWildCards then
    exit;
  Result:=Esc(Result,'?','(.)');
  Result:=Esc(Result,'*','(.*)');
end;

function TRegEx.Match(const aInput: TREString): TMatch;
begin
  Result:=Match(aInput,1,Length(aInput));
end;

function TRegEx.Match(const aInput: TREString; aStartPos: Integer): TMatch;
begin
  Result:=Match(aInput,aStartPos,Length(aInput));
end;

function TRegEx.Match(const aInput: TREString; aStartPos, aLength: Integer): TMatch;
var
  Found: Boolean;
  L,O : Integer;
  S : TREString;

begin
  L:=0;
  O:=0;
  S:='';
  With FRegEx do
    begin
    Subject:=aInput;
    FRegex.Start:=aStartPos;
    FRegex.Stop:=aStartPos+aLength-1;
    Found:=Match;
    if Found then
      begin
      S:=MatchedText;
      O:=MatchedOffset;
      L:=MatchedLength;
      end;
    end;
  Result:=TMatch.Create(FRef,S,O,L,Found);
end;

class function TRegEx.Match(const aInput, aPattern: TREString): TMatch;

var
  RE : TRegEx;
begin
  RE:=TRegex.Create(aPattern);
  Result:=RE.Match(aInput);
end;

class function TRegEx.Match(const aInput, aPattern: TREString; aOptions: TRegExOptions): TMatch;

var
  RE : TRegEx;

begin
  RE:=TRegex.Create(aPattern,aOptions);
  Result:=RE.Match(aInput);
end;

function TRegEx.Matches(const aInput: TREString): TMatchCollection;

begin
  Result:=TMatchCollection.Create(FRef,aInput,FOptions,1);
end;

function TRegEx.Matches(const aInput: TREString; aStartPos: Integer): TMatchCollection;
begin
  Result:=TMatchCollection.Create(FRef,aInput,FOptions,aStartPos);
end;

class function TRegEx.Matches(const aInput, aPattern: TREString): TMatchCollection;

var
  RE: TRegEx;

begin
  RE:=TRegEx.Create(aPattern);
  Result:=RE.Matches(aInput);
end;

class function TRegEx.Matches(const aInput, aPattern: TREString; aOptions: TRegExOptions): TMatchCollection;
var
  RE: TRegEx;

begin
  RE:=TRegEx.Create(aPattern,aOptions);
  Result:=RE.Matches(aInput);
end;

function TRegEx.Replace(const aInput, aReplacement: TREString): TREString;
begin
  FRegEx.Subject:=aInput;
  FRegEx.Replacement:=aReplacement;
  FRegEx.ReplaceAll;
  Result:=FRegEx.Subject;
end;

function TRegEx.Replace(const aInput: TREString; aEvaluator: TMatchEvaluator): TREString;

var
  M : TMatcher;

begin
  FRegEx.Subject:=aInput;
  M:=TMatcher.Create(aEvaluator,FRef);
  try
    FRegEx.OnReplace:=@M.ReplaceEvent;
    FRegEx.ReplaceAll;
    Result := FRegEx.Subject;
  finally
    M.Free;
  end;
end;

function TRegEx.Replace(const aInput, aReplacement: TREString; aCount: Integer): TREString;

var
  I: Integer;

begin
  if aCount<0 then
    Exit(Replace(aInput,aReplacement));
  I:=0;
  FRegEx.Subject:=aInput;
  FRegEx.Replacement:=aReplacement;
  if FRegEx.Match then
    repeat
      Inc(I);
      FRegEx.Replace;
    until (not FRegEx.MatchAgain) or (I>=aCount);
  Result:=FRegEx.Subject;
end;

function TRegEx.Replace(const aInput: TREString; aEvaluator: TMatchEvaluator; aCount: Integer): TREString;

var
  M : TMatcher;
  I : integer;

begin
  FRegEx.Subject:=aInput;
  M:=TMatcher.Create(aEvaluator,FRef);
  try
    I:=0;
    FRegEx.Subject:=aInput;
    FRegEx.OnReplace:=@M.ReplaceEvent;
    if FRegEx.Match then
      repeat
        Inc(I);
        FRegEx.Replace;
      until (not FRegEx.MatchAgain) or (I>=aCount);
    Result:=FRegEx.Subject;
  finally
    M.Free;
  end;
end;

class function TRegEx.Replace(const aInput, aPattern, aReplacement: TREString): TREString;

var
  RE : TRegex;

begin
  RE:=TRegex.Create(aPattern);
  Result:=RE.Replace(aInput,aReplacement);
end;

class function TRegEx.Replace(const aInput, aPattern: TREString; aEvaluator: TMatchEvaluator): TREString;
begin
  Result:=Replace(aInput,aPattern,aEvaluator,[roNotEmpty]);
end;

class function TRegEx.Replace(const aInput, aPattern, aReplacement: TREString; aOptions: TRegExOptions): TREString;
var
  RE : TRegex;

begin
  RE:=TRegex.Create(aPattern,aOptions);
  Result:=RE.Replace(aInput,aReplacement);
end;

class function TRegEx.Replace(const aInput, aPattern: TREString; aEvaluator: TMatchEvaluator; aOptions: TRegExOptions): TREString;

var
  RE: TRegEx;

begin
  RE:=TRegEx.Create(aPattern,aOptions);
  Result:=RE.Replace(aInput,aEvaluator);
end;

function TRegEx.Split(const aInput: TREString): TREStringDynArray;
begin
  Result:=Split(aInput,0,1);
end;

function TRegEx.Split(const aInput: TREString; aCount: Integer): TREStringDynArray;
begin
  Result:=Split(aInput,aCount,1);
end;

function TRegEx.Split(const aInput: TREString; aCount, aStartPos: Integer): TREStringDynArray;

var
  L: TStrings;

begin
  Result:=Default(TREStringDynArray);
  if aInput='' then
    exit;
  FRegEx.Subject:=aInput;
  Result:=FRegEx.SplitCapture(aCount,aStartPos);
end;

class function TRegEx.Split(const aInput, aPattern: TREString): TREStringDynArray;

var
  RE: TRegEx;

begin
  RE:=TRegEx.Create(aPattern);
  Result:= RE.Split(aInput);
end;

class function TRegEx.Split(const aInput, aPattern: TREString; aOptions: TRegExOptions): TREStringDynArray;
var
  RE: TRegEx;

begin
  RE:=TRegEx.Create(aPattern,aOptions);
  Result:=RE.Split(aInput);
end;

{ TRegExHelper }

procedure TRegExHelper.Study(Options: TRegExStudyOptions);
begin

end;

procedure TRegExHelper.AddRawOptions(const Value: Integer);
begin

end;


function TGroupCollection.GetCount: Integer;
begin
  Result:=Length(FGroups);
end;

function TGroupCollection.GetItem(const aIndex: Variant): TGroup;

var
  Idx: Integer;
  IdxIsName : Boolean;
begin
  IdxIsName:=False;
  Idx:=-1;
  case VarType(aIndex) of
    varByte,
    varWord,
    varLongWord,
    varQWord,
    varSmallint,
    varShortInt,
    varInteger,
    varInt64:
      Idx:=aIndex;
    varString,
    varUString,
    varOleStr:
      begin
      Idx:=IndexOfName(TREString(aIndex));
      idxIsName:=True;
      end
  else
    raise ERegularExpressionError.Create(SRegExInvalidIndexType);
  end;

  if (Idx>=0) and (Idx<Length(FGroups)) then
    Result:=FGroups[Idx]
  else if (Idx=-1) and (IdxIsName) then
    raise ERegularExpressionError.CreateFmt(SRegExInvalidGroupName,[TREString(aIndex)])
  else
    raise ERegularExpressionError.CreateFmt(SRegExIndexOutOfBounds,[Idx]);
end;

function TGroupCollection.IndexOfName(const aName: TREString): Integer;
begin
  Result:=Length(FGroups)-1;
  While (Result>=0) and (FGroups[Result].Name<>'') and Not SameText(aName,FGroups[Result].Name) do
    Dec(Result);
end;

constructor TGroupCollection.Create(const aGroups: TGroupArray);

begin
  FGroups:=aGroups;
end;

function TGroupCollection.GetEnumerator: TGroupCollectionEnumerator;
begin
  Result:=TGroupCollectionEnumerator.Create(Self);
end;

end.

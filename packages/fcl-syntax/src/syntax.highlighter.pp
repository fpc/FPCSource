{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Basic syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
unit syntax.highlighter;

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.Classes, System.SysUtils, System.Contnrs;
{$ELSE}
  Types, Classes, SysUtils, contnrs;
{$ENDIF}

type
  ESyntaxHighlighter = class(Exception);

  // Various token kinds
  TSyntaxHighlightKind = (
    shDefault,
    shInvalid,
    shSymbol,
    shKeyword,
    shComment,
    shDirective,
    shNumbers,
    shCharacters,
    shStrings,
    shAssembler,
    shRegex,
    shInterpolation,
    shEscape,
    shRawString,
    shTemplate,
    shLambda,
    shOperator,
    shError,
    shWarning,
    shAttribute,
    shSection,
    shKey
  );

  TSyntaxHighlightKindHelper = type helper for TSyntaxHighlightKind
    function toString : string;
  end;

const
  // Maximum number of categories of one token
  MaxTokenCategories = 7;
  // Highest category ID
  MaxCategoryID = High(SmallInt);

type

  { TSyntaxToken }

  // One syntax token
  TSyntaxToken = record
  Private
    FCategories: array[0..MaxTokenCategories - 1] of SmallInt;  // Registered category IDs
    FCategoryCount : Byte;
  Public
    Kind: TSyntaxHighlightKind;
    // 1-based source line of the first character of the token
    Line: Integer;
    Text: string;
  Private
    function GetCategoryCount: Integer; inline;
  Public
    // Create a syntax token; ESyntaxHighlighter for more than MaxTokenCategories categories or an ID above MaxCategoryID
    Constructor create (const aText : string; aKind : TSyntaxHighlightKind; const aCategories : Array of Integer); overload;
    Constructor create (const aText : string; aKind : TSyntaxHighlightKind; const aCategory : Integer); overload;
    Constructor create (const aText : string; aKind : TSyntaxHighlightKind); overload;
    // Categories for this token
    Function Categories : TIntegerDynArray;
    // Categories as strings
    function CategoriesAsString : String;
    // Add a category; ESyntaxHighlighter if the token has MaxTokenCategories categories or the ID is out of range
    Procedure AddCategory(aCategoryID : integer);
    // Check if a particular category is present
    function HasCategory(aCategoryID : integer) : Boolean;
    // Number of categories associated to this token
    Property CategoryCount : Integer read GetCategoryCount;
  end;
  TSyntaxTokenArray = array of TSyntaxToken;

  // Receives one token from a highlighter
  TSyntaxTokenEvent = procedure(Sender: TObject; const aToken: TSyntaxToken) of object;

  { TSyntaxTokenList }

  TSyntaxTokenList = record
  private
    FList : TSyntaxTokenArray;
    FTokenCount : integer;
    function GetToken(aIndex : Integer): TSyntaxToken;
  Public
    // Create an empty token list
    class function Create : TSyntaxTokenList; static;
    // Add a token
    Procedure AddToken(const aToken : TSyntaxToken);
    // Clear the token list
    procedure reset;
    // Get the actual list of tokens
    Function GetTokens : TSyntaxTokenArray;
    // Indexed access to the tokens
    Property Tokens[aIndex : Integer] : TSyntaxToken Read GetToken;
    // Number of tokens in the list
    Function Count : Integer;
  end;

  { TSyntaxHighlighter }

  TSyntaxHighlighter = class
  private
    const
      SharedTextSlots = 1024;
      MaxSharedTextLength = 16;
    var
    FSharedTexts: array[0..SharedTextSlots - 1] of string;
    FShareTexts: Boolean;
    FToken: TSyntaxToken;
    FDefaultCategory: Integer;
    FOnToken: TSyntaxTokenEvent;
    FRangeFrom: Integer;
    FRangeTo: Integer;
    FLine: Integer;
    FAfterCR: Boolean;
    FStopping: Boolean;
    class var
      FCategories: TStringList;
      FNextCategoryID: Integer;
    procedure StoreToken(Sender: TObject; const aToken: TSyntaxToken);
    // Set aText to aLength bytes of aSource from aStart, shared with earlier identical texts when ShareTexts is set
    procedure AssignSourceText(var aText: string; const aSource: string; aStart, aLength: SizeInt);
    // Set aText to Copy(aSource, aStart, aLength)
    procedure CopySourceText(var aText: string; const aSource: string; aStart, aLength: SizeInt);
    // Set kind, line and categories (DefaultCategory and aCategory) of aToken
    procedure SetTokenFields(var aToken: TSyntaxToken; aKind: TSyntaxHighlightKind; aCategory: Integer); inline;
  protected
    FTokens : TSyntaxTokenList;
    class function GetCategoryID(const category: string): Integer;
    class function GetLanguages : TStringDynArray; virtual; abstract;
    class procedure RegisterDefaultCategories; virtual;
    class constructor init;
    class destructor done;
    // Split aSource in tokens and pass each one to AddToken or EmitToken
    procedure DoExecute(const aSource: string); virtual; abstract;
    // Create a token with the default category and aCategory, and emit it
    procedure AddToken(const aText: string; akind: TSyntaxHighlightKind; aCategory : Integer = 0);
    // Text of aLength bytes of aSource from aStart; with ShareTexts, short texts are shared with earlier identical texts
    function SourceText(const aSource: string; aStart, aLength: SizeInt): string;
    // AddToken with the text of aLength bytes of aSource from aStart
    procedure AddSourceToken(const aSource: string; aStart, aLength: SizeInt; aKind: TSyntaxHighlightKind; aCategory : Integer = 0);
    // Set the line of aToken and pass it to the token handler if it overlaps the range
    procedure EmitToken(var aToken: TSyntaxToken); virtual;
    // True while Execute builds a token array: SourceText then shares short texts
    property ShareTexts : Boolean Read FShareTexts Write FShareTexts;
  public
    class procedure Register;
    class function RegisterCategory(const category: string): Integer;
    class function GetRegisteredCategoryID(const category: string): Integer;
    class procedure GetRegisteredCategories(categories: TStringList);
    class function GetCategoryName(const aCategoryId: Integer): string;
  public
    constructor Create; virtual;
    // Reset the token list and other attributes
    procedure reset; virtual;
    // Split aSource in tokens and return the tokens that overlap the range
    function Execute(const aSource: string): TSyntaxTokenArray; overload;
    // Split aSource in tokens and pass the tokens that overlap the range to aOnToken
    procedure Execute(const aSource: string; aOnToken: TSyntaxTokenEvent); overload;
    // Only emit tokens that overlap source lines aFrom to aTo; -1 removes the limit on that side
    procedure SetRange(aFrom, aTo: Integer);
    // Set this to zero if you don't want a default category to be added
    property DefaultCategory : Integer Read FDefaultCategory Write FDefaultCategory;
    // First line of the range, -1 if there is no lower limit
    property RangeFrom : Integer Read FRangeFrom;
    // Last line of the range, -1 if there is no upper limit
    property RangeTo : Integer Read FRangeTo;
  end;
  TSyntaxHighlighterClass = class of TSyntaxHighlighter;

  { TSyntaxHighlighterRegistry }

  TSyntaxHighlighterRegistry = class
  protected
    Type
    TLanguageDef = class
      language : string;
      highlighter : TSyntaxHighlighterClass;
      constructor create(aLanguage : string; aHighlighter :TSyntaxHighlighterClass);
    end;
  private
    class var _instance : TSyntaxHighlighterRegistry;
  private
    FDefs : TFPObjectHashTable;
  protected
    Function FindLanguageDef(const aLanguage : String) : TLanguageDef;
    class constructor init;
    class destructor done;
  public
    constructor create;
    destructor destroy; override;
    Function FindSyntaxHighlighterClass(const aLanguage : String) : TSyntaxHighlighterClass;
    Function GetSyntaxHighlighterClass(const aLanguage : String) : TSyntaxHighlighterClass;
    function CreateSyntaxHighlighter(const aLanguage : string) : TSyntaxHighlighter;
    Procedure RegisterSyntaxHighlighter(aClass : TSyntaxHighlighterClass; const aLanguages : Array of string);
    Procedure UnRegisterSyntaxHighlighter(aClass : TSyntaxHighlighterClass);
    class property Instance : TSyntaxHighlighterRegistry Read _Instance;
  end;

implementation

const
  HighlightKindNames : Array[TSyntaxHighlightKind] of string = (
    'Default',
    'Invalid',
    'Symbol',
    'Keyword',
    'Comment',
    'Directive',
    'Numbers',
    'Characters',
    'Strings',
    'Assembler',
    'Regex',
    'Interpolation',
    'Escape',
    'RawString',
    'Template',
    'Lambda',
    'Operator',
    'Error',
    'Warning',
    'Attribute',
    'Section',
    'Key'
  );

function TSyntaxHighlightKindHelper.toString : string;

begin
  Result:=HighlightKindNames[Self];
end;

{ TSyntaxToken }

// Raise ESyntaxHighlighter for a category ID that does not fit in a token
procedure InvalidCategoryID(aCategoryID: Integer);

begin
  Raise ESyntaxHighlighter.CreateFmt('Category ID %d out of range [%d..%d]',[aCategoryID,Low(SmallInt),MaxCategoryID]);
end;


// Raise ESyntaxHighlighter for a token with more than MaxTokenCategories categories
procedure TooManyCategories(aCount: Integer);

begin
  Raise ESyntaxHighlighter.CreateFmt('%d categories for one token, at most %d allowed',[aCount,MaxTokenCategories]);
end;


constructor TSyntaxToken.create(const aText: string; aKind: TSyntaxHighlightKind; const aCategories: array of Integer);
var
  I : integer;
begin
  if Length(aCategories)>MaxTokenCategories then
    TooManyCategories(Length(aCategories));
  For I:=0 to Length(aCategories)-1 do
    begin
    if (aCategories[I]<Low(SmallInt)) or (aCategories[I]>MaxCategoryID) then
      InvalidCategoryID(aCategories[I]);
    FCategories[I]:=aCategories[I];
    end;
  FCategoryCount:=Length(aCategories);
  Text:=aText;
  Kind:=aKind;
  Line:=0;
end;

constructor TSyntaxToken.create(const aText: string; aKind: TSyntaxHighlightKind; const aCategory: Integer);

begin
  if (aCategory<Low(SmallInt)) or (aCategory>MaxCategoryID) then
    InvalidCategoryID(aCategory);
  FCategories[0]:=aCategory;
  FCategoryCount:=1;
  Text:=aText;
  Kind:=aKind;
  Line:=0;
end;

constructor TSyntaxToken.create(const aText: string; aKind: TSyntaxHighlightKind);
begin
  Text:=aText;
  Kind:=aKind;
  Line:=0;
  FCategoryCount:=0;
end;

function TSyntaxToken.GetCategoryCount: Integer;
begin
  Result:=FCategoryCount;
end;

function TSyntaxToken.Categories: TIntegerDynArray;
var
  I : integer;
begin
  Result:=[];
  SetLength(Result,FCategoryCount);
  For I:=0 to FCategoryCount-1 do
    Result[I]:=FCategories[I];
end;

function TSyntaxToken.CategoriesAsString: String;
var
  I : integer;
begin
  Result:='';
  For I:=0 to FCategoryCount-1 do
    begin
    if i>0 then
      Result:=Result+' ';
    Result:=Result+TSyntaxHighlighter.GetCategoryName(FCategories[i]);
    end;
end;

procedure TSyntaxToken.AddCategory(aCategoryID: integer);
begin
  if FCategoryCount>=MaxTokenCategories then
    TooManyCategories(FCategoryCount+1);
  if (aCategoryID<Low(SmallInt)) or (aCategoryID>MaxCategoryID) then
    InvalidCategoryID(aCategoryID);
  FCategories[FCategoryCount]:=aCategoryID;
  inc(FCategoryCount);
end;

function TSyntaxToken.HasCategory(aCategoryID: integer): Boolean;
var
  I : integer;
begin
  Result:=False;
  I:=FCategoryCount-1;
  While (not Result) and (I>=0) do
    begin
    Result:=FCategories[I]=aCategoryID;
    Dec(I);
    end;
end;

{ TSyntaxTokenList }

function TSyntaxTokenList.GetToken(aIndex : Integer): TSyntaxToken;
begin
  if (aIndex<0) or (aIndex>=FTokenCount) then
    Raise ESyntaxHighlighter.CreateFmt('Index %d out of bounds [0..%d[',[aIndex,FTokenCount]);
  Result:=FList[aIndex];
end;

class function TSyntaxTokenList.Create: TSyntaxTokenList;
begin
  Result:=Default(TSyntaxTokenList);
end;

procedure TSyntaxTokenList.AddToken(const aToken: TSyntaxToken);
var
  lSlot : ^TSyntaxToken;
begin
  if FTokenCount=Length(FList) then
    if FTokenCount<64 then
      SetLength(FList,64)
    else
      SetLength(FList,FTokenCount*2);
  // Assign field by field instead of the whole record
  lSlot:=@FList[FTokenCount];
  lSlot^.Text:=aToken.Text;
  lSlot^.Kind:=aToken.Kind;
  lSlot^.Line:=aToken.Line;
  lSlot^.FCategories:=aToken.FCategories;
  lSlot^.FCategoryCount:=aToken.FCategoryCount;
  Inc(FTokenCount);
end;

procedure TSyntaxTokenList.reset;
begin
  FList:=[];
  FTokenCount:=0;
end;

function TSyntaxTokenList.GetTokens: TSyntaxTokenArray;
begin
  SetLength(FList,FTokenCount);
  Result:=Flist;
  Reset;
end;

function TSyntaxTokenList.Count: Integer;
begin
  Result:=FTokenCount;
end;

class constructor TSyntaxHighlighter.init;
begin
  FCategories := TStringList.Create;
  FCategories.Sorted := True;
  FCategories.Duplicates := dupIgnore;
  FNextCategoryID := 1;
end;

class destructor TSyntaxHighlighter.done;
begin
  FCategories.Free;
end;

procedure TSyntaxHighlighter.SetTokenFields(var aToken: TSyntaxToken; aKind: TSyntaxHighlightKind; aCategory: Integer);
var
  lCat1,lCat2 : Integer;

begin
  lCat2:=0;
  lCat1:=DefaultCategory;
  if aCategory<>0 then
    if lCat1=0 then
      lCat1:=aCategory
    else
      lCat2:=aCategory;
  if (lCat1<Low(SmallInt)) or (lCat1>MaxCategoryID) then
    InvalidCategoryID(lCat1);
  if (lCat2<Low(SmallInt)) or (lCat2>MaxCategoryID) then
    InvalidCategoryID(lCat2);
  aToken.Kind:=aKind;
  aToken.Line:=0;
  aToken.FCategories[0]:=lCat1;
  aToken.FCategories[1]:=lCat2;
  if lCat2>0 then
    aToken.FCategoryCount:=2
  else if lCat1>0 then
    aToken.FCategoryCount:=1
  else
    aToken.FCategoryCount:=0;
end;


procedure TSyntaxHighlighter.AddToken(const aText: string; akind: TSyntaxHighlightKind; aCategory: Integer);

begin
  if Length(aText) = 0 then Exit;
  FToken.Text:=aText;
  SetTokenFields(FToken,aKind,aCategory);
  EmitToken(FToken);
end;


procedure TSyntaxHighlighter.CopySourceText(var aText: string; const aSource: string; aStart, aLength: SizeInt);

begin
  aText := Copy(aSource, aStart, aLength);
end;


{$push}{$Q-}{$R-}
procedure TSyntaxHighlighter.AssignSourceText(var aText: string; const aSource: string; aStart, aLength: SizeInt);

var
  I: Integer;
  lText: PChar;
  lHash: DWord;
  lSlot: Integer;
begin
  if (aLength <= 0) or (aStart < 1) or (aStart + aLength - 1 > Length(aSource)) then
    begin
    CopySourceText(aText, aSource, aStart, aLength);
    Exit;
    end;
  lText := PChar(Pointer(aSource)) + aStart - 1;
  lSlot := -1;
  if FShareTexts and (aLength <= MaxSharedTextLength) then
    begin
    lHash := aLength;
    for I := 0 to aLength - 1 do
      lHash := lHash * 31 + Ord(lText[I]);
    lSlot := (lHash xor (lHash shr 11)) and (SharedTextSlots - 1);
    if (Length(FSharedTexts[lSlot]) = aLength)
       and (CompareByte(Pointer(FSharedTexts[lSlot])^, lText^, aLength) = 0)
       and (StringCodePage(FSharedTexts[lSlot]) = StringCodePage(aSource)) then
      begin
      aText := FSharedTexts[lSlot];
      Exit;
      end;
    end;
  SetString(aText, lText, aLength);
  if StringCodePage(aText) <> StringCodePage(aSource) then
    SetCodePage(RawByteString(aText), StringCodePage(aSource), False);
  if lSlot >= 0 then
    FSharedTexts[lSlot] := aText;
end;
{$pop}


function TSyntaxHighlighter.SourceText(const aSource: string; aStart, aLength: SizeInt): string;

begin
  AssignSourceText(Result, aSource, aStart, aLength);
end;


procedure TSyntaxHighlighter.AddSourceToken(const aSource: string; aStart, aLength: SizeInt; aKind: TSyntaxHighlightKind; aCategory: Integer);

begin
  if aLength <= 0 then
    Exit;
  AssignSourceText(FToken.Text, aSource, aStart, aLength);
  if Length(FToken.Text) = 0 then
    Exit;
  SetTokenFields(FToken,aKind,aCategory);
  EmitToken(FToken);
end;


procedure TSyntaxHighlighter.EmitToken(var aToken: TSyntaxToken);

var
  lFirst, lLast, lLine, I: Integer;
  lAfterCR: Boolean;
  lText: PChar;
  lChar: Char;
begin
  lLine:=FLine;
  lAfterCR:=FAfterCR;
  lFirst:=lLine;
  lLast:=lLine;
  lText:=PChar(Pointer(aToken.Text));
  for I:=0 to Length(aToken.Text)-1 do
    begin
    lChar:=lText[I];
    lLast:=lLine;
    if lChar>#13 then
      lAfterCR:=False
    else
      begin
      if (lChar=#13) or ((lChar=#10) and not lAfterCR) then
        Inc(lLine);
      lAfterCR:=(lChar=#13);
      end;
    end;
  FLine:=lLine;
  FAfterCR:=lAfterCR;
  if (FRangeTo<>-1) and (lFirst>FRangeTo) then
    begin
    FStopping:=True;
    Abort;
    end;
  if (FRangeFrom<>-1) and (lLast<FRangeFrom) then
    Exit;
  aToken.Line:=lFirst;
  if Assigned(FOnToken) then
    FOnToken(Self,aToken);
end;


procedure TSyntaxHighlighter.StoreToken(Sender: TObject; const aToken: TSyntaxToken);

begin
  FTokens.AddToken(aToken);
end;


function TSyntaxHighlighter.Execute(const aSource: string): TSyntaxTokenArray;

begin
  FTokens.Reset;
  FShareTexts:=True;
  try
    Execute(aSource,@StoreToken);
  finally
    FShareTexts:=False;
  end;
  Result:=FTokens.GetTokens;
end;


procedure TSyntaxHighlighter.Execute(const aSource: string; aOnToken: TSyntaxTokenEvent);

begin
  FOnToken:=aOnToken;
  FLine:=1;
  FAfterCR:=False;
  FStopping:=False;
  try
    try
      DoExecute(aSource);
    except
      on EAbort do
        if not FStopping then
          raise;
    end;
  finally
    FOnToken:=nil;
    FStopping:=False;
    FToken.Text:='';
  end;
end;


procedure TSyntaxHighlighter.SetRange(aFrom, aTo: Integer);

begin
  if ((aFrom<>-1) and (aFrom<1)) or ((aTo<>-1) and (aTo<1))
     or ((aFrom<>-1) and (aTo<>-1) and (aFrom>aTo)) then
    raise ESyntaxHighlighter.CreateFmt('Invalid line range %d..%d',[aFrom,aTo]);
  FRangeFrom:=aFrom;
  FRangeTo:=aTo;
end;

class function TSyntaxHighlighter.GetCategoryID(const category: string): Integer;
var
  index: Integer;
begin
  if FCategories.Find(category, index) then
    Result := PtrInt(FCategories.Objects[index])
  else
    Result := -1;
end;

class procedure TSyntaxHighlighter.RegisterDefaultCategories;
begin
  // Do nothing
end;

constructor TSyntaxHighlighter.Create;
begin
  Inherited;
  FRangeFrom:=-1;
  FRangeTo:=-1;
  Reset;
end;

procedure TSyntaxHighlighter.reset;
begin
  FTokens.Reset;
end;

class procedure TSyntaxHighlighter.Register;
begin
  TSyntaxHighlighterRegistry.Instance.RegisterSyntaxHighlighter(Self,GetLanguages);
end;

class function TSyntaxHighlighter.RegisterCategory(const category: string): Integer;
var
  existingID: Integer;
begin
  existingID := GetCategoryID(category);
  if existingID <> -1 then
    Result := existingID
  else
    begin
    if FNextCategoryID>MaxCategoryID then
      Raise ESyntaxHighlighter.CreateFmt('Cannot register category "%s": more than %d categories',[category,MaxCategoryID]);
    Result := FNextCategoryID;
    FCategories.AddObject(category, TObject(PtrInt(Result)));
    Inc(FNextCategoryID);
    end;
end;

class function TSyntaxHighlighter.GetRegisteredCategoryID(const category: string): Integer;
begin
  Result := GetCategoryID(category);
end;

class procedure TSyntaxHighlighter.GetRegisteredCategories(categories: TStringList);
var
  i: Integer;
begin
  categories.Clear;
  for i := 0 to FCategories.Count - 1 do
    categories.AddObject(FCategories[i], FCategories.Objects[i]);
end;

class function TSyntaxHighlighter.GetCategoryName(const aCategoryId: Integer): string;
var
  i: Integer;
begin
  Result:='';
  if aCategoryID=0 then
    exit;
  for i := FCategories.Count - 1 downto 0 do
    begin
    if PtrInt(FCategories.Objects[i]) = aCategoryID then
       Exit(FCategories[i]);
    end;
end;

{ TSyntaxHighlighterRegistry }


class constructor TSyntaxHighlighterRegistry.init;
begin
  _instance:=TSyntaxHighlighterRegistry.Create;
end;

class destructor TSyntaxHighlighterRegistry.done;
begin
  FreeAndNil(_instance);
end;

constructor TSyntaxHighlighterRegistry.create;
begin
  FDefs:=TFPObjectHashTable.Create(True);
end;

destructor TSyntaxHighlighterRegistry.destroy;
begin
  FReeAndNil(FDefs);
  inherited destroy;
end;

function TSyntaxHighlighterRegistry.FindLanguageDef(const aLanguage: String): TLanguageDef;

begin
  Result:=TLanguageDef(FDefs.Items[aLanguage]);
end;

function TSyntaxHighlighterRegistry.FindSyntaxHighlighterClass(const aLanguage: String): TSyntaxHighlighterClass;
var
  lDef : TLanguageDef;
begin
  Result:=Nil;
  lDef:=FindLanguageDef(aLanguage);
  if assigned(lDef) then
    Result:=lDef.highlighter;
end;

function TSyntaxHighlighterRegistry.GetSyntaxHighlighterClass(const aLanguage: String): TSyntaxHighlighterClass;

begin
  Result:=FindSyntaxHighlighterClass(aLanguage);
  if (Result=Nil) then
    Raise ESyntaxHighlighter.CreateFmt('No highlighter for language "%s"',[aLanguage]);
end;

function TSyntaxHighlighterRegistry.CreateSyntaxHighlighter(const aLanguage: string): TSyntaxHighlighter;
var
  lClass : TSyntaxHighlighterClass;
begin
  lClass:=GetSyntaxHighlighterClass(aLanguage);
  Result:=lClass.Create;
end;

procedure TSyntaxHighlighterRegistry.RegisterSyntaxHighlighter(aClass: TSyntaxHighlighterClass; const aLanguages: array of string);
var
  lLanguage : String;
  lDef : TLanguageDef;
begin
  aClass.RegisterDefaultCategories;
  For lLanguage in aLanguages do
    begin
    lDef:=FindLanguageDef(lLanguage);
    if lDef=Nil then
      begin
      lDef:=TLanguageDef.Create(lLanguage,aClass);
      FDefs.Add(lLanguage,lDef);
      end
    else
      lDef.highlighter:=aClass;
    end;
end;

procedure TSyntaxHighlighterRegistry.UnRegisterSyntaxHighlighter(aClass: TSyntaxHighlighterClass);
var
  lLanguage : string;
begin
  For lLanguage in aClass.GetLanguages do
    FDefs.Delete(lLanguage);
end;

{ TSyntaxHighlighterRegistry.TLanguageDef }

constructor TSyntaxHighlighterRegistry.TLanguageDef.create(aLanguage: string; aHighlighter: TSyntaxHighlighterClass);
begin
  Language:=aLanguage;
  HighLighter:=aHighlighter;
end;

end.

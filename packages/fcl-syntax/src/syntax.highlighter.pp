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

  { TSyntaxToken }

  // One syntax token
  TSyntaxToken = record
  Private
    FCategoryCount : integer;
    FCategories: TIntegerDynArray;  // Registered category IDs
  Public
    Text: string;
    Kind: TSyntaxHighlightKind;
    // Create a syntax token
    Constructor create (const aText : string; aKind : TSyntaxHighlightKind; const aCategories : Array of Integer); overload;
    Constructor create (const aText : string; aKind : TSyntaxHighlightKind; const aCategory : Integer); overload;
    Constructor create (const aText : string; aKind : TSyntaxHighlightKind); overload;
    // Categories for this token
    Function Categories : TIntegerDynArray;
    // Categories as strings
    function CategoriesAsString : String;
    // Add a category
    Procedure AddCategory(aCategoryID : integer);
    // Check if a particular category is present
    function HasCategory(aCategoryID : integer) : Boolean;
    // Number of categories associated to this token
    Property CategoryCount : Integer read FCategoryCount;
  end;
  TSyntaxTokenArray = array of TSyntaxToken;

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
    Procedure AddToken(aToken : TSyntaxToken);
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
    FDefaultCategory: Integer;
    class var
      FCategories: TStringList;
      FNextCategoryID: Integer;
  protected
    FTokens : TSyntaxTokenList;
    class function GetCategoryID(const category: string): Integer;
    class function GetLanguages : TStringDynArray; virtual; abstract;
    class procedure RegisterDefaultCategories; virtual;
    class constructor init;
    class destructor done;
    procedure AddToken(const aText: string; akind: TSyntaxHighlightKind; aCategory : Integer = 0);
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
    // Start the tokenizing process, returns array of syntax tokens.
    function Execute(const source: string): TSyntaxTokenArray; virtual; abstract;
    // Set this to zero if you don't want a default category to be added
    property DefaultCategory : Integer Read FDefaultCategory Write FDefaultCategory;
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

constructor TSyntaxToken.create(const aText: string; aKind: TSyntaxHighlightKind; const aCategories: array of Integer);
var
  I : integer;
begin
  Text:=aText;
  Kind:=aKind;
  FCategoryCount:=Length(aCategories);
  SetLength(FCategories,FCategoryCount+1);
  For I:=0 to Length(aCategories)-1 do
    FCategories[I]:=aCategories[i];
end;

constructor TSyntaxToken.create(const aText: string; aKind: TSyntaxHighlightKind; const aCategory: Integer);

begin
  Text:=aText;
  Kind:=aKind;
  FCategoryCount:=1;
  SetLength(FCategories,2);
  FCategories[0]:=aCategory;
end;

constructor TSyntaxToken.create(const aText: string; aKind: TSyntaxHighlightKind);
begin
  Text:=aText;
  Kind:=aKind;
  FCategoryCount:=0;
end;

function TSyntaxToken.Categories: TIntegerDynArray;
begin
  Result:=Copy(FCategories,0,CategoryCount);
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
  if FCategoryCount=Length(Categories) then
    SetLength(FCategories,FCategoryCount+3);
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

procedure TSyntaxTokenList.AddToken(aToken: TSyntaxToken);
begin
  if FTokenCount=Length(FList) then
    SetLength(FList,FTokenCount+100);
  FList[FTokenCount]:=aToken;
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

procedure TSyntaxHighlighter.AddToken(const aText: string; akind: TSyntaxHighlightKind; aCategory: Integer);
var
  lCat1,lCat2 : Integer;
  lToken : TSyntaxToken;

begin
  if Length(aText) = 0 then Exit;
  lCat2:=0;
  lCat1:=DefaultCategory;
  if aCategory<>0 then
    if lCat1=0 then
      lCat1:=aCategory
    else
      lCat2:=aCategory;
 if lCat2>0 then
    lToken:=TSyntaxToken.Create(aText,aKind,[lCat1,lCat2])
 else if lCat1>0 then
    lToken:=TSyntaxToken.Create(aText,aKind,lCat1)
 else
    lToken:=TSyntaxToken.Create(aText,aKind);
  FTokens.AddToken(ltoken);
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

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Byte scanner and table driven syntax highlighter base classes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.table;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter;
  {$ELSE}
  Types, SysUtils, syntax.highlighter;
  {$ENDIF}

const
  NameStartChars = ['A'..'Z', 'a'..'z', '_', #128..#255];
  NameChars = NameStartChars + ['0'..'9'];

type

  { TByteSyntaxHighlighter }

  TByteSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FOutput: TSyntaxTokenArray;
    FCount: SizeInt;
    FCollect: boolean;
    // Store aText as a token of kind aKind
    procedure CollectText(const aText: string; aKind: TSyntaxHighlightKind);
    // Store the text from aFirst up to the cursor as a token of kind aKind
    procedure CollectToken(aFirst: SizeInt; aKind: TSyntaxHighlightKind);
    // Emit aText as a token of kind aKind, or store it when collecting
    procedure EmitText(const aText: string; aKind: TSyntaxHighlightKind);
  protected
    FText: string;
    FCursor: SizeInt;
    FLimit: SizeInt;
    // Category name registered as default category: the first language name
    class function CategoryName: string; virtual;
    // Prepare scanning aSource; with aCollect, Emit stores the tokens instead of emitting them
    procedure Start(const aSource: string; aCollect: boolean = False);
    // Return the stored tokens and release the source
    function Finish: TSyntaxTokenArray;
    // Character at the cursor plus aOffset, #0 outside the text
    function Ch(aOffset: SizeInt = 0): char; inline;
    // True if aValue occurs at the cursor plus aOffset
    function At(const aValue: string; aOffset: SizeInt = 0): boolean; inline;
    // Emit the text from aFirst up to the cursor as a token of kind aKind
    procedure Emit(aFirst: SizeInt; aKind: TSyntaxHighlightKind);
    // Move the cursor to the end of the line
    procedure LineEnd;
    // Move the cursor past a quoted text
    procedure Quoted(const aOpenText, aCloseText: string; aEscape: char; aMultiline, aDoubled: boolean);
    // Move the cursor past a delimited block, optionally nested
    procedure Block(const aOpenText, aCloseText: string; aNested: boolean);
    // Move the cursor past whitespace
    procedure White;
    // Move the cursor past one character, including UTF-8 continuation bytes
    procedure Unknown;
  public
    constructor Create; override;
  end;

  TCommentForm = record
    OpenText, CloseText: string;
    Nested, LineStartOnly, WordBoundary: boolean;
  end;

  TStringForm = record
    OpenText, CloseText: string;
    Escape: char;
    Multiline, Doubled: boolean;
    Kind: TSyntaxHighlightKind;
  end;

  TNumberForms = record
    Hex, Binary, Octal, LeadingOctal, Separators, Fraction, Exponent,
    HexExponent, LeadingDot, Based, TrailingBase: boolean;
    Separator: char;
    ExponentLetters, Suffixes: string;
  end;

  TLanguageFlavor = (lfPlain, lfCpp, lfCSharp, lfRust, lfSwift, lfLua, lfZig,
    lfBasic, lfAda, lfBatch, lfDocker, lfToml, lfRuby, lfPerl, lfPhp,
    lfPowerShell, lfFortran, lfAssembly);

  TLanguageDescription = record
    Names, Keywords, Builtins: string;
    CaseSensitive: boolean;
    Comments: array of TCommentForm;
    Strings: array of TStringForm;
    Numbers: TNumberForms;
    Directive, Attribute: string;
    IdentifierStart, IdentifierRest, Operators, Symbols: TSysCharSet;
    Flavor: TLanguageFlavor;
  end;

  { TSyntaxWordTable }

  // Hash set of words, with a token kind per word
  TSyntaxWordTable = class
  private
    FKeys: array of string;
    FKinds: array of TSyntaxHighlightKind;
    FCount: SizeInt;
    FMask: SizeInt;
    FCaseSensitive: boolean;
    function HashOf(aValue: PChar; aLength: Integer): SizeInt;
    function SameWord(const aKey: string; aValue: PChar; aLength: Integer): boolean;
    function IndexOf(aValue: PChar; aLength: Integer): SizeInt;
    procedure Insert(const aKey: string; aKind: TSyntaxHighlightKind);
    procedure Grow;
  public
    // Create an empty table; without aCaseSensitive, words match in any ASCII case
    constructor Create(aCaseSensitive: boolean);
    // Add the space separated words of aWords with kind aKind; a word already present keeps its kind
    procedure AddWords(const aWords: string; aKind: TSyntaxHighlightKind);
    // Kind of aValue, aDefault if aValue is not in the table
    function Lookup(const aValue: string; aDefault: TSyntaxHighlightKind = shDefault): TSyntaxHighlightKind; overload;
    // Kind of the aLength characters at aValue, aDefault if they are not in the table
    function Lookup(aValue: PChar; aLength: Integer; aDefault: TSyntaxHighlightKind = shDefault): TSyntaxHighlightKind; overload;
    // True if aValue is in the table
    function Contains(const aValue: string): boolean;
    // Number of words in the table
    property Count: SizeInt read FCount;
  end;

  { TTableSyntaxHighlighter }

  TTableSyntaxHighlighter = class(TByteSyntaxHighlighter)
  private
    FDescription: TLanguageDescription;
    FWords: TSyntaxWordTable;
    FWordKindOverridden: boolean;
    FLineStart: boolean;
    FSpecialStart: TSysCharSet;
    FCommentStart: TSysCharSet;
    FStringStart: TSysCharSet;
    function CommentToken: boolean;
    function AtFolded(const aValue: string): boolean;
    function StringToken: boolean;
    function SpecialString: boolean; inline;
    function FlavorString: boolean;
    procedure NumberToken;
  protected
    // The lexical description of the language
    class function Describe: TLanguageDescription; virtual; abstract;
    class function GetLanguages: TStringDynArray; override;
    // Token kind of an identifier
    function WordKind(const aValue: string): TSyntaxHighlightKind; virtual;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

// Language description with default identifier, operator, symbol and number forms
function Language(const aNames, aKeywords, aBuiltins: string): TLanguageDescription;
// Add a comment running to the end of the line
procedure LineComment(var aDesc: TLanguageDescription; const aOpenText: string; aLineStartOnly: boolean = False; aWordBoundary: boolean = False);
// Add a delimited comment
procedure BlockComment(var aDesc: TLanguageDescription; const aOpenText, aCloseText: string; aNested: boolean = False);
// Add a string literal form
procedure StringForm(var aDesc: TLanguageDescription; const aOpenText, aCloseText: string; aEscape: char = '\'; aMultiline: boolean = False;
  aDoubled: boolean = False; aKind: TSyntaxHighlightKind = shStrings);
// True if aValue is one of the space separated words in aWords
function WordsContain(const aWords, aValue: string): boolean;
// Split a space separated list of names
function LanguageNames(const aNames: string): TStringDynArray;

implementation

function WordsContain(const aWords, aValue: string): boolean;

var
  lPos, lLen: SizeInt;
begin
  Result := False;
  lLen := Length(aValue);
  if (lLen = 0) or (lLen > 64) then
    Exit;
  lPos := Pos(aValue, aWords);
  while lPos > 0 do
    begin
    if ((lPos = 1) or (aWords[lPos - 1] = ' '))
       and ((lPos + lLen > Length(aWords)) or (aWords[lPos + lLen] = ' ')) then
      Exit(True);
    lPos := Pos(aValue, aWords, lPos + 1);
    end;
end;


function LanguageNames(const aNames: string): TStringDynArray;

var
  lPos, lFirst, lCount: SizeInt;
begin
  Result := nil;
  lCount := 0;
  lPos := 1;
  while lPos <= Length(aNames) do
    begin
    while (lPos <= Length(aNames)) and (aNames[lPos] = ' ') do
      Inc(lPos);
    lFirst := lPos;
    while (lPos <= Length(aNames)) and (aNames[lPos] <> ' ') do
      Inc(lPos);
    if lPos > lFirst then
      begin
      SetLength(Result, lCount + 1);
      Result[lCount] := Copy(aNames, lFirst, lPos - lFirst);
      Inc(lCount);
      end;
    end;
end;


function Language(const aNames, aKeywords, aBuiltins: string): TLanguageDescription;

begin
  Result := Default(TLanguageDescription);
  Result.Names := aNames;
  Result.Keywords := aKeywords;
  Result.Builtins := aBuiltins;
  Result.CaseSensitive := True;
  Result.IdentifierStart := NameStartChars;
  Result.IdentifierRest := NameChars;
  Result.Operators := ['+', '-', '*', '/', '%', '=', '<', '>', '!', '&', '|', '^', '~', '?'];
  Result.Symbols := ['(', ')', '[', ']', '{', '}', '.', ',', ':', ';'];
  Result.Numbers.Fraction := True;
  Result.Numbers.LeadingDot := True;
  Result.Numbers.Exponent := True;
  Result.Numbers.ExponentLetters := 'eE';
  Result.Numbers.Separator := '_';
end;


procedure LineComment(var aDesc: TLanguageDescription; const aOpenText: string; aLineStartOnly: boolean; aWordBoundary: boolean);

var
  lCount: SizeInt;
begin
  lCount := Length(aDesc.Comments);
  SetLength(aDesc.Comments, lCount + 1);
  aDesc.Comments[lCount].OpenText := aOpenText;
  aDesc.Comments[lCount].LineStartOnly := aLineStartOnly;
  aDesc.Comments[lCount].WordBoundary := aWordBoundary;
end;


procedure BlockComment(var aDesc: TLanguageDescription; const aOpenText, aCloseText: string; aNested: boolean);

var
  lCount: SizeInt;
begin
  lCount := Length(aDesc.Comments);
  SetLength(aDesc.Comments, lCount + 1);
  aDesc.Comments[lCount].OpenText := aOpenText;
  aDesc.Comments[lCount].CloseText := aCloseText;
  aDesc.Comments[lCount].Nested := aNested;
end;


procedure StringForm(var aDesc: TLanguageDescription; const aOpenText, aCloseText: string; aEscape: char; aMultiline, aDoubled: boolean;
  aKind: TSyntaxHighlightKind);

var
  lCount: SizeInt;
begin
  lCount := Length(aDesc.Strings);
  SetLength(aDesc.Strings, lCount + 1);
  aDesc.Strings[lCount].OpenText := aOpenText;
  aDesc.Strings[lCount].CloseText := aCloseText;
  aDesc.Strings[lCount].Escape := aEscape;
  aDesc.Strings[lCount].Multiline := aMultiline;
  aDesc.Strings[lCount].Doubled := aDoubled;
  aDesc.Strings[lCount].Kind := aKind;
end;


{ TByteSyntaxHighlighter }

class function TByteSyntaxHighlighter.CategoryName: string;

var
  lNames: TStringDynArray;
begin
  lNames := GetLanguages;
  if Length(lNames) > 0 then
    Result := lNames[0]
  else
    Result := '';
end;


constructor TByteSyntaxHighlighter.Create;

var
  lName: string;
begin
  inherited Create;
  lName := CategoryName;
  if lName <> '' then
    DefaultCategory := RegisterCategory(lName);
end;


procedure TByteSyntaxHighlighter.Start(const aSource: string; aCollect: boolean);

begin
  FText := aSource;
  FCursor := 1;
  FLimit := Length(FText);
  FOutput := nil;
  FCount := 0;
  FCollect := aCollect;
end;


function TByteSyntaxHighlighter.Finish: TSyntaxTokenArray;

begin
  SetLength(FOutput, FCount);
  Result := FOutput;
  FOutput := nil;
  FText := '';
end;


function TByteSyntaxHighlighter.Ch(aOffset: SizeInt): char;

begin
  if (FCursor + aOffset >= 1) and (FCursor + aOffset <= FLimit) then
    Result := FText[FCursor + aOffset]
  else
    Result := #0;
end;


function TByteSyntaxHighlighter.At(const aValue: string; aOffset: SizeInt): boolean;

var
  lPos: SizeInt;
begin
  lPos := FCursor + aOffset;
  Result := (Length(aValue) > 0) and (lPos >= 1) and (Length(aValue) <= FLimit - lPos + 1)
            and (FText[lPos] = aValue[1])
            and ((Length(aValue) = 1)
                 or (CompareByte(PChar(Pointer(FText))[lPos], PChar(Pointer(aValue))[1], Length(aValue) - 1) = 0));
end;


procedure TByteSyntaxHighlighter.Emit(aFirst: SizeInt; aKind: TSyntaxHighlightKind);

begin
  if FCursor <= aFirst then
    Exit;
  if FCollect then
    CollectToken(aFirst, aKind)
  else
    AddSourceToken(FText, aFirst, FCursor - aFirst, aKind);
end;


procedure TByteSyntaxHighlighter.CollectText(const aText: string; aKind: TSyntaxHighlightKind);

begin
  if FCount = Length(FOutput) then
    SetLength(FOutput, FCount * 2 + 64);
  if DefaultCategory > 0 then
    FOutput[FCount] := TSyntaxToken.Create(aText, aKind, DefaultCategory)
  else
    FOutput[FCount] := TSyntaxToken.Create(aText, aKind);
  Inc(FCount);
end;


procedure TByteSyntaxHighlighter.CollectToken(aFirst: SizeInt; aKind: TSyntaxHighlightKind);

begin
  CollectText(SourceText(FText, aFirst, FCursor - aFirst), aKind);
end;


procedure TByteSyntaxHighlighter.EmitText(const aText: string; aKind: TSyntaxHighlightKind);

begin
  if aText = '' then
    Exit;
  if FCollect then
    CollectText(aText, aKind)
  else
    AddToken(aText, aKind);
end;


procedure TByteSyntaxHighlighter.LineEnd;

begin
  while (FCursor <= FLimit) and not (Ch in [#10, #13]) do
    Inc(FCursor);
end;


procedure TByteSyntaxHighlighter.White;

begin
  while (FCursor <= FLimit) and (Ch in [#9, #10, #13, ' ']) do
    Inc(FCursor);
end;


procedure TByteSyntaxHighlighter.Unknown;

begin
  Inc(FCursor);
  while (FCursor <= FLimit) and (Ch >= #128) do
    Inc(FCursor);
end;


procedure TByteSyntaxHighlighter.Quoted(const aOpenText, aCloseText: string; aEscape: char; aMultiline, aDoubled: boolean);

begin
  Inc(FCursor, Length(aOpenText));
  while FCursor <= FLimit do
    begin
    if not aMultiline and (Ch in [#10, #13]) then
      Break;
    if At(aCloseText) then
      begin
      Inc(FCursor, Length(aCloseText));
      if aDoubled and At(aCloseText) then
        Inc(FCursor, Length(aCloseText))
      else
        Break;
      end
    else if (aEscape <> #0) and (Ch = aEscape) then
      begin
      Inc(FCursor);
      if FCursor <= FLimit then
        begin
        if not aMultiline and (Ch in [#10, #13]) and At(#13#10) then
          Inc(FCursor);
        Inc(FCursor);
        end;
      end
    else
      Inc(FCursor);
    end;
end;


procedure TByteSyntaxHighlighter.Block(const aOpenText, aCloseText: string; aNested: boolean);

var
  lDepth: SizeInt;
begin
  lDepth := 1;
  Inc(FCursor, Length(aOpenText));
  while FCursor <= FLimit do
    begin
    if At(aCloseText) then
      begin
      Inc(FCursor, Length(aCloseText));
      Dec(lDepth);
      if lDepth = 0 then
        Break;
      end
    else if aNested and At(aOpenText) then
      begin
      Inc(lDepth);
      Inc(FCursor, Length(aOpenText));
      end
    else
      Inc(FCursor);
    end;
end;


{ TSyntaxWordTable }

constructor TSyntaxWordTable.Create(aCaseSensitive: boolean);

begin
  inherited Create;
  FCaseSensitive := aCaseSensitive;
  FMask := 15;
  SetLength(FKeys, FMask + 1);
  SetLength(FKinds, FMask + 1);
end;


{$push}{$Q-}{$R-}
function TSyntaxWordTable.HashOf(aValue: PChar; aLength: Integer): SizeInt;

var
  I: Integer;
  lHash: DWord;
  lChar: char;
begin
  lHash := 2166136261;
  for I := 0 to aLength - 1 do
    begin
    lChar := aValue[I];
    if not FCaseSensitive and (lChar in ['A'..'Z']) then
      lChar := Chr(Ord(lChar) + 32);
    lHash := (lHash xor Ord(lChar)) * 16777619;
    end;
  Result := SizeInt(lHash) and FMask;
end;
{$pop}


function TSyntaxWordTable.SameWord(const aKey: string; aValue: PChar; aLength: Integer): boolean;

var
  I: Integer;
  lChar: char;
begin
  if Length(aKey) <> aLength then
    Exit(False);
  if FCaseSensitive then
    Exit(CompareByte(aKey[1], aValue^, aLength) = 0);
  for I := 0 to aLength - 1 do
    begin
    lChar := aValue[I];
    if lChar in ['A'..'Z'] then
      lChar := Chr(Ord(lChar) + 32);
    if aKey[I + 1] <> lChar then
      Exit(False);
    end;
  Result := True;
end;


function TSyntaxWordTable.IndexOf(aValue: PChar; aLength: Integer): SizeInt;

begin
  if (FCount = 0) or (aLength = 0) then
    Exit(-1);
  Result := HashOf(aValue, aLength);
  while FKeys[Result] <> '' do
    begin
    if SameWord(FKeys[Result], aValue, aLength) then
      Exit;
    Result := (Result + 1) and FMask;
    end;
  Result := -1;
end;


procedure TSyntaxWordTable.Insert(const aKey: string; aKind: TSyntaxHighlightKind);

var
  lSlot: SizeInt;
begin
  lSlot := HashOf(PChar(aKey), Length(aKey));
  while FKeys[lSlot] <> '' do
    lSlot := (lSlot + 1) and FMask;
  FKeys[lSlot] := aKey;
  FKinds[lSlot] := aKind;
end;


procedure TSyntaxWordTable.Grow;

var
  lKeys: array of string;
  lKinds: array of TSyntaxHighlightKind;
  I: Integer;
begin
  lKeys := FKeys;
  lKinds := FKinds;
  FMask := FMask * 2 + 1;
  FKeys := nil;
  FKinds := nil;
  SetLength(FKeys, FMask + 1);
  SetLength(FKinds, FMask + 1);
  for I := 0 to High(lKeys) do
    if lKeys[I] <> '' then
      Insert(lKeys[I], lKinds[I]);
end;


procedure TSyntaxWordTable.AddWords(const aWords: string; aKind: TSyntaxHighlightKind);

var
  lPos, lFirst: SizeInt;
  lWord: string;
begin
  lPos := 1;
  while lPos <= Length(aWords) do
    begin
    while (lPos <= Length(aWords)) and (aWords[lPos] = ' ') do
      Inc(lPos);
    lFirst := lPos;
    while (lPos <= Length(aWords)) and (aWords[lPos] <> ' ') do
      Inc(lPos);
    if (lPos > lFirst) and (IndexOf(@aWords[lFirst], lPos - lFirst) < 0) then
      begin
      lWord := Copy(aWords, lFirst, lPos - lFirst);
      if not FCaseSensitive then
        lWord := LowerCase(lWord);
      if (FCount + 1) * 2 > FMask + 1 then
        Grow;
      Insert(lWord, aKind);
      Inc(FCount);
      end;
    end;
end;


function TSyntaxWordTable.Lookup(const aValue: string; aDefault: TSyntaxHighlightKind): TSyntaxHighlightKind;

var
  lIndex: SizeInt;
begin
  lIndex := IndexOf(PChar(aValue), Length(aValue));
  if lIndex < 0 then
    Result := aDefault
  else
    Result := FKinds[lIndex];
end;


function TSyntaxWordTable.Lookup(aValue: PChar; aLength: Integer; aDefault: TSyntaxHighlightKind): TSyntaxHighlightKind;

var
  lIndex: SizeInt;
begin
  lIndex := IndexOf(aValue, aLength);
  if lIndex < 0 then
    Result := aDefault
  else
    Result := FKinds[lIndex];
end;


function TSyntaxWordTable.Contains(const aValue: string): boolean;

begin
  Result := IndexOf(PChar(aValue), Length(aValue)) >= 0;
end;


{ TTableSyntaxHighlighter }

class function TTableSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := LanguageNames(Describe.Names);
end;


type
  TWordKindMethod = function(const aValue: string): TSyntaxHighlightKind of object;

constructor TTableSyntaxHighlighter.Create;

var
  lWordKind: TWordKindMethod;
  I: Integer;
  lChar: char;
begin
  inherited Create;
  FDescription := Describe;
  FWords := TSyntaxWordTable.Create(FDescription.CaseSensitive);
  FWords.AddWords(FDescription.Keywords, shKeyword);
  FWords.AddWords(FDescription.Builtins, shKey);
  // Compare the virtual WordKind of this instance with the base implementation
  lWordKind := @WordKind;
  FWordKindOverridden := TMethod(lWordKind).Code <> Pointer(@TTableSyntaxHighlighter.WordKind);
  case FDescription.Flavor of
    lfRust: FSpecialStart := ['r', 'b', 'c'];
    lfSwift: FSpecialStart := ['#'];
    lfCpp: FSpecialStart := ['R', 'u', 'U', 'L'];
    lfCSharp: FSpecialStart := ['"', '$'];
    lfLua: FSpecialStart := ['[', '-'];
    lfZig: FSpecialStart := ['\'];
    lfRuby: FSpecialStart := ['%', '<'];
    lfPerl: FSpecialStart := ['q', '<'];
    lfPowerShell: FSpecialStart := ['@'];
    lfPhp: FSpecialStart := ['<'];
  else
    FSpecialStart := [];
  end;
  FCommentStart := [];
  for I := 0 to High(FDescription.Comments) do
    if FDescription.Comments[I].OpenText <> '' then
      begin
      lChar := FDescription.Comments[I].OpenText[1];
      Include(FCommentStart, lChar);
      if not FDescription.CaseSensitive and (lChar in ['a'..'z']) then
        Include(FCommentStart, UpCase(lChar));
      end;
  FStringStart := [];
  for I := 0 to High(FDescription.Strings) do
    if FDescription.Strings[I].OpenText <> '' then
      Include(FStringStart, FDescription.Strings[I].OpenText[1]);
end;


destructor TTableSyntaxHighlighter.Destroy;

begin
  FreeAndNil(FWords);
  inherited Destroy;
end;


function TTableSyntaxHighlighter.WordKind(const aValue: string): TSyntaxHighlightKind;

begin
  Result := FWords.Lookup(aValue);
end;


function TTableSyntaxHighlighter.CommentToken: boolean;

var
  I: Integer;
  lFirst, lLen: SizeInt;
  lForm: ^TCommentForm;
  lHit: boolean;
begin
  Result := False;
  if At(FDescription.Attribute) then
    Exit;
  for I := 0 to High(FDescription.Comments) do
    begin
    lForm := @FDescription.Comments[I];
    lLen := Length(lForm^.OpenText);
    if FDescription.CaseSensitive then
      lHit := At(lForm^.OpenText)
    else
      lHit := AtFolded(lForm^.OpenText);
    if lHit and (not lForm^.LineStartOnly or FLineStart)
       and (not lForm^.WordBoundary or not (Ch(lLen) in FDescription.IdentifierRest)) then
      begin
      lFirst := FCursor;
      if lForm^.CloseText = '' then
        LineEnd
      else
        Block(lForm^.OpenText, lForm^.CloseText, lForm^.Nested);
      Emit(lFirst, shComment);
      Exit(True);
      end;
    end;
end;


function TTableSyntaxHighlighter.AtFolded(const aValue: string): boolean;

var
  I: Integer;
  lChar: char;
begin
  if (Length(aValue) = 0) or (Length(aValue) > FLimit - FCursor + 1) then
    Exit(False);
  for I := 0 to Length(aValue) - 1 do
    begin
    lChar := FText[FCursor + I];
    if lChar in ['A'..'Z'] then
      lChar := Chr(Ord(lChar) + 32);
    if lChar <> aValue[I + 1] then
      Exit(False);
    end;
  Result := True;
end;


function TTableSyntaxHighlighter.SpecialString: boolean;

begin
  Result := (Ch in FSpecialStart) and FlavorString;
end;


function TTableSyntaxHighlighter.FlavorString: boolean;

var
  lFirst, lPos, lHashes, lCount, lDepth, lTagStart, lLineStart: SizeInt;
  lOpening, lClosing, lTag: string;
  lQuote: char;
  lKind: TSyntaxHighlightKind;
  lSpaced, lFound: boolean;
begin
  Result := False;
  lFirst := FCursor;
  // Rust and Swift raw strings: a run of hashes in the delimiters
  if ((FDescription.Flavor = lfRust) and (At('r') or At('br') or At('cr'))) or ((FDescription.Flavor = lfSwift) and At('#')) then
    begin
    lPos := FCursor;
    if FDescription.Flavor = lfRust then
      begin
      if Ch in ['b', 'c'] then
        Inc(lPos);
      Inc(lPos);
      end;
    lHashes := 0;
    while (lPos <= FLimit) and (FText[lPos] = '#') do
      begin
      Inc(lPos);
      Inc(lHashes);
      end;
    if (lPos <= FLimit) and (FText[lPos] = '"') then
      begin
      lOpening := Copy(FText, FCursor, lPos - FCursor + 1);
      lClosing := '"' + StringOfChar('#', lHashes);
      Quoted(lOpening, lClosing, #0, True, False);
      Emit(lFirst, shRawString);
      Exit(True);
      end;
    if FDescription.Flavor = lfSwift then
      begin
      // A hash run without a quote is one symbol token
      FCursor := lPos;
      Emit(lFirst, shSymbol);
      Exit(True);
      end;
    end;
  if (FDescription.Flavor = lfCpp) and (At('R"') or At('u8R"') or At('uR"') or At('UR"') or At('LR"')) then
    begin
    lPos := FCursor;
    while (lPos <= FLimit) and (FText[lPos] <> '"') do
      Inc(lPos);
    lTagStart := lPos + 1;
    Inc(lPos);
    while (lPos <= FLimit) and (lPos - lTagStart <= 16) and not (FText[lPos] in ['(', ' ', #9, #10, #13, '\', ')']) do
      Inc(lPos);
    if (lPos <= FLimit) and (FText[lPos] = '(') then
      begin
      lOpening := Copy(FText, FCursor, lPos - FCursor + 1);
      lClosing := ')' + Copy(FText, lTagStart, lPos - lTagStart) + '"';
      Quoted(lOpening, lClosing, #0, True, False);
      Emit(lFirst, shRawString);
      Exit(True);
      end;
    end;
  if (FDescription.Flavor = lfCSharp) and (At('"""') or At('$"""')) then
    begin
    lPos := FCursor;
    while (lPos <= FLimit) and (FText[lPos] = '$') do
      Inc(lPos);
    lCount := lPos;
    while (lPos <= FLimit) and (FText[lPos] = '"') do
      Inc(lPos);
    lCount := lPos - lCount;
    FCursor := lPos;
    while FCursor <= FLimit do
      begin
      if Ch = '"' then
        begin
        lPos := FCursor;
        while (FCursor <= FLimit) and (Ch = '"') do
          Inc(FCursor);
        if FCursor - lPos >= lCount then
          begin
          FCursor := lPos + lCount;
          Break;
          end;
        end
      else
        Inc(FCursor);
      end;
    Emit(lFirst, shRawString);
    Exit(True);
    end;
  if (FDescription.Flavor = lfLua) and (At('[') or At('--[')) then
    begin
    lPos := FCursor;
    lKind := shRawString;
    if At('--') then
      begin
      Inc(lPos, 2);
      lKind := shComment;
      end;
    Inc(lPos);
    lCount := 0;
    while (lPos <= FLimit) and (FText[lPos] = '=') do
      begin
      Inc(lPos);
      Inc(lCount);
      end;
    if (lPos <= FLimit) and (FText[lPos] = '[') then
      begin
      lOpening := Copy(FText, FCursor, lPos - FCursor + 1);
      Quoted(lOpening, ']' + StringOfChar('=', lCount) + ']', #0, True, False);
      Emit(lFirst, lKind);
      Exit(True);
      end;
    end;
  if (FDescription.Flavor = lfZig) and At('\\') then
    begin
    LineEnd;
    Emit(lFirst, shRawString);
    Exit(True);
    end;
  // Ruby percent literals and Perl quote operators, with paired delimiters
  lPos := FCursor;
  if (FDescription.Flavor = lfRuby) and At('%') then
    begin
    Inc(lPos);
    if (lPos <= FLimit) and (FText[lPos] in ['q', 'Q', 'w', 'W', 'i', 'I', 'x', 'r', 's']) then
      Inc(lPos);
    end
  else if (FDescription.Flavor = lfPerl) and At('q') then
    begin
    Inc(lPos);
    if (lPos <= FLimit) and (FText[lPos] in ['q', 'w', 'x', 'r']) then
      Inc(lPos);
    end;
  if (lPos > FCursor) and (lPos <= FLimit) and (FText[lPos] in ['(', '[', '{', '<', '/', '!', '|']) then
    begin
    lQuote := FText[lPos];
    lClosing := lQuote;
    case lQuote of
      '(': lClosing := ')';
      '[': lClosing := ']';
      '{': lClosing := '}';
      '<': lClosing := '>';
    end;
    FCursor := lPos + 1;
    lDepth := 1;
    while FCursor <= FLimit do
      begin
      if Ch = '\' then
        begin
        Inc(FCursor);
        if FCursor <= FLimit then
          Inc(FCursor);
        end
      else if At(lClosing) then
        begin
        Inc(FCursor);
        Dec(lDepth);
        if lDepth = 0 then
          Break;
        end
      else
        begin
        if (lClosing <> lQuote) and (Ch = lQuote) then
          Inc(lDepth);
        Inc(FCursor);
        end;
      end;
    Emit(lFirst, shStrings);
    Exit(True);
    end;
  if (FDescription.Flavor = lfPowerShell) and (At('@"' + #10) or At('@"' + #13) or At('@''' + #10) or At('@''' + #13)) then
    begin
    lClosing := Ch(1) + '@';
    Inc(FCursor, 2);
    while FCursor <= FLimit do
      begin
      if (Ch(-1) in [#10, #13]) and At(lClosing) then
        begin
        Inc(FCursor, 2);
        Break;
        end;
      Inc(FCursor);
      end;
    Emit(lFirst, shStrings);
    Exit(True);
    end;
  if ((FDescription.Flavor = lfPhp) and At('<<<')) or ((FDescription.Flavor in [lfRuby, lfPerl]) and At('<<')) then
    begin
    lPos := FCursor + 2;
    if FDescription.Flavor = lfPhp then
      Inc(lPos);
    if (lPos <= FLimit) and (FText[lPos] in ['-', '~']) then
      Inc(lPos);
    lTagStart := lPos;
    while (lPos <= FLimit) and (FText[lPos] in [' ', #9]) do
      Inc(lPos);
    lSpaced := lPos > lTagStart;
    lQuote := #0;
    if (lPos <= FLimit) and (FText[lPos] in ['"', '''']) then
      begin
      lQuote := FText[lPos];
      Inc(lPos);
      end;
    lTagStart := lPos;
    if (lPos <= FLimit) and (FText[lPos] in ['A'..'Z', 'a'..'z', '_']) then
      while (lPos <= FLimit) and (FText[lPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        Inc(lPos);
    lTag := Copy(FText, lTagStart, lPos - lTagStart);
    if (lTag <> '') and ((lQuote = #0) or ((lPos <= FLimit) and (FText[lPos] = lQuote)))
       and not (lSpaced and (lQuote = #0) and (FDescription.Flavor <> lfPhp)) then
      begin
      FCursor := lPos;
      LineEnd;
      lFound := False;
      while not lFound and (FCursor <= FLimit) do
        begin
        if At(#13#10) then
          Inc(FCursor, 2)
        else
          Inc(FCursor);
        lLineStart := FCursor;
        while (FCursor <= FLimit) and (Ch in [' ', #9]) do
          Inc(FCursor);
        lFound := At(lTag) and not (Ch(Length(lTag)) in FDescription.IdentifierRest);
        if lFound then
          Inc(FCursor, Length(lTag))
        else
          begin
          FCursor := lLineStart;
          LineEnd;
          end;
        end;
      if not lFound then
        begin
        FCursor := lFirst;
        Exit(False);
        end;
      Emit(lFirst, shStrings);
      Exit(True);
      end;
    end;
end;


function TTableSyntaxHighlighter.StringToken: boolean;

var
  I: Integer;
  lFirst, lPos: SizeInt;
  lForm: ^TStringForm;
begin
  Result := False;
  for I := 0 to High(FDescription.Strings) do
    begin
    lForm := @FDescription.Strings[I];
    if At(lForm^.OpenText) then
      begin
      // A Rust lifetime or an Ada attribute is not an unfinished character literal
      if (lForm^.Kind = shCharacters) and (FDescription.Flavor in [lfRust, lfAda]) and (Ch = '''') then
        begin
        lPos := FCursor + 1;
        if (lPos <= FLimit) and (FText[lPos] = '\') then
          begin
          Inc(lPos);
          if (lPos + 1 <= FLimit) and (FText[lPos] = 'u') and (FText[lPos + 1] = '{') then
            begin
            Inc(lPos, 2);
            while (lPos <= FLimit) and (FText[lPos] in ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
              Inc(lPos);
            if (lPos <= FLimit) and (FText[lPos] = '}') then
              Inc(lPos);
            end
          else if (lPos <= FLimit) and (FText[lPos] = 'x') then
            Inc(lPos, 3)
          else
            Inc(lPos);
          end
        else
          begin
          if lPos <= FLimit then
            Inc(lPos);
          while (lPos <= FLimit) and (FText[lPos] in [#128..#191]) do
            Inc(lPos);
          end;
        if (lPos > FLimit) or (FText[lPos] <> '''') then
          Continue;
        lFirst := FCursor;
        FCursor := lPos + 1;
        Emit(lFirst, shCharacters);
        Exit(True);
        end;
      lFirst := FCursor;
      Quoted(lForm^.OpenText, lForm^.CloseText, lForm^.Escape, lForm^.Multiline, lForm^.Doubled);
      Emit(lFirst, lForm^.Kind);
      Exit(True);
      end;
    end;
end;


procedure TTableSyntaxHighlighter.NumberToken;

var
  lFirst, lPos: SizeInt;
  lBase: Byte;
  lAllowed: TSysCharSet;
  lForm: ^TNumberForms;
  lIsHex: boolean;
begin
  lFirst := FCursor;
  lForm := @FDescription.Numbers;
  lBase := 10;
  lIsHex := False;
  lAllowed := ['0'..'9'];
  if lForm^.Hex and (At('0x') or At('0X') or At('&h') or At('&H')) then
    begin
    Inc(FCursor, 2);
    lBase := 16;
    lIsHex := True;
    lAllowed := lAllowed + ['a'..'f', 'A'..'F'];
    end
  else if lForm^.Binary and (At('0b') or At('0B') or At('&b') or At('&B')) then
    begin
    Inc(FCursor, 2);
    lBase := 2;
    lAllowed := ['0', '1'];
    end
  else if lForm^.Octal and (At('0o') or At('0O') or At('&o') or At('&O')) then
    begin
    Inc(FCursor, 2);
    lBase := 8;
    lAllowed := ['0'..'7'];
    end
  else if lForm^.LeadingOctal and (Ch = '0') and (Ch(1) in ['0'..'7']) then
    begin
    lBase := 8;
    lAllowed := ['0'..'7'];
    end;
  if lForm^.Separators then
    Include(lAllowed, lForm^.Separator);
  while (FCursor <= FLimit) and (Ch in lAllowed) do
    Inc(FCursor);
  if lForm^.Based and (Ch = '#') then
    begin
    Inc(FCursor);
    while (FCursor <= FLimit) and (Ch in ['0'..'9', 'a'..'f', 'A'..'F', '_', '.']) do
      Inc(FCursor);
    if Ch = '#' then
      Inc(FCursor);
    end
  else if lForm^.Fraction and (lBase in [10, 16]) and (Ch = '.') and (Ch(1) <> '.')
          and ((Ch(1) in lAllowed) or (FCursor = lFirst)) then
    begin
    Inc(FCursor);
    while (FCursor <= FLimit) and (Ch in lAllowed) do
      Inc(FCursor);
    end;
  if lForm^.Exponent and (FCursor <= FLimit)
     and (((not lIsHex) and (Pos(Ch, lForm^.ExponentLetters) > 0)) or (lIsHex and lForm^.HexExponent and (Ch in ['p', 'P']))) then
    begin
    lPos := FCursor + 1;
    if (lPos <= FLimit) and (FText[lPos] in ['+', '-']) then
      Inc(lPos);
    if (lPos <= FLimit) and (FText[lPos] in ['0'..'9']) then
      begin
      FCursor := lPos + 1;
      while (FCursor <= FLimit) and (Ch in ['0'..'9', '_']) do
        Inc(FCursor);
      end;
    end;
  if lForm^.TrailingBase then
    while (FCursor <= FLimit) and (Ch in ['0'..'9', 'A'..'F', 'a'..'f', 'H', 'h', 'X', 'x']) do
      Inc(FCursor);
  while (FCursor <= FLimit) and (Pos(Ch, lForm^.Suffixes) > 0) do
    Inc(FCursor);
  if FCursor = lFirst then
    Inc(FCursor);
  Emit(lFirst, shNumbers);
end;


procedure TTableSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lPos: SizeInt;
  lValue: string;
  lKind: TSyntaxHighlightKind;
begin
  Start(aSource);
  FLineStart := True;
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    if Ch in [#9, ' '] then
      begin
      while (FCursor <= FLimit) and (Ch in [#9, ' ']) do
        Inc(FCursor);
      Emit(lFirst, shDefault);
      Continue;
      end;
    if Ch in [#10, #13] then
      begin
      if At(#13#10) then
        Inc(FCursor, 2)
      else
        Inc(FCursor);
      Emit(lFirst, shDefault);
      FLineStart := True;
      Continue;
      end;
    if (FDescription.Flavor = lfAssembly) and (Ch = '#') and not (Ch(1) in ['0'..'9', '+', '-']) then
      begin
      LineEnd;
      Emit(lFirst, shComment);
      Continue;
      end;
    if SpecialString or ((Ch in FCommentStart) and CommentToken) or ((Ch in FStringStart) and StringToken) then
      begin
      FLineStart := False;
      Continue;
      end;
    if FLineStart and At(FDescription.Directive) then
      begin
      LineEnd;
      Emit(lFirst, shDirective);
      end
    else if At(FDescription.Attribute) then
      begin
      Inc(FCursor, Length(FDescription.Attribute));
      while (FCursor <= FLimit) and (Ch in FDescription.IdentifierRest + ['.']) do
        Inc(FCursor);
      Emit(lFirst, shAttribute);
      end
    else if (Ch in ['0'..'9']) or (FDescription.Numbers.LeadingDot and (Ch = '.') and (Ch(1) in ['0'..'9']))
            or ((FDescription.Flavor = lfBasic) and (Ch = '&') and (Ch(1) in ['h', 'H', 'o', 'O', 'b', 'B'])) then
      NumberToken
    else if Ch in FDescription.IdentifierStart then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in FDescription.IdentifierRest) do
        Inc(FCursor);
      if FWordKindOverridden then
        begin
        lValue := SourceText(FText, lFirst, FCursor - lFirst);
        lKind := WordKind(lValue);
        end
      else
        lKind := FWords.Lookup(PChar(Pointer(FText)) + lFirst - 1, FCursor - lFirst);
      if (FDescription.Flavor = lfDocker) and not FLineStart then
        lKind := shDefault;
      if FDescription.Flavor = lfToml then
        begin
        lPos := FCursor;
        while (lPos <= FLimit) and (FText[lPos] in [' ', #9]) do
          Inc(lPos);
        if (lPos <= FLimit) and (FText[lPos] = '=') then
          lKind := shKey;
        end;
      if FWordKindOverridden then
        EmitText(lValue, lKind)
      else
        Emit(lFirst, lKind);
      end
    else
      begin
      lKind := shDefault;
      if Ch in FDescription.Operators then
        lKind := shOperator
      else if Ch in FDescription.Symbols then
        lKind := shSymbol;
      Unknown;
      Emit(lFirst, lKind);
      end;
    FLineStart := False;
    end;
  Finish;
end;

end.

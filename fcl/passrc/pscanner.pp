{
    $Id$
    This file is part of the Free Component Library

    Pascal source lexical scanner
    Copyright (c) 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit PScanner;

interface

uses SysUtils, Classes;

resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrOpenString = 'String exceeds end of line';
  SErrIncludeFileNotFound = 'Could not find include file ''%s''';

type

  TToken = (
    tkEOF,
    tkWhitespace,
    tkComment,
    tkIdentifier,
    tkString,
    tkNumber,
    tkChar,
    // Simple (one-character) tokens
    tkBraceOpen,	// '('
    tkBraceClose,	// ')'
    tkMul,		// '*'
    tkPlus,		// '+'
    tkComma,		// ','
    tkMinus,		// '-'
    tkDot,		// '.'
    tkDivision,		// '/'
    tkColon,		// ':'
    tkSemicolon,	// ';'
    tkEqual,		// '='
    tkSquaredBraceOpen,	// '['
    tkSquaredBraceClose,// ']'
    tkCaret,		// '^'
    // Two-character tokens
    tkDotDot,		// '..'
    tkAssign,		// ':='
    // Reserved words
    tkabsolute,
    tkand,
    tkarray,
    tkas,
    tkasm,
    tkbegin,
    tkbreak,
    tkcase,
    tkclass,
    tkconst,
    tkconstructor,
    tkcontinue,
    tkdestructor,
    tkdispose,
    tkdiv,
    tkdo,
    tkdownto,
    tkelse,
    tkend,
    tkexcept,
    tkexit,
    tkexports,
    tkfalse,
    tkfinalization,
    tkfinally,
    tkfor,
    tkfunction,
    tkgoto,
    tkif,
    tkimplementation,
    tkin,
    tkinherited,
    tkinitialization,
    tkinline,
    tkinterface,
    tkis,
    tklabel,
    tklibrary,
    tkmod,
    tknew,
    tknil,
    tknot,
    tkobject,
    tkof,
    tkon,
    tkoperator,
    tkor,
    tkpacked,
    tkprocedure,
    tkprogram,
    tkproperty,
    tkraise,
    tkrecord,
    tkrepeat,
    tkResourceString,
    tkself,
    tkset,
    tkshl,
    tkshr,
//    tkstring,
    tkthen,
    tkto,
    tktrue,
    tktry,
    tktype,
    tkunit,
    tkuntil,
    tkuses,
    tkvar,
    tkwhile,
    tkwith,
    tkxor);

  TLineReader = class
  public
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: String; virtual; abstract;
  end;

  TFileLineReader = class(TLineReader)
  private
    FTextFile: Text;
    FileOpened: Boolean;
  public
    constructor Create(const AFilename: String);
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: String; override;
  end;

  TFileResolver = class
  private
    FIncludePaths: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddIncludePath(const APath: String);
    function FindSourceFile(const AName: String): TLineReader;
    function FindIncludeFile(const AName: String): TLineReader;
  end;

  EScannerError = class(Exception);

  TPascalScanner = class
  private
    FFileResolver: TFileResolver;
    FCurSourceFile: TLineReader;
    FCurFilename: String;
    FCurRow: Integer;
    FCurToken: TToken;
    FCurTokenString: String;
    FCurLine: String;
    TokenStr: PChar;
    FIncludeStack: TList;
    function GetCurColumn: Integer;
  protected
    procedure Error(const Msg: String);
    procedure Error(const Msg: String; Args: array of Const);
    function DoFetchToken: TToken;
  public
    constructor Create(AFileResolver: TFileResolver; const AFilename: String);
    destructor Destroy; override;
    function FetchToken: TToken;

    property FileResolver: TFileResolver read FFileResolver;
    property CurSourceFile: TLineReader read FCurSourceFile;
    property CurFilename: String read FCurFilename;

    property CurLine: String read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TToken read FCurToken;
    property CurTokenString: String read FCurTokenString;
  end;

const
  TokenInfos: array[TToken] of String = (
    'EOF',
    'Whitespace',
    'Comment',
    'Identifier',
    'String',
    'Number',
    'Character',
    '(',
    ')',
    '*',
    '+',
    ',',
    '-',
    '.',
    '/',
    ':',
    ';',
    '=',
    '[',
    ']',
    '^',
    '..',
    ':=',
    // Reserved words
    'absolute',
    'and',
    'array',
    'as',
    'asm',
    'begin',
    'break',
    'case',
    'class',
    'const',
    'constructor',
    'continue',
    'destructor',
    'dispose',
    'div',
    'do',
    'downto',
    'else',
    'end',
    'except',
    'exit',
    'exports',
    'false',
    'finalization',
    'finally',
    'for',
    'function',
    'goto',
    'if',
    'implementation',
    'in',
    'inherited',
    'initialization',
    'inline',
    'interface',
    'is',
    'label',
    'library',
    'mod',
    'new',
    'nil',
    'not',
    'object',
    'of',
    'on',
    'operator',
    'or',
    'packed',
    'procedure',
    'program',
    'property',
    'raise',
    'record',
    'repeat',
    'resourcestring',
    'self',
    'set',
    'shl',
    'shr',
//    'string',
    'then',
    'to',
    'true',
    'try',
    'type',
    'unit',
    'until',
    'uses',
    'var',
    'while',
    'with',
    'xor'
  );


implementation

type
  TIncludeStackItem = class
    SourceFile: TLineReader;
    Filename: String;
    Token: TToken;
    TokenString: String;
    Line: String;
    Row: Integer;
    TokenStr: PChar;
  end;


constructor TFileLineReader.Create(const AFilename: String);
begin
  inherited Create;
  Assign(FTextFile, AFilename);
  Reset(FTextFile);
  FileOpened := True;
end;

destructor TFileLineReader.Destroy;
begin
  if FileOpened then
    Close(FTextFile);
  inherited Destroy;
end;

function TFileLineReader.IsEOF: Boolean;
begin
  Result := EOF(FTextFile);
end;

function TFileLineReader.ReadLine: String;
begin
  ReadLn(FTextFile, Result);
end;


constructor TFileResolver.Create;
begin
  inherited Create;
  FIncludePaths := TStringList.Create;
end;

destructor TFileResolver.Destroy;
begin
  FIncludePaths.Free;
  inherited Destroy;
end;

procedure TFileResolver.AddIncludePath(const APath: String);
begin
  FIncludePaths.Add(IncludeTrailingPathDelimiter(APath));
end;

function TFileResolver.FindSourceFile(const AName: String): TLineReader;
begin
  try
    Result := TFileLineReader.Create(AName);
  except
    Result := nil;
  end;
end;

function TFileResolver.FindIncludeFile(const AName: String): TLineReader;
var
  i: Integer;
begin
  Result := nil;
  try
    Result := TFileLineReader.Create(AName);
  except
    for i := 0 to FIncludePaths.Count - 1 do
      try
        Result := TFileLineReader.Create(FIncludePaths[i] + AName);
	break;
      except
      end;
  end;
end;


constructor TPascalScanner.Create(AFileResolver: TFileResolver;
  const AFilename: String);
begin
  inherited Create;
  FFileResolver := AFileResolver;
  FCurSourceFile := FileResolver.FindSourceFile(AFilename);
  FCurFilename := AFilename;
  FIncludeStack := TList.Create;
end;

destructor TPascalScanner.Destroy;
begin
  // Dont' free the first element, because it is CurSourceFile
  while FIncludeStack.Count > 1 do
    TFileResolver(FIncludeStack[1]).Free;
  FIncludeStack.Free;

  CurSourceFile.Free;
  inherited Destroy;
end;

function TPascalScanner.FetchToken: TToken;
var
  IncludeStackItem: TIncludeStackItem;
begin
  while True do
  begin
    Result := DoFetchToken;
    if FCurToken = tkEOF then
      if FIncludeStack.Count > 0 then
      begin
        CurSourceFile.Free;
	IncludeStackItem :=
	  TIncludeStackItem(FIncludeStack[FIncludeStack.Count - 1]);
	FIncludeStack.Delete(FIncludeStack.Count - 1);
	FCurSourceFile := IncludeStackItem.SourceFile;
	FCurFilename := IncludeStackItem.Filename;
	FCurToken := IncludeStackItem.Token;
	FCurTokenString := IncludeStackItem.TokenString;
	FCurLine := IncludeStackItem.Line;
	FCurRow := IncludeStackItem.Row;
	TokenStr := IncludeStackItem.TokenStr;
	IncludeStackItem.Free;
	Result := FCurToken;
      end else
        break
    else
      break;
  end;
end;

procedure TPascalScanner.Error(const Msg: String);
begin
  raise EScannerError.Create(Msg);
end;

procedure TPascalScanner.Error(const Msg: String; Args: array of Const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TPascalScanner.DoFetchToken: TToken;

  function FetchLine: Boolean;
  begin
    if CurSourceFile.IsEOF then
    begin
      FCurLine := '';
      TokenStr := nil;
      Result := False;
    end else
    begin
      FCurLine := CurSourceFile.ReadLine;
      TokenStr := PChar(CurLine);
      Result := True;
      Inc(FCurRow);
    end;
  end;

var
  TokenStart, CurPos: PChar;
  i: TToken;
  OldLength, SectionLength, NestingLevel: Integer;
  Directive, Param: String;
  IncludeStackItem: TIncludeStackItem;
begin
  if TokenStr = nil then
    if not FetchLine then
    begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
    end;

  FCurTokenString := '';

  case TokenStr[0] of
    #0:		// Empty line
      begin
        FetchLine;
        Result := tkWhitespace;
      end;
    #9, ' ':
      begin
	Result := tkWhitespace;
        repeat
	  Inc(TokenStr);
	  if TokenStr[0] = #0 then
	    if not FetchLine then
	    begin
	      FCurToken := Result;
	      exit;
	    end;
	until not (TokenStr[0] in [#9, ' ']);
      end;
    '#':
      begin
        TokenStart := TokenStr;
	Inc(TokenStr);
	if TokenStr[0] = '$' then
	begin
	  Inc(TokenStr);
	  repeat
	    Inc(TokenStr);
	  until not (TokenStr[0] in ['0'..'9', 'A'..'F', 'a'..'F']);
	end else
	  repeat
	    Inc(TokenStr);
	  until not (TokenStr[0] in ['0'..'9']);

	SectionLength := TokenStr - TokenStart;
	SetLength(FCurTokenString, SectionLength);
	if SectionLength > 0 then
	  Move(TokenStart^, FCurTokenString[1], SectionLength);
	Result := tkChar;
      end;
    '$':
      begin
        TokenStart := TokenStr;
	repeat
	  Inc(TokenStr);
	until not (TokenStr[0] in ['0'..'9', 'A'..'F', 'a'..'F']);
	SectionLength := TokenStr - TokenStart;
	SetLength(FCurTokenString, SectionLength);
	if SectionLength > 0 then
	  Move(TokenStart^, FCurTokenString[1], SectionLength);
	Result := tkNumber;
      end;
    '''':
      begin
        Inc(TokenStr);
        TokenStart := TokenStr;
	OldLength := 0;
	FCurTokenString := '';

	while True do
	begin
	  if TokenStr[0] = '''' then
	    if TokenStr[1] = '''' then
	    begin
	      SectionLength := TokenStr - TokenStart + 1;
	      SetLength(FCurTokenString, OldLength + SectionLength);
	      if SectionLength > 1 then
	        Move(TokenStart^, FCurTokenString[OldLength + 1],
		  SectionLength);
	      Inc(OldLength, SectionLength);
	      Inc(TokenStr);
	      TokenStart := TokenStr;
	    end else
	      break;

	  if TokenStr[0] = #0 then
	    Error(SErrOpenString);

	  Inc(TokenStr);
	end;

	SectionLength := TokenStr - TokenStart;
	SetLength(FCurTokenString, OldLength + SectionLength);
	if SectionLength > 0 then
	  Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);

	Inc(TokenStr);
	Result := tkString;
      end;
    '(':
      begin
        Inc(TokenStr);
	if TokenStr[0] = '*' then
	begin
	  // Old-style multi-line comment
	  Inc(TokenStr);
          while (TokenStr[0] <> '*') or (TokenStr[1] <> ')') do
	  begin
	    if TokenStr[0] = #0 then
	    begin
	      if not FetchLine then
	      begin
	        Result := tkEOF;
	        FCurToken := Result;
	        exit;
	      end;
	    end else
	      Inc(TokenStr);
	  end;
	  Inc(TokenStr, 2);
	  Result := tkComment;
        end else
	  Result := tkBraceOpen;
      end;
    ')':
      begin
        Inc(TokenStr);
	Result := tkBraceClose;
      end;
    '*':
      begin
        Inc(TokenStr);
	Result := tkMul;
      end;
    '+':
      begin
        Inc(TokenStr);
	Result := tkPlus;
      end;
    ',':
      begin
        Inc(TokenStr);
	Result := tkComma;
      end;
    '-':
      begin
        Inc(TokenStr);
	Result := tkMinus;
      end;
    '.':
      begin
        Inc(TokenStr);
	if TokenStr[0] = '.' then
	begin
	  Inc(TokenStr);
	  Result := tkDotDot;
	end else
	  Result := tkDot;
      end;
    '/':
      begin
        Inc(TokenStr);
	if TokenStr[0] = '/' then	// Single-line comment
	begin
	  Inc(TokenStr);
	  TokenStart := TokenStr;
	  FCurTokenString := '';
          while TokenStr[0] <> #0 do
	    Inc(TokenStr);
	  SectionLength := TokenStr - TokenStart;
	  SetLength(FCurTokenString, SectionLength);
	  if SectionLength > 0 then
	    Move(TokenStart^, FCurTokenString[1], SectionLength);
	  Result := tkComment;
	  //WriteLn('Einzeiliger Kommentar: "', CurTokenString, '"');
	end else
	  Result := tkDivision;
      end;
    '0'..'9':
      begin
        TokenStart := TokenStr;
	repeat
	  Inc(TokenStr);
	until not (TokenStr[0] in ['0'..'9', '.', 'e', 'E']);
	SectionLength := TokenStr - TokenStart;
	SetLength(FCurTokenString, SectionLength);
	if SectionLength > 0 then
	  Move(TokenStart^, FCurTokenString[1], SectionLength);
	Result := tkNumber;
      end;
    ':':
      begin
        Inc(TokenStr);
	if TokenStr[0] = '=' then
	begin
	  Inc(TokenStr);
	  Result := tkAssign;
	end else
          Result := tkColon;
      end;
    ';':
      begin
        Inc(TokenStr);
        Result := tkSemicolon;
      end;
    '=':
      begin
        Inc(TokenStr);
        Result := tkEqual;
      end;
    '[':
      begin
        Inc(TokenStr);
	Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(TokenStr);
	Result := tkSquaredBraceClose;
      end;
    '^':
      begin
        Inc(TokenStr);
	Result := tkCaret;
      end;
    '{':	// Multi-line comment
      begin
        Inc(TokenStr);
	TokenStart := TokenStr;
	FCurTokenString := '';
	OldLength := 0;
	NestingLevel := 0;
        while (TokenStr[0] <> '}') or (NestingLevel > 0) do
	begin
	  if TokenStr[0] = #0 then
	  begin
	    SectionLength := TokenStr - TokenStart + 1;
	    SetLength(FCurTokenString, OldLength + SectionLength);
	    if SectionLength > 1 then
	      Move(TokenStart^, FCurTokenString[OldLength + 1],
	        SectionLength - 1);
	    Inc(OldLength, SectionLength);
	    FCurTokenString[OldLength] := #10;
	    if not FetchLine then
	    begin
	      Result := tkEOF;
	      FCurToken := Result;
	      exit;
	    end;
	    TokenStart := TokenStr;
	  end else
	  begin
	    if TokenStr[0] = '{' then
	      Inc(NestingLevel)
	    else if TokenStr[0] = '}' then
	      Dec(NestingLevel);
	    Inc(TokenStr);
	  end;
	end;
	SectionLength := TokenStr - TokenStart;
	SetLength(FCurTokenString, OldLength + SectionLength);
	if SectionLength > 0 then
	  Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
	Inc(TokenStr);
	Result := tkComment;
	//WriteLn('Kommentar: "', CurTokenString, '"');
	if (Length(CurTokenString) > 0) and (CurTokenString[1] = '$') then
	begin
	  TokenStart := @CurTokenString[2];
	  CurPos := TokenStart;
	  while (CurPos[0] <> ' ') and (CurPos[0] <> #0) do
	    Inc(CurPos);
	  SectionLength := CurPos - TokenStart;
	  SetLength(Directive, SectionLength);
	  if SectionLength > 0 then
	  begin
	    Move(TokenStart^, Directive[1], SectionLength);
	    Directive := UpperCase(Directive);
	    if CurPos[0] <> #0 then
	    begin
	      TokenStart := CurPos + 1;
	      CurPos := TokenStart;
	      while CurPos[0] <> #0 do
	        Inc(CurPos);
	      SectionLength := CurPos - TokenStart;
	      SetLength(Param, SectionLength);
	      if SectionLength > 0 then
	        Move(TokenStart^, Param[1], SectionLength);
	    end else
	      Param := '';
  	    // WriteLn('Direktive: "', Directive, '", Param: "', Param, '"');
	    if (Directive = 'I') or (Directive = 'INCLUDE') then
	    begin
	      IncludeStackItem := TIncludeStackItem.Create;
	      IncludeStackItem.SourceFile := CurSourceFile;
	      IncludeStackItem.Filename := CurFilename;
	      IncludeStackItem.Token := CurToken;
	      IncludeStackItem.TokenString := CurTokenString;
	      IncludeStackItem.Line := CurLine;
	      IncludeStackItem.Row := CurRow;
	      IncludeStackItem.TokenStr := TokenStr;
	      FIncludeStack.Add(IncludeStackItem);
	      FCurSourceFile := FileResolver.FindIncludeFile(Param);
	      if not Assigned(CurSourceFile) then
	        Error(SErrIncludeFileNotFound, [Param]);
	      FCurFilename := Param;
	      FCurRow := 0;
	    end;
	  end else
	    Directive := '';
	end;
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
        TokenStart := TokenStr;
	repeat
	  Inc(TokenStr);
	until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
	SectionLength := TokenStr - TokenStart;
	SetLength(FCurTokenString, SectionLength);
	if SectionLength > 0 then
	  Move(TokenStart^, FCurTokenString[1], SectionLength);

	// Check if this is a keyword or identifier
	// !!!: Optimize this!
	for i := tkAbsolute to tkXOR do
	  if CompareText(CurTokenString, TokenInfos[i]) = 0 then
	  begin
	    Result := i;
	    FCurToken := Result;
	    exit;
	  end;

	Result := tkIdentifier;
      end;
  else
    Error(SErrInvalidCharacter, [TokenStr[0]]);
  end;

  FCurToken := Result;
end;

function TPascalScanner.GetCurColumn: Integer;
begin
  Result := TokenStr - PChar(CurLine);
end;

end.


{
  $Log$
  Revision 1.1  2003-03-13 21:47:42  sg
  * First version as part of FCL

}

{
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

{$mode objfpc}
{$h+}

unit PScanner;

interface

uses SysUtils, Classes;

// message numbers
const
  nErrInvalidCharacter = 1001;
  nErrOpenString = 1002;
  nErrIncludeFileNotFound = 1003;
  nErrIfXXXNestingLimitReached = 1004;
  nErrInvalidPPElse = 1005;
  nErrInvalidPPEndif = 1006;
  nLogOpeningFile = 1007;
  nLogLineNumber = 1008;
  nLogIFDefAccepted = 1009;
  nLogIFDefRejected = 1010;
  nLogIFNDefAccepted = 1011;
  nLogIFNDefRejected = 1012;
  nLogIFOPTIgnored = 1013;
  nLogIFIgnored = 1014;

// resourcestring patterns of messages
resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrOpenString = 'string exceeds end of line';
  SErrIncludeFileNotFound = 'Could not find include file ''%s''';
  SErrIfXXXNestingLimitReached = 'Nesting of $IFxxx too deep';
  SErrInvalidPPElse = '$ELSE without matching $IFxxx';
  SErrInvalidPPEndif = '$ENDIF without matching $IFxxx';
  SLogOpeningFile = 'Opening source file "%s".';
  SLogLineNumber = 'Reading line %d.';
  SLogIFDefAccepted = 'IFDEF %s found, accepting.';
  SLogIFDefRejected = 'IFDEF %s found, rejecting.';
  SLogIFNDefAccepted = 'IFNDEF %s found, accepting.';
  SLogIFNDefRejected = 'IFNDEF %s found, rejecting.';
  SLogIFOPTIgnored = 'IFOPT %s found, ignoring (rejected).';
  SLogIFIgnored = 'IF %s found, ignoring (rejected).';

type
  TMessageType = (
    mtFatal,
    mtError,
    mtWarning,
    mtNote,
    mtHint,
    mtInfo,
    mtDebug
    );
  TMessageTypes = set of TMessageType;

  TMessageArgs = array of string;

  TToken = (
    tkEOF,
    tkWhitespace,
    tkComment,
    tkIdentifier,
    tkString,
    tkNumber,
    tkChar,
    // Simple (one-character) tokens
    tkBraceOpen,             // '('
    tkBraceClose,            // ')'
    tkMul,                   // '*'
    tkPlus,                  // '+'
    tkComma,                 // ','
    tkMinus,                 // '-'
    tkDot,                   // '.'
    tkDivision,              // '/'
    tkColon,                 // ':'
    tkSemicolon,             // ';'
    tkLessThan,              // '<'
    tkEqual,                 // '='
    tkGreaterThan,           // '>'
    tkAt,                    // '@'
    tkSquaredBraceOpen,      // '['
    tkSquaredBraceClose,     // ']'
    tkCaret,                 // '^'
    tkBackslash,             // '\'
    // Two-character tokens
    tkDotDot,                // '..'
    tkAssign,                // ':='
    tkNotEqual,              // '<>'
    tkLessEqualThan,         // '<='
    tkGreaterEqualThan,      // '>='
    tkPower,                 // '**'
    tkSymmetricalDifference, // '><'
    tkAssignPlus,            // +=
    tkAssignMinus,           // -=
    tkAssignMul,             // *=
    tkAssignDivision,        // /=
    // Reserved words
    tkabsolute,
    tkand,
    tkarray,
    tkas,
    tkasm,
    tkbegin,
    tkbitpacked,
    tkcase,
    tkclass,
    tkconst,
    tkconstref,
    tkconstructor,
    tkdestructor,
    tkdispinterface,
    tkdiv,
    tkdo,
    tkdownto,
    tkelse,
    tkend,
    tkexcept,
    tkexports,
    tkfalse,
    tkfile,
    tkfinalization,
    tkfinally,
    tkfor,
    tkfunction,
    tkgeneric,
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
    tkspecialize,
//    tkstring,
    tkthen,
    tkthreadvar,
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
    tkxor,
    tkLineEnding,
    tkTab
    );
  TTokens = set of TToken;

  { TMacroDef }

  TMacroDef = Class(TObject)
  Private
    FName: String;
    FValue: String;
  Public
    Constructor Create(Const AName,AValue : String);
    Property Name  : String Read FName;
    Property Value : String Read FValue Write FValue;
  end;

  { TLineReader }

  TLineReader = class
  Private
    FFilename: string;
  public
    constructor Create(const AFilename: string); virtual;
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: string; virtual; abstract;
    property Filename: string read FFilename;
  end;

  { TFileLineReader }

  TFileLineReader = class(TLineReader)
  private
    FTextFile: Text;
    FileOpened: Boolean;
    FBuffer : Array[0..4096-1] of byte;
  public
    constructor Create(const AFilename: string); override;
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: string; override;
  end;

  { TStreamLineReader }

  TStreamLineReader = class(TLineReader)
  private
    FContent: AnsiString;
    FPos : Integer;
  public
    Procedure InitFromStream(AStream : TStream);
    function IsEOF: Boolean; override;
    function ReadLine: string; override;
  end;

  { TFileStreamLineReader }

  TFileStreamLineReader = class(TStreamLineReader)
  Public
    constructor Create(const AFilename: string); override;
  end;

  { TStringStreamLineReader }

  TStringStreamLineReader = class(TStreamLineReader)
  Public
    constructor Create( const AFilename: string; Const ASource: String); reintroduce;
  end;

  { TMacroReader }

  TMacroReader = Class(TStringStreamLineReader)
  private
    FCurCol: Integer;
    FCurRow: Integer;
  Public
    Property CurCol : Integer Read FCurCol Write FCurCol;
    Property CurRow : Integer Read FCurRow Write FCurRow;
  end;

  { TBaseFileResolver }

  TBaseFileResolver = class
  private
    FBaseDirectory: string;
    FIncludePaths: TStringList;
    FStrictFileCase : Boolean;
  Protected
    procedure SetBaseDirectory(AValue: string); virtual;
    procedure SetStrictFileCase(AValue: Boolean); virtual;
    Function FindIncludeFileName(const AName: string): String;
    Property IncludePaths: TStringList Read FIncludePaths;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddIncludePath(const APath: string); virtual;
    function FindSourceFile(const AName: string): TLineReader; virtual; abstract;
    function FindIncludeFile(const AName: string): TLineReader; virtual; abstract;
    Property StrictFileCase : Boolean Read FStrictFileCase Write SetStrictFileCase;
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
  end;

  { TFileResolver }

  TFileResolver = class(TBaseFileResolver)
  private
    FUseStreams: Boolean;
  Protected
    Function CreateFileReader(Const AFileName : String) : TLineReader; virtual;
  Public
    function FindSourceFile(const AName: string): TLineReader; override;
    function FindIncludeFile(const AName: string): TLineReader; override;
    Property UseStreams : Boolean Read FUseStreams Write FUseStreams;
  end;

  { TStreamResolver }

  TStreamResolver = class(TBaseFileResolver)
  Private
    FOwnsStreams: Boolean;
    FStreams : TStringList;
    function FindStream(const AName: string; ScanIncludes: Boolean): TStream;
    function FindStreamReader(const AName: string; ScanIncludes: Boolean): TLineReader;
    procedure SetOwnsStreams(AValue: Boolean);
  Public
    constructor Create; override;
    destructor Destroy; override;
    Procedure Clear;
    Procedure AddStream(Const AName : String; AStream : TStream);
    function FindSourceFile(const AName: string): TLineReader; override;
    function FindIncludeFile(const AName: string): TLineReader; override;
    Property OwnsStreams : Boolean Read FOwnsStreams write SetOwnsStreams;
    Property Streams: TStringList read FStreams;
  end;

  EScannerError       = class(Exception);
  EFileNotFoundError  = class(Exception);

  TPascalScannerPPSkipMode = (ppSkipNone, ppSkipIfBranch, ppSkipElseBranch, ppSkipAll);

  TPOption = (
    po_delphi, // Delphi mode: forbid nested comments
    po_cassignments,  // allow C-operators += -= *= /=
    po_resolvestandardtypes, // search for 'longint', 'string', etc., do not use dummies, TPasResolver sets this to use its declarations
    po_asmwhole,  // store whole text between asm..end in TPasImplAsmStatement.Tokens
    po_nooverloadedprocs,  // do not create TPasOverloadedProc for procs with same name
    po_keepclassforward,   // disabled: delete class fowards when there is a class declaration
    po_arrayrangeexpr    // enable: create TPasArrayType.IndexRange, disable: create TPasArrayType.Ranges
    );
  TPOptions = set of TPOption;

type
  TPasSourcePos = Record
    FileName: String;
    Row, Column: Cardinal;
  end;

type
  { TPascalScanner }

  TPScannerLogHandler = Procedure (Sender : TObject; Const Msg : String) of object;
  TPScannerLogEvent = (sleFile,sleLineNumber,sleConditionals);
  TPScannerLogEvents = Set of TPScannerLogEvent;

  TPascalScanner = class
  private
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FFileResolver: TBaseFileResolver;
    FCurSourceFile: TLineReader;
    FCurFilename: string;
    FCurRow: Integer;
    FCurToken: TToken;
    FCurTokenString: string;
    FCurLine: string;
    FMacros,
    FDefines: TStrings;
    FOptions: TPOptions;
    FLogEvents: TPScannerLogEvents;
    FOnLog: TPScannerLogHandler;
    FSkipComments: Boolean;
    FSkipWhiteSpace: Boolean;
    TokenStr: PChar;
    FIncludeStack: TFPList;

    // Preprocessor $IFxxx skipping data
    PPSkipMode: TPascalScannerPPSkipMode;
    PPIsSkipping: Boolean;
    PPSkipStackIndex: Integer;
    PPSkipModeStack: array[0..255] of TPascalScannerPPSkipMode;
    PPIsSkippingStack: array[0..255] of Boolean;

    function GetCurColumn: Integer;
    procedure SetOptions(AValue: TPOptions);
  protected
    function FetchLine: boolean;
    procedure SetCurMsg(MsgType: TMessageType; MsgNumber: integer; Const Fmt : String; Args : Array of const);
    Procedure DoLog(MsgType: TMessageType; MsgNumber: integer; Const Msg : String; SkipSourceInfo : Boolean = False);overload;
    Procedure DoLog(MsgType: TMessageType; MsgNumber: integer; Const Fmt : String; Args : Array of const;SkipSourceInfo : Boolean = False);overload;
    procedure Error(MsgNumber: integer; const Msg: string);overload;
    procedure Error(MsgNumber: integer; const Fmt: string; Args: array of Const);overload;
    procedure PushSkipMode;
    function HandleDirective(const ADirectiveText: String): TToken; virtual;
    procedure HandleIFDEF(const AParam: String);
    procedure HandleIFNDEF(const AParam: String);
    procedure HandleIFOPT(const AParam: String);
    procedure HandleIF(const AParam: String);
    procedure HandleELSE(const AParam: String);
    procedure HandleENDIF(const AParam: String);
    procedure HandleDefine(Param: String); virtual;
    procedure HandleIncludeFile(Param: String); virtual;
    procedure HandleUnDefine(Param: String);virtual;
    function HandleInclude(const Param: String): TToken;virtual;
    procedure HandleMode(const Param: String);virtual;
    function HandleMacro(AIndex: integer): TToken;virtual;
    procedure PushStackItem; virtual;
    function DoFetchTextToken: TToken;
    function DoFetchToken: TToken;
    procedure ClearFiles;
    Procedure ClearMacros;
    Procedure SetCurTokenString(AValue : string);
    function LogEvent(E : TPScannerLogEvent) : Boolean; inline;
  public
    constructor Create(AFileResolver: TBaseFileResolver);
    destructor Destroy; override;
    procedure OpenFile(const AFilename: string);
    function FetchToken: TToken;
    function ReadNonPascalTilEndToken(StopAtLineEnd: boolean): TToken;
    Procedure AddDefine(S : String);
    Procedure RemoveDefine(S : String);
    function CurSourcePos: TPasSourcePos;

    property FileResolver: TBaseFileResolver read FFileResolver;
    property CurSourceFile: TLineReader read FCurSourceFile;
    property CurFilename: string read FCurFilename;
    Property SkipWhiteSpace : Boolean Read FSkipWhiteSpace Write FSkipWhiteSpace;
    Property SkipComments : Boolean Read FSkipComments Write FSkipComments;
    property CurLine: string read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TToken read FCurToken;
    property CurTokenString: string read FCurTokenString;

    property Defines: TStrings read FDefines;
    property Macros: TStrings read FMacros;
    Property Options : TPOptions Read FOptions Write SetOptions;
    Property LogEvents : TPScannerLogEvents Read FLogEvents Write FLogEvents;
    Property OnLog : TPScannerLogHandler Read FOnLog Write FOnLog;

    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
  end;

const
  TokenInfos: array[TToken] of string = (
    'EOF',
    'Whitespace',
    'Comment',
    'Identifier',
    'string',
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
    '<',
    '=',
    '>',
    '@',
    '[',
    ']',
    '^',
    '\',
    '..',
    ':=',
    '<>',
    '<=',
    '>=',
    '**',
    '><',
    '+=',
    '-=',
    '*=',
    '/=',
    // Reserved words
    'absolute',
    'and',
    'array',
    'as',
    'asm',
    'begin',
    'bitpacked',
    'case',
    'class',
    'const',
    'constref',
    'constructor',
    'destructor',
    'dispinterface',
    'div',
    'do',
    'downto',
    'else',
    'end',
    'except',
    'exports',
    'false',
    'file',
    'finalization',
    'finally',
    'for',
    'function',
    'generic',
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
    'specialize',
//    'string',
    'then',
    'threadvar',
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
    'xor',
    'LineEnding',
    'Tab'
  );

function FilenameIsAbsolute(const TheFilename: string):boolean;
function FilenameIsWinAbsolute(const TheFilename: string): boolean;
function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
function IsNamedToken(Const AToken : String; Out T : TToken) : Boolean;

procedure CreateMsgArgs(var MsgArgs: TMessageArgs; Args: array of const);

implementation

Var
  SortedTokens : array of TToken;
  LowerCaseTokens  : Array[ttoken] of String;

Procedure SortTokenInfo;

Var
  tk: tToken;
  I,J,K, l: integer;

begin
  for tk:=Low(TToken) to High(ttoken) do
    LowerCaseTokens[tk]:=LowerCase(TokenInfos[tk]);
  SetLength(SortedTokens,Ord(tkXor)-Ord(tkAbsolute)+1);
  I:=0;
  for tk := tkAbsolute to tkXOR do
    begin
    SortedTokens[i]:=tk;
    Inc(i);
    end;
  l:=Length(SortedTokens)-1;
  k:=l shr 1;
  while (k>0) do
    begin
    for i:=0 to l-k do
      begin
      j:=i;
      while (J>=0) and (LowerCaseTokens[SortedTokens[J]]>LowerCaseTokens[SortedTokens[J+K]]) do
        begin
        tk:=SortedTokens[J];
        SortedTokens[J]:=SortedTokens[J+K];
        SortedTokens[J+K]:=tk;
        if (J>K) then
          Dec(J,K)
        else
          J := 0
        end;
      end;
      K:=K shr 1;
    end;
end;

function IndexOfToken(Const AToken : string) : Integer;

var
  B,T,M : Integer;
  N : String;
begin
  B:=0;
  T:=Length(SortedTokens)-1;
  while (B<=T) do
    begin
    M:=(B+T) div 2;
    N:=LowerCaseTokens[SortedTokens[M]];
    if (AToken<N) then
      T:=M-1
    else if (AToken=N) then
      Exit(M)
    else
      B:=M+1;
    end;
  Result:=-1;
end;

function IsNamedToken(Const AToken : String; Out T : TToken) : Boolean;

Var
  I : Integer;

begin
  if (Length(SortedTokens)=0) then
    SortTokenInfo;
  I:=IndexOfToken(LowerCase(AToken));
  Result:=I<>-1;
  If Result then
    T:=SortedTokens[I];
end;

procedure CreateMsgArgs(var MsgArgs: TMessageArgs; Args: array of const);
var
  i: Integer;
begin
  SetLength(MsgArgs, High(Args)-Low(Args)+1);
  for i:=Low(Args) to High(Args) do
  begin
    case Args[i].VType of
      vtInteger:      MsgArgs[i] := IntToStr(Args[i].VInteger);
      vtBoolean:      MsgArgs[i] := BoolToStr(Args[i].VBoolean);
      vtChar:         MsgArgs[i] := Args[i].VChar;
      {$ifndef FPUNONE}
      vtExtended:     ; //  Args[i].VExtended^;
      {$ENDIF}
      vtString:       MsgArgs[i] := Args[i].VString^;
      vtPointer:      ; //  Args[i].VPointer;
      vtPChar:        MsgArgs[i] := Args[i].VPChar;
      vtObject:       ; //  Args[i].VObject;
      vtClass:        ; //  Args[i].VClass;
      vtWideChar:     MsgArgs[i] := AnsiString(Args[i].VWideChar);
      vtPWideChar:    MsgArgs[i] := Args[i].VPWideChar;
      vtAnsiString:   MsgArgs[i] := AnsiString(Args[i].VAnsiString);
      vtCurrency:     ; //  Args[i].VCurrency^);
      vtVariant:      ; //  Args[i].VVariant^);
      vtInterface:    ; //  Args[i].VInterface^);
      vtWidestring:   MsgArgs[i] := AnsiString(WideString(Args[i].VWideString));
      vtInt64:        MsgArgs[i] := IntToStr(Args[i].VInt64^);
      vtQWord:        MsgArgs[i] := IntToStr(Args[i].VQWord^);
      vtUnicodeString:MsgArgs[i] := AnsiString(UnicodeString(Args[i].VUnicodeString));
    end;
  end;
end;

type
  TIncludeStackItem = class
    SourceFile: TLineReader;
    Filename: string;
    Token: TToken;
    TokenString: string;
    Line: string;
    Row: Integer;
    TokenStr: PChar;
  end;

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  {$IFDEF WINDOWS}
  // windows
  Result:=FilenameIsWinAbsolute(TheFilename);
  {$ELSE}
  // unix
  Result:=FilenameIsUnixAbsolute(TheFilename);
  {$ENDIF}
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  Result:=((length(TheFilename)>=2) and (TheFilename[1] in ['A'..'Z','a'..'z'])
           and (TheFilename[2]=':'))
     or ((length(TheFilename)>=2)
         and (TheFilename[1]='\') and (TheFilename[2]='\'));
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
end;

{ TMacroDef }

constructor TMacroDef.Create(const AName, AValue: String);
begin
  FName:=AName;
  FValue:=AValue;
end;

{ TStreamResolver }

procedure TStreamResolver.SetOwnsStreams(AValue: Boolean);
begin
  if FOwnsStreams=AValue then Exit;
  FOwnsStreams:=AValue;
end;

constructor TStreamResolver.Create;
begin
  Inherited;
  FStreams:=TStringList.Create;
  FStreams.Sorted:=True;
  FStreams.Duplicates:=dupError;
end;

destructor TStreamResolver.Destroy;
begin
  Clear;
  FreeAndNil(FStreams);
  inherited Destroy;
end;

procedure TStreamResolver.Clear;

Var
  I : integer;
begin
  if OwnsStreams then
    begin
    For I:=0 to FStreams.Count-1 do
      Fstreams.Objects[i].Free;
    end;
  FStreams.Clear;
end;

procedure TStreamResolver.AddStream(const AName: String; AStream: TStream);
begin
  FStreams.AddObject(AName,AStream);
end;

function TStreamResolver.FindStream(const AName: string; ScanIncludes : Boolean) : TStream;

Var
  I,J : Integer;
  FN : String;
begin
  Result:=Nil;
  I:=FStreams.IndexOf(AName);
  If (I=-1) and ScanIncludes then
    begin
    J:=0;
    While (I=-1) and (J<IncludePaths.Count-1) do
      begin
      FN:=IncludeTrailingPathDelimiter(IncludePaths[i])+AName;
      I:=FStreams.IndexOf(FN);
      Inc(J);
      end;
    end;
  If (I<>-1) then
    Result:=FStreams.Objects[i] as TStream;
end;

function TStreamResolver.FindStreamReader(const AName: string; ScanIncludes : Boolean) : TLineReader;

Var
  S : TStream;
  SL : TStreamLineReader;

begin
  Result:=Nil;
  S:=FindStream(AName,ScanIncludes);
  If (S<>Nil) then
    begin
    SL:=TStreamLineReader.Create(AName);
    try
      SL.InitFromStream(S);
      Result:=SL;
    except
      FreeAndNil(SL);
      Raise;
    end;
    end;
end;

function TStreamResolver.FindSourceFile(const AName: string): TLineReader;

begin
  Result:=FindStreamReader(AName,False);
end;

function TStreamResolver.FindIncludeFile(const AName: string): TLineReader;
begin
  Result:=FindStreamReader(AName,True);
end;

{ TStringStreamLineReader }

constructor TStringStreamLineReader.Create(const AFilename: string;  const ASource: String);

Var
  S : TStringStream;

begin
  inherited Create(AFilename);
  S:=TStringStream.Create(ASource);
  try
     InitFromStream(S);
  finally
    S.Free;
  end;
end;

{ TFileStreamLineReader }

constructor TFileStreamLineReader.Create(const AFilename: string);

Var
  S : TFileStream;

begin
  inherited Create(AFilename);
  S:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
     InitFromStream(S);
  finally
    S.Free;
  end;
end;

{ TStreamLineReader }

Procedure TStreamLineReader.InitFromStream(AStream : TStream);

begin
  SetLength(FContent,AStream.Size);
  if FContent<>'' then
    AStream.Read(FContent[1],length(FContent));
  FPos:=0;
end;

function TStreamLineReader.IsEOF: Boolean;
begin
  Result:=FPos>=Length(FContent);
end;

function TStreamLineReader.ReadLine: string;

Var
  LPos : Integer;
  EOL : Boolean;

begin
  If isEOF then
    exit;
  LPos:=FPos+1;
  Repeat
    Inc(FPos);
    EOL:=(FContent[FPos] in [#10,#13]);
  until isEOF or EOL;
  If EOL then
   Result:=Copy(FContent,LPos,FPos-LPos)
  else
   Result:=Copy(FContent,LPos,FPos-LPos+1);
  If (not isEOF) and (FContent[FPos]=#13) and (FContent[FPos+1]=#10) then
    inc(FPos);
end;

{ TLineReader }

constructor TLineReader.Create(const AFilename: string);
begin
  FFileName:=AFileName;
end;

{ ---------------------------------------------------------------------
  TFileLineReader
  ---------------------------------------------------------------------}

constructor TFileLineReader.Create(const AFilename: string);

begin
  inherited Create(AFileName);
  Assign(FTextFile, AFilename);
  Reset(FTextFile);
  SetTextBuf(FTextFile,FBuffer,SizeOf(FBuffer));
  FileOpened := true;
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

function TFileLineReader.ReadLine: string;
begin
  ReadLn(FTextFile, Result);
end;

{ ---------------------------------------------------------------------
  TBaseFileResolver
  ---------------------------------------------------------------------}

procedure TBaseFileResolver.SetBaseDirectory(AValue: string);
begin
  if FBaseDirectory=AValue then Exit;
  FBaseDirectory:=AValue;
end;

procedure TBaseFileResolver.SetStrictFileCase(AValue: Boolean);
begin
  if FStrictFileCase=AValue then Exit;
  FStrictFileCase:=AValue;
end;

function TBaseFileResolver.FindIncludeFileName(const AName: string): String;

  function SearchLowUpCase(FN: string): string;

  var
    Dir: String;

  begin
    If FileExists(FN) then
      Result:=FN
    else if StrictFileCase then
      Result:=''
    else
      begin
      Dir:=ExtractFilePath(FN);
      FN:=ExtractFileName(FN);
      Result:=Dir+LowerCase(FN);
      If FileExists(Result) then exit;
      Result:=Dir+uppercase(Fn);
      If FileExists(Result) then exit;
      Result:='';
      end;
  end;

var
  i: Integer;
  FN : string;

begin
  Result := '';
  // convert pathdelims to system
  FN:=SetDirSeparators(AName);
  If FilenameIsAbsolute(FN) then
    begin
    // Maybe this should also do a SearchLowUpCase ?
    if FileExists(FN) then
      Result := FN;
    end
  else
    begin
    // file name is relative
    // search in include path
    I:=0;
    While (Result='') and (I<FIncludePaths.Count) do
      begin
      Result:=SearchLowUpCase(FIncludePaths[i]+AName);
      Inc(I);
      end;
    // search in BaseDirectory
    if (Result='') and (BaseDirectory<>'') then
      Result:=SearchLowUpCase(BaseDirectory+AName);
    end;
end;

constructor TBaseFileResolver.Create;
begin
  inherited Create;
  FIncludePaths := TStringList.Create;
end;

destructor TBaseFileResolver.Destroy;
begin
  FIncludePaths.Free;
  inherited Destroy;
end;

procedure TBaseFileResolver.AddIncludePath(const APath: string);
begin
  if (APath='') then
    FIncludePaths.Add('./')
  else
    FIncludePaths.Add(IncludeTrailingPathDelimiter(ExpandFileName(APath)));
end;

{ ---------------------------------------------------------------------
  TFileResolver
  ---------------------------------------------------------------------}

function TFileResolver.CreateFileReader(const AFileName: String): TLineReader;
begin
  If UseStreams then
    Result:=TFileStreamLineReader.Create(AFileName)
  else
    Result:=TFileLineReader.Create(AFileName);
end;

function TFileResolver.FindSourceFile(const AName: string): TLineReader;
begin
  if not FileExists(AName) then
    Raise EFileNotFoundError.create(Aname)
  else
    try
      Result := CreateFileReader(AName)
    except
      Result := nil;
    end;
end;

function TFileResolver.FindIncludeFile(const AName: string): TLineReader;

Var
  FN : String;

begin
  Result:=Nil;
  FN:=FindIncludeFileName(ANAme);
  If (FN<>'') then
    try
      Result := TFileLineReader.Create(FN);
    except
      Result:=Nil;
    end;
end;

{ ---------------------------------------------------------------------
  TPascalScanner
  ---------------------------------------------------------------------}

constructor TPascalScanner.Create(AFileResolver: TBaseFileResolver);

  Function CS : TStringList;

  begin
    Result:=TStringList.Create;
    Result.Sorted:=True;
    Result.Duplicates:=dupError;
  end;

begin
  inherited Create;
  FFileResolver := AFileResolver;
  FIncludeStack := TFPList.Create;
  FDefines := CS;
  FMacros:=CS;
end;

destructor TPascalScanner.Destroy;
begin
  ClearMacros;
  FreeAndNil(FMacros);
  FreeAndNil(FDefines);
  ClearFiles;
  FIncludeStack.Free;
  inherited Destroy;
end;

procedure TPascalScanner.ClearFiles;

begin
  // Dont' free the first element, because it is CurSourceFile
  while FIncludeStack.Count > 1 do
    begin
    TFileResolver(FIncludeStack[1]).Free;
    FIncludeStack.Delete(1);
    end;
  FIncludeStack.Clear;
  FreeAndNil(FCurSourceFile);
end;

procedure TPascalScanner.ClearMacros;

Var
  I : Integer;

begin
  For I:=0 to FMacros.Count-1 do
      FMacros.Objects[i].Free;
  FMacros.Clear;
end;

procedure TPascalScanner.SetCurTokenString(AValue: string);
begin
  FCurtokenString:=AValue;
end;

procedure TPascalScanner.OpenFile(const AFilename: string);
begin
  Clearfiles;
  FCurSourceFile := FileResolver.FindSourceFile(AFilename);
  if LogEvent(sleFile) then
    DoLog(mtInfo,nLogOpeningFile,SLogOpeningFile,[AFileName],True);
  FCurFilename := AFilename;
  FileResolver.BaseDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(AFilename));
end;

function TPascalScanner.FetchToken: TToken;
var
  IncludeStackItem: TIncludeStackItem;
begin
  while true do
  begin
    Result := DoFetchToken;
    Case FCurToken of
    tkEOF:
      begin
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
        end
      else
        break
      end;
    tkWhiteSpace,
    tkLineEnding:
      if not (FSkipWhiteSpace or PPIsSkipping) then
        Break;
    tkComment:
      if not (FSkipComments or PPIsSkipping) then
        Break;
    else
      if not PPIsSkipping then
        break;
    end; // Case
  end;
//  Writeln(Result, '(',CurTokenString,')');
end;

function TPascalScanner.ReadNonPascalTilEndToken(StopAtLineEnd: boolean
  ): TToken;
var
  StartPos: PChar;

  Procedure Add;
  var
    AddLen: PtrInt;
    OldLen: Integer;
  begin
    AddLen:=TokenStr-StartPos;
    if AddLen=0 then exit;
    OldLen:=length(FCurTokenString);
    SetLength(FCurTokenString,OldLen+AddLen);
    Move(StartPos^,PChar(PChar(FCurTokenString)+OldLen)^,AddLen);
    StartPos:=TokenStr;
  end;

begin
  FCurTokenString := '';
  if (TokenStr = nil) or (TokenStr^ = #0) then
    if not FetchLine then
    begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
    end;

  StartPos:=TokenStr;
  repeat
    case TokenStr[0] of
      #0: // end of line
        begin
          Add;
          if StopAtLineEnd then
            begin
            Result := tkLineEnding;
            FCurToken := Result;
            exit;
            end;
          if not FetchLine then
            begin
            Result := tkEOF;
            FCurToken := Result;
            exit;
            end;
          StartPos:=TokenStr;
        end;
      '0'..'9', 'A'..'Z', 'a'..'z','_':
        begin
          // number or identifier
          if (TokenStr[0] in ['e','E'])
              and (TokenStr[1] in ['n','N'])
              and (TokenStr[2] in ['d','D'])
              and not (TokenStr[3] in ['0'..'9', 'A'..'Z', 'a'..'z','_']) then
            begin
            // 'end' found
            Add;
            Result := tkend;
            SetLength(FCurTokenString, 3);
            Move(TokenStr^, FCurTokenString[1], 3);
            inc(TokenStr,3);
            FCurToken := Result;
            exit;
            end
          else
            begin
            // skip identifier
            while TokenStr[0] in ['0'..'9', 'A'..'Z', 'a'..'z','_'] do
              inc(TokenStr);
            end;
        end;
      else
        inc(TokenStr);
    end;
  until false;
end;

procedure TPascalScanner.Error(MsgNumber: integer; const Msg: string);
begin
  SetCurMsg(mtError,MsgNumber,Msg,[]);
  raise EScannerError.Create(Msg);
end;

procedure TPascalScanner.Error(MsgNumber: integer; const Fmt: string;
  Args: array of const);
begin
  SetCurMsg(mtError,MsgNumber,Fmt,Args);
  raise EScannerError.CreateFmt(Fmt, Args);
end;

function TPascalScanner.DoFetchTextToken:TToken;
var
  OldLength     : Integer;
  TokenStart    : PChar;
  SectionLength : Integer;
begin
  Result:=tkEOF;
  OldLength:=0;
  FCurTokenString := '';

  while TokenStr[0] in ['#', ''''] do
  begin
    case TokenStr[0] of
      '#':
        begin
          TokenStart := TokenStr;
          Inc(TokenStr);
          if TokenStr[0] = '$' then
          begin
            Inc(TokenStr);
            repeat
              Inc(TokenStr);
            until not (TokenStr[0] in ['0'..'9', 'A'..'F', 'a'..'f']);
          end else
            repeat
              Inc(TokenStr);
            until not (TokenStr[0] in ['0'..'9']);
          if Result=tkEOF then Result := tkChar else Result:=tkString;
        end;
      '''':
        begin
          TokenStart := TokenStr;
          Inc(TokenStr);

          while true do
          begin
            if TokenStr[0] = '''' then
              if TokenStr[1] = '''' then
                Inc(TokenStr)
              else
                break;

            if TokenStr[0] = #0 then
              Error(nErrOpenString,SErrOpenString);

            Inc(TokenStr);
          end;
          Inc(TokenStr);
          Result := tkString;
        end;
    else
      Break;
    end;
    SectionLength := TokenStr - TokenStart;
    SetLength(FCurTokenString, OldLength + SectionLength);
    if SectionLength > 0 then
      Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
    Inc(OldLength, SectionLength);
  end;

end;

procedure TPascalScanner.PushStackItem;

Var
  SI: TIncludeStackItem;

begin
  SI := TIncludeStackItem.Create;
  SI.SourceFile := CurSourceFile;
  SI.Filename := CurFilename;
  SI.Token := CurToken;
  SI.TokenString := CurTokenString;
  SI.Line := CurLine;
  SI.Row := CurRow;
  SI.TokenStr := TokenStr;
  FIncludeStack.Add(SI);
  TokenStr:=Nil;
  FCurRow := 0;
end;

procedure TPascalScanner.HandleIncludeFile(Param: String);

begin
  PushStackItem;
  if Length(Param)>1 then
    begin
      if (Param[1]=#39) and (Param[length(Param)]=#39) then
       param:=copy(param,2,length(param)-2);
    end;
  FCurSourceFile := FileResolver.FindIncludeFile(Param);
  if not Assigned(FCurSourceFile) then
    Error(nErrIncludeFileNotFound, SErrIncludeFileNotFound, [Param]);
  FCurFilename := Param;
  if FCurSourceFile is TFileLineReader then
    FCurFilename := TFileLineReader(FCurSourceFile).Filename; // nicer error messages
  If LogEvent(sleFile) then
    DoLog(mtInfo,nLogOpeningFile,SLogOpeningFile,[FCurFileName],True);
end;

function TPascalScanner.HandleMacro(AIndex : integer) : TToken;

Var
  M : TMacroDef;
  ML : TMacroReader;

begin
  PushStackItem;
  M:=FMacros.Objects[AIndex] as TMacroDef;
  ML:=TMacroReader.Create(FCurFileName,M.Value);
  ML.CurRow:=FCurRow;
  ML.CurCol:=CurColumn;
  FCurSourceFile:=ML;
  Result:=DofetchToken;
//  Writeln(Result,Curtoken);
end;

procedure TPascalScanner.HandleDefine(Param: String);

Var
  Index : Integer;
  MN,MV : String;

begin
  Param := UpperCase(Param);
  Index:=Pos(':=',Param);
  If (Index=0) then
    AddDefine(Param)
  else
    begin
    MV:=Trim(Param);
    MN:=Trim(Copy(MV,1,Index-1));
    Delete(MV,1,Index+1);
    Index:=FMacros.IndexOf(MN);
    If (Index=-1) then
      FMacros.AddObject(MN,TMacroDef.Create(MN,MV))
    else
      TMacroDef(FMacros.Objects[index]).Value:=MV;
    end;
end;

procedure TPascalScanner.HandleUnDefine(Param: String);

Var
  Index : integer;

begin
  Param := UpperCase(Param);
  Index:=FDefines.IndexOf(Param);
  If (Index>=0) then
    RemoveDefine(Param)
  else
    begin
    Index := FMacros.IndexOf(Param);
    If (Index>=0) then
      begin
      FMacros.Objects[Index].FRee;
      FMacros.Delete(Index);
      end;
    end;
end;

Function TPascalScanner.HandleInclude(Const Param : String) : TToken;

begin
  Result:=tkComment;
  if ((Param='') or (Param[1]<>'%')) then
    HandleIncludeFile(param)
  else if Param[1]='%' then
    begin
    fcurtokenstring:='{$i '+param+'}';
    fcurtoken:=tkstring;
    result:=fcurtoken;
    end
end;

Procedure TPascalScanner.HandleMode(Const Param : String);

Var
  P : String;

begin
  P:=UpperCase(Param);
  // Eventually, we'll need to make the distinction...
  // For now, treat OBJFPC as Delphi mode.
  if (P='DELPHI') or (P='OBJFPC') then
    Options:=Options+[po_delphi]
  else
    Options:=Options-[po_delphi]
end;

Procedure TPascalScanner.PushSkipMode;

begin
  if PPSkipStackIndex = High(PPSkipModeStack) then
    Error(nErrIfXXXNestingLimitReached,SErrIfXXXNestingLimitReached);
  PPSkipModeStack[PPSkipStackIndex] := PPSkipMode;
  PPIsSkippingStack[PPSkipStackIndex] := PPIsSkipping;
  Inc(PPSkipStackIndex);
end;

Procedure TPascalScanner.HandleIFDEF(Const AParam : String);

Var
  ADefine : String;
  Index : Integer;

begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    ADefine := UpperCase(AParam);
    Index := Defines.IndexOf(ADefine);
    if Index < 0 then
      Index := Macros.IndexOf(ADefine);
    if Index < 0 then
      begin
      PPSkipMode := ppSkipIfBranch;
      PPIsSkipping := true;
      end
    else
      PPSkipMode := ppSkipElseBranch;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogIFDefAccepted,sLogIFDefAccepted,[AParam])
      else
        DoLog(mtInfo,nLogIFDefRejected,sLogIFDefRejected,[AParam])
    end;
end;

Procedure TPascalScanner.HandleIFNDEF(Const AParam : String);

Var
  ADefine : String;
  Index : Integer;

begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    ADefine := UpperCase(AParam);
    Index := Defines.IndexOf(ADefine);
    // Not sure about this
    if Index < 0 then
      Index := Macros.IndexOf(ADefine);
    if Index >= 0 then
      begin
      PPSkipMode := ppSkipIfBranch;
      PPIsSkipping := true;
      end
    else
      PPSkipMode := ppSkipElseBranch;
    If LogEvent(sleConditionals) then
      if PPSkipMode=ppSkipElseBranch then
        DoLog(mtInfo,nLogIFNDefAccepted,sLogIFNDefAccepted,[AParam])
      else
        DoLog(mtInfo,nLogIFNDefRejected,sLogIFNDefRejected,[AParam])
    end;
end;

Procedure TPascalScanner.HandleIFOPT(Const AParam : String);

begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    { !!!: Currently, options are not supported, so they are just
      assumed as not being set. }
    PPSkipMode := ppSkipIfBranch;
    PPIsSkipping := true;
    end;
  If LogEvent(sleConditionals) then
    DoLog(mtInfo,nLogIFOPTIgnored,sLogIFOPTIgnored,[Uppercase(AParam)])
end;

Procedure TPascalScanner.HandleIF(Const AParam : String);

begin
  PushSkipMode;
  if PPIsSkipping then
    PPSkipMode := ppSkipAll
  else
    begin
    { !!!: Currently, expressions are not supported, so they are
      just assumed as evaluating to false. }
    PPSkipMode := ppSkipIfBranch;
    PPIsSkipping := true;
    If LogEvent(sleConditionals) then
       DoLog(mtInfo,nLogIFIgnored,sLogIFIgnored,[Uppercase(AParam)])
    end;
end;

Procedure TPascalScanner.HandleELSE(Const AParam : String);

begin
  if PPSkipStackIndex = 0 then
     Error(nErrInvalidPPElse,sErrInvalidPPElse);
  if PPSkipMode = ppSkipIfBranch then
    PPIsSkipping := false
  else if PPSkipMode = ppSkipElseBranch then
    PPIsSkipping := true;
end;


Procedure TPascalScanner.HandleENDIF(Const AParam : String);

begin
  if PPSkipStackIndex = 0 then
    Error(nErrInvalidPPEndif,sErrInvalidPPEndif);
  Dec(PPSkipStackIndex);
  PPSkipMode := PPSkipModeStack[PPSkipStackIndex];
  PPIsSkipping := PPIsSkippingStack[PPSkipStackIndex];
end;

Function TPascalScanner.HandleDirective(Const ADirectiveText : String) : TToken;

Var
  Directive,Param : String;
  P : Integer;

begin
  Result:=tkComment;
  P:=Pos(' ',ADirectiveText);
  If P=0 then
    P:=Length(ADirectiveText)+1;
  Directive:=Copy(ADirectiveText,2,P-2); // 1 is $
  Param:=ADirectiveText;
  Delete(Param,1,P);
//  Writeln('Directive: "',Directive,'", Param : "',Param,'"');
  Case UpperCase(Directive) of
  'I':
    if not PPIsSkipping then
      Result:=HandleInclude(Param);
  'INCLUDE':
    if not PPIsSkipping then
      Result:=HandleInclude(Param);
  'MODE':
     if not PPIsSkipping then
      HandleMode(Param);
  'DEFINE':
     if not PPIsSkipping then
       HandleDefine(Param);
  'UNDEF':
     if not PPIsSkipping then
       HandleUnDefine(Param);
  'IFDEF':
     HandleIFDEF(Param);
  'IFNDEF':
     HandleIFNDEF(Param);
  'IFOPT':
     HandleIFOPT(Param);
  'IF':
     HandleIF(Param);
  'ELSE':
     HandleELSE(Param);
  'ENDIF':
    HandleENDIF(Param);
  'IFEND':
    HandleENDIF(Param);
  end;
end;

function TPascalScanner.DoFetchToken: TToken;
var
  TokenStart: PChar;
  i: TToken;
  OldLength, SectionLength, NestingLevel, Index: Integer;
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
    #0:         // Empty line
      begin
        FetchLine;
        Result := tkLineEnding;
      end;
    ' ':
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
        until not (TokenStr[0] in [' ']);
      end;
    #9:
      begin
        Result := tkTab;
        repeat
          Inc(TokenStr);
          if TokenStr[0] = #0 then
            if not FetchLine then
            begin
              FCurToken := Result;
              exit;
            end;
        until not (TokenStr[0] in [#9]);
      end;
    '#', '''':
      Result:=DoFetchTextToken;
    '&':
      begin
        TokenStart := TokenStr;
        repeat
          Inc(TokenStr);
        until not (TokenStr[0] in ['0'..'7']);
        SectionLength := TokenStr - TokenStart;
        SetLength(FCurTokenString, SectionLength);
        if SectionLength > 0 then
          Move(TokenStart^, FCurTokenString[1], SectionLength);
        Result := tkNumber;
      end;
    '$':
      begin
        TokenStart := TokenStr;
        repeat
          Inc(TokenStr);
        until not (TokenStr[0] in ['0'..'9', 'A'..'F', 'a'..'f']);
        SectionLength := TokenStr - TokenStart;
        SetLength(FCurTokenString, SectionLength);
        if SectionLength > 0 then
          Move(TokenStart^, FCurTokenString[1], SectionLength);
        Result := tkNumber;
      end;
    '%':
      begin
        TokenStart := TokenStr;
        repeat
          Inc(TokenStr);
        until not (TokenStr[0] in ['0','1']);
        SectionLength := TokenStr - TokenStart;
        SetLength(FCurTokenString, SectionLength);
        if SectionLength > 0 then
          Move(TokenStart^, FCurTokenString[1], SectionLength);
        Result := tkNumber;
      end;
    '(':
      begin
        Inc(TokenStr);
        if TokenStr[0] <> '*' then
          Result := tkBraceOpen
        else
          begin
          // Old-style multi-line comment
          Inc(TokenStr);
          TokenStart := TokenStr;
          FCurTokenString := '';
          OldLength := 0;
          while (TokenStr[0] <> '*') or (TokenStr[1] <> ')') do
            begin
            if TokenStr[0] = #0 then
              begin
              SectionLength:=TokenStr - TokenStart +1;
              SetLength(FCurTokenString, OldLength + SectionLength);
              if SectionLength > 1 then
                Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength - 1);
              Inc(OldLength, SectionLength);
              FCurTokenString[OldLength] := #10;
              if not FetchLine then
                begin
                Result := tkEOF;
                FCurToken := Result;
                exit;
                end;
              TokenStart:=TokenStr;
              end
            else
              Inc(TokenStr);
          end;
          SectionLength := TokenStr - TokenStart;
          SetLength(FCurTokenString, OldLength + SectionLength);
          if SectionLength > 0 then
            Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
          Inc(TokenStr, 2);
          Result := tkComment;
          if Copy(CurTokenString,1,1)='$' then
            Result := HandleDirective(CurTokenString);
          end;
      end;
    ')':
      begin
        Inc(TokenStr);
        Result := tkBraceClose;
      end;
    '*':
      begin
        Result:=tkMul;
        Inc(TokenStr);
        if TokenStr[0] = '*' then
          begin
          Inc(TokenStr);
          Result := tkPower;
          end 
        else if (po_cassignments in options) then
          begin
          if TokenStr[0]='=' then
            begin
            Inc(TokenStr);
            Result:=tkAssignMul;
            end;
          end
      end;
    '+':
      begin
        Result:=tkPlus;
        Inc(TokenStr);
        if (po_cassignments in options) then
          begin
          if TokenStr[0]='=' then
            begin
            Inc(TokenStr);
            Result:=tkAssignPlus;
            end;
          end
      end;
    ',':
      begin
        Inc(TokenStr);
        Result := tkComma;
      end;
    '-':
      begin
        Result := tkMinus;
        Inc(TokenStr);
        if (po_cassignments in options) then
          begin
          if TokenStr[0]='=' then
            begin
            Inc(TokenStr);
            Result:=tkAssignMinus;
            end;
          end
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
        Result := tkDivision;
        Inc(TokenStr);
        if (TokenStr[0] = '/') then       // Single-line comment
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
          end
        else if (po_cassignments in options) then
          begin
          if TokenStr[0]='=' then
            begin
            Inc(TokenStr);
            Result:=tkAssignDivision;
            end;
          end
      end;
    '0'..'9':
      begin
        TokenStart := TokenStr;
        while true do
        begin
          Inc(TokenStr);
          case TokenStr[0] of
            '.':
              begin
                if TokenStr[1] in ['0'..'9', 'e', 'E'] then
                begin
                  Inc(TokenStr);
                  repeat
                    Inc(TokenStr);
                  until not (TokenStr[0] in ['0'..'9', 'e', 'E']);
                end;
                break;
              end;
            '0'..'9': ;
            'e', 'E':
              begin
                Inc(TokenStr);
                if TokenStr[0] = '-'  then
                  Inc(TokenStr);
                while TokenStr[0] in ['0'..'9'] do
                  Inc(TokenStr);
                break;
              end;
            else
              break;
          end;
        end;
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
    '<':
      begin
        Inc(TokenStr);
        if TokenStr[0] = '>' then
          begin
          Inc(TokenStr);
          Result := tkNotEqual;
          end
        else if TokenStr[0] = '=' then
          begin
          Inc(TokenStr);
          Result := tkLessEqualThan;
          end
        else if TokenStr[0] = '<' then
          begin
          Inc(TokenStr);
          Result := tkshl;
          end
        else
          Result := tkLessThan;
      end;
    '=':
      begin
        Inc(TokenStr);
        Result := tkEqual;
      end;
    '>':
      begin
        Inc(TokenStr);
        if TokenStr[0] = '=' then
          begin
          Inc(TokenStr);
          Result := tkGreaterEqualThan;
            end else if TokenStr[0] = '<' then
            begin
          Inc(TokenStr);
          Result := tkSymmetricalDifference;
          end
        else if TokenStr[0] = '>' then
          begin
          Inc(TokenStr);
          Result := tkshr;
          end
        else
          Result := tkGreaterThan;
      end;
    '@':
      begin
        Inc(TokenStr);
        Result := tkAt;
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
    '\':
      begin
        Inc(TokenStr);
        Result := tkBackslash;
      end;
    '{':        // Multi-line comment
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
            if not(po_delphi in Options) and (TokenStr[0] = '{') then
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
        if (Copy(CurTokenString,1,1)='$') then
          Result:=HandleDirective(CurTokenString);
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
        for i := tkAbsolute to tkXOR do
          if CompareText(CurTokenString, TokenInfos[i]) = 0 then
          begin
            Result := i;
            FCurToken := Result;
            exit;
          end;
        Index:=FMacros.IndexOf(CurtokenString);
        if (Index=-1) then
          Result := tkIdentifier
        else
          Result:=HandleMacro(index);
      end;
  else
    if PPIsSkipping then
      Inc(TokenStr)
    else
      Error(nErrInvalidCharacter, SErrInvalidCharacter, [TokenStr[0]]);
  end;

  FCurToken := Result;
end;

function TPascalScanner.LogEvent(E: TPScannerLogEvent): Boolean;
begin
  Result:=E in FLogEvents;
end;

function TPascalScanner.GetCurColumn: Integer;
begin
  If (TokenStr<>Nil) then
    Result := TokenStr - PChar(CurLine)
  else
    Result:=0;
end;

procedure TPascalScanner.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Msg: String; SkipSourceInfo: Boolean);
begin
  DoLog(MsgType,MsgNumber,Msg,[],SkipSourceInfo);
end;

procedure TPascalScanner.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const; SkipSourceInfo: Boolean);
begin
  SetCurMsg(MsgType,MsgNumber,Fmt,Args);
  If Assigned(FOnLog) then
    if SkipSourceInfo then
      FOnLog(Self,FLastMsg)
    else
      FOnLog(Self,Format('%s(%d) : %s',[FCurFileName,FCurRow,FLastMsg]));
end;

procedure TPascalScanner.SetOptions(AValue: TPOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

function TPascalScanner.FetchLine: boolean;
begin
  if CurSourceFile.IsEOF then
  begin
    FCurLine := '';
    TokenStr := nil;
    Result := false;
  end else
  begin
    FCurLine := CurSourceFile.ReadLine;
    TokenStr := PChar(CurLine);
    Result := true;
    Inc(FCurRow);
    if LogEvent(sleLineNumber) and ((FCurRow Mod 100) = 0) then
      DoLog(mtInfo,nLogLineNumber,SLogLineNumber,[FCurRow],True);
  end;
end;

procedure TPascalScanner.SetCurMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const);
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := Format(Fmt,Args);
  CreateMsgArgs(FLastMsgArgs,Args);
end;

procedure TPascalScanner.AddDefine(S: String);

begin
  If FDefines.IndexOf(S)=-1 then
    FDefines.Add(S);
end;

procedure TPascalScanner.RemoveDefine(S: String);

Var
  I : Integer;

begin
  I:=FDefines.IndexOf(S);
  if (I<>-1) then
    FDefines.Delete(I);
end;

function TPascalScanner.CurSourcePos: TPasSourcePos;
begin
  Result.FileName:=CurFilename;
  Result.Row:=CurRow;
  Result.Column:=CurColumn;
end;

end.

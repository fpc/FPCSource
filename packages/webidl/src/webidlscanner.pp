{
    This file is part of the Free Component Library

    WEBIDL source lexical scanner
    Copyright (c) 2021 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

{$IFNDEF FPC_DOTTEDUNITS}
unit webidlscanner;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Classes;
{$ENDIF FPC_DOTTEDUNITS}


type
{$IF SIZEOF(CHAR)=2}
  TIDLString = String;
{$ELSE}
  TIDLString = UTF8String;
{$ENDIF}

  EWebIDLError = class(Exception);

  TWebIDLVersion = (v1,v2);

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

  TMessageArgs = array of TIDLString;
  TIDLToken = (
    tkEOF,
    tkUnknown ,
    tkComment,
    tkWhitespace,
    tkString,
    tkNumberInteger,
    tkNumberFloat,
    // Simple (one-character) tokens
    tkDot, // '.',
    tkSemiColon, // ';'
    tkComma,                 // ','
    tkColon,                 // ':'
    tkBracketOpen,           // '('
    tkBracketClose,          // ')'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkLess, // '<'
    tkLessEqual, // '<='
    tkEqual, // '='
    tkLarger, // '>'
    tkLargerEqual, // '>='
    tkQuestionmark, // '?'
    tkMinus, // '-'
    tkStar, // '*'
    tkIdentifier,            // Any  identifier
    tkTrue,
    tkFalse,
    tkNull,
    tkAny,
    tkAttribute,
    tkCallback,
    tkConst,
    tkDeleter,
    tkDictionary,
    tkEllipsis,
    tkEnum,
    tkGetter,
    tkImplements,
    tkInfinity,
    tkInherit,
    tkInterface,
    tkIterable,
    tkLegacyCaller,
    tkNan,
    tkNegInfinity,
    tkOptional,
    tkOr,
    tkPartial,
    tkReadOnly,
    tkRequired,
    tkSetter,
    tkStatic,
    tkStringifier,
    tkSerializer,
    tkTypedef,
    tkUnrestricted,
    tkPromise,
    tkFrozenArray,
    tkByteString,
    tkDOMString,
    tkUSVString,
    tkboolean,
    tkbyte,
    tkdouble,
    tkfloat,
    tklong,
    tkobject,
    tkoctet,
    tkunsigned,
    tkvoid,
    tkShort,
    tkSequence,
    //tkStringToken, Mattias: there is no TIDLString token in webidl
    tkMixin,
    tkIncludes,
    tkMapLike,
    tkRecord,
    tkSetLike,
    tkOther,
    tkConstructor
    );
  TIDLTokens = Set of TIDLToken;
  EWebIDLScanner = class(EParserError);

Const
  V2Tokens = [tkMixin,tkIncludes,tkMapLike,tkRecord,tkSetLike,tkFrozenArray,tkConstructor];
  V1Tokens = [tkImplements];
  VersionNonTokens : Array[TWebIDLVersion] of TIDLTokens = (V2Tokens,V1Tokens);

  nErrXExpectedButYFound = 1001;
  nErrRangeCheck = 1002;
  nErrOperandAndOperatorMismatch = 1003;
  nErrDivByZero = 1004;
  nErrInvalidCharacterX = 1005;

Type
  TMaxPrecInt = {$ifdef fpc}int64{$else}NativeInt{$endif};
  TMaxFloat = double;

  TDirectiveEvaluator = class;
  TDirectiveEvalVarEvent = function(Sender: TDirectiveEvaluator; Name: TIDLString; out Value: TIDLString): boolean of object;
  TDirectiveEvalFunctionEvent = function(Sender: TDirectiveEvaluator; Name, Param: TIDLString; out Value: TIDLString): boolean of object;
  TDirectiveEvalLogEvent = procedure(Sender: TDirectiveEvaluator; Args : Array of const) of object;

  { TDirectiveEvaluator }

  TDirectiveEvaluator = class
  private
    FOnEvalFunction: TDirectiveEvalFunctionEvent;
    FOnEvalVariable: TDirectiveEvalVarEvent;
    FOnLog: TDirectiveEvalLogEvent;
  protected
    type
      TDirectiveToken = (
        dtEOF,
        dtIdentifier,
        dtNumberInteger,
        dtNumberFloat,
        dtBracketOpen,
        dtBracketClose,
        dtNot,
        dtEqual,
        dtLess,
        dtLessEqual,
        dtGreater,
        dtGreaterEqual
        );
      TPrecedenceLevel = (
        ceplFirst, // tkNot
        ceplSecond, // *, /, div, mod, and, shl, shr
        ceplThird, // +, -, or, xor
        ceplFourth // =, <>, <, >, <=, >=
        );
      TStackItem = record
        Level: TPrecedenceLevel;
        Operathor: TDirectiveToken;
        Operand: TIDLString;
        OperandPos: PChar;
      end;
    const
      BoolValues: array[boolean] of TIDLString = (
        '0', // false
        '1'  // true  Note: True is <>'0'
        );
      dtNames: array[TDirectiveToken] of TIDLString = (
        'EOF',
        'Identifier',
        'Integer',
        'Float',
        '(',
        ')',
        '!',
        '=',
        '<',
        '<=',
        '>',
        '>='
        );
  protected
    FExpr: PChar;
    FToken: TDirectiveToken;
    FTokenStart: PChar;
    FTokenEnd: PChar;
    FStack: array of TStackItem;
    FStackTop: Integer;
    function IsFalse(const Value: TIDLString): boolean;
    function IsTrue(const Value: TIDLString): boolean;
    function IsInteger(const Value: TIDLString; out i: TMaxPrecInt): boolean;
    function IsFloat(const Value: TIDLString; out e: TMaxFloat): boolean;
    procedure NextToken;
    procedure Log(aMsgType: TMessageType; aMsgNumber: integer;
      const aMsgFmt: TIDLString; const Args: array of const; MsgPos: PChar = nil);
    procedure LogXExpectedButTokenFound(const X: TIDLString; ErrorPos: PChar = nil);
    procedure ReadOperand(Skip: boolean = false); // unary operators plus one operand
    procedure ReadExpression; // binary operators
    procedure ResolveStack(MinStackLvl: integer; Level: TPrecedenceLevel;
      NewOperator: TDirectiveToken);
    function GetTokenString: TIDLString;
    function GetStringLiteralValue: TIDLString; // read value of tkString
    procedure Push(const AnOperand: TIDLString; OperandPosition: PChar);
  public
    MsgLineNumber : Integer;
    MsgPos: integer;
    MsgNumber: integer;
    MsgType: TMessageType;
    MsgPattern: TIDLString; // Format parameter
    constructor Create;
    destructor Destroy; override;
    function Eval(const Expr: PChar; aLineNumber: integer): boolean;
    property OnEvalVariable: TDirectiveEvalVarEvent read FOnEvalVariable write FOnEvalVariable;
    property OnEvalFunction: TDirectiveEvalFunctionEvent read FOnEvalFunction write FOnEvalFunction;
    property OnLog: TDirectiveEvalLogEvent read FOnLog write FOnLog;
  end;

  TWebIDLScannerSkipMode = (wisSkipNone, wisSkipIfBranch, wisSkipElseBranch, wisSkipAll);

  { TWebIDLScanner }

  TWebIDLScanner = class
  private
    FCurFile: TIDLString;
    FEvaluator: TDirectiveEvaluator;
    FSource : TStringList;
    FCurRow: Integer;
    FCurToken: TIDLToken;
    FCurTokenString: TIDLString;
    FCurLine: TIDLString;
    FVersion: TWebIDLVersion;
    TokenStr: PChar;
    // Preprocessor #IFxxx skipping data
    FSkipMode: TWebIDLScannerSkipMode;
    FIsSkipping: Boolean;
    FSkipStackIndex: Integer;
    FSkipModeStack: array[0..255] of TWebIDLScannerSkipMode;
    FIsSkippingStack: array[0..255] of Boolean;
    function DetermineToken: TIDLToken;
    function DetermineToken2: TIDLToken;
    function FetchLine: Boolean;
    function GetCurColumn: Integer;
    function OnEvalFunction(Sender: TDirectiveEvaluator; Name, Param: TIDLString;
      out Value: TIDLString): boolean;
    procedure OnEvalLog(Sender: TDirectiveEvaluator; Args: array of const);
    function OnEvalVar(Sender: TDirectiveEvaluator; Name: TIDLString; out
      Value: TIDLString): boolean;
    function ReadComment: TIDLString;
    function ReadIdent: TIDLString;
    function ReadNumber(var S: TIDLString): TIDLToken;
  protected
    Function GetErrorPos : TIDLString;
    procedure Error(const Msg: String);overload;
    procedure Error(const Msg: String; Const Args: array of Const);overload;
    function ReadString: TIDLString; virtual;
    function DoFetchToken: TIDLToken;
    procedure HandleDirective; virtual;
    procedure HandleIfDef; virtual;
    procedure HandleIfNDef; virtual;
    procedure HandleIf; virtual;
    procedure HandleElse; virtual;
    procedure HandleEndIf; virtual;
    procedure PushSkipMode; virtual;
    function IsDefined(const aName: TIDLString): boolean; virtual;
    procedure SkipWhitespace;
    procedure SkipLineBreak;
    procedure Init; virtual;
  public
    constructor Create(Source: TStream); overload;
    constructor Create(const Source: TIDLString); overload;
    constructor CreateFile(const aFileName: TIDLString);
    destructor Destroy; override;
    function FetchToken: TIDLToken;

    property CurLine: TIDLString read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    property CurFile: TIDLString read FCurFile write FCurFile;

    property CurToken: TIDLToken read FCurToken;
    property CurTokenString: TIDLString read FCurTokenString;
    property Version : TWebIDLVersion Read FVersion Write FVersion;

    property Evaluator: TDirectiveEvaluator read FEvaluator;
  end;

const
  TokenInfos: array[TIDLToken] of TIDLString = (
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  // Simple (one-character) tokens
  '.',
  ';',
  ',',                 // ','
  ':',                 // ':'
  '(',           // '('
  ')',          // ')'
  '{',        // '{'
  '}',       // '}'
  '[',       // '['
  ']',      // ']'
  '<',
  '<=',
  '=',
  '>',
  '>=',
  '?',
  '-',
  '*',
  '',            // Any  identifier
  'true',
  'false',
  'null',
  'any',
  'attribute',
  'callback',
  'const',
  'deleter',
  'dictionary',
  'ellipsis',
  'enum',
  'getter',
  'implements',
  'Infinity',
  'inherit',
  'interface',
  'iterable',
  'legacycaller',
  'NaN',
  '-Infinity',
  'optional',
  'or',
  'partial',
  'readonly',
  'required',
  'setter',
  'static',
  'stringifier',
  'serializer',
  'typedef',
  'unrestricted',
  'Promise',
  'FrozenArray',
  'ByteString',
  'DOMString',
  'USVString',
  'boolean',
  'byte',
  'double',
  'float',
  'long',
  'object',
  'octet',
  'unsigned',
  'void',
  'short',
  'sequence',
  //'TIDLString',
  'mixin',
  'includes',
  'maplike',
  'record',
  'setlike',
  'other',
  'constructor'
  );

Function GetTokenName(aToken : TIDLToken) : String;
Function GetTokenNames(aTokenList : TIDLTokens) : String;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  SErrUnknownTerminator = 'Unknown terminator: "%s"';
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d: ''%s''';
  SErrOpenString = 'TIDLString exceeds end of line';
  SErrInvalidEllipsis = 'Invalid ellipsis token';
  SErrUnknownToken = 'Unknown token, expected number or minus : "%s"';
  SErrXExpectedButYFound = '"%s" expected, but "%s" found';
  SErrRangeCheck = 'range check failed';
  SErrOperandAndOperatorMismatch = 'operand and operator mismatch';
  SErrDivByZero = 'division by zero';
  SErrInvalidCharacterX = 'Invalid character ''%s''';
  SErrUnknownDirectiveX = 'Unknown directive ''%s''';

Function MakeString(P : PChar; Len : Integer) : TIDLString; inline;

begin
  Result:='';
  SetLength(Result,Len);
  Move(P^,Result[1],Len*Sizeof(Char));
end;


Function GetTokenName(aToken : TIDLToken) : String;

begin
  Result:=TokenInfos[aToken];
  if Result='' then
    begin
    Result:=GetEnumName(TypeInfo(TIDLToken),Ord(aToken));
    Delete(Result,1,2);
    end;
end;

Function GetTokenNames(aTokenList : TIDLTokens) : String;

Var
  T : TIDLToken;

begin
  Result:='';
  For T in aTokenList do
    begin
    if (Result<>'') then
      Result:=Result+',';
    Result:=Result+GetTokenName(T);
    end;
end;

{ TDirectiveEvaluator }

function TDirectiveEvaluator.IsFalse(const Value: TIDLString): boolean;
begin
  Result:=Value=BoolValues[false];
end;

function TDirectiveEvaluator.IsTrue(const Value: TIDLString): boolean;
begin
  Result:=Value<>BoolValues[false];
end;

function TDirectiveEvaluator.IsInteger(const Value: TIDLString; out i: TMaxPrecInt
  ): boolean;
var
  Code: integer;
begin
  val(Value,i,Code);
  Result:=Code=0;
end;

function TDirectiveEvaluator.IsFloat(const Value: TIDLString; out e: TMaxFloat
  ): boolean;
var
  Code: integer;
begin
  val(Value,e,Code);
  Result:=Code=0;
end;

procedure TDirectiveEvaluator.NextToken;
const
  IdentChars = ['a'..'z','A'..'Z','0'..'9','_'];
  Digits = ['0'..'9'];
begin
  FTokenStart:=FTokenEnd;

  // skip white space
  repeat
    case FTokenStart^ of
      #0:
        begin
        FToken:=dtEOF;
        FTokenEnd:=FTokenStart;
        exit;
        end;
      #9,#10,#13,' ':
        inc(FTokenStart);
      else break;
    end;
  until false;

  // read token
  FTokenEnd:=FTokenStart;
  case FTokenEnd^ of
  'a'..'z','A'..'Z','_':
    begin
    inc(FTokenEnd);
    while FTokenEnd^ in IdentChars do inc(FTokenEnd);
    FToken:=dtIdentifier;
    end;
  '0'..'9':
    begin
    FToken:=dtNumberInteger;
    // examples: 1, 1.2, 1.2E3, 1E-2
    inc(FTokenEnd);
    while FTokenEnd^ in Digits do inc(FTokenEnd);
    if (FTokenEnd^='.') and (FTokenEnd[1]<>'.') then
      begin
      FToken:=dtNumberFloat;
      inc(FTokenEnd);
      while FTokenEnd^ in Digits do inc(FTokenEnd);
      end;
    if FTokenEnd^ in ['e','E'] then
      begin
      FToken:=dtNumberFloat;
      inc(FTokenEnd);
      if FTokenEnd^ in ['-','+'] then inc(FTokenEnd);
      while FTokenEnd^ in Digits do inc(FTokenEnd);
      end;
    end;
  '(':
    begin
    FToken:=dtBracketOpen;
    inc(FTokenEnd);
    end;
  ')':
    begin
    FToken:=dtBracketClose;
    inc(FTokenEnd);
    end;
  '=':
    begin
    FToken:=dtEqual;
    inc(FTokenEnd);
    end;
  '!':
    begin
    FToken:=dtNot;
    inc(FTokenEnd);
    end;
  '<':
    begin
    inc(FTokenEnd);
    case FTokenEnd^ of
    '=':
      begin
      FToken:=dtLessEqual;
      inc(FTokenEnd);
      end;
    //'<':
    //  begin
    //  FToken:=tkshl;
    //  inc(FTokenEnd);
    //  end;
    else
      FToken:=dtLess;
    end;
    end;
  '>':
    begin
    inc(FTokenEnd);
    case FTokenEnd^ of
    '=':
      begin
      FToken:=dtGreaterEqual;
      inc(FTokenEnd);
      end;
    //'>':
    //  begin
    //  FToken:=tkshr;
    //  inc(FTokenEnd);
    //  end;
    else
      FToken:=dtGreater;
    end;
    end;
  //'+':
  //  begin
  //  FToken:=tkPlus;
  //  inc(FTokenEnd);
  //  end;
  //'-':
  //  begin
  //  FToken:=tkMinus;
  //  inc(FTokenEnd);
  //  end;
  //'*':
  //  begin
  //  FToken:=tkMul;
  //  inc(FTokenEnd);
  //  end;
  //'/':
  //  begin
  //  FToken:=tkDivision;
  //  inc(FTokenEnd);
  //  end;
  //'''':
  //  begin
  //  FToken:=tkString;
  //  repeat
  //    inc(FTokenEnd);
  //    if FTokenEnd^='''' then
  //      begin
  //      inc(FTokenEnd);
  //      if FTokenEnd^<>'''' then break;
  //      end
  //    else if FTokenEnd^ in [#0,#10,#13] then
  //      Log(mtError,nErrOpenString,SErrOpenString,[]);
  //  until false;
  //  end
  else
    FToken:=dtEOF;
  end;
end;

procedure TDirectiveEvaluator.Log(aMsgType: TMessageType; aMsgNumber: integer;
  const aMsgFmt: TIDLString; const Args: array of const; MsgPos: PChar);
begin
  if MsgPos=nil then
    MsgPos:=FTokenEnd;
  MsgType:=aMsgType;
  MsgNumber:=aMsgNumber;
  MsgPattern:=aMsgFmt;
  if Assigned(OnLog) then
    begin
    OnLog(Self,Args);
    if not (aMsgType in [mtError,mtFatal]) then exit;
    end;
  raise EWebIDLError.CreateFmt(MsgPattern+' at pos '+IntToStr(PtrInt(MsgPos-FExpr))+' line '+IntToStr(MsgLineNumber),Args);
end;

procedure TDirectiveEvaluator.LogXExpectedButTokenFound(const X: TIDLString;
  ErrorPos: PChar);
begin
  Log(mtError,nErrXExpectedButYFound,SErrXExpectedButYFound,
      [X,dtNames[FToken]],ErrorPos);
end;

procedure TDirectiveEvaluator.ReadOperand(Skip: boolean);
{ Read operand and put it on the stack
  Examples:
   Variable
   defined(Variable)
   !Variable
   123
   $45
   'Abc'
   (expression)
}
var
  i: TMaxPrecInt;
  e: TMaxFloat;
  S, aName, Param: TIDLString;
  Code: integer;
  p, NameStartP: PChar;
  Lvl: integer;
begin
  {$IFDEF VerboseWebIDLScanner}
  writeln('TDirectiveEvaluator.ReadOperand START Token[',FTokenStart-FExpr+1,']="',GetTokenString,'" ',FToken,BoolToStr(Skip,' SKIP',''));
  {$ENDIF}
  case FToken of
    dtNot:
      begin
      // boolean not
      NextToken;
      ReadOperand(Skip);
      if not Skip then
        FStack[FStackTop].Operand:=BoolValues[IsFalse(FStack[FStackTop].Operand)];
      end;
    //tkMinus:
    //  begin
    //  // unary minus
    //  NextToken;
    //  ReadOperand(Skip);
    //  if not Skip then
    //    begin
    //    i:=StrToInt64Def(FStack[FStackTop].Operand,0);
    //    FStack[FStackTop].Operand:=IntToStr(-i);
    //    end;
    //  end;
    //tkPlus:
    //  begin
    //  // unary plus
    //  NextToken;
    //  ReadOperand(Skip);
    //  if not Skip then
    //    begin
    //    i:=StrToInt64Def(FStack[FStackTop].Operand,0);
    //    FStack[FStackTop].Operand:=IntToStr(i);
    //    end;
    //  end;
    dtNumberInteger:
      begin
      // integer
      if not Skip then
        begin
        S:=GetTokenString;
        val(S,i,Code);
        if Code=0 then
          Push(IntToStr(i),FTokenStart)
        else
          Log(mtError,nErrRangeCheck,sErrRangeCheck,[]);
        end;
      NextToken;
      end;
    dtNumberFloat:
      begin
      // float
      if not Skip then
        begin
        S:=GetTokenString;
        val(S,e,Code);
        if Code>0 then
          Log(mtError,nErrRangeCheck,sErrRangeCheck,[]);
        if e=0 then ;
        // float
        Push(S,FTokenStart);
        end;
      NextToken;
      end;
    //tkString:
    //  begin
    //  // TIDLString literal
    //  if not Skip then
    //    Push(GetStringLiteralValue,FTokenStart{$ifdef UsePChar}-PChar(Expression)+1{$endif});
    //  NextToken;
    //  end;
    dtIdentifier:
      if Skip then
        begin
        aName:=GetTokenString;
        NextToken;
        if FToken=dtBracketOpen then
          begin
          // only one parameter is supported
          NextToken;
          if FToken=dtIdentifier then
            NextToken;
          if FToken<>dtBracketClose then
            LogXExpectedButTokenFound(')');
          NextToken;
          end;
        end
      else
        begin
        aName:=GetTokenString;
        p:=FTokenStart;
        NextToken;
        if FToken=dtBracketOpen then
          begin
          // function
          NameStartP:=FTokenStart;
          NextToken;
          // only one parameter is supported
          Param:='';
          if FToken=dtIdentifier then
            begin
            Param:=GetTokenString;
            NextToken;
            end;
          if FToken<>dtBracketClose then
            LogXExpectedButTokenFound(')');
          if not OnEvalFunction(Self,aName,Param,S) then
            begin
            FTokenStart:=NameStartP;
            FTokenEnd:=FTokenStart+length(aName);
            LogXExpectedButTokenFound('function');
            end;
          Push(S,p);
          NextToken;
          end
        else
          begin
          // variable
          if OnEvalVariable(Self,aName,S) then
            Push(S,p)
          else
            begin
            // variable does not exist -> evaluates to false
            Push(BoolValues[false],p);
            end;
          end;
        end;
    dtBracketOpen:
      begin
      NextToken;
      if Skip then
        begin
        Lvl:=1;
        repeat
          case FToken of
          dtEOF:
            LogXExpectedButTokenFound(')');
          dtBracketOpen: inc(Lvl);
          dtBracketClose:
            begin
            dec(Lvl);
            if Lvl=0 then break;
            end;
          else
            // Do nothing, satisfy compiler
          end;
          NextToken;
        until false;
        end
      else
        begin
        ReadExpression;
        if FToken<>dtBracketClose then
          LogXExpectedButTokenFound(')');
        end;
      NextToken;
      end;
  else
    LogXExpectedButTokenFound('identifier');
  end;
  {$IFDEF VerboseWebIDLScanner}
  writeln('TDirectiveEvaluator.ReadOperand END Top=',FStackTop,' Value="',FStack[FStackTop].Operand,'" Token[',FTokenStart-FExpr+1,']="',GetTokenString,'" ',FToken);
  {$ENDIF}
end;

procedure TDirectiveEvaluator.ReadExpression;
// read operand operator operand ... til tkEOF or tkBracketClose
var
  OldStackTop: Integer;

  procedure ReadBinary(Level: TPrecedenceLevel; NewOperator: TDirectiveToken);
  begin
    ResolveStack(OldStackTop,Level,NewOperator);
    NextToken;
    ReadOperand;
  end;

begin
  OldStackTop:=FStackTop;
  {$IFDEF VerboseWebIDLScanner}
  writeln('TDirectiveEvaluator.ReadExpression START Top=',FStackTop,' Token[',FTokenStart-FExpr+1,']="',GetTokenString,'" ',FToken);
  {$ENDIF}
  ReadOperand;
  repeat
    {$IFDEF VerboseWebIDLScanner}
    writeln('TDirectiveEvaluator.ReadExpression NEXT Top=',FStackTop,' Token[',FTokenStart-FExpr+1,']="',GetTokenString,'" ',FToken);
    {$ENDIF}
    case FToken of
    dtEOF,dtBracketClose:
      begin
      ResolveStack(OldStackTop,high(TPrecedenceLevel),dtEOF);
      exit;
      end;
    //tkand:
    //  begin
    //  ResolveStack(OldStackTop,ceplSecond,tkand);
    //  NextToken;
    //  if (FStackTop=OldStackTop+1) and IsFalse(FStack[FStackTop].Operand) then
    //    begin
    //    // false and ...
    //    // -> skip all "and"
    //    repeat
    //      ReadOperand(true);
    //      if FToken<>tkand then break;
    //      NextToken;
    //    until false;
    //    FStack[FStackTop].Operathor:=tkEOF;
    //    end
    //  else
    //    ReadOperand;
    //  end;
    //tkMul,tkDivision,tkdiv,tkmod,tkshl,tkshr:
    //  ReadBinary(ceplSecond,FToken);
    //tkor:
    //  begin
    //  ResolveStack(OldStackTop,ceplThird,tkor);
    //  NextToken;
    //  if (FStackTop=OldStackTop+1) and IsTrue(FStack[FStackTop].Operand) then
    //    begin
    //    // true or ...
    //    // -> skip all "and" and "or"
    //    repeat
    //      ReadOperand(true);
    //      if not (FToken in [tkand,tkor]) then break;
    //      NextToken;
    //    until false;
    //    FStack[FStackTop].Operathor:=tkEOF;
    //    end
    //  else
    //    ReadOperand;
    //  end;
    //tkPlus,tkMinus,tkxor:
    //  ReadBinary(ceplThird,FToken);
    dtEqual,dtNot,dtLess,dtLessEqual,dtGreater,dtGreaterEqual:
      ReadBinary(ceplFourth,FToken);
    else
      LogXExpectedButTokenFound('operator');
    end;
  until false;
end;

procedure TDirectiveEvaluator.ResolveStack(MinStackLvl: integer;
  Level: TPrecedenceLevel; NewOperator: TDirectiveToken);
var
  A, B, R: TIDLString;
  Op: TDirectiveToken;
  AInt, BInt: TMaxPrecInt;
  AFloat, BFloat: TMaxFloat;
  BPos: PChar;
begin
  // resolve all higher or equal level operations
  // Note: the stack top contains operand B
  //       the stack second contains operand A and the operator between A and B

  //writeln('TDirectiveEvaluator.ResolveStack FStackTop=',FStackTop,' MinStackLvl=',MinStackLvl);
  //if FStackTop>MinStackLvl+1 then
  //  writeln('  FStack[FStackTop-1].Level=',FStack[FStackTop-1].Level,' Level=',Level);
  while (FStackTop>MinStackLvl+1) and (FStack[FStackTop-1].Level<=Level) do
    begin
    // pop last operand and operator from stack
    B:=FStack[FStackTop].Operand;
    BPos:=FStack[FStackTop].OperandPos;
    dec(FStackTop);
    Op:=FStack[FStackTop].Operathor;
    A:=FStack[FStackTop].Operand;
    {$IFDEF VerboseWebIDLScanner}
    writeln('  ResolveStack Top=',FStackTop,' A="',A,'" ',Op,' B="',B,'"');
    {$ENDIF}
    {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
    {$R+}
    try
      case Op of
      //tkand: // boolean and
      //  R:=BoolValues[IsTrue(A) and IsTrue(B)];
      //tkor: // boolean or
      //  R:=BoolValues[IsTrue(A) or IsTrue(B)];
      //tkxor: // boolean xor
      //  R:=BoolValues[IsTrue(A) xor IsTrue(B)];
      //tkMul, tkdiv, tkmod, tkshl, tkshr, tkPlus, tkMinus:
      //  if IsInteger(A,AInt) then
      //    begin
      //    if IsInteger(B,BInt) then
      //      case Op of
      //        tkMul: R:=IntToStr(AInt*BInt);
      //        tkdiv: R:=IntToStr(AInt div BInt);
      //        tkmod: R:=IntToStr(AInt mod BInt);
      //        tkshl: R:=IntToStr(AInt shl BInt);
      //        tkshr: R:=IntToStr(AInt shr BInt);
      //        tkPlus: R:=IntToStr(AInt+BInt);
      //        tkMinus: R:=IntToStr(AInt-BInt);
      //      else
      //         Do nothing, satisfy compiler
      //      end
      //    else if IsExtended(B,BFloat) then
      //      case Op of
      //        tkMul: R:=FloatToStr(Extended(AInt)*BFloat);
      //        tkPlus: R:=FloatToStr(Extended(AInt)+BFloat);
      //        tkMinus: R:=FloatToStr(Extended(AInt)-BFloat);
      //      else
      //        LogXExpectedButTokenFound('integer',BPos);
      //      end
      //    else
      //      LogXExpectedButTokenFound('integer',BPos);
      //    end
      //  else if IsExtended(A,AFloat) then
      //    begin
      //    if IsExtended(B,BFloat) then
      //      case Op of
      //        tkMul: R:=FloatToStr(AFloat*BFloat);
      //        tkPlus: R:=FloatToStr(AFloat+BFloat);
      //        tkMinus: R:=FloatToStr(AFloat-BFloat);
      //      else
      //        LogXExpectedButTokenFound('float',BPos);
      //      end
      //    else
      //      LogXExpectedButTokenFound('float',BPos);
      //    end
      //  else
      //    Log(mtError,nErrOperandAndOperatorMismatch,sErrOperandAndOperatorMismatch,[]);
      //tkDivision:
      //  if IsExtended(A,AFloat) then
      //    begin
      //    if IsExtended(B,BFloat) then
      //      R:=FloatToStr(AFloat/BFloat)
      //    else
      //      LogXExpectedButTokenFound('float',BPos);
      //    end
      //  else
      //    Log(mtError,nErrOperandAndOperatorMismatch,sErrOperandAndOperatorMismatch,[]);
      dtEqual,
      dtNot,
      dtLess,dtLessEqual,
      dtGreater,dtGreaterEqual:
        begin
        if IsInteger(A,AInt) and IsInteger(B,BInt) then
          case Op of
          dtEqual: R:=BoolValues[AInt=BInt];
          dtNot: R:=BoolValues[AInt<>BInt];
          dtLess: R:=BoolValues[AInt<BInt];
          dtLessEqual: R:=BoolValues[AInt<=BInt];
          dtGreater: R:=BoolValues[AInt>BInt];
          dtGreaterEqual: R:=BoolValues[AInt>=BInt];
          else
          // Do nothing, satisfy compiler
          end
        else if IsFloat(A,AFloat) and IsFloat(B,BFloat) then
          case Op of
          dtEqual: R:=BoolValues[AFloat=BFloat];
          dtNot: R:=BoolValues[AFloat<>BFloat];
          dtLess: R:=BoolValues[AFloat<BFloat];
          dtLessEqual: R:=BoolValues[AFloat<=BFloat];
          dtGreater: R:=BoolValues[AFloat>BFloat];
          dtGreaterEqual: R:=BoolValues[AFloat>=BFloat];
          else
          // Do nothing, satisfy compiler
          end
        else
          case Op of
          dtEqual: R:=BoolValues[A=B];
          dtNot: R:=BoolValues[A<>B];
          dtLess: R:=BoolValues[A<B];
          dtLessEqual: R:=BoolValues[A<=B];
          dtGreater: R:=BoolValues[A>B];
          dtGreaterEqual: R:=BoolValues[A>=B];
          else
          // Do nothing, satisfy compiler
          end;
        end;
      else
        Log(mtError,nErrOperandAndOperatorMismatch,sErrOperandAndOperatorMismatch,[]);
      end;
    except
      on E: EDivByZero do
        Log(mtError,nErrDivByZero,sErrDivByZero,[]);
      on E: EZeroDivide do
        Log(mtError,nErrDivByZero,sErrDivByZero,[]);
      on E: EMathError do
        Log(mtError,nErrRangeCheck,sErrRangeCheck+' '+E.Message,[]);
      on E: EInterror do
        Log(mtError,nErrRangeCheck,sErrRangeCheck+' '+E.Message,[]);
    end;
    {$IFNDEF RangeChecking}{$R-}{$UNDEF RangeChecking}{$ENDIF}
    {$IFDEF VerboseWebIDLScanner}
    writeln('  ResolveStack Top=',FStackTop,' A="',A,'" ',Op,' B="',B,'" = "',R,'"');
    {$ENDIF}
    FStack[FStackTop].Operand:=R;
    FStack[FStackTop].OperandPos:=BPos;
    end;
  FStack[FStackTop].Operathor:=NewOperator;
  FStack[FStackTop].Level:=Level;
end;

function TDirectiveEvaluator.GetTokenString: TIDLString;

begin
  Result:=MakeString(FTokenStart,FTokenEnd-FTokenStart);
end;

function TDirectiveEvaluator.GetStringLiteralValue: TIDLString;
var
  p, StartP: PChar;
  s: TIDLString;
  len : Integer;

begin
  S:='';
  Result:='';
  p:=FTokenStart;
  repeat
    case p^ of
    '''':
      begin
      inc(p);
      StartP:=p;
      repeat
        case p^ of
        #0,#10,#13: Log(mtError,nErrInvalidCharacterX,SErrInvalidCharacterX,['#0']);
        '''': break;
        else inc(p);
        end;
      until false;
      if p>StartP then
        begin
        S:=MakeString(StartP,p-StartP);
        Result:=Result+s;
        end;
      inc(p);
      end;
    else
      Log(mtError,nErrInvalidCharacterX,SErrInvalidCharacterX,['#0']);
    end;
  until false;
end;

procedure TDirectiveEvaluator.Push(const AnOperand: TIDLString;
  OperandPosition: PChar);
begin
  inc(FStackTop);
  if FStackTop>=length(FStack) then
    SetLength(FStack,length(FStack)*2+4);
  with FStack[FStackTop] do
    begin
    Operand:=AnOperand;
    OperandPos:=OperandPosition;
    Operathor:=dtEOF;
    Level:=ceplFourth;
    end;
end;

constructor TDirectiveEvaluator.Create;
begin

end;

destructor TDirectiveEvaluator.Destroy;
begin
  inherited Destroy;
end;

function TDirectiveEvaluator.Eval(const Expr: PChar; aLineNumber: integer
  ): boolean;
begin
  {$IFDEF VerboseWebIDLScanner}
  writeln('TDirectiveEvaluator.Eval Line=',aLineNumber,' Expr="',Expr,'"');
  {$ENDIF}
  MsgLineNumber:=aLineNumber;
  fExpr:=Expr;
  FTokenStart:=Expr;
  FTokenEnd:=FTokenStart;
  FStackTop:=-1;
  NextToken;
  ReadExpression;
  Result:=IsTrue(FStack[0].Operand);
end;

{ TWebIDLScanner }

constructor TWebIDLScanner.Create(Source: TStream);
begin
  Init;
  FSource.LoadFromStream(Source);
end;

constructor TWebIDLScanner.Create(const Source: TIDLString);
begin
  Init;
  FSource.Text:=Source;
end;

constructor TWebIDLScanner.CreateFile(const aFileName: TIDLString);
begin
  Init;
  FSource.LoadFromFile(aFileName);
  FCurFile:=aFileName;
end;

destructor TWebIDLScanner.Destroy;
begin
  FreeAndNil(FEvaluator);
  FreeAndNil(FSource);
  Inherited;
end;

function TWebIDLScanner.FetchToken: TIDLToken;

begin
  Result:=DoFetchToken;
end;

procedure TWebIDLScanner.Error(const Msg: String);
begin
  raise EWebIDLScanner.Create(GetErrorPos+Msg);
end;

procedure TWebIDLScanner.Error(const Msg: String; const Args: array of const);
begin
  raise EWebIDLScanner.Create(GetErrorPos+Format(Msg, Args));
end;

function TWebIDLScanner.ReadString : TIDLString;

Var
  C : Char;
  I, OldLength, SectionLength: Integer;
  S : TIDLString;
  TokenStart: PChar;
begin
  C:=TokenStr[0];
  Inc(TokenStr);
  TokenStart := TokenStr;
  OldLength := 0;
  Result := '';
  while not (TokenStr[0] in [#0,C]) do
    begin
    if (TokenStr[0]='\') then
      begin
      // Save length
      SectionLength := TokenStr - TokenStart;
      Inc(TokenStr);
      // Read escaped token
      Case TokenStr[0] of
        '"' : S:='"';
        '''' : S:='''';
        't' : S:=#9;
        'b' : S:=#8;
        'n' : S:=#10;
        'r' : S:=#13;
        'f' : S:=#12;
        '\' : S:='\';
        '/' : S:='/';
        'u' : begin
              S:='0000';
              For I:=1 to 4 do
                begin
                Inc(TokenStr);
                Case TokenStr[0] of
                  '0'..'9','A'..'F','a'..'f' :
                    S[i]:=Upcase(TokenStr[0]);
                else
                  Error(SErrInvalidCharacter, [CurRow,CurColumn,TokenStr[0]]);
                end;
                end;
              // WideChar takes care of conversion...
{$IF SIZEOF(CHAR)=1}
              S:=Utf8Encode(WideString(WideChar(StrToInt('$'+S))));
{$ELSE}
              S:=WideChar(StrToInt('$'+S));
{$ENDIF}
              end;
        #0  : Error(SErrOpenString);
      else
        Error(SErrInvalidCharacter, [CurRow,CurColumn,TokenStr[0]]);
      end;
      SetLength(Result, OldLength + SectionLength+1+Length(S));
      if SectionLength > 0 then
        Move(TokenStart^, Result[OldLength + 1], SectionLength*SizeOf(Char));
      Move(S[1],Result[OldLength + SectionLength+1],Length(S)*SizeOf(char));
      Inc(OldLength, SectionLength+Length(S));
      // Next char
      // Inc(TokenStr);
      TokenStart := TokenStr+1;
      end;
    if TokenStr[0] = #0 then
      Error(SErrOpenString);
    Inc(TokenStr);
    end;
  if TokenStr[0] = #0 then
    Error(SErrOpenString);
  SectionLength := TokenStr - TokenStart;
  SetLength(Result, OldLength + SectionLength);
  if SectionLength > 0 then
    Move(TokenStart^, Result[OldLength + 1], SectionLength*SizeOf(Char));
  Inc(TokenStr);
end;

function TWebIDLScanner.ReadIdent: TIDLString;

Var
  TokenStart : PChar;
  SectionLength : Integer;

begin
  Result:='';
  if TokenStr[0]='_' then
    Inc(TokenStr);
  if TokenStr[0]=#0 then
    Exit;
  TokenStart := TokenStr;
  repeat
    Inc(TokenStr);
  until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  SectionLength := TokenStr - TokenStart;

  SetString(Result, TokenStart, SectionLength);
end;

function TWebIDLScanner.FetchLine: Boolean;

begin
  Result:=FCurRow<FSource.Count;
  if Result then
    begin
    FCurLine:=FSource[FCurRow];
    TokenStr:=PChar(FCurLine);
    Inc(FCurRow);
    end
  else
    begin
    FCurLine:='';
    TokenStr:=nil;
    end;
end;

function TWebIDLScanner.ReadNumber(var S : TIDLString) : TIDLToken;

Var
  TokenStart : PChar;
  SectionLength : Integer;
  isHex : Boolean;

begin
  isHex:=False;
  TokenStart := TokenStr;
  Result:=tkNumberInteger;
  while true do
    begin
    Inc(TokenStr);
    SectionLength := TokenStr - TokenStart;
    case TokenStr[0] of
    'x':
      begin
      isHex:=True;
      end;
    'I':
      begin
      repeat
        Inc(TokenStr);
      until not (TokenStr[0] in ['i','n','f','t','y']);
      Result:=tkNegInfinity; // We'll check at the end if the TIDLString is actually correct
      break;
      end;
    '.':
      begin
      Result:=tkNumberFloat;
      if TokenStr[1] in ['0'..'9', 'e', 'E'] then
        begin
        Inc(TokenStr);
        repeat
          Inc(TokenStr);
        until not (TokenStr[0] in ['0'..'9', 'e', 'E','-','+']);
        end;
        break;
      end;
    '0'..'9':
      begin
      end;
    'a'..'d','f',
    'A'..'D','F':
      begin
      if Not isHex then
        Error(SErrUnknownToken,[S]);
      end;
    'e', 'E':
      begin
      if not  IsHex then
        begin
        Inc(TokenStr);
        if TokenStr[0] in ['-','+']  then
          Inc(TokenStr);
        while TokenStr[0] in ['0'..'9'] do
          Inc(TokenStr);
        break;
        end;
      end;
    else
      if (SectionLength=1) and (TokenStart[0]='-') then
        result:=tkMinus;
      break;
    end;
    end;
  SectionLength := TokenStr - TokenStart;
  S:=MakeString(TokenStart,SectionLength);
  if (Result=tkNegInfinity) and (S<>'-Infinity') then
    Error(SErrUnknownToken,[S]);
  if (Result=tkMinus) and (S<>'-') then
    Error(SErrUnknownTerminator,[s]);
end;

function TWebIDLScanner.GetErrorPos: TIDLString;
begin
  Result:=CurFile+'('+IntToStr(CurRow)+','+IntToStr(CurColumn)+')';
  Result:=Format('Scanner error at %s: ',[Result]);
end;

function TWebIDLScanner.ReadComment : TIDLString;

Var
  TokenStart : PChar;
  SectionLength : Integer;
  EOC,IsStar : Boolean;
  S : TIDLString;

begin
  Result:='';
  TokenStart:=TokenStr;
  Inc(TokenStr);
  Case Tokenstr[0] of
    '/' : begin
          SectionLength := Length(FCurLine)- (TokenStr - PChar(FCurLine));
          Inc(TokenStr);
          SetString(Result, TokenStr, SectionLength);
          Fetchline;
          end;
    '*' :
      begin
      IsStar:=False;
      Inc(TokenStr);
      TokenStart:=TokenStr;
      Repeat
        if (TokenStr[0]=#0) then
          begin
          SectionLength := (TokenStr - TokenStart);
          S:='';
          SetString(S, TokenStart, SectionLength);
          Result:=Result+S;
          if not fetchLine then
            Error(SUnterminatedComment, [CurRow,CurCOlumn,TokenStr[0]]);
          TokenStart:=TokenStr;
          end;
        IsStar:=TokenStr[0]='*';
        Inc(TokenStr);
        EOC:=(isStar and (TokenStr[0]='/'));
      Until EOC;
      if EOC then
        begin
        SectionLength := (TokenStr - TokenStart-1);
        S:='';
        SetString(S, TokenStart, SectionLength);
        Result:=Result+S;
        Inc(TokenStr);
        end;
      end;
  else
    Error(SErrInvalidCharacter, [CurRow,CurCOlumn,TokenStr[0]]);
  end;
end;

function TWebIDLScanner.DetermineToken : TIDLToken;

begin
  Result:=High(TIDLToken);
  While (Result<>tkIdentifier) and (TokenInfos[result]<>FCurTokenString) do
    Result:=Pred(Result);
  if Result in VersionNonTokens[Version] then
    Result:=tkIdentifier;
//  if Result=tkIdentifier then
//    Error(SErrExpectedTokenButWasIdentifier,[FCurTokenString]);
end;

function TWebIDLScanner.DetermineToken2 : TIDLToken;

Const
  InfTokens = [tkNan,tkInfinity,tkNegInfinity,tkByteString,tkUSVString,tkDOMString,tkPromise,tkFrozenArray];

begin
  For Result in InfTokens do
    if (TokenInfos[result]=FCurTokenString) then exit;
  Result:=tkIdentifier;
end;

function TWebIDLScanner.DoFetchToken: TIDLToken;

  Procedure SetSingleToken(tk : TIDLToken);

  begin
    FCurTokenString:=TokenStr[0];
    Inc(TokenStr);
    Result :=tk;
  end;

begin
  repeat
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
        if not FetchLine then
          Result:=tkEOF
        else
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
      '"':
        begin
          FCurTokenString:=ReadString;
          Result := tkString;
        end;
      ',':
        begin
          Inc(TokenStr);
          Result := tkComma;
        end;
      '0'..'9','-':
        begin
        Result := ReadNumber(FCurTokenString);
        end;
      ':': SetSingleToken(tkColon);
      '(': SetSingleToken(tkBracketOpen);
      ')': SetSingleToken(tkBracketClose);
      '{': SetSingleToken(tkCurlyBraceOpen);
      '}': SetSingleToken(tkCurlyBraceClose);
      '[': SetSingleToken(tkSquaredBraceOpen);
      ']': SetSingleToken(tkSquaredBraceClose);
      '<': SetSingleToken(tkLess);
      '=': SetSingleToken(tkEqual);
      '>': SetSingleToken(tkLarger);
      '?' : SetSingleToken(tkQuestionmark);
      ';' : SetSingleToken(tkSemicolon);
      '*' : SetSingleToken(tkStar);
      '.' :
         begin
         inc(TokenStr);
         if TokenStr[0]<>'.' then
           begin
           Dec(Tokenstr);// Setsingletoken advances
           SetSingleToken(tkDot);
           end
         else
           begin
           inc(TokenStr);
           if TokenStr[0]<>'.' then
             Error(SErrInvalidEllipsis);
           inc(TokenStr);
           FCurTokenString:='...';
           Result:=tkEllipsis;
           end;
         end;
      '/' :
        begin
        FCurTokenString:=ReadComment;
        Result:=tkComment;
        end;
      'a'..'z':
        begin
        FCurTokenString:=ReadIdent;
        Result:=DetermineToken;
        end;
      'A'..'Z','_':
        begin
        FCurTokenString:=ReadIdent;
        Result:=DetermineToken2;
        end;
      '#':
        begin
        Result:=tkComment;
        HandleDirective;
        end;
    else
      Error(SErrInvalidCharacter, [CurRow,CurColumn,TokenStr[0]]);
    end;
  until not FIsSkipping;

  FCurToken := Result;
end;

procedure TWebIDLScanner.HandleDirective;
var
  p: PChar;
  aDirective: TIDLString;
begin
  inc(TokenStr);
  p:=TokenStr;
  while TokenStr^ in ['a'..'z','A'..'Z','_','0'..'9'] do inc(TokenStr);
  SetString(aDirective, p, TokenStr-p);
  SkipWhitespace;
  case lowercase(aDirective) of
  'ifdef': HandleIfDef;
  'ifndef': HandleIfNDef;
  'if': HandleIf;
  'else': HandleElse;
  'endif': HandleEndIf;
  else
    Error(SErrUnknownDirectiveX, [CurRow,CurColumn,aDirective]);
  end;
  SkipWhitespace;
  SkipLineBreak;
end;

procedure TWebIDLScanner.HandleIfDef;
var
  StartP: PChar;
  aName: TIDLString;
begin
  PushSkipMode;
  if FIsSkipping then
    FSkipMode := wisSkipAll
  else
    begin
    StartP:=TokenStr;
    while TokenStr^ in ['a'..'z','A'..'Z','0'..'9','_'] do
      inc(TokenStr);
    SetString(aName,StartP,TokenStr-StartP);
    if IsDefined(aName) then
      FSkipMode := wisSkipElseBranch
    else
      begin
      FSkipMode := wisSkipIfBranch;
      FIsSkipping := true;
      end;
    //If LogEvent(sleConditionals) then
    //  if FSkipMode=wisSkipElseBranch then
    //    DoLog(mtInfo,nLogIFDefAccepted,sLogIFDefAccepted,[aName])
    //  else
    //    DoLog(mtInfo,nLogIFDefRejected,sLogIFDefRejected,[aName]);
    end;
end;

procedure TWebIDLScanner.HandleIfNDef;
var
  StartP: PChar;
  aName: TIDLString;
begin
  PushSkipMode;
  if FIsSkipping then
    FSkipMode := wisSkipAll
  else
    begin
    StartP:=TokenStr;
    while TokenStr^ in ['a'..'z','A'..'Z','0'..'9','_'] do
      inc(TokenStr);
    SetString(aName,StartP,TokenStr-StartP);
    if not IsDefined(aName) then
      FSkipMode := wisSkipElseBranch
    else
      begin
      FSkipMode := wisSkipIfBranch;
      FIsSkipping := true;
      end;
    //If LogEvent(sleConditionals) then
    //  if FSkipMode=wisSkipElseBranch then
    //    DoLog(mtInfo,nLogIFDefAccepted,sLogIFDefAccepted,[aName])
    //  else
    //    DoLog(mtInfo,nLogIFDefRejected,sLogIFDefRejected,[aName]);
    end;
end;

procedure TWebIDLScanner.HandleIf;
var
  StartP: PChar;
begin
  PushSkipMode;
  if FIsSkipping then
    FSkipMode := wisSkipAll
  else
    begin
    StartP:=TokenStr;
    while not (TokenStr^ in [#0,#10,#13]) do
      inc(TokenStr);
    if Evaluator.Eval(StartP,CurRow) then
      FSkipMode := wisSkipElseBranch
    else
      begin
      FSkipMode := wisSkipIfBranch;
      FIsSkipping := true;
      end;
    //If LogEvent(sleConditionals) then
    //  if FSkipMode=FSkipElseBranch then
    //    DoLog(mtInfo,nLogIFAccepted,sLogIFAccepted,[AParam])
    //  else
    //    DoLog(mtInfo,nLogIFRejected,sLogIFRejected,[AParam]);
    end;
end;

procedure TWebIDLScanner.HandleElse;
begin
  if FSkipStackIndex = 0 then
    Error('Invalid #Else');
  if FSkipMode = wisSkipIfBranch then
    FIsSkipping := false
  else if FSkipMode = wisSkipElseBranch then
    FIsSkipping := true;
end;

procedure TWebIDLScanner.HandleEndIf;
begin
  if FSkipStackIndex = 0 then
    Error('Invalid #EndIf');
  Dec(FSkipStackIndex);
  FSkipMode := FSkipModeStack[FSkipStackIndex];
  FIsSkipping := FIsSkippingStack[FSkipStackIndex];
end;

procedure TWebIDLScanner.PushSkipMode;
begin
  if FSkipStackIndex = High(FSkipModeStack) then
    Error('Nesting of #IFxxx too deep');
  FSkipModeStack[FSkipStackIndex] := FSkipMode;
  FIsSkippingStack[FSkipStackIndex] := FIsSkipping;
  Inc(FSkipStackIndex);
end;

function TWebIDLScanner.IsDefined(const aName: TIDLString): boolean;
begin
  Result:=false;
  if aName='' then ;
end;

procedure TWebIDLScanner.SkipWhitespace;
begin
  while TokenStr^ in [' ',#9] do
    inc(TokenStr);
end;

procedure TWebIDLScanner.SkipLineBreak;
begin
  case TokenStr^ of
  #10: inc(TokenStr);
  #13:
    begin
    inc(TokenStr);
    if TokenStr^=#10 then
      inc(TokenStr);
    end;
  end;
end;

procedure TWebIDLScanner.Init;
begin
  FSource:=TStringList.Create;
  FEvaluator:=TDirectiveEvaluator.Create;
  FEvaluator.OnLog:=@OnEvalLog;
  FEvaluator.OnEvalVariable:=@OnEvalVar;
  FEvaluator.OnEvalFunction:=@OnEvalFunction;
end;

function TWebIDLScanner.GetCurColumn: Integer;
begin
  Result := TokenStr - PChar(CurLine);
end;

function TWebIDLScanner.OnEvalFunction(Sender: TDirectiveEvaluator; Name,
  Param: TIDLString; out Value: TIDLString): boolean;
begin
  Result:=true;
  if Name='defined' then
    Value:=TDirectiveEvaluator.BoolValues[IsDefined(Param)]
  else
    Value:='';
end;

procedure TWebIDLScanner.OnEvalLog(Sender: TDirectiveEvaluator;
  Args: array of const);
var
  Msg: TIDLString;
begin
  if Sender.MsgType<=mtError then
    begin
    Msg:=Format(Sender.MsgPattern,Args);
    //SetCurMsg(Sender.MsgType,Sender.MsgNumber,Sender.MsgPattern,Args);
    //Msg:=Format('%s(%d,%d) : %s',[FormatPath(FCurFileName),CurRow,CurColumn,FLastMsg]);
    raise EWebIDLScanner.Create(Msg);
    end
  else
    ; //DoLog(Sender.MsgType,Sender.MsgNumber,Sender.MsgPattern,Args,true);
end;

function TWebIDLScanner.OnEvalVar(Sender: TDirectiveEvaluator; Name: TIDLString;
  out Value: TIDLString): boolean;
begin
  Result:=true;
  Value:='';
  if Name='' then ;
end;


end.

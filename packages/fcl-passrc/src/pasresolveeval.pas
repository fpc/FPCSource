unit PasResolveEval;

{$mode objfpc}{$H+}

{$IFOPT Q+}{$DEFINE OverflowCheckOn}{$ENDIF}

interface

uses
  PasTree, PScanner, sysutils;

// message numbers
const
  nIdentifierNotFound = 3001;
  nNotYetImplemented = 3002;
  nIllegalQualifier = 3003;
  nSyntaxErrorExpectedButFound = 3004;
  nWrongNumberOfParametersForCallTo = 3005;
  nIncompatibleTypeArgNo = 3006;
  nIncompatibleTypeArgNoVarParamMustMatchExactly = 3007;
  nVariableIdentifierExpected = 3008;
  nDuplicateIdentifier = 3009;
  nXExpectedButYFound = 3010;
  nAncestorCycleDetected = 3011;
  nCantUseForwardDeclarationAsAncestor = 3012;
  nCantDetermineWhichOverloadedFunctionToCall = 3013;
  nForwardTypeNotResolved = 3014;
  nForwardProcNotResolved = 3015;
  nInvalidXModifierY = 3016;
  nAbstractMethodsMustNotHaveImplementation = 3017;
  nCallingConventionMismatch = 3018;
  nResultTypeMismatchExpectedButFound = 3019;
  nFunctionHeaderMismatchForwardVarName = 3020;
  nFunctionHidesIdentifier = 3021;
  nNoMethodInAncestorToOverride = 3022;
  nInheritedOnlyWorksInMethods = 3023;
  nInheritedNeedsAncestor = 3024;
  nNoPropertyFoundToOverride = 3025;
  nExprTypeMustBeClassOrRecordTypeGot = 3026;
  nPropertyNotWritable = 3027;
  nIncompatibleTypesGotExpected = 3028;
  nTypesAreNotRelated = 3029;
  nAbstractMethodsCannotBeCalledDirectly = 3030;
  nMissingParameterX = 3031;
  nCannotAccessThisMemberFromAX = 3032;
  nInOperatorExpectsSetElementButGot = 3033;
  nWrongNumberOfParametersForTypeCast = 3034;
  nIllegalTypeConversionTo = 3035;
  nConstantExpressionExpected = 3036;
  nLeftSideOfIsOperatorExpectsAClassButGot = 3037;
  nNotReadable = 3038;
  nClassPropertyAccessorMustBeStatic = 3039;
  nClassPropertyAccessorMustNotBeStatic = 3040;
  nOnlyOneDefaultPropertyIsAllowed = 3041;
  nWrongNumberOfParametersForArray = 3042;
  nCantAssignValuesToAnAddress = 3043;
  nIllegalExpression = 3044;
  nCantAccessPrivateMember = 3045;
  nMustBeInsideALoop = 3046;
  nExpectXArrayElementsButFoundY = 3047;
  nCannotCreateADescendantOfTheSealedClass = 3048;
  nAncestorIsNotExternal = 3049;
  nVirtualMethodXHasLowerVisibility = 3050; // FPC 3250
  nExternalClassInstanceCannotAccessStaticX = 3051;
  nXModifierMismatchY = 3052;
  nSymbolCannotBePublished = 3053;
  nCannotTypecastAType = 3054;
  nTypeIdentifierExpected = 3055;
  nCannotNestAnonymousX = 3056;
  nFoundCallCandidateX = 3057;
  nSymbolXIsNotPortable = 3058;
  nSymbolXIsExperimental = 3059;
  nSymbolXIsNotImplemented = 3060;
  nSymbolXBelongsToALibrary = 3061;
  nSymbolXIsDeprecated = 3062;
  nSymbolXIsDeprecatedY = 3063;
  nRangeCheckError = 3064;
  nHighRangeLimitLTLowRangeLimit = 3065;
  nRangeCheckEvaluatingConstantsVMinMax = 3066;
  nIllegalChar = 3067;
  nOverflowInArithmeticOperation = 3068;

// resourcestring patterns of messages
resourcestring
  sIdentifierNotFound = 'identifier not found "%s"';
  sNotYetImplemented = 'not yet implemented: %s';
  sIllegalQualifier = 'illegal qualifier "%s"';
  sSyntaxErrorExpectedButFound = 'Syntax error, "%s" expected but "%s" found';
  sWrongNumberOfParametersForCallTo = 'Wrong number of parameters specified for call to "%s"';
  sIncompatibleTypeArgNo = 'Incompatible type arg no. %s: Got "%s", expected "%s"';
  sIncompatibleTypeArgNoVarParamMustMatchExactly = 'Incompatible type arg no. %s: Got "%s", expected "%s". Var param must match exactly.';
  sVariableIdentifierExpected = 'Variable identifier expected';
  sDuplicateIdentifier = 'Duplicate identifier "%s" at %s';
  sXExpectedButYFound = '%s expected, but %s found';
  sAncestorCycleDetected = 'Ancestor cycle detected';
  sCantUseForwardDeclarationAsAncestor = 'Can''t use forward declaration "%s" as ancestor';
  sCantDetermineWhichOverloadedFunctionToCall = 'Can''t determine which overloaded function to call';
  sForwardTypeNotResolved = 'Forward type not resolved "%s"';
  sForwardProcNotResolved = 'Forward %s not resolved "%s"';
  sInvalidXModifierY = 'Invalid %s modifier %s';
  sAbstractMethodsMustNotHaveImplementation = 'Abstract method must not have an implementation.';
  sCallingConventionMismatch = 'Calling convention mismatch';
  sResultTypeMismatchExpectedButFound = 'Result type mismatch, expected %s, but found %s';
  sFunctionHeaderMismatchForwardVarName = 'function header "%s" doesn''t match forward : var name changes %s => %s';
  sFunctionHidesIdentifier = 'function hides identifier "%s" at "%s"';
  sNoMethodInAncestorToOverride = 'There is no method in an ancestor class to be overridden "%s"';
  sInheritedOnlyWorksInMethods = 'Inherited works only in methods';
  sInheritedNeedsAncestor = 'inherited needs an ancestor';
  sNoPropertyFoundToOverride = 'No property found to override';
  sExprTypeMustBeClassOrRecordTypeGot = 'Expression type must be class or record type, got %s';
  sPropertyNotWritable = 'No member is provided to access property';
  sIncompatibleTypesGotExpected = 'Incompatible types: got "%s" expected "%s"';
  sTypesAreNotRelated = 'Types are not related';
  sAbstractMethodsCannotBeCalledDirectly = 'Abstract methods cannot be called directly';
  sMissingParameterX = 'Missing parameter %s';
  sCannotAccessThisMemberFromAX = 'Cannot access this member from a %s';
  sInOperatorExpectsSetElementButGot = 'the in-operator expects a set element, but got %s';
  sWrongNumberOfParametersForTypeCast = 'wrong number of parameters for type cast to %s';
  sIllegalTypeConversionTo = 'Illegal type conversion: "%s" to "%s"';
  sConstantExpressionExpected = 'Constant expression expected';
  sLeftSideOfIsOperatorExpectsAClassButGot = 'left side of is-operator expects a class, but got %s';
  sNotReadable = 'not readable';
  sClassPropertyAccessorMustBeStatic = 'class property accessor must be static';
  sClassPropertyAccessorMustNotBeStatic = 'class property accessor must not be static';
  sOnlyOneDefaultPropertyIsAllowed = 'Only one default property is allowed';
  sWrongNumberOfParametersForArray = 'Wrong number of parameters for array';
  sCantAssignValuesToAnAddress = 'Can''t assign values to an address';
  sIllegalExpression = 'Illegal expression';
  sCantAccessPrivateMember = 'Can''t access %s member %s';
  sMustBeInsideALoop = '%s must be inside a loop';
  sExpectXArrayElementsButFoundY = 'Expect %s array elements, but found %s';
  sCannotCreateADescendantOfTheSealedClass = 'Cannot create a descendant of the sealed class "%s"';
  sAncestorIsNotExternal = 'Ancestor "%s" is not external';
  sVirtualMethodXHasLowerVisibility = 'Virtual method "%s" has a lower visibility (%s) than parent class %s (%s)';
  sExternalClassInstanceCannotAccessStaticX = 'External class instance cannot access static %s';
  sXModifierMismatchY = '%s modifier "%s" mismatch';
  sSymbolCannotBePublished = 'Symbol cannot be published';
  sCannotTypecastAType = 'Cannot type cast a type';
  sTypeIdentifierExpected = 'Type identifier expected';
  sCannotNestAnonymousX = 'Cannot nest anonymous %s';
  sFoundCallCandidateX = 'Found call candidate %s';
  sSymbolXIsNotPortable = 'Symbol "%s" is not portable';
  sSymbolXIsExperimental = 'Symbol "%s" is experimental';
  sSymbolXIsNotImplemented = 'Symbol "%s" is implemented';
  sSymbolXBelongsToALibrary = 'Symbol "%s" belongs to a library';
  sSymbolXIsDeprecated = 'Symbol "%s" is deprecated';
  sSymbolXIsDeprecatedY = 'Symbol "%s" is deprecated: %s';
  sRangeCheckError = 'Range check error';
  sHighRangeLimitLTLowRangeLimit = 'High range limit < low range limit';
  sRangeCheckEvaluatingConstantsVMinMax = 'range check error while evaluating constants (%s must be between %s and %s)';
  sIllegalChar = 'Illegal character';
  sOverflowInArithmeticOperation = 'Overflow in arithmetic operation';

type
  { TResolveData - base class for data stored in TPasElement.CustomData }

  TResolveData = Class(TPasElementBase)
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    Owner: TObject; // e.g. a TPasResolver
    Next: TResolveData; // TPasResolver uses this for its memory chain
    constructor Create; virtual;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;// Element.CustomData=Self
  end;
  TResolveDataClass = class of TResolveData;

type
  { TResEvalValue }

  TREVKind = (
    revkNone,
    revkCustom,
    revkNil,  // TResEvalValue
    revkBool, // TResEvalInt
    revkInt,  // TResEvalInt
    revkUInt, // TResEvalUInt
    revkFloat, // TResEvalFloat
    revkString, // TResEvalString
    revkUnicodeString, // TResEvalUTF16
    revkEnum,     // TResEvalEnum
    revkRangeInt, // range of enum, int, char, widechar, e.g. 1..2
    revkRangeUInt, // range of uint, e.g. 1..2
    revkSetEmpty, // []
    revkSetOfInt  // set of enum, int, char, widechar, e.g. [1,2..3]
    );
  TResEvalValue = class(TResolveData)
  public
    Kind: TREVKind;
    IdentEl: TPasElement;
    function Clone: TResEvalValue; virtual;
    function AsDebugString: string; virtual;
    function AsString: string; virtual;
  end;
  TResEvalValueClass = class of TResEvalValue;

  { TResEvalInt }

  TResEvalInt = class(TResEvalValue)
  public
    Int: NativeInt;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
  end;

  { TResEvalUInt }

  TResEvalUInt = class(TResEvalValue)
  public
    UInt: NativeUInt;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
  end;

  { TResEvalFloat }

  TResEvalFloat = class(TResEvalValue)
  public
    FloatValue: extended;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
  end;

  { TResEvalString - Kind=revkString }

  TResEvalString = class(TResEvalValue)
  public
    S: RawByteString;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
  end;

  { TResEvalUTF16 - Kind=revkUnicodeString }

  TResEvalUTF16 = class(TResEvalValue)
  public
    S: UnicodeString;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
  end;

  { TResEvalEnum - Kind=revkEnum, Value.Int, IdentEl is TPasEnumValue }

  TResEvalEnum = class(TResEvalValue)
  public
    Index: integer;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsDebugString: string; override;
    function AsString: string; override;
  end;

  { TResEvalRangeInt - Kind=revkRangeInt }

  TResEvalRangeInt = class(TResEvalValue)
  public
    type
      TRgIntElKind = (
        revrikBool,
        revrikEnum, // IdentEl is TPasEnumType
        revrikInt,
        revrikChar
        );
  public
    ElKind: TRgIntElKind;
    RangeStart, RangeEnd: int64;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
    function ElementAsString(El: int64): string;
  end;

  { TResEvalRangeUInt }

  TResEvalRangeUInt = class(TResEvalValue)
  public
    RangeStart, RangeEnd: qword;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
  end;

  { TResEvalSetInt - Kind=revkASet }

  TResEvalSetInt = class(TResEvalValue)
  public
    type
      TSetElKind = (
        revsikEnum, // IdentEl is TPasEnumType
        revsikInt,
        revsikChar,
        revsikWChar
        );
      TItem = record
        RangeStart, RangeEnd: int64;
      end;
      TItems = array of TItem;
  public
    ElKind: TSetElKind;
    Ranges: TItems;
    constructor Create; override;
    function Clone: TResEvalValue; override;
    function AsString: string; override;
    function ElementAsString(El: int64): string;
  end;

  TResEvalFlag = (
    refConst, // computing a const, error if a value is not const
    refAutoConst, // set refConst if in a const
    refSet  // computing a set, allow ranges
    );
  TResEvalFlags = set of TResEvalFlag;

  TResExprEvaluator = class;

  TPasResEvalLogHandler = procedure(Sender: TResExprEvaluator; const id: int64;
    MsgType: TMessageType; MsgNumber: integer;
    const Fmt: String; Args: Array of const; PosEl: TPasElement) of object;
  TPasResEvalIdentHandler = function(Sender: TResExprEvaluator;
    Expr: TPrimitiveExpr; Flags: TResEvalFlags): TResEvalValue of object;

  { TResExprEvaluator }

  TResExprEvaluator = class
  private
    FOnEvalIdentifier: TPasResEvalIdentHandler;
    FOnLog: TPasResEvalLogHandler;
  protected
    procedure LogMsg(const id: int64; MsgType: TMessageType; MsgNumber: integer;
      const Fmt: String; Args: Array of const; PosEl: TPasElement); overload;
    procedure RaiseMsg(const Id: int64; MsgNumber: integer; const Fmt: String;
      Args: Array of const; ErrorPosEl: TPasElement);
    procedure RaiseNotYetImplemented(id: int64; El: TPasElement; Msg: string = ''); virtual;
    procedure RaiseInternalError(id: int64; const Msg: string = '');
    procedure RaiseConstantExprExp(id: int64; ErrorEl: TPasElement);
    procedure RaiseRangeCheck(id: int64; ErrorEl: TPasElement);
    procedure RaiseOverflowArithmetic(id: int64; ErrorEl: TPasElement);
    function EvalUnaryExpr(Expr: TUnaryExpr; Flags: TResEvalFlags): TResEvalValue;
    function EvalBinaryExpr(Expr: TBinaryExpr; Flags: TResEvalFlags): TResEvalValue;
    function EvalArrayParams(Expr: TParamsExpr; Flags: TResEvalFlags): TResEvalValue;
    function EvalFuncParams(Expr: TParamsExpr; Flags: TResEvalFlags): TResEvalValue;
    function EvalSetParams(Expr: TParamsExpr; Flags: TResEvalFlags): TResEvalValue;
    function ExprStringToOrd(Value: TResEvalValue; PosEl: TPasElement): longword; virtual;
    function EvalPrimitiveExprString(Expr: TPrimitiveExpr): TResEvalValue; virtual;
  public
    function Eval(Expr: TPasExpr; Flags: TResEvalFlags): TResEvalValue;
    function IsInRange(Expr, RangeExpr: TPasExpr; EmitHints: boolean): boolean;
    function IsConst(Expr: TPasExpr): boolean;
    function IsSimpleExpr(Expr: TPasExpr): boolean; // true = no need to store result
    procedure EmitRangeCheckConst(id: int64; const aValue, MinVal, MaxVal: String;
      PosEl: TPasElement); virtual;
    procedure EmitRangeCheckConst(id: int64; const aValue: String;
      MinVal, MaxVal: int64; PosEl: TPasElement);
    property OnLog: TPasResEvalLogHandler read FOnLog write FOnLog;
    property OnEvalIdentifier: TPasResEvalIdentHandler read FOnEvalIdentifier write FOnEvalIdentifier;
  end;

procedure ReleaseEvalValue(var Value: TResEvalValue);

function RawStrToCaption(const r: RawByteString; MaxLength: integer): string;
function UnicodeStrToCaption(const u: UnicodeString; MaxLength: integer): Unicodestring;
function CanBeConvertedToUTF16(const s: String): integer;
function CodePointToString(CodePoint: longword): String;
function CodePointToUnicodeString(u: longword): UnicodeString;

function GetObjName(o: TObject): string;
function dbgs(const Flags: TResEvalFlags): string; overload;
function dbgs(v: TResEvalValue): string; overload;

implementation

procedure ReleaseEvalValue(var Value: TResEvalValue);
begin
  if Value=nil then exit;
  if Value.Element<>nil then exit;
  Value.Free;
  Value:=nil;
end;

function RawStrToCaption(const r: RawByteString; MaxLength: integer): string;
var
  s: RawByteString;
  p: PAnsiChar;
  InLit: boolean;
  Len: integer;

  procedure AddHash(o: integer);
  var
    h: String;
  begin
    if (Result<>'') and InLit then
      begin
      Result:=Result+'''';
      inc(Len);
      InLit:=false;
      end;
    h:='#'+IntToStr(o);
    inc(Len,length(h));
    if Len<=MaxLength then
      Result:=Result+h;
  end;

  procedure AddLit(const Lit: string; CaptionLen: integer);
  begin
    if not InLit then
      begin
      Result:=Result+'''';
      inc(Len);
      InLit:=true;
      end;
    Result:=Result+Lit;
    inc(Len,CaptionLen);
  end;

var
  l: SizeInt;
  CP: TSystemCodePage;
  EndP: PAnsiChar;
begin
  Result:='';
  s:=r;
  CP:=StringCodePage(s);
  if (CP<>CP_ACP) and (CP<>CP_UTF8) then
    SetCodePage(s, CP_ACP, true);
  p:=PAnsiChar(s);
  EndP:=p+length(s);
  Len:=0;
  InLit:=false;
  while Len<MaxLength do
    case p^ of
    #0:
      begin
      if p-PAnsiChar(s)=length(s) then
        break;
      AddHash(0);
      inc(p);
      end;
    '''':
      begin
      AddLit('''''',2);
      inc(p);
      end;
    #1..#31,#127..#192:
      begin
      AddHash(ord(p^));
      inc(p);
      end
    else
      begin
      l:=Utf8CodePointLen(p,EndP-p,true);
      if l<=0 then
        begin
        // invalid
        AddHash(ord(p^));
        inc(p);
        end
      else
        begin
        AddLit(copy(s,p-PAnsiChar(s)+1,l),1);
        inc(p,l);
        end;
      end;
    end;
  if InLit then
    Result:=Result+'''';
end;

function UnicodeStrToCaption(const u: UnicodeString; MaxLength: integer
  ): Unicodestring;
var
  p: PWideChar;
  InLit: boolean;
  Len: integer;

  procedure AddHash(o: integer);
  var
    h: UnicodeString;
  begin
    if (Result<>'') and InLit then
      begin
      Result:=Result+'''';
      inc(Len);
      InLit:=false;
      end;
    h:='#'+UnicodeString(IntToStr(o));
    inc(Len,length(h));
    if Len<=MaxLength then
      Result:=Result+h;
  end;

  procedure AddLit(const Lit: Unicodestring; CaptionLen: integer);
  begin
    if not InLit then
      begin
      Result:=Result+'''';
      inc(Len);
      InLit:=true;
      end;
    Result:=Result+Lit;
    inc(Len,CaptionLen);
  end;

begin
  Result:='';
  p:=PWideChar(u);
  Len:=0;
  InLit:=false;
  while Len<MaxLength do
    case p^ of
    #0:
      begin
      if p-PWideChar(u)=length(u) then
        break;
      AddHash(0);
      inc(p);
      end;
    '''':
      begin
      AddLit('''''',2);
      inc(p);
      end;
    #1..#31,#127..#255,#$D800..#$ffff:
      begin
      AddHash(ord(p^));
      inc(p);
      end
    else
      begin
      AddLit(p^,1);
      inc(p);
      end;
    end;
  if InLit then
    Result:=Result+'''';
end;

function CanBeConvertedToUTF16(const s: String): integer;
var
  p, EndP: PAnsiChar;
  cp: TSystemCodePage;
  l: SizeInt;
begin
  if s='' then exit(0);
  cp:=StringCodePage(s);
  if (cp<>CP_UTF8) and ((cp<>CP_ACP) or (DefaultSystemCodePage<>CP_UTF8)) then
    begin
    // need conversion -> not yet supported
    exit(1);
    end;
  p:=PChar(s);
  EndP:=p+length(s);
  while p<EndP do
    begin
    l:=Utf8CodePointLen(p,EndP-p,false);
    if l<=0 then
      exit(p-PAnsiChar(s)+1);
    inc(p,l);
    end;
end;

function CodePointToString(CodePoint: longword): String;
begin
  case CodePoint of
    0..$7f:
      begin
        Result:=char(byte(CodePoint));
      end;
    $80..$7ff:
      begin
        Result:=char(byte($c0 or (CodePoint shr 6)))
               +char(byte($80 or (CodePoint and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=char(byte($e0 or (CodePoint shr 12)))
               +char(byte((CodePoint shr 6) and $3f) or $80)
               +char(byte(CodePoint and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=char(byte($f0 or (CodePoint shr 18)))
               +char(byte((CodePoint shr 12) and $3f) or $80)
               +char(byte((CodePoint shr 6) and $3f) or $80)
               +char(byte(CodePoint and $3f) or $80);
      end;
  else
    Result:='';
  end;
end;

function CodePointToUnicodeString(u: longword): UnicodeString;
begin
  if u < $10000 then
    // Note: codepoints $D800 - $DFFF are reserved
    Result:=WideChar(u)
  else
    Result:=WideChar($D800+((u - $10000) shr 10))+WideChar($DC00+((u - $10000) and $3ff));
end;

function GetObjName(o: TObject): string;
begin
  if o=nil then
    Result:='nil'
  else if o is TPasElement then
    Result:=TPasElement(o).Name+':'+o.ClassName
  else
    Result:=o.ClassName;
end;

function dbgs(const Flags: TResEvalFlags): string;
var
  s: string;
  f: TResEvalFlag;
begin
  Result:='';
  for f in Flags do
    if f in Flags then
      begin
      if Result<>'' then Result:=Result+',';
      str(f,s);
      Result:=Result+s;
      end;
  Result:='['+Result+']';
end;

function dbgs(v: TResEvalValue): string;
begin
  if v=nil then
    Result:='nil'
  else
    Result:=v.AsDebugString;
end;

{ TResEvalRangeUInt }

constructor TResEvalRangeUInt.Create;
begin
  inherited Create;
  Kind:=revkRangeInt;
end;

function TResEvalRangeUInt.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalRangeUInt(Result).RangeStart:=RangeStart;
  TResEvalRangeUInt(Result).RangeEnd:=RangeEnd;
end;

function TResEvalRangeUInt.AsString: string;
begin
  Result:=IntToStr(RangeStart)+'..'+IntToStr(RangeEnd);
end;

{ TResExprEvaluator }

procedure TResExprEvaluator.LogMsg(const id: int64; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String; Args: array of const;
  PosEl: TPasElement);
begin
  OnLog(Self,id,MsgType,MsgNumber,Fmt,Args,PosEl);
end;

procedure TResExprEvaluator.RaiseMsg(const Id: int64; MsgNumber: integer;
  const Fmt: String; Args: array of const; ErrorPosEl: TPasElement);
begin
  LogMsg(id,mtError,MsgNumber,Fmt,Args,ErrorPosEl);
  raise Exception.Create('['+IntToStr(id)+'] ('+IntToStr(MsgNumber)+') '+SafeFormat(Fmt,Args));
end;

procedure TResExprEvaluator.RaiseNotYetImplemented(id: int64; El: TPasElement;
  Msg: string);
var
  s: String;
begin
  s:=sNotYetImplemented+' ['+IntToStr(id)+']';
  if Msg<>'' then
    s:=s+' '+Msg;
  {$IFDEF VerbosePasResolver}
  writeln('TResExprEvaluator.RaiseNotYetImplemented s="',s,'" El=',GetObjName(El));
  {$ENDIF}
  RaiseMsg(id,nNotYetImplemented,s,[GetObjName(El)],El);
end;

procedure TResExprEvaluator.RaiseInternalError(id: int64; const Msg: string);
begin
  raise Exception.Create('Internal error: ['+IntToStr(id)+'] '+Msg);
end;

procedure TResExprEvaluator.RaiseConstantExprExp(id: int64; ErrorEl: TPasElement
  );
begin
  RaiseMsg(id,nConstantExpressionExpected,sConstantExpressionExpected,[],ErrorEl);
end;

procedure TResExprEvaluator.RaiseRangeCheck(id: int64; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nRangeCheckError,sRangeCheckError,[],ErrorEl);
end;

procedure TResExprEvaluator.RaiseOverflowArithmetic(id: int64;
  ErrorEl: TPasElement);
begin
  RaiseMsg(id,nOverflowInArithmeticOperation,sOverflowInArithmeticOperation,[],ErrorEl);
end;

function TResExprEvaluator.EvalUnaryExpr(Expr: TUnaryExpr; Flags: TResEvalFlags
  ): TResEvalValue;
begin
  Result:=Eval(Expr.Operand,Flags);
  if Result=nil then exit;
  case Expr.OpCode of
    eopAdd: ;
    eopSubtract:
      case Result.Kind of
      revkInt:
        begin
        if TResEvalInt(Result).Int=0 then exit;
        if Result.Element<>nil then
          Result:=Result.Clone;
        TResEvalInt(Result).Int:=-TResEvalInt(Result).Int;
        end;
      revkUInt:
        begin
        if TResEvalUInt(Result).UInt=0 then exit;
        if Result.Element<>nil then
          Result:=Result.Clone;
        TResEvalUInt(Result).UInt:=-TResEvalUInt(Result).UInt;
        end
      else
        begin
        if Result.Element=nil then
          Result.Free;
        RaiseNotYetImplemented(20170518230738,Expr);
        end;
      end;
    eopNot:
      case Result.Kind of
      revkBool:
        begin
        if Result.Element<>nil then
          Result:=Result.Clone;
        if TResEvalInt(Result).Int=0 then
          TResEvalInt(Result).Int:=1
        else
          TResEvalInt(Result).Int:=0;
        end
      else
        begin
        if Result.Element=nil then
          Result.Free;
        RaiseNotYetImplemented(20170518232804,Expr);
        end;
      end;
    eopAddress:
      begin
      if Result.Element=nil then
        Result.Free;
      // @ operator requires a compiler -> return nil
      Result:=TResEvalValue.Create;
      Result.Kind:=revkNil;
      end
    else
      RaiseNotYetImplemented(20170518232823,Expr);
    end;
end;

function TResExprEvaluator.EvalBinaryExpr(Expr: TBinaryExpr;
  Flags: TResEvalFlags): TResEvalValue;
var
  LeftValue, RightValue: TResEvalValue;
  LeftInt, RightInt: LongWord;
  Int: NativeInt;
  UInt: NativeUInt;
begin
  Result:=nil;
  LeftValue:=nil;
  RightValue:=nil;
  try
    LeftValue:=Eval(Expr.left,Flags);
    if LeftValue=nil then exit;
    RightValue:=Eval(Expr.right,Flags);
    if RightValue=nil then exit;
    case Expr.Kind of
    pekRange:
      // leftvalue..rightvalue
      case LeftValue.Kind of
      revkInt:
        if RightValue.Kind=revkInt then
          begin
          if TResEvalInt(LeftValue).Int>TResEvalInt(RightValue).Int then
            RaiseMsg(20170518222939,nHighRangeLimitLTLowRangeLimit,
              sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
          Result:=TResEvalRangeInt.Create;
          TResEvalRangeInt(Result).ElKind:=revrikInt;
          TResEvalRangeInt(Result).RangeStart:=TResEvalInt(LeftValue).Int;
          TResEvalRangeInt(Result).RangeEnd:=TResEvalInt(RightValue).Int;
          exit;
          end
        else if RightValue.Kind=revkUInt then
          begin
          // Note: when FPC compares int64 with qword it converts the qword to an int64
          if TResEvalUInt(RightValue).UInt<=NativeUInt(High(NativeInt)) then
            begin
            if TResEvalInt(LeftValue).Int>TResEvalUInt(RightValue).UInt then
              RaiseMsg(20170519000235,nHighRangeLimitLTLowRangeLimit,
                sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
            Result:=TResEvalRangeInt.Create;
            TResEvalRangeInt(Result).ElKind:=revrikInt;
            TResEvalRangeInt(Result).RangeStart:=TResEvalInt(LeftValue).Int;
            TResEvalRangeInt(Result).RangeEnd:=TResEvalUInt(RightValue).UInt;
            exit;
            end
          else if TResEvalInt(LeftValue).Int<0 then
            RaiseRangeCheck(20170522151629,Expr.Right)
          else if qword(TResEvalInt(LeftValue).Int)>TResEvalUInt(RightValue).UInt then
            RaiseMsg(20170522151708,nHighRangeLimitLTLowRangeLimit,
              sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
          Result:=TResEvalRangeUInt.Create;
          TResEvalRangeUInt(Result).RangeStart:=TResEvalInt(LeftValue).Int;
          TResEvalRangeUInt(Result).RangeEnd:=TResEvalUInt(RightValue).UInt;
          exit;
          end
        else
          RaiseRangeCheck(20170518222812,Expr.Right);
      revkUInt:
        if RightValue.Kind=revkInt then
          begin
          // Note: when FPC compares int64 with qword it converts the qword to an int64
          if TResEvalUInt(LeftValue).UInt>NativeUInt(High(NativeInt)) then
            begin
            if TResEvalInt(RightValue).Int<0 then
              RaiseRangeCheck(20170522152608,Expr.Right)
            else if TResEvalUInt(LeftValue).UInt>qword(TResEvalInt(RightValue).Int) then
              RaiseMsg(20170522152648,nHighRangeLimitLTLowRangeLimit,
                sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
            Result:=TResEvalRangeUInt.Create;
            TResEvalRangeUInt(Result).RangeStart:=TResEvalUInt(LeftValue).UInt;
            TResEvalRangeUInt(Result).RangeEnd:=TResEvalInt(RightValue).Int;
            exit;
            end
          else if TResEvalUInt(LeftValue).UInt>TResEvalInt(RightValue).Int then
            RaiseMsg(20170522152804,nHighRangeLimitLTLowRangeLimit,
              sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
          Result:=TResEvalRangeInt.Create;
          TResEvalRangeInt(Result).ElKind:=revrikInt;
          TResEvalRangeInt(Result).RangeStart:=TResEvalUInt(LeftValue).UInt;
          TResEvalRangeInt(Result).RangeEnd:=TResEvalInt(RightValue).Int;
          exit;
          end
        else if RightValue.Kind=revkUInt then
          begin
          if TResEvalUInt(LeftValue).UInt>TResEvalUInt(RightValue).UInt then
            RaiseMsg(20170519000240,nHighRangeLimitLTLowRangeLimit,
              sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
          Result:=TResEvalRangeUInt.Create;
          TResEvalRangeUInt(Result).RangeStart:=TResEvalUInt(LeftValue).UInt;
          TResEvalRangeUInt(Result).RangeEnd:=TResEvalUInt(RightValue).UInt;
          exit;
          end
        else
          RaiseRangeCheck(20170522123106,Expr.Right);
      revkEnum:
        if (RightValue.Kind<>revkEnum) then
          RaiseRangeCheck(20170522153003,Expr.Right)
        else if (TResEvalEnum(LeftValue).IdentEl<>TResEvalEnum(RightValue).IdentEl) then
          RaiseRangeCheck(20170522123241,Expr.Right)
        else if TResEvalEnum(LeftValue).Index>TResEvalEnum(RightValue).Index then
          RaiseMsg(20170522123320,nHighRangeLimitLTLowRangeLimit,
            sHighRangeLimitLTLowRangeLimit,[],Expr.Right)
        else
          begin
          Result:=TResEvalRangeInt.Create;
          TResEvalRangeInt(Result).ElKind:=revrikEnum;
          TResEvalRangeInt(Result).RangeStart:=TResEvalEnum(LeftValue).Index;
          TResEvalRangeInt(Result).RangeEnd:=TResEvalEnum(RightValue).Index;
          exit;
          end;
      revkString,revkUnicodeString:
        begin
        LeftInt:=ExprStringToOrd(LeftValue,Expr.left);
        if RightValue.Kind in [revkString,revkUnicodeString] then
          begin
          RightInt:=ExprStringToOrd(RightValue,Expr.right);
          if LeftInt>RightInt then
            RaiseMsg(20170523151508,nHighRangeLimitLTLowRangeLimit,
              sHighRangeLimitLTLowRangeLimit,[],Expr.Right);
          Result:=TResEvalRangeInt.Create;
          TResEvalRangeInt(Result).ElKind:=revrikChar;
          TResEvalRangeInt(Result).RangeStart:=LeftInt;
          TResEvalRangeInt(Result).RangeEnd:=RightInt;
          exit;
          end
        else
          RaiseRangeCheck(20170522123106,Expr.Right);
        end
      else
        {$IFDEF EnablePasResRangeCheck}
        writeln('TPasResolver.Eval pekRange Left=',GetObjName(Expr.Left),' LeftValue.Kind=',LeftValue.Kind);
        RaiseNotYetImplemented(20170518221103,Expr.Left);
        {$ELSE}
        exit(nil);
        {$ENDIF}
      end;
    pekBinary:
      case Expr.OpCode of
      eopAdd:
        case LeftValue.Kind of
        revkInt:
          if RightValue.Kind=revkInt then
            // int+int
            try
              {$Q+}
              Int:=TResEvalInt(LeftValue).Int+TResEvalInt(RightValue).Int;
              {$IFNDEF OverflowCheckOn}{$Q-}{$ENDIF}
              Result:=TResEvalInt.Create;
              TResEvalInt(Result).Int:=NativeInt(Int);
            except
              on E: EOverflow do
                if (TResEvalInt(LeftValue).Int>0) and (TResEvalInt(RightValue).Int>0) then
                  begin
                  UInt:=NativeUInt(TResEvalInt(LeftValue).Int)+NativeUInt(TResEvalInt(RightValue).Int);
                  Result:=TResEvalUInt.Create;
                  TResEvalUInt(Result).UInt:=UInt;
                  end
                else
                  RaiseOverflowArithmetic(20170525122256,Expr);
            end
          else
            begin
            {$IFDEF VerbosePasResolver}
            writeln('TResExprEvaluator.EvalBinaryExpr add int+? Left=',LeftValue.AsDebugString,' Right=',RightValue.AsDebugString);
            {$ENDIF}
            RaiseNotYetImplemented(20170525115537,Expr);
            end;
        else
          {$IFDEF VerbosePasResolver}
          writeln('TResExprEvaluator.EvalBinaryExpr add ?+ Left=',LeftValue.AsDebugString,' Right=',RightValue.AsDebugString);
          {$ENDIF}
          RaiseNotYetImplemented(20170525115548,Expr);
        end;
      else
        {$IFDEF VerbosePasResolver}
        writeln('TResExprEvaluator.EvalBinaryExpr Opcode=',OpcodeStrings[Expr.OpCode],' Left=',LeftValue.AsDebugString,' Right=',RightValue.AsDebugString);
        {$ENDIF}
        RaiseNotYetImplemented(20170518232823,Expr);
      end;
    else
      {$IFDEF VerbosePasResolver}
      writeln('TResExprEvaluator.EvalBinaryExpr Kind=',Expr.Kind,' Opcode=',OpcodeStrings[Expr.OpCode]);
      {$ENDIF}
      RaiseNotYetImplemented(20170518232823,Expr);
    end;
  finally
    ReleaseEvalValue(LeftValue);
    ReleaseEvalValue(RightValue);
  end;
end;

function TResExprEvaluator.EvalArrayParams(Expr: TParamsExpr;
  Flags: TResEvalFlags): TResEvalValue;
begin
  Result:=nil;
  {$IFDEF VerbosePasResEval}
  writeln('TResExprEvaluator.EvalArrayParams ');
  {$ENDIF}
  if refConst in Flags then
    RaiseConstantExprExp(20170522173150,Expr);
end;

function TResExprEvaluator.EvalFuncParams(Expr: TParamsExpr;
  Flags: TResEvalFlags): TResEvalValue;
begin
  Result:=nil;
  {$IFDEF VerbosePasResEval}
  writeln('TResExprEvaluator.EvalFuncParams ');
  {$ENDIF}
  if refConst in Flags then
    RaiseConstantExprExp(20170522173150,Expr);
end;

function TResExprEvaluator.EvalSetParams(Expr: TParamsExpr; Flags: TResEvalFlags
  ): TResEvalValue;
begin
  Result:=nil;
  {$IFDEF VerbosePasResEval}
  writeln('TResExprEvaluator.EvalSetParams ');
  {$ENDIF}
  if length(Expr.Params)=0 then
    begin
    Result:=TResEvalValue.Create;
    Result.Kind:=revkSetEmpty;
    exit;
    end;
  if refConst in Flags then
    RaiseConstantExprExp(20170522173150,Expr);
end;

function TResExprEvaluator.ExprStringToOrd(Value: TResEvalValue;
  PosEl: TPasElement): longword;
var
  l: SizeInt;
  S: RawByteString;
  U: UnicodeString;
begin
  if Value.Kind=revkString then
    begin
    S:=TResEvalString(Value).S;
    l:=length(S);
    if l=0 then
      RaiseMsg(20170522221143,nXExpectedButYFound,sXExpectedButYFound,
        ['char','string'],PosEl)
    else if l=1 then
      Result:=ord(S[1])
    else if l<=4 then
      begin
      U:=UTF8Decode(S);
      if length(U)<>1 then
        RaiseMsg(20170523150826,nXExpectedButYFound,sXExpectedButYFound,
          ['char','string'],PosEl);
      Result:=ord(U[1]);
      end;
    end
  else if Value.Kind=revkUnicodeString then
    begin
    if length(TResEvalUTF16(Value).S)<>1 then
      RaiseMsg(20170522221358,nXExpectedButYFound,sXExpectedButYFound,
        ['char','string'],PosEl)
    else
      Result:=ord(TResEvalUTF16(Value).S[1]);
    end
  else
    RaiseNotYetImplemented(20170522220959,PosEl);
end;

function TResExprEvaluator.EvalPrimitiveExprString(Expr: TPrimitiveExpr
  ): TResEvalValue;
{ Extracts the value from a Pascal string literal

  S is a Pascal string literal e.g. 'Line'#10
    ''  empty string
    '''' => "'"
    #decimal
    #$hex
    ^l  l is a letter a-z
}

  procedure RangeError(id: int64);
  begin
    Result.Free;
    RaiseRangeCheck(id,Expr);
  end;

  procedure Add(h: String);
  begin
    if Result.Kind=revkString then
      TResEvalString(Result).S:=TResEvalString(Result).S+h
    else
      begin
      if CanBeConvertedToUTF16(h)>0 then
        begin
        Result.Free;
        RaiseMsg(20170523114733,nIllegalChar,sIllegalChar,[],Expr);
        end;
      TResEvalUTF16(Result).S:=TResEvalUTF16(Result).S+UnicodeString(h);
      end;
  end;

  procedure AddHash(u: longword);
  var
    h: RawByteString;
  begin
    if (u>255) and (Result.Kind=revkString) then
      begin
      h:=TResEvalString(Result).S;
      Result.Free;
      if CanBeConvertedToUTF16(h)>0 then
        RaiseMsg(20170523123140,nIllegalChar,sIllegalChar,[],Expr);
      Result:=TResEvalUTF16.Create;
      TResEvalUTF16(Result).S:=UnicodeString(h);
      end;
    if Result.Kind=revkString then
      TResEvalString(Result).S:=TResEvalString(Result).S+Chr(u)
    else
      TResEvalUTF16(Result).S:=TResEvalUTF16(Result).S+WideChar(u);
  end;

var
  p, StartP: PChar;
  c: Char;
  u: longword;
  S: String;
begin
  Result:=nil;
  S:=Expr.Value;
  {$IFDEF VerbosePasResEval}
  writeln('TResExprEvaluator.EvalPrimitiveExprString (',S,')');
  {$ENDIF}
  if S='' then
    RaiseInternalError(20170523113809);
  Result:=TResEvalString.Create;
  p:=PChar(S);
  repeat
    case p^ of
    #0: break;
    '''':
      begin
      inc(p);
      StartP:=p;
      repeat
        c:=p^;
        case c of
        #0:
          RaiseInternalError(20170523113938);
        '''':
          begin
          if p>StartP then
            Add(copy(S,StartP-PChar(S)+1,p-StartP));
          inc(p);
          StartP:=p;
          if p^<>'''' then
            break;
          Add('''');
          inc(p);
          StartP:=p;
          end;
        else
          inc(p);
        end;
      until false;
      if p>StartP then
        Add(copy(S,StartP-PChar(S)+1,p-StartP));
      end;
    '#':
      begin
      inc(p);
      if p^='$' then
        begin
        // #$hexnumber
        inc(p);
        StartP:=p;
        u:=0;
        repeat
          c:=p^;
          case c of
          #0: break;
          '0'..'9': u:=u*16+ord(c)-ord('0');
          'a'..'f': u:=u*16+ord(c)-ord('a')+10;
          'A'..'F': u:=u*16+ord(c)-ord('A')+10;
          else break;
          end;
          if u>$ffff then
            RangeError(20170523115712);
          inc(p);
        until false;
        if p=StartP then
          RaiseInternalError(20170207164956);
        AddHash(u);
        end
      else
        begin
        // #decimalnumber
        StartP:=p;
        u:=0;
        repeat
          c:=p^;
          case c of
          #0: break;
          '0'..'9': u:=u*10+ord(c)-ord('0');
          else break;
          end;
          if u>$ffff then
            RangeError(20170523123137);
          inc(p);
        until false;
        if p=StartP then
          RaiseInternalError(20170523123806);
        AddHash(u);
        end;
      end;
    '^':
      begin
      // ^A is #1
      inc(p);
      c:=p^;
      case c of
      'a'..'z': AddHash(ord(c)-ord('a')+1);
      'A'..'Z': AddHash(ord(c)-ord('A')+1);
      else RaiseInternalError(20170523123809);
      end;
      inc(p);
      end;
    else
      RaiseNotYetImplemented(20170523123815,Expr,'ord='+IntToStr(ord(p^)));
    end;
  until false;
  {$IFDEF VerbosePasResEval}
  writeln('TResExprEvaluator.EvalPrimitiveExprString Result=',Result.AsString);
  {$ENDIF}
end;

function TResExprEvaluator.Eval(Expr: TPasExpr; Flags: TResEvalFlags
  ): TResEvalValue;
var
  C: TClass;
  Code: integer;
  Int: NativeInt;
  UInt: NativeUInt;
  Ext: Extended;
begin
  Result:=nil;
  if Expr.CustomData is TResEvalValue then
    begin
    Result:=TResEvalValue(Expr.CustomData);
    exit;
    end;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.Eval Expr=',GetObjName(Expr),' Flags=',dbgs(Flags));
  {$ENDIF}
  if refAutoConst in Flags then
    begin
    Exclude(Flags,refAutoConst);
    if IsConst(Expr) then
      Include(Flags,refConst);
    end;

  C:=Expr.ClassType;
  if C=TPrimitiveExpr then
    begin
    case TPrimitiveExpr(Expr).Kind of
      pekIdent:
        Result:=OnEvalIdentifier(Self,TPrimitiveExpr(Expr),Flags);
      pekNumber:
        begin
        // try int64
        val(TPrimitiveExpr(Expr).Value,Int,Code);
        if Code=0 then
          begin
          Result:=TResEvalInt.Create;
          TResEvalInt(Result).Int:=Int;
          exit;
          end;
        // try qword
        val(TPrimitiveExpr(Expr).Value,UInt,Code);
        if Code=0 then
          begin
          Result:=TResEvalUInt.Create;
          TResEvalUInt(Result).UInt:=UInt;
          exit;
          end;
        // try float
        val(TPrimitiveExpr(Expr).Value,Ext,Code);
        if Code=0 then
          begin
          Result:=TResEvalFloat.Create;
          TResEvalFloat(Result).FloatValue:=Ext;
          exit;
          end;
        RaiseRangeCheck(20170518202252,Expr);
        end;
      pekString:
        begin
        Result:=EvalPrimitiveExprString(TPrimitiveExpr(Expr));
        exit;
        end;
    else
      RaiseNotYetImplemented(20170518200951,Expr);
    end;
    end
  else if C=TNilExpr then
    begin
    Result:=TResEvalValue.Create;
    Result.Kind:=revkNil;
    end
  else if C=TBoolConstExpr then
    begin
    Result:=TResEvalInt.Create;
    Result.Kind:=revkBool;
    if TBoolConstExpr(Expr).Value then
      TResEvalInt(Result).Int:=1
    else
      TResEvalInt(Result).Int:=0;
    end
  else if C=TUnaryExpr then
    Result:=EvalUnaryExpr(TUnaryExpr(Expr),Flags)
  else if C=TBinaryExpr then
    Result:=EvalBinaryExpr(TBinaryExpr(Expr),Flags)
  else if C=TParamsExpr then
    case TParamsExpr(Expr).Kind of
    pekArrayParams: Result:=EvalArrayParams(TParamsExpr(Expr),Flags);
    pekFuncParams: Result:=EvalFuncParams(TParamsExpr(Expr),Flags);
    pekSet: Result:=EvalSetParams(TParamsExpr(Expr),Flags);
    else
      RaiseInternalError(20170522173013);
    end
  else if refConst in Flags then
    RaiseConstantExprExp(20170518213800,Expr);
end;

function TResExprEvaluator.IsInRange(Expr, RangeExpr: TPasExpr;
  EmitHints: boolean): boolean;
var
  ExprValue, RangeValue: TResEvalValue;
  RgInt: TResEvalRangeInt;
  RgUInt: TResEvalRangeUInt;
  CharIndex: LongWord;
begin
  Result:=false;
  ExprValue:=Eval(Expr,[refAutoConst]);
  if ExprValue=nil then
    exit(true); // a variable -> ok
  RangeValue:=nil;
  try
    RangeValue:=Eval(RangeExpr,[]);
    {$IFDEF VerbosePasResEval}
    //writeln('TResExprEvaluator.IsInRange ExprValue=',dbgs(ExprValue),' RangeValue=',dbgs(RangeValue));
    {$ENDIF}
    if RangeValue=nil then
      RaiseNotYetImplemented(20170522171226,RangeExpr);
    case RangeValue.Kind of
    revkSetEmpty:
      begin
      Result:=false;
      exit;
      end;
    revkRangeInt:
      begin
      RgInt:=TResEvalRangeInt(RangeValue);
      case RgInt.ElKind of
        revrikBool:
          if ExprValue.Kind=revkBool then
            exit(true)
          else
            RaiseNotYetImplemented(20170522220104,Expr);
        revrikEnum:
          begin
          if ExprValue.Kind<>revkEnum then
            RaiseInternalError(20170522172754)
          else if ExprValue.IdentEl<>RgInt.IdentEl then
            RaiseInternalError(20170522174028)
          else if (TResEvalEnum(ExprValue).Index<RgInt.RangeStart)
              or (TResEvalEnum(ExprValue).Index>RgInt.RangeEnd) then
            begin
            if EmitHints then
              EmitRangeCheckConst(20170522174406,ExprValue.AsString,
                RgInt.ElementAsString(RgInt.RangeStart),
                RgInt.ElementAsString(RgInt.RangeEnd),
                Expr);
            exit(false);
            end
          else
            exit(true);
          end;
        revrikInt: // int..int
          if ExprValue.Kind=revkInt then
            begin
            // int in int..int
            if (TResEvalInt(ExprValue).Int<RgInt.RangeStart)
                or (TResEvalInt(ExprValue).Int>RgInt.RangeEnd) then
              begin
              if EmitHints then
                EmitRangeCheckConst(20170522174958,ExprValue.AsString,
                  RgInt.ElementAsString(RgInt.RangeStart),
                  RgInt.ElementAsString(RgInt.RangeEnd),
                  Expr);
              exit(false);
              end
            else
              exit(true);
            end
          else if ExprValue.Kind=revkUInt then
            begin
            // uint in int..int
            if (TResEvalUInt(ExprValue).UInt>NativeUInt(High(NativeInt)))
                or (NativeInt(TResEvalUInt(ExprValue).UInt)<RgInt.RangeStart)
                or (NativeInt(TResEvalUInt(ExprValue).UInt)>RgInt.RangeEnd) then
              begin
              if EmitHints then
                EmitRangeCheckConst(20170522215852,ExprValue.AsString,
                  RgInt.ElementAsString(RgInt.RangeStart),
                  RgInt.ElementAsString(RgInt.RangeEnd),
                  Expr);
              exit(false);
              end
            else
              exit(true);
            end
          else
            RaiseNotYetImplemented(20170522215906,Expr);
        revrikChar:
          if ExprValue.Kind in [revkString,revkUnicodeString] then
            begin
            // string in char..char
            CharIndex:=ExprStringToOrd(ExprValue,Expr);
            if (CharIndex<RgInt.RangeStart) or (CharIndex>RgInt.RangeEnd) then
              begin
              if EmitHints then
                EmitRangeCheckConst(20170522221709,ExprValue.AsString,
                  RgInt.ElementAsString(RgInt.RangeStart),
                  RgInt.ElementAsString(RgInt.RangeEnd),
                  Expr);
              exit(false);
              end
            else
              exit(true);
            end
          else
            RaiseNotYetImplemented(20170522220210,Expr);
      else
        RaiseInternalError(20170522172630);
      end;
      end;
    revkRangeUInt:
      if ExprValue.Kind=revkInt then
        begin
        // int in uint..uint
        RgUInt:=TResEvalRangeUInt(RangeValue);
        if (TResEvalInt(ExprValue).Int<0)
            or (NativeUInt(TResEvalInt(ExprValue).Int)<RgUInt.RangeStart)
            or (NativeUInt(TResEvalInt(ExprValue).Int)>RgUInt.RangeEnd) then
          begin
          if EmitHints then
            EmitRangeCheckConst(20170522172250,ExprValue.AsString,
              IntToStr(RgUInt.RangeStart),
              IntToStr(RgUInt.RangeEnd),Expr);
          exit(false);
          end
        else
          exit(true);
        end
      else if ExprValue.Kind=revkUInt then
        begin
        // uint in uint..uint
        RgUInt:=TResEvalRangeUInt(RangeValue);
        if (TResEvalUInt(ExprValue).UInt<RgUInt.RangeStart)
            or (TResEvalUInt(ExprValue).UInt>RgUInt.RangeEnd) then
          begin
          if EmitHints then
            EmitRangeCheckConst(20170522172544,IntToStr(TResEvalUInt(ExprValue).UInt),
              IntToStr(RgUInt.RangeStart),
              IntToStr(RgUInt.RangeEnd),Expr);
          exit(false);
          end
        else
          exit(true);
        end
      else
        RaiseNotYetImplemented(20170522171551,Expr);
    else
      RaiseNotYetImplemented(20170522171307,RangeExpr);
    end;
  finally
    ReleaseEvalValue(ExprValue);
    ReleaseEvalValue(RangeValue);
  end;
end;

function TResExprEvaluator.IsConst(Expr: TPasExpr): boolean;
var
  El: TPasElement;
  C: TClass;
begin
  El:=Expr;
  while El<>nil do
    begin
    C:=El.ClassType;
    if C.InheritsFrom(TPasProcedure) then exit(true);
    if C.InheritsFrom(TPasImplBlock) then exit(false);
    El:=El.Parent;
    end;
  Result:=true;
end;

function TResExprEvaluator.IsSimpleExpr(Expr: TPasExpr): boolean;
var
  C: TClass;
begin
  C:=Expr.ClassType;
  Result:=(C=TNilExpr)
       or (C=TBoolConstExpr)
       or (C=TPrimitiveExpr);
end;

procedure TResExprEvaluator.EmitRangeCheckConst(id: int64; const aValue,
  MinVal, MaxVal: String; PosEl: TPasElement);
begin
  LogMsg(id,mtWarning,nRangeCheckEvaluatingConstantsVMinMax,
    sRangeCheckEvaluatingConstantsVMinMax,[aValue,MinVal,MaxVal],PosEl);
end;

procedure TResExprEvaluator.EmitRangeCheckConst(id: int64;
  const aValue: String; MinVal, MaxVal: int64; PosEl: TPasElement);
begin
  EmitRangeCheckConst(id,aValue,IntToStr(MinVal),IntToStr(MaxVal),PosEl);
end;

{ TResolveData }

procedure TResolveData.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if Element<>nil then
    Element.Release;
  FElement:=AValue;
  if Element<>nil then
    Element.AddRef;
end;

constructor TResolveData.Create;
begin

end;

destructor TResolveData.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolveData.Destroy START ',ClassName);
  {$ENDIF}
  Element:=nil;
  Owner:=nil;
  Next:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolveData.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TResEvalValue }

function TResEvalValue.Clone: TResEvalValue;
begin
  Result:=TResEvalValueClass(ClassType).Create;
  Result.Kind:=Kind;
  Result.IdentEl:=IdentEl;
end;

function TResEvalValue.AsDebugString: string;
begin
  str(Kind,Result);
  Result:=Result+'='+AsString;
end;

function TResEvalValue.AsString: string;
begin
  case Kind of
    revkNone: Result:='<None>';
    revkNil: Result:='nil';
    revkSetEmpty: Result:='[]';
  else
    str(Kind,Result);
  end;
end;

{ TResEvalUInt }

constructor TResEvalUInt.Create;
begin
  inherited Create;
  Kind:=revkUInt;
end;

function TResEvalUInt.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalUInt(Result).UInt:=UInt;
end;

function TResEvalUInt.AsString: string;
begin
  Result:=IntToStr(UInt);
end;

{ TResEvalInt }

constructor TResEvalInt.Create;
begin
  inherited Create;
  Kind:=revkInt;
end;

function TResEvalInt.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalInt(Result).Int:=Int;
end;

function TResEvalInt.AsString: string;
begin
  case Kind of
  revkBool: if Int=0 then Result:='false' else Result:='true';
  revkInt: Result:=IntToStr(Int);
  end;
end;

{ TResEvalFloat }

constructor TResEvalFloat.Create;
begin
  inherited Create;
  Kind:=revkFloat;
end;

function TResEvalFloat.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalFloat(Result).FloatValue:=FloatValue;
end;

function TResEvalFloat.AsString: string;
begin
  str(FloatValue,Result);
end;

{ TResEvalString }

constructor TResEvalString.Create;
begin
  inherited Create;
  Kind:=revkString;
end;

function TResEvalString.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalString(Result).S:=S;
end;

function TResEvalString.AsString: string;
begin
  Result:=RawStrToCaption(S,60);
end;

{ TResEvalUTF16 }

constructor TResEvalUTF16.Create;
begin
  inherited Create;
  Kind:=revkUnicodeString;
end;

function TResEvalUTF16.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalUTF16(Result).S:=S;
end;

function TResEvalUTF16.AsString: string;
begin
  Result:=String(UnicodeStrToCaption(S,60));
end;

{ TResEvalEnum }

constructor TResEvalEnum.Create;
begin
  inherited Create;
  Kind:=revkEnum;
end;

function TResEvalEnum.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalEnum(Result).Index:=Index;
end;

function TResEvalEnum.AsDebugString: string;
begin
  str(Kind,Result);
  Result:=Result+'='+IdentEl.Name+'='+IntToStr(Index);
end;

function TResEvalEnum.AsString: string;
begin
  Result:=IdentEl.Name;
end;

{ TResEvalRangeInt }

constructor TResEvalRangeInt.Create;
begin
  inherited Create;
  Kind:=revkRangeInt;
end;

function TResEvalRangeInt.Clone: TResEvalValue;
begin
  Result:=inherited Clone;
  TResEvalRangeInt(Result).ElKind:=ElKind;
  TResEvalRangeInt(Result).RangeStart:=RangeStart;
  TResEvalRangeInt(Result).RangeEnd:=RangeEnd;
end;

function TResEvalRangeInt.AsString: string;
begin
  Result:=ElementAsString(RangeStart)+'..'+ElementAsString(RangeEnd);
end;

function TResEvalRangeInt.ElementAsString(El: int64): string;
begin
  case ElKind of
    revrikBool: if El=0 then Result:='false' else Result:='true';
    revrikEnum: Result:=TPasEnumValue(TPasEnumType(IdentEl).Values[El]).Name;
    revrikInt: Result:=IntToStr(El);
    revrikChar:
      if ((El>=32) and (El<=38)) or ((El>=40) and (El<=126)) then
        Result:=''''+Chr(El)+''''
      else
        Result:='#'+IntToStr(El);
  end;
end;

{ TResEvalSetInt }

constructor TResEvalSetInt.Create;
begin
  inherited Create;
  Kind:=revkSetOfInt;
end;

function TResEvalSetInt.Clone: TResEvalValue;
var
  RS: TResEvalSetInt;
  i: Integer;
begin
  Result:=inherited Clone;
  TResEvalSetInt(Result).ElKind:=ElKind;
  RS:=TResEvalSetInt(Result);
  SetLength(RS.Ranges,length(Ranges));
  for i:=0 to length(Ranges)-1 do
    RS.Ranges[i]:=Ranges[i];
end;

function TResEvalSetInt.AsString: string;
var
  i: Integer;
begin
  Result:='[';
  for i:=0 to length(Ranges)-1 do
    begin
    if i>0 then Result:=Result+',';
    Result:=Result+ElementAsString(Ranges[i].RangeStart);
    if Ranges[i].RangeStart<>Ranges[i].RangeEnd then
      Result:=Result+'..'+ElementAsString(Ranges[i].RangeEnd);
    end;
  Result:=Result+']';
end;

function TResEvalSetInt.ElementAsString(El: int64): string;
begin
  case ElKind of
    revsikEnum: Result:=TPasEnumValue(TPasEnumType(IdentEl).Values[El]).Name;
    revsikInt: Result:=IntToStr(El);
    revsikChar: Result:=Chr(El);
    revsikWChar: Result:=String(WideChar(El));
  end;
end;

end.


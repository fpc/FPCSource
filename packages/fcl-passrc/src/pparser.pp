{
    This file is part of the Free Component Library

    Pascal source parser
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

unit PParser;

interface

uses SysUtils, Classes, PasTree, PScanner;

// message numbers
const
  nErrNoSourceGiven = 2001;
  nErrMultipleSourceFiles = 2002;
  nParserError = 2003;
  nParserErrorAtToken = 2004;
  nParserUngetTokenError = 2005;
  nParserExpectTokenError = 2006;
  nParserForwardNotInterface = 2007;
  nParserExpectVisibility = 2008;
  nParserStrangeVisibility = 2009;
  nParserExpectToken2Error = 2010;
  nParserExpectedCommaRBracket = 2011;
  nParserExpectedCommaSemicolon = 2012;
  nParserExpectedAssignIn = 2013;
  nParserExpectedCommaColon = 2014;
  nErrUnknownOperatorType = 2015;
  nParserOnlyOneArgumentCanHaveDefault = 2016;
  nParserExpectedLBracketColon = 2017;
  nParserExpectedSemiColonEnd = 2018;
  nParserExpectedConstVarID = 2019;
  nParserExpectedNested = 2020;
  nParserExpectedColonID = 2021;
  nParserSyntaxError = 2022;
  nParserTypeSyntaxError = 2023;
  nParserArrayTypeSyntaxError = 2024;
  nParserExpectedIdentifier = 2026;
  nParserNotAProcToken = 2026;
  nRangeExpressionExpected = 2027;
  nParserExpectCase = 2028;
  nParserHelperNotAllowed = 2029;
  nLogStartImplementation = 2030;
  nLogStartInterface = 2031;
  nParserNoConstructorAllowed = 2032;
  nParserNoFieldsAllowed = 2033;
  nParserInvalidRecordVisibility = 2034;
  nErrRecordConstantsNotAllowed = 2035;
  nErrRecordMethodsNotAllowed = 2036;
  nErrRecordPropertiesNotAllowed = 2037;
  nErrRecordVisibilityNotAllowed = 2038;
  nParserTypeNotAllowedHere = 2039;
  nParserNotAnOperand = 2040;
  nParserArrayPropertiesCannotHaveDefaultValue = 2041;
  nParserDefaultPropertyMustBeArray = 2042;
  nParserUnknownProcedureType = 2043;
  nParserGenericArray1Element = 2044;
  nParserGenericClassOrArray = 2045;
  nParserDuplicateIdentifier = 2046;
  nParserDefaultParameterRequiredFor = 2047;
  nParserOnlyOneVariableCanBeInitialized = 2048;
  nParserExpectedTypeButGot = 2049;
  nParserPropertyArgumentsCanNotHaveDefaultValues = 2050;
  nParserExpectedExternalClassName = 2051;

// resourcestring patterns of messages
resourcestring
  SErrNoSourceGiven = 'No source file specified';
  SErrMultipleSourceFiles = 'Please specify only one source file';
  SParserError = 'Error';
  SParserErrorAtToken = '%s at token "%s" in file %s at line %d column %d';
  SParserUngetTokenError = 'Internal error: Cannot unget more tokens, history buffer is full';
  SParserExpectTokenError = 'Expected "%s"';
  SParserForwardNotInterface = 'The use of a FORWARD procedure modifier is not allowed in the interface';
  SParserExpectVisibility = 'Expected visibility specifier';
  SParserStrangeVisibility = 'Strange strict visibility encountered : "%s"';
  SParserExpectToken2Error = 'Expected "%s" or "%s"';
  SParserExpectedCommaRBracket = 'Expected "," or ")"';
  SParserExpectedCommaSemicolon = 'Expected "," or ";"';
  SParserExpectedAssignIn = 'Expected := or in';
  SParserExpectedCommaColon = 'Expected "," or ":"';
  SErrUnknownOperatorType = 'Unknown operator type: %s';
  SParserOnlyOneArgumentCanHaveDefault = 'A default value can only be assigned to 1 parameter';
  SParserExpectedLBracketColon = 'Expected "(" or ":"';
  SParserExpectedSemiColonEnd = 'Expected ";" or "End"';
  SParserExpectedConstVarID = 'Expected "const", "var" or identifier';
  SParserExpectedNested = 'Expected nested keyword';
  SParserExpectedColonID = 'Expected ":" or identifier';
  SParserSyntaxError = 'Syntax error';
  SParserTypeSyntaxError = 'Syntax error in type';
  SParserArrayTypeSyntaxError = 'Syntax error in array type';
  SParserExpectedIdentifier = 'Identifier expected';
  SParserNotAProcToken = 'Not a procedure or function token';
  SRangeExpressionExpected = 'Range expression expected';
  SParserExpectCase = 'Case label expression expected';
  SParserHelperNotAllowed = 'Helper objects not allowed for "%s"';
  SLogStartImplementation = 'Start parsing implementation section.';
  SLogStartInterface = 'Start parsing interface section';
  SParserNoConstructorAllowed = 'Constructors or Destructors are not allowed in Interfaces or Record helpers';
  SParserNoFieldsAllowed = 'Fields are not allowed in Interfaces';
  SParserInvalidRecordVisibility = 'Records can only have public and (strict) private as visibility specifiers';
  SErrRecordConstantsNotAllowed = 'Record constants not allowed at this location.';
  SErrRecordMethodsNotAllowed = 'Record methods not allowed at this location.';
  SErrRecordPropertiesNotAllowed = 'Record properties not allowed at this location.';
  SErrRecordVisibilityNotAllowed = 'Record visibilities not allowed at this location.';
  SParserTypeNotAllowedHere = 'Type "%s" not allowed here';
  SParserNotAnOperand = 'Not an operand: (%d : %s)';
  SParserArrayPropertiesCannotHaveDefaultValue = 'Array properties cannot have default value';
  SParserDefaultPropertyMustBeArray = 'The default property must be an array property';
  SParserUnknownProcedureType = 'Unknown procedure type "%d"';
  SParserGenericArray1Element = 'Generic arrays can have only 1 template element';
  SParserGenericClassOrArray = 'Generic can only be used with classes or arrays';
  SParserDuplicateIdentifier = 'Duplicate identifier "%s"';
  SParserDefaultParameterRequiredFor = 'Default parameter required for "%s"';
  SParserOnlyOneVariableCanBeInitialized = 'Only one variable can be initialized';
  SParserExpectedTypeButGot = 'Expected type, but got %s';
  SParserPropertyArgumentsCanNotHaveDefaultValues = 'Property arguments can not have default values';
  SParserExpectedExternalClassName = 'Expected external class name';

type
  TPasScopeType = (
    stModule,  // e.g. unit, program, library
    stUsesClause,
    stTypeSection,
    stTypeDef, // e.g. a TPasType
    stConstDef, // e.g. a TPasConst
    stProcedure, // also method, procedure, constructor, destructor, ...
    stProcedureHeader,
    stExceptOnExpr,
    stExceptOnStatement,
    stDeclaration, // e.g. a TPasProperty, TPasVariable, TPasArgument
    stAncestors // the list of ancestors and interfaces of a class
    );
  TPasScopeTypes = set of TPasScopeType;

  TPasParserLogHandler = Procedure (Sender : TObject; Const Msg : String) of object;
  TPParserLogEvent = (pleInterface,pleImplementation);
  TPParserLogEvents = set of TPParserLogEvent;
  TPasParser = Class;

  { TPasTreeContainer }

  TPasTreeContainer = class
  private
    FCurrentParser: TPasParser;
    FNeedComments: Boolean;
    FOnLog: TPasParserLogHandler;
    FPParserLogEvents: TPParserLogEvents;
    FScannerLogEvents: TPScannerLogEvents;
  protected
    FPackage: TPasPackage;
    FInterfaceOnly : Boolean;
    procedure SetCurrentParser(AValue: TPasParser); virtual;
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; const ASourceFilename: String;
      ASourceLinenumber: Integer): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;overload;
      virtual; abstract;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos): TPasElement; overload;
      virtual;
    function CreateFunctionType(const AName, AResultName: String; AParent: TPasElement;
      UseParentAsResultParent: Boolean; const ASrcPos: TPasSourcePos): TPasFunctionType;
    function FindElement(const AName: String): TPasElement; virtual; abstract;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); virtual;
    function FindModule(const AName: String): TPasModule; virtual;
    function NeedArrayValues(El: TPasElement): boolean; virtual;
    property Package: TPasPackage read FPackage;
    property InterfaceOnly : Boolean Read FInterfaceOnly Write FInterFaceOnly;
    property ScannerLogEvents : TPScannerLogEvents Read FScannerLogEvents Write FScannerLogEvents;
    property ParserLogEvents : TPParserLogEvents Read FPParserLogEvents Write FPParserLogEvents;
    property OnLog : TPasParserLogHandler Read FOnLog Write FOnLog;
    property CurrentParser : TPasParser Read FCurrentParser Write SetCurrentParser;
    property NeedComments : Boolean Read FNeedComments Write FNeedComments;
  end;

  EParserError = class(Exception)
  private
    FFilename: String;
    FRow, FColumn: Integer;
  public
    constructor Create(const AReason, AFilename: String;
      ARow, AColumn: Integer);
    property Filename: String read FFilename;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
  end;

  TProcType = (ptProcedure, ptFunction, ptOperator, ptClassOperator, ptConstructor, ptDestructor,
               ptClassProcedure, ptClassFunction, ptClassConstructor, ptClassDestructor);


  TExprKind = (ek_Normal, ek_PropertyIndex);
  TIndentAction = (iaNone,iaIndent,iaUndent);

  { TPasParser }

  TPasParser = class
  private
    const FTokenRingSize = 32;
    type
      TTokenRec = record
        Token: TToken;
        AsString: String;
        Comments: TStrings;
        SourcePos: TPasSourcePos;
      end;
      PTokenRec = ^TTokenRec;
  private
    FCurModule: TPasModule;
    FFileResolver: TBaseFileResolver;
    FImplicitUses: TStrings;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FLogEvents: TPParserLogEvents;
    FOnLog: TPasParserLogHandler;
    FOptions: TPOptions;
    FScanner: TPascalScanner;
    FEngine: TPasTreeContainer;
    FCurToken: TToken;
    FCurTokenString: String;
    FSavedComments : String;
    // UngetToken support:
    FTokenRing: array[0..FTokenRingSize-1] of TTokenRec;
    FTokenRingCur: Integer; // index of current token in FTokenBuffer
    FTokenRingStart: Integer; // first valid ring index in FTokenBuffer, if FTokenRingStart=FTokenRingEnd the ring is empty
    FTokenRingEnd: Integer; // first invalid ring index in FTokenBuffer
    FDumpIndent : String;
    function CheckOverloadList(AList: TFPList; AName: String; out OldMember: TPasElement): TPasOverloadedProc;
    function DoCheckHint(Element: TPasElement): Boolean;
    procedure DumpCurToken(Const Msg : String; IndentAction : TIndentAction = iaNone);
    function GetCurrentModeSwitches: TModeSwitches;
    Procedure SetCurrentModeSwitches(AValue: TModeSwitches);
    function GetVariableModifiers(Parent: TPasElement; Out VarMods: TVariableModifiers; Out LibName, ExportName: TPasExpr; ExternalClass : Boolean): string;
    function GetVariableValueAndLocation(Parent : TPasElement; Out Value : TPasExpr; Out Location: String): Boolean;
    procedure HandleProcedureModifier(Parent: TPasElement; pm : TProcedureModifier);
    procedure HandleProcedureTypeModifier(ProcType: TPasProcedureType; ptm : TProcTypeModifier);
    procedure ParseClassLocalConsts(AType: TPasClassType; AVisibility: TPasMemberVisibility);
    procedure ParseClassLocalTypes(AType: TPasClassType; AVisibility: TPasMemberVisibility);
    procedure ParseVarList(Parent: TPasElement; VarList: TFPList; AVisibility: TPasMemberVisibility; Full: Boolean);
    procedure SetOptions(AValue: TPOptions);
  protected
    Function SaveComments : String;
    Function SaveComments(Const AValue : String) : String;
    function LogEvent(E : TPParserLogEvent) : Boolean; inline;
    Procedure DoLog(MsgType: TMessageType; MsgNumber: integer; Const Msg : String; SkipSourceInfo : Boolean = False);overload;
    Procedure DoLog(MsgType: TMessageType; MsgNumber: integer; Const Fmt : String; Args : Array of const;SkipSourceInfo : Boolean = False);overload;
    function GetProcTypeFromToken(tk: TToken; IsClass: Boolean=False ): TProcType;
    procedure ParseAsmBlock(AsmBlock: TPasImplAsmStatement); virtual;
    procedure ParseRecordFieldList(ARec: TPasRecordType; AEndToken: TToken; AllowMethods : Boolean);
    procedure ParseRecordVariantParts(ARec: TPasRecordType; AEndToken: TToken);
    function GetProcedureClass(ProcType : TProcType): TPTreeElement;
    procedure ParseClassFields(AType: TPasClassType; const AVisibility: TPasMemberVisibility; IsClassField : Boolean);
    procedure ParseClassMembers(AType: TPasClassType);
    procedure ProcessMethod(AType: TPasClassType; IsClass : Boolean; AVisibility : TPasMemberVisibility);
    procedure ReadGenericArguments(List : TFPList;Parent : TPasElement);
    procedure ReadSpecializeArguments(Spec: TPasSpecializeType);
    function ReadDottedIdentifier(Parent: TPasElement; out Expr: TPasExpr; NeedAsString: boolean): String;
    function CheckProcedureArgs(Parent: TPasElement;
      Args: TFPList; // list of TPasArgument
      Mandatory: Boolean): boolean;
    function CheckVisibility(S: String; var AVisibility: TPasMemberVisibility): Boolean;
    procedure ParseExc(MsgNumber: integer; const Msg: String);
    procedure ParseExc(MsgNumber: integer; const Fmt: String; Args : Array of const);
    procedure ParseExcExpectedIdentifier;
    procedure ParseExcSyntaxError;
    procedure ParseExcTokenError(const Arg: string);
    function OpLevel(t: TToken): Integer;
    Function TokenToExprOp (AToken : TToken) : TExprOpCode;
    function CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement; const ASrcPos: TPasSourcePos): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility; const ASrcPos: TPasSourcePos): TPasElement;overload;
    function CreatePrimitiveExpr(AParent: TPasElement; AKind: TPasExprKind; const AValue: String): TPrimitiveExpr;
    function CreateBoolConstExpr(AParent: TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean): TBoolConstExpr;
    function CreateBinaryExpr(AParent : TPasElement; xleft, xright: TPasExpr; AOpCode: TExprOpCode): TBinaryExpr;
    procedure AddToBinaryExprChain(var ChainFirst: TPasExpr;
      Element: TPasExpr; AOpCode: TExprOpCode);
    procedure AddParamsToBinaryExprChain(var ChainFirst: TPasExpr;
      Params: TParamsExpr);
    {$IFDEF VerbosePasParser}
    procedure WriteBinaryExprChain(Prefix: string; First, Last: TPasExpr);
    {$ENDIF}
    function CreateUnaryExpr(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode): TUnaryExpr;
    function CreateArrayValues(AParent : TPasElement): TArrayValues;
    function CreateFunctionType(const AName, AResultName: String; AParent: TPasElement;
             UseParentAsResultParent: Boolean; const NamePos: TPasSourcePos): TPasFunctionType;
    function CreateInheritedExpr(AParent : TPasElement): TInheritedExpr;
    function CreateSelfExpr(AParent : TPasElement): TSelfExpr;
    function CreateNilExpr(AParent : TPasElement): TNilExpr;
    function CreateRecordValues(AParent : TPasElement): TRecordValues;
    Function IsCurTokenHint(out AHint : TPasMemberHint) : Boolean; overload;
    Function IsCurTokenHint: Boolean; overload;
    Function TokenIsCallingConvention(const S : String; out CC : TCallingConvention) : Boolean; virtual;
    Function TokenIsProcedureModifier(Parent : TPasElement; const S : String; Out PM : TProcedureModifier) : Boolean; virtual;
    Function TokenIsProcedureTypeModifier(Parent : TPasElement; const S : String; Out PTM : TProcTypeModifier) : Boolean; virtual;
    Function CheckHint(Element : TPasElement; ExpectSemiColon : Boolean) : TPasMemberHints;
    function ParseParams(AParent : TPasElement;paramskind: TPasExprKind; AllowFormatting : Boolean = False): TParamsExpr;
    function ParseExpIdent(AParent : TPasElement): TPasExpr;
    procedure DoParseClassType(AType: TPasClassType);
    function DoParseExpression(AParent: TPaselement;InitExpr: TPasExpr=nil; AllowEqual : Boolean = True): TPasExpr;
    function DoParseConstValueExpression(AParent: TPasElement): TPasExpr;
    function CheckPackMode: TPackMode;
    function AddUseUnit(ASection: TPasSection; const NamePos: TPasSourcePos;
      AUnitName : string; NameExpr: TPasExpr; InFileExpr: TPrimitiveExpr): TPasElement;
    procedure CheckImplicitUsedUnits(ASection: TPasSection);
    // Overload handling
    procedure AddProcOrFunction(Decs: TPasDeclarations; AProc: TPasProcedure);
    function  CheckIfOverloaded(AParent: TPasElement; const AName: String): TPasElement;
  public
    constructor Create(AScanner: TPascalScanner; AFileResolver: TBaseFileResolver;  AEngine: TPasTreeContainer);
    Destructor Destroy; override;
    procedure SetLastMsg(MsgType: TMessageType; MsgNumber: integer; Const Fmt : String; Args : Array of const);
    // General parsing routines
    function CurTokenName: String;
    function CurTokenText: String;
    Function CurComments : TStrings;
    function CurSourcePos: TPasSourcePos;
    function HasToken: boolean;
    Function SavedComments : String;
    procedure NextToken; // read next non whitespace, non space
    procedure ChangeToken(tk: TToken);
    procedure UngetToken;
    procedure CheckToken(tk: TToken);
    procedure CheckTokens(tk: TTokens);
    procedure ExpectToken(tk: TToken);
    procedure ExpectTokens(tk:  TTokens);
    function ExpectIdentifier: String;
    Function CurTokenIsIdentifier(Const S : String) : Boolean;
    // Expression parsing
    function isEndOfExp(AllowEqual : Boolean = False; CheckHints : Boolean = True): Boolean;
    function ExprToText(Expr: TPasExpr): String;
    function ArrayExprToText(Expr: TPasExprArray): String;
    // Type declarations
    function ResolveTypeReference(Name: string; Parent: TPasElement): TPasType;
    function ParseComplexType(Parent : TPasElement = Nil): TPasType;
    function ParseTypeDecl(Parent: TPasElement): TPasType;
    function ParseType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: String = ''; Full: Boolean = false; GenericArgs: TFPList = nil): TPasType;
    function ParseReferenceToProcedureType(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String): TPasProcedureType;
    function ParseProcedureType(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String; const PT: TProcType): TPasProcedureType;
    function ParseStringType(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String): TPasAliasType;
    function ParseSimpleType(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String; IsFull : Boolean = False): TPasType;
    function ParseAliasType(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String): TPasTypeAliasType;
    function ParseTypeReference(Parent: TPasElement; NeedExpr: boolean; out Expr: TPasExpr): TPasType;
    function ParsePointerType(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String): TPasPointerType;
    Function ParseArrayType(Parent : TPasElement; Const NamePos: TPasSourcePos; Const TypeName : String; PackMode : TPackMode) : TPasArrayType;
    Function ParseFileType(Parent : TPasElement; Const NamePos: TPasSourcePos; Const TypeName  : String) : TPasFileType;
    Function ParseRecordDecl(Parent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName : string; const Packmode : TPackMode = pmNone) : TPasRecordType;
    function ParseEnumType(Parent: TPasElement; Const NamePos: TPasSourcePos; const TypeName: String): TPasEnumType;
    function ParseSetType(Parent: TPasElement; Const NamePos: TPasSourcePos; const TypeName: String; AIsPacked : Boolean = False): TPasSetType;
    function ParseSpecializeType(Parent: TPasElement; Const TypeName: String): TPasSpecializeType;
    Function ParseClassDecl(Parent: TPasElement; Const NamePos: TPasSourcePos; Const AClassName: String; AObjKind: TPasObjKind; PackMode : TPackMode= pmNone; GenericArgs: TFPList = nil): TPasType;
    Function ParseProperty(Parent : TPasElement; Const AName : String; AVisibility : TPasMemberVisibility; IsClassField: boolean) : TPasProperty;
    function ParseRangeType(AParent: TPasElement; Const NamePos: TPasSourcePos; Const TypeName: String; Full: Boolean = True): TPasRangeType;
    procedure ParseExportDecl(Parent: TPasElement; List: TFPList);
    // Constant declarations
    function ParseConstDecl(Parent: TPasElement): TPasConst;
    function ParseResourcestringDecl(Parent: TPasElement): TPasResString;
    // Variable handling. This includes parts of records
    procedure ParseVarDecl(Parent: TPasElement; List: TFPList);
    procedure ParseInlineVarDecl(Parent: TPasElement; List: TFPList;  AVisibility : TPasMemberVisibility  = visDefault; ClosingBrace: Boolean = False);
    // Main scope parsing
    procedure ParseMain(var Module: TPasModule);
    procedure ParseUnit(var Module: TPasModule);
    procedure ParseProgram(var Module: TPasModule; SkipHeader : Boolean = False);
    procedure ParseLibrary(var Module: TPasModule);
    procedure ParseOptionalUsesList(ASection: TPasSection);
    procedure ParseUsesList(ASection: TPasSection);
    procedure ParseInterface;
    procedure ParseImplementation;
    procedure ParseInitialization;
    procedure ParseFinalization;
    procedure ParseDeclarations(Declarations: TPasDeclarations);
    procedure ParseStatement(Parent: TPasImplBlock;  out NewImplElement: TPasImplElement);
    procedure ParseLabels(AParent: TPasElement);
    procedure ParseProcBeginBlock(Parent: TProcedureBody);
    procedure ParseProcAsmBlock(Parent: TProcedureBody);
    // Function/Procedure declaration
    function  ParseProcedureOrFunctionDecl(Parent: TPasElement; ProcType: TProcType;AVisibility : TPasMemberVisibility = VisDefault): TPasProcedure;
    procedure ParseArgList(Parent: TPasElement;
      Args: TFPList; // list of TPasArgument
      EndToken: TToken);
    procedure ParseProcedureOrFunctionHeader(Parent: TPasElement; Element: TPasProcedureType; ProcType: TProcType; OfObjectPossible: Boolean);
    procedure ParseProcedureBody(Parent: TPasElement);
    // Properties for external access
    property FileResolver: TBaseFileResolver read FFileResolver;
    property Scanner: TPascalScanner read FScanner;
    property Engine: TPasTreeContainer read FEngine;
    property CurToken: TToken read FCurToken;
    property CurTokenString: String read FCurTokenString;
    Property Options : TPOptions Read FOptions Write SetOptions;
    Property CurrentModeswitches : TModeSwitches Read GetCurrentModeSwitches Write SetCurrentModeSwitches;
    Property CurModule : TPasModule Read FCurModule;
    Property LogEvents : TPParserLogEvents Read FLogEvents Write FLogEvents;
    Property OnLog : TPasParserLogHandler Read FOnLog Write FOnLog;
    property ImplicitUses: TStrings read FImplicitUses;
    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
  end;

Type
  TParseSourceOption = (poUseStreams,poSkipDefaultDefs);
  TParseSourceOptions = set of TParseSourceOption;
function ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine, OSTarget, CPUTarget: String): TPasModule;
function ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine, OSTarget, CPUTarget: String;
                     UseStreams  : Boolean): TPasModule; deprecated;
function ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine, OSTarget, CPUTarget: String;
                     Options : TParseSourceOptions): TPasModule;
                     
Function IsHintToken(T : String; Out AHint : TPasMemberHint) : boolean;
Function IsProcModifier(S : String; Out PM : TProcedureModifier) : Boolean;
Function IsCallingConvention(S : String; out CC : TCallingConvention) : Boolean;
Function TokenToAssignKind( tk : TToken) : TAssignKind;

implementation

const
  WhitespaceTokensToIgnore = [tkWhitespace, tkComment, tkLineEnding, tkTab];

type
  TDeclType = (declNone, declConst, declResourcestring, declType,
               declVar, declThreadvar, declProperty, declExports);

Function IsHintToken(T : String; Out AHint : TPasMemberHint) : boolean;

Const
   MemberHintTokens : Array[TPasMemberHint] of string =
     ('deprecated','library','platform','experimental','unimplemented');
Var
  I : TPasMemberHint;

begin
  t:=LowerCase(t);
  Result:=False;
  For I:=Low(TPasMemberHint) to High(TPasMemberHint) do
    begin
    result:=(t=MemberHintTokens[i]);
    if Result then
      begin
      aHint:=I;
      exit;
      end;
    end;
end;


Function IsCallingConvention(S : String; out CC : TCallingConvention) : Boolean;

Var
  CCNames : Array[TCallingConvention] of String
         = ('','register','pascal','cdecl','stdcall','oldfpccall','safecall','syscall');
Var
  C : TCallingConvention;

begin
  S:=Lowercase(s);
  Result:=False;
  for C:=Low(TCallingConvention) to High(TCallingConvention) do
    begin
    Result:=(CCNames[c]<>'') and (s=CCnames[c]);
    If Result then
      begin
      CC:=C;
      exit;
      end;
    end;
end;

Function IsProcModifier(S : String; Out PM : TProcedureModifier) : Boolean;

Var
  P : TProcedureModifier;

begin
  S:=LowerCase(S);
  Result:=False;
  For P:=Low(TProcedureModifier) to High(TProcedureModifier) do
    begin
    Result:=s=ModifierNames[P];
    If Result then
      begin
      PM:=P;
      exit;
      end;
    end;
end;

Function TokenToAssignKind( tk : TToken) : TAssignKind;

begin
  case tk of
    tkAssign         : Result:=akDefault;
    tkAssignPlus     : Result:=akAdd;
    tkAssignMinus    : Result:=akMinus;
    tkAssignMul      : Result:=akMul;
    tkAssignDivision : Result:=akDivision;
  else
    Raise Exception.CreateFmt('Not an assignment token : %s',[TokenInfos[tk]]);
  end;
end;

function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: String): TPasModule;

begin
  Result:=ParseSource(AEngine,FPCCommandLine, OSTarget, CPUTarget,[]);
end;

function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: String; UseStreams : Boolean): TPasModule;

begin
  if UseStreams then
    Result:=ParseSource(AEngine,FPCCommandLine, OSTarget, CPUTarget,[poUseStreams])
  else
    Result:=ParseSource(AEngine,FPCCommandLine, OSTarget, CPUTarget,[]);
end;

function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: String;
  Options : TParseSourceOptions): TPasModule;
var
  FileResolver: TFileResolver;
  Parser: TPasParser;
  Start, CurPos: PChar;
  Filename: String;
  Scanner: TPascalScanner;

  procedure ProcessCmdLinePart;
  var
    l: Integer;
    s: String;
  begin
    l := CurPos - Start;
    SetLength(s, l);
    if l > 0 then
      Move(Start^, s[1], l)
    else
      exit;
    if (s[1] = '-') and (length(s)>1) then
    begin
      case s[2] of
        'd': // -d define
          Scanner.AddDefine(UpperCase(Copy(s, 3, Length(s))));
        'u': // -u undefine
          Scanner.RemoveDefine(UpperCase(Copy(s, 3, Length(s))));
        'F': // -F
          if (length(s)>2) and (s[3] = 'i') then // -Fi include path
            FileResolver.AddIncludePath(Copy(s, 4, Length(s)));
        'I': // -I include path
          FileResolver.AddIncludePath(Copy(s, 3, Length(s)));
        'S': // -S mode
          if  (length(s)>2) then
            begin
            l:=3;
            While L<=Length(S) do
              begin
              case S[l] of
                'c' : Scanner.Options:=Scanner.Options+[po_cassignments];
                'd' : Scanner.SetCompilerMode('DELPHI');
                '2' : Scanner.SetCompilerMode('OBJFPC');
                'h' : ; // do nothing
              end;
              inc(l);
              end;
            end;
        'M' :
           begin
           delete(S,1,2);
           Scanner.SetCompilerMode(S);
           end;
      end;
    end else
      if Filename <> '' then
        raise Exception.Create(SErrMultipleSourceFiles)
      else
        Filename := s;
  end;

var
  s: String;
begin
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  Parser := nil;
  try
    FileResolver := TFileResolver.Create;
    FileResolver.UseStreams:=poUseStreams in Options;
    Scanner := TPascalScanner.Create(FileResolver);
    SCanner.LogEvents:=AEngine.ScannerLogEvents;
    SCanner.OnLog:=AEngine.Onlog;
    if not (poSkipDefaultDefs in Options) then
      begin
      Scanner.AddDefine('FPK');
      Scanner.AddDefine('FPC');
      // TargetOS
      s := UpperCase(OSTarget);
      Scanner.AddDefine(s);
      if s = 'LINUX' then
        Scanner.AddDefine('UNIX')
      else if s = 'FREEBSD' then
      begin
        Scanner.AddDefine('BSD');
        Scanner.AddDefine('UNIX');
      end else if s = 'NETBSD' then
      begin
        Scanner.AddDefine('BSD');
        Scanner.AddDefine('UNIX');
      end else if s = 'SUNOS' then
      begin
        Scanner.AddDefine('SOLARIS');
        Scanner.AddDefine('UNIX');
      end else if s = 'GO32V2' then
        Scanner.AddDefine('DPMI')
      else if s = 'BEOS' then
        Scanner.AddDefine('UNIX')
      else if s = 'QNX' then
        Scanner.AddDefine('UNIX')
      else if s = 'AROS' then
        Scanner.AddDefine('HASAMIGA')
      else if s = 'MORPHOS' then
        Scanner.AddDefine('HASAMIGA')
      else if s = 'AMIGA' then
        Scanner.AddDefine('HASAMIGA');

      // TargetCPU
      s := UpperCase(CPUTarget);
      Scanner.AddDefine('CPU'+s);
      if (s='X86_64') then
        Scanner.AddDefine('CPU64')
      else
        Scanner.AddDefine('CPU32');
      end;
    Parser := TPasParser.Create(Scanner, FileResolver, AEngine);
    Filename := '';
    Parser.LogEvents:=AEngine.ParserLogEvents;
    Parser.OnLog:=AEngine.Onlog;

    if FPCCommandLine<>'' then
      begin
        Start := @FPCCommandLine[1];
        CurPos := Start;
        while CurPos[0] <> #0 do
        begin
          if CurPos[0] = ' ' then
          begin
            ProcessCmdLinePart;
            Start := CurPos + 1;
          end;
          Inc(CurPos);
        end;
        ProcessCmdLinePart;
      end;

    if Filename = '' then
      raise Exception.Create(SErrNoSourceGiven);
    FileResolver.AddIncludePath(ExtractFilePath(FileName));
    Scanner.OpenFile(Filename);
    Parser.ParseMain(Result);
  finally
    Parser.Free;
    Scanner.Free;
    FileResolver.Free;
  end;
end;

{ ---------------------------------------------------------------------
  TPasTreeContainer
  ---------------------------------------------------------------------}

procedure TPasTreeContainer.SetCurrentParser(AValue: TPasParser);
begin
  if FCurrentParser=AValue then Exit;
  FCurrentParser:=AValue;
end;

function TPasTreeContainer.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; const ASourceFilename: String;
  ASourceLinenumber: Integer): TPasElement;
begin
  Result := CreateElement(AClass, AName, AParent, visDefault, ASourceFilename,
    ASourceLinenumber);
end;

function TPasTreeContainer.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
begin
  Result := CreateElement(AClass, AName, AParent, AVisibility, ASrcPos.FileName,
    ASrcPos.Row);
end;

function TPasTreeContainer.CreateFunctionType(const AName, AResultName: String;
  AParent: TPasElement; UseParentAsResultParent: Boolean;
  const ASrcPos: TPasSourcePos): TPasFunctionType;
var
  ResultParent: TPasElement;
begin
  Result := TPasFunctionType(CreateElement(TPasFunctionType, AName, AParent,
    visDefault, ASrcPos));

  if UseParentAsResultParent then
    ResultParent := AParent
  else
    ResultParent := Result;

  TPasFunctionType(Result).ResultEl :=
    TPasResultElement(CreateElement(TPasResultElement, AResultName, ResultParent,
    visDefault, ASrcPos));
end;

procedure TPasTreeContainer.FinishScope(ScopeType: TPasScopeType;
  El: TPasElement);
begin
  if ScopeType=stModule then ;
  if El=nil then ;
end;

function TPasTreeContainer.FindModule(const AName: String): TPasModule;
begin
  if AName='' then ;
  Result := nil;
end;

function TPasTreeContainer.NeedArrayValues(El: TPasElement): boolean;
begin
  Result:=false;
  if El=nil then ;
end;

{ ---------------------------------------------------------------------
  EParserError
  ---------------------------------------------------------------------}

constructor EParserError.Create(const AReason, AFilename: String;
  ARow, AColumn: Integer);
begin
  inherited Create(AReason);
  FFilename := AFilename;
  FRow := ARow;
  FColumn := AColumn;
end;

{ ---------------------------------------------------------------------
  TPasParser
  ---------------------------------------------------------------------}

procedure TPasParser.ParseExc(MsgNumber: integer; const Msg: String);
begin
  ParseExc(MsgNumber,Msg,[]);
end;

procedure TPasParser.ParseExc(MsgNumber: integer; const Fmt: String;
  Args: array of const);
begin
  {$IFDEF VerbosePasParser}
  writeln('TPasParser.ParseExc Token="',CurTokenText,'"');
  {$ENDIF}
  SetLastMsg(mtError,MsgNumber,Fmt,Args);
  raise EParserError.Create(SafeFormat(SParserErrorAtToken,
    [FLastMsg, CurTokenName, Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn])
    {$ifdef addlocation}+' ('+inttostr(scanner.currow)+' '+inttostr(scanner.curcolumn)+')'{$endif},
    Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn);
end;

procedure TPasParser.ParseExcExpectedIdentifier;
begin
  ParseExc(nParserExpectedIdentifier,SParserExpectedIdentifier);
end;

procedure TPasParser.ParseExcSyntaxError;
begin
  ParseExc(nParserSyntaxError,SParserSyntaxError);
end;

procedure TPasParser.ParseExcTokenError(const Arg: string);
begin
  ParseExc(nParserExpectTokenError,SParserExpectTokenError,[Arg]);
end;

constructor TPasParser.Create(AScanner: TPascalScanner;
  AFileResolver: TBaseFileResolver; AEngine: TPasTreeContainer);
begin
  inherited Create;
  FScanner := AScanner;
  FFileResolver := AFileResolver;
  FTokenRingCur:=High(FTokenRing);
  FEngine := AEngine;
  if Assigned(FEngine) then
    begin
    FEngine.CurrentParser:=Self;
    If FEngine.NeedComments then
      FScanner.SkipComments:=Not FEngine.NeedComments;
    end;
  FImplicitUses := TStringList.Create;
  FImplicitUses.Add('System'); // system always implicitely first.
end;

destructor TPasParser.Destroy;
var
  i: Integer;
begin
  if Assigned(FEngine) then
    begin
    FEngine.CurrentParser:=Nil;
    FEngine:=nil;
    end;
  FreeAndNil(FImplicitUses);
  for i:=low(FTokenRing) to high(FTokenRing) do
    FreeAndNil(FTokenRing[i].Comments);
  inherited Destroy;
end;

function TPasParser.CurTokenName: String;
begin
  if CurToken = tkIdentifier then
    Result := 'Identifier ' + FCurTokenString
  else
    Result := TokenInfos[CurToken];
end;

function TPasParser.CurTokenText: String;
begin
  case CurToken of
    tkIdentifier, tkString, tkNumber, tkChar:
      Result := FCurTokenString;
    else
      Result := TokenInfos[CurToken];
  end;
end;

function TPasParser.CurComments: TStrings;
begin
  if FTokenRingStart=FTokenRingEnd then
    Result:=nil
  else
    Result:=FTokenRing[FTokenRingCur].Comments;
end;

function TPasParser.CurSourcePos: TPasSourcePos;
begin
  if HasToken then
    Result:=FTokenRing[FTokenRingCur].SourcePos
  else
    begin
    if Scanner<>nil then
      Result:=Scanner.CurSourcePos
    else
      Result:=Default(TPasSourcePos);
    end;
end;

function TPasParser.HasToken: boolean;
begin
  if FTokenRingStart<FTokenRingEnd then
    Result:=(FTokenRingCur>=FTokenRingStart) and (FTokenRingCur<FTokenRingEnd)
  else
    Result:=(FTokenRingCur>=FTokenRingStart) or (FTokenRingCur<FTokenRingEnd);
end;

function TPasParser.SavedComments: String;
begin
  Result:=FSavedComments;
end;

procedure TPasParser.NextToken;

Var
  P: PTokenRec;
begin
  FTokenRingCur:=(FTokenRingCur+1) mod FTokenRingSize;
  P:=@FTokenRing[FTokenRingCur];
  if FTokenRingCur <> FTokenRingEnd then
    begin
    // Get token from buffer
    //writeln('TPasParser.NextToken REUSE Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
    FCurToken := P^.Token;
    FCurTokenString := P^.AsString;
    end
  else
    begin
    // Fetch new token
    //writeln('TPasParser.NextToken FETCH Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
    FTokenRingEnd:=(FTokenRingEnd+1) mod FTokenRingSize;
    if FTokenRingStart=FTokenRingEnd then
      FTokenRingStart:=(FTokenRingStart+1) mod FTokenRingSize;
    try
      if p^.Comments=nil then
        p^.Comments:=TStringList.Create
      else
        p^.Comments.Clear;
      repeat
        FCurToken := Scanner.FetchToken;
        if FCurToken=tkComment then
          p^.Comments.Add(Scanner.CurTokenString);
      until not (FCurToken in WhitespaceTokensToIgnore);
    except
      on e: EScannerError do
        begin
        if po_KeepScannerError in Options then
          raise
        else
          begin
          FLastMsgType := mtError;
          FLastMsgNumber := Scanner.LastMsgNumber;
          FLastMsgPattern := Scanner.LastMsgPattern;
          FLastMsg := Scanner.LastMsg;
          FLastMsgArgs := Scanner.LastMsgArgs;
          raise EParserError.Create(e.Message,
            Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn);
          end;
        end;
    end;
    p^.Token:=FCurToken;
    FCurTokenString := Scanner.CurTokenString;
    p^.AsString:=FCurTokenString;
    p^.SourcePos:=Scanner.CurSourcePos;
    end;
  //writeln('TPasParser.NextToken END Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
end;

procedure TPasParser.ChangeToken(tk: TToken);
var
  Cur, Last: PTokenRec;
  IsLast: Boolean;
begin
  //writeln('TPasParser.ChangeToken FTokenBufferSize=',FTokenRingStart,' FTokenBufferIndex=',FTokenRingCur);
  IsLast:=((FTokenRingCur+1) mod FTokenRingSize)=FTokenRingEnd;
  if (CurToken=tkshr) and (tk=tkGreaterThan) and IsLast then
    begin
    // change last token '>>' into two '>'
    Cur:=@FTokenRing[FTokenRingCur];
    Cur^.Token:=tkGreaterThan;
    Cur^.AsString:='>';
    Last:=@FTokenRing[FTokenRingEnd];
    Last^.Token:=tkGreaterThan;
    Last^.AsString:='>';
    if Last^.Comments<>nil then
      Last^.Comments.Clear;
    Last^.SourcePos:=Cur^.SourcePos;
    inc(Last^.SourcePos.Column);
    FTokenRingEnd:=(FTokenRingEnd+1) mod FTokenRingSize;
    if FTokenRingStart=FTokenRingEnd then
      FTokenRingStart:=(FTokenRingStart+1) mod FTokenRingSize;
    FCurToken:=tkGreaterThan;
    FCurTokenString:='>';
    end
  else
    CheckToken(tk);
end;

procedure TPasParser.UngetToken;

var
  P: PTokenRec;
begin
  //writeln('TPasParser.UngetToken START Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
  if FTokenRingStart = FTokenRingEnd then
    ParseExc(nParserUngetTokenError,SParserUngetTokenError);
  if FTokenRingCur>0 then
    dec(FTokenRingCur)
  else
    FTokenRingCur:=High(FTokenRing);
  P:=@FTokenRing[FTokenRingCur];
  FCurToken := P^.Token;
  FCurTokenString := P^.AsString;
  //writeln('TPasParser.UngetToken END Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
end;

procedure TPasParser.CheckToken(tk: TToken);
begin
  if (CurToken<>tk) then
    begin
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.ParseExcTokenError String="',CurTokenString,'" Text="',CurTokenText,'" CurToken=',CurToken,' tk=',tk);
    {$ENDIF}
    ParseExcTokenError(TokenInfos[tk]);
    end;
end;

procedure TPasParser.CheckTokens(tk: TTokens);

Var
  S : String;
  T : TToken;
begin
  if not (CurToken in tk) then
    begin
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.ParseExcTokenError String="',CurTokenString,'" Text="',CurTokenText,'" CurToken=',CurToken);
    {$ENDIF}
    S:='';
    For T in TToken do
      if t in tk then
        begin
        if (S<>'') then
          S:=S+' or ';
        S:=S+TokenInfos[t];
        end;
    ParseExcTokenError(S);
    end;

end;


procedure TPasParser.ExpectToken(tk: TToken);
begin
  NextToken;
  CheckToken(tk);
end;

procedure TPasParser.ExpectTokens(tk: TTokens);
begin
  NextToken;
  CheckTokens(tk);
end;

function TPasParser.ExpectIdentifier: String;
begin
  ExpectToken(tkIdentifier);
  Result := CurTokenString;
end;

function TPasParser.CurTokenIsIdentifier(const S: String): Boolean;
begin
  Result:=(Curtoken=tkIdentifier) and (CompareText(S,CurtokenText)=0);
end;


function TPasParser.IsCurTokenHint(out AHint: TPasMemberHint): Boolean;
begin
  Result:=CurToken=tklibrary;
  if Result then
    AHint:=hLibrary
  else if (CurToken=tkIdentifier) then
    Result:=IsHintToken(CurTokenString,ahint);
end;

function TPasParser.IsCurTokenHint: Boolean;
var
  dummy : TPasMemberHint;
begin
  Result:=IsCurTokenHint(dummy);
end;

function TPasParser.TokenIsCallingConvention(const S: String; out
  CC: TCallingConvention): Boolean;
begin
  Result:=IsCallingConvention(S,CC);
end;

function TPasParser.TokenIsProcedureModifier(Parent: TPasElement;
  const S: String; out PM: TProcedureModifier): Boolean;
begin
  Result:=IsProcModifier(S,PM);
  if Result and (PM in [pmPublic,pmForward]) then
    begin
    While (Parent<>Nil) and Not ((Parent is TPasClassType) or (Parent is TPasRecordType)) do
      Parent:=Parent.Parent;
    Result:=Not Assigned(Parent);
    end;
end;

function TPasParser.TokenIsProcedureTypeModifier(Parent: TPasElement;
  const S: String; out PTM: TProcTypeModifier): Boolean;
begin
  if CompareText(S,ProcTypeModifiers[ptmVarargs])=0 then
    begin
    Result:=true;
    PTM:=ptmVarargs;
    end
  else if CompareText(S,ProcTypeModifiers[ptmStatic])=0 then
    begin
    Result:=true;
    PTM:=ptmStatic;
    end
  else
   Result:=false;
  if Parent=nil then;
end;

function TPasParser.CheckHint(Element: TPasElement; ExpectSemiColon: Boolean
  ): TPasMemberHints;

Var
  Found : Boolean;
  h : TPasMemberHint;

begin
  Result:=[];
  Repeat
    NextToken;
    Found:=IsCurTokenHint(h);
    If Found then
      begin
      Include(Result,h);
      if (h=hDeprecated) then
        begin
        NextToken;
        if (Curtoken<>tkString) then
          UnGetToken
        else if assigned(Element) then
          Element.HintMessage:=CurTokenString;
        end;
      end;
  Until Not Found;
  UnGetToken;
  If Assigned(Element) then
    Element.Hints:=Result;
  if ExpectSemiColon then
    ExpectToken(tkSemiColon);
end;

function TPasParser.CheckPackMode: TPackMode;

begin
  NextToken;
  Case CurToken of
    tkPacked    : Result:=pmPacked;
    tkbitpacked : Result:=pmBitPacked;
  else
    result:=pmNone;
  end;
  if (Result<>pmNone) then
     begin
     NextToken;
     if Not (CurToken in [tkArray, tkRecord, tkObject, tkClass, tkSet]) then
       ParseExcTokenError('SET, ARRAY, RECORD, OBJECT or CLASS');
     end;
end;

Function IsSimpleTypeToken(Var AName : String) : Boolean;

Const
   SimpleTypeCount = 15;
   SimpleTypeNames : Array[1..SimpleTypeCount] of string =
     ('byte','boolean','char','integer','int64','longint','longword','double',
      'shortint','smallint','string','word','qword','cardinal','widechar');
   SimpleTypeCaseNames : Array[1..SimpleTypeCount] of string =
     ('Byte','Boolean','Char','Integer','Int64','LongInt','LongWord','Double',
     'ShortInt','SmallInt','String','Word','QWord','Cardinal','WideChar');

Var
  S : String;
  I : Integer;

begin
  S:=LowerCase(AName);
  I:=SimpleTypeCount;
  While (I>0) and (s<>SimpleTypeNames[i]) do
    Dec(I);
  Result:=(I>0);
  if Result Then
    AName:=SimpleTypeCaseNames[I];
end;

function TPasParser.ParseStringType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String): TPasAliasType;

Var
  LengthAsText : String;
  ok: Boolean;
  Params: TParamsExpr;
  LengthExpr: TPasExpr;

begin
  Result := TPasAliasType(CreateElement(TPasAliasType, TypeName, Parent, NamePos));
  ok:=false;
  try
    If (Result.Name='') then
      Result.Name:='string';
    Result.Expr:=CreatePrimitiveExpr(Result,pekIdent,TypeName);
    NextToken;
    LengthAsText:='';
    if CurToken=tkSquaredBraceOpen then
      begin
      Params:=TParamsExpr(CreateElement(TParamsExpr,'',Result));
      Params.Value:=Result.Expr;
      Result.Expr:=Params;
      LengthAsText:='';
      NextToken;
      LengthExpr:=DoParseExpression(Result,nil,false);
      Params.AddParam(LengthExpr);
      CheckToken(tkSquaredBraceClose);
      LengthAsText:=ExprToText(LengthExpr);
      end
    else
      UngetToken;
    Result.DestType:=TPasStringType(CreateElement(TPasStringType,'string',Parent));
    TPasStringType(Result.DestType).LengthExpr:=LengthAsText;
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

function TPasParser.ParseSimpleType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String; IsFull: Boolean
  ): TPasType;

Type
  TSimpleTypeKind = (stkAlias,stkString,stkRange,stkSpecialize);

Var
  Ref: TPasType;
  K : TSimpleTypeKind;
  Name : String;
  ST : TPasSpecializeType;
  Expr: TPasExpr;

begin
  Result:=nil;
  Name := CurTokenString;
  Expr:=nil;
  Ref:=nil;
  ST:=nil;
  try
    if IsFull then
      Expr:=CreatePrimitiveExpr(Parent,pekIdent,Name);
    NextToken;
    while CurToken=tkDot do
      begin
      ExpectIdentifier;
      Name := Name+'.'+CurTokenString;
      if IsFull then
        AddToBinaryExprChain(Expr,CreatePrimitiveExpr(Parent,pekIdent,CurTokenString),eopSubIdent);
      NextToken;
      end;

    // Current token is first token after identifier.
    if IsFull and (CurToken=tkSemicolon) or isCurTokenHint then // Type A = B;
      begin
      K:=stkAlias;
      UnGetToken; // ToDo: dotted identifier
      end
    else if IsFull and (CurToken=tkSquaredBraceOpen) then
      begin
      if LowerCase(Name)='string' then // Type A = String[12]; shortstring
        K:=stkString
      else
        ParseExcSyntaxError;
      UnGetToken; // ToDo: dotted identifier
      end
    else if (CurToken = tkLessThan) then // A = B<t>;
      begin
      K:=stkSpecialize;
      end
    else if (CurToken in [tkBraceOpen,tkDotDot]) then // A: B..C;
      begin
      K:=stkRange;
      UnGetToken; // ToDo: dotted identifier
      end
    else
      begin
      if IsFull then
        ParseExcTokenError(';');
      K:=stkAlias;
      if (not (po_resolvestandardtypes in Options)) and (LowerCase(Name)='string') then
        K:=stkString;
      UnGetToken; // ToDo: dotted identifier
      end;

    Case K of
      stkString:
        begin
        FreeAndNil(Expr);
        Result:=ParseStringType(Parent,NamePos,TypeName);
        end;
      stkSpecialize:
        begin
        ST := TPasSpecializeType(CreateElement(TPasSpecializeType, TypeName, Parent, CurSourcePos));
        Ref:=ResolveTypeReference(Name,ST);
        ReadSpecializeArguments(ST);
        ST.Expr:=Expr;
        ST.DestType:=Ref;
        Result:=ST;
        ST:=Nil;
        end;
      stkRange:
        begin
        FreeAndNil(Expr);
        UnGetToken; // move to '='
        Result:=ParseRangeType(Parent,NamePos,TypeName,False);
        end;
      stkAlias:
        begin
        Ref:=ResolveTypeReference(Name,Parent);
        if isFull then
          begin
          Result := TPasAliasType(CreateElement(TPasAliasType, TypeName, Parent, NamePos));
          TPasAliasType(Result).DestType:=Ref;
          TPasAliasType(Result).Expr:=Expr;
          end
        else
          Result:=Ref;
        end;
    end;
  finally
    if Result=nil then
      begin
      Expr.Free;
      ReleaseAndNil(TPasElement(Ref));
      ST.Free;
      end;
  end;
end;

// On entry, we're on the TYPE token
function TPasParser.ParseAliasType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String): TPasTypeAliasType;
var
  ok: Boolean;
begin
  Result := TPasTypeAliasType(CreateElement(TPasTypeAliasType, TypeName, Parent, NamePos));
  ok:=false;
  try
    Result.DestType := ParseType(Result,NamePos,'');
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

function TPasParser.ParseTypeReference(Parent: TPasElement; NeedExpr: boolean;
  out Expr: TPasExpr): TPasType;
// returns either
// a) TPasSpecializeType, Expr=nil
// b) TPasUnresolvedTypeRef, Expr<>nil
// c) TPasType, Expr<>nil
var
  Name: String;
  IsSpecialize: Boolean;
  ST: TPasSpecializeType;
begin
  Result:=nil;
  Expr:=nil;
  ST:=nil;
  try
    if not (msDelphi in CurrentModeswitches) and (CurToken=tkspecialize) then
      begin
      IsSpecialize:=true;
      NextToken;
      end
    else
      IsSpecialize:=false;
    // read dotted identifier
    CheckToken(tkIdentifier);
    Name:=ReadDottedIdentifier(Parent,Expr,true);
    // resolve type
    Result:=ResolveTypeReference(Name,Parent);

    if CurToken=tkLessThan then
      begin
      // specialize
      ST:=TPasSpecializeType(CreateElement(TPasSpecializeType,'',Parent));
      ST.DestType:=Result;
      Result:=nil;
      ST.Expr:=Expr;
      Expr:=nil;
      // read nested specialize arguments
      ReadSpecializeArguments(ST);
      Result:=ST;
      ST:=nil;
      NextToken;
      end
    else if IsSpecialize then
      CheckToken(tkLessThan)
    else
      begin
      // simple type reference
      if not NeedExpr then
        ReleaseAndNil(TPasElement(Expr));
      end;
  finally
    if ST<>nil then St.Release;
  end;
end;

function TPasParser.ParsePointerType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String): TPasPointerType;

var
  ok: Boolean;
begin
  Result := TPasPointerType(CreateElement(TPasPointerType, TypeName, Parent, NamePos));
  ok:=false;
  Try
    TPasPointerType(Result).DestType := ParseType(Result,CurSourcePos);
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

function TPasParser.ParseEnumType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String): TPasEnumType;

Var
  EnumValue: TPasEnumValue;
  ok: Boolean;

begin
  Result := TPasEnumType(CreateElement(TPasEnumType, TypeName, Parent, NamePos));
  ok:=false;
  try
    while True do
      begin
      NextToken;
      SaveComments;
      EnumValue := TPasEnumValue(CreateElement(TPasEnumValue, CurTokenString, Result));
      Result.Values.Add(EnumValue);
      NextToken;
      if CurToken = tkBraceClose then
        break
      else if CurToken in [tkEqual,tkAssign] then
        begin
        NextToken;
        EnumValue.Value:=DoParseExpression(Result);
       // UngetToken;
        if CurToken = tkBraceClose then
          Break
        else if not (CurToken=tkComma) then
          ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
        end
      else if not (CurToken=tkComma) then
        ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket)
      end;
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
  Engine.FinishScope(stTypeDef,Result);
end;

function TPasParser.ParseSetType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String; AIsPacked : Boolean = False): TPasSetType;

var
  ok: Boolean;
begin
  Result := TPasSetType(CreateElement(TPasSetType, TypeName, Parent, NamePos));
  Result.IsPacked:=AIsPacked;
  ok:=false;
  try
    ExpectToken(tkOf);
    Result.EnumType := ParseType(Result,CurSourcePos);
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
  Engine.FinishScope(stTypeDef,Result);
end;

function TPasParser.ParseType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String = ''; Full: Boolean = false; GenericArgs : TFPList = Nil
  ): TPasType;

Const
  // These types are allowed only when full type declarations
  FullTypeTokens = [tkGeneric,{tkSpecialize,}tkClass,tkInterface,tkDispInterface,tkType];
  // Parsing of these types already takes care of hints
  NoHintTokens = [tkProcedure,tkFunction];
var
  PM : TPackMode;
  CH , isHelper,ok: Boolean; // Check hint ?
begin
  Result := nil;
  // NextToken and check pack mode
  Pm:=CheckPackMode;
  if Full then
    CH:=Not (CurToken in NoHintTokens)
  else
    begin
    CH:=False;
    if (CurToken in FullTypeTokens) then
      ParseExc(nParserTypeNotAllowedHere,SParserTypeNotAllowedHere,[CurtokenText]);
    end;
  ok:=false;
  Try
    case CurToken of
      // types only allowed when full
      tkObject: Result := ParseClassDecl(Parent, NamePos, TypeName, okObject,PM);
      tkDispInterface:
        Result := ParseClassDecl(Parent, NamePos, TypeName, okDispInterface);
      tkInterface:
        Result := ParseClassDecl(Parent, NamePos, TypeName, okInterface);
      tkSpecialize: Result:=ParseSpecializeType(Parent,TypeName);
      tkClass: Result := ParseClassDecl(Parent, NamePos, TypeName, okClass, PM, GenericArgs);
      tkType:
        begin
        NextToken;
        isHelper:=CurTokenIsIdentifier('helper');
        UnGetToken;
        if isHelper then
          Result:=ParseClassDecl(Parent,NamePos,TypeName,okTypeHelper,PM)
        else
          Result:=ParseAliasType(Parent,NamePos,TypeName);
        end;
      // Always allowed
      tkIdentifier:
        begin
        // Bug 31709: PReference = ^Reference;
        // Checked in Delphi: ^Reference to procedure; is not allowed !!
        if CurTokenIsIdentifier('reference') and Not (Parent is TPasPointerType) then
          begin
          CH:=False;
          Result:=ParseReferencetoProcedureType(Parent,NamePos,TypeName)
          end
        else
          Result:=ParseSimpleType(Parent,NamePos,TypeName,Full);
        end;
      tkCaret: Result:=ParsePointerType(Parent,NamePos,TypeName);
      tkFile: Result:=ParseFileType(Parent,NamePos,TypeName);
      tkArray: Result:=ParseArrayType(Parent,NamePos,TypeName,pm);
      tkBraceOpen: Result:=ParseEnumType(Parent,NamePos,TypeName);
      tkSet: Result:=ParseSetType(Parent,NamePos,TypeName,pm=pmPacked);
      tkProcedure: Result:=ParseProcedureType(Parent,NamePos,TypeName,ptProcedure);
      tkFunction: Result:=ParseProcedureType(Parent,NamePos,TypeName,ptFunction);
      tkRecord:
        begin
        NextToken;
        if CurTokenIsIdentifier('Helper') then
          begin
          UnGetToken;
          Result:=ParseClassDecl(Parent,NamePos,TypeName,okRecordHelper,PM);
          end
        else
          begin
          UnGetToken;
          Result := ParseRecordDecl(Parent,NamePos,TypeName,PM);
          end;
        end;
      tkNumber,tkMinus,tkChar:
        begin
        UngetToken;
        Result:=ParseRangeType(Parent,NamePos,TypeName,Full);
        end;
    else
      ParseExcExpectedIdentifier;
    end;
    if CH then
      CheckHint(Result,True);
    ok:=true;
  finally
    if not ok then
      if Result<>nil then
        Result.Release;
  end;
end;

function TPasParser.ParseReferenceToProcedureType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: String
  ): TPasProcedureType;
begin
  if not CurTokenIsIdentifier('reference') then
    ParseExcTokenError('reference');
  ExpectToken(tkTo);
  NextToken;
  Case CurToken of
   tkprocedure : Result:=ParseProcedureType(Parent,NamePos,TypeName,ptProcedure);
   tkfunction : Result:=ParseProcedureType(Parent,NamePos,TypeName,ptFunction);
  else
    ParseExcTokenError('procedure or function');
  end;
  Result.IsReferenceTo:=True;
end;

function TPasParser.ParseComplexType(Parent : TPasElement = Nil): TPasType;
begin
  NextToken;
  case CurToken of
    tkProcedure:
      begin
        Result := TPasProcedureType(CreateElement(TPasProcedureType, '', Parent));
        ParseProcedureOrFunctionHeader(Result, TPasProcedureType(Result), ptProcedure, True);
        if CurToken = tkSemicolon then
          UngetToken;        // Unget semicolon
      end;
    tkFunction:
      begin
        Result := CreateFunctionType('', 'Result', Parent, False, CurSourcePos);
        ParseProcedureOrFunctionHeader(Result, TPasFunctionType(Result), ptFunction, True);
        if CurToken = tkSemicolon then
          UngetToken;        // Unget semicolon
      end;
  else
    UngetToken;
    Result := ParseType(Parent,CurSourcePos);
  end;
end;

function TPasParser.ParseArrayType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String; PackMode: TPackMode
  ): TPasArrayType;

Var
  S : String;
  ok: Boolean;
  RangeExpr: TPasExpr;

begin
  Result := TPasArrayType(CreateElement(TPasArrayType, TypeName, Parent, NamePos));
  ok:=false;
  try
    Result.PackMode:=PackMode;
    NextToken;
    S:='';
    case CurToken of
      tkSquaredBraceOpen:
        begin
          repeat
            NextToken;
            if po_arrayrangeexpr in Options then
              begin
              RangeExpr:=DoParseExpression(Result);
              Result.AddRange(RangeExpr);
              end
            else if CurToken<>tkSquaredBraceClose then
               S:=S+CurTokenText;
            if CurToken=tkSquaredBraceClose then
              break
            else if CurToken=tkComma then
              continue
            else if po_arrayrangeexpr in Options then
              ParseExcTokenError(']');
          until false;
          Result.IndexRange:=S;
          ExpectToken(tkOf);
          Result.ElType := ParseType(Result,CurSourcePos);
        end;
      tkOf:
        begin
          NextToken;
          if CurToken = tkConst then
          else
          begin
            UngetToken;
            Result.ElType := ParseType(Result,CurSourcePos);
          end
        end
      else
        ParseExc(nParserArrayTypeSyntaxError,SParserArrayTypeSyntaxError);
    end;
    // TPasProcedureType parsing has eaten the semicolon;
    // We know it was a local definition if the array def (result) is the parent
    if (Result.ElType is TPasProcedureType) and (Result.ElType.Parent=Result) then
      UnGetToken;
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
  Engine.FinishScope(stTypeDef,Result);
end;

function TPasParser.ParseFileType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String): TPasFileType;
begin
  Result:=TPasFileType(CreateElement(TPasFileType, TypeName, Parent, NamePos));
  NextToken;
  If CurToken=tkOf then
    Result.ElType := ParseType(Result,CurSourcePos)
  else
   UngetToken;
end;

function TPasParser.isEndOfExp(AllowEqual : Boolean = False; CheckHints : Boolean = True):Boolean;
const
  EndExprToken = [
    tkEOF, tkBraceClose, tkSquaredBraceClose, tkSemicolon, tkComma, tkColon,
    tkdo, tkdownto, tkelse, tkend, tkof, tkthen, tkto
  ];
begin
  Result:=(CurToken in EndExprToken) or (CheckHints and IsCurTokenHint);
  if Not (Result or AllowEqual) then
    Result:=(Curtoken=tkEqual);
end;

function TPasParser.ExprToText(Expr: TPasExpr): String;
var
  C: TClass;
begin
  C:=Expr.ClassType;
  if C=TPrimitiveExpr then
    Result:=TPrimitiveExpr(Expr).Value
  else if C=TSelfExpr then
    Result:='self'
  else if C=TBoolConstExpr then
    Result:=BoolToStr(TBoolConstExpr(Expr).Value,'true','false')
  else if C=TNilExpr then
    Result:='nil'
  else if C=TInheritedExpr then
    Result:='inherited'
  else if C=TUnaryExpr then
    Result:=OpcodeStrings[TUnaryExpr(Expr).OpCode]+ExprToText(TUnaryExpr(Expr).Operand)
  else if C=TBinaryExpr then
    begin
    Result:=ExprToText(TBinaryExpr(Expr).left);
    if OpcodeStrings[TBinaryExpr(Expr).OpCode]<>'' then
      Result:=Result+OpcodeStrings[TBinaryExpr(Expr).OpCode]
    else
      Result:=Result+' ';
    Result:=Result+ExprToText(TBinaryExpr(Expr).right)
    end
  else if C=TParamsExpr then
    begin
    case TParamsExpr(Expr).Kind of
      pekArrayParams: Result:=ExprToText(TParamsExpr(Expr).Value)
        +'['+ArrayExprToText(TParamsExpr(Expr).Params)+']';
      pekFuncParams: Result:=ExprToText(TParamsExpr(Expr).Value)
        +'('+ArrayExprToText(TParamsExpr(Expr).Params)+')';
      pekSet: Result:='['+ArrayExprToText(TParamsExpr(Expr).Params)+']';
      else ParseExc(nErrUnknownOperatorType,SErrUnknownOperatorType,[ExprKindNames[TParamsExpr(Expr).Kind]]);
    end;
    end
  else
    ParseExc(nErrUnknownOperatorType,SErrUnknownOperatorType,['TPasParser.ExprToText: '+Expr.ClassName]);
end;

function TPasParser.ArrayExprToText(Expr: TPasExprArray): String;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to length(Expr)-1 do
    begin
    if i>0 then
      Result:=Result+',';
    Result:=Result+ExprToText(Expr[i]);
    end;
end;

function TPasParser.ResolveTypeReference(Name: string; Parent: TPasElement): TPasType;
var
  SS: Boolean;
  Ref: TPasElement;
begin
  Ref:=Nil;
  SS:=(not (po_resolvestandardtypes in FOptions)) and isSimpleTypeToken(Name);
  if not SS then
    begin
    Ref:=Engine.FindElement(Name);
    if Ref=nil then
      begin
      {$IFDEF VerbosePasResolver}
      if po_resolvestandardtypes in FOptions then
        begin
        writeln('ERROR: TPasParser.ParseSimpleType resolver failed to raise an error');
        ParseExcExpectedIdentifier;
        end;
      {$ENDIF}
      end
    else if not (Ref is TPasType) then
      ParseExc(nParserExpectedTypeButGot,SParserExpectedTypeButGot,[Ref.ElementTypeName]);
    end;
  if (Ref=Nil) then
    Result:=TPasUnresolvedTypeRef(CreateElement(TPasUnresolvedTypeRef,Name,Parent))
  else
    begin
    Ref.AddRef;
    Result:=TPasType(Ref);
    end;
end;

function TPasParser.ParseParams(AParent: TPasElement; paramskind: TPasExprKind;
  AllowFormatting: Boolean = False): TParamsExpr;
var
  params  : TParamsExpr;
  p       : TPasExpr;
  PClose  : TToken;

begin
  Result:=nil;
  if paramskind in [pekArrayParams, pekSet] then
    begin
    if CurToken<>tkSquaredBraceOpen then
      ParseExc(nParserExpectTokenError,SParserExpectTokenError,['[']);
    PClose:=tkSquaredBraceClose;
    end
  else
    begin
    if CurToken<>tkBraceOpen then
      ParseExc(nParserExpectTokenError,SParserExpectTokenError,['(']);
    PClose:=tkBraceClose;
    end;

  params:=TParamsExpr(CreateElement(TParamsExpr,'',AParent));
  try
    params.Kind:=paramskind;
    NextToken;
    if not isEndOfExp(false,false) then
      begin
      repeat
        p:=DoParseExpression(params);
        if not Assigned(p) then
          ParseExcSyntaxError;
        params.AddParam(p);
        if (CurToken=tkColon) then
          if Not AllowFormatting then
            ParseExc(nParserExpectTokenError,SParserExpectTokenError,[','])
          else
            begin
            NextToken;
            p.format1:=DoParseExpression(p);
            if (CurToken=tkColon) then
              begin
              NextToken;
              p.format2:=DoParseExpression(p);
              end;
            end;
        if not (CurToken in [tkComma, PClose]) then
          ParseExc(nParserExpectTokenError,SParserExpectTokenError,[',']);

        if CurToken = tkComma then
          begin
          NextToken;
          if CurToken = PClose then
            begin
            //ErrorExpected(parser, 'identifier');
            ParseExcSyntaxError;
            end;
          end;
      until CurToken=PClose;
      end;
    NextToken;
    Result:=params;
  finally
    if not Assigned(Result) then params.Release;
  end;
end;

function TPasParser.TokenToExprOp(AToken: TToken): TExprOpCode;

begin
  Case AToken of
    tkMul                   : Result:=eopMultiply;
    tkPlus                  : Result:=eopAdd;
    tkMinus                 : Result:=eopSubtract;
    tkDivision              : Result:=eopDivide;
    tkLessThan              : Result:=eopLessThan;
    tkEqual                 : Result:=eopEqual;
    tkGreaterThan           : Result:=eopGreaterThan;
    tkAt                    : Result:=eopAddress;
    tkNotEqual              : Result:=eopNotEqual;
    tkLessEqualThan         : Result:=eopLessthanEqual;
    tkGreaterEqualThan      : Result:=eopGreaterThanEqual;
    tkPower                 : Result:=eopPower;
    tkSymmetricalDifference : Result:=eopSymmetricalDifference;
    tkIs                    : Result:=eopIs;
    tkAs                    : Result:=eopAs;
    tkSHR                   : Result:=eopSHR;
    tkSHL                   : Result:=eopSHL;
    tkAnd                   : Result:=eopAnd;
    tkOr                    : Result:=eopOR;
    tkXor                   : Result:=eopXOR;
    tkMod                   : Result:=eopMod;
    tkDiv                   : Result:=eopDiv;
    tkNot                   : Result:=eopNot;
    tkIn                    : Result:=eopIn;
    tkDot                   : Result:=eopSubIdent;
    tkCaret                 : Result:=eopDeref;
  else
    ParseExc(nParserNotAnOperand,SParserNotAnOperand,[AToken,TokenInfos[AToken]]);
  end;
end;

function TPasParser.ParseExpIdent(AParent: TPasElement): TPasExpr;

  Function IsWriteOrStr(P : TPasExpr) : boolean;

  Var
    N : String;
  begin
    Result:=P is TPrimitiveExpr;
    if Result then
      begin
      N:=LowerCase(TPrimitiveExpr(P).Value);
      // We should actually resolve this to system.NNN
      Result:=(N='write') or (N='str') or (N='writeln');
      end;
  end;

  Procedure HandleSelf(Var Last: TPasExpr);

  Var
    b       : TBinaryExpr;
    optk    : TToken;

  begin
    NextToken;
    if CurToken = tkDot then
      begin // self.Write(EscapeText(AText));
      optk:=CurToken;
      NextToken;
      b:=CreateBinaryExpr(AParent,Last, ParseExpIdent(AParent), TokenToExprOp(optk));
      if not Assigned(b.right) then
        begin
        b.Release;
        ParseExcExpectedIdentifier;
        end;
      Last:=b;
      end;
    UngetToken;
  end;

  function IsSpecialize: boolean;
  var
    LookAhead, i: Integer;

    function Next: boolean;
    begin
      if LookAhead=FTokenRingSize then exit(false);
      NextToken;
      inc(LookAhead);
      Result:=true;
    end;

  begin
    Result:=false;
    LookAhead:=0;
    CheckToken(tkLessThan);
    try
      Next;
      if not (CurToken in [tkIdentifier,tkself]) then exit;
      while Next do
        case CurToken of
        tkDot:
          begin
          if not Next then exit;
          if not (CurToken in [tkIdentifier,tkself,tktrue,tkfalse]) then exit;
          end;
        tkComma:
          begin
          if not Next then exit;
          if not (CurToken in [tkIdentifier,tkself]) then exit;
          end;
        tkLessThan:
          begin
          // e.g. A<B<
          // not a valid comparison, could be a specialization -> good enough
          exit(true);
          end;
        tkGreaterThan:
          begin
          // e.g. A<B>
          exit(true);
          end;
        else
          exit;
        end;
    finally
      for i:=1 to LookAhead do
        UngetToken;
    end;
  end;

var
  Last,func, Expr: TPasExpr;
  prm     : TParamsExpr;
  b       : TBinaryExpr;
  ok, CanSpecialize: Boolean;
  aName: String;
  ISE: TInlineSpecializeExpr;
  ST: TPasSpecializeType;

begin
  Result:=nil;
  CanSpecialize:=false;
  aName:='';
  case CurToken of
    tkString:           Last:=CreatePrimitiveExpr(AParent,pekString,CurTokenString);
    tkChar:             Last:=CreatePrimitiveExpr(AParent,pekString,CurTokenText);
    tkNumber:           Last:=CreatePrimitiveExpr(AParent,pekNumber,CurTokenString);
    tkIdentifier:
      begin
      CanSpecialize:=true;
      aName:=CurTokenText;
      if CompareText(aName,'self')=0 then
        begin
        Last:=CreateSelfExpr(AParent);
        HandleSelf(Last);
        end
      else
        Last:=CreatePrimitiveExpr(AParent,pekIdent,aName);
      end;
    tkfalse, tktrue:    Last:=CreateBoolConstExpr(Aparent,pekBoolConst, CurToken=tktrue);
    tknil:              Last:=CreateNilExpr(AParent);
    tkSquaredBraceOpen: Last:=ParseParams(AParent,pekSet);
    tkinherited:
      begin
      //inherited; inherited function
      Last:=CreateInheritedExpr(AParent);
      NextToken;
      if (CurToken=tkIdentifier) then
        begin
        b:=CreateBinaryExpr(AParent,Last, DoParseExpression(AParent), eopNone);
        if not Assigned(b.right) then
          begin
          b.Release;
          ParseExcExpectedIdentifier;
          end;
        Last:=b;
        end;
      UngetToken;
      end;
    tkself:
      begin
      CanSpecialize:=true;
      aName:=CurTokenText;
      Last:=CreateSelfExpr(AParent);
      HandleSelf(Last);
      end;
    tkAt:
      begin
      // is this still needed?
      // P:=@function;
      NextToken;
      if (length(CurTokenText)=0) or not (CurTokenText[1] in ['A'..'_']) then
        begin
        UngetToken;
        ParseExcExpectedIdentifier;
        end;
      Last:=CreatePrimitiveExpr(AParent,pekString, '@'+CurTokenText);
      end;
    tkCaret:
      begin
      // is this still needed?
      // ^A..^_ characters. See #16341
      NextToken;
      if not (length(CurTokenText)=1) or not (CurTokenText[1] in ['A'..'_']) then
        begin
        UngetToken;
        ParseExcExpectedIdentifier;
        end;
      Last:=CreatePrimitiveExpr(AParent,pekString, '^'+CurTokenText);
      end;
  else
    ParseExcExpectedIdentifier;
  end;

  Result:=Last;
  func:=Last;
  
  if Last.Kind<>pekSet then NextToken;
  if not (Last.Kind in [pekIdent,pekSelf,pekNil]) then
    exit;

  ok:=false;
  ISE:=nil;
  try
    repeat
      case CurToken of
      tkDot:
        begin
        NextToken;
        if CurToken in [tkIdentifier,tktrue,tkfalse,tkself] then // true and false are sub identifiers as well
          begin
          aName:=aName+'.'+CurTokenString;
          expr:=CreatePrimitiveExpr(AParent,pekIdent,CurTokenString);
          AddToBinaryExprChain(Result,expr,eopSubIdent);
          func:=expr;
          NextToken;
          end
        else
          begin
          UngetToken;
          ParseExcExpectedIdentifier;
          end;
        end;
      tkBraceOpen,tkSquaredBraceOpen:
        begin
        if CurToken=tkBraceOpen then
          prm:=ParseParams(AParent,pekFuncParams,IsWriteOrStr(func))
        else
          prm:=ParseParams(AParent,pekArrayParams);
        if not Assigned(prm) then Exit;
        AddParamsToBinaryExprChain(Result,prm);
        CanSpecialize:=false;
        end;
      tkCaret:
        begin
        Result:=CreateUnaryExpr(AParent,Result,TokenToExprOp(CurToken));
        NextToken;
        CanSpecialize:=false;
        end;
      tkLessThan:
        if (not CanSpecialize) or not IsSpecialize then
          break
        else
          begin
          // an inline specialization (e.g. A<B,C>)
          ISE:=TInlineSpecializeExpr(CreateElement(TInlineSpecializeExpr,'',AParent));
          ST:=TPasSpecializeType(CreateElement(TPasSpecializeType,'',ISE));
          ISE.DestType:=ST;
          ReadSpecializeArguments(ST);
          ST.DestType:=ResolveTypeReference(aName,ST);
          ST.Expr:=Result;
          Result:=ISE;
          ISE:=nil;
          CanSpecialize:=false;
          NextToken;
          end;
      else
        break;
      end;
    until false;
    ok:=true;
  finally
    if not ok then
      begin
      Result.Release;
      ISE.Free;
      end;
  end;
end;

function TPasParser.OpLevel(t: TToken): Integer;
begin
  case t of
  //  tkDot:
  //    Result:=5;
    tknot,tkAt:
      Result:=4;
    tkMul, tkDivision, tkdiv, tkmod, tkand, tkShl,tkShr, tkas, tkPower :
      Result:=3;
    tkPlus, tkMinus, tkor, tkxor:
      Result:=2;
    tkEqual, tkNotEqual, tkLessThan, tkLessEqualThan, tkGreaterThan, tkGreaterEqualThan, tkin, tkis:
      Result:=1;
  else
    Result:=0;
  end;
end;

function TPasParser.DoParseExpression(AParent : TPaselement;InitExpr: TPasExpr; AllowEqual : Boolean = True): TPasExpr;
var
  expstack  : TFPList;
  opstack   : array of TToken;
  opstackTop: integer;
  pcount    : Integer;
  x         : TPasExpr;
  i         : Integer;
  tempop    : TToken;
  NotBinary : Boolean;

const
  PrefixSym = [tkPlus, tkMinus, tknot, tkAt]; // + - not @
  BinaryOP  = [tkMul, tkDivision, tkdiv, tkmod,  tkDotDot,
               tkand, tkShl,tkShr, tkas, tkPower,
               tkPlus, tkMinus, tkor, tkxor, tkSymmetricalDifference,
               tkEqual, tkNotEqual, tkLessThan, tkLessEqualThan,
               tkGreaterThan, tkGreaterEqualThan, tkin, tkis];

  function PopExp: TPasExpr; inline;
  begin
    if expstack.Count>0 then begin
      Result:=TPasExpr(expstack[expstack.Count-1]);
      expstack.Delete(expstack.Count-1);
    end else
      Result:=nil;
  end;

  procedure PushOper(token: TToken); inline;
  begin
    inc(opstackTop);
    if opstackTop=length(opstack) then
      SetLength(opstack,length(opstack)*2+4);
    opstack[opstackTop]:=token;
  end;

  function PeekOper: TToken; inline;
  begin
    if opstackTop>=0 then Result:=opstack[opstackTop]
    else Result:=tkEOF;
  end;

  function PopOper: TToken; inline;
  begin
    Result:=PeekOper;
    if Result<>tkEOF then dec(opstackTop);
  end;

  procedure PopAndPushOperator;
  var
    t       : TToken;
    xright  : TPasExpr;
    xleft   : TPasExpr;
    bin     : TBinaryExpr;
  begin
    t:=PopOper;
    xright:=PopExp;
    xleft:=PopExp;
    if t=tkDotDot then
      begin
      bin:=CreateBinaryExpr(AParent,xleft,xright,eopNone);
      bin.Kind:=pekRange;
      end
    else
      bin:=CreateBinaryExpr(AParent,xleft,xright,TokenToExprOp(t));
    expstack.Add(bin);
  end;

Var
  AllowedBinaryOps : Set of TToken;

begin
  AllowedBinaryOps:=BinaryOP;
  if Not AllowEqual then
    Exclude(AllowedBinaryOps,tkEqual);
  //DumpCurToken('Entry',iaIndent);
  Result:=nil;
  expstack := TFPList.Create;
  SetLength(opstack,4);
  opstackTop:=-1;
  try
    repeat
      NotBinary:=True;
      pcount:=0;
      if not Assigned(InitExpr) then
        begin
        // the first part of the expression has been parsed externally.
        // this is used by Constant Expression parser (CEP) parsing only,
        // whenever it makes a false assuming on constant expression type.
        // i.e: SI_PAD_SIZE = ((128/sizeof(longint)) - 3);
        //
        // CEP assumes that it's array or record, because the expression
        // starts with "(". After the first part is parsed, the CEP meets "-"
        // that assures, it's not an array expression. The CEP should give the
        // first part back to the expression parser, to get the correct
        // token tree according to the operations priority.
        //
        // quite ugly. type information is required for CEP to work clean

        while CurToken in PrefixSym do
          begin
          PushOper(CurToken);
          inc(pcount);
          NextToken;
          end;

        if (CurToken = tkBraceOpen) then
          begin
          NextToken;
          x:=DoParseExpression(AParent);
          if (CurToken<>tkBraceClose) then
            begin
            x.Release;
            CheckToken(tkBraceClose);
            end;
          NextToken;
          // for expressions like (ppdouble)^^;
          while (x<>Nil) and (CurToken=tkCaret) do
            begin
            NextToken;
            x:=CreateUnaryExpr(AParent,x, TokenToExprOp(tkCaret));
            end;
          // for expressions like (TObject(m)).Free;
          if (x<>Nil) and (CurToken=tkDot) then
            begin
            NextToken;
            x:=CreateBinaryExpr(AParent,x, ParseExpIdent(AParent), TokenToExprOp(tkDot));
            end;
          // for expressions like (PChar(a)+10)[0];
          if (x<>Nil) and (CurToken=tkSquaredBraceOpen) then
            begin
            x:=ParseParams(x,pekArrayParams,False);
            end;
          end
        else
          begin
          x:=ParseExpIdent(AParent);
          end;
        if not Assigned(x) then
          ParseExcSyntaxError;
        expstack.Add(x);

        for i:=1 to pcount do
          begin
          tempop:=PopOper;
          x:=popexp;
          if (tempop=tkMinus) and (x.Kind=pekRange) then
            begin
            TBinaryExpr(x).Left:=CreateUnaryExpr(x, TBinaryExpr(x).left, eopSubtract);
            expstack.Add(x);
            end
          else
            expstack.Add(CreateUnaryExpr(AParent, x, TokenToExprOp(tempop) ));
          end;
        end
      else
        begin
        expstack.Add(InitExpr);
        InitExpr:=nil;
        end;
      if (CurToken in AllowedBinaryOPs) then
        begin
        // Adjusting order of the operations
        NotBinary:=False;
        tempop:=PeekOper;
        while (opstackTop>=0) and (OpLevel(tempop)>=OpLevel(CurToken)) do begin
          PopAndPushOperator;
          tempop:=PeekOper;
        end;
        PushOper(CurToken);
        NextToken;
        end;
       //Writeln('Bin ',NotBinary ,' or EOE ',isEndOfExp, ' Ex ',Assigned(x),' stack ',ExpStack.Count);
    until NotBinary or isEndOfExp(AllowEqual, NotBinary);

    if not NotBinary then ParseExcExpectedIdentifier;

    while opstackTop>=0 do PopAndPushOperator;

    // only 1 expression should be on the stack, at the end of the correct expression
    if expstack.Count<>1 then
      ParseExcSyntaxError;
    if expstack.Count=1 then
      begin
      Result:=TPasExpr(expstack[0]);
      Result.Parent:=AParent;
      end;

  finally
    {if Not Assigned(Result) then
      DumpCurToken('Exiting (no result)',iaUndent)
    else
      DumpCurtoken('Exiting (Result: "'+Result.GetDeclaration(true)+'") ',iaUndent);}
    if not Assigned(Result) then begin
      // expression error!
      for i:=0 to expstack.Count-1 do
        TPasExpr(expstack[i]).Release;
    end;
    SetLength(opstack,0);
    expstack.Free;
  end;
end;


function GetExprIdent(p: TPasExpr): String;
begin
  Result:='';
  if not Assigned(p) then exit;
  if (p.ClassType=TPrimitiveExpr) and (p.Kind=pekIdent) then
    Result:=TPrimitiveExpr(p).Value
  else if (p.ClassType=TSelfExpr) then
    Result:='Self';
end;

function TPasParser.DoParseConstValueExpression(AParent: TPasElement): TPasExpr;

  function lastfield:boolean;

  begin
    result:= CurToken<>tkSemicolon;
    if not result then
     begin
       nexttoken;
       if curtoken=tkbraceclose then
         result:=true
       else
         ungettoken;
     end;
  end;

  procedure ReadArrayValues(x : TPasExpr);
  var
    a: TArrayValues;
  begin
    Result:=nil;
    a:=nil;
    try
      a:=CreateArrayValues(AParent);
      if x<>nil then
        begin
        a.AddValues(x);
        x:=nil;
        end;
      repeat
        NextToken;
        a.AddValues(DoParseConstValueExpression(AParent));
      until CurToken<>tkComma;
      Result:=a;
    finally
      if Result=nil then
        begin
        a.Free;
        x.Free;
        end;
    end;
  end;

var
  x : TPasExpr;
  n : AnsiString;
  r : TRecordValues;
begin
  if CurToken <> tkBraceOpen then
    Result:=DoParseExpression(AParent)
  else begin
    Result:=nil;
    if Engine.NeedArrayValues(AParent) then
      ReadArrayValues(nil)
    else
      begin
      NextToken;
      x:=DoParseConstValueExpression(AParent);
      case CurToken of
        tkComma: // array of values (a,b,c);
          ReadArrayValues(x);

        tkColon: // record field (a:xxx;b:yyy;c:zzz);
          begin
            r:=nil;
            try
              n:=GetExprIdent(x);
              ReleaseAndNil(TPasElement(x));
              r:=CreateRecordValues(AParent);
              NextToken;
              x:=DoParseConstValueExpression(AParent);
              r.AddField(n, x);
              x:=nil;
              if not lastfield then
                repeat
                  n:=ExpectIdentifier;
                  ExpectToken(tkColon);
                  NextToken;
                  x:=DoParseConstValueExpression(AParent);
                  r.AddField(n, x);
                  x:=nil;
                until lastfield; // CurToken<>tkSemicolon;
              Result:=r;
            finally
              if Result=nil then
                begin
                r.Free;
                x.Free;
                end;
            end;
          end;
      else
        // Binary expression!  ((128 div sizeof(longint)) - 3);
        Result:=DoParseExpression(AParent,x);
        if CurToken<>tkBraceClose then
          begin
          ReleaseAndNil(TPasElement(Result));
          ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
          end;
        NextToken;
        if CurToken <> tkSemicolon then // the continue of expression
          Result:=DoParseExpression(AParent,Result);
        Exit;
      end;
      end;
    if CurToken<>tkBraceClose then
      begin
      ReleaseAndNil(TPasElement(Result));
      ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
      end;
    NextToken;
  end;
end;

function TPasParser.CheckOverloadList(AList: TFPList; AName: String; out
  OldMember: TPasElement): TPasOverloadedProc;

Var
  I : Integer;

begin
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<AList.Count) do
    begin
    OldMember:=TPasElement(AList[i]);
    if CompareText(OldMember.Name, AName) = 0 then
      begin
      if OldMember is TPasOverloadedProc then
        Result:=TPasOverloadedProc(OldMember)
      else
        begin
        Result:=TPasOverloadedProc(CreateElement(TPasOverloadedProc, AName, OldMember.Parent));
        Result.Visibility:=OldMember.Visibility;
        Result.Overloads.Add(OldMember);
        Result.SourceFilename:=OldMember.SourceFilename;
        Result.SourceLinenumber:=OldMember.SourceLinenumber;
        Result.DocComment:=Oldmember.DocComment;
        AList[i] := Result;
        end;
      end;
    Inc(I);
    end;
  If Result=Nil then
    OldMember:=Nil;
end;

procedure TPasParser.AddProcOrFunction(Decs: TPasDeclarations;
  AProc: TPasProcedure);
var
  I : Integer;
  OldMember: TPasElement;
  OverloadedProc: TPasOverloadedProc;
begin
  With Decs do
    begin
    if not (po_nooverloadedprocs in Options) then
      OverloadedProc:=CheckOverloadList(Functions,AProc.Name,OldMember)
    else
      OverloadedProc:=nil;
    If (OverloadedProc<>Nil) then
      begin
      OverLoadedProc.Overloads.Add(AProc);
      if (OldMember<>OverloadedProc) then
        begin
        I:=Declarations.IndexOf(OldMember);
        If I<>-1 then
          Declarations[i]:=OverloadedProc;
        end;
      end
    else
      begin
      Declarations.Add(AProc);
      Functions.Add(AProc);
      end;
    end;
end;

// Return the parent of a function declaration. This is AParent,
// except when AParent is a class, and the function is overloaded.
// Then the parent is the overload object.
function TPasParser.CheckIfOverloaded(AParent: TPasElement; const AName: String): TPasElement;
var
  Member: TPasElement;
  OverloadedProc: TPasOverloadedProc;

begin
  Result:=AParent;
  If (not (po_nooverloadedprocs in Options)) and (AParent is TPasClassType) then
    begin
    OverloadedProc:=CheckOverLoadList(TPasClassType(AParent).Members,AName,Member);
    If (OverloadedProc<>Nil) then
      Result:=OverloadedProc;
    end;
end;


procedure TPasParser.ParseMain(var Module: TPasModule);
begin
  Module:=nil;
  NextToken;
  SaveComments;
  case CurToken of
    tkUnit:
      ParseUnit(Module);
    tkProgram:
      ParseProgram(Module);
    tkLibrary:
      ParseLibrary(Module);
  else
    UngetToken;
    ParseProgram(Module,True);
  //    ParseExcTokenError('unit');
  end;
end;

// Starts after the "unit" token
procedure TPasParser.ParseUnit(var Module: TPasModule);
var
  AUnitName: String;
begin
  Module := nil;
  AUnitName := ExpectIdentifier;
  NextToken;
  while CurToken = tkDot do
    begin
    ExpectIdentifier;
    AUnitName := AUnitName + '.' + CurTokenString;
    NextToken;
    end;
  UngetToken;
  Module := TPasModule(CreateElement(TPasModule, AUnitName,
    Engine.Package));
  FCurModule:=Module;
  try
    if Assigned(Engine.Package) then
      begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
      Module.AddRef;
      end;
    CheckHint(Module,True);
//    ExpectToken(tkSemicolon);
    ExpectToken(tkInterface);
    If LogEvent(pleInterface) then
      DoLog(mtInfo,nLogStartInterface,SLogStartInterface);
    ParseInterface;
    Engine.FinishScope(stModule,Module);
  finally
    FCurModule:=nil;
  end;
end;

// Starts after the "program" token
procedure TPasParser.ParseProgram(var Module: TPasModule; SkipHeader : Boolean = False);

Var
  PP : TPasProgram;
  Section : TProgramSection;
  N : String;

begin
  if SkipHeader then
    N:=ChangeFileExt(Scanner.CurFilename,'')
  else
    begin
    N:=ExpectIdentifier;
    NextToken;
    while CurToken = tkDot do
      begin
      ExpectIdentifier;
      N := N + '.' + CurTokenString;
      NextToken;
      end;
    UngetToken;
    end;
  Module := nil;
  PP:=TPasProgram(CreateElement(TPasProgram, N, Engine.Package));
  Module :=PP;
  FCurModule:=Module;
  try
    if Assigned(Engine.Package) then
    begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
    end;
    if not SkipHeader then
      begin
      NextToken;
      If (CurToken=tkBraceOpen) then
        begin
        PP.InputFile:=ExpectIdentifier;
        NextToken;
        if Not (CurToken in [tkBraceClose,tkComma]) then
          ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
        If (CurToken=tkComma) then
          PP.OutPutFile:=ExpectIdentifier;
        ExpectToken(tkBraceClose);
        NextToken;
        end;
      if (CurToken<>tkSemicolon) then
        ParseExcTokenError(';');
      end;
    Section := TProgramSection(CreateElement(TProgramSection, '', CurModule));
    PP.ProgramSection := Section;
    ParseOptionalUsesList(Section);
    ParseDeclarations(Section);
    Engine.FinishScope(stModule,Module);
  finally
    FCurModule:=nil;
  end;
end;

// Starts after the "library" token
procedure TPasParser.ParseLibrary(var Module: TPasModule);
Var
  PP : TPasLibrary;
  Section : TLibrarySection;
  N: String;

begin
  N:=ExpectIdentifier;
  NextToken;
  while CurToken = tkDot do
    begin
    ExpectIdentifier;
    N := N + '.' + CurTokenString;
    NextToken;
    end;
  UngetToken;
  Module := nil;
  PP:=TPasLibrary(CreateElement(TPasLibrary, N, Engine.Package));
  Module :=PP;
  FCurModule:=Module;
  try
    if Assigned(Engine.Package) then
    begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
    end;
    NextToken;
    if (CurToken<>tkSemicolon) then
        ParseExcTokenError(';');
    Section := TLibrarySection(CreateElement(TLibrarySection, '', CurModule));
    PP.LibrarySection := Section;
    ParseOptionalUsesList(Section);
    ParseDeclarations(Section);
    Engine.FinishScope(stModule,Module);
  finally
    FCurModule:=nil;
  end;
end;

procedure TPasParser.ParseOptionalUsesList(ASection: TPasSection);
// checks if next token is Uses keyword and reads the uses list
begin
  NextToken;
  if CurToken=tkuses then
    ParseUsesList(ASection)
  else begin
    CheckImplicitUsedUnits(ASection);
    Engine.FinishScope(stUsesClause,ASection);
    UngetToken;
  end;
end;

// Starts after the "interface" token
procedure TPasParser.ParseInterface;
var
  Section: TInterfaceSection;
begin
  Section := TInterfaceSection(CreateElement(TInterfaceSection, '', CurModule));
  CurModule.InterfaceSection := Section;
  ParseOptionalUsesList(Section);
  ParseDeclarations(Section); // this also parses the Implementation section
end;

// Starts after the "implementation" token
procedure TPasParser.ParseImplementation;
var
  Section: TImplementationSection;
begin
  Section := TImplementationSection(CreateElement(TImplementationSection, '', CurModule));
  CurModule.ImplementationSection := Section;
  ParseOptionalUsesList(Section);
  ParseDeclarations(Section);
end;

procedure TPasParser.ParseInitialization;
var
  Section: TInitializationSection;
  SubBlock: TPasImplElement;
begin
  Section := TInitializationSection(CreateElement(TInitializationSection, '', CurModule));
  CurModule.InitializationSection := Section;
  repeat
    NextToken;
    if (CurToken=tkend) then
    begin
      ExpectToken(tkDot);
      exit;
    end
    else if (CurToken=tkfinalization) then
    begin
      ParseFinalization;
      exit;
    end
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(Section,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
end;

procedure TPasParser.ParseFinalization;
var
  Section: TFinalizationSection;
  SubBlock: TPasImplElement;
begin
  Section := TFinalizationSection(CreateElement(TFinalizationSection, '', CurModule));
  CurModule.FinalizationSection := Section;
  repeat
    NextToken;
    if (CurToken=tkend) then
    begin
      ExpectToken(tkDot);
      exit;
    end
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(Section,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
  UngetToken;
end;

function TPasParser.GetProcTypeFromToken(tk: TToken; IsClass: Boolean
  ): TProcType;

begin
  Case tk of
    tkProcedure :
      if IsClass then
        Result:=ptClassProcedure
      else
        Result:=ptProcedure;
    tkFunction:
      if IsClass then
        Result:=ptClassFunction
      else
        Result:=ptFunction;
    tkConstructor:
      if IsClass then
        Result:=ptClassConstructor
      else
        Result:=ptConstructor;
    tkDestructor:
      if IsClass then
        Result:=ptClassDestructor
      else
        Result:=ptDestructor;
    tkOperator:
      if IsClass then
        Result:=ptClassOperator
      else
        Result:=ptOperator;
  else
    ParseExc(nParserNotAProcToken,SParserNotAProcToken);
  end;
end;

procedure TPasParser.ParseDeclarations(Declarations: TPasDeclarations);
var
  CurBlock: TDeclType;

  procedure SetBlock(NewBlock: TDeclType);
  begin
    if CurBlock=NewBlock then exit;
    if CurBlock=declType then
      Engine.FinishScope(stTypeSection,Declarations);
    CurBlock:=NewBlock;
    Scanner.SetForceCaret(NewBlock=declType);
  end;

var
  ConstEl: TPasConst;
  ResStrEl: TPasResString;
  TypeEl: TPasType;
  ClassEl: TPasClassType;
  ArrEl : TPasArrayType;
  List: TFPList;
  i,j: Integer;
  VarEl: TPasVariable;
  ExpEl: TPasExportSymbol;
  PropEl : TPasProperty;
  TypeName: String;
  PT : TProcType;
  NamePos: TPasSourcePos;
  ok: Boolean;
  Proc: TPasProcedure;
  RecordEl: TPasRecordType;

begin
  CurBlock := declNone;
  while True do
  begin
    NextToken;
  //  writeln('TPasParser.ParseSection Token=',CurTokenString,' ',CurToken, ' ',scanner.CurFilename);
    case CurToken of
      tkend:
        begin
        If (CurModule is TPasProgram) and (CurModule.InitializationSection=Nil) then
          ParseExcTokenError('begin');
        ExpectToken(tkDot);
        break;
        end;
      tkimplementation:
        if (Declarations is TInterfaceSection) then
          begin
          If Not Engine.InterfaceOnly then
            begin
            If LogEvent(pleImplementation) then
              DoLog(mtInfo,nLogStartImplementation,SLogStartImplementation);
            SetBlock(declNone);
            ParseImplementation;
            end;
          break;
          end;
      tkinitialization:
        if (Declarations is TInterfaceSection)
        or ((Declarations is TImplementationSection) and not (Declarations is TProgramSection)) then
          begin
          SetBlock(declNone);
          ParseInitialization;
          break;
          end;
      tkfinalization:
        if (Declarations is TInterfaceSection)
        or ((Declarations is TImplementationSection) and not (Declarations is TProgramSection)) then
          begin
          SetBlock(declNone);
          ParseFinalization;
          break;
          end;
      tkUses:
        if Declarations.ClassType=TInterfaceSection then
          ParseExcTokenError(TokenInfos[tkimplementation])
        else if Declarations is TPasSection then
          ParseExcTokenError(TokenInfos[tkend])
        else
          ParseExcSyntaxError;
      tkConst:
        SetBlock(declConst);
      tkexports:
        SetBlock(declExports);
      tkResourcestring:
        SetBlock(declResourcestring);
      tkType:
        SetBlock(declType);
      tkVar:
        SetBlock(declVar);
      tkThreadVar:
        SetBlock(declThreadVar);
      tkProperty:
        SetBlock(declProperty);
      tkProcedure, tkFunction, tkConstructor, tkDestructor,tkOperator:
        begin
        SetBlock(declNone);
        SaveComments;
        pt:=GetProcTypeFromToken(CurToken);
        AddProcOrFunction(Declarations, ParseProcedureOrFunctionDecl(Declarations, pt));
        end;
      tkClass:
        begin
          SetBlock(declNone);
          SaveComments;
          NextToken;
          If CurToken in [tkprocedure,tkFunction,tkConstructor, tkDestructor] then
            begin
            pt:=GetProcTypeFromToken(CurToken,True);
            AddProcOrFunction(Declarations,ParseProcedureOrFunctionDecl(Declarations, pt));
            end
          else
            ExpectToken(tkprocedure);
        end;
      tkIdentifier:
        begin
          SaveComments;
          case CurBlock of
            declConst:
              begin
                ConstEl := ParseConstDecl(Declarations);
                Declarations.Declarations.Add(ConstEl);
                Declarations.Consts.Add(ConstEl);
              end;
            declResourcestring:
              begin
                ResStrEl := ParseResourcestringDecl(Declarations);
                Declarations.Declarations.Add(ResStrEl);
                Declarations.ResStrings.Add(ResStrEl);
              end;
            declType:
              begin
              TypeEl := ParseTypeDecl(Declarations);
              // Scanner.SetForceCaret(OldForceCaret); // It may have been switched off
              if Assigned(TypeEl) then        // !!!
                begin
                  Declarations.Declarations.Add(TypeEl);
                  if (TypeEl.ClassType = TPasClassType)
                      and (not (po_keepclassforward in Options)) then
                  begin
                    // Remove previous forward declarations, if necessary
                    for i := 0 to Declarations.Classes.Count - 1 do
                    begin
                      ClassEl := TPasClassType(Declarations.Classes[i]);
                      if CompareText(ClassEl.Name, TypeEl.Name) = 0 then
                      begin
                        Declarations.Classes.Delete(i);
                        for j := 0 to Declarations.Declarations.Count - 1 do
                          if CompareText(TypeEl.Name,
                            TPasElement(Declarations.Declarations[j]).Name) = 0 then
                          begin
                            Declarations.Declarations.Delete(j);
                            break;
                          end;
                        ClassEl.Release;
                        break;
                      end;
                    end;
                    // Add the new class to the class list
                    Declarations.Classes.Add(TypeEl)
                  end else
                    Declarations.Types.Add(TypeEl);
                end;
              end;
            declExports:
              begin
              List := TFPList.Create;
              try
                ok:=false;
                try
                  ParseExportDecl(Declarations, List);
                  ok:=true;
                finally
                  if not ok then
                    for i := 0 to List.Count - 1 do
                      TPasExportSymbol(List[i]).Release;
                end;
                for i := 0 to List.Count - 1 do
                begin
                  ExpEl := TPasExportSymbol(List[i]);
                  Declarations.Declarations.Add(ExpEl);
                  Declarations.ExportSymbols.Add(ExpEl);
                end;
              finally
                List.Free;
              end;
              end;
            declVar, declThreadVar:
              begin
                List := TFPList.Create;
                try
                  ParseVarDecl(Declarations, List);
                  for i := 0 to List.Count - 1 do
                  begin
                    VarEl := TPasVariable(List[i]);
                    Engine.FinishScope(stDeclaration,VarEl);
                    Declarations.Declarations.Add(VarEl);
                    Declarations.Variables.Add(VarEl);
                  end;
                  CheckToken(tkSemicolon);
                finally
                  List.Free;
                end;
              end;
            declProperty:
              begin
              PropEl:=ParseProperty(Declarations,CurtokenString,visDefault,false);
              Declarations.Declarations.Add(PropEl);
              Declarations.Properties.Add(PropEl);
              end;
          else
            ParseExcSyntaxError;
          end;
        end;
      tkGeneric:
        begin
          if CurBlock <> declType then
            ParseExcSyntaxError;
          TypeName := ExpectIdentifier;
          NamePos:=CurSourcePos;
          List:=TFPList.Create;
          try
            ReadGenericArguments(List,Nil);
            ExpectToken(tkEqual);
            NextToken;
            Case CurToken of
              tkObject,
              tkClass :
                 begin
                 ClassEl := TPasClassType(CreateElement(TPasClassType,
                   TypeName, Declarations, NamePos));
                 ClassEl.SetGenericTemplates(List);
                 NextToken;
                 DoParseClassType(ClassEl);
                 Declarations.Declarations.Add(ClassEl);
                 Declarations.Classes.Add(ClassEl);
                 CheckHint(classel,True);
                 Engine.FinishScope(stTypeDef,ClassEl);
                 end;
              tkRecord:
                begin
                RecordEl := TPasRecordType(CreateElement(TPasRecordType,
                  TypeName, Declarations, NamePos));
                RecordEl.SetGenericTemplates(List);
                NextToken;
                ParseRecordFieldList(RecordEl,tkend,true);
                Declarations.Declarations.Add(RecordEl);
                Declarations.Classes.Add(RecordEl);
                CheckHint(RecordEl,True);
                Engine.FinishScope(stTypeDef,RecordEl);
                end;
              tkArray:
                 begin
                 if List.Count<>1 then
                   ParseExc(nParserGenericArray1Element,sParserGenericArray1Element);
                 ArrEl:=TPasArrayType(ParseArrayType(Declarations,NamePos,TypeName,pmNone));
                 CheckHint(ArrEl,True);
                 ArrEl.ElType.Release;
                 ArrEl.ElType:=TPasGenericTemplateType(List[0]);
                 Declarations.Declarations.Add(ArrEl);
                 Declarations.Types.Add(ArrEl);
                 Engine.FinishScope(stTypeDef,ArrEl);
                 end;
            else
              ParseExc(nParserGenericClassOrArray,SParserGenericClassOrArray);
            end;
          finally
            List.Free;
          end;
        end;
      tkbegin:
        begin
        if Declarations is TProcedureBody then
          begin
          Proc:=Declarations.Parent as TPasProcedure;
          if pmAssembler in Proc.Modifiers then
            ParseExc(nParserExpectTokenError,SParserExpectTokenError,['asm']);
          SetBlock(declNone);
          ParseProcBeginBlock(TProcedureBody(Declarations));
          break;
          end
        else if (Declarations is TInterfaceSection)
        or (Declarations is TImplementationSection) then
          begin
          SetBlock(declNone);
          ParseInitialization;
          break;
          end
        else
          ParseExcSyntaxError;
        end;
      tkasm:
        begin
        if Declarations is TProcedureBody then
          begin
          Proc:=Declarations.Parent as TPasProcedure;
          // Assembler keyword is optional in Delphi mode (bug 31690)
          if not ((pmAssembler in Proc.Modifiers) or (msDelphi in CurrentModeswitches)) then
            ParseExc(nParserExpectTokenError,SParserExpectTokenError,['begin']);
          SetBlock(declNone);
          ParseProcAsmBlock(TProcedureBody(Declarations));
          break;
          end
        else
          ParseExcSyntaxError;
        end;
      tklabel:
        begin
          SetBlock(declNone);
          if not (Declarations is TInterfaceSection) then
            ParseLabels(Declarations);
        end;
    else
      ParseExcSyntaxError;
    end;
  end;
  SetBlock(declNone);
end;

function TPasParser.AddUseUnit(ASection: TPasSection;
  const NamePos: TPasSourcePos; AUnitName: string; NameExpr: TPasExpr;
  InFileExpr: TPrimitiveExpr): TPasElement;

  procedure CheckDuplicateInUsesList(AUnitName : string; UsesClause: TPasUsesClause);
  var
    i: Integer;
  begin
    if UsesClause=nil then exit;
    for i:=0 to length(UsesClause)-1 do
      if CompareText(AUnitName,UsesClause[i].Name)=0 then
        ParseExc(nParserDuplicateIdentifier,SParserDuplicateIdentifier,[AUnitName]);
  end;

var
  UnitRef: TPasElement;
  UsesUnit: TPasUsesUnit;
begin
  Result:=nil;
  UsesUnit:=nil;
  try
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.AddUseUnit AUnitName=',AUnitName,' CurModule.Name=',CurModule.Name);
    {$ENDIF}
    if CompareText(AUnitName,CurModule.Name)=0 then
      begin
      if CompareText(AUnitName,'System')=0 then
        exit; // for compatibility ignore implicit use of system in system
      ParseExc(nParserDuplicateIdentifier,SParserDuplicateIdentifier,[AUnitName]);
      end;
    CheckDuplicateInUsesList(AUnitName,ASection.UsesClause);
    if ASection.ClassType=TImplementationSection then
      CheckDuplicateInUsesList(AUnitName,CurModule.InterfaceSection.UsesClause);

    UnitRef := Engine.FindModule(AUnitName);  // should we resolve module here when "IN" filename is not known yet?
    if Assigned(UnitRef) then
      UnitRef.AddRef
    else
      UnitRef := TPasUnresolvedUnitRef(CreateElement(TPasUnresolvedUnitRef,
        AUnitName, ASection));

    UsesUnit:=TPasUsesUnit(CreateElement(TPasUsesUnit,AUnitName,ASection,NamePos));
    Result:=ASection.AddUnitToUsesList(AUnitName,NameExpr,InFileExpr,UnitRef,UsesUnit);
    if InFileExpr<>nil then
      begin
      if UnitRef is TPasModule then
        begin
        if TPasModule(UnitRef).Filename='' then
          TPasModule(UnitRef).Filename:=InFileExpr.Value;
        end
      else if UnitRef is TPasUnresolvedUnitRef then
        TPasUnresolvedUnitRef(UnitRef).FileName:=InFileExpr.Value;
      end;
  finally
    if Result=nil then
      begin
      if UsesUnit<>nil then
        UsesUnit.Release;
      if NameExpr<>nil then
        NameExpr.Release;
      if InFileExpr<>nil then
        InFileExpr.Release;
      end;
  end;
end;

procedure TPasParser.CheckImplicitUsedUnits(ASection: TPasSection);
var
  i: Integer;
  NamePos: TPasSourcePos;
begin
  If not (ASection.ClassType=TImplementationSection) Then // interface,program,library,package
    begin
    // load implicit units, like 'System'
    NamePos:=CurSourcePos;
    for i:=0 to ImplicitUses.Count-1 do
      AddUseUnit(ASection,NamePos,ImplicitUses[i],nil,nil);
    end;
end;

// Starts after the "uses" token
procedure TPasParser.ParseUsesList(ASection: TPasSection);
var
  AUnitName, aName: String;
  NameExpr: TPasExpr;
  InFileExpr: TPrimitiveExpr;
  FreeExpr: Boolean;
  NamePos: TPasSourcePos;
begin
  CheckImplicitUsedUnits(ASection);

  NameExpr:=nil;
  InFileExpr:=nil;
  FreeExpr:=true;
  try
    Repeat
      FreeExpr:=true;
      AUnitName := ExpectIdentifier;
      NamePos:=CurSourcePos;
      NameExpr:=CreatePrimitiveExpr(ASection,pekString,AUnitName);
      NextToken;
      while CurToken = tkDot do
      begin
        ExpectIdentifier;
        aName:=CurTokenString;
        AUnitName := AUnitName + '.' + aName;
        AddToBinaryExprChain(NameExpr,CreatePrimitiveExpr(ASection,pekString,aName),eopSubIdent);
        NextToken;
      end;
      if (CurToken=tkin) then
        begin
        ExpectToken(tkString);
        InFileExpr:=CreatePrimitiveExpr(ASection,pekString,CurTokenString);
        NextToken;
        end;
      FreeExpr:=false;
      AddUseUnit(ASection,NamePos,AUnitName,NameExpr,InFileExpr);
      InFileExpr:=nil;
      NameExpr:=nil;

      if Not (CurToken in [tkComma,tkSemicolon]) then
        ParseExc(nParserExpectedCommaSemicolon,SParserExpectedCommaSemicolon);
    Until (CurToken=tkSemicolon);
  finally
    if FreeExpr then
      begin
      NameExpr.Release;
      InFileExpr.Release;
      end;
  end;

  Engine.FinishScope(stUsesClause,ASection);
end;

// Starts after the variable name
function TPasParser.ParseConstDecl(Parent: TPasElement): TPasConst;

var
  OldForceCaret,ok: Boolean;

begin
  SaveComments;
  Result := TPasConst(CreateElement(TPasConst, CurTokenString, Parent));
  if Parent is TPasClassType then
    Include(Result.VarModifiers,vmClass);
  ok:=false;
  try
    NextToken;
    if CurToken = tkColon then
      begin
      OldForceCaret:=Scanner.SetForceCaret(True);
      try
        Result.VarType := ParseType(Result,CurSourcePos);
      finally
        Scanner.SetForceCaret(OldForceCaret);
      end;
{      if Result.VarType is TPasRangeType then
        Ungettoken; // Range type stops on token after last range token}
      end
    else
      UngetToken;
    ExpectToken(tkEqual);
    NextToken;
    Result.Expr:=DoParseConstValueExpression(Result);
    UngetToken;
    CheckHint(Result,True);
    ok:=true;
  finally
    if not ok then
      ReleaseAndNil(TPasElement(Result));
  end;
  Engine.FinishScope(stConstDef,Result);
end;

// Starts after the variable name
function TPasParser.ParseResourcestringDecl(Parent: TPasElement): TPasResString;
var
  ok: Boolean;
begin
  SaveComments;
  Result := TPasResString(CreateElement(TPasResString, CurTokenString, Parent));
  ok:=false;
  try
    ExpectToken(tkEqual);
    NextToken; // skip tkEqual
    Result.Expr:=DoParseConstValueExpression(Result);
    UngetToken;
    CheckHint(Result,True);
    ok:=true;
  finally
    if not ok then
      ReleaseAndNil(TPasElement(Result));
  end;
end;

procedure TPasParser.ReadGenericArguments(List : TFPList;Parent : TPasElement);

Var
  N : String;

begin
  ExpectToken(tkLessThan);
  repeat
    N:=ExpectIdentifier;
    List.Add(CreateElement(TPasGenericTemplateType,N,Parent));
    NextToken;
    if not (CurToken in [tkComma, tkGreaterThan]) then
      ParseExc(nParserExpectToken2Error,SParserExpectToken2Error,
        [TokenInfos[tkComma], TokenInfos[tkGreaterThan]]);
  until CurToken = tkGreaterThan;
end;

procedure TPasParser.ReadSpecializeArguments(Spec: TPasSpecializeType);

Var
  Name : String;
  Ref: TPasType;
  IsNested: Boolean;
  NestedSpec: TPasSpecializeType;
  Expr: TPasExpr;

begin
  CheckToken(tkLessThan);
  NextToken;
  Expr:=nil;
  Ref:=nil;
  NestedSpec:=nil;
  try
    repeat
      if not (msDelphi in CurrentModeswitches) and (CurToken=tkspecialize) then
        begin
        IsNested:=true;
        NextToken;
        end
      else
        IsNested:=false;
      // read dotted identifier
      CheckToken(tkIdentifier);
      Expr:=nil;
      Name:=ReadDottedIdentifier(Spec,Expr,true);

      if CurToken=tkLessThan then
        begin
        // nested specialize
        // resolve type
        Ref:=ResolveTypeReference(Name,Spec);
        // create nested specialize
        NestedSpec:=TPasSpecializeType(CreateElement(TPasSpecializeType,'',Spec));
        NestedSpec.DestType:=Ref;
        Ref:=nil;
        NestedSpec.Expr:=Expr;
        Expr:=nil;
        // read nested specialize arguments
        ReadSpecializeArguments(NestedSpec);
        // add nested specialize
        Spec.AddParam(NestedSpec);
        NestedSpec:=nil;
        NextToken;
        end
      else if IsNested then
        CheckToken(tkLessThan)
      else
        begin
        // simple type reference
        Spec.AddParam(Expr);
        Expr:=nil;
        end;

      if CurToken=tkComma then
        begin
        NextToken;
        continue;
        end
      else if CurToken=tkshr then
        begin
        ChangeToken(tkGreaterThan);
        break;
        end
      else if CurToken=tkGreaterThan then
        break
      else
        ParseExc(nParserExpectToken2Error,SParserExpectToken2Error,
          [TokenInfos[tkComma], TokenInfos[tkGreaterThan]]);
    until false;
  finally
    Expr.Free;
    if Ref<>nil then Ref.Release;
    if NestedSpec<>nil then NestedSpec.Release;
  end;
end;

function TPasParser.ReadDottedIdentifier(Parent: TPasElement; out
  Expr: TPasExpr; NeedAsString: boolean): String;
begin
  Expr:=nil;
  if NeedAsString then
    Result := CurTokenString
  else
    Result:='';
  CheckToken(tkIdentifier);
  Expr:=CreatePrimitiveExpr(Parent,pekIdent,Result);
  NextToken;
  while CurToken=tkDot do
    begin
    ExpectIdentifier;
    if NeedAsString then
      Result := Result+'.'+CurTokenString;
    AddToBinaryExprChain(Expr,CreatePrimitiveExpr(Parent,pekIdent,CurTokenString),eopSubIdent);
    NextToken;
    end;
end;

// Starts after the type name
function TPasParser.ParseRangeType(AParent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String; Full: Boolean
  ): TPasRangeType;

Var
  PE : TPasExpr;
  ok: Boolean;

begin
  Result := TPasRangeType(CreateElement(TPasRangeType, TypeName, AParent, NamePos));
  ok:=false;
  try
    if Full then
      begin
      If not (CurToken=tkEqual) then
        ParseExcTokenError(TokenInfos[tkEqual]);
      end;
    NextToken;
    PE:=DoParseExpression(Result,Nil,False);
    if not ((PE is TBinaryExpr) and (TBinaryExpr(PE).Kind=pekRange)) then
      begin
      PE.Release;
      ParseExc(nRangeExpressionExpected,SRangeExpressionExpected);
      end;
    Result.RangeExpr:=PE as TBinaryExpr;
    UngetToken;
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
  Engine.FinishScope(stTypeDef,Result);
end;

// Starts after Exports, on first identifier.
procedure TPasParser.ParseExportDecl(Parent: TPasElement; List: TFPList);
Var
  E : TPasExportSymbol;
begin
  Repeat
    if List.Count<>0 then
      ExpectIdentifier;
    E:=TPasExportSymbol(CreateElement(TPasExportSymbol,CurtokenString,Parent));
    List.Add(E);
    NextToken;
    if CurTokenIsIdentifier('INDEX') then
      begin
      NextToken;
      E.Exportindex:=DoParseExpression(E,Nil)
      end
    else if CurTokenIsIdentifier('NAME') then
      begin
      NextToken;
      E.ExportName:=DoParseExpression(E,Nil)
      end;
    if not (CurToken in [tkComma,tkSemicolon]) then
      ParseExc(nParserExpectedCommaSemicolon,SParserExpectedCommaSemicolon);
  until (CurToken=tkSemicolon);
end;

function TPasParser.ParseSpecializeType(Parent: TPasElement;
  const TypeName: String): TPasSpecializeType;

begin
  NextToken;
  Result:=ParseSimpleType(Parent,CurSourcePos,TypeName) as TPasSpecializeType;
end;

function TPasParser.ParseProcedureType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: String; const PT: TProcType
  ): TPasProcedureType;

var
  ok: Boolean;
begin
  if PT in [ptFunction,ptClassFunction] then
    Result := CreateFunctionType(TypeName, 'Result', Parent, False, NamePos)
  else
    Result := TPasProcedureType(CreateElement(TPasProcedureType, TypeName, Parent, NamePos));
  ok:=false;
  try
    ParseProcedureOrFunctionHeader(Result, TPasProcedureType(Result), PT, True);
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

function TPasParser.ParseTypeDecl(Parent: TPasElement): TPasType;

var
  TypeName: String;
  NamePos: TPasSourcePos;
  OldForceCaret : Boolean;
  List : TFPList;

begin
  TypeName := CurTokenString;
  NamePos:=CurSourcePos;
  List:=Nil;
  OldForceCaret:=Scanner.SetForceCaret(True);
  try
    NextToken;
    if (CurToken=tkLessThan) and (msDelphi in CurrentModeswitches) then
      List:=TFPList.Create;
    UnGetToken; // ReadGenericArguments starts at <
    if Assigned(List) then
      ReadGenericArguments(List,Parent);
    ExpectToken(tkEqual);
    Result:=ParseType(Parent,NamePos,TypeName,True,List);
  finally
    Scanner.SetForceCaret(OldForceCaret);
    List.Free;
  end;
end;

function TPasParser.GetVariableValueAndLocation(Parent: TPasElement; out
  Value: TPasExpr; out Location: String): Boolean;

begin
  Value:=Nil;
  NextToken;
  Result:=CurToken=tkEqual;
  if Result then
    begin
    NextToken;
    Value := DoParseConstValueExpression(Parent);
//    NextToken;
    end;
  if (CurToken=tkAbsolute) then
    begin
    Result:=True;
    ExpectIdentifier;
    Location:=CurTokenText;
    NextToken;
    While CurToken=tkDot do
      begin
      ExpectIdentifier;
      Location:=Location+'.'+CurTokenText;
      NextToken;
      end;
    UnGetToken;
    end
  else
    UngetToken;
end;

function TPasParser.GetVariableModifiers(Parent: TPasElement; out
  VarMods: TVariableModifiers; out LibName, ExportName: TPasExpr;
  ExternalClass: Boolean): string;

Var
  S : String;
  ExtMod: TVariableModifier;
begin
  Result := '';
  LibName := nil;
  ExportName := nil;
  VarMods := [];
  NextToken;
  If CurTokenIsIdentifier('cvar') and not ExternalClass then
    begin
    Result:=';cvar';
    Include(VarMods,vmcvar);
    ExpectToken(tkSemicolon);
    NextToken;
    end;
  s:=LowerCase(CurTokenText);
  if s='external' then
    ExtMod:=vmExternal
  else if (s='public') and not externalclass then
    ExtMod:=vmPublic
  else if (s='export') and not externalclass then
    ExtMod:=vmExport
  else
    begin
    UngetToken;
    exit;
    end;
  Include(varMods,ExtMod);
  Result:=Result+';'+CurTokenText;

  NextToken;
  if not (CurToken in [tkString,tkIdentifier]) then
    begin
    if (CurToken=tkSemicolon) and (ExtMod in [vmExternal,vmPublic]) then
      exit;
    ParseExcSyntaxError;
    end;
  // export name exportname;
  // public;
  // public name exportname;
  // external;
  // external libname;
  // external libname name exportname;
  // external name exportname;
  if (ExtMod=vmExternal) and (CurToken in [tkString,tkIdentifier])
      and Not (CurTokenIsIdentifier('name')) and not ExternalClass then
    begin
    Result := Result + ' ' + CurTokenText;
    LibName:=DoParseExpression(Parent);
    end;
  if not CurTokenIsIdentifier('name') then
    ParseExcSyntaxError;
  NextToken;
  if not (CurToken in [tkChar,tkString,tkIdentifier]) then
    ParseExcTokenError(TokenInfos[tkString]);
  Result := Result + ' ' + CurTokenText;
  ExportName:=DoParseExpression(Parent);
end;


// Full means that a full variable declaration is being parsed.
procedure TPasParser.ParseVarList(Parent: TPasElement; VarList: TFPList;
  AVisibility: TPasMemberVisibility; Full : Boolean);
// on Exception the VarList is restored, no need to Release the new elements

var
  i, OldListCount: Integer;
  Value , aLibName, aExpName: TPasExpr;
  VarType: TPasType;
  VarEl: TPasVariable;
  H : TPasMemberHints;
  VarMods: TVariableModifiers;
  D,Mods,Loc: string;
  OldForceCaret,ok,ExternalClass: Boolean;

begin
  Value:=Nil;
  aLibName:=nil;
  aExpName:=nil;
  OldListCount:=VarList.Count;
  ok:=false;
  try
    D:=SaveComments; // This means we support only one comment per 'list'.
    VarEl:=nil;
    Repeat
      // create the TPasVariable here, so that SourceLineNumber is correct
      VarEl:=TPasVariable(CreateElement(TPasVariable,CurTokenString,Parent,AVisibility));
      VarList.Add(VarEl);
      NextToken;
      if Not (CurToken in [tkComma,tkColon]) then
        ParseExc(nParserExpectedCommaColon,SParserExpectedCommaColon);
      if CurToken=tkComma then
        ExpectIdentifier;
    Until (CurToken=tkColon);
    OldForceCaret:=Scanner.SetForceCaret(True);
    try
      VarType := ParseComplexType(VarEl);
    finally
      Scanner.SetForceCaret(OldForceCaret);
    end;
    // read type
    for i := OldListCount to VarList.Count - 1 do
      begin
      VarEl:=TPasVariable(VarList[i]);
      // Writeln(VarEl.Name, AVisibility);
      VarEl.VarType := VarType;
      //VarType.Parent := VarEl; // this is wrong for references
      if (i>OldListCount) then
        VarType.AddRef;
      end;

    H:=CheckHint(Nil,False);
    If Full then
      GetVariableValueAndLocation(Parent,Value,Loc);
    if (Value<>nil) and (VarList.Count>OldListCount+1) then
      ParseExc(nParserOnlyOneVariableCanBeInitialized,SParserOnlyOneVariableCanBeInitialized);
    TPasVariable(VarList[OldListCount]).Expr:=Value;
    Value:=nil;

    // Note: external members are allowed for non external classes too
    ExternalClass:=(msExternalClass in CurrentModeSwitches)
                    and (Parent is TPasClassType);

    H:=H+CheckHint(Nil,False);
    if Full or Externalclass then
      begin
      NextToken;
      If Curtoken<>tkSemicolon then
        UnGetToken;
      Mods:=GetVariableModifiers(Parent,VarMods,aLibName,aExpName,ExternalClass);
      if (mods='') and (CurToken<>tkSemicolon) then
        NextToken;
      end
    else
      begin
      NextToken;
      VarMods:=[];
      Mods:='';
      end;
    SaveComments(D);

    // connect
    for i := OldListCount to VarList.Count - 1 do
      begin
      VarEl:=TPasVariable(VarList[i]);
      // Writeln(VarEl.Name, AVisibility);
      // Procedure declaration eats the hints.
      if Assigned(VarType) and (VarType is TPasProcedureType) then
        VarEl.Hints:=VarType.Hints
      else
        VarEl.Hints:=H;
      VarEl.Modifiers:=Mods;
      VarEl.VarModifiers:=VarMods;
      VarEl.AbsoluteLocation:=Loc;
      if aLibName<>nil then
        begin
        VarEl.LibraryName:=aLibName;
        aLibName.AddRef;
        end;
      if aExpName<>nil then
        begin
        VarEl.ExportName:=aExpName;
        aExpName.AddRef;
        end;
      end;
    ok:=true;
  finally
    if aLibName<>nil then aLibName.Release;
    if aExpName<>nil then aExpName.Release;
    if not ok then
      begin
        if Value<>nil then Value.Release;
        for i:=OldListCount to VarList.Count-1 do
          TPasElement(VarList[i]).Release;
        VarList.Count:=OldListCount;
      end;
  end;
end;

procedure TPasParser.SetOptions(AValue: TPOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  If Assigned(FScanner) then
    FScanner.Options:=AValue;
end;

function TPasParser.SaveComments: String;
begin
  if Engine.NeedComments then
    FSavedComments:=CurComments.Text; // Expensive, so don't do unless needed.
  Result:=FSavedComments;
end;

function TPasParser.SaveComments(const AValue: String): String;
begin
  FSavedComments:=AValue;
  Result:=FSavedComments;
end;

function TPasParser.LogEvent(E: TPParserLogEvent): Boolean;
begin
  Result:=E in FLogEvents;
end;

procedure TPasParser.SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const);
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := SafeFormat(Fmt,Args);
  CreateMsgArgs(FLastMsgArgs,Args);
end;

procedure TPasParser.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Msg: String; SkipSourceInfo: Boolean);
begin
  DoLog(MsgType,MsgNumber,Msg,[],SkipSourceInfo);
end;

procedure TPasParser.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const; SkipSourceInfo: Boolean);
begin
  SetLastMsg(MsgType,MsgNumber,Fmt,Args);
  If Assigned(FOnLog) then
    if SkipSourceInfo or not assigned(scanner) then
      FOnLog(Self,FLastMsg)
    else
      FOnLog(Self,Format('%s(%d) : %s',[Scanner.CurFilename,Scanner.CurRow,FLastMsg]));
end;

procedure TPasParser.ParseInlineVarDecl(Parent: TPasElement; List: TFPList;
  AVisibility: TPasMemberVisibility = VisDefault; ClosingBrace: Boolean = False);

Var
  tt : TTokens;
begin
  ParseVarList(Parent,List,AVisibility,False);
  tt:=[tkEnd,tkSemicolon];
  if ClosingBrace then
   include(tt,tkBraceClose);
  if not (CurToken in tt) then
    ParseExc(nParserExpectedSemiColonEnd,SParserExpectedSemiColonEnd);
end;

// Starts after the variable name
procedure TPasParser.ParseVarDecl(Parent: TPasElement; List: TFPList);

begin
  ParseVarList(Parent,List,visDefault,True);
end;

// Starts after the opening bracket token
procedure TPasParser.ParseArgList(Parent: TPasElement; Args: TFPList; EndToken: TToken);
var
  IsUntyped, ok, LastHadDefaultValue: Boolean;
  Name : String;
  Value : TPasExpr;
  i, OldArgCount: Integer;
  Arg: TPasArgument;
  Access: TArgumentAccess;
  ArgType: TPasType;
begin
  LastHadDefaultValue := false;
  while True do
  begin
    OldArgCount:=Args.Count;
    Access := argDefault;
    IsUntyped := False;
    ArgType := nil;
    while True do
    begin
      NextToken;
      if CurToken = tkConst then
      begin
        Access := argConst;
        Name := ExpectIdentifier;
      end else if CurToken = tkConstRef then
      begin
        Access := argConstref;
        Name := ExpectIdentifier;
      end else if CurToken = tkVar then
      begin
        Access := ArgVar;
        Name := ExpectIdentifier;
      end else if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'OUT') then
      begin
        Access := ArgOut;
        Name := ExpectIdentifier;
      end else if CurToken = tkIdentifier then
        Name := CurTokenString
      else
        ParseExc(nParserExpectedConstVarID,SParserExpectedConstVarID);
      Arg := TPasArgument(CreateElement(TPasArgument, Name, Parent));
      Arg.Access := Access;
      Args.Add(Arg);
      NextToken;
      if CurToken = tkColon then
        break
      else if ((CurToken = tkSemicolon) or (CurToken = tkBraceClose)) and
        (Access <> argDefault) then
      begin
        // found an untyped const or var argument
        UngetToken;
        IsUntyped := True;
        break
      end
      else if CurToken <> tkComma then
        ParseExc(nParserExpectedCommaColon,SParserExpectedCommaColon);
    end;
    Value:=Nil;
    if not IsUntyped then
      begin
      Arg := TPasArgument(Args[0]);
      ArgType := ParseType(Arg,CurSourcePos);
      ok:=false;
      try
        NextToken;
        if CurToken = tkEqual then
          begin
          if (Args.Count>OldArgCount+1) then
            begin
            ArgType.Release;
            ArgType:=nil;
            ParseExc(nParserOnlyOneArgumentCanHaveDefault,SParserOnlyOneArgumentCanHaveDefault);
            end;
          if Parent is TPasProperty then
            ParseExc(nParserPropertyArgumentsCanNotHaveDefaultValues,
              SParserPropertyArgumentsCanNotHaveDefaultValues);
          NextToken;
          Value := DoParseExpression(Parent,Nil);
          // After this, we're on ), which must be unget.
          LastHadDefaultValue:=true;
          end
        else if LastHadDefaultValue then
          ParseExc(nParserDefaultParameterRequiredFor,
            SParserDefaultParameterRequiredFor,[TPasArgument(Args[OldArgCount]).Name]);
        UngetToken;
        ok:=true;
      finally
        if (not ok) and (ArgType<>nil) then
          ArgType.Release;
      end;
      end;

    for i := OldArgCount to Args.Count - 1 do
    begin
      Arg := TPasArgument(Args[i]);
      Arg.ArgType := ArgType;
      if Assigned(ArgType) then
        begin
        if (i > OldArgCount) then
          ArgType.AddRef;
        end;
      Arg.ValueExpr := Value;
      Value:=Nil; // Only the first gets a value. OK, since Var A,B : Integer = 1 is not allowed.
    end;

    for i := OldArgCount to Args.Count - 1 do
      Engine.FinishScope(stDeclaration,TPasArgument(Args[i]));

    NextToken;
    if (CurToken = tkIdentifier) and (LowerCase(CurTokenString) = 'location') then
      begin
        NextToken; // remove 'location'
        NextToken; // remove register
      end;
    if CurToken = EndToken then
      break;
  end;
end;


function TPasParser.CheckProcedureArgs(Parent: TPasElement; Args: TFPList;
  Mandatory: Boolean): boolean;

begin
  NextToken;
  case CurToken of
  tkBraceOpen:
    begin
    Result:=true;
    NextToken;
    if (CurToken<>tkBraceClose) then
      begin
      UngetToken;
      ParseArgList(Parent, Args, tkBraceClose);
      end;
    end;
  tkSemicolon,tkColon,tkof,tkis,tkIdentifier:
    begin
    Result:=false;
    if Mandatory then
      ParseExc(nParserExpectedLBracketColon,SParserExpectedLBracketColon)
    else
      UngetToken;
    end
  else
    ParseExcTokenError(';');
  end;
end;

procedure TPasParser.HandleProcedureModifier(Parent: TPasElement; pm: TProcedureModifier);

Var
  Tok : String;
  P : TPasProcedure;
  E : TPasExpr;

  procedure AddModifier;
  begin
    if pm in P.Modifiers then
      ParseExcSyntaxError;
    P.AddModifier(pm);
  end;

begin
  P:=TPasProcedure(Parent);
  if pm<>pmPublic then
    AddModifier;
  Case pm of
  pmExternal:
    begin
    NextToken;
    if CurToken in [tkString,tkIdentifier] then
      begin
      // external libname
      // external libname name XYZ
      // external name XYZ
      Tok:=UpperCase(CurTokenString);
      if Not ((CurToken=tkIdentifier) and (Tok='NAME')) then
        begin
        E:=DoParseExpression(Parent);
        if Assigned(P) then
          P.LibraryExpr:=E;
        end;
      if CurToken=tkSemicolon then
        UnGetToken
      else
        begin
        Tok:=UpperCase(CurTokenString);
        if ((CurToken=tkIdentifier) and (Tok='NAME')) then
          begin
          NextToken;
          if not (CurToken in [tkChar,tkString,tkIdentifier]) then
            ParseExcTokenError(TokenInfos[tkString]);
          E:=DoParseExpression(Parent);
          if Assigned(P) then
            P.LibrarySymbolName:=E;
          end;
        end;
      end
    else
      UngetToken;
    end;
  pmPublic:
    begin
    NextToken;
    If not CurTokenIsIdentifier('name') then
      begin
      if P.Parent is TPasClassType then
        begin
        // public section starts
        UngetToken;
        UngetToken;
        exit;
        end;
      AddModifier;
      CheckToken(tkSemicolon);
      exit;
      end
    else
      begin
      AddModifier;
      NextToken;  // Should be export name string.
      if not (CurToken in [tkString,tkIdentifier]) then
        ParseExcTokenError(TokenInfos[tkString]);
      E:=DoParseExpression(Parent);
      if Parent is TPasProcedure then
        TPasProcedure(Parent).PublicName:=E;
      if (CurToken <> tkSemicolon) then
        ParseExcTokenError(TokenInfos[tkSemicolon]);
      end;
    end;
  pmForward:
    begin
    if (Parent.Parent is TInterfaceSection) then
       begin
       ParseExc(nParserForwardNotInterface,SParserForwardNotInterface);
       UngetToken;
       end;
    end;
  pmMessage:
    begin
    Repeat
      NextToken;
      If CurToken<>tkSemicolon then
        begin
        if Parent is TPasProcedure then
          TPasProcedure(Parent).MessageName:=CurtokenString;
        If (CurToken=tkString) and (Parent is TPasProcedure) then
          TPasProcedure(Parent).Messagetype:=pmtString;
        end;
    until CurToken = tkSemicolon;
    UngetToken;
    end;
  pmDispID:
    begin
    TPasProcedure(Parent).DispIDExpr:=DoParseExpression(Parent,Nil);
    if CurToken = tkSemicolon then
      UngetToken;
    end;
  end; // Case
end;

procedure TPasParser.HandleProcedureTypeModifier(ProcType: TPasProcedureType;
  ptm: TProcTypeModifier);
begin
  if ptm in ProcType.Modifiers then
    ParseExcSyntaxError;
  Include(ProcType.Modifiers,ptm);
end;

// Next token is expected to be a "(", ";" or for a function ":". The caller
// will get the token after the final ";" as next token.

function TPasParser.DoCheckHint(Element : TPasElement): Boolean;

var
  ahint : TPasMemberHint;

begin
  Result:= IsCurTokenHint(ahint);
  if Result then  // deprecated,platform,experimental,library, unimplemented etc
    begin
    Element.Hints:=Element.Hints+[ahint];
    if aHint=hDeprecated then
      begin
      NextToken;
      if (CurToken<>tkString) then
        UngetToken
      else
        Element.HintMessage:=CurTokenString;
      end;
    end;
end;

procedure TPasParser.ParseProcedureOrFunctionHeader(Parent: TPasElement;
  Element: TPasProcedureType; ProcType: TProcType; OfObjectPossible: Boolean);

  Function FindInSection(AName : String;ASection : TPasSection) : Boolean;

  Var
    I : integer;
    Cn,FN : String;
    CT : TPasClassType;

  begin
    I:=ASection.Functions.Count-1;
    While (I>=0) and (CompareText(TPasElement(ASection.Functions[I]).Name,AName)<>0) do
      Dec(I);
    Result:=I<>-1;
    I:=Pos('.',AName);
    if (Not Result) and (I>0) then
      begin
      CN:=Copy(AName,1,I-1);
      FN:=AName;
      Delete(FN,1,I);
      I:=ASection.Classes.Count-1;
      While Not Result and (I>=0) do
        begin
        CT:=TPasClassType(ASection.Classes[i]);
        if CompareText(CT.Name,CN)=0 then
          Result:=CT.FindMember(TPasFunction, FN)<>Nil;
        Dec(I);
        end;
      end;
  end;

  procedure ConsumeSemi;
  begin
    NextToken;
    if (CurToken <> tkSemicolon) and IsCurTokenHint then
      UngetToken;
  end;

Var
  Tok : String;
  CC : TCallingConvention;
  PM : TProcedureModifier;
  Done: Boolean;
  ResultEl: TPasResultElement;
  OK,IsProc : Boolean;
  PTM: TProcTypeModifier;
  ModCount: Integer;
  LastToken: TToken;

begin
  // Element must be non-nil. Removed all checks for not-nil.
  // If it is nil, the following fails anyway.
  CheckProcedureArgs(Element,Element.Args,ProcType in [ptOperator,ptClassOperator]);
  IsProc:=Parent is TPasProcedure;
  case ProcType of
    ptFunction,ptClassFunction:
      begin
      NextToken;
      if CurToken = tkColon then
        begin
        ResultEl:=TPasFunctionType(Element).ResultEl;
        ResultEl.ResultType := ParseType(ResultEl,CurSourcePos);
        end
      // In Delphi mode, the implementation in the implementation section can be
      // without result as it was declared
      // We actually check if the function exists in the interface section.
      else if (msDelphi in CurrentModeswitches) and
              (Assigned(CurModule.ImplementationSection) or
               (CurModule is TPasProgram)) then
        begin
        if Assigned(CurModule.InterfaceSection) then
          OK:=FindInSection(Parent.Name,CurModule.InterfaceSection)
        else if (CurModule is TPasProgram) and Assigned(TPasProgram(CurModule).ProgramSection) then
          OK:=FindInSection(Parent.Name,TPasProgram(CurModule).ProgramSection);
        if Not OK then
          CheckToken(tkColon)
        else
          begin
          CheckToken(tkSemiColon);
          UngetToken;
          end;
        end
      else
        begin
        // Raise error
        CheckToken(tkColon);
        end;
      end;
    ptOperator,ptClassOperator:
      begin
      NextToken;
      ResultEl:=TPasFunctionType(Element).ResultEl;
      if (CurToken=tkIdentifier) then
        begin
        ResultEl.Name := CurTokenName;
        ExpectToken(tkColon);
        end
      else
        if (CurToken=tkColon) then
          ResultEl.Name := 'Result'
        else
          ParseExc(nParserExpectedColonID,SParserExpectedColonID);
        ResultEl.ResultType := ParseType(ResultEl,CurSourcePos);
      end;
  end;
  if OfObjectPossible then
    begin
    NextToken;
    if (CurToken = tkOf) then
      begin
      ExpectToken(tkObject);
      Element.IsOfObject := True;
      end
    else if (CurToken = tkIs) then
      begin
      expectToken(tkIdentifier);
      if (lowerCase(CurTokenString)<>'nested') then
        ParseExc(nParserExpectedNested,SParserExpectedNested);
      Element.IsNested:=True;
      end
    else
      UnGetToken;
    end;
  ModCount:=0;
  Repeat
    inc(ModCount);
    // Writeln(modcount, curtokentext);
    LastToken:=CurToken;
    NextToken;
    if (ModCount in [1,2,3]) and (CurToken = tkEqual) then
      begin
      // for example: const p: procedure = nil;
      UngetToken;
      exit;
      end;
    If CurToken=tkSemicolon then
      begin
      if LastToken=tkSemicolon then
        ParseExcSyntaxError;
      end
    else if TokenIsCallingConvention(CurTokenString,cc) then
      begin
      Element.CallingConvention:=Cc;
      if cc = ccSysCall then
      begin
        // remove LibBase
        NextToken;
        if CurToken=tkSemiColon then
          UngetToken
        else
          // remove legacy or basesysv on MorphOS syscalls
          begin
          if CurTokenIsIdentifier('legacy') or CurTokenIsIdentifier('BaseSysV') then
            NextToken;
          NextToken; // remove offset
          end;
      end;
      ExpectTokens([tkSemicolon,tkEqual]);
      if curtoken=tkEqual then
        ungettoken;
      end
    else if IsProc and TokenIsProcedureModifier(Parent,CurTokenString,PM) then
      HandleProcedureModifier(Parent,PM)
    else if TokenIsProcedureTypeModifier(Parent,CurTokenString,PTM) then
      HandleProcedureTypeModifier(Element,PTM)
    else if (CurToken=tklibrary) then // library is a token and a directive.
      begin
      Tok:=UpperCase(CurTokenString);
      NextToken;
      If (tok<>'NAME') then
        Element.Hints:=Element.Hints+[hLibrary]
      else
        begin
        NextToken;  // Should be export name string.
        ExpectToken(tkSemicolon);
        end;
      end
    else if DoCheckHint(Element) then
      ConsumeSemi
    else if (CurToken=tkIdentifier) and (CompareText(CurTokenText,'alias')=0) then
      begin
      ExpectToken(tkColon);
      ExpectToken(tkString);
      if (Parent is TPasProcedure) then
        (Parent as TPasProcedure).AliasName:=CurTokenText;
      ExpectToken(tkSemicolon);
      end
    else if (CurToken = tkSquaredBraceOpen) then
      begin
      repeat
        NextToken
      until CurToken = tkSquaredBraceClose;
      ExpectToken(tkSemicolon);
      end
    else
      CheckToken(tkSemicolon);
    Done:=(CurToken=tkSemiColon);
    if Done then
      begin
      NextToken;
      Done:=Not ((Curtoken=tkSquaredBraceOpen) or
                  TokenIsProcedureModifier(Parent,CurtokenString,PM) or
                  TokenIsProcedureTypeModifier(Parent,CurtokenString,PTM) or
                  IsCurTokenHint() or
                  TokenIsCallingConvention(CurTokenString,cc) or
                  (CurToken=tkIdentifier) and (CompareText(CurTokenText,'alias')=0));
//      DumpCurToken('Done '+IntToStr(Ord(Done)));
      UngetToken;
      end;

//    Writeln('Done: ',TokenInfos[Curtoken],' ',CurtokenString);
  Until Done;
  if DoCheckHint(Element) then  // deprecated,platform,experimental,library, unimplemented etc
    ConsumeSemi;
  if (ProcType in [ptOperator,ptClassOperator]) and (Parent is TPasOperator) then
    TPasOperator(Parent).CorrectName;
  Engine.FinishScope(stProcedureHeader,Element);
  if (Parent is TPasProcedure)
  and (not TPasProcedure(Parent).IsForward)
  and (not TPasProcedure(Parent).IsExternal)
  and ((Parent.Parent is TImplementationSection)
     or (Parent.Parent is TProcedureBody))
  then
    ParseProcedureBody(Parent);
  if Parent is TPasProcedure then
    Engine.FinishScope(stProcedure,Parent);
end;

// starts after the semicolon
procedure TPasParser.ParseProcedureBody(Parent: TPasElement);

var
  Body: TProcedureBody;

begin
  Body := TProcedureBody(CreateElement(TProcedureBody, '', Parent));
  TPasProcedure(Parent).Body:=Body;
  ParseDeclarations(Body);
end;


function TPasParser.ParseProperty(Parent: TPasElement; const AName: String;
  AVisibility: TPasMemberVisibility; IsClassField: boolean): TPasProperty;

  function GetAccessorName(aParent: TPasElement; out Expr: TPasExpr): String;
  var
    Params: TParamsExpr;
    Param: TPasExpr;
  begin
    ExpectIdentifier;
    Result := CurTokenString;
    Expr := CreatePrimitiveExpr(aParent,pekIdent,CurTokenString);

    // read .subident.subident...
    repeat
      NextToken;
      if CurToken <> tkDot then break;
      ExpectIdentifier;
      Result := Result + '.' + CurTokenString;
      AddToBinaryExprChain(Expr,CreatePrimitiveExpr(aParent,pekIdent,CurTokenString),eopSubIdent);
    until false;

    // read optional array index
    if CurToken <> tkSquaredBraceOpen then
      UnGetToken
    else
      begin
      Result := Result + '[';
      Params:=TParamsExpr(CreateElement(TParamsExpr,'',aParent));
      Params.Kind:=pekArrayParams;
      AddParamsToBinaryExprChain(Expr,Params);
      NextToken;
      case CurToken of
        tkChar:             Param:=CreatePrimitiveExpr(aParent,pekString, CurTokenText);
        tkNumber:           Param:=CreatePrimitiveExpr(aParent,pekNumber, CurTokenString);
        tkIdentifier:       Param:=CreatePrimitiveExpr(aParent,pekIdent, CurTokenText);
        tkfalse, tktrue:    Param:=CreateBoolConstExpr(aParent,pekBoolConst, CurToken=tktrue);
      else
        ParseExcExpectedIdentifier;
      end;
      Params.AddParam(Param);
      Result := Result + CurTokenString;
      ExpectToken(tkSquaredBraceClose);
      Result := Result + ']';
      end;
    repeat
      NextToken;
      if CurToken <> tkDot then
        begin
        UngetToken;
        break;
        end;
      ExpectIdentifier;
      Result := Result + '.' + CurTokenString;
      AddToBinaryExprChain(Expr,CreatePrimitiveExpr(aParent,pekIdent,CurTokenString),eopSubIdent);
    until false;
  end;

var
  isArray , ok: Boolean;
begin
  Result:=TPasProperty(CreateElement(TPasProperty,AName,Parent,AVisibility));
  if IsClassField then
    Include(Result.VarModifiers,vmClass);
  ok:=false;
  try
    NextToken;
    isArray:=CurToken=tkSquaredBraceOpen;
    if isArray then
      begin
      ParseArgList(Result, Result.Args, tkSquaredBraceClose);
      NextToken;
      end;
    if CurToken = tkColon then
      begin
      Result.VarType := ParseType(Result,CurSourcePos);
      NextToken;
      end;
    if CurTokenIsIdentifier('INDEX') then
      begin
      NextToken;
      Result.IndexExpr := DoParseExpression(Result);
      end;
    if CurTokenIsIdentifier('READ') then
      begin
      Result.ReadAccessorName := GetAccessorName(Result,Result.ReadAccessor);
      NextToken;
      end;
    if CurTokenIsIdentifier('WRITE') then
      begin
      Result.WriteAccessorName := GetAccessorName(Result,Result.WriteAccessor);
      NextToken;
      end;
    if CurTokenIsIdentifier('READONLY') then
      begin
      Result.DispIDReadOnly:=True;
      NextToken;
      end;
    if CurTokenIsIdentifier('DISPID') then
      begin
      NextToken;
      Result.DispIDExpr := DoParseExpression(Result,Nil);
      NextToken;
      end;
    if CurTokenIsIdentifier('IMPLEMENTS') then
      begin
      Result.ImplementsName := GetAccessorName(Result,Result.ImplementsFunc);
      NextToken;
      end;
    if CurTokenIsIdentifier('STORED') then
      begin
      NextToken;
      if CurToken = tkTrue then
        Result.StoredAccessorName := 'True'
      else if CurToken = tkFalse then
        Result.StoredAccessorName := 'False'
      else if CurToken = tkIdentifier then
        begin
        UngetToken;
        Result.StoredAccessorName := GetAccessorName(Result,Result.StoredAccessor);
        end
      else
        ParseExcSyntaxError;
      NextToken;
      end;
    if CurTokenIsIdentifier('DEFAULT') then
      begin
      if isArray then
        ParseExc(nParserArrayPropertiesCannotHaveDefaultValue,SParserArrayPropertiesCannotHaveDefaultValue);
      NextToken;
      Result.DefaultExpr := DoParseExpression(Result);
//      NextToken;
      end
    else if CurtokenIsIdentifier('NODEFAULT') then
      begin
      Result.IsNodefault:=true;
      if Result.DefaultExpr<>nil then
        ParseExcSyntaxError;
      NextToken;
      end;
    // Here the property ends. There can still be a 'default'
    if CurToken = tkSemicolon then
      NextToken;
    if CurTokenIsIdentifier('DEFAULT') then
      begin
      if (Result.VarType<>Nil) and (not isArray) then
        ParseExc(nParserDefaultPropertyMustBeArray,SParserDefaultPropertyMustBeArray);
      NextToken;
      if CurToken = tkSemicolon then
        begin
        Result.IsDefault := True;
        NextToken;
        end
      end;
    // Handle hints
    while DoCheckHint(Result) do
      NextToken;
    if Result.Hints=[] then
      UngetToken;
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
  Engine.FinishScope(stDeclaration,Result);
end;

// Starts after the "begin" token
procedure TPasParser.ParseProcBeginBlock(Parent: TProcedureBody);
var
  BeginBlock: TPasImplBeginBlock;
  SubBlock: TPasImplElement;
begin
  BeginBlock := TPasImplBeginBlock(CreateElement(TPasImplBeginBlock, '', Parent));
  Parent.Body := BeginBlock;
  repeat
    NextToken;
//    writeln('TPasParser.ParseProcBeginBlock ',curtokenstring);
    if CurToken=tkend then
      break
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(BeginBlock,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
  ExpectToken(tkSemicolon);
//  writeln('TPasParser.ParseProcBeginBlock ended ',curtokenstring);
end;

procedure TPasParser.ParseProcAsmBlock(Parent: TProcedureBody);
var
  AsmBlock: TPasImplAsmStatement;
begin
  AsmBlock:=TPasImplAsmStatement(CreateElement(TPasImplAsmStatement,'',Parent));
  Parent.Body:=AsmBlock;
  ParseAsmBlock(AsmBlock);
  ExpectToken(tkSemicolon);
end;

procedure TPasParser.ParseAsmBlock(AsmBlock: TPasImplAsmStatement);

Var
  LastToken : TToken;
  p: PTokenRec;

  Function atEndOfAsm : Boolean;

  begin
    Result:=(CurToken=tkEnd) and (LastToken<>tkAt);
  end;

begin
  if po_asmwhole in Options then
    begin
    FTokenRingCur:=0;
    FTokenRingStart:=0;
    FTokenRingEnd:=1;
    p:=@FTokenRing[0];
    p^.Comments.Clear;
    repeat
      Scanner.ReadNonPascalTillEndToken(true);
      case Scanner.CurToken of
      tkLineEnding:
        AsmBlock.Tokens.Add(Scanner.CurTokenString);
      tkend:
        begin
        p^.Token := tkend;
        p^.AsString := Scanner.CurTokenString;
        break;
        end
      else
        begin
        // missing end
        p^.Token := tkEOF;
        p^.AsString := '';
        break;
        end;
      end;
    until false;
    FCurToken := p^.Token;
    FCurTokenString := p^.AsString;
    CheckToken(tkend);
    end
  else
    begin
    LastToken:=tkEOF;
    NextToken;
    While Not atEndOfAsm do
      begin
      AsmBlock.Tokens.Add(CurTokenText);
      LastToken:=CurToken;
      NextToken;
      end;
    end;
  // NextToken; // Eat end.
  // Do not consume end. Current token will normally be end;
end;

// Next token is start of (compound) statement
// After parsing CurToken is on last token of statement
procedure TPasParser.ParseStatement(Parent: TPasImplBlock;
  out NewImplElement: TPasImplElement);
var
  CurBlock: TPasImplBlock;

  {$IFDEF VerbosePasParser}
  function i: string;
  var
    c: TPasElement;
  begin
    Result:='ParseImplCompoundStatement ';
    c:=CurBlock;
    while c<>nil do begin
      Result:=Result+'  ';
      c:=c.Parent;
    end;
  end;
  {$ENDIF}

  function CloseBlock: boolean; // true if parent reached
  begin
    if CurBlock.ClassType=TPasImplExceptOn then
      Engine.FinishScope(stExceptOnStatement,CurBlock);
    CurBlock:=CurBlock.Parent as TPasImplBlock;
    Result:=CurBlock=Parent;
  end;

  function CloseStatement(CloseIfs: boolean): boolean; // true if parent reached
  begin
    if CurBlock=Parent then exit(true);
    while CurBlock.CloseOnSemicolon
    or (CloseIfs and (CurBlock is TPasImplIfElse)) do
      if CloseBlock then exit(true);
    Result:=false;
  end;

  procedure CreateBlock(NewBlock: TPasImplBlock);
  begin
    CurBlock.AddElement(NewBlock);
    CurBlock:=NewBlock;
    if NewImplElement=nil then NewImplElement:=CurBlock;
  end;

var
  SubBlock: TPasImplElement;
  CmdElem: TPasImplElement;
  left, right: TPasExpr;
  El : TPasImplElement;
  ak : TAssignKind;
  lt : TLoopType;
  ok: Boolean;
  SrcPos: TPasSourcePos;
  Name: String;
  TypeEl: TPasType;

begin
  NewImplElement:=nil;
  CurBlock := Parent;
  while True do
  begin
    NextToken;
    // WriteLn({$IFDEF VerbosePasParser}i,{$ENDIF}' Token=',CurTokenText);
    case CurToken of
    tkasm:
      begin
      El:=TPasImplElement(CreateElement(TPasImplAsmStatement,'',CurBlock));
      ParseAsmBlock(TPasImplAsmStatement(El));
      CurBlock.AddElement(El);
      if NewImplElement=nil then NewImplElement:=CurBlock;
      if CloseStatement(False) then
        break;
      end;
    tkbegin:
      begin
      El:=TPasImplElement(CreateElement(TPasImplBeginBlock,'',CurBlock));
      CreateBlock(TPasImplBeginBlock(El));
      end;
    tkrepeat:
      begin
      El:=TPasImplRepeatUntil(CreateElement(TPasImplRepeatUntil,'',CurBlock));
      CreateBlock(TPasImplRepeatUntil(El));
      end;
    tkIf:
      begin
        NextToken;
        Left:=DoParseExpression(CurBlock);
        UngetToken;
        El:=TPasImplIfElse(CreateElement(TPasImplIfElse,'',CurBlock));
        TPasImplIfElse(El).ConditionExpr:=Left;
        Left.Parent:=El;
        //WriteLn(i,'IF Condition="',Condition,'" Token=',CurTokenText);
        CreateBlock(TPasImplIfElse(El));
        ExpectToken(tkthen);
      end;
    tkelse:
      if (CurBlock is TPasImplIfElse) then
      begin
        if TPasImplIfElse(CurBlock).IfBranch=nil then
        begin
        El:=TPasImplCommand(CreateElement(TPasImplCommand,'', CurBlock));
        CurBlock.AddElement(El);
        end;
        if TPasImplIfElse(CurBlock).ElseBranch<>nil then
        begin
          // this and the following 3 may solve TPasImplIfElse.AddElement BUG
          // ifs without begin end
          // if .. then
          //  if .. then
          //   else
          // else
          CloseBlock;
          CloseStatement(false);
        end;
        // Case ... else without semicolon in front.
      end else if (CurBlock is TPasImplCaseStatement) then
      begin
        UngetToken;
        CloseStatement(False);
        exit;
      end else if (CurBlock is TPasImplWhileDo) then
      begin
        CloseBlock;
        UngetToken;
      end else if (CurBlock is TPasImplForLoop) then
      begin
        //if .. then for .. do smt else ..
        CloseBlock;
        UngetToken;
      end else if (CurBlock is TPasImplWithDo) then
      begin
        //if .. then with .. do smt else ..
        CloseBlock;
        UngetToken;
      end else if (CurBlock is TPasImplRaise) then
      begin
        //if .. then Raise Exception else ..
        CloseBlock;
        UngetToken;
      end else if (CurBlock is TPasImplAsmStatement) then
      begin
        //if .. then asm end else ..
        CloseBlock;
        UngetToken;
      end else if (CurBlock is TPasImplTryExcept) then
      begin
        CloseBlock;
        El:=TPasImplTryExceptElse(CreateElement(TPasImplTryExceptElse,'',CurBlock));
        TPasImplTry(CurBlock).ElseBranch:=TPasImplTryExceptElse(El);
        CurBlock:=TPasImplTryExceptElse(El);
      end else
        ParseExcSyntaxError;
    tkwhile:
      begin
        // while Condition do
        NextToken;
        left:=DoParseExpression(CurBlock);
        UngetToken;
        //WriteLn(i,'WHILE Condition="',Condition,'" Token=',CurTokenText);
        El:=TPasImplWhileDo(CreateElement(TPasImplWhileDo,'',CurBlock));
        TPasImplWhileDo(El).ConditionExpr:=left;
        CreateBlock(TPasImplWhileDo(El));
        ExpectToken(tkdo);
      end;
    tkgoto:
      begin
        NextToken;
        curblock.AddCommand('goto '+curtokenstring);
        // expecttoken(tkSemiColon);
      end;
    tkfor:
      begin
        // for VarName := StartValue to EndValue do
        // for VarName in Expression do
        El:=TPasImplForLoop(CreateElement(TPasImplForLoop,'',CurBlock));
        ok:=false;
        Try
          ExpectIdentifier;
          Left:=CreatePrimitiveExpr(El,pekIdent,CurTokenString);
          TPasImplForLoop(El).VariableName:=Left;
          repeat
            NextToken;
            case CurToken of
              tkAssign:
                begin
                lt:=ltNormal;
                break;
                end;
              tkin:
                begin
                lt:=ltIn;
                break;
                end;
              tkDot:
                begin
                ExpectIdentifier;
                AddToBinaryExprChain(Left,
                  CreatePrimitiveExpr(El,pekIdent,CurTokenString), eopSubIdent);
                TPasImplForLoop(El).VariableName:=Left;
                end;
            else
              ParseExc(nParserExpectedAssignIn,SParserExpectedAssignIn);
            end;
          until false;
          NextToken;
          TPasImplForLoop(El).StartExpr:=DoParseExpression(El);
          if (Lt=ltNormal) then
            begin
            if Not (CurToken in [tkTo,tkDownTo]) then
              ParseExcTokenError(TokenInfos[tkTo]);
            if CurToken=tkdownto then
              Lt:=ltDown;
            NextToken;
            TPasImplForLoop(El).EndExpr:=DoParseExpression(El);
            end;
          TPasImplForLoop(El).LoopType:=lt;
          if (CurToken<>tkDo) then
            ParseExcTokenError(TokenInfos[tkDo]);
          ok:=true;
        finally
          if not ok then
            El.Release;
        end;
        CreateBlock(TPasImplForLoop(El));
        //WriteLn(i,'FOR "',VarName,'" := ',StartValue,' to ',EndValue,' Token=',CurTokenText);
      end;
    tkwith:
      begin
        // with Expr do
        // with Expr, Expr do
        SrcPos:=CurSourcePos;
        NextToken;
        Left:=DoParseExpression(CurBlock);
        //writeln(i,'WITH Expr="',Expr,'" Token=',CurTokenText);
        El:=TPasImplWithDo(CreateElement(TPasImplWithDo,'',CurBlock,SrcPos));
        TPasImplWithDo(El).AddExpression(Left);
        Left.Parent:=El;
        CreateBlock(TPasImplWithDo(El));
        repeat
          if CurToken=tkdo then break;
          if CurToken<>tkComma then
            ParseExcTokenError(TokenInfos[tkdo]);
          NextToken;
          Left:=DoParseExpression(CurBlock);
          //writeln(i,'WITH ...,Expr="',Expr,'" Token=',CurTokenText);
          TPasImplWithDo(CurBlock).AddExpression(Left);
        until false;
      end;
    tkcase:
      begin
        NextToken;
        Left:=DoParseExpression(CurBlock);
        UngetToken;
        //writeln(i,'CASE OF Expr="',Expr,'" Token=',CurTokenText);
        ExpectToken(tkof);
        El:=TPasImplCaseOf(CreateElement(TPasImplCaseOf,'',CurBlock));
        TPasImplCaseOf(El).CaseExpr:=Left;
        Left.Parent:=El;
        CreateBlock(TPasImplCaseOf(El));
        repeat
          NextToken;
          //writeln(i,'CASE OF Token=',CurTokenText);
          case CurToken of
          tkend:
            begin
            if CurBlock.Elements.Count=0 then
              ParseExc(nParserExpectCase,SParserExpectCase);
            break; // end without else
            end;
          tkelse:
            begin
              // create case-else block
              El:=TPasImplCaseElse(CreateElement(TPasImplCaseElse,'',CurBlock));
              TPasImplCaseOf(CurBlock).ElseBranch:=TPasImplCaseElse(El);
              CreateBlock(TPasImplCaseElse(El));
              break;
            end
          else
            // read case values
            if (curToken=tkIdentifier) and (LowerCase(CurtokenString)='otherwise') then
              begin
              // create case-else block
              El:=TPasImplCaseElse(CreateElement(TPasImplCaseElse,'',CurBlock));
              TPasImplCaseOf(CurBlock).ElseBranch:=TPasImplCaseElse(El);
              CreateBlock(TPasImplCaseElse(El));
              break;
              end
            else
              repeat
                Left:=DoParseExpression(CurBlock);
                //writeln(i,'CASE value="',Expr,'" Token=',CurTokenText);
                if CurBlock is TPasImplCaseStatement then
                  TPasImplCaseStatement(CurBlock).Expressions.Add(Left)
                else
                  begin
                  El:=TPasImplCaseStatement(CreateElement(TPasImplCaseStatement,'',CurBlock));
                  TPasImplCaseStatement(El).AddExpression(Left);
                  CurBlock.AddElement(El);
                  CurBlock:=TPasImplCaseStatement(El);
                  end;
                //writeln(i,'CASE after value Token=',CurTokenText);
                if (CurToken=tkComma) then
                  NextToken
                else if (CurToken<>tkColon) then
                  ParseExcTokenError(TokenInfos[tkComma]);
              until Curtoken=tkColon;
            // read statement
            ParseStatement(CurBlock,SubBlock);
            CloseBlock;
            if CurToken<>tkSemicolon then
            begin
              NextToken;
              if not (CurToken in [tkSemicolon,tkelse,tkend]) then
                ParseExcTokenError(TokenInfos[tkSemicolon]);
              if CurToken<>tkSemicolon then
                UngetToken;
            end;
          end;
        until false;
        if CurToken=tkend then
        begin
          if CloseBlock then break;
          if CloseStatement(false) then break;
        end;
      end;
    tktry:
      begin
      El:=TPasImplTry(CreateElement(TPasImplTry,'',CurBlock));
      CreateBlock(TPasImplTry(El));
      end;
    tkfinally:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplTry then
        begin
          El:=TPasImplTryFinally(CreateElement(TPasImplTryFinally,'',CurBlock));
          TPasImplTry(CurBlock).FinallyExcept:=TPasImplTryFinally(El);
          CurBlock:=TPasImplTryFinally(El);
        end else
          ParseExcSyntaxError;
      end;
    tkexcept:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplTry then
        begin
          //writeln(i,'EXCEPT');
          El:=TPasImplTryExcept(CreateElement(TPasImplTryExcept,'',CurBlock));
          TPasImplTry(CurBlock).FinallyExcept:=TPasImplTryExcept(El);
          CurBlock:=TPasImplTryExcept(El);
        end else
          ParseExcSyntaxError;
      end;
    tkraise:
      begin
      El:=TPasImplRaise(CreateElement(TPasImplRaise,'',CurBlock));
      CreateBlock(TPasImplRaise(El));
      NextToken;
      If Curtoken in [tkElse,tkEnd,tkSemicolon] then
        UnGetToken
      else
        begin
        TPasImplRaise(El).ExceptObject:=DoParseExpression(El);
        if (CurToken=tkIdentifier) and (Uppercase(CurtokenString)='AT') then
          begin
          NextToken;
          TPasImplRaise(El).ExceptAddr:=DoParseExpression(El);
          end;
        if Curtoken in [tkSemicolon,tkEnd] then
          UngetToken
        end;
      end;
    tkend:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplBeginBlock then
        begin
          if CloseBlock then break; // close end
          if CloseStatement(false) then break;
        end else if CurBlock is TPasImplCaseElse then
        begin
          if CloseBlock then break; // close else
          if CloseBlock then break; // close caseof
          if CloseStatement(false) then break;
        end else if CurBlock is TPasImplTryHandler then
        begin
          if CloseBlock then break; // close finally/except
          if CloseBlock then break; // close try
          if CloseStatement(false) then break;
        end else
          ParseExcSyntaxError;
      end;
    tkSemiColon:
      if CloseStatement(true) then break;
    tkFinalization:
      if CloseStatement(true) then break;
    tkuntil:
      begin
        if CloseStatement(true) then
        begin
          UngetToken;
          break;
        end;
        if CurBlock is TPasImplRepeatUntil then
        begin
          NextToken;
          Left:=DoParseExpression(CurBlock);
          UngetToken;
          TPasImplRepeatUntil(CurBlock).ConditionExpr:=Left;
          //WriteLn(i,'UNTIL Condition="',Condition,'" Token=',CurTokenString);
          if CloseBlock then break;
        end else
          ParseExcSyntaxError;
      end;
    tkEOF:
      CheckToken(tkend);
    tkAt,tkBraceOpen,tkIdentifier,tkNumber,tkSquaredBraceOpen,tkMinus,tkPlus,tkinherited:
      begin
// This should in fact not be checked here.
//      if (CurToken=tkAt) and not (msDelphi in CurrentModeswitches) then
//        ParseExc;
      // On is usable as an identifier
      if lowerCase(CurTokenText)='on' then
        begin
          // in try except:
          // on E: Exception do
          // on Exception do
          if CurBlock is TPasImplTryExcept then
          begin
            ExpectIdentifier;
            El:=TPasImplExceptOn(CreateElement(TPasImplExceptOn,'',CurBlock));
            SrcPos:=CurSourcePos;
            Name:=CurTokenString;
            NextToken;
            //writeln('ON t=',Name,' Token=',CurTokenText);
            if CurToken=tkColon then
              begin
              // the first expression was the variable name
              NextToken;
              TypeEl:=ParseSimpleType(El,SrcPos,'');
              TPasImplExceptOn(El).TypeEl:=TypeEl;
              TPasImplExceptOn(El).VarEl:=TPasVariable(CreateElement(TPasVariable,
                                    Name,El,SrcPos));
              TPasImplExceptOn(El).VarEl.VarType:=TypeEl;
              TypeEl.AddRef;
              end
            else
              begin
              UngetToken;
              TPasImplExceptOn(El).TypeEl:=ParseSimpleType(El,SrcPos,'');
              end;
            Engine.FinishScope(stExceptOnExpr,El);
            CurBlock.AddElement(El);
            CurBlock:=TPasImplExceptOn(El);
            ExpectToken(tkDo);
          end else
            ParseExcSyntaxError;
        end
      else
        begin
        left:=DoParseExpression(CurBlock);
        case CurToken of
          tkAssign,
          tkAssignPlus,
          tkAssignMinus,
          tkAssignMul,
          tkAssignDivision:
          begin
            // assign statement
            Ak:=TokenToAssignKind(CurToken);
            NextToken;
            right:=DoParseExpression(CurBlock); // this may solve TPasImplWhileDo.AddElement BUG
            El:=TPasImplAssign(CreateElement(TPasImplAssign,'',CurBlock));
            left.Parent:=El;
            right.Parent:=El;
            TPasImplAssign(El).left:=Left;
            TPasImplAssign(El).right:=Right;
            TPasImplAssign(El).Kind:=ak;
            CurBlock.AddElement(El);
            CmdElem:=TPasImplAssign(El);
            UngetToken;
          end;
          tkColon:
          begin
            if not (left is TPrimitiveExpr) then
              ParseExcTokenError(TokenInfos[tkSemicolon]);
            // label mark. todo: check mark identifier in the list of labels
            El:=TPasImplLabelMark(CreateElement(TPasImplLabelMark,'', CurBlock));
            TPasImplLabelMark(El).LabelId:=TPrimitiveExpr(left).Value;
            CurBlock.AddElement(El);
            CmdElem:=TPasImplLabelMark(El);
            left.Free;
          end;
        else
          // simple statement (function call)
          El:=TPasImplSimple(CreateElement(TPasImplSimple,'',CurBlock));
          TPasImplSimple(El).expr:=Left;
          CurBlock.AddElement(El);
          CmdElem:=TPasImplSimple(El);
          UngetToken;
        end;

        if not (CmdElem is TPasImplLabelMark) then
          if NewImplElement=nil then NewImplElement:=CmdElem;
        end;
      end;
    else
      ParseExcSyntaxError;
    end;
  end;
end;

procedure TPasParser.ParseLabels(AParent: TPasElement);
var
  Labels: TPasLabels;
begin
  Labels:=TPasLabels(CreateElement(TPasLabels, '', AParent));
  repeat
    Labels.Labels.Add(ExpectIdentifier);
    NextToken;
    if not (CurToken in [tkSemicolon, tkComma]) then
      ParseExcTokenError(TokenInfos[tkSemicolon]);
  until CurToken=tkSemicolon;
end;

// Starts after the "procedure" or "function" token
function TPasParser.GetProcedureClass(ProcType: TProcType): TPTreeElement;

begin
  Case ProcType of
    ptFunction       : Result:=TPasFunction;
    ptClassFunction  : Result:=TPasClassFunction;
    ptClassProcedure : Result:=TPasClassProcedure;
    ptClassConstructor : Result:=TPasClassConstructor;
    ptClassDestructor  : Result:=TPasClassDestructor;
    ptProcedure      : Result:=TPasProcedure;
    ptConstructor    : Result:=TPasConstructor;
    ptDestructor     : Result:=TPasDestructor;
    ptOperator       : Result:=TPasOperator;
    ptClassOperator  : Result:=TPasClassOperator;
  else
    ParseExc(nParserUnknownProcedureType,SParserUnknownProcedureType,[Ord(ProcType)]);
  end;
end;

function TPasParser.ParseProcedureOrFunctionDecl(Parent: TPasElement; ProcType: TProcType;AVisibility : TPasMemberVisibility = VisDefault): TPasProcedure;

  function ExpectProcName: string;

  Var
    L : TFPList;
    I : Integer;

  begin
    Result:=ExpectIdentifier;
    //writeln('ExpectProcName ',Parent.Classname);
    if Parent is TImplementationSection then
    begin
      NextToken;
      While CurToken in [tkDot,tkLessThan] do
        begin
        if CurToken=tkDot then
          Result:=Result+'.'+ExpectIdentifier
        else
          begin // <> can be ignored, we read the list but discard its content
          UnGetToken;
          L:=TFPList.Create;
          Try
            ReadGenericArguments(L,Parent);
          finally
            For I:=0 to L.Count-1 do
              TPasElement(L[i]).Release;
            L.Free;
          end;
          end;
        NextToken;
        end;
      UngetToken;
    end;
  end;

var
  Name: String;
  PC : TPTreeElement;
  Ot : TOperatorType;
  IsTokenBased , ok: Boolean;

begin
  If (Not (ProcType in [ptOperator,ptClassOperator])) then
    Name:=ExpectProcName
  else
    begin
    NextToken;
    IsTokenBased:=Curtoken<>tkIdentifier;
    if IsTokenBased then
      OT:=TPasOperator.TokenToOperatorType(CurTokenText)
    else
      OT:=TPasOperator.NameToOperatorType(CurTokenString);
    if (ot=otUnknown) then
      ParseExc(nErrUnknownOperatorType,SErrUnknownOperatorType,[CurTokenString]);
    Name:=OperatorNames[Ot];
    end;
  PC:=GetProcedureClass(ProcType);
  Parent:=CheckIfOverLoaded(Parent,Name);
  Result:=TPasProcedure(CreateElement(PC,Name,Parent,AVisibility));
  ok:=false;
  try
    if Not (ProcType in [ptFunction, ptClassFunction, ptOperator, ptClassOperator]) then
      Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '', Result))
    else
      begin
      Result.ProcType := CreateFunctionType('', 'Result', Result, True, CurSourcePos);
      if (ProcType in [ptOperator, ptClassOperator]) then
        begin
        TPasOperator(Result).TokenBased:=IsTokenBased;
        TPasOperator(Result).OperatorType:=OT;
        TPasOperator(Result).CorrectName;
        end;
      end;
    ParseProcedureOrFunctionHeader(Result, Result.ProcType, ProcType, False);
    Result.Hints:=Result.ProcType.Hints;
    Result.HintMessage:=Result.ProcType.HintMessage;
    // + is detected as 'positive', but is in fact Add if there are 2 arguments.
    if (ProcType in [ptOperator, ptClassOperator]) then
      With TPasOperator(Result) do
        begin
        if (OperatorType in [otPositive, otNegative]) then
          begin
          if (ProcType.Args.Count>1) then
            begin
            Case OperatorType of
              otPositive : OperatorType:=otPlus;
              otNegative : OperatorType:=otMinus;
            end;
            Name:=OperatorNames[OperatorType];
            TPasOperator(Result).CorrectName;
            end;
          end;
        end;
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

// Current token is the first token after tkOf
procedure TPasParser.ParseRecordVariantParts(ARec: TPasRecordType;
  AEndToken: TToken);

Var
  M : TPasRecordType;
  V : TPasVariant;
  Done : Boolean;

begin
  Repeat
    V:=TPasVariant(CreateElement(TPasVariant, '', ARec));
    ARec.Variants.Add(V);
    Repeat
      NextToken;
      V.Values.Add(DoParseExpression(ARec));
      if Not (CurToken in [tkComma,tkColon]) then
        ParseExc(nParserExpectedCommaColon,SParserExpectedCommaColon);
    Until (curToken=tkColon);
    ExpectToken(tkBraceOpen);
    NextToken;
    M:=TPasRecordType(CreateElement(TPasRecordType,'',V));
    V.Members:=M;
    ParseRecordFieldList(M,tkBraceClose,False);
    // Current token is closing ), so we eat that
    NextToken;
    // If there is a semicolon, we eat that too.
    if CurToken=tkSemicolon then
      NextToken;
    // ParseExpression starts with a nexttoken.
    // So we need to determine the next token, and if it is an ending token, unget.
    Done:=CurToken=AEndToken;
    If not Done then
      Ungettoken;
  Until Done;
end;

procedure TPasParser.DumpCurToken(const Msg: String; IndentAction: TIndentAction
  );
begin
  if IndentAction=iaUndent then
    FDumpIndent:=copy(FDumpIndent,1,Length(FDumpIndent)-2);
  Writeln(FDumpIndent,Msg,' : ',TokenInfos[CurToken],' "',CurTokenString,'", Position: ',Scanner.CurFilename,'(',Scanner.CurRow,',',SCanner.CurColumn,') : ',Scanner.CurLine);
  if IndentAction=iaIndent then
    FDumpIndent:=FDumpIndent+'  ';
  Flush(output);
end;

function TPasParser.GetCurrentModeSwitches: TModeSwitches;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurrentModeSwitches
  else
    Result:=[msNone];
end;

procedure TPasParser.SetCurrentModeSwitches(AValue: TModeSwitches);
begin
  if Assigned(FScanner) then
    FScanner.CurrentModeSwitches:=AValue;
end;

// Starts on first token after Record or (. Ends on AEndToken
procedure TPasParser.ParseRecordFieldList(ARec: TPasRecordType;
  AEndToken: TToken; AllowMethods: Boolean);

Var
  VariantName : String;
  v : TPasmemberVisibility;
  Proc: TPasProcedure;
  ProcType: TProcType;
  Prop : TPasProperty;
  Cons : TPasConst;
  isClass : Boolean;
  NamePos: TPasSourcePos;
  OldCount, i: Integer;
begin
  v:=visDefault;
  isClass:=False;
  while CurToken<>AEndToken do
    begin
    SaveComments;
    Case CurToken of
      tkConst:
        begin
        if Not AllowMethods then
          ParseExc(nErrRecordConstantsNotAllowed,SErrRecordConstantsNotAllowed);
        ExpectToken(tkIdentifier);
        Cons:=ParseConstDecl(ARec);
        Cons.Visibility:=v;
        ARec.members.Add(Cons);
        end;
      tkClass:
        begin
        if Not AllowMethods then
          ParseExc(nErrRecordMethodsNotAllowed,SErrRecordMethodsNotAllowed);
        if isClass then
          ParseExc(nParserTypeSyntaxError,SParserTypeSyntaxError);
        isClass:=True;
        end;
      tkProperty:
        begin
        if Not AllowMethods then
          ParseExc(nErrRecordPropertiesNotAllowed,SErrRecordPropertiesNotAllowed);
        ExpectToken(tkIdentifier);
        Prop:=ParseProperty(ARec,CurtokenString,v,isClass);
        Arec.Members.Add(Prop);
        end;
      tkOperator,
      tkProcedure,
      tkConstructor,
      tkFunction :
        begin
        if Not AllowMethods then
          ParseExc(nErrRecordMethodsNotAllowed,SErrRecordMethodsNotAllowed);
        ProcType:=GetProcTypeFromToken(CurToken,isClass);
        Proc:=ParseProcedureOrFunctionDecl(ARec,ProcType,v);
        if Proc.Parent is TPasOverloadedProc then
          TPasOverloadedProc(Proc.Parent).Overloads.Add(Proc)
        else
          ARec.Members.Add(Proc);
        end;
      tkGeneric, // Counts as field name
      tkIdentifier :
        begin
          if CheckVisibility(CurtokenString,v) then
            begin
            If not (msAdvancedRecords in Scanner.CurrentModeSwitches) then
              ParseExc(nErrRecordVisibilityNotAllowed,SErrRecordVisibilityNotAllowed);
            if not (v in [visPrivate,visPublic,visStrictPrivate]) then
              ParseExc(nParserInvalidRecordVisibility,SParserInvalidRecordVisibility);
            NextToken;
            Continue;
            end;
        OldCount:=ARec.Members.Count;
        ParseInlineVarDecl(ARec, ARec.Members, v, AEndToken=tkBraceClose);
        for i:=OldCount to ARec.Members.Count-1 do
          Engine.FinishScope(stDeclaration,TPasVariable(ARec.Members[i]));
        end;
      tkCase :
        begin
        ARec.Variants:=TFPList.Create;
        NextToken;
        VariantName:=CurTokenString;
        NamePos:=CurSourcePos;
        NextToken;
        If CurToken=tkColon then
          begin
          ARec.VariantEl:=TPasVariable(CreateElement(TPasVariable,VariantName,ARec,NamePos));
          TPasVariable(ARec.VariantEl).VarType:=ParseType(ARec,CurSourcePos);
          end
        else
          begin
          UnGetToken;
          UnGetToken;
          ARec.VariantEl:=ParseType(ARec,CurSourcePos);
          end;
        ExpectToken(tkOf);
        ParseRecordVariantParts(ARec,AEndToken);
        end;
    else
      ParseExc(nParserTypeSyntaxError,SParserTypeSyntaxError);
    end;
    If CurToken<>tkClass then
      isClass:=False;
    if CurToken<>AEndToken then
      NextToken;
    end;
end;

// Starts after the "record" token
function TPasParser.ParseRecordDecl(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string;
  const Packmode: TPackMode): TPasRecordType;

var
  ok: Boolean;
begin
  Result := TPasRecordType(CreateElement(TPasRecordType, TypeName, Parent, NamePos));
  ok:=false;
  try
    Result.PackMode:=PackMode;
    NextToken;
    ParseRecordFieldList(Result,tkEnd,true);
    Engine.FinishScope(stTypeDef,Result);
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

Function IsVisibility(S : String;  var AVisibility :TPasMemberVisibility) : Boolean;

Const
  VNames : array[TPasMemberVisibility] of string =
    ('', 'private', 'protected', 'public', 'published', 'automated', '', '');
Var
  V : TPasMemberVisibility;

begin
  Result:=False;
  S:=lowerCase(S);
  For V :=Low(TPasMemberVisibility) to High(TPasMemberVisibility) do
    begin
    Result:=(VNames[V]<>'') and (S=VNames[V]);
    if Result then
      begin
      AVisibility := v;
      Exit;
      end;
    end;
end;

function TPasParser.CheckVisibility(S: String;
  var AVisibility: TPasMemberVisibility): Boolean;

Var
  B : Boolean;

begin
  s := LowerCase(CurTokenString);
  B:=(S='strict');
  if B then
    begin
    NextToken;
    s:=LowerCase(CurTokenString);
    end;
  Result:=isVisibility(S,AVisibility);
  if Result then
    begin
    if B then
      case AVisibility of
        visPrivate   : AVisibility:=visStrictPrivate;
        visProtected : AVisibility:=visStrictProtected;
      else
        ParseExc(nParserStrangeVisibility,SParserStrangeVisibility,[S]);
      end
    end
  else if B then
    ParseExc(nParserExpectVisibility,SParserExpectVisibility);
end;

procedure TPasParser.ProcessMethod(AType: TPasClassType; IsClass : Boolean; AVisibility : TPasMemberVisibility);

var
  Proc: TPasProcedure;
  ProcType: TProcType;
begin
  ProcType:=GetProcTypeFromToken(CurToken,isClass);
  Proc:=ParseProcedureOrFunctionDecl(AType,ProcType,AVisibility);
  if Proc.Parent is TPasOverloadedProc then
    TPasOverloadedProc(Proc.Parent).Overloads.Add(Proc)
  else
    AType.Members.Add(Proc);
end;

procedure TPasParser.ParseClassFields(AType: TPasClassType;
  const AVisibility: TPasMemberVisibility; IsClassField: Boolean);

Var
  VarList: TFPList;
  Element: TPasElement;
  I : Integer;
  isStatic : Boolean;
  VarEl: TPasVariable;

begin
  VarList := TFPList.Create;
  try
    ParseInlineVarDecl(AType, VarList, AVisibility, False);
    if CurToken=tkSemicolon then
      begin
      NextToken;
      isStatic:=CurTokenIsIdentifier('static');
      if isStatic then
        ExpectToken(tkSemicolon)
      else
        UngetToken;
      end;
    for i := 0 to VarList.Count - 1 do
      begin
      Element := TPasElement(VarList[i]);
      Element.Visibility := AVisibility;
      if (Element is TPasVariable) then
        begin
        VarEl:=TPasVariable(Element);
        if IsClassField then
          Include(VarEl.VarModifiers,vmClass);
        if isStatic then
          Include(VarEl.VarModifiers,vmStatic);
        Engine.FinishScope(stDeclaration,VarEl);
        end;
      AType.Members.Add(Element);
      end;
  finally
    VarList.Free;
  end;
end;

procedure TPasParser.ParseClassLocalTypes(AType: TPasClassType; AVisibility : TPasMemberVisibility);

Var
  T : TPasType;
  Done : Boolean;
begin
//  Writeln('Parsing local types');
  Repeat
    T:=ParseTypeDecl(AType);
    T.Visibility:=AVisibility;
    AType.Members.Add(t);
//    Writeln(CurtokenString,' ',TokenInfos[Curtoken]);
    NextToken;
    Done:=(Curtoken<>tkIdentifier) or CheckVisibility(CurtokenString,AVisibility);
    if Done then
      UngetToken;
  Until Done;
end;

procedure TPasParser.ParseClassLocalConsts(AType: TPasClassType; AVisibility : TPasMemberVisibility);

Var
  C : TPasConst;
  Done : Boolean;
begin
//  Writeln('Parsing local consts');
  Repeat
    C:=ParseConstDecl(AType);
    C.Visibility:=AVisibility;
    AType.Members.Add(C);
//    Writeln(CurtokenString,' ',TokenInfos[Curtoken]);
    NextToken;
    Done:=(Curtoken<>tkIdentifier) or CheckVisibility(CurtokenString,AVisibility);
    if Done then
      UngetToken;
  Until Done;
end;

procedure TPasParser.ParseClassMembers(AType: TPasClassType);

Type
  TSectionType = (stNone,stConst,stType,stVar);

Var
  CurVisibility : TPasMemberVisibility;
  CurSection : TSectionType;
  haveClass : Boolean;

begin
  CurSection:=stNone;
  CurVisibility := visDefault;
  HaveClass:=False;
  while (CurToken<>tkEnd) do
    begin
    case CurToken of
      tkType:
        CurSection:=stType;
      tkConst:
        CurSection:=stConst;
      tkVar:
        CurSection:=stVar;
      tkIdentifier:
        if CheckVisibility(CurtokenString,CurVisibility) then
          CurSection:=stNone
        else
          begin
          if not haveClass then
            SaveComments;
          Case CurSection of
          stType:
            ParseClassLocalTypes(AType,CurVisibility);
          stConst :
            ParseClassLocalConsts(AType,CurVisibility);
          stNone,
          stvar:
            begin
            if (AType.ObjKind in [okInterface,okDispInterface]) then
              ParseExc(nParserNoFieldsAllowed,SParserNoFieldsAllowed);
            ParseClassFields(AType,CurVisibility,HaveClass);
            HaveClass:=False;
            end;
          else
            Raise Exception.Create('Internal error 201704251415');
          end;
          end;
      tkProcedure,tkFunction,tkConstructor,tkDestructor:
        begin
        curSection:=stNone;
        if not haveClass then
          SaveComments;
        if (Curtoken in [tkConstructor,tkDestructor]) and (AType.ObjKind in [okInterface,okDispInterface,okRecordHelper]) then
          ParseExc(nParserNoConstructorAllowed,SParserNoConstructorAllowed);
        ProcessMethod(AType,HaveClass,CurVisibility);
        haveClass:=False;
        end;
      tkclass:
        begin
        SaveComments;
        HaveClass:=True;
        curSection:=stNone;
        end;
      tkProperty:
        begin
        curSection:=stNone;
        if not haveClass then
          SaveComments;
        ExpectIdentifier;
        AType.Members.Add(ParseProperty(AType,CurtokenString,CurVisibility,HaveClass));
        HaveClass:=False;
        end
    else
      CheckToken(tkIdentifier);
    end;
    NextToken;
    end;
end;

procedure TPasParser.DoParseClassType(AType: TPasClassType);

var
  s: String;
  Expr: TPasExpr;

begin
  if (CurToken=tkIdentifier) and (AType.ObjKind in [okClass,okGeneric]) then
    begin
    s := LowerCase(CurTokenString);
    if (s = 'sealed') or (s = 'abstract') then
      begin
      AType.Modifiers.Add(s);
      NextToken;
      end;
    end;
  // Parse ancestor list
  AType.IsForward:=(CurToken=tkSemiColon);
  if (CurToken=tkBraceOpen) then
    begin
    // read ancestor and interfaces
    NextToken;
    AType.AncestorType := ParseTypeReference(AType,false,Expr);
    while CurToken=tkComma do
      begin
      NextToken;
      AType.Interfaces.Add(ParseTypeReference(AType,false,Expr));
      end;
    CheckToken(tkBraceClose);
    NextToken;
    AType.IsShortDefinition:=(CurToken=tkSemicolon);
    end;
  if (AType.ObjKind in [okClassHelper,okRecordHelper]) then
    begin
    CheckToken(tkfor);
    NextToken;
    AType.HelperForType:=ParseTypeReference(AType,false,Expr);
    end;
  Engine.FinishScope(stAncestors,AType);
  if (AType.IsShortDefinition or AType.IsForward) then
    UngetToken
  else
    begin
    if (AType.ObjKind in [okInterface,okDispInterface]) and (CurToken = tkSquaredBraceOpen) then
      begin
      NextToken;
      AType.GUIDExpr:=DoParseExpression(AType);
      if (CurToken<>tkSquaredBraceClose) then
        ParseExcTokenError(TokenInfos[tkSquaredBraceClose]);
      NextToken;
      end;
    ParseClassMembers(AType);
    end;
end;

function TPasParser.ParseClassDecl(Parent: TPasElement;
  const NamePos: TPasSourcePos; const AClassName: String;
  AObjKind: TPasObjKind; PackMode: TPackMode; GenericArgs: TFPList): TPasType;

Var
  ok: Boolean;
  FT : TPasType;
  AExternalNameSpace,AExternalName : String;
  PCT:TPasClassType;
begin
  NextToken;
  FT:=Nil;
  if (AObjKind = okClass) and (CurToken = tkOf) then
    begin
    Result := TPasClassOfType(CreateElement(TPasClassOfType, AClassName,
      Parent, NamePos));
    ExpectIdentifier;
    UngetToken;                // Only names are allowed as following type
    TPasClassOfType(Result).DestType := ParseType(Result,CurSourcePos);
    Engine.FinishScope(stTypeDef,Result);
    exit;
    end;
  if ((AobjKind in [okClass,OKInterface]) and (msExternalClass in CurrentModeswitches) and  CurTokenIsIdentifier('external')) then
    begin
    NextToken;
    if CurToken<>tkString then
      UnGetToken
    else
      AExternalNameSpace:=CurTokenString;
    ExpectIdentifier;
    If Not CurTokenIsIdentifier('Name')  then
      ParseExc(nParserExpectedExternalClassName,SParserExpectedExternalClassName);
    NextToken;
    if not (CurToken in [tkChar,tkString]) then
      CheckToken(tkString);
    AExternalName:=CurTokenString;
    NextToken;
    end
  else
    begin
    AExternalNameSpace:='';
    AExternalName:='';
    end;
  if (CurTokenIsIdentifier('Helper')) then
    begin
    if Not (AObjKind in [okClass,okTypeHelper,okRecordHelper]) then
      ParseExc(nParserHelperNotAllowed,SParserHelperNotAllowed,[ObjKindNames[AObjKind]]);
    Case AObjKind of
     okClass:
       AObjKind:=okClassHelper;
     okTypeHelper:
       begin
       ExpectToken(tkFor);
       FT:=ParseType(Parent,CurSourcePos,'',False);
       end
    end;
    NextToken;
    end;
  PCT := TPasClassType(CreateElement(TPasClassType, AClassName,
    Parent, NamePos));
  Result:=PCT;
  PCT.HelperForType:=FT;
  PCT.IsExternal:=(AExternalName<>'');
  if AExternalName<>'' then
    PCT.ExternalName:=AnsiDequotedStr(AExternalName,'''');
  if AExternalNameSpace<>'' then
    PCT.ExternalNameSpace:=AnsiDequotedStr(AExternalNameSpace,'''');
  ok:=false;
  try
    PCT.ObjKind := AObjKind;
    PCT.PackMode:=PackMode;
    if Assigned(GenericArgs) then
      PCT.SetGenericTemplates(GenericArgs);
    DoParseClassType(PCT);
    Engine.FinishScope(stTypeDef,Result);
    ok:=true;
  finally
    if not ok then
      Result.Release;
  end;
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, visDefault, CurSourcePos);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; const ASrcPos: TPasSourcePos): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, visDefault, ASrcPos);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, AVisibility,
    CurSourcePos);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, AVisibility, ASrcPos);
end;

function TPasParser.CreatePrimitiveExpr(AParent: TPasElement;
  AKind: TPasExprKind; const AValue: String): TPrimitiveExpr;
begin
  Result:=TPrimitiveExpr(CreateElement(TPrimitiveExpr,'',AParent));
  Result.Kind:=AKind;
  Result.Value:=AValue;
end;

function TPasParser.CreateBoolConstExpr(AParent: TPasElement;
  AKind: TPasExprKind; const ABoolValue: Boolean): TBoolConstExpr;
begin
  Result:=TBoolConstExpr(CreateElement(TBoolConstExpr,'',AParent));
  Result.Kind:=AKind;
  Result.Value:=ABoolValue;
end;

function TPasParser.CreateBinaryExpr(AParent: TPasElement; xleft,
  xright: TPasExpr; AOpCode: TExprOpCode): TBinaryExpr;
begin
  Result:=TBinaryExpr(CreateElement(TBinaryExpr,'',AParent));
  Result.OpCode:=AOpCode;
  Result.Kind:=pekBinary;
  if xleft<>nil then
    begin
    Result.left:=xleft;
    xleft.Parent:=Result;
    end;
  if xright<>nil then
    begin
    Result.right:=xright;
    xright.Parent:=Result;
    end;
end;

procedure TPasParser.AddToBinaryExprChain(var ChainFirst: TPasExpr;
  Element: TPasExpr; AOpCode: TExprOpCode);
begin
  if Element=nil then
    exit
  else if ChainFirst=nil then
    begin
    // empty chain => simply add element, no need to create TBinaryExpr
    ChainFirst:=Element;
    end
  else
    begin
    // create new binary, old becomes left, Element right
    ChainFirst:=CreateBinaryExpr(ChainFirst.Parent,ChainFirst,Element,AOpCode);
    end;
end;

procedure TPasParser.AddParamsToBinaryExprChain(var ChainFirst: TPasExpr;
  Params: TParamsExpr);
// append Params to chain, using the last(right) element as Params.Value
var
  Bin: TBinaryExpr;
begin
  if Params.Value<>nil then
    ParseExcSyntaxError;
  if ChainFirst=nil then
    ParseExcSyntaxError;
  if ChainFirst is TBinaryExpr then
    begin
    Bin:=TBinaryExpr(ChainFirst);
    if Bin.left=nil then
      ParseExcSyntaxError;
    if Bin.right=nil then
      ParseExcSyntaxError;
    Params.Value:=Bin.right;
    Params.Value.Parent:=Params;
    Bin.right:=Params;
    Params.Parent:=Bin;
    end
  else
    begin
    Params.Value:=ChainFirst;
    Params.Parent:=ChainFirst.Parent;
    ChainFirst.Parent:=Params;
    ChainFirst:=Params;
    end;
end;

{$IFDEF VerbosePasParser}
procedure TPasParser.WriteBinaryExprChain(Prefix: string; First, Last: TPasExpr
  );
var
  i: Integer;
begin
  if First=nil then
    begin
    write(Prefix,'First=nil');
    if Last=nil then
      writeln('=Last')
    else
      begin
      writeln(', ERROR Last=',Last.ClassName);
      ParseExcSyntaxError;
      end;
    end
  else if Last=nil then
    begin
    writeln(Prefix,'ERROR Last=nil First=',First.ClassName);
    ParseExcSyntaxError;
    end
  else if First is TBinaryExpr then
    begin
    i:=0;
    while First is TBinaryExpr do
      begin
      writeln(Prefix,Space(i*2),'bin.left=',TBinaryExpr(First).left.ClassName);
      if First=Last then break;
      First:=TBinaryExpr(First).right;
      inc(i);
      end;
    if First<>Last then
      begin
      writeln(Prefix,Space(i*2),'ERROR Last is not last in chain');
      ParseExcSyntaxError;
      end;
    if not (Last is TBinaryExpr) then
      begin
      writeln(Prefix,Space(i*2),'ERROR Last is not TBinaryExpr: ',Last.ClassName);
      ParseExcSyntaxError;
      end;
    if TBinaryExpr(Last).right=nil then
      begin
      writeln(Prefix,Space(i*2),'ERROR Last.right=nil');
      ParseExcSyntaxError;
      end;
    writeln(Prefix,Space(i*2),'last.right=',TBinaryExpr(Last).right.ClassName);
    end
  else if First=Last then
    writeln(Prefix,'First=Last=',First.ClassName)
  else
    begin
    write(Prefix,'ERROR First=',First.ClassName);
    if Last<>nil then
      writeln(' Last=',Last.ClassName)
    else
      writeln(' Last=nil');
    end;
end;
{$ENDIF}

function TPasParser.CreateUnaryExpr(AParent: TPasElement; AOperand: TPasExpr;
  AOpCode: TExprOpCode): TUnaryExpr;
begin
  Result:=TUnaryExpr(CreateElement(TUnaryExpr,'',AParent));
  Result.Kind:=pekUnary;
  Result.Operand:=AOperand;
  Result.Operand.Parent:=Result;
  Result.OpCode:=AOpCode;
end;

function TPasParser.CreateArrayValues(AParent: TPasElement): TArrayValues;
begin
  Result:=TArrayValues(CreateElement(TArrayValues,'',AParent));
  Result.Kind:=pekListOfExp;
end;

function TPasParser.CreateFunctionType(const AName, AResultName: String;
  AParent: TPasElement; UseParentAsResultParent: Boolean;
  const NamePos: TPasSourcePos): TPasFunctionType;
begin
  Result:=Engine.CreateFunctionType(AName,AResultName,
                                    AParent,UseParentAsResultParent,
                                    NamePos);
end;

function TPasParser.CreateInheritedExpr(AParent: TPasElement): TInheritedExpr;
begin
  Result:=TInheritedExpr(CreateElement(TInheritedExpr,'',AParent));
  Result.Kind:=pekInherited;
end;

function TPasParser.CreateSelfExpr(AParent: TPasElement): TSelfExpr;
begin
  Result:=TSelfExpr(CreateElement(TSelfExpr,'Self',AParent));
  Result.Kind:=pekSelf;
end;

function TPasParser.CreateNilExpr(AParent: TPasElement): TNilExpr;
begin
  Result:=TNilExpr(CreateElement(TNilExpr,'nil',AParent));
  Result.Kind:=pekNil;
end;

function TPasParser.CreateRecordValues(AParent: TPasElement): TRecordValues;
begin
  Result:=TRecordValues(CreateElement(TRecordValues,'',AParent));
  Result.Kind:=pekListOfExp;
end;

end.

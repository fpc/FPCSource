{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Abstract:
  Write and read a precompiled module (pju).

  Default format is gzipped json

  Store whole unit, except all
    procedure declarations, proc bodies, finalization/initialization sections are
    replaced by
    -precompiled code
    -lists of references
    -local consts
  The useanalyzer needs the references - TPas2jsUseAnalyzer.

  Due to uses cycles, ability to stop read after interface uses and implementation uses
  Needs function to find out where it stopped, and a procedure ReadContinue
}
unit Pas2JsFiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, contnrs, AVL_Tree, crc,
  fpjson, jsonparser, jsonscanner,
  PasTree, PScanner, PParser, PasResolveEval, PasResolver,
  Pas2jsFileUtils, FPPas2Js;

const
  PJUMagic = 'Pas2JSCache';
  PJUVersion = 1;

  PJUDefaultParserOptions: TPOptions = po_Pas2js;

  PJUBoolStr: array[boolean] of string = (
    'False',
    'True'
    );

  PJUParserOptionNames: array[TPOption] of string = (
    'delphi',
    'KeepScannerError',
    'CAssignments',
    'ResolveStandardTypes',
    'AsmWhole',
    'NoOverloadedProcs',
    'KeepClassForward',
    'ArrayRangeExpr',
    'SelfToken',
    'CheckModeSwitches',
    'CheckCondFunction',
    'StopOnErrorDirective',
    'ExtClassConstWithoutExpr');

  PJUDefaultModeSwitches: TModeSwitches = [
    msObjfpc,
    msClass,
    msResult,
    msNestedComment,
    msRepeatForward,
    msInitFinal,
    msOut,
    msDefaultPara,
    msHintDirective,
    msProperty,
    msExcept,
    msDefaultUnicodestring,
    msCBlocks];

  PJUModeSwitchNames: array[TModeSwitch] of string = (
    'None',
    'Fpc',
    'Objfpc',
    'Delphi',
    'DelphiUnicode',
    'TP7',
    'Mac',
    'Iso',
    'Extpas',
    'GPC',
    'Class',
    'Objpas',
    'Result',
    'StringPchar',
    'CVarSupport',
    'NestedComment',
    'TPProcVar',
    'MacProcVar',
    'RepeatForward',
    'Pointer2Procedure',
    'AutoDeref',
    'InitFinal',
    'DefaultAnsistring',
    'Out',
    'DefaultPara',
    'HintDirective',
    'DuplicateNames',
    'Property',
    'DefaultInline',
    'Except',
    'ObjectiveC1',
    'ObjectiveC2',
    'NestedProcVars',
    'NonLocalGoto',
    'AdvancedRecords',
    'ISOLikeUnaryMinus',
    'SystemCodePage',
    'FinalFields',
    'DefaultUnicodestring',
    'TypeHelpers',
    'CBlocks',
    'ISOLikeIO',
    'ISOLikeProgramsPara',
    'ISOLikeMod',
    'ExternalClass',
    'PrefixedAttributes',
    'IgnoreInterfaces',
    'IgnoreAttributes'
    );

  PJUDefaultBoolSwitches: TBoolSwitches = [
    bsHints,
    bsNotes,
    bsWarnings
    ];
  PJUBoolSwitchNames: array[TBoolSwitch] of string = (
    'None',
    'Align',
    'BoolEval',
    'Assertions',
    'DebugInfo',
    'Extension',
    'ImportedData',
    'LongStrings',
    'IOChecks',
    'WriteableConst',
    'LocalSymbols',
    'TypeInfo',
    'Optimization',
    'OpenStrings',
    'OverflowChecks',
    'RangeChecks',
    'TypedAddress',
    'SafeDivide',
    'VarStringChecks',
    'Stackframes',
    'ExtendedSyntax',
    'ReferenceInfo',
    'Hints',
    'Notes',
    'Warnings',
    'Macro',
    'ScopedEnums',
    'ObjectChecks'
    );

  PJUDefaultConvertOptions: TPasToJsConverterOptions = [];
  PJUConverterOptions: array[TPasToJsConverterOption] of string = (
    'LowerCase',
    'SwitchStatement',
    'EnumNumbers',
    'UseStrict',
    'NoTypeInfo',
    'EliminateDeadCode'
    );

  PJUDefaultTargetPlatform = PlatformBrowser;
  PJUTargetPlatformNames: array[TPasToJsPlatform] of string = (
    'Browser',
    'NodeJS'
    );

  PJUDefaultTargetProcessor = ProcessorECMAScript5;
  PJUTargetProcessorNames: array[TPasToJsProcessor] of string = (
    'ECMAScript5',
    'ECMAScript6'
    );

  PJUMemberVisibilityNames: array[TPasMemberVisibility] of string = (
    'Default',
    'Private',
    'Protected',
    'Public',
    'Published',
    'Automated',
    'StrictPrivate',
    'StrictProtected'
    );

  PJUMemberHintNames: array[TPasMemberHint] of string = (
    'Deprecated',
    'Library',
    'Platform',
    'Experimental',
    'Unimplemented'
    );

  PJUDefaultModuleScopeFlags = [pmsfRangeErrorSearched];
  PJUModuleScopeFlagNames: array[TPasModuleScopeFlag] of string = (
    'AssertSearched',
    'RangeErrorNeeded',
    'RangeErrorSearched'
    ) ;

  PJUDefaultIdentifierKind = pikSimple;
  PJUIdentifierKindNames: array[TPasIdentifierKind] of string = (
    'None',
    'BaseType',
    'BuiltInProc',
    'Simple',
    'Proc',
    'Namespace'
    );

  PJUVarModifierNames: array[TVariableModifier] of string = (
    'CVar',
    'External',
    'Public',
    'Export',
    'Class',
    'Static'
    );

  PJUDefaultExprKind = pekIdent;
  PJUExprKindNames: array[TPasExprKind] of string = (
    'Ident',
    'Number',
    'String',
    'Set',
    'Nil',
    'Bool',
    'Range',
    'Unary',
    'Binary',
    'Func',
    'Array',
    'List',
    'Inherited',
    'Self'
    );

  PJUExprOpCodeNames: array[TExprOpCode] of string = (
    'None',
    'Add',
    'Sub',
    'Mul',
    'DivF',
    'DivI',
    'Mod',
    'Pow',
    'Shr',
    'Shl',
    'Not',
    'And',
    'Or',
    'Xor',
    'Eq',
    'NE',
    'LT',
    'GT',
    'LTE',
    'GTE',
    'In',
    'Is',
    'As',
    'SymDif',
    'Addr',
    'Deref',
    'MemAddr',
    'SubId'
    );

  PJUPackModeNames: array[TPackMode] of string = (
    'None',
    'Packed',
    'BitPacked'
    );

  PJUObjKindNames: array[TPasObjKind] of string = (
    'Object',
    'Class',
    'Interface',
    'Generic',
    'ClassHelper',
    'RecordHelper',
    'TypeHelper',
    'DispInterface'
    );

  PJUClassScopeFlagNames: array[TPasClassScopeFlag] of string = (
    'AncestorResolved',
    'Sealed',
    'Published'
    );

  PJUArgumentAccessNames: array[TArgumentAccess] of string = (
    'Default',
    'Const',
    'Var',
    'Out',
    'ConstRef'
    );

  PJUCallingConventionNames: array[TCallingConvention] of string = (
    'Default',
    'Register',
    'Pascal',
    'CDecl',
    'StdCall',
    'OldFPCCall',
    'SafeCall',
    'SysCall'
    );

  PJUProcTypeModifierNames: array[TProcTypeModifier] of string = (
    'OfObject',
    'IsNested',
    'Static',
    'Varargs',
    'ReferenceTo'
    );

  PJUProcedureMessageTypeNames: array[TProcedureMessageType] of string = (
    'None',
    'Integer',
    'String'
    );

  PJUOperatorTypeNames: array[TOperatorType] of string = (
    'Unknown',
    'Implicit',
    'Explicit',
    'Mul',
    'Plus',
    'Minus',
    'Division',
    'LessThan',
    'Equal',
    'GreaterThan',
    'Assign',
    'NotEqual',
    'LessEqualThan',
    'GreaterEqualThan',
    'Power',
    'SymmetricalDifference',
    'Inc',
    'Dec',
    'Mod',
    'Negative',
    'Positive',
    'BitWiseOr',
    'Div',
    'LeftShift',
    'LogicalOr',
    'BitwiseAnd',
    'bitwiseXor',
    'LogicalAnd',
    'LogicalNot',
    'LogicalXor',
    'RightShift',
    'Enumerator'
    );

  PJUProcedureModifierNames: array[TProcedureModifier] of string = (
    'Virtual',
    'Dynamic',
    'Abstract',
    'Override',
    'Export',
    'Overload',
    'Message',
    'Reintroduce',
    'Inline',
    'Assembler',
    'Public',
    'CompilerProc',
    'External',
    'Forward',
    'DispId',
    'NoReturn',
    'Far',
    'Final'
    );

  PJUProcedureScopeFlagNames: array[TPasProcedureScopeFlag] of string = (
    'GrpOverload'
    );

  PJUResolvedRefAccessNames: array[TResolvedRefAccess] of string = (
    'None',
    'Read',
    'Assign',
    'ReadAndAssign',
    'VarParam',
    'OutParam',
    'ParamToUnknownProc'
    );

type
  { TPJUInitialFlags }

  TPJUInitialFlags = class
  public
    ParserOptions: TPOptions;
    ModeSwitches: TModeSwitches;
    BoolSwitches: TBoolSwitches;
    ConverterOptions: TPasToJsConverterOptions;
    TargetPlatform: TPasToJsPlatform;
    TargetProcessor: TPasToJsProcessor;
    // ToDo: defines
    constructor Create;
    procedure Clear;
  end;

type
  TPJUSourceFileType = (
    sftUnit,
    sftInclude
  );
  TPJUSourceFileKinds = set of TPJUSourceFileType;
const
  PJUSourceFileTypeNames: array[TPJUSourceFileType] of string = (
    'Unit',
    'Include'
    );

type
  TPJUSourceFileChecksum = cardinal;
  EPas2JsFilerError = class(Exception)
  public
    Owner: TObject;
  end;
  EPas2JsWriteError = class(EPas2JsFilerError);
  EPas2JsReadError = class(EPas2JsFilerError);

  { TPJUSourceFile }

  TPJUSourceFile = class
  public
    FileType: TPJUSourceFileType;
    Filename: string;
    Checksum: TPJUSourceFileChecksum;
    Index: integer;
  end;
  TPJUSourceFileArray = array of TPJUSourceFile;

  TPJUGetSrcEvent = procedure(Sender: TObject; aFilename: string;
    out p: PChar; out Count: integer) of object;

  { TPJUFilerContext - base class TPJUWriterContext/TPJUReaderContext }

  TPJUFilerContext = class
  public
    LastElement: TPasElement;
    ModeSwitches: TModeSwitches;
    BoolSwitches: TBoolSwitches;
  end;

  { TPJUFilerPendingElRef }

  TPJUFilerPendingElRef = class
  public
    Next: TPJUFilerPendingElRef;
    ErrorEl: TPasElement;
  end;

  { TPJUFilerElementRef }

  TPJUFilerElementRef = class
  public
    Element: TPasElement;
    Id: integer; // 0 = pending
    Pending: TPJUFilerPendingElRef;
    Obj: TJSONObject;
    procedure AddPending(Item: TPJUFilerPendingElRef);
    procedure Clear;
    destructor Destroy; override;
  end;
  TPJUFilerElementRefArray = array of TPJUFilerElementRef;

  { TPJUFiler - base class TPJUWriter/TPJUReader}

  TPJUFiler = class
  private
    FElementRefs: TAVLTree; // tree of TPJUFilerElementRef sorted for Element
    FInitialFlags: TPJUInitialFlags;
    FOnGetSrc: TPJUGetSrcEvent;
    FParser: TPasParser;
    FResolver: TPas2JSResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
    function GetSourceFiles(Index: integer): TPJUSourceFile;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = ''); virtual; abstract; overload;
    procedure RaiseMsg(Id: int64; El: TPasElement; const Msg: string = ''); overload;
    function GetDefaultMemberVisibility(El, LastElement: TPasElement): TPasMemberVisibility; virtual;
    function GetDefaultPasScopeVisibilityContext(Scope: TPasScope): TPasElement; virtual;
    procedure GetDefaultsPasIdentifierProps(El: TPasElement; out Kind: TPasIdentifierKind; out Name: string); virtual;
    function GetDefaultClassScopeFlags(Scope: TPas2JSClassScope): TPasClassScopeFlags; virtual;
    function GetDefaultProcModifiers(Proc: TPasProcedure): TProcedureModifiers; virtual;
    function GetDefaultProcTypeModifiers(Proc: TPasProcedureType): TProcTypeModifiers; virtual;
    function GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum; virtual;
    function GetElementReference(El: TPasElement; AutoCreate: boolean = true): TPJUFilerElementRef;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Resolver: TPas2JSResolver read FResolver;
    property Parser: TPasParser read FParser;
    property Scanner: TPascalScanner read FScanner;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
    property OnGetSrc: TPJUGetSrcEvent read FOnGetSrc write FOnGetSrc;
    function SourceFileCount: integer;
    property SourceFiles[Index: integer]: TPJUSourceFile read GetSourceFiles;
    property ElementRefs: TAVLTree read FElementRefs;
  end;

  { TPJUWriterContext }

  TPJUWriterContext = class(TPJUFilerContext)
  public
  end;

  { TPJUWriterPendingElRefObj }

  TPJUWriterPendingElRefObj = class(TPJUFilerPendingElRef)
  public
    Obj: TJSONObject;
    PropName: string;
  end;

  { TPJUWriterPendingElRefArray }

  TPJUWriterPendingElRefArray = class(TPJUFilerPendingElRef)
  public
    Arr: TJSONArray;
    Index: integer;
  end;

  { TPJUWriter }

  TPJUWriter = class(TPJUFiler)
  private
    FElementIdCounter: integer;
    FSourceFilesSorted: TPJUSourceFileArray;
    FInImplementation: boolean;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = ''); override; overload;
    procedure ResolvePendingElRefs(Ref: TPJUFilerElementRef);
    function CheckElScope(El: TPasElement; NotNilId: int64; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure AddArrayFlag(Obj: TJSONObject; var Arr: TJSONArray;
      const ArrName, Flag: string; Enable: boolean);
    procedure AddReferenceToArray(Arr: TJSONArray; El: TPasElement); virtual;
    procedure AddReferenceToObj(Obj: TJSONObject; const PropName: string; El: TPasElement); virtual;
    procedure CreateElReferenceId(Ref: TPJUFilerElementRef); virtual;
    procedure WriteHeaderMagic(Obj: TJSONObject); virtual;
    procedure WriteHeaderVersion(Obj: TJSONObject); virtual;
    procedure WriteInitialFlags(Obj: TJSONObject); virtual;
    procedure WriteParserOptions(Obj: TJSONObject; const Value, DefaultValue: TPOptions); virtual;
    procedure WriteModeSwitches(Obj: TJSONObject; const Value, DefaultValue: TModeSwitches); virtual;
    procedure WriteBoolSwitches(Obj: TJSONObject; const Value, DefaultValue: TBoolSwitches); virtual;
    procedure WriteConvertOptions(Obj: TJSONObject; const Value, DefaultValue: TPasToJsConverterOptions); virtual;
    procedure WriteSrcFiles(Obj: TJSONObject); virtual;
    procedure WriteMemberHints(Obj: TJSONObject; const Value, DefaultValue: TPasMemberHints); virtual;
    procedure WritePasScope(Obj: TJSONObject; Scope: TPasScope; aContext: TPJUWriterContext); virtual;
    procedure WriteIdentifierScope(Obj: TJSONObject; Scope: TPasIdentifierScope; aContext: TPJUWriterContext); virtual;
    procedure WriteModuleScopeFlags(Obj: TJSONObject; const Value, DefaultValue: TPasModuleScopeFlags); virtual;
    procedure WriteModuleScope(Obj: TJSONObject; Scope: TPasModuleScope; aContext: TPJUWriterContext); virtual;
    procedure WritePasElement(Obj: TJSONObject; El: TPasElement; aContext: TPJUWriterContext); virtual;
    procedure WriteModule(Obj: TJSONObject; aModule: TPasModule; aContext: TPJUWriterContext); virtual;
    procedure WriteSection(ParentJSON: TJSONObject; Section: TPasSection;
      const PropName: string; aContext: TPJUWriterContext); virtual;
    procedure WriteDeclarations(ParentJSON: TJSONObject; Decls: TPasDeclarations; aContext: TPJUWriterContext); virtual;
    procedure WriteElementProperty(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; El: TPasElement; aContext: TPJUWriterContext); virtual;
    procedure WriteElementList(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; ListOfElements: TFPList; aContext: TPJUWriterContext;
      ReferencesAllowed: boolean = false); virtual;
    procedure WriteElement(Obj: TJSONObject; El: TPasElement; aContext: TPJUWriterContext); virtual;
    procedure WriteElType(Obj: TJSONObject; El: TPasElement; const PropName: string; aType: TPasType; aContext: TPJUWriterContext); virtual;
    procedure WriteVarModifiers(Obj: TJSONObject; const PropName: string; const Value, DefaultValue: TVariableModifiers); virtual;
    procedure WriteExpr(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; Expr: TPasExpr; aContext: TPJUWriterContext); virtual;
    procedure WritePasExpr(Obj: TJSONObject; Expr: TPasExpr;
      WriteKind: boolean; DefaultOpCode: TExprOpCode; aContext: TPJUWriterContext); virtual;
    procedure WritePasExprArray(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; const ExprArr: TPasExprArray; aContext: TPJUWriterContext); virtual;
    procedure WriteUnaryExpr(Obj: TJSONObject; Expr: TUnaryExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteBinaryExpr(Obj: TJSONObject; Expr: TBinaryExpr; aContext: TPJUWriterContext); virtual;
    procedure WritePrimitiveExpr(Obj: TJSONObject; Expr: TPrimitiveExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteBoolConstExpr(Obj: TJSONObject; Expr: TBoolConstExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteParamsExpr(Obj: TJSONObject; Expr: TParamsExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteRecordValues(Obj: TJSONObject; Expr: TRecordValues; aContext: TPJUWriterContext); virtual;
    procedure WriteArrayValues(Obj: TJSONObject; Expr: TArrayValues; aContext: TPJUWriterContext); virtual;
    procedure WriteResString(Obj: TJSONObject; El: TPasResString; aContext: TPJUWriterContext); virtual;
    procedure WriteAliasType(Obj: TJSONObject; El: TPasAliasType; aContext: TPJUWriterContext); virtual;
    procedure WritePointerType(Obj: TJSONObject; El: TPasPointerType; aContext: TPJUWriterContext); virtual;
    procedure WriteSpecializeType(Obj: TJSONObject; El: TPasSpecializeType; aContext: TPJUWriterContext); virtual;
    procedure WriteInlineTypeExpr(Obj: TJSONObject; Expr: TInlineTypeExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteInlineSpecializeExpr(Obj: TJSONObject; Expr: TInlineSpecializeExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteArrayType(Obj: TJSONObject; El: TPasArrayType; aContext: TPJUWriterContext); virtual;
    procedure WriteFileType(Obj: TJSONObject; El: TPasFileType; aContext: TPJUWriterContext); virtual;
    procedure WriteEnumValue(Obj: TJSONObject; El: TPasEnumValue; aContext: TPJUWriterContext); virtual;
    procedure WriteEnumTypeScope(Obj: TJSONObject; Scope: TPasEnumTypeScope; aContext: TPJUWriterContext); virtual;
    procedure WriteEnumType(Obj: TJSONObject; El: TPasEnumType; aContext: TPJUWriterContext); virtual;
    procedure WriteSetType(Obj: TJSONObject; El: TPasSetType; aContext: TPJUWriterContext); virtual;
    procedure WriteRecordVariant(Obj: TJSONObject; El: TPasVariant; aContext: TPJUWriterContext); virtual;
    procedure WriteRecordTypeScope(Obj: TJSONObject; Scope: TPasRecordScope; aContext: TPJUWriterContext); virtual;
    procedure WriteRecordType(Obj: TJSONObject; El: TPasRecordType; aContext: TPJUWriterContext); virtual;
    procedure WriteClassScopeFlags(Obj: TJSONObject; const PropName: string; const Value, DefaultValue: TPasClassScopeFlags); virtual;
    procedure WriteClassScope(Obj: TJSONObject; Scope: TPas2JSClassScope; aContext: TPJUWriterContext); virtual;
    procedure WriteClassType(Obj: TJSONObject; El: TPasClassType; aContext: TPJUWriterContext); virtual;
    procedure WriteArgument(Obj: TJSONObject; El: TPasArgument; aContext: TPJUWriterContext); virtual;
    procedure WriteProcTypeModifiers(Obj: TJSONObject; const PropName: string; const Value, DefaultValue: TProcTypeModifiers); virtual;
    procedure WriteProcedureType(Obj: TJSONObject; El: TPasProcedureType; aContext: TPJUWriterContext); virtual;
    procedure WriteResultElement(Obj: TJSONObject; El: TPasResultElement; aContext: TPJUWriterContext); virtual;
    procedure WriteFunctionType(Obj: TJSONObject; El: TPasFunctionType; aContext: TPJUWriterContext); virtual;
    procedure WriteStringType(Obj: TJSONObject; El: TPasStringType; aContext: TPJUWriterContext); virtual;
    procedure WriteVariable(Obj: TJSONObject; El: TPasVariable; aContext: TPJUWriterContext); virtual;
    procedure WriteExportSymbol(Obj: TJSONObject; El: TPasExportSymbol; aContext: TPJUWriterContext); virtual;
    procedure WriteConst(Obj: TJSONObject; El: TPasConst; aContext: TPJUWriterContext); virtual;
    procedure WritePropertyScope(Obj: TJSONObject; Scope: TPasPropertyScope; aContext: TPJUWriterContext); virtual;
    procedure WriteProperty(Obj: TJSONObject; El: TPasProperty; aContext: TPJUWriterContext); virtual;
    procedure WriteProcedureModifiers(Obj: TJSONObject; const PropName: string; const Value, DefaultValue: TProcedureModifiers); virtual;
    procedure WriteProcScopeFlags(Obj: TJSONObject; const PropName: string; const Value, DefaultValue: TPasProcedureScopeFlags); virtual;
    procedure WriteProcedureScope(Obj: TJSONObject; Scope: TPas2JSProcedureScope; aContext: TPJUWriterContext); virtual;
    procedure WriteProcedure(Obj: TJSONObject; El: TPasProcedure; aContext: TPJUWriterContext); virtual;
    procedure WriteOperator(Obj: TJSONObject; El: TPasOperator; aContext: TPJUWriterContext); virtual;
    procedure WriteExternalReferences(ParentJSON: TJSONObject); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure WritePJU(aResolver: TPas2JSResolver;
      InitFlags: TPJUInitialFlags; aStream: TStream); virtual;
    function WriteJSON(aResolver: TPas2JSResolver;
      InitFlags: TPJUInitialFlags): TJSONObject; virtual;
    function IndexOfSourceFile(const Filename: string): integer;
    property SourceFilesSorted: TPJUSourceFileArray read FSourceFilesSorted;
  end;

  { TPJUReaderContext }

  TPJUReaderContext = class(TPJUFilerContext)
  end;

  TOnSetElReference = procedure(El: TPasElement; Data: TObject) of object;

  { TPJUReaderPendingElRef }

  TPJUReaderPendingElRef = class(TPJUFilerPendingElRef)
  public
    Data: TObject;
    Setter: TOnSetElReference;
  end;

  { TPJUReaderPendingElListRef }

  TPJUReaderPendingElListRef = class(TPJUFilerPendingElRef)
  public
    List: TFPList;
    Index: integer;
  end;

  { TPJUReaderPendingIdentifierScope }

  TPJUReaderPendingIdentifierScope = class
  public
    Scope: TPasIdentifierScope;
    Arr: TJSONArray;
  end;

  { TPJUReader }

  TPJUReader = class(TPJUFiler)
  private
    FElementRefsArray: TPJUFilerElementRefArray; // TPJUFilerElementRef by Id
    FFileVersion: longint;
    FPendingIdentifierScopes: TObjectList; // list of TPJUReaderPendingIdentifierScope
    procedure Set_Variable_VarType(RefEl: TPasElement; Data: TObject);
    procedure Set_AliasType_DestType(RefEl: TPasElement; Data: TObject);
    procedure Set_PointerType_DestType(RefEl: TPasElement; Data: TObject);
    procedure Set_InlineTypeExpr_DestType(RefEl: TPasElement; Data: TObject);
    procedure Set_ArrayType_ElType(RefEl: TPasElement; Data: TObject);
    procedure Set_FileType_ElType(RefEl: TPasElement; Data: TObject);
    procedure Set_SetType_EnumType(RefEl: TPasElement; Data: TObject);
    procedure Set_Variant_Members(RefEl: TPasElement; Data: TObject);
    procedure Set_RecordType_VariantEl(RefEl: TPasElement; Data: TObject);
    procedure Set_Argument_ArgType(RefEl: TPasElement; Data: TObject);
    procedure Set_ClassScope_NewInstanceFunction(RefEl: TPasElement; Data: TObject);
    procedure Set_ClassScope_DirectAncestor(RefEl: TPasElement; Data: TObject);
    procedure Set_ClassScope_DefaultProperty(RefEl: TPasElement; Data: TObject);
    procedure Set_ClassType_AncestorType(RefEl: TPasElement; Data: TObject);
    procedure Set_ClassType_HelperForType(RefEl: TPasElement; Data: TObject);
    procedure Set_ResultElement_ResultType(RefEl: TPasElement; Data: TObject);
    procedure Set_PasScope_VisibilityContext(RefEl: TPasElement; Data: TObject);
    procedure Set_ModScope_AssertClass(RefEl: TPasElement; Data: TObject);
    procedure Set_ModScope_AssertDefConstructor(RefEl: TPasElement; Data: TObject);
    procedure Set_ModScope_AssertMsgConstructor(RefEl: TPasElement; Data: TObject);
    procedure Set_ModScope_RangeErrorClass(RefEl: TPasElement; Data: TObject);
    procedure Set_ModScope_RangeErrorConstructor(RefEl: TPasElement; Data: TObject);
    procedure Set_PropertyScope_AncestorProp(RefEl: TPasElement; Data: TObject);
    procedure Set_ProcedureScope_Overridden(RefEl: TPasElement; Data: TObject);
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = ''); overload; override;
    function CheckJSONArray(Data: TJSONData; El: TPasElement; const PropName: string): TJSONArray;
    function CheckJSONObject(Data: TJSONData; Id: int64): TJSONObject;
    function CheckJSONString(Data: TJSONData; Id: int64): String;
    function ReadString(Obj: TJSONObject; const PropName: string; out s: string; El: TPasElement): boolean;
    function ReadInteger(Obj: TJSONObject; const PropName: string; out i: integer; El: TPasElement): boolean;
    function ReadBoolean(Obj: TJSONObject; const PropName: string; out b: boolean; El: TPasElement): boolean;
    function ReadArray(Obj: TJSONObject; const PropName: string; out Arr: TJSONArray; El: TPasElement): boolean;
    function ReadObject(Obj: TJSONObject; const PropName: string; out SubObj: TJSONObject; El: TPasElement): boolean;
    function AddElReference(Id: integer; ErrorEl: TPasElement; El: TPasElement): TPJUFilerElementRef; virtual;
    procedure PromiseSetElReference(Id: integer; const Setter: TOnSetElReference; Data: TObject; ErrorEl: TPasElement); virtual;
    procedure PromiseSetElListReference(Id: integer; List: TFPList; Index: integer; ErrorEl: TPasElement); virtual;
    procedure ReadHeaderMagic(Obj: TJSONObject); virtual;
    procedure ReadHeaderVersion(Obj: TJSONObject); virtual;
    procedure ReadArrayFlags(Data: TJSONData; El: TPasElement; const PropName: string; out Names: TStringDynArray; out Enable: TBooleanDynArray);
    function ReadParserOptions(Data: TJSONData; El: TPasElement; const DefaultValue: TPOptions): TPOptions; virtual;
    function ReadModeSwitches(Data: TJSONData; El: TPasElement; const DefaultValue: TModeSwitches): TModeSwitches; virtual;
    function ReadBoolSwitches(Data: TJSONData; El: TPasElement; const DefaultValue: TBoolSwitches): TBoolSwitches; virtual;
    function ReadConverterOptions(Data: TJSONData; El: TPasElement; const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions; virtual;
    procedure ReadTargetPlatform(Data: TJSONData); virtual;
    procedure ReadTargetProcessor(Data: TJSONData); virtual;
    procedure ReadSrcFiles(Data: TJSONData); virtual;
    function ReadMemberHints(Obj: TJSONObject; El: TPasElement; const DefaultValue: TPasMemberHints): TPasMemberHints; virtual;
    procedure ReadPasElement(Obj: TJSONObject; El: TPasElement; aContext: TPJUReaderContext); virtual;
    procedure ReadSectionScope(Obj: TJSONObject; Scope: TPasSectionScope; aContext: TPJUReaderContext); virtual;
    procedure ReadSection(Obj: TJSONObject; Section: TPasSection; aContext: TPJUReaderContext); virtual;
    procedure ReadDeclarations(Obj: TJSONObject; Section: TPasSection; aContext: TPJUReaderContext); virtual;
    procedure ReadDeclaration(Obj: TJSONObject; Section: TPasSection; aContext: TPJUReaderContext); virtual;
    function ReadElement(Obj: TJSONObject; Parent: TPasElement; aContext: TPJUReaderContext): TPasElement; virtual;
    function ReadElementProperty(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; BaseClass: TPTreeElement; aContext: TPJUReaderContext): TPasElement; virtual;
    procedure ReadElementReference(Obj: TJSONObject; Instance: TPasElementBase;
      const PropName: string; const Setter: TOnSetElReference); virtual;
    procedure ReadElementList(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; ListOfElements: TFPList; aContext: TPJUReaderContext); virtual;
    procedure ReadElType(Obj: TJSONObject; const PropName: string; El: TPasElement;
      const Setter: TOnSetElReference; aContext: TPJUReaderContext); virtual;
    function ReadExpr(Obj: TJSONObject; Parent: TPasElement; const PropName: string;
      aContext: TPJUReaderContext): TPasExpr; virtual;
    procedure ReadPasScope(Obj: TJSONObject; Scope: TPasScope; aContext: TPJUReaderContext); virtual;
    procedure ReadIdentifierScopeArray(Arr: TJSONArray; Scope: TPasIdentifierScope); virtual;
    procedure ReadIdentifierScope(Obj: TJSONObject; Scope: TPasIdentifierScope; aContext: TPJUReaderContext); virtual;
    function ReadModuleScopeFlags(Obj: TJSONObject; El: TPasElement; const DefaultValue: TPasModuleScopeFlags): TPasModuleScopeFlags; virtual;
    procedure ReadModuleScope(Obj: TJSONObject; Scope: TPasModuleScope; aContext: TPJUReaderContext); virtual;
    procedure ReadModule(Data: TJSONData; aContext: TPJUReaderContext); virtual;
    procedure ReadPasExpr(Obj: TJSONObject; Expr: TPasExpr; ReadKind: boolean; aContext: TPJUReaderContext); virtual;
    procedure ReadPasExprArray(Obj: TJSONObject; Parent: TPasElement;
      const PropName: string; var ExprArr: TPasExprArray; aContext: TPJUReaderContext); virtual;
    procedure ReadUnaryExpr(Obj: TJSONObject; Expr: TUnaryExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadBinaryExpr(Obj: TJSONObject; Expr: TBinaryExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadBoolConstExpr(Obj: TJSONObject; Expr: TBoolConstExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadParamsExpr(Obj: TJSONObject; Expr: TParamsExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadRecordValues(Obj: TJSONObject; Expr: TRecordValues; aContext: TPJUReaderContext); virtual;
    procedure ReadArrayValues(Obj: TJSONObject; Expr: TArrayValues; aContext: TPJUReaderContext); virtual;
    procedure ReadResString(Obj: TJSONObject; El: TPasResString; aContext: TPJUReaderContext); virtual;
    procedure ReadAliasType(Obj: TJSONObject; El: TPasAliasType; aContext: TPJUReaderContext); virtual;
    procedure ReadPointerType(Obj: TJSONObject; El: TPasPointerType; aContext: TPJUReaderContext); virtual;
    procedure ReadSpecializeType(Obj: TJSONObject; El: TPasSpecializeType; aContext: TPJUReaderContext); virtual;
    procedure ReadInlineTypeExpr(Obj: TJSONObject; Expr: TInlineTypeExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadInlineSpecializeExpr(Obj: TJSONObject; Expr: TInlineSpecializeExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadArrayType(Obj: TJSONObject; El: TPasArrayType; aContext: TPJUReaderContext); virtual;
    procedure ReadFileType(Obj: TJSONObject; El: TPasFileType; aContext: TPJUReaderContext); virtual;
    procedure ReadEnumValue(Obj: TJSONObject; El: TPasEnumValue; aContext: TPJUReaderContext); virtual;
    procedure ReadEnumTypeScope(Obj: TJSONObject; Scope: TPasEnumTypeScope; aContext: TPJUReaderContext); virtual;
    procedure ReadEnumType(Obj: TJSONObject; El: TPasEnumType; aContext: TPJUReaderContext); virtual;
    procedure ReadSetType(Obj: TJSONObject; El: TPasSetType; aContext: TPJUReaderContext); virtual;
    function ReadPackedMode(Obj: TJSONObject; const PropName: string; ErrorEl: TPasElement): TPackMode; virtual;
    procedure ReadRecordVariant(Obj: TJSONObject; El: TPasVariant; aContext: TPJUReaderContext); virtual;
    procedure ReadRecordScope(Obj: TJSONObject; Scope: TPasRecordScope; aContext: TPJUReaderContext); virtual;
    procedure ReadRecordType(Obj: TJSONObject; El: TPasRecordType; aContext: TPJUReaderContext); virtual;
    function ReadClassScopeFlags(Obj: TJSONObject; El: TPasElement;
      const PropName: string; const DefaultValue: TPasClassScopeFlags): TPasClassScopeFlags; virtual;
    procedure ReadClassScopeAbstractProcs(Obj: TJSONObject; Scope: TPas2JSClassScope); virtual;
    procedure ReadClassScope(Obj: TJSONObject; Scope: TPas2JSClassScope; aContext: TPJUReaderContext); virtual;
    procedure ReadClassType(Obj: TJSONObject; El: TPasClassType; aContext: TPJUReaderContext); virtual;
    procedure ReadArgument(Obj: TJSONObject; El: TPasArgument; aContext: TPJUReaderContext); virtual;
    function ReadProcTypeModifiers(Obj: TJSONObject; El: TPasElement;
      const PropName: string; const DefaultValue: TProcTypeModifiers): TProcTypeModifiers; virtual;
    procedure ReadProcedureType(Obj: TJSONObject; El: TPasProcedureType; aContext: TPJUReaderContext); virtual;
    procedure ReadResultElement(Obj: TJSONObject; El: TPasResultElement; aContext: TPJUReaderContext); virtual;
    procedure ReadFunctionType(Obj: TJSONObject; El: TPasFunctionType; aContext: TPJUReaderContext); virtual;
    procedure ReadStringType(Obj: TJSONObject; El: TPasStringType; aContext: TPJUReaderContext); virtual;
    function ReadVarModifiers(Obj: TJSONObject; El: TPasElement;
      const PropName: string; const DefaultValue: TVariableModifiers): TVariableModifiers; virtual;
    procedure ReadVariable(Obj: TJSONObject; El: TPasVariable; aContext: TPJUReaderContext); virtual;
    procedure ReadExportSymbol(Obj: TJSONObject; El: TPasExportSymbol; aContext: TPJUReaderContext); virtual;
    procedure ReadConst(Obj: TJSONObject; El: TPasConst; aContext: TPJUReaderContext); virtual;
    procedure ReadPropertyScope(Obj: TJSONObject; Scope: TPasPropertyScope; aContext: TPJUReaderContext); virtual;
    procedure ReadProperty(Obj: TJSONObject; El: TPasProperty; aContext: TPJUReaderContext); virtual;
    function ReadProcedureModifiers(Obj: TJSONObject; El: TPasElement;
      const PropName: string; const DefaultValue: TProcedureModifiers): TProcedureModifiers; virtual;
    function ReadProcScopeFlags(Obj: TJSONObject; El: TPasElement;
      const PropName: string; const DefaultValue: TPasProcedureScopeFlags): TPasProcedureScopeFlags; virtual;
    procedure ReadProcedureScope(Obj: TJSONObject; Scope: TPas2JSProcedureScope; aContext: TPJUReaderContext); virtual;
    procedure ReadProcedure(Obj: TJSONObject; El: TPasProcedure; aContext: TPJUReaderContext); virtual;
    procedure ReadOperator(Obj: TJSONObject; El: TPasOperator; aContext: TPJUReaderContext); virtual;
    // ToDo: procedure ReadExternalReferences(ParentJSON: TJSONObject); virtual;
    procedure ResolvePending; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ReadPJU(aResolver: TPas2JSResolver; aStream: TStream); virtual;
    procedure ReadJSON(aResolver: TPas2JSResolver; Obj: TJSONObject); virtual;
    property FileVersion: longint read FFileVersion;
  end;

function ComparePointer(Data1, Data2: Pointer): integer;
function ComparePJUSrcFiles(File1, File2: Pointer): integer;
function ComparePJUFilerElementRef(Ref1, Ref2: Pointer): integer;
function CompareElWithPJUFilerElementRef(El, Ref: Pointer): integer;

function EncodeVLQ(i: MaxPrecInt): string; overload;
function EncodeVLQ(i: MaxPrecUInt): string; overload;
function DecodeVLQ(const s: string): MaxPrecInt; // base256 Variable Length Quantity
function DecodeVLQ(var p: PByte): MaxPrecInt; // base256 Variable Length Quantity

function ComputeChecksum(p: PChar; Cnt: integer): TPJUSourceFileChecksum;

function ModeSwitchToInt(ms: TModeSwitch): byte;
function StrToPasIdentifierKind(const s: string): TPasIdentifierKind;

function dbgmem(const s: string): string; overload;
function dbgmem(p: PChar; Cnt: integer): string; overload;

implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function ComparePJUSrcFiles(File1, File2: Pointer): integer;
var
  Src1: TPJUSourceFile absolute File1;
  Src2: TPJUSourceFile absolute File2;
begin
  Result:=CompareStr(Src1.Filename,Src2.Filename);
end;

function ComparePJUFilerElementRef(Ref1, Ref2: Pointer): integer;
var
  Reference1: TPJUFilerElementRef absolute Ref1;
  Reference2: TPJUFilerElementRef absolute Ref2;
begin
  Result:=ComparePointer(Reference1.Element,Reference2.Element);
end;

function CompareElWithPJUFilerElementRef(El, Ref: Pointer): integer;
var
  Element: TPasElement absolute El;
  Reference: TPJUFilerElementRef absolute Ref;
begin
  Result:=ComparePointer(Element,Reference.Element);
end;

function EncodeVLQ(i: MaxPrecInt): string;
{ Convert signed number to base256-VLQ:
  Each byte has 8bit, where the least significant bit is the continuation bit
  (1=there is a next byte).
  The first byte contains the sign bit in the last bit
  and the 6 most significant bits of the number.
  For example:
  0 = %00000000 => 0
  1 = %00000001 => -0
  2 = %00000010 => 1
  130 5 = %10000010 %00000101 = 000010 0000101 = 100000101 = 133
}
var
  digits: integer;
begin
  digits:=0;
  if i<0 then
    begin
    if i=Low(MaxPrecInt) then
      begin
      Result:=EncodeVLQ(High(MaxPrecInt)+1);
      Result[1]:=chr(ord(Result[1]) or 1);
      exit;
      end;
    digits:=1;
    i:=-i;
    end;
  inc(digits,(i and %111111) shl 1);
  i:=i shr 6;
  if i>0 then
    inc(digits,%10000000); // need another byte -> set continuation bit
  Result:=chr(digits);
  while i>0 do
    begin
    digits:=i and %1111111;
    i:=i shr 7;
    if i>0 then
      inc(digits,%10000000); // need another byte -> set continuation bit
    Result:=Result+chr(digits);
    end;
end;

function EncodeVLQ(i: MaxPrecUInt): string;
var
  digits: integer;
begin
  digits:=(i and %111111) shl 1;
  if i>0 then
    inc(digits,%10000000); // need another byte -> set continuation bit
  Result:=chr(digits);
  i:=i shr 6;
  while i>0 do
    begin
    digits:=i and %1111111;
    i:=i shr 7;
    if i>0 then
      inc(digits,%10000000); // need another byte -> set continuation bit
    Result:=Result+chr(digits);
    end;
end;

function DecodeVLQ(const s: string): MaxPrecInt;
var
  p: PByte;
begin
  if s='' then
    raise EConvertError.Create('DecodeVLQ empty');
  p:=PByte(s);
  Result:=DecodeVLQ(p);
  if p-PByte(s)<>length(s) then
    raise EConvertError.Create('DecodeVLQ waste');
end;

function DecodeVLQ(var p: PByte): MaxPrecInt;
{ Convert base256-VLQ to signed number,
  For the fomat see EncodeVLQ
}

  procedure RaiseInvalid;
  begin
    raise ERangeError.Create('DecodeVLQ');
  end;

const
  MaxShift = 63; // actually log2(High(MaxPrecInt))
var
  digit, Shift: Integer;
  Negated: Boolean;
begin
  digit:=p^;
  inc(p);
  Negated:=(digit and 1)>0;
  Result:=(digit shr 1) and %111111;
  Shift:=6;
  while digit>=%10000000 do
    begin
    digit:=p^;
    inc(p);
    if Shift>MaxShift then
      RaiseInvalid;
    inc(Result,MaxPrecInt(digit and %1111111) shl Shift);
    inc(Shift,7);
    end;
  if Negated then
    Result:=-Result;
end;

function ComputeChecksum(p: PChar; Cnt: integer): TPJUSourceFileChecksum;
var
  SrcP, SrcEndP, SrcLineEndP, SrcLineStartP: PChar;
  l: PtrInt;
  CheckSum, CurLen: Cardinal;
begin
  if Cnt=0 then exit(0);

  // ignore trailing spaces and unify line endings
  SrcP:=p;
  SrcEndP:=p+Cnt;
  while (SrcEndP>SrcP) and (SrcEndP[-1] in [#9,#10,#13,' ']) do
    dec(SrcEndP);
  CheckSum:=crc32(0,nil,0);
  while SrcP<SrcEndP do
    begin
    SrcLineStartP:=SrcP;
    while (SrcP<SrcEndP) and not (SrcP^ in [#10,#13]) do
      inc(SrcP);
    SrcLineEndP:=SrcP;
    while (SrcLineEndP>SrcLineStartP) and (SrcLineEndP[-1] in [#9,' ']) do
      dec(SrcLineEndP);
    l:=SrcLineEndP-SrcLineStartP;
    while l>0 do
      begin
      if l<$8000 then
        CurLen:=l
      else
        CurLen:=$8000;
      CheckSum:=crc32(CheckSum, PByte(SrcLineStartP), CurLen);
      inc(SrcLineStartP,CurLen);
      dec(l,CurLen);
      end;
    while (SrcP<SrcEndP) and (SrcP^ in [#10,#13]) do
      inc(SrcP);
    end;
  Result:=CheckSum;
end;

function ModeSwitchToInt(ms: TModeSwitch): byte;
begin
  case ms of
    msNone: Result:=0;
    msFpc: Result:=1;
    msObjfpc: Result:=2;
    msDelphi: Result:=3;
    msDelphiUnicode: Result:=4;
    msTP7: Result:=5;
    msMac: Result:=6;
    msIso: Result:=7;
    msExtpas: Result:=8;
    msGPC: Result:=9;
    msClass: Result:=10;
    msObjpas: Result:=11;
    msResult: Result:=12;
    msStringPchar: Result:=13;
    msCVarSupport: Result:=14;
    msNestedComment: Result:=15;
    msTPProcVar: Result:=16;
    msMacProcVar: Result:=17;
    msRepeatForward: Result:=18;
    msPointer2Procedure: Result:=19;
    msAutoDeref: Result:=20;
    msInitFinal: Result:=21;
    msDefaultAnsistring: Result:=22;
    msOut: Result:=23;
    msDefaultPara: Result:=24;
    msHintDirective: Result:=25;
    msDuplicateNames: Result:=26;
    msProperty: Result:=27;
    msDefaultInline: Result:=28;
    msExcept: Result:=29;
    msObjectiveC1: Result:=30;
    msObjectiveC2: Result:=31;
    msNestedProcVars: Result:=32;
    msNonLocalGoto: Result:=33;
    msAdvancedRecords: Result:=34;
    msISOLikeUnaryMinus: Result:=35;
    msSystemCodePage: Result:=36;
    msFinalFields: Result:=37;
    msDefaultUnicodestring: Result:=38;
    msTypeHelpers: Result:=39;
    msCBlocks: Result:=40;
    msISOLikeIO: Result:=41;
    msISOLikeProgramsPara: Result:=42;
    msISOLikeMod: Result:=43;
    msExternalClass: Result:=44;
    msPrefixedAttributes: Result:=45;
    msIgnoreInterfaces: Result:=46;
    msIgnoreAttributes: Result:=47;
  end;
end;

function StrToPasIdentifierKind(const s: string): TPasIdentifierKind;
var
  Kind: TPasIdentifierKind;
begin
  for Kind in TPasIdentifierKind do
    if s=PJUIdentifierKindNames[Kind] then
      exit(Kind);
  Result:=pikNone;
end;

function dbgmem(const s: string): string;
begin
  if s='' then exit('');
  Result:=dbgmem(PChar(s),length(s));
end;

function dbgmem(p: PChar; Cnt: integer): string;

  procedure AddLine(const Line: string);
  begin
    if Result<>'' then
      Result:=Result+LineEnding;
    Result:=Result+Line;
  end;

var
  c: Char;
  IsTxt: boolean;
  Line: String;
  i: Integer;
begin
  Result:='';
  if (p=nil) or (Cnt<=0) then exit;
  Line:='';
  IsTxt:=false;
  for i:=0 to Cnt-1 do
    begin
    c:=p[i];
    if c in ['a'..'z','A'..'Z','_','/','0'..'9'] then
      begin
      if not IsTxt then
        begin
        Line:=Line+'''';
        IsTxt:=true;
        end;
      Line:=Line+c;
      end
    else
      begin
      if IsTxt then
        begin
        Line:=Line+'''';
        IsTxt:=false;
        end;
      Line:=Line+'#'+HexStr(ord(c),2);
      end;
    if length(Line)>78 then
      begin
      AddLine(Line);
      Line:='';
      end;
    end;
  if Line<>'' then
    AddLine(Line);
end;

{ TPJUFilerElementRef }

procedure TPJUFilerElementRef.AddPending(Item: TPJUFilerPendingElRef);
begin
  Item.Next:=Pending;
  Pending:=Item;
end;

procedure TPJUFilerElementRef.Clear;
var
  Ref, NextRef: TPJUFilerPendingElRef;
begin
  Ref:=Pending;
  while Ref<>nil do
    begin
    NextRef:=Ref.Next;
    Ref.Next:=nil;
    Ref.Free;
    Ref:=NextRef;
    end;
end;

destructor TPJUFilerElementRef.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TPJUFiler }

function TPJUFiler.GetSourceFiles(Index: integer): TPJUSourceFile;
begin
  Result:=TPJUSourceFile(FSourceFiles[Index]);
end;

procedure TPJUFiler.RaiseMsg(Id: int64; El: TPasElement; const Msg: string);
var
  Path, s: String;
begin
  Path:='';
  while El<>nil do
    begin
    if Path<>'' then Path:='.'+Path;
    s:=El.Name;
    if s='' then
      s:=El.ClassName;
    Path:=s+Path;
    El:=El.Parent;
    end;
  RaiseMsg(Id,Path+': '+Msg);
end;

function TPJUFiler.GetDefaultMemberVisibility(El, LastElement: TPasElement
  ): TPasMemberVisibility;
begin
  if El=nil then ;
  if LastElement<>nil then
    Result:=LastElement.Visibility
  else
    Result:=visDefault;
end;

function TPJUFiler.GetDefaultPasScopeVisibilityContext(Scope: TPasScope
  ): TPasElement;
var
  El: TPasElement;
begin
  El:=Scope.Element;
  if El is TPasClassType then
    Result:=El
  else if El is TPasModule then
    Result:=El
  else if (Scope is TPasProcedureScope) and (TPasProcedureScope(Scope).ClassScope<>nil) then
    Result:=TPasProcedureScope(Scope).ClassScope.Element
  else
    Result:=nil;
end;

procedure TPJUFiler.GetDefaultsPasIdentifierProps(El: TPasElement; out
  Kind: TPasIdentifierKind; out Name: string);
begin
  Kind:=PJUDefaultIdentifierKind;
  if El is TPasProcedure then
    Kind:=pikProc;
  Name:=El.Name;
end;

function TPJUFiler.GetDefaultClassScopeFlags(Scope: TPas2JSClassScope
  ): TPasClassScopeFlags;
begin
  Result:=[];
  if Scope.AncestorScope<>nil then
    begin
    if pcsfPublished in Scope.AncestorScope.Flags then
      Include(Result,pcsfPublished);
    end;
end;

function TPJUFiler.GetDefaultProcModifiers(Proc: TPasProcedure
  ): TProcedureModifiers;
begin
  Result:=[];
  if Proc.Parent is TPasClassType then
    begin
    if TPasClassType(Proc.Parent).IsExternal then
      Include(Result,pmExternal);
    end;
end;

function TPJUFiler.GetDefaultProcTypeModifiers(Proc: TPasProcedureType
  ): TProcTypeModifiers;
begin
  Result:=[];
  if Proc=nil then ;
end;

function TPJUFiler.GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum;
var
  p: PChar;
  Cnt: integer;
begin
  OnGetSrc(Self,aFilename,p,Cnt);
  Result:=ComputeChecksum(p,Cnt);
end;

function TPJUFiler.GetElementReference(El: TPasElement; AutoCreate: boolean
  ): TPJUFilerElementRef;
var
  Node: TAVLTreeNode;
  Data: TObject;
begin
  if El.CustomData is TResElDataBuiltInSymbol then
    begin
    // built-in symbol -> redirect to symbol of this module
    Data:=El.CustomData;
    if Data is TResElDataBaseType then
      El:=Resolver.BaseTypes[TResElDataBaseType(Data).BaseType]
    else if Data is TResElDataBuiltInProc then
      El:=TResElDataBuiltInProc(Data).Proc
    else
      RaiseMsg(20180207121004,El,Data.ClassName);
    end;
  Node:=FElementRefs.FindKey(El,@CompareElWithPJUFilerElementRef);
  if Node<>nil then
    Result:=TPJUFilerElementRef(Node.Data)
  else if AutoCreate then
    begin
    Result:=TPJUFilerElementRef.Create;
    Result.Element:=El;
    FElementRefs.Add(Result);
    end
  else
    Result:=nil;
end;

constructor TPJUFiler.Create;
begin
  FSourceFiles:=TObjectList.Create(true);
  FElementRefs:=TAVLTree.Create(@ComparePJUFilerElementRef);
  FElementRefs.SetNodeManager(TAVLTreeNodeMemManager.Create,true); // no shared manager, needed for multithreading
end;

destructor TPJUFiler.Destroy;
begin
  Clear;
  FreeAndNil(FSourceFiles);
  FreeAndNil(FElementRefs);
  inherited Destroy;
end;

procedure TPJUFiler.Clear;
begin
  FElementRefs.FreeAndClear;
  FSourceFiles.Clear;
  FResolver:=nil;
  FParser:=nil;
  FScanner:=nil;
end;

function TPJUFiler.SourceFileCount: integer;
begin
  Result:=FSourceFiles.Count;
end;

{ TPJUInitialFlags }

constructor TPJUInitialFlags.Create;
begin
  Clear;
end;

procedure TPJUInitialFlags.Clear;
begin
  ParserOptions:=PJUDefaultParserOptions;
  ModeSwitches:=PJUDefaultModeSwitches;
  BoolSwitches:=PJUDefaultBoolSwitches;
  ConverterOptions:=PJUDefaultConvertOptions;
  TargetPlatform:=PJUDefaultTargetPlatform;
  TargetProcessor:=PJUDefaultTargetProcessor;
end;

{ TPJUWriter }

procedure TPJUWriter.ResolvePendingElRefs(Ref: TPJUFilerElementRef);
var
  RefItem: TPJUFilerPendingElRef;
  RefObj: TPJUWriterPendingElRefObj;
  RefArr: TPJUWriterPendingElRefArray;
begin
  if Ref.Pending=nil then exit;
  // this element is referenced
  if Ref.Id=0 then
    CreateElReferenceId(Ref);
  // resolve all pending references
  while Ref.Pending<>nil do
    begin
    RefItem:=Ref.Pending;
    if RefItem is TPJUWriterPendingElRefObj then
      begin
      RefObj:=TPJUWriterPendingElRefObj(RefItem);
      RefObj.Obj.Add(RefObj.PropName,Ref.Id);
      end
    else if RefItem is TPJUWriterPendingElRefArray then
      begin
      RefArr:=TPJUWriterPendingElRefArray(RefItem);
      RefArr.Arr.Integers[RefArr.Index]:=Ref.Id;
      end
    else
      RaiseMsg(20180207113335,RefItem.ClassName);
    Ref.Pending:=RefItem.Next;
    RefItem.Next:=nil;
    RefItem.Free;
    end;
end;

procedure TPJUWriter.RaiseMsg(Id: int64; const Msg: string);
var
  E: EPas2JsWriteError;
begin
  E:=EPas2JsWriteError.Create('['+IntToStr(Id)+'] '+Msg);
  E.Owner:=Self;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUWriter.RaiseMsg ',E.Message);
  {$ENDIF}
  raise E;
end;

function TPJUWriter.CheckElScope(El: TPasElement; NotNilId: int64;
  ScopeClass: TPasScopeClass): TPasScope;
var
  Data: TObject;
begin
  Data:=El.CustomData;
  if Data=nil then
    begin
    if NotNilId>0 then
      RaiseMsg(NotNilId);
    exit(nil);
    end;
  if Data.ClassType<>ScopeClass then
    RaiseMsg(20180206113601,'expected '+ScopeClass.ClassName+', but found '+Data.ClassName);
  Result:=TPasScope(Data);
  if Result.Element<>El then
    RaiseMsg(20180206113723,'El='+GetObjName(El)+' Scope.Element='+GetObjName(Result.Element));
  if Result.Owner<>Resolver then
    RaiseMsg(20180206113750,El,GetObjName(Result));
end;

procedure TPJUWriter.AddArrayFlag(Obj: TJSONObject; var Arr: TJSONArray;
  const ArrName, Flag: string; Enable: boolean);
begin
  if Arr=nil then
    begin
    Arr:=TJSONArray.Create;
    Obj.Add(ArrName,Arr);
    end;
  if Enable then
    Arr.Add(Flag)
  else
    Arr.Add('-'+Flag);
end;

procedure TPJUWriter.AddReferenceToArray(Arr: TJSONArray; El: TPasElement);
var
  Ref: TPJUFilerElementRef;
  Item: TPJUWriterPendingElRefArray;
begin
  if El=nil then exit;
  Ref:=GetElementReference(El);
  if (Ref.Obj<>nil) and (Ref.Id=0) then
    CreateElReferenceId(Ref);
  Arr.Add(Ref.Id);
  if Ref.Id<>0 then
    exit;
  // Element was not yet written -> add a pending item to the queue
  Item:=TPJUWriterPendingElRefArray.Create;
  Item.Arr:=Arr;
  Item.Index:=Arr.Count-1;
  Ref.AddPending(Item);
end;

procedure TPJUWriter.AddReferenceToObj(Obj: TJSONObject;
  const PropName: string; El: TPasElement);
var
  Ref: TPJUFilerElementRef;
  Item: TPJUWriterPendingElRefObj;
begin
  if El=nil then exit;
  Ref:=GetElementReference(El);
  if (Ref.Obj<>nil) and (Ref.Id=0) then
    CreateElReferenceId(Ref);
  if Ref.Id<>0 then
    Obj.Add(PropName,Ref.Id)
  else
    begin
    // Element was not yet written -> add a pending item to the queue
    Item:=TPJUWriterPendingElRefObj.Create;
    Item.Obj:=Obj;
    Item.PropName:=PropName;
    Ref.AddPending(Item);
    end;
end;

procedure TPJUWriter.CreateElReferenceId(Ref: TPJUFilerElementRef);
begin
  if Ref.Id<>0 then
    RaiseMsg(20180207114300,Ref.Element,IntToStr(Ref.Id));
  inc(FElementIdCounter);
  Ref.Id:=FElementIdCounter;
  Ref.Obj.Add('Id',Ref.Id);
end;

procedure TPJUWriter.WriteHeaderMagic(Obj: TJSONObject);
begin
  Obj.Add('FileType',PJUMagic);
end;

procedure TPJUWriter.WriteHeaderVersion(Obj: TJSONObject);
begin
  Obj.Add('Version',PJUVersion);
end;

procedure TPJUWriter.WriteInitialFlags(Obj: TJSONObject);
begin
  WriteParserOptions(Obj,InitialFlags.ParserOptions,PJUDefaultParserOptions);
  WriteModeSwitches(Obj,InitialFlags.Modeswitches,PJUDefaultModeSwitches);
  WriteBoolSwitches(Obj,InitialFlags.BoolSwitches,PJUDefaultBoolSwitches);
  WriteConvertOptions(Obj,InitialFlags.ConverterOptions,PJUDefaultConvertOptions);
  if InitialFlags.TargetPlatform<>PJUDefaultTargetPlatform then
    Obj.Add('TargetPlatform',PJUTargetPlatformNames[InitialFlags.TargetPlatform]);
  if InitialFlags.TargetProcessor<>PJUDefaultTargetProcessor then
    Obj.Add('TargetProcessor',PJUTargetProcessorNames[InitialFlags.TargetProcessor]);
  // ToDo: write initial flags: used defines, used macros
end;

procedure TPJUWriter.WriteParserOptions(Obj: TJSONObject; const Value,
  DefaultValue: TPOptions);
var
  Arr: TJSONArray;
  f: TPOption;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TPOptions do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ParserOptions',PJUParserOptionNames[f],f in Value);
end;

procedure TPJUWriter.WriteModeSwitches(Obj: TJSONObject; const Value,
  DefaultValue: TModeSwitches);
var
  Arr: TJSONArray;
  f: TModeSwitch;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TModeSwitch do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ModeSwitches',PJUModeSwitchNames[f],f in Value);
end;

procedure TPJUWriter.WriteBoolSwitches(Obj: TJSONObject; const Value,
  DefaultValue: TBoolSwitches);
var
  Arr: TJSONArray;
  f: TBoolSwitch;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TBoolSwitch do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'BoolSwitches',PJUBoolSwitchNames[f],f in Value);
end;

procedure TPJUWriter.WriteConvertOptions(Obj: TJSONObject; const Value,
  DefaultValue: TPasToJsConverterOptions);
var
  Arr: TJSONArray;
  f: TPasToJsConverterOption;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TPasToJsConverterOption do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ConverterOptions',PJUConverterOptions[f],f in Value);
end;

procedure TPJUWriter.WriteSrcFiles(Obj: TJSONObject);
var
  CurFile: TPJUSourceFile;
  List: TFPList;
  i: Integer;
  SourcesArr: TJSONArray;
  Src: TJSONObject;
begin
  List:=TFPList.Create;
  try
    // get files from scanner
    for i:=0 to Scanner.Files.Count-1 do
      begin
      CurFile:=TPJUSourceFile.Create;
      CurFile.Index:=i;
      CurFile.Filename:=Scanner.Files[i];
      if i=0 then
        CurFile.FileType:=sftUnit
      else
        CurFile.FileType:=sftInclude;
      FSourceFiles.Add(CurFile);
      CurFile.Checksum:=GetSrcCheckSum(CurFile.Filename);
      List.Add(CurFile);
      end;

    // create FSourceFilesSorted
    List.Sort(@ComparePJUSrcFiles);
    SetLength(FSourceFilesSorted,List.Count);
    for i:=0 to List.Count-1 do
      FSourceFilesSorted[i]:=TPJUSourceFile(List[i]);

    // write
    SourcesArr:=TJSONArray.Create;
    Obj.Add('Sources',SourcesArr);
    for i:=0 to FSourceFiles.Count-1 do
      begin
      CurFile:=TPJUSourceFile(FSourceFiles[i]);
      Src:=TJSONObject.Create;
      SourcesArr.Add(Src);
      if (i=0) then
        // the first file is the unit source, no need to write Kind
      else if (CurFile.FileType=sftInclude) then
        // the default file type is include, no need to write Kind
      else
        Src.Add('Type',PJUSourceFileTypeNames[CurFile.FileType]);
      Src.Add('File',CurFile.Filename);
      Src.Add('CheckSum',CurFile.Checksum);
      end;
  finally
    List.Free;
  end;
end;

procedure TPJUWriter.WriteMemberHints(Obj: TJSONObject; const Value,
  DefaultValue: TPasMemberHints);
var
  Arr: TJSONArray;
  f: TPasMemberHint;
begin
  Arr:=nil;
  for f in TPasMemberHints do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'Hints',PJUMemberHintNames[f],f in Value);
end;

procedure TPJUWriter.WritePasElement(Obj: TJSONObject; El: TPasElement;
  aContext: TPJUWriterContext);
var
  i: Integer;
  LastElement: TPasElement;
  DefHints: TPasMemberHints;
  DefVisibility: TPasMemberVisibility;
  Ref: TPJUFilerElementRef;
begin
  if El.Name<>'' then
    Obj.Add('Name',El.Name);
  LastElement:=aContext.LastElement;

  // Id
  Ref:=GetElementReference(El);
  Ref.Obj:=Obj;
  ResolvePendingElRefs(Ref);

  if (LastElement=nil) or (LastElement.SourceFilename<>El.SourceFilename) then
    begin
    i:=IndexOfSourceFile(El.SourceFilename);
    if i<0 then
      RaiseMsg(20180205110259,El,El.SourceFilename);
    Obj.Add('File',i);
    end;

  if (LastElement=nil) or (LastElement.SourceLinenumber<>El.SourceLinenumber) then
    Obj.Add('Pos',El.SourceLinenumber);
  // not needed: El.SourceEndLinenumber

  DefVisibility:=GetDefaultMemberVisibility(El,LastElement);
  if El.Visibility<>DefVisibility then
    Obj.Add('Visibility',PJUMemberVisibilityNames[El.Visibility]);

  DefHints:=[];
  if LastElement<>nil then
    DefHints:=LastElement.Hints;
  WriteMemberHints(Obj,El.Hints,DefHints);

  if El.HintMessage<>'' then
    Obj.Add('HintMessage',El.HintMessage);

  // not needed El.DocComment

  // ToDo: El.CustomData
end;

procedure TPJUWriter.WriteModuleScopeFlags(Obj: TJSONObject; const Value,
  DefaultValue: TPasModuleScopeFlags);
var
  Arr: TJSONArray;
  f: TPasModuleScopeFlag;
begin
  Arr:=nil;
  for f in TPasModuleScopeFlags do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ScopeFlags',PJUModuleScopeFlagNames[f],f in Value);
end;

procedure TPJUWriter.WriteModule(Obj: TJSONObject; aModule: TPasModule;
  aContext: TPJUWriterContext);

  procedure WSection(Section: TPasSection; const PropName: string);
  begin
    if Section=nil then exit;
    if Section.Parent<>aModule then
      RaiseMsg(20180205153912,aModule,PropName);
    WriteSection(Obj,Section,PropName,aContext);
    aContext.LastElement:=Section;
  end;

var
  ModScope: TPasModuleScope;
begin
  WritePasElement(Obj,aModule,aContext);

  if aModule.ClassType=TPasModule then
    Obj.Add('Type','Unit')
  else if aModule.ClassType=TPasProgram then
    Obj.Add('Type','Program')
  else if aModule.ClassType=TPasLibrary then
    Obj.Add('Type','Library')
  else
    RaiseMsg(20180203163923);

  // module scope
  ModScope:=TPasModuleScope(CheckElScope(aModule,20180206113855,TPasModuleScope));
  WriteModuleScope(Obj,ModScope,aContext);

  // write sections
  aContext.LastElement:=aModule;
  WSection(aModule.InterfaceSection,'Interface');

  WSection(aModule.ImplementationSection,'Implementation');
  if aModule.ClassType=TPasProgram then
    WSection(TPasProgram(aModule).ProgramSection,'Program')
  else if aModule.ClassType=TPasLibrary then
    WSection(TPasLibrary(aModule).LibrarySection,'Library');
  // ToDo: write precompiled aModule.InitializationSection
  // ToDo: write precompiled aModule.FinalizationSection

  WriteExternalReferences(Obj);
end;

procedure TPJUWriter.WritePasScope(Obj: TJSONObject; Scope: TPasScope;
  aContext: TPJUWriterContext);
var
  DefVisibilityContext: TPasElement;
begin
  if aContext=nil then ;
  DefVisibilityContext:=GetDefaultPasScopeVisibilityContext(Scope);
  if Scope.VisibilityContext<>DefVisibilityContext then
    AddReferenceToObj(Obj,'VisibilityContext',Scope.VisibilityContext);
end;

procedure TPJUWriter.WriteIdentifierScope(Obj: TJSONObject;
  Scope: TPasIdentifierScope; aContext: TPJUWriterContext);
var
  Arr: TJSONArray;

  procedure WriteItem(Item: TPasIdentifier);
  var
    DefKind: TPasIdentifierKind;
    DefName: string;
    Sub: TJSONObject;
  begin
    GetDefaultsPasIdentifierProps(Item.Element,DefKind,DefName);
    if (Item.Kind=DefKind) and (Item.Identifier=DefName) then
    begin
      // add simply the element Id
      AddReferenceToArray(Arr,Item.Element);
    end
    else begin
      // add a json object
      Sub:=TJSONObject.Create;
      Arr.Add(Sub);
      if Item.Kind<>DefKind then
        Sub.Add('Kind',PJUIdentifierKindNames[Item.Kind]);
      if Item.Identifier<>DefName then
        Sub.Add('Name',Item.Identifier);
      AddReferenceToObj(Sub,'El',Item.Element);
    end;
  end;

var
  Locals: TFPList;
  i, p: Integer;
  Item: TPasIdentifier;
  Ordered: TPasIdentifierArray;
begin
  WritePasScope(Obj,Scope,aContext);
  Arr:=nil;
  if aContext=nil then ;
  Locals:=Scope.GetLocalIdentifiers;
  try
    p:=0;
    Ordered:=nil;
    for i:=0 to Locals.Count-1 do
      begin
      if Arr=nil then
        begin
        Arr:=TJSONArray.Create;
        Obj.Add('SItems',Arr);
        end;
      Item:=TPasIdentifier(Locals[i]);
      if Item.NextSameIdentifier=nil then
        WriteItem(Item)
      else
        begin
        // write in declaration order (i.e. reverse)
        p:=0;
        while Item<>nil do
          begin
          if length(Ordered)<=p then
            SetLength(Ordered,length(Ordered)+4);
          Ordered[p]:=Item;
          inc(p);
          Item:=Item.NextSameIdentifier;
          end;
        while p>0 do
          begin
          dec(p);
          WriteItem(Ordered[p]);
          end;
        end;
      end;
  finally
    Locals.Free;
  end;
end;

procedure TPJUWriter.WriteModuleScope(Obj: TJSONObject; Scope: TPasModuleScope;
  aContext: TPJUWriterContext);
var
  aModule: TPasModule;
begin
  aModule:=Scope.Element as TPasModule;
  if Scope.FirstName<>FirstDottedIdentifier(aModule.Name) then
    RaiseMsg(20180206114233,aModule);
  // write not needed: Scope.FirstName
  WriteModuleScopeFlags(Obj,Scope.Flags,PJUDefaultModuleScopeFlags);
  WriteBoolSwitches(Obj,Scope.BoolSwitches,aContext.BoolSwitches);
  AddReferenceToObj(Obj,'AssertClass',Scope.AssertClass);
  AddReferenceToObj(Obj,'AssertDefConstructor',Scope.AssertDefConstructor);
  AddReferenceToObj(Obj,'AssertMsgConstructor',Scope.AssertMsgConstructor);
  AddReferenceToObj(Obj,'RangeErrorClass',Scope.RangeErrorClass);
  AddReferenceToObj(Obj,'RangeErrorConstructor',Scope.RangeErrorConstructor);
  WritePasScope(Obj,Scope,aContext);
end;

procedure TPJUWriter.WriteSection(ParentJSON: TJSONObject;
  Section: TPasSection; const PropName: string; aContext: TPJUWriterContext);
var
  Obj: TJSONObject;
  Scope, UsesScope: TPasSectionScope;
  i: Integer;
  Arr: TJSONArray;
  UsesUnit: TPasUsesUnit;
begin
  if Section=nil then exit;
  Obj:=TJSONObject.Create;
  ParentJSON.Add(PropName,Obj);
  WritePasElement(Obj,Section,aContext);

  Scope:=TPasSectionScope(CheckElScope(Section,20180206121825,TPasSectionScope));
  if not Scope.Finished then
    RaiseMsg(20180206130333,Section);
  if Scope.UsesScopes.Count<>length(Section.UsesClause) then
    RaiseMsg(20180206122222,Section);
  if length(Section.UsesClause)>0 then
    begin
    Arr:=TJSONArray.Create;
    ParentJSON.Add('Uses',Arr);
    for i:=0 to Scope.UsesScopes.Count-1 do
      begin
      UsesUnit:=Section.UsesClause[i];
      UsesScope:=TPasSectionScope(Scope.UsesScopes[i]);
      if UsesScope.Element<>UsesUnit.Module then
        RaiseMsg(20180206122459,Section,'usesscope '+IntToStr(i)+' UsesScope.Element='+GetObjName(UsesScope.Element)+' Module='+GetObjName(Section.UsesClause[i].Module));
      // ToDo
      RaiseMsg(20180206124005,'ToDo');
      end;
    end;
  WriteIdentifierScope(Obj,Scope,aContext);

  WriteDeclarations(Obj,Section,aContext);
end;

procedure TPJUWriter.WriteDeclarations(ParentJSON: TJSONObject;
  Decls: TPasDeclarations; aContext: TPJUWriterContext);
var
  i: Integer;
  Decl: TPasElement;
  Arr: TJSONArray;
  DeclObj: TJSONObject;
begin
  Arr:=nil;
  for i:=0 to Decls.Declarations.Count-1 do
    begin
    Decl:=TPasElement(Decls.Declarations[i]);
    if Decl.Parent<>Decls then
      RaiseMsg(20180208221915,Decl,'['+IntToStr(i)+']='+GetObjName(Decl)+': '+GetObjName(Decls)+'<>'+GetObjName(Decl.Parent));
    if Arr=nil then
      begin
      Arr:=TJSONArray.Create;
      ParentJSON.Add('Declarations',Arr);
      end;
    DeclObj:=TJSONObject.Create;
    Arr.Add(DeclObj);
    WriteElement(DeclObj,Decl,aContext);
    end;
end;

procedure TPJUWriter.WriteElementProperty(Obj: TJSONObject;
  Parent: TPasElement; const PropName: string; El: TPasElement;
  aContext: TPJUWriterContext);
var
  SubObj: TJSONObject;
begin
  if El=nil then exit;
  if Parent<>El.Parent then
    RaiseMsg(20180208221751,El,GetObjName(Parent)+'<>'+GetObjName(El.Parent));
  SubObj:=TJSONObject.Create;
  Obj.Add(PropName,SubObj);
  WriteElement(SubObj,El,aContext);
end;

procedure TPJUWriter.WriteElementList(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; ListOfElements: TFPList; aContext: TPJUWriterContext;
  ReferencesAllowed: boolean);
var
  Arr: TJSONArray;
  i: Integer;
  SubObj: TJSONObject;
  Item: TPasElement;
begin
  if (ListOfElements=nil) or (ListOfElements.Count=0) then exit;
  Arr:=TJSONArray.Create;
  Obj.Add(PropName,Arr);
  for i:=0 to ListOfElements.Count-1 do
    begin
    Item:=TPasElement(ListOfElements[i]);
    if Item.Parent<>Parent then
      begin
      if not ReferencesAllowed then
        RaiseMsg(20180209191444,Item,GetObjName(Parent)+'<>'+GetObjName(Item.Parent));
      AddReferenceToArray(Arr,Item);
      end
    else
      begin
      SubObj:=TJSONObject.Create;
      Arr.Add(SubObj);
      WriteElement(SubObj,Item,aContext);
      end;
    end;
end;

procedure TPJUWriter.WriteElement(Obj: TJSONObject;
  El: TPasElement; aContext: TPJUWriterContext);
var
  C: TClass;
  ProcScope: TPasProcedureScope;
  Kind: TPasExprKind;
begin
  C:=El.ClassType;
  if C=TUnaryExpr then
    begin
    Obj.Add('Type','Unary');
    WriteUnaryExpr(Obj,TUnaryExpr(El),aContext);
    end
  else if C=TBinaryExpr then
    begin
    Obj.Add('Type','Binary');
    WriteBinaryExpr(Obj,TBinaryExpr(El),aContext);
    end
  else if C=TPrimitiveExpr then
    begin
    Kind:=TPrimitiveExpr(El).Kind;
    if not (Kind in [pekIdent,pekNumber,pekString]) then
      RaiseMsg(20180210153604,El,PJUExprKindNames[Kind]);
    Obj.Add('Type',PJUExprKindNames[Kind]);
    WritePrimitiveExpr(Obj,TPrimitiveExpr(El),aContext);
    end
  else if C=TBoolConstExpr then
    begin
    if El.CustomData=nil then
      Obj.Add('Type',PJUBoolStr[TBoolConstExpr(El).Value])
    else
      begin
      Obj.Add('Type','Bool');
      WriteBoolConstExpr(Obj,TBoolConstExpr(El),aContext);
      end;
    end
  else if C=TNilExpr then
    Obj.Add('Type','Nil')
  else if C=TInheritedExpr then
    begin
    Obj.Add('Type','Inherited');
    WritePasExpr(Obj,TInheritedExpr(El),false,eopNone,aContext);
    end
  else if C=TSelfExpr then
    begin
    Obj.Add('Type','Self');
    WritePasExpr(Obj,TSelfExpr(El),false,eopNone,aContext);
    end
  else if C=TParamsExpr then
    begin
    case TParamsExpr(El).Kind of
    pekArrayParams: Obj.Add('Type','A[]');
    pekFuncParams: Obj.Add('Type','F()');
    pekSet: Obj.Add('Type','[]');
    end;
    WriteParamsExpr(Obj,TParamsExpr(El),aContext);
    end
  else if C=TRecordValues then
    begin
    Obj.Add('Type','RecValues');
    WriteRecordValues(Obj,TRecordValues(El),aContext);
    end
  else if C=TArrayValues then
    begin
    Obj.Add('Type','ArrValues');
    WriteArrayValues(Obj,TArrayValues(El),aContext);
    end
  else if C=TPasResString then
    begin
    Obj.Add('Type','ResString');
    WriteResString(Obj,TPasResString(El),aContext);
    end
  else if C=TPasAliasType then
    begin
    Obj.Add('Type','Alias');
    WriteAliasType(Obj,TPasAliasType(El),aContext);
    end
  else if C=TPasPointerType then
    begin
    Obj.Add('Type','Pointer');
    WritePointerType(Obj,TPasPointerType(El),aContext);
    end
  else if C=TPasTypeAliasType then
    begin
    Obj.Add('Type','TypeAlias');
    WriteAliasType(Obj,TPasTypeAliasType(El),aContext);
    end
  else if C=TPasClassOfType then
    begin
    Obj.Add('Type','ClassOf');
    WriteAliasType(Obj,TPasClassOfType(El),aContext);
    end
  else if C=TPasSpecializeType then
    begin
    Obj.Add('Type','Specialize');
    WriteSpecializeType(Obj,TPasSpecializeType(El),aContext);
    end
  else if C=TInlineSpecializeExpr then
    begin
    Obj.Add('Type','InlineSpecialize');
    WriteInlineSpecializeExpr(Obj,TInlineSpecializeExpr(El),aContext);
    end
  else if C=TPasArrayType then
    begin
    Obj.Add('Type','ArrType');
    WriteArrayType(Obj,TPasArrayType(El),aContext);
    end
  else if C=TPasFileType then
    begin
    Obj.Add('Type','File');
    WriteFileType(Obj,TPasFileType(El),aContext);
    end
  else if C=TPasEnumValue then
    begin
    Obj.Add('Type','EnumV');
    WriteEnumValue(Obj,TPasEnumValue(El),aContext);
    end
  else if C=TPasEnumType then
    begin
    Obj.Add('Type','EnumType');
    WriteEnumType(Obj,TPasEnumType(El),aContext);
    end
  else if C=TPasSetType then
    begin
    Obj.Add('Type','SetType');
    WriteSetType(Obj,TPasSetType(El),aContext);
    end
  else if C=TPasVariant then
    begin
    Obj.Add('Type','RecVariant');
    WriteRecordVariant(Obj,TPasVariant(El),aContext);
    end
  else if C=TPasRecordType then
    begin
    Obj.Add('Type','Record');
    WriteRecordType(Obj,TPasRecordType(El),aContext);
    end
  else if C=TPasClassType then
    begin
    Obj.Add('Type',PJUObjKindNames[TPasClassType(El).ObjKind]);
    WriteClassType(Obj,TPasClassType(El),aContext);
    end
  else if C=TPasArgument then
    begin
    Obj.Add('Type','Arg');
    WriteArgument(Obj,TPasArgument(El),aContext);
    end
  else if C=TPasProcedureType then
    begin
    Obj.Add('Type','ProcType');
    WriteProcedureType(Obj,TPasProcedureType(El),aContext);
    end
  else if C=TPasResultElement then
    begin
    Obj.Add('Type','Result');
    WriteResultElement(Obj,TPasResultElement(El),aContext);
    end
  else if C=TPasFunctionType then
    begin
    Obj.Add('Type','FuncType');
    WriteFunctionType(Obj,TPasFunctionType(El),aContext);
    end
  else if C=TPasStringType then
    begin
    Obj.Add('Type','StringType');
    WriteStringType(Obj,TPasStringType(El),aContext);
    end
  else if C=TPasVariable then
    begin
    Obj.Add('Type','Var');
    WriteVariable(Obj,TPasVariable(El),aContext);
    end
  else if C=TPasExportSymbol then
    begin
    Obj.Add('Type','Export');
    WriteExportSymbol(Obj,TPasExportSymbol(El),aContext);
    end
  else if C=TPasConst then
    begin
    Obj.Add('Type','Const');
    WriteConst(Obj,TPasConst(El),aContext);
    end
  else if C=TPasProperty then
    begin
    Obj.Add('Type','Property');
    WriteProperty(Obj,TPasProperty(El),aContext);
    end
  else if C.InheritsFrom(TPasProcedure) then
    begin
    ProcScope:=El.CustomData as TPasProcedureScope;
    if ProcScope.DeclarationProc<>nil then
      exit;
    if C.InheritsFrom(TPasOperator) then
      begin
      if C=TPasOperator then
        Obj.Add('Type','Operator')
      else if C=TPasClassOperator then
        Obj.Add('Type','ClassOperator')
      else
        RaiseMsg(20180210130142,El);
      WriteOperator(Obj,TPasOperator(El),aContext);
      exit;
      end;
    if C=TPasProcedure then
      Obj.Add('Type','Procedure')
    else if C=TPasClassProcedure then
      Obj.Add('Type','ClassProcedure')
    else if C=TPasFunction then
      Obj.Add('Type','Function')
    else if C=TPasClassFunction then
      Obj.Add('Type','ClassFunction')
    else if C=TPasConstructor then
      Obj.Add('Type','Constructor')
    else if C=TPasClassConstructor then
      Obj.Add('Type','ClassConstructor')
    else if C=TPasDestructor then
      Obj.Add('Type','Destructor')
    else if C=TPasClassDestructor then
      Obj.Add('Type','Class Destructor')
    else
      RaiseMsg(20180210130202,El);
    WriteProcedure(Obj,TPasProcedure(El),aContext);
    end
  else
    RaiseMsg(20180205154041,El);
end;

procedure TPJUWriter.WriteElType(Obj: TJSONObject; El: TPasElement;
  const PropName: string; aType: TPasType; aContext: TPJUWriterContext);
begin
  if aType=nil then exit;
  if (aType.Name='') or (aType.Parent=El) then
    begin
    // anonymous type
    WriteElementProperty(Obj,El,PropName,aType,aContext);
    end
  else
    begin
    // reference
    AddReferenceToObj(Obj,PropName,aType);
    end;
  RaiseMsg(20180206183542,El);
end;

procedure TPJUWriter.WriteVarModifiers(Obj: TJSONObject;
  const PropName: string; const Value, DefaultValue: TVariableModifiers);
var
  Arr: TJSONArray;
  f: TVariableModifier;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TVariableModifier do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,PropName,PJUVarModifierNames[f],f in Value);
end;

procedure TPJUWriter.WriteExpr(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; Expr: TPasExpr; aContext: TPJUWriterContext);
var
  SubObj: TJSONObject;
begin
  if Expr=nil then exit;
  if Parent<>Expr.Parent then
    RaiseMsg(20180208221051,Expr,GetObjName(Parent)+'<>'+GetObjName(Expr.Parent));
  // ToDo: write simple expressions in a compact format
  SubObj:=TJSONObject.Create;
  Obj.Add(PropName,SubObj);
  WriteElement(SubObj,Expr,aContext);
end;

procedure TPJUWriter.WritePasExpr(Obj: TJSONObject; Expr: TPasExpr;
  WriteKind: boolean; DefaultOpCode: TExprOpCode; aContext: TPJUWriterContext);
begin
  if WriteKind then
    Obj.Add('Kind',PJUExprKindNames[Expr.Kind]);
  if (Expr.OpCode<>DefaultOpCode) then
    Obj.Add('Op',PJUExprOpCodeNames[Expr.OpCode]);
  WriteExpr(Obj,Expr,'Format1',Expr.format1,aContext);
  WriteExpr(Obj,Expr,'Format2',Expr.format2,aContext);
  WritePasElement(Obj,Expr,aContext);
end;

procedure TPJUWriter.WritePasExprArray(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; const ExprArr: TPasExprArray;
  aContext: TPJUWriterContext);
var
  Arr: TJSONArray;
  i: Integer;
  Expr: TPasExpr;
  SubObj: TJSONObject;
begin
  if length(ExprArr)=0 then exit;
  Arr:=TJSONArray.Create;
  Obj.Add(PropName,Arr);
  for i:=0 to length(ExprArr)-1 do
    begin
    Expr:=ExprArr[i];
    if Expr.Parent<>Parent then
      RaiseMsg(20180209191444,Expr,GetObjName(Parent)+'<>'+GetObjName(Expr.Parent));
    SubObj:=TJSONObject.Create;
    Arr.Add(SubObj);
    WriteElement(SubObj,Expr,aContext);
    end;
end;

procedure TPJUWriter.WriteUnaryExpr(Obj: TJSONObject; Expr: TUnaryExpr;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,Expr,'Operand',Expr.Operand,aContext);
  WritePasExpr(Obj,Expr,false,eopAdd,aContext);
end;

procedure TPJUWriter.WriteBinaryExpr(Obj: TJSONObject; Expr: TBinaryExpr;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,Expr,'left',Expr.left,aContext);
  WriteExpr(Obj,Expr,'right',Expr.right,aContext);
  WritePasExpr(Obj,Expr,false,eopAdd,aContext);
end;

procedure TPJUWriter.WritePrimitiveExpr(Obj: TJSONObject; Expr: TPrimitiveExpr;
  aContext: TPJUWriterContext);
begin
  if Expr.Value<>'' then
    Obj.Add('Value',Expr.Value);
  WritePasExpr(Obj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteBoolConstExpr(Obj: TJSONObject; Expr: TBoolConstExpr;
  aContext: TPJUWriterContext);
begin
  if Expr.Value then
    Obj.Add('Value',true);
  WritePasExpr(Obj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteParamsExpr(Obj: TJSONObject; Expr: TParamsExpr;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,Expr,'Value',Expr.Value,aContext);
  WritePasExprArray(Obj,Expr,'Params',Expr.Params,aContext);
  WritePasExpr(Obj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteRecordValues(Obj: TJSONObject; Expr: TRecordValues;
  aContext: TPJUWriterContext);
var
  Arr: TJSONArray;
  i: Integer;
  SubObj: TJSONObject;
  RecValue: TRecordValuesItem;
begin
  if length(Expr.Fields)>0 then
    begin
    Arr:=TJSONArray.Create;
    Obj.Add('Fields',Arr);
    for i:=0 to length(Expr.Fields)-1 do
      begin
      RecValue:=Expr.Fields[i];
      SubObj:=TJSONObject.Create;
      Arr.Add(SubObj);
      SubObj.Add('Name',RecValue.Name);
      if (RecValue.ValueExp<>nil) and (RecValue.ValueExp.Name<>'') then
        RaiseMsg(20180209192240,RecValue.ValueExp);
      WriteElement(SubObj,RecValue.ValueExp,aContext);
      end;
    end;
  WritePasExpr(Obj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteArrayValues(Obj: TJSONObject; Expr: TArrayValues;
  aContext: TPJUWriterContext);
begin
  WritePasExprArray(Obj,Expr,'Values',Expr.Values,aContext);
  WritePasExpr(Obj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteResString(Obj: TJSONObject; El: TPasResString;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,El,'Expr',El.Expr,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteAliasType(Obj: TJSONObject; El: TPasAliasType;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,El,'Dest',El.DestType,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WritePointerType(Obj: TJSONObject; El: TPasPointerType;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,El,'Dest',El.DestType,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteSpecializeType(Obj: TJSONObject;
  El: TPasSpecializeType; aContext: TPJUWriterContext);
begin
  WriteElementList(Obj,El,'Params',El.Params,aContext);
  WriteAliasType(Obj,El,aContext);
end;

procedure TPJUWriter.WriteInlineTypeExpr(Obj: TJSONObject; Expr: TInlineTypeExpr;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,Expr,'Dest',Expr.DestType,aContext);
  WritePasExpr(Obj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteInlineSpecializeExpr(Obj: TJSONObject;
  Expr: TInlineSpecializeExpr; aContext: TPJUWriterContext);
begin
  WriteInlineTypeExpr(Obj,Expr,aContext);
end;

procedure TPJUWriter.WriteArrayType(Obj: TJSONObject; El: TPasArrayType;
  aContext: TPJUWriterContext);
begin
  WritePasExprArray(Obj,El,'Ranges',El.Ranges,aContext);
  if El.PackMode<>pmNone then
    Obj.Add('Packed',PJUPackModeNames[El.PackMode]);
  WriteElType(Obj,El,'ElType',El.ElType,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteFileType(Obj: TJSONObject; El: TPasFileType;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,El,'ElType',El.ElType,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteEnumValue(Obj: TJSONObject; El: TPasEnumValue;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,El,'Value',El.Value,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteEnumTypeScope(Obj: TJSONObject;
  Scope: TPasEnumTypeScope; aContext: TPJUWriterContext);
begin
  WriteElementProperty(Obj,Scope.Element,'CanonicalSet',Scope.CanonicalSet,aContext);
  WriteIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUWriter.WriteEnumType(Obj: TJSONObject; El: TPasEnumType;
  aContext: TPJUWriterContext);
begin
  WriteElementList(Obj,El,'Values',El.Values,aContext);
  WriteEnumTypeScope(Obj,EL.CustomData as TPasEnumTypeScope,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteSetType(Obj: TJSONObject; El: TPasSetType;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,El,'EnumType',El.EnumType,aContext);
  if El.IsPacked then
    Obj.Add('Packed',true);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteRecordVariant(Obj: TJSONObject; El: TPasVariant;
  aContext: TPJUWriterContext);
begin
  WriteElementList(Obj,El,'Values',El.Values,aContext);
  WriteElType(Obj,El,'Members',El.Members,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteRecordTypeScope(Obj: TJSONObject;
  Scope: TPasRecordScope; aContext: TPJUWriterContext);
begin
  WriteIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUWriter.WriteRecordType(Obj: TJSONObject; El: TPasRecordType;
  aContext: TPJUWriterContext);
begin
  if El.PackMode<>pmNone then
    Obj.Add('Packed',PJUPackModeNames[El.PackMode]);
  WriteElementList(Obj,El,'Members',El.Members,aContext);
  // VariantEl: TPasElement can be TPasVariable or TPasType
  if El.VariantEl is TPasType then
    WriteElType(Obj,El,'VariantEl',TPasType(El.VariantEl),aContext)
  else
    WriteElementProperty(Obj,El,'VariantEl',El.VariantEl,aContext);
  WriteElementList(Obj,El,'Variants',El.Variants,aContext);
  WriteElementList(Obj,El,'Templates',El.GenericTemplateTypes,aContext);

  WriteRecordTypeScope(Obj,El.CustomData as TPasRecordScope,aContext);

  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteClassScopeFlags(Obj: TJSONObject;
  const PropName: string; const Value, DefaultValue: TPasClassScopeFlags);
var
  Arr: TJSONArray;
  f: TPasClassScopeFlag;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TPasClassScopeFlag do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,PropName,PJUClassScopeFlagNames[f],f in Value);
end;

procedure TPJUWriter.WriteClassScope(Obj: TJSONObject;
  Scope: TPas2JSClassScope; aContext: TPJUWriterContext);
var
  Arr: TJSONArray;
  i: Integer;
begin
  AddReferenceToObj(Obj,'NewInstanceFunction',Scope.NewInstanceFunction);
  // AncestorScope can be derived from DirectAncestor
  WriteElementProperty(Obj,Scope.Element,'CanonicalClassOf',Scope.CanonicalClassOf,aContext);
  AddReferenceToObj(Obj,'DirectAncestor',Scope.DirectAncestor);
  AddReferenceToObj(Obj,'DefaultProperty',Scope.DefaultProperty);
  WriteClassScopeFlags(Obj,'SFlags',Scope.Flags,GetDefaultClassScopeFlags(Scope));

  if length(Scope.AbstractProcs)>0 then
    begin
    Arr:=TJSONArray.Create;
    Obj.Add('AbstractProcs',Arr);
    for i:=0 to length(Scope.AbstractProcs)-1 do
      AddReferenceToArray(Arr,Scope.AbstractProcs[i]);
    end;

  WriteIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUWriter.WriteClassType(Obj: TJSONObject; El: TPasClassType;
  aContext: TPJUWriterContext);
var
  Arr: TJSONArray;
  i: Integer;
begin
  if El.PackMode<>pmNone then
    Obj.Add('Packed',PJUPackModeNames[El.PackMode]);
  // ObjKind is the 'Type'
  WriteElType(Obj,El,'Ancestor',El.AncestorType,aContext);
  WriteElType(Obj,El,'HelperFor',El.HelperForType,aContext);
  if El.IsForward then
    Obj.Add('Forward',true);
  if El.IsExternal then
    Obj.Add('External',true);
  // not needed IsShortDefinition: Boolean; -> class(anchestor); without end
  WriteExpr(Obj,El,'GUID',El.GUIDExpr,aContext);
  WriteElementList(Obj,El,'Members',El.Members,aContext);
  if El.Modifiers.Count>0 then
    begin
    Arr:=TJSONArray.Create;
    Obj.Add('Modifiers',Arr);
    for i:=0 to El.Modifiers.Count-1 do
      Arr.Add(El.Modifiers[i]);
    end;
  WriteElementList(Obj,El,'Interfaces',El.Interfaces,aContext,true);
  WriteElementList(Obj,El,'Templates',El.GenericTemplateTypes,aContext);
  if El.ExternalNameSpace<>'' then
    Obj.Add('ExternalNameSpace',El.ExternalNameSpace);
  if El.ExternalName<>'' then
    Obj.Add('ExternalName',El.ExternalName);

  WriteClassScope(Obj,El.CustomData as TPas2JSClassScope,aContext);

  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteArgument(Obj: TJSONObject; El: TPasArgument;
  aContext: TPJUWriterContext);
begin
  if El.Access<>argDefault then
    Obj.Add('Access',PJUArgumentAccessNames[El.Access]);
  WriteElType(Obj,El,'ArgType',El.ArgType,aContext);
  WriteExpr(Obj,El,'Value',El.ValueExpr,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteProcTypeModifiers(Obj: TJSONObject;
  const PropName: string; const Value, DefaultValue: TProcTypeModifiers);
var
  Arr: TJSONArray;
  f: TProcTypeModifier;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TProcTypeModifier do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,PropName,PJUProcTypeModifierNames[f],f in Value);
end;

procedure TPJUWriter.WriteProcedureType(Obj: TJSONObject;
  El: TPasProcedureType; aContext: TPJUWriterContext);
begin
  WriteElementList(Obj,El,'Args',El.Args,aContext);
  if El.CallingConvention<>ccDefault then
    Obj.Add('Call',PJUCallingConventionNames[El.CallingConvention]);
  WriteProcTypeModifiers(Obj,'Modifiers',El.Modifiers,GetDefaultProcTypeModifiers(El));
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteResultElement(Obj: TJSONObject;
  El: TPasResultElement; aContext: TPJUWriterContext);
begin
  WriteElType(Obj,El,'Result',El.ResultType,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteFunctionType(Obj: TJSONObject; El: TPasFunctionType;
  aContext: TPJUWriterContext);
begin
  WriteElementProperty(Obj,El,'Result',El.ResultEl,aContext);
  WriteProcedureType(Obj,El,aContext);
end;

procedure TPJUWriter.WriteStringType(Obj: TJSONObject; El: TPasStringType;
  aContext: TPJUWriterContext);
begin
  Obj.Add('Length',El.LengthExpr);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteVariable(Obj: TJSONObject; El: TPasVariable;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,El,'VarType',El.VarType,aContext);
  WriteVarModifiers(Obj,'VarMods',El.VarModifiers,[]);
  WriteExpr(Obj,El,'Library',El.LibraryName,aContext);
  WriteExpr(Obj,El,'Export',El.ExportName,aContext);
  WriteExpr(Obj,El,'Absolute',El.AbsoluteExpr,aContext);
  WriteExpr(Obj,El,'Expr',El.Expr,aContext);

  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteExportSymbol(Obj: TJSONObject; El: TPasExportSymbol;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,El,'ExportName',El.ExportName,aContext);
  WriteExpr(Obj,El,'ExportIndex',El.ExportIndex,aContext);
  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteConst(Obj: TJSONObject; El: TPasConst;
  aContext: TPJUWriterContext);
begin
  if El.IsConst<>(El.VarType=nil) then
    Obj.Add('IsConst',El.IsConst);
  WriteVariable(Obj,El,aContext);
end;

procedure TPJUWriter.WritePropertyScope(Obj: TJSONObject;
  Scope: TPasPropertyScope; aContext: TPJUWriterContext);
begin
  AddReferenceToObj(Obj,'AncestorProp',Scope.AncestorProp);
  WriteIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUWriter.WriteProperty(Obj: TJSONObject; El: TPasProperty;
  aContext: TPJUWriterContext);
begin
  WriteExpr(Obj,El,'Index',El.IndexExpr,aContext);
  WriteExpr(Obj,El,'Read',El.ReadAccessor,aContext);
  WriteExpr(Obj,El,'Write',El.WriteAccessor,aContext);
  WriteExpr(Obj,El,'Implements',El.ImplementsFunc,aContext);
  WriteExpr(Obj,El,'DispId',El.DispIDExpr,aContext);
  WriteExpr(Obj,El,'Stored',El.StoredAccessor,aContext);
  WriteExpr(Obj,El,'DefaultValue',El.DefaultExpr,aContext);
  WriteElementList(Obj,El,'Args',El.Args,aContext);
  //ReadAccessorName: string; // not used by resolver
  //WriteAccessorName: string; // not used by resolver
  //ImplementsName: string; // not used by resolver
  //StoredAccessorName: string; // not used by resolver
  if El.DispIDReadOnly then
    Obj.Add('ReadOnly',true);
  if El.isDefault then
    Obj.Add('Default',true);
  if El.IsNodefault then
    Obj.Add('NoDefault',true);

  WritePropertyScope(Obj,El.CustomData as TPasPropertyScope,aContext);

  WriteVariable(Obj,El,aContext);
end;

procedure TPJUWriter.WriteProcedureModifiers(Obj: TJSONObject;
  const PropName: string; const Value, DefaultValue: TProcedureModifiers);
var
  Arr: TJSONArray;
  f: TProcedureModifier;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TProcedureModifier do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,PropName,PJUProcedureModifierNames[f],f in Value);
end;

procedure TPJUWriter.WriteProcScopeFlags(Obj: TJSONObject;
  const PropName: string; const Value, DefaultValue: TPasProcedureScopeFlags);
var
  Arr: TJSONArray;
  f: TPasProcedureScopeFlag;
begin
  if Value=DefaultValue then exit;
  Arr:=nil;
  for f in TPasProcedureScopeFlag do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,PropName,PJUProcedureScopeFlagNames[f],f in Value);
end;

procedure TPJUWriter.WriteProcedureScope(Obj: TJSONObject;
  Scope: TPas2JSProcedureScope; aContext: TPJUWriterContext);
begin
  if Scope.ResultVarName<>'' then
    Obj.Add('ResultVarName',Scope.ResultVarName);
  // DeclarationProc: TPasProcedure; not needed, because only DeclarationProc is stored
  // ImplProc: TPasProcedure; not needed, because only DeclarationProc is stored
  AddReferenceToObj(Obj,'Overridden',Scope.OverriddenProc);
  // ClassScope: TPasClassScope; auto derived
  if Scope.SelfArg<>nil then
    RaiseMsg(20180211180457,Scope.Element); // SelfArg only valid for method implementation
  // Mode: TModeSwitch: auto derived
  WriteProcScopeFlags(Obj,'SFlags',Scope.Flags,[]);
  WriteBoolSwitches(Obj,Scope.BoolSwitches,aContext.BoolSwitches);

  WriteIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUWriter.WriteProcedure(Obj: TJSONObject; El: TPasProcedure;
  aContext: TPJUWriterContext);
var
  DefProcMods: TProcedureModifiers;
begin
  WriteElementProperty(Obj,El,'ProcType',El.ProcType,aContext);
  // ToDo: Body : TProcedureBody;
  WriteExpr(Obj,El,'Public',El.PublicName,aContext);
  // e.g. external LibraryExpr name LibrarySymbolName;
  WriteExpr(Obj,El,'Lib',El.LibraryExpr,aContext);
  WriteExpr(Obj,El,'LibName',El.LibrarySymbolName,aContext);
  WriteExpr(Obj,El,'DispId',El.DispIDExpr,aContext);
  if El.AliasName<>'' then
    Obj.Add('Alias',El.AliasName);
  DefProcMods:=GetDefaultProcModifiers(El);
  WriteProcedureModifiers(Obj,'PMods',El.Modifiers,DefProcMods);
  if (El.MessageName<>'') or (El.MessageType<>pmtNone) then
    begin
    Obj.Add('Message',El.MessageName);
    if El.MessageType<>pmtInteger then
      Obj.Add('MessageType',PJUProcedureMessageTypeNames[El.MessageType]);
    end;

  WriteProcedureScope(Obj,El.CustomData as TPas2JSProcedureScope,aContext);

  WritePasElement(Obj,El,aContext);
end;

procedure TPJUWriter.WriteOperator(Obj: TJSONObject; El: TPasOperator;
  aContext: TPJUWriterContext);
begin
  Obj.Add('Operator',PJUOperatorTypeNames[El.OperatorType]);
  if El.TokenBased then
    Obj.Add('TokenBased',El.TokenBased);
  WriteProcedure(Obj,El,aContext);
end;

procedure TPJUWriter.WriteExternalReferences(ParentJSON: TJSONObject);
var
  Node: TAVLTreeNode;
  Ref: TPJUFilerElementRef;
  El: TPasElement;
  Data: TObject;
  SystemArr, ExtArr: TJSONArray;
  Obj: TJSONObject;
begin
  ExtArr:=nil;
  SystemArr:=nil;
  Node:=FElementRefs.FindLowest;
  while Node<>nil do
    begin
    Ref:=TPJUFilerElementRef(Node.Data);
    Node:=FElementRefs.FindSuccessor(Node);
    if Ref.Pending=nil then continue;
    El:=Ref.Element;
    Data:=El.CustomData;
    if Data is TResElDataBuiltInSymbol then
      begin
      // add built-in symbol to System array
      if El.GetModule<>Resolver.RootElement then
        RaiseMsg(20180207124914,El);
      if SystemArr=nil then
        begin
        SystemArr:=TJSONArray.Create;
        ParentJSON.Add('System');
        end;
      Obj:=TJSONObject.Create;
      SystemArr.Add(Obj);
      Obj.Add('Name',El.Name);
      if Data is TResElDataBuiltInProc then
        case TResElDataBuiltInProc(Data).BuiltIn of
        bfStrFunc: Obj.Add('Type','Func');
        end;
      Ref.Obj:=Obj;
      ResolvePendingElRefs(Ref);
      continue;
      end;
    if Ref.Element.GetModule=Resolver.RootElement then
      RaiseMsg(20180207115645,Ref.Element); // an element of this module was not written
    // external element
    if ExtArr=nil then
      begin
      ExtArr:=TJSONArray.Create;
      ParentJSON.Add('External');
      end;
    Obj:=TJSONObject.Create;
    ExtArr.Add(Obj);
    Obj.Add('Name',El.Name);

    // ToDo
    RaiseMsg(20180207115730,Ref.Element);
    Ref.Obj:=Obj;
    ResolvePendingElRefs(Ref);
    end;
end;

constructor TPJUWriter.Create;
begin
  inherited Create;
end;

destructor TPJUWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TPJUWriter.Clear;
begin
  FInitialFlags:=nil;
  FElementIdCounter:=0;
  FSourceFilesSorted:=nil;
  FInImplementation:=false;
  inherited Clear;
end;

procedure TPJUWriter.WritePJU(aResolver: TPas2JSResolver;
  InitFlags: TPJUInitialFlags; aStream: TStream);
var
  aJSON: TJSONObject;
begin
  aJSON:=WriteJSON(aResolver,InitFlags);
  try
    aJSON.DumpJSON(aStream);
  finally
    aJSON.Free;
  end;
end;

function TPJUWriter.WriteJSON(aResolver: TPas2JSResolver;
  InitFlags: TPJUInitialFlags): TJSONObject;
var
  Obj, JSMod: TJSONObject;
  aContext: TPJUWriterContext;
begin
  Result:=nil;
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;
  FInitialFlags:=InitFlags;

  aContext:=nil;
  Obj:=TJSONObject.Create;
  try
    WriteHeaderMagic(Obj);
    WriteHeaderVersion(Obj);
    WriteInitialFlags(Obj);
    WriteSrcFiles(Obj);
    // ToDo: WriteUsedModulesPrecompiledChecksums
    aContext:=TPJUWriterContext.Create;
    aContext.ModeSwitches:=InitialFlags.ModeSwitches;
    aContext.BoolSwitches:=InitialFlags.BoolSwitches;
    JSMod:=TJSONObject.Create;
    Obj.Add('Module',JSMod);
    WriteModule(JSMod,aResolver.RootElement,aContext);
    // ToDo: write final flags: modeswitches, boolswitches, used defines

    Result:=Obj;
  finally
    aContext.Free;
    if Result=nil then
      Obj.Free;
  end;
end;

function TPJUWriter.IndexOfSourceFile(const Filename: string): integer;
var
  l, r, m, cmp: Integer;
begin
  l:=0;
  r:=length(FSourceFilesSorted)-1;
  while l<=r do
    begin
    m:=(l+r) div 2;
    cmp:=CompareStr(Filename,FSourceFilesSorted[m].Filename);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      exit(FSourceFilesSorted[m].Index);
    end;
  Result:=-1;
end;

{ TPJUReader }

procedure TPJUReader.Set_Variable_VarType(RefEl: TPasElement; Data: TObject);
var
  El: TPasVariable absolute Data;
begin
  if RefEl is TPasType then
    El.VarType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121809,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_AliasType_DestType(RefEl: TPasElement; Data: TObject);
var
  El: TPasAliasType absolute Data;
begin
  if RefEl is TPasType then
    El.DestType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121801,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_PointerType_DestType(RefEl: TPasElement; Data: TObject
  );
var
  El: TPasPointerType absolute Data;
begin
  if RefEl is TPasType then
    El.DestType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121757,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_InlineTypeExpr_DestType(RefEl: TPasElement;
  Data: TObject);
var
  El: TInlineTypeExpr absolute Data;
begin
  if RefEl is TPasType then
    El.DestType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121750,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ArrayType_ElType(RefEl: TPasElement; Data: TObject);
var
  El: TPasArrayType absolute Data;
begin
  if RefEl is TPasType then
    El.ElType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121732,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_FileType_ElType(RefEl: TPasElement; Data: TObject);
var
  El: TPasFileType absolute Data;
begin
  if RefEl is TPasType then
    El.ElType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121726,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_SetType_EnumType(RefEl: TPasElement; Data: TObject);
var
  El: TPasSetType absolute Data;
begin
  if RefEl is TPasType then
    El.EnumType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121714,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_Variant_Members(RefEl: TPasElement; Data: TObject);
var
  El: TPasVariant absolute Data;
begin
  if RefEl is TPasRecordType then
    El.Members:=TPasRecordType(RefEl)
  else
    RaiseMsg(20180211121657,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_RecordType_VariantEl(RefEl: TPasElement; Data: TObject
  );
var
  El: TPasRecordType absolute Data;
begin
  if (RefEl is TPasType) or (RefEl.ClassType=TPasVariable) then
    El.VariantEl:=RefEl
  else
    RaiseMsg(20180210205031,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_Argument_ArgType(RefEl: TPasElement; Data: TObject);
var
  El: TPasArgument absolute Data;
begin
  if RefEl is TPasType then
    El.ArgType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121643,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ClassScope_NewInstanceFunction(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPas2JSClassScope absolute Data;
begin
  if RefEl is TPasClassFunction then
    Scope.NewInstanceFunction:=TPasClassFunction(RefEl)
  else
    RaiseMsg(20180214114043,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ClassScope_DirectAncestor(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPas2JSClassScope absolute Data;
  aClassAncestor: TPasType;
begin
  if not (RefEl is TPasType) then
    RaiseMsg(20180214114823,Scope.Element,GetObjName(RefEl));
  Scope.DirectAncestor:=TPasType(RefEl);
  if Scope.DirectAncestor=nil then exit;

  // set AncestorScope
  aClassAncestor:=Resolver.ResolveAliasType(Scope.DirectAncestor);
  if not (aClassAncestor is TPasClassType) then
    RaiseMsg(20180214114322,Scope.Element,GetObjName(RefEl));
  if pcsfPublished in Scope.AncestorScope.Flags then
    Include(Scope.Flags,pcsfPublished);
  Scope.AncestorScope:=aClassAncestor.CustomData as TPas2JSClassScope
end;

procedure TPJUReader.Set_ClassScope_DefaultProperty(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPas2JSClassScope absolute Data;
begin
  if RefEl is TPasProperty then
    Scope.DefaultProperty:=TPasProperty(RefEl)
  else
    RaiseMsg(20180214115044,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ClassType_AncestorType(RefEl: TPasElement;
  Data: TObject);
var
  El: TPasClassType absolute Data;
begin
  if RefEl is TPasType then
    El.AncestorType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121632,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ClassType_HelperForType(RefEl: TPasElement;
  Data: TObject);
var
  El: TPasClassType absolute Data;
begin
  if RefEl is TPasType then
    El.HelperForType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121612,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ResultElement_ResultType(RefEl: TPasElement; Data: TObject
  );
var
  El: TPasResultElement absolute Data;
begin
  if RefEl is TPasType then
    El.ResultType:=TPasType(RefEl)
  else
    RaiseMsg(20180211121537,El,GetObjName(RefEl));
end;

procedure TPJUReader.Set_PasScope_VisibilityContext(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPasScope absolute Data;
begin
  Scope.VisibilityContext:=RefEl;
end;

procedure TPJUReader.Set_ModScope_AssertClass(RefEl: TPasElement; Data: TObject
  );
var
  Scope: TPasModuleScope absolute Data;
begin
  if RefEl is TPasClassType then
    Scope.AssertClass:=TPasClassType(RefEl)
  else
    RaiseMsg(20180211121441,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ModScope_AssertDefConstructor(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPasModuleScope absolute Data;
begin
  if RefEl is TPasConstructor then
    Scope.AssertDefConstructor:=TPasConstructor(RefEl)
  else
    RaiseMsg(20180211123001,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ModScope_AssertMsgConstructor(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPasModuleScope absolute Data;
begin
  if RefEl is TPasConstructor then
    Scope.AssertMsgConstructor:=TPasConstructor(RefEl)
  else
    RaiseMsg(20180211123020,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ModScope_RangeErrorClass(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPasModuleScope absolute Data;
begin
  if RefEl is TPasClassType then
    Scope.RangeErrorClass:=TPasClassType(RefEl)
  else
    RaiseMsg(20180211123041,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ModScope_RangeErrorConstructor(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPasModuleScope absolute Data;
begin
  if RefEl is TPasConstructor then
    Scope.RangeErrorConstructor:=TPasConstructor(RefEl)
  else
    RaiseMsg(20180211123100,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_PropertyScope_AncestorProp(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPasPropertyScope absolute Data;
begin
  if RefEl is TPasProperty then
    Scope.AncestorProp:=TPasProperty(RefEl)
  else
    RaiseMsg(20180213214723,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.Set_ProcedureScope_Overridden(RefEl: TPasElement;
  Data: TObject);
var
  Scope: TPas2JSProcedureScope absolute Data;
begin
  if RefEl is TPasProcedure then
    Scope.OverriddenProc:=TPasProcedure(RefEl)
  else
    RaiseMsg(20180213215959,Scope.Element,GetObjName(RefEl));
end;

procedure TPJUReader.RaiseMsg(Id: int64; const Msg: string);
var
  E: EPas2JsReadError;
begin
  E:=EPas2JsReadError.Create('['+IntToStr(Id)+'] '+Msg);
  E.Owner:=Self;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.RaiseMsg ',E.Message);
  {$ENDIF}
  raise E;
end;

function TPJUReader.CheckJSONArray(Data: TJSONData; El: TPasElement;
  const PropName: string): TJSONArray;
begin
  if Data is TJSONArray then exit(TJSONArray(Data));
  if Data=nil then
    RaiseMsg(20180205140943,El,PropName+': nil')
  else
    RaiseMsg(20180205140358,El,PropName+': '+Data.ClassName);
  Result:=nil;
end;

function TPJUReader.CheckJSONObject(Data: TJSONData; Id: int64): TJSONObject;
begin
  if Data is TJSONObject then exit(TJSONObject(Data));
  RaiseMsg(Id);
  Result:=nil;
end;

function TPJUReader.CheckJSONString(Data: TJSONData; Id: int64): String;
begin
  if Data is TJSONString then
    exit(String(Data.AsString));
  RaiseMsg(Id);
  Result:='';
end;

function TPJUReader.ReadString(Obj: TJSONObject; const PropName: string; out
  s: string; El: TPasElement): boolean;
var
  C: TClass;
  Data: TJSONData;
begin
  s:='';
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  C:=Data.ClassType;
  if C=TJSONString then
    begin
    s:=String(Data.AsString);
    exit(true);
    end;
  RaiseMsg(20180205133227,El,PropName+':'+Data.ClassName);
  Result:=false;
end;

function TPJUReader.ReadInteger(Obj: TJSONObject; const PropName: string; out
  i: integer; El: TPasElement): boolean;
var
  C: TClass;
  Data: TJSONData;
begin
  i:=0;
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  C:=Data.ClassType;
  if C=TJSONIntegerNumber then
    begin
    i:=Data.AsInteger;
    exit(true);
    end;
  RaiseMsg(20180205133132,El,PropName+':'+Data.ClassName);
  Result:=false;
end;

function TPJUReader.ReadBoolean(Obj: TJSONObject; const PropName: string; out
  b: boolean; El: TPasElement): boolean;
var
  C: TClass;
  Data: TJSONData;
begin
  b:=false;
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  C:=Data.ClassType;
  if C=TJSONBoolean then
    begin
    b:=Data.AsBoolean;
    exit(true);
    end;
  RaiseMsg(20180207183730,El,PropName+':'+Data.ClassName);
  Result:=false;
end;

function TPJUReader.ReadArray(Obj: TJSONObject; const PropName: string; out
  Arr: TJSONArray; El: TPasElement): boolean;
var
  Data: TJSONData;
begin
  Arr:=nil;
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  if not (Data is TJSONArray) then
    RaiseMsg(20180207144507,El,PropName+':'+Data.ClassName);
  Arr:=TJSONArray(Data);
  Result:=true;
end;

function TPJUReader.ReadObject(Obj: TJSONObject; const PropName: string; out
  SubObj: TJSONObject; El: TPasElement): boolean;
var
  Data: TJSONData;
begin
  SubObj:=nil;
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  if not (Data is TJSONObject) then
    RaiseMsg(20180210212719,El,PropName+':'+Data.ClassName);
  SubObj:=TJSONObject(Data);
  Result:=true;
end;

function TPJUReader.AddElReference(Id: integer; ErrorEl: TPasElement;
  El: TPasElement): TPJUFilerElementRef;
var
  NewCapacity, OldCapacity: Integer;
  Ref: TPJUFilerElementRef;
  RefItem: TPJUFilerPendingElRef;
  PendingElRef: TPJUReaderPendingElRef;
  PendingElListRef: TPJUReaderPendingElListRef;
begin
  if Id<=0 then
    RaiseMsg(20180207151233,ErrorEl);
  OldCapacity:=length(FElementRefsArray);
  if Id>=OldCapacity then
    begin
    // grow
    NewCapacity:=OldCapacity;
    if NewCapacity=0 then NewCapacity:=16;
    while NewCapacity<Id+1 do NewCapacity:=NewCapacity*2;
    SetLength(FElementRefsArray,NewCapacity);
    FillByte(FElementRefsArray[OldCapacity],SizeOf(Pointer)*(NewCapacity-OldCapacity),0);
    end;

  Ref:=FElementRefsArray[Id];
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.AddElReference Id=',Id,' El=',GetObjName(El),' ErrorEl=',GetObjName(ErrorEl),' OldRef=',GetObjName(Ref));
  {$ENDIF}
  if Ref=nil then
    begin
    // new target element
    if El<>nil then
      begin
      Ref:=GetElementReference(El,true);
      if Ref.Id=0 then
        Ref.Id:=Id
      else if Ref.Id<>Id then
        RaiseMsg(20180207152251,ErrorEl,IntToStr(Ref.Id)+'<>'+IntToStr(Id));
      end
    else
      begin
      Ref:=TPJUFilerElementRef.Create;
      Ref.Id:=Id;
      end;
    FElementRefsArray[Id]:=Ref;
    end;
  Result:=Ref;

  if El=nil then exit;

  if Ref.Element=nil then
    begin
    Ref.Element:=El;
    if Ref.Pending<>nil then
      begin
      // resolve pending references
      while Ref.Pending<>nil do
        begin
        RefItem:=Ref.Pending;
        if RefItem is TPJUReaderPendingElRef then
          begin
          PendingElRef:=TPJUReaderPendingElRef(RefItem);
          PendingElRef.Setter(Ref.Element,PendingElRef.Data);
          end
        else if RefItem is TPJUReaderPendingElListRef then
          begin
          PendingElListRef:=TPJUReaderPendingElListRef(RefItem);
          PendingElListRef.List[PendingElListRef.Index]:=Ref.Element;
          end
        else
          RaiseMsg(20180207153056,ErrorEl,RefItem.ClassName);
        Ref.Pending:=RefItem.Next;
        RefItem.Next:=nil;
        RefItem.Free;
        end;
      end;
    end
  else if El<>Ref.Element then
    RaiseMsg(20180207194919,ErrorEl,'Duplicate Id='+IntToStr(Id)+' El='+GetObjName(El)+' Ref.Element='+GetObjName(Ref.Element));
end;

procedure TPJUReader.PromiseSetElReference(Id: integer;
  const Setter: TOnSetElReference; Data: TObject; ErrorEl: TPasElement);
var
  Ref: TPJUFilerElementRef;
  PendingItem: TPJUReaderPendingElRef;
begin
  Ref:=AddElReference(Id,ErrorEl,nil);
  if Ref.Element<>nil then
    begin
    // element was already created -> execute Setter immediately
    Setter(Ref.Element,Data);
    end
  else
    begin
    // element was not yet created -> store Setter
    PendingItem:=TPJUReaderPendingElRef.Create;
    PendingItem.Setter:=Setter;
    PendingItem.Data:=Data;
    PendingItem.ErrorEl:=ErrorEl;
    Ref.AddPending(PendingItem);
    end;
end;

procedure TPJUReader.PromiseSetElListReference(Id: integer; List: TFPList;
  Index: integer; ErrorEl: TPasElement);
var
  Ref: TPJUFilerElementRef;
  PendingItem: TPJUReaderPendingElListRef;
begin
  Ref:=AddElReference(Id,ErrorEl,nil);
  if Ref.Element<>nil then
    begin
    // element was already created -> set list item immediately
    List[Index]:=Ref.Element;
    end
  else
    begin
    // element was not yet created -> store
    PendingItem:=TPJUReaderPendingElListRef.Create;
    PendingItem.List:=List;
    PendingItem.Index:=Index;
    PendingItem.ErrorEl:=ErrorEl;
    Ref.AddPending(PendingItem);
    end;
end;

procedure TPJUReader.ReadHeaderMagic(Obj: TJSONObject);
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadHeaderMagic ',Obj.Get('FileType',''));
  {$ENDIF}
  if Obj.Get('FileType','')<>PJUMagic then
    RaiseMsg(20180130201710,'not a pju file');
end;

procedure TPJUReader.ReadHeaderVersion(Obj: TJSONObject);
begin
  FFileVersion:=Obj.Get('Version',0);
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadHeaderVersion ',FFileVersion);
  {$ENDIF}
  if FFileVersion<1 then
    RaiseMsg(20180130201801,'invalid pju file version');
  if FFileVersion>PJUVersion then
    RaiseMsg(20180130201822,'pju file was created by a newer compiler.');
end;

procedure TPJUReader.ReadArrayFlags(Data: TJSONData; El: TPasElement;
  const PropName: string; out Names: TStringDynArray; out
  Enable: TBooleanDynArray);
const
  IdentStart = ['a'..'z','A'..'Z','_'];
var
  Arr: TJSONArray;
  Cnt, i: Integer;
  s: String;
begin
  Names:=nil;
  Enable:=nil;
  if Data=nil then exit;
  Arr:=CheckJSONArray(Data,El,PropName);
  Cnt:=Arr.Count;
  if Cnt=0 then exit;
  SetLength(Names,Cnt);
  SetLength(Enable,Cnt);
  for i:=0 to Cnt-1 do
    begin
    Data:=Arr[i];
    if not (Data is TJSONString) then
      RaiseMsg(20180202132350,El,PropName+' elements must be string');
    s:=String(TJSONString(Data).AsString);
    if s='' then
      RaiseMsg(20180202133605,El,PropName+' elements must be string');
    if s[1]='-' then
      begin
      Enable[i]:=false;
      system.Delete(s,1,1);
      end
    else
      Enable[i]:=true;
    if not (s[1] in IdentStart) then
      RaiseMsg(20180202133605,El,PropName+' elements must be identifiers');
    Names[i]:=s;
    end;
end;

function TPJUReader.ReadParserOptions(Data: TJSONData; El: TPasElement;
  const DefaultValue: TPOptions): TPOptions;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPOption;
  Found: Boolean;
  i: Integer;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadParserOptions START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'ParserOptions',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPOption do
      if s=PJUParserOptionNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144009,'unknown ParserOption "'+s+'"');
    end;
end;

function TPJUReader.ReadModeSwitches(Data: TJSONData; El: TPasElement;
  const DefaultValue: TModeSwitches): TModeSwitches;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TModeSwitch;
  Found: Boolean;
  i: Integer;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModeSwitches START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'ModeSwitches',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TModeSwitch do
      if s=PJUModeSwitchNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144054,'unknown ModeSwitch "'+s+'"');
    end;
end;

function TPJUReader.ReadBoolSwitches(Data: TJSONData; El: TPasElement;
  const DefaultValue: TBoolSwitches): TBoolSwitches;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TBoolSwitch;
  i: Integer;
  Found: Boolean;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadBoolSwitches START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'BoolSwitches',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TBoolSwitch do
      if s=PJUBoolSwitchNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144116,'unknown BoolSwitch "'+s+'"');
    end;
end;

function TPJUReader.ReadConverterOptions(Data: TJSONData; El: TPasElement;
  const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasToJsConverterOption;
  i: Integer;
  Found: Boolean;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadConverterOptions START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'ConverterOptions',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasToJsConverterOption do
      if s=PJUConverterOptions[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144136,'unknown ConvertOptions "'+s+'"');
    end;
end;

procedure TPJUReader.ReadTargetPlatform(Data: TJSONData);
var
  p: TPasToJsPlatform;
  s: String;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadTargetPlatform START');
  {$ENDIF}
  s:=CheckJSONString(Data,20180203100215);
  for p in TPasToJsPlatform do
    if s=PJUTargetPlatformNames[p] then
      begin
      InitialFlags.TargetPlatform:=p;
      exit;
      end;
  RaiseMsg(20180202145542,'invalid TargetPlatform');
end;

procedure TPJUReader.ReadTargetProcessor(Data: TJSONData);
var
  p: TPasToJsProcessor;
  s: String;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadTargetProcessor START');
  {$ENDIF}
  s:=CheckJSONString(Data,20180203100235);
  for p in TPasToJsProcessor do
    if s=PJUTargetProcessorNames[p] then
      begin
      InitialFlags.TargetProcessor:=p;
      exit;
      end;
  RaiseMsg(20180202145623,'invalid TargetProcessor');
end;

procedure TPJUReader.ReadSrcFiles(Data: TJSONData);
var
  SourcesArr: TJSONArray;
  i, j: Integer;
  Src: TJSONObject;
  CurFile: TPJUSourceFile;
  Found: Boolean;
  ft: TPJUSourceFileType;
  s: TJSONStringType;
  CurFilename, PropName: string;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadSrcFiles START ');
  {$ENDIF}
  SourcesArr:=CheckJSONArray(Data,nil,'Sources');
  for i:=0 to SourcesArr.Count-1 do
    begin
    Src:=CheckJSONObject(SourcesArr[i],20180203100307);
    CurFile:=TPJUSourceFile.Create;
    FSourceFiles.Add(CurFile);
    if i=0 then
      CurFile.FileType:=sftUnit
    else
      CurFile.FileType:=sftInclude;

    for j:=0 to Src.Count-1 do
      begin
      PropName:=Src.Names[j];
      Data:=Src.Elements[PropName];
      case PropName of
      'Type':
        begin
        s:=CheckJSONString(Data,20180203101322);
        Found:=false;
        for ft in TPJUSourceFileType do
          if s=PJUSourceFileTypeNames[ft] then
            begin
            Found:=true;
            CurFile.FileType:=ft;
            break;
            end;
        if not Found then
          RaiseMsg(20180202144347,'unknown filetype "'+s+'"');
        end;
      'File':
        begin
        CurFilename:=CheckJSONString(Data,20180203100410);
        if CurFilename='' then
          RaiseMsg(20180130203605);
        if length(CurFilename)>MAX_PATH then
          RaiseMsg(20180130203624);
        DoDirSeparators(CurFilename);
        if CurFilename<>ResolveDots(CurFilename) then
          RaiseMsg(20180130203841);
        if ExtractFilenameOnly(CurFilename)='' then
          RaiseMsg(20180130203924);
        CurFile.Filename:=CurFilename;
        end;
      'CheckSum':
        CurFile.Checksum:=Data.AsInt64;
      else
        RaiseMsg(20180202152628,'unknown file property "'+PropName+'"');
      end;
      end;
    end;
end;

function TPJUReader.ReadMemberHints(Obj: TJSONObject; El: TPasElement;
  const DefaultValue: TPasMemberHints): TPasMemberHints;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasMemberHint;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadMemberHints START');
  {$ENDIF}
  Data:=Obj.Find('Hints');
  if Data=nil then exit;
  ReadArrayFlags(Data,El,'Hints',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasMemberHint do
      if s=PJUMemberHintNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180205134551,'unknown element Hints "'+s+'"');
    end;
end;

procedure TPJUReader.ReadPasElement(Obj: TJSONObject; El: TPasElement;
  aContext: TPJUReaderContext);

  function StrToMemberVisibility(const s: string): TPasMemberVisibility;
  var
    vis: TPasMemberVisibility;
  begin
    for vis in TPasMemberVisibility do
      if PJUMemberVisibilityNames[vis]=s then
        exit(vis);
    RaiseMsg(20180205134334,El,s);
  end;

var
  i, Id: integer;
  s: string;
  LastElement: TPasElement;
  DefHints: TPasMemberHints;
begin
  LastElement:=aContext.LastElement;

  if ReadInteger(Obj,'Id',Id,El) then
    AddElReference(Id,El,El);

  if ReadInteger(Obj,'File',i,El) then
    El.SourceFilename:=SourceFiles[i].Filename
  else
    El.SourceFilename:=LastElement.SourceFilename;

  if ReadInteger(Obj,'Pos',i,El) then
    El.SourceLinenumber:=i
  else
    El.SourceLinenumber:=LastElement.SourceLinenumber;

  if ReadString(Obj,'Visibility',s,El) then
    El.Visibility:=StrToMemberVisibility(s)
  else
    El.Visibility:=GetDefaultMemberVisibility(El,LastElement);

  DefHints:=[];
  if LastElement<>nil then
    DefHints:=LastElement.Hints;
  El.Hints:=ReadMemberHints(Obj,El,DefHints);

  if ReadString(Obj,'HintMessage',s,El) then
    El.HintMessage:=s;
end;

procedure TPJUReader.ReadSectionScope(Obj: TJSONObject;
  Scope: TPasSectionScope; aContext: TPJUReaderContext);
var
  Data: TJSONData;
  UsesArr: TJSONArray;
  Section: TPasSection;
  i: Integer;
begin
  Section:=Scope.Element as TPasSection;
  Scope.Finished:=true;
  Data:=Obj.Find('Uses');
  if Data<>nil then
    begin
    UsesArr:=CheckJSONArray(Data,Section,'Uses');
    // ToDo UsesClause
    RaiseMsg(20180206124604,'ToDo');
    for i:=0 to UsesArr.Count-1 do ;
    end;
  ReadIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadSection(Obj: TJSONObject; Section: TPasSection;
  aContext: TPJUReaderContext);
var
  Scope: TPasSectionScope;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadSection ',GetObjName(Section));
  {$ENDIF}
  ReadPasElement(Obj,Section,aContext);

  Scope:=TPasSectionScope(Resolver.CreateScope(Section,TPasSectionScope));
  ReadSectionScope(Obj,Scope,aContext);

  ReadDeclarations(Obj,Section,aContext);
end;

procedure TPJUReader.ReadDeclarations(Obj: TJSONObject; Section: TPasSection;
  aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  i: Integer;
  Data: TJSONData;
begin
  if not ReadArray(Obj,'Declarations',Arr,Section) then exit;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadDeclarations ',GetObjName(Section),' ',Arr.Count);
  {$ENDIF}
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if not (Data is TJSONObject) then
      RaiseMsg(20180207182304,Section,IntToStr(i)+' '+GetObjName(Data));
    ReadDeclaration(TJSONObject(Data),Section,aContext);
    end;
end;

procedure TPJUReader.ReadDeclaration(Obj: TJSONObject; Section: TPasSection;
  aContext: TPJUReaderContext);
var
  aType, Name: string;
  El: TPasConst;
begin
  if not ReadString(Obj,'Type',aType,Section) then
    RaiseMsg(20180207183050,Section);
  if not ReadString(Obj,'Name',Name,Section) then
    RaiseMsg(20180207183415,Section);
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadDeclaration ',GetObjName(Section),' Type="',aType,'" Name="',Name,'"');
  {$ENDIF}
  case aType of
  'Const':
    begin
    El:=TPasConst.Create(Name,Section);
    Section.Declarations.Add(El);
    ReadConst(Obj,TPasConst(El),aContext);
    end
  else
    RaiseMsg(20180207183141,Section,'unknown type "'+LeftStr(aType,100)+'"');
  end;
end;

function TPJUReader.ReadElement(Obj: TJSONObject; Parent: TPasElement;
  aContext: TPJUReaderContext): TPasElement;

  procedure ReadPrimitive(Kind: TPasExprKind);
  var
    Prim: TPrimitiveExpr;
    Value: string;
  begin
    ReadString(Obj,'Value',Value,Parent);
    Prim:=TPrimitiveExpr.Create(Parent,Kind,Value);
    Result:=Prim;
    Prim.Name:='';
    ReadPasExpr(Obj,Prim,false,aContext);
  end;

  procedure CreateClassType(Kind: TPasObjKind; const aName: string);
  begin
    Result:=TPasClassType.Create(aName,Parent);
    TPasClassType(Result).ObjKind:=Kind;
    ReadClassType(Obj,TPasClassType(Result),aContext);
  end;

  procedure ReadProc(aClass: TPasProcedureClass; const aName: string);
  begin
    Result:=aClass.Create(aName,Parent);
    ReadProcedure(Obj,TPasProcedure(Result),aContext);
  end;

  procedure ReadOper(aClass: TPasProcedureClass; const aName: string);
  begin
    Result:=aClass.Create(aName,Parent);
    ReadOperator(Obj,TPasOperator(Result),aContext);
  end;

var
  aType, Name: string;
  ok: Boolean;
begin
  Result:=nil;
  if not ReadString(Obj,'Type',aType,Parent) then
    RaiseMsg(20180210143327,Parent);
  if not ReadString(Obj,'Name',Name,Parent) then
    Name:='';
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadElement ',GetObjName(Parent),' Type="',aType,'" Name="',Name,'"');
  {$ENDIF}
  ok:=false;
  try
    case aType of
    'Unary':
      begin
      Result:=TUnaryExpr.Create(Name,Parent);
      ReadUnaryExpr(Obj,TUnaryExpr(Result),aContext);
      end;
    'Binary':
      begin
      Result:=TBinaryExpr.Create(Name,Parent);
      ReadBinaryExpr(Obj,TBinaryExpr(Result),aContext);
      end;
    'Ident': ReadPrimitive(pekIdent);
    'Number': ReadPrimitive(pekNumber);
    'String': ReadPrimitive(pekString);
    'Bool':
      begin
      Result:=TBoolConstExpr.Create(Parent,pekBoolConst,false);
      ReadBoolConstExpr(Obj,TBoolConstExpr(Result),aContext);
      end;
    'False','True':
      begin
      Result:=TBoolConstExpr.Create(Parent,pekBoolConst,aType='True');
      ReadPasExpr(Obj,TBoolConstExpr(Result),false,aContext);
      end;
    'Nil':
      begin
      Result:=TNilExpr.Create(Parent);
      ReadPasExpr(Obj,TNilExpr(Result),false,aContext);
      end;
    'Inherited':
      begin
      Result:=TInheritedExpr.Create(Parent);
      ReadPasExpr(Obj,TInheritedExpr(Result),false,aContext);
      end;
    'Self':
      begin
      Result:=TSelfExpr.Create(Parent);
      ReadPasExpr(Obj,TSelfExpr(Result),false,aContext);
      end;
    'A[]':
      begin
      Result:=TParamsExpr.Create(Parent,pekArrayParams);
      ReadParamsExpr(Obj,TParamsExpr(Result),aContext);
      end;
    'F()':
      begin
      Result:=TParamsExpr.Create(Parent,pekFuncParams);
      ReadParamsExpr(Obj,TParamsExpr(Result),aContext);
      end;
    '[]':
      begin
      Result:=TParamsExpr.Create(Parent,pekSet);
      ReadParamsExpr(Obj,TParamsExpr(Result),aContext);
      end;
    'RecValues':
      begin
      Result:=TRecordValues.Create(Parent);
      ReadRecordValues(Obj,TRecordValues(Result),aContext);
      end;
    'ArrValues':
      begin
      Result:=TArrayValues.Create(Parent);
      ReadArrayValues(Obj,TArrayValues(Result),aContext);
      end;
    'ResString':
      begin
      Result:=TPasResString.Create(Name,Parent);
      ReadResString(Obj,TPasResString(Result),aContext);
      end;
    'Alias':
      begin
      Result:=TPasAliasType.Create(Name,Parent);
      ReadAliasType(Obj,TPasAliasType(Result),aContext);
      end;
    'Pointer':
      begin
      Result:=TPasPointerType.Create(Name,Parent);
      ReadPointerType(Obj,TPasPointerType(Result),aContext);
      end;
    'TypeAlias':
      begin
      Result:=TPasPointerType.Create(Name,Parent);
      ReadAliasType(Obj,TPasTypeAliasType(Result),aContext);
      end;
    'ClassOf':
      begin
      Result:=TPasClassOfType.Create(Name,Parent);
      ReadAliasType(Obj,TPasClassOfType(Result),aContext);
      end;
    'Specialize':
      begin
      Result:=TPasSpecializeType.Create(Name,Parent);
      ReadSpecializeType(Obj,TPasSpecializeType(Result),aContext);
      end;
    'InlineSpecialize':
      begin
      Result:=TInlineSpecializeExpr.Create(Name,Parent);
      ReadInlineSpecializeExpr(Obj,TInlineSpecializeExpr(Result),aContext);
      end;
    'ArrType':
      begin
      Result:=TPasArrayType.Create(Name,Parent);
      ReadArrayType(Obj,TPasArrayType(Result),aContext);
      end;
    'File':
      begin
      Result:=TPasFileType.Create(Name,Parent);
      ReadFileType(Obj,TPasFileType(Result),aContext);
      end;
    'EnumV':
      begin
      Result:=TPasEnumValue.Create(Name,Parent);
      ReadEnumValue(Obj,TPasEnumValue(Result),aContext);
      end;
    'EnumType':
      begin
      Result:=TPasEnumType.Create(Name,Parent);
      ReadEnumType(Obj,TPasEnumType(Result),aContext);
      end;
    'SetType':
      begin
      Result:=TPasSetType.Create(Name,Parent);
      ReadSetType(Obj,TPasSetType(Result),aContext);
      end;
    'RecVariant':
      begin
      Result:=TPasVariant.Create(Name,Parent);
      ReadRecordVariant(Obj,TPasVariant(Result),aContext);
      end;
    'Record':
      begin
      Result:=TPasRecordType.Create(Name,Parent);
      ReadRecordType(Obj,TPasRecordType(Result),aContext);
      end;
    'Object': CreateClassType(okObject,Name);
    'Class': CreateClassType(okClass,Name);
    'Interface': CreateClassType(okInterface,Name);
    'Generic': CreateClassType(okGeneric,Name);
    'ClassHelper': CreateClassType(okClassHelper,Name);
    'RecordHelper': CreateClassType(okRecordHelper,Name);
    'TypeHelper': CreateClassType(okTypeHelper,Name);
    'DispInterface': CreateClassType(okDispInterface,Name);
    'Arg':
      begin
      Result:=TPasArgument.Create(Name,Parent);
      ReadArgument(Obj,TPasArgument(Result),aContext);
      end;
    'ProcType':
      begin
      Result:=TPasProcedureType.Create(Name,Parent);
      ReadProcedureType(Obj,TPasProcedureType(Result),aContext);
      end;
    'Result':
      begin
      Result:=TPasResultElement.Create(Name,Parent);
      ReadResultElement(Obj,TPasResultElement(Result),aContext);
      end;
    'FuncType':
      begin
      Result:=TPasFunctionType.Create(Name,Parent);
      ReadFunctionType(Obj,TPasFunctionType(Result),aContext);
      end;
    'StringType':
      begin
      Result:=TPasStringType.Create(Name,Parent);
      ReadStringType(Obj,TPasStringType(Result),aContext);
      end;
    'Var':
      begin
      Result:=TPasVariable.Create(Name,Parent);
      ReadVariable(Obj,TPasVariable(Result),aContext);
      end;
    'Export':
      begin
      Result:=TPasExportSymbol.Create(Name,Parent);
      ReadExportSymbol(Obj,TPasExportSymbol(Result),aContext);
      end;
    'Const':
      begin
      Result:=TPasConst.Create(Name,Parent);
      ReadConst(Obj,TPasConst(Result),aContext);
      end;
    'Property':
      begin
      Result:=TPasProperty.Create(Name,Parent);
      ReadProperty(Obj,TPasProperty(Result),aContext);
      end;
    'Procedure': ReadProc(TPasProcedure,Name);
    'ClassProcedure': ReadProc(TPasClassProcedure,Name);
    'Function': ReadProc(TPasFunction,Name);
    'ClassFunction': ReadProc(TPasClassFunction,Name);
    'Constructor': ReadProc(TPasConstructor,Name);
    'ClassConstructor': ReadProc(TPasClassConstructor,Name);
    'Destructor': ReadProc(TPasDestructor,Name);
    'ClassDestructor': ReadProc(TPasClassDestructor,Name);
    'Operator': ReadOper(TPasConstructor,Name);
    'ClassOperator': ReadOper(TPasClassConstructor,Name);
    else
      RaiseMsg(20180210143758,Parent,'unknown type "'+LeftStr(aType,100)+'"');
    end;
    ok:=true;
  finally
    if not ok then
      if Result<>nil then
        begin
        Result.Release;
        Result:=nil;
        end;
  end;
end;

function TPJUReader.ReadElementProperty(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; BaseClass: TPTreeElement; aContext: TPJUReaderContext
  ): TPasElement;
var
  SubObj: TJSONObject;
  s: String;
begin
  if not ReadObject(Obj,PropName,SubObj,Parent) then exit;
  Result:=ReadElement(SubObj,Parent,aContext);
  if (Result is BaseClass) then exit;
  s:=GetObjName(Result);
  Result.Release;
  Result:=nil;
  RaiseMsg(20180211105744,Parent,PropName+' is '+s);
end;

procedure TPJUReader.ReadElementReference(Obj: TJSONObject;
  Instance: TPasElementBase; const PropName: string;
  const Setter: TOnSetElReference);
var
  Data: TJSONData;
  ErrorEl: TPasElement;
  Id: Integer;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  if Instance is TPasElement then
    ErrorEl:=TPasElement(Instance)
  else if Instance is TResolveData then
    ErrorEl:=TResolveData(Instance).Element
  else
    RaiseMsg(20180211120642,GetObjName(Instance)+'.'+PropName);
  if Data is TJSONIntegerNumber then
    begin
    Id:=Data.AsInteger;
    PromiseSetElReference(Id,Setter,Instance,ErrorEl);
    end
  else
    RaiseMsg(20180211120300,ErrorEl,PropName+' is '+GetObjName(Data));
end;

procedure TPJUReader.ReadElementList(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; ListOfElements: TFPList; aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  i, Id: Integer;
  Data: TJSONData;
  SubObj: TJSONObject;
  SubEl: TPasElement;
begin
  if not ReadArray(Obj,PropName,Arr,Parent) then exit;
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if Data is TJSONIntegerNumber then
      begin
      // reference
      Id:=Data.AsInteger;
      ListOfElements.Add(nil);
      PromiseSetElListReference(Id,ListOfElements,ListOfElements.Count-1,Parent);
      end
    else if Data is TJSONObject then
      begin
      SubObj:=TJSONObject(Data);
      SubEl:=ReadElement(SubObj,Parent,aContext);
      ListOfElements.Add(SubEl);
      end
    else
      RaiseMsg(20180210201001,Parent,'['+IntToStr(i)+'] is '+GetObjName(Data));
    end;
end;

procedure TPJUReader.ReadElType(Obj: TJSONObject; const PropName: string;
  El: TPasElement; const Setter: TOnSetElReference; aContext: TPJUReaderContext
  );
var
  Data: TJSONData;
  Id: Integer;
  SubEl: TPasElement;
  s: String;
begin
  if aContext=nil then ;
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  if Data is TJSONIntegerNumber then
    begin
    // reference
    Id:=Data.AsInteger;
    PromiseSetElReference(Id,Setter,El,El);
    end
  else if Data is TJSONObject then
    begin
    // anonymous type
    SubEl:=ReadElement(TJSONObject(Data),El,aContext);
    if not (SubEl is TPasType) then
      begin
      s:=GetObjName(SubEl);
      SubEl.Release;
      RaiseMsg(20180210150730,El,PropName+', expected type, but got '+s);
      end;
    Setter(SubEl,El);
    end
  else
    RaiseMsg(20180207185313,El,PropName+':'+GetObjName(Data));
end;

function TPJUReader.ReadExpr(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; aContext: TPJUReaderContext): TPasExpr;
var
  Data: TJSONData;
  s: string;
  SubObj: TJSONObject;
  El: TPasElement;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit(nil);
  if Data is TJSONObject then
    begin
    SubObj:=TJSONObject(Data);
    El:=ReadElement(SubObj,Parent,aContext);
    if not (El is TPasExpr) then
      begin
      s:=GetObjName(El);
      El.Release;
      RaiseMsg(20180210152134,Parent,PropName+' got '+s);
      end;
    Result:=TPasExpr(El);
    end
  else
    RaiseMsg(20180207190200,Parent,PropName+':'+GetObjName(Data));
end;

procedure TPJUReader.ReadPasScope(Obj: TJSONObject; Scope: TPasScope;
  aContext: TPJUReaderContext);
begin
  if Obj.Find('VisibilityContext')=nil then
    Scope.VisibilityContext:=GetDefaultPasScopeVisibilityContext(Scope)
  else
    ReadElementReference(Obj,Scope,'VisibilityContext',@Set_PasScope_VisibilityContext);
  if aContext=nil then ;
end;

procedure TPJUReader.ReadIdentifierScopeArray(Arr: TJSONArray;
  Scope: TPasIdentifierScope);
// called after reading module, i.e. all elements are created

  function GetElRef(Id: integer; out DefKind: TPasIdentifierKind;
    out DefName: string): TPJUFilerElementRef;
  begin
    Result:=AddElReference(Id,Scope.Element,nil);
    if Result.Element=nil then
      RaiseMsg(20180207161358,Scope.Element,'Id not found: '+IntToStr(Id));
    GetDefaultsPasIdentifierProps(Result.Element,DefKind,DefName);
  end;

var
  i, Id: Integer;
  Data: TJSONData;
  ItemObj: TJSONObject;
  s, Name, DefName: string;
  Kind, DefKind: TPasIdentifierKind;
  Ref: TPJUFilerElementRef;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadIdentifierScope ',Arr.Count);
  {$ENDIF}
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if Data is TJSONIntegerNumber then
      begin
      Id:=Data.AsInteger;
      Ref:=GetElRef(Id,DefKind,DefName);
      {$IFDEF VerbosePJUFiler}
      writeln('TPJUReader.ReadIdentifierScope Id=',Id,' ',DefName,' ',DefKind,' ',GetObjName(Ref.Element));
      {$ENDIF}
      Scope.AddIdentifier(DefName,Ref.Element,DefKind);
      end
    else if Data is TJSONObject then
      begin
      ItemObj:=TJSONObject(Data);
      if not ReadInteger(ItemObj,'El',Id,Scope.Element) then
        RaiseMsg(20180207162015,Scope.Element,'missing El:integer');
      Ref:=GetElRef(Id,DefKind,DefName);
      if ReadString(ItemObj,'Kind',s,Scope.Element) then
        Kind:=StrToPasIdentifierKind(s)
      else
        Kind:=DefKind;
      if not ReadString(ItemObj,'Name',Name,Scope.Element) then
        Name:=DefName;
      if Name='' then
        RaiseMsg(20180207162358,Scope.Element,IntToStr(Id));
      Scope.AddIdentifier(Name,Ref.Element,Kind);
      end
    else
      RaiseMsg(20180207154839,Scope.Element,GetObjName(Data));
    end;
end;

procedure TPJUReader.ReadIdentifierScope(Obj: TJSONObject;
  Scope: TPasIdentifierScope; aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  Pending: TPJUReaderPendingIdentifierScope;
begin
  if ReadArray(Obj,'SItems',Arr,Scope.Element) then
    begin
    Pending:=TPJUReaderPendingIdentifierScope.Create;
    Pending.Scope:=Scope;
    Pending.Arr:=Arr;
    FPendingIdentifierScopes.Add(Pending);
    end;
  ReadPasScope(Obj,Scope,aContext);
end;

function TPJUReader.ReadModuleScopeFlags(Obj: TJSONObject; El: TPasElement;
  const DefaultValue: TPasModuleScopeFlags): TPasModuleScopeFlags;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasModuleScopeFlag;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModuleScopeFlags START');
  {$ENDIF}
  Data:=Obj.Find('ScopeFlags');
  if Data=nil then exit;
  ReadArrayFlags(Data,El,'ScopeFlags',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasModuleScopeFlag do
      if s=PJUModuleScopeFlagNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180206114404,'unknown ModuleScopeFlag "'+s+'"');
    end;
end;

procedure TPJUReader.ReadModuleScope(Obj: TJSONObject; Scope: TPasModuleScope;
  aContext: TPJUReaderContext);
var
  aModule: TPasModule;
begin
  aModule:=Scope.Element as TPasModule;
  Scope.FirstName:=FirstDottedIdentifier(aModule.Name);
  Scope.Flags:=ReadModuleScopeFlags(Obj,aModule,PJUDefaultModuleScopeFlags);
  Scope.BoolSwitches:=ReadBoolSwitches(Obj.Find('BoolSwitches'),aModule,aContext.BoolSwitches);
  ReadElementReference(Obj,Scope,'AssertClass',@Set_ModScope_AssertClass);
  ReadElementReference(Obj,Scope,'AssertDefConstructor',@Set_ModScope_AssertDefConstructor);
  ReadElementReference(Obj,Scope,'AssertMsgConstructor',@Set_ModScope_AssertMsgConstructor);
  ReadElementReference(Obj,Scope,'RangeErrorClass',@Set_ModScope_RangeErrorClass);
  ReadElementReference(Obj,Scope,'RangeErrorConstructor',@Set_ModScope_RangeErrorConstructor);
  ReadPasScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadModule(Data: TJSONData; aContext: TPJUReaderContext);

  function PreReadSection(ParentJSON: TJSONObject; const PropName: string): TJSONObject;
  var
    PropData: TJSONData;
  begin
    PropData:=ParentJSON.Find(PropName);
    if PropData=nil then exit(nil);
    Result:=CheckJSONObject(PropData,20180205121719);
  end;

var
  Obj, SubObj: TJSONObject;
  aType, aName: String;
  aModule: TPasModule;
  ModScope: TPasModuleScope;
  OldBoolSwitches: TBoolSwitches;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModule START ');
  {$ENDIF}
  Obj:=CheckJSONObject(Data,20180203100422);
  aName:=String(Obj.Get('Name',''));
  aType:=String(Obj.Get('Type',''));
  case aType of
  'Unit': aModule:=TPasModule.Create(aName,nil);
  'Program': aModule:=TPasProgram.Create(aName,nil);
  'Library': aModule:=TPasLibrary.Create(aName,nil);
  else
    {$IFDEF VerbosePJUFiler}
    writeln('TPJUReader.ReadModule Type="',aType,'"');
    {$ENDIF}
    RaiseMsg(20180203100748);
  end;
  Resolver.RootElement:=aModule;

  ReadPasElement(Obj,aModule,aContext);

  // modscope
  ModScope:=TPasModuleScope(Resolver.CreateScope(aModule,TPasModuleScope));
  ReadModuleScope(Obj,ModScope,aContext);
  OldBoolSwitches:=aContext.BoolSwitches;
  aContext.BoolSwitches:=ModScope.BoolSwitches;
  try

    // read sections
    aContext.LastElement:=aModule;
    SubObj:=PreReadSection(Obj,'Interface');
    if SubObj<>nil then
      begin
      aModule.InterfaceSection:=TInterfaceSection.Create('',aModule);
      ReadSection(SubObj,aModule.InterfaceSection,aContext);
      aContext.LastElement:=aModule.InterfaceSection;
      end;
    SubObj:=PreReadSection(Obj,'Implementation');
    if SubObj<>nil then
      begin
      aModule.ImplementationSection:=TImplementationSection.Create('',aModule);
      ReadSection(SubObj,aModule.ImplementationSection,aContext);
      aContext.LastElement:=aModule.InterfaceSection;
      end;
    if aModule.ClassType=TPasProgram then
      begin
      SubObj:=PreReadSection(Obj,'Program');
      if SubObj<>nil then
        begin
        TPasProgram(aModule).ProgramSection:=TProgramSection.Create('',aModule);
        ReadSection(SubObj,TPasProgram(aModule).ProgramSection,aContext);
        aContext.LastElement:=TPasProgram(aModule).ProgramSection;
        end;
      end
    else if aModule.ClassType=TPasLibrary then
      begin
      SubObj:=PreReadSection(Obj,'Library');
      if SubObj<>nil then
        begin
        TPasLibrary(aModule).LibrarySection:=TLibrarySection.Create('',aModule);
        ReadSection(SubObj,TPasLibrary(aModule).LibrarySection,aContext);
        aContext.LastElement:=TPasLibrary(aModule).LibrarySection;
        end;
      end;
    // ToDo: read precompiled aModule.InitializationSection
    // ToDo: read precompiled aModule.FinalizationSection
  finally
    aContext.BoolSwitches:=OldBoolSwitches;
  end;

  ResolvePending;
end;

procedure TPJUReader.ReadPasExpr(Obj: TJSONObject; Expr: TPasExpr;
  ReadKind: boolean; aContext: TPJUReaderContext);
var
  Kind: TPasExprKind;
  s: string;
  Op: TExprOpCode;
  Found: Boolean;
begin
  if ReadKind and ReadString(Obj,'Kind',s,Expr) then
    begin
    Found:=false;
    for Kind in TPasExprKind do
      if s=PJUExprKindNames[Kind] then
        begin
        Expr.Kind:=Kind;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180208074859,Expr,s);
    end;
  if ReadString(Obj,'Op',s,Expr) then
    begin
    Found:=false;
    for Op in TExprOpCode do
      if s=PJUExprOpCodeNames[Op] then
        begin
        Expr.OpCode:=Op;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180208074950,Expr,s);
    end;
  Expr.format1:=ReadExpr(Obj,Expr,'format1',aContext);
  Expr.format2:=ReadExpr(Obj,Expr,'format2',aContext);
  ReadPasElement(Obj,Expr,aContext);
end;

procedure TPJUReader.ReadPasExprArray(Obj: TJSONObject; Parent: TPasElement;
  const PropName: string; var ExprArr: TPasExprArray;
  aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  i: Integer;
  Data: TJSONData;
  SubEl: TPasElement;
begin
  if not ReadArray(Obj,PropName,Arr,Parent) then exit;
  SetLength(ExprArr,Arr.Count);
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if not (Data is TJSONObject) then
      RaiseMsg(20180210173026,Parent,'['+IntToStr(i)+'] is '+GetObjName(Data));
    SubEl:=ReadElement(TJSONObject(Data),Parent,aContext);
    if not (SubEl is TPasExpr) then
      RaiseMsg(20180210173026,Parent,'['+IntToStr(i)+'] is '+GetObjName(SubEl));
    ExprArr[i]:=TPasExpr(SubEl);
    end;
end;

procedure TPJUReader.ReadUnaryExpr(Obj: TJSONObject; Expr: TUnaryExpr;
  aContext: TPJUReaderContext);
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  Expr.Operand:=ReadExpr(Obj,Expr,'Operand',aContext);
end;

procedure TPJUReader.ReadBinaryExpr(Obj: TJSONObject; Expr: TBinaryExpr;
  aContext: TPJUReaderContext);
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  Expr.left:=ReadExpr(Obj,Expr,'left',aContext);
  Expr.right:=ReadExpr(Obj,Expr,'right',aContext);
end;

procedure TPJUReader.ReadBoolConstExpr(Obj: TJSONObject; Expr: TBoolConstExpr;
  aContext: TPJUReaderContext);
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  ReadBoolean(Obj,'Value',Expr.Value,Expr);
end;

procedure TPJUReader.ReadParamsExpr(Obj: TJSONObject; Expr: TParamsExpr;
  aContext: TPJUReaderContext);
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  Expr.Value:=ReadExpr(Obj,Expr,'Value',aContext);
  ReadPasExprArray(Obj,Expr,'Params',Expr.Params,aContext);
end;

procedure TPJUReader.ReadRecordValues(Obj: TJSONObject; Expr: TRecordValues;
  aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  i: Integer;
  Data: TJSONData;
  SubObj: TJSONObject;
  SubEl: TPasElement;
  aName: string;
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  if ReadArray(Obj,'Fields',Arr,Expr) then
    begin
    SetLength(Expr.Fields,Arr.Count);
    for i:=0 to Arr.Count-1 do
      begin
      Data:=Arr[i];
      if not (Data is TJSONObject) then
        RaiseMsg(20180210173636,Expr,'['+IntToStr(i)+'] is '+GetObjName(Data));
      SubObj:=TJSONObject(Data);
      if ReadString(SubObj,'Name',aName,Expr) then
        Expr.Fields[i].Name:=aName;
      SubEl:=ReadElement(TJSONObject(Data),Expr,aContext);
      if not (SubEl is TPasExpr) then
        RaiseMsg(20180210174041,Expr,'['+IntToStr(i)+'] is '+GetObjName(SubEl));
      Expr.Fields[i].ValueExp:=TPasExpr(SubEl);
      end;
    end;
end;

procedure TPJUReader.ReadArrayValues(Obj: TJSONObject; Expr: TArrayValues;
  aContext: TPJUReaderContext);
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  ReadPasExprArray(Obj,Expr,'Values',Expr.Values,aContext);
end;

procedure TPJUReader.ReadResString(Obj: TJSONObject; El: TPasResString;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  El.Expr:=ReadExpr(Obj,El,'Expr',aContext);
end;

procedure TPJUReader.ReadAliasType(Obj: TJSONObject; El: TPasAliasType;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadElType(Obj,'Dest',El,@Set_AliasType_DestType,aContext);
end;

procedure TPJUReader.ReadPointerType(Obj: TJSONObject; El: TPasPointerType;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadElType(Obj,'Dest',El,@Set_PointerType_DestType,aContext);
end;

procedure TPJUReader.ReadSpecializeType(Obj: TJSONObject;
  El: TPasSpecializeType; aContext: TPJUReaderContext);
begin
  ReadAliasType(Obj,El,aContext);
  ReadElementList(Obj,El,'Params',El.Params,aContext);
end;

procedure TPJUReader.ReadInlineTypeExpr(Obj: TJSONObject;
  Expr: TInlineTypeExpr; aContext: TPJUReaderContext);
begin
  ReadPasExpr(Obj,Expr,false,aContext);
  ReadElType(Obj,'Dest',Expr,@Set_InlineTypeExpr_DestType,aContext);
end;

procedure TPJUReader.ReadInlineSpecializeExpr(Obj: TJSONObject;
  Expr: TInlineSpecializeExpr; aContext: TPJUReaderContext);
begin
  ReadInlineTypeExpr(Obj,Expr,aContext);
end;

procedure TPJUReader.ReadArrayType(Obj: TJSONObject; El: TPasArrayType;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadPasExprArray(Obj,El,'Ranges',El.Ranges,aContext);
  if El.PackMode<>pmNone then
    Obj.Add('Packed',PJUPackModeNames[El.PackMode]);
  ReadElType(Obj,'ElType',El,@Set_ArrayType_ElType,aContext);
end;

procedure TPJUReader.ReadFileType(Obj: TJSONObject; El: TPasFileType;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadElType(Obj,'ElType',El,@Set_FileType_ElType,aContext);
end;

procedure TPJUReader.ReadEnumValue(Obj: TJSONObject; El: TPasEnumValue;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  El.Value:=ReadExpr(Obj,El,'Value',aContext);
end;

procedure TPJUReader.ReadEnumTypeScope(Obj: TJSONObject;
  Scope: TPasEnumTypeScope; aContext: TPJUReaderContext);
begin
  Scope.CanonicalSet:=TPasSetType(ReadElementProperty(Obj,Scope.Element,'CanonicalSet',TPasSetType,aContext));
  ReadIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadEnumType(Obj: TJSONObject; El: TPasEnumType;
  aContext: TPJUReaderContext);
var
  Scope: TPasEnumTypeScope;
begin
  Scope:=TPasEnumTypeScope(Resolver.CreateScope(El,TPasEnumTypeScope));
  El.CustomData:=Scope;

  ReadPasElement(Obj,El,aContext);
  ReadEnumTypeScope(Obj,Scope,aContext);
  ReadElementList(Obj,El,'Values',El.Values,aContext);
end;

procedure TPJUReader.ReadSetType(Obj: TJSONObject; El: TPasSetType;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadElType(Obj,'EnumType',El,@Set_SetType_EnumType,aContext);
  ReadBoolean(Obj,'Packed',El.IsPacked,El);
end;

function TPJUReader.ReadPackedMode(Obj: TJSONObject; const PropName: string;
  ErrorEl: TPasElement): TPackMode;
var
  p: TPackMode;
  s: string;
begin
  Result:=pmNone;
  if not ReadString(Obj,PropName,s,ErrorEl) then exit;
  for p in TPackMode do
    if s=PJUPackModeNames[p] then
      exit(p);
  RaiseMsg(20180210210038,ErrorEl,PropName+' "'+s+'"');
end;

procedure TPJUReader.ReadRecordVariant(Obj: TJSONObject; El: TPasVariant;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadElementList(Obj,El,'Values',El.Values,aContext);
  ReadElType(Obj,'Members',El,@Set_Variant_Members,aContext);
end;

procedure TPJUReader.ReadRecordScope(Obj: TJSONObject; Scope: TPasRecordScope;
  aContext: TPJUReaderContext);
begin
  ReadIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadRecordType(Obj: TJSONObject; El: TPasRecordType;
  aContext: TPJUReaderContext);
var
  Data: TJSONData;
  Id: Integer;
  Scope: TPasRecordScope;
begin
  Scope:=TPasRecordScope(Resolver.CreateScope(El,TPasRecordScope));
  El.CustomData:=Scope;

  ReadPasElement(Obj,El,aContext);
  El.PackMode:=ReadPackedMode(Obj,'Packed',El);
  ReadElementList(Obj,El,'Members',El.Members,aContext);

  // VariantEl: TPasElement can be TPasVariable or TPasType
  Data:=Obj.Find('VariantEl');
  if Data is TJSONIntegerNumber then
    begin
    Id:=Data.AsInteger;
    PromiseSetElReference(Id,@Set_RecordType_VariantEl,El,El);
    end
  else if Data is TJSONObject then
    El.VariantEl:=ReadElement(TJSONObject(Data),El,aContext);

  ReadElementList(Obj,El,'Variants',El.Variants,aContext);
  ReadElementList(Obj,El,'Templates',El.GenericTemplateTypes,aContext);

  ReadRecordScope(Obj,Scope,aContext);
end;

function TPJUReader.ReadClassScopeFlags(Obj: TJSONObject; El: TPasElement;
  const PropName: string; const DefaultValue: TPasClassScopeFlags
  ): TPasClassScopeFlags;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasClassScopeFlag;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadClassScopeFlags START');
  {$ENDIF}
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  ReadArrayFlags(Data,El,PropName,Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasClassScopeFlag do
      if s=PJUClassScopeFlagNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180214115647,'unknown class scope flag "'+s+'"');
    end;
end;

procedure TPJUReader.ReadClassScopeAbstractProcs(Obj: TJSONObject;
  Scope: TPas2JSClassScope);
var
  Arr: TJSONArray;
  Data: TJSONData;
  Id, i: Integer;
  Ref: TPJUFilerElementRef;
begin
  if not ReadArray(Obj,'AbstractProcs',Arr,Scope.Element) then exit;
  SetLength(Scope.AbstractProcs,Arr.Count);
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if Data is TJSONIntegerNumber then
      begin
      Id:=Data.AsInteger;
      Ref:=AddElReference(Id,Scope.Element,nil);
      if Ref.Element=nil then
        RaiseMsg(20180214121727,Scope.Element,'['+IntToStr(i)+'] missing Id '+IntToStr(Id));
      if Ref.Element is TPasProcedure then
        Scope.AbstractProcs[i]:=TPasProcedure(Ref.Element)
      else
        RaiseMsg(20180214121902,Scope.Element,'['+IntToStr(i)+'] is '+GetObjName(Ref.Element));
      end
    else
      RaiseMsg(20180214121627,Scope.Element,'['+IntToStr(i)+'] is '+GetObjName(Data));
    end;
end;

procedure TPJUReader.ReadClassScope(Obj: TJSONObject; Scope: TPas2JSClassScope;
  aContext: TPJUReaderContext);
begin
  ReadElementReference(Obj,Scope,'NewInstanceFunction',@Set_ClassScope_NewInstanceFunction);
  Scope.CanonicalClassOf:=TPasClassOfType(ReadElementProperty(Obj,Scope.Element,
    'CanonicalClassOf',TPasClassOfType,aContext));
  ReadElementReference(Obj,Scope,'DirectAncestor',@Set_ClassScope_DirectAncestor);
  ReadElementReference(Obj,Scope,'DefaultProperty',@Set_ClassScope_DefaultProperty);
  Scope.Flags:=ReadClassScopeFlags(Obj,Scope.Element,'SFlags',GetDefaultClassScopeFlags(Scope));

  ReadIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadClassType(Obj: TJSONObject; El: TPasClassType;
  aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  i: Integer;
  Data: TJSONData;
  Scope: TPas2JSClassScope;
begin
  Scope:=TPas2JSClassScope(Resolver.CreateScope(El,TPas2JSClassScope));
  El.CustomData:=Scope;

  ReadPasElement(Obj,El,aContext);
  El.PackMode:=ReadPackedMode(Obj,'Packed',El);
  // ObjKind is the 'Type'
  ReadElType(Obj,'Ancestor',El,@Set_ClassType_AncestorType,aContext);
  ReadElType(Obj,'HelperFor',El,@Set_ClassType_HelperForType,aContext);
  ReadBoolean(Obj,'Forward',El.IsForward,El);
  ReadBoolean(Obj,'External',El.IsExternal,El);
  // not needed IsShortDefinition: Boolean; -> class(anchestor); without end
  El.GUIDExpr:=ReadExpr(Obj,El,'GUID',aContext);

  // Modifiers
  if ReadArray(Obj,'Modifiers',Arr,El) then
    begin
    for i:=0 to Arr.Count-1 do
      begin
      Data:=Arr[i];
      if not (Data is TJSONString) then
        RaiseMsg(20180210211250,El,'Modifiers['+IntToStr(i)+'] '+GetObjName(Data));
      El.Modifiers.Add(String(Data.AsString));
      end;
    end;

  ReadElementList(Obj,El,'Interfaces',El.Interfaces,aContext);
  ReadElementList(Obj,El,'Templates',El.GenericTemplateTypes,aContext);
  ReadString(Obj,'ExternalNameSpace',El.ExternalNameSpace,El);
  ReadString(Obj,'ExternalName',El.ExternalName,El);

  ReadClassScope(Obj,Scope,aContext);

  // read Members as last
  ReadElementList(Obj,El,'Members',El.Members,aContext);

  ReadClassScopeAbstractProcs(Obj,Scope);
end;

procedure TPJUReader.ReadArgument(Obj: TJSONObject; El: TPasArgument;
  aContext: TPJUReaderContext);
var
  s: string;
  Found: Boolean;
  Arg: TArgumentAccess;
begin
  ReadPasElement(Obj,El,aContext);
  if ReadString(Obj,'Access',s,El) then
    begin
    Found:=false;
    for Arg in TArgumentAccess do
      if s=PJUArgumentAccessNames[Arg] then
        begin
        El.Access:=Arg;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180210205544,El,'Access "'+s+'"');
    end;
  ReadElType(Obj,'ArgType',El,@Set_Argument_ArgType,aContext);
  El.ValueExpr:=ReadExpr(Obj,El,'Value',aContext);
end;

function TPJUReader.ReadProcTypeModifiers(Obj: TJSONObject; El: TPasElement;
  const PropName: string; const DefaultValue: TProcTypeModifiers
  ): TProcTypeModifiers;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TProcTypeModifier;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadProcTypeModifiers START');
  {$ENDIF}
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  ReadArrayFlags(Data,El,PropName,Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TProcTypeModifier do
      if s=PJUProcTypeModifierNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180210212015,'unknown procedure modifier "'+s+'"');
    end;
end;

procedure TPJUReader.ReadProcedureType(Obj: TJSONObject; El: TPasProcedureType;
  aContext: TPJUReaderContext);
var
  s: string;
  Found: Boolean;
  c: TCallingConvention;
begin
  ReadPasElement(Obj,El,aContext);
  ReadElementList(Obj,El,'Args',El.Args,aContext);

  if ReadString(Obj,'Call',s,El) then
    begin
    Found:=false;
    for c in TCallingConvention do
      if s=PJUCallingConventionNames[c] then
        begin
        El.CallingConvention:=c;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180210212130,El,'Call "'+s+'"');
    end;
  El.Modifiers:=ReadProcTypeModifiers(Obj,El,'PTModifiers',GetDefaultProcTypeModifiers(El));
end;

procedure TPJUReader.ReadResultElement(Obj: TJSONObject; El: TPasResultElement;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadElType(Obj,'Result',El,@Set_ResultElement_ResultType,aContext);
end;

procedure TPJUReader.ReadFunctionType(Obj: TJSONObject; El: TPasFunctionType;
  aContext: TPJUReaderContext);
begin
  ReadProcedureType(Obj,El,aContext);
  El.ResultEl:=TPasResultElement(ReadElementProperty(Obj,El,'Result',TPasResultElement,aContext));
end;

procedure TPJUReader.ReadStringType(Obj: TJSONObject; El: TPasStringType;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  ReadString(Obj,'Length',El.LengthExpr,El);
end;

function TPJUReader.ReadVarModifiers(Obj: TJSONObject; El: TPasElement;
  const PropName: string; const DefaultValue: TVariableModifiers
  ): TVariableModifiers;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TVariableModifier;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadVarModifiers START');
  {$ENDIF}
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  ReadArrayFlags(Data,El,PropName,Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TVariableModifier do
      if s=PJUVarModifierNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180207184723,'unknown var modifier "'+s+'"');
    end;
end;

procedure TPJUReader.ReadVariable(Obj: TJSONObject; El: TPasVariable;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);

  ReadElType(Obj,'VarType',El,@Set_Variable_VarType,aContext);
  El.VarModifiers:=ReadVarModifiers(Obj,El,'VarMods',[]);
  El.LibraryName:=ReadExpr(Obj,El,'Library',aContext);
  El.ExportName:=ReadExpr(Obj,El,'Export',aContext);
  El.AbsoluteExpr:=ReadExpr(Obj,El,'Absolute',aContext);
  El.Expr:=ReadExpr(Obj,El,'Expr',aContext);
end;

procedure TPJUReader.ReadExportSymbol(Obj: TJSONObject; El: TPasExportSymbol;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,El,aContext);
  El.ExportName:=ReadExpr(Obj,El,'ExportName',aContext);
  El.ExportIndex:=ReadExpr(Obj,El,'ExportIndex',aContext);
end;

procedure TPJUReader.ReadConst(Obj: TJSONObject; El: TPasConst;
  aContext: TPJUReaderContext);
begin
  ReadVariable(Obj,El,aContext);
  if not ReadBoolean(Obj,'IsConst',El.IsConst,El) then
    El.IsConst:=Obj.Find('VarType')=nil;
end;

procedure TPJUReader.ReadPropertyScope(Obj: TJSONObject;
  Scope: TPasPropertyScope; aContext: TPJUReaderContext);
begin
  ReadElementReference(Obj,Scope,'AncestorProp',@Set_PropertyScope_AncestorProp);
  ReadIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadProperty(Obj: TJSONObject; El: TPasProperty;
  aContext: TPJUReaderContext);
var
  Scope: TPasPropertyScope;
begin
  Scope:=TPasPropertyScope(Resolver.CreateScope(El,TPasPropertyScope));
  El.CustomData:=Scope;

  ReadVariable(Obj,El,aContext);
  El.IndexExpr:=ReadExpr(Obj,El,'Index',aContext);
  El.ReadAccessor:=ReadExpr(Obj,El,'Read',aContext);
  El.WriteAccessor:=ReadExpr(Obj,El,'Write',aContext);
  El.ImplementsFunc:=ReadExpr(Obj,El,'Implements',aContext);
  El.DispIDExpr:=ReadExpr(Obj,El,'DispId',aContext);
  El.StoredAccessor:=ReadExpr(Obj,El,'Stored',aContext);
  El.DefaultExpr:=ReadExpr(Obj,El,'DefaultValue',aContext);
  ReadElementList(Obj,El,'Args',El.Args,aContext);
  //ReadAccessorName: string; // not used by resolver
  //WriteAccessorName: string; // not used by resolver
  //ImplementsName: string; // not used by resolver
  //StoredAccessorName: string; // not used by resolver
  ReadBoolean(Obj,'ReadOnly',El.DispIDReadOnly,El);
  ReadBoolean(Obj,'Default',El.IsDefault,El);
  ReadBoolean(Obj,'NoDefault',El.IsNodefault,El);

  ReadPropertyScope(Obj,Scope,aContext);
end;

function TPJUReader.ReadProcedureModifiers(Obj: TJSONObject; El: TPasElement;
  const PropName: string; const DefaultValue: TProcedureModifiers
  ): TProcedureModifiers;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TProcedureModifier;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadProcedureModifiers START');
  {$ENDIF}
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  ReadArrayFlags(Data,El,PropName,Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TProcedureModifier do
      if s=PJUProcedureModifierNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180211110407,'unknown proc modifier "'+s+'"');
    end;
end;

function TPJUReader.ReadProcScopeFlags(Obj: TJSONObject; El: TPasElement;
  const PropName: string; const DefaultValue: TPasProcedureScopeFlags
  ): TPasProcedureScopeFlags;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasProcedureScopeFlag;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadProcedureScopeFlags START');
  {$ENDIF}
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  ReadArrayFlags(Data,El,PropName,Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasProcedureScopeFlag do
      if s=PJUProcedureScopeFlagNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180213220601,'unknown proc scope flag "'+s+'"');
    end;
end;

procedure TPJUReader.ReadProcedureScope(Obj: TJSONObject;
  Scope: TPas2JSProcedureScope; aContext: TPJUReaderContext);
begin
  ReadString(Obj,'ResultVarName',Scope.ResultVarName,Scope.Element);

  // DeclarationProc: TPasProcedure; not needed, because only DeclarationProc is stored
  // ImplProc: TPasProcedure; not needed, because only DeclarationProc is stored
  ReadElementReference(Obj,Scope,'Overridden',@Set_ProcedureScope_Overridden);
  // ClassScope: TPasClassScope; auto derived
  // Scope.SelfArg only valid for method implementation

  if msDelphi in aContext.ModeSwitches then
    Scope.Mode:=msDelphi
  else if msObjfpc in aContext.ModeSwitches then
    Scope.Mode:=msObjfpc
  else
    RaiseMsg(20180213220335,Scope.Element);

  Scope.Flags:=ReadProcScopeFlags(Obj,Scope.Element,'SFlags',[]);
  Scope.BoolSwitches:=ReadBoolSwitches(Obj,Scope.Element,aContext.BoolSwitches);

  ReadIdentifierScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadProcedure(Obj: TJSONObject; El: TPasProcedure;
  aContext: TPJUReaderContext);
var
  DefProcMods: TProcedureModifiers;
  t: TProcedureMessageType;
  s: string;
  Found: Boolean;
  Scope: TPas2JSProcedureScope;
begin
  Scope:=TPas2JSProcedureScope(Resolver.CreateScope(El,TPas2JSProcedureScope));
  El.CustomData:=Scope;

  ReadPasElement(Obj,El,aContext);
  El.ProcType:=TPasProcedureType(ReadElementProperty(Obj,El,'ProcType',TPasProcedureType,aContext));
  // ToDo: Body : TProcedureBody;
  El.PublicName:=ReadExpr(Obj,El,'Public',aContext);
  // e.g. external LibraryExpr name LibrarySymbolName;
  El.LibraryExpr:=ReadExpr(Obj,El,'Lib',aContext);
  El.LibrarySymbolName:=ReadExpr(Obj,El,'LibName',aContext);
  El.DispIDExpr:=ReadExpr(Obj,El,'DispId',aContext);
  ReadString(Obj,'Alias',El.AliasName,El);
  if ReadString(Obj,'Message',s,El) then
    begin
    El.MessageName:=s;
    El.MessageType:=pmtInteger;
    if ReadString(Obj,'MessageType',s,El) then
      begin
      Found:=false;
      for t in TProcedureMessageType do
        if s=PJUProcedureMessageTypeNames[t] then
          begin
          El.MessageType:=t;
          Found:=true;
          break;
          end;
      if not Found then
        RaiseMsg(20180211104537,El,'MessageType "'+s+'"');
      end;
    end;
  DefProcMods:=GetDefaultProcModifiers(El);
  El.Modifiers:=ReadProcedureModifiers(Obj,El,'PMods',DefProcMods);

  ReadProcedureScope(Obj,Scope,aContext);
end;

procedure TPJUReader.ReadOperator(Obj: TJSONObject; El: TPasOperator;
  aContext: TPJUReaderContext);
var
  s: string;
  Found, b: Boolean;
  t: TOperatorType;
begin
  ReadProcedure(Obj,El,aContext);
  if ReadString(Obj,'Operator',s,El) then
    begin
    Found:=false;
    for t in TOperatorType do
      if s=PJUOperatorTypeNames[t] then
        begin
        El.OperatorType:=t;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180211110647,El,'Operator "'+s+'"');
    end;
  if ReadBoolean(Obj,'TokenBased',b,El) then
    El.TokenBased:=b;
end;

procedure TPJUReader.ResolvePending;
var
  i: Integer;
  PendingIdentifierScope: TPJUReaderPendingIdentifierScope;
  Node: TAVLTreeNode;
  Ref: TPJUFilerElementRef;
begin
  for i:=0 to FPendingIdentifierScopes.Count-1 do
    begin
    PendingIdentifierScope:=TPJUReaderPendingIdentifierScope(FPendingIdentifierScopes[i]);
    ReadIdentifierScopeArray(PendingIdentifierScope.Arr,PendingIdentifierScope.Scope);
    end;

  Node:=FElementRefs.FindLowest;
  while Node<>nil do
    begin
    Ref:=TPJUFilerElementRef(Node.Data);
    {$IFDEF VerbosePJUFiler}
    write('TPJUReader.ResolvePending Ref.Id=',Ref.Id,' Ref.Element=',GetObjName(Ref.Element));
    {$ENDIF}
    Node:=FElementRefs.FindSuccessor(Node);
    if Ref.Pending<>nil then
      begin
      if Ref.Pending.ErrorEl<>nil then
        RaiseMsg(20180207194340,Ref.Pending.ErrorEl,IntToStr(Ref.Id))
      else
        RaiseMsg(20180207194341,Ref.Element,IntToStr(Ref.Id))
      end;
    end;
end;

constructor TPJUReader.Create;
begin
  inherited Create;
  FInitialFlags:=TPJUInitialFlags.Create;
  FPendingIdentifierScopes:=TObjectList.Create(true);
end;

destructor TPJUReader.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPendingIdentifierScopes);
  FreeAndNil(FInitialFlags);
end;

procedure TPJUReader.Clear;
begin
  FElementRefsArray:=nil;
  FPendingIdentifierScopes.Clear;
  inherited Clear;
  FInitialFlags.Clear;
end;

procedure TPJUReader.ReadPJU(aResolver: TPas2JSResolver; aStream: TStream);
var
  JParser: TJSONParser;
  Data: TJSONData;
begin
  JParser:=TJSONParser.Create(aStream,[joUTF8,joStrict]);
  try
    Data:=JParser.Parse;
    if not (Data is TJSONObject) then
      RaiseMsg(20180202130727,'expected JSON object, but found '+JSONTypeName(Data.JSONType));
  finally
    JParser.Free;
  end;
  ReadJSON(aResolver,TJSONObject(Data));
end;

procedure TPJUReader.ReadJSON(aResolver: TPas2JSResolver;
  Obj: TJSONObject);
var
  aName: String;
  Data: TJSONData;
  i: Integer;
  aContext: TPJUReaderContext;
  aModule: TPasModule;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModuleAsJSON START ');
  {$ENDIF}
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;

  ReadHeaderMagic(Obj);
  ReadHeaderVersion(Obj);

  aModule:=nil;
  for i:=0 to Obj.Count-1 do
    begin
    aName:=Obj.Names[i];
    {$IFDEF VerbosePJUFiler}
    writeln('TPJUReader.ReadModuleAsJSON ',aName);
    {$ENDIF}
    Data:=Obj.Elements[aName];
    case Obj.Names[i] of
    'FileType': ;
    'Version': ;
    'ParserOptions': InitialFlags.ParserOptions:=ReadParserOptions(Data,aModule,PJUDefaultParserOptions);
    'ModeSwitches': InitialFlags.ModeSwitches:=ReadModeSwitches(Data,aModule,PJUDefaultModeSwitches);
    'BoolSwitches': InitialFlags.BoolSwitches:=ReadBoolSwitches(Data,aModule,PJUDefaultBoolSwitches);
    'ConverterOptions': InitialFlags.ConverterOptions:=ReadConverterOptions(Data,aModule,PJUDefaultConvertOptions);
    'TargetPlatform': ReadTargetPlatform(Data);
    'TargetProcessor': ReadTargetProcessor(Data);
    'Sources': ReadSrcFiles(Data);
    'Module':
      begin
      aContext:=TPJUReaderContext.Create;
      try
        aContext.ModeSwitches:=InitialFlags.ModeSwitches;
        aContext.BoolSwitches:=InitialFlags.BoolSwitches;
        ReadModule(Data,aContext);
        aModule:=aResolver.RootElement;
      finally
        aContext.Free;
      end;
      end
    else
      RaiseMsg(20180202151706,'unknown property "'+aName+'"');
    end;
    end;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModuleAsJSON END');
  {$ENDIF}
end;

end.



{
    This file is part of the Free Component Library

    Pascal parse tree classes
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

unit PasTree;

interface

uses Classes;

resourcestring
  // Parse tree node type names
  SPasTreeElement = 'generic element';
  SPasTreeSection = 'unit section';
  SPasTreeProgramSection = 'program section';
  SPasTreeLibrarySection = 'library section';
  SPasTreeInterfaceSection = 'interface section';
  SPasTreeImplementationSection = 'implementation section';
  SPasTreeUsesUnit = 'uses unit';
  SPasTreeModule = 'module';
  SPasTreeUnit = 'unit';
  SPasTreeProgram = 'program';
  SPasTreePackage = 'package';
  SPasTreeResString = 'resource string';
  SPasTreeType = 'generic type';
  SPasTreePointerType = 'pointer type';
  SPasTreeAliasType = 'alias type';
  SPasTreeTypeAliasType = '"type" alias type';
  SPasTreeClassOfType = '"class of" type';
  SPasTreeRangeType = 'range type';
  SPasTreeArrayType = 'array type';
  SPasTreeFileType = 'file type';
  SPasTreeEnumValue = 'enumeration value';
  SPasTreeEnumType = 'enumeration type';
  SPasTreeSetType = 'set type';
  SPasTreeRecordType = 'record type';
  SPasStringType = 'string type';
  SPasTreeObjectType = 'object';
  SPasTreeClassType = 'class';
  SPasTreeInterfaceType = 'interface';
  SPasTreeGenericType = 'generic class';
  SPasTreeSpecializedType = 'specialized class type';
  SPasClassHelperType = 'Class helper type';
  SPasRecordHelperType = 'Record helper type';
  SPasTreeArgument = 'argument';
  SPasTreeProcedureType = 'procedure type';
  SPasTreeResultElement = 'function result';
  SPasTreeConstructorType = 'constructor type';
  SPasTreeDestructorType = 'destructor type';
  SPasTreeFunctionType = 'function type';
  SPasTreeUnresolvedTypeRef = 'unresolved type reference';
  SPasTreeVariable = 'variable';
  SPasTreeConst = 'constant';
  SPasTreeProperty = 'property';
  SPasTreeOverloadedProcedure = 'overloaded procedure';
  SPasTreeProcedure = 'procedure';
  SPasTreeFunction = 'function';
  SPasTreeOperator = 'operator';
  SPasTreeClassOperator = 'class operator';
  SPasTreeClassProcedure = 'class procedure';
  SPasTreeClassFunction = 'class function';
  SPasTreeClassConstructor = 'class constructor';
  SPasTreeClassDestructor = 'class destructor';
  SPasTreeConstructor = 'constructor';
  SPasTreeDestructor = 'destructor';
  SPasTreeProcedureImpl = 'procedure/function implementation';
  SPasTreeConstructorImpl = 'constructor implementation';
  SPasTreeDestructorImpl = 'destructor implementation';

type

  // Visitor pattern.
  TPassTreeVisitor = class;

  { TPasElementBase }

  TPasElementBase = class
  private
    FData: TObject;
  protected
    procedure Accept(Visitor: TPassTreeVisitor); virtual;
  public
    Property CustomData : TObject Read FData Write FData;
  end;
  TPasElementBaseClass = class of TPasElementBase;


  TPasModule = class;

  TPasMemberVisibility = (visDefault, visPrivate, visProtected, visPublic,
    visPublished, visAutomated,
    visStrictPrivate, visStrictProtected);

  TCallingConvention = (ccDefault,ccRegister,ccPascal,ccCDecl,ccStdCall,
                        ccOldFPCCall,ccSafeCall,ccSysCall);
  TProcTypeModifier = (ptmOfObject,ptmIsNested,ptmStatic,ptmVarargs,ptmReferenceTo);
  TProcTypeModifiers = set of TProcTypeModifier;
  TPackMode = (pmNone,pmPacked,pmBitPacked);

  TPasMemberVisibilities = set of TPasMemberVisibility;
  TPasMemberHint = (hDeprecated,hLibrary,hPlatform,hExperimental,hUnimplemented);
  TPasMemberHints = set of TPasMemberHint; 

  TPasElement = class;
  TPTreeElement = class of TPasElement;

  TOnForEachPasElement = procedure(El: TPasElement; arg: pointer) of object;

  { TPasElement }

  TPasElement = class(TPasElementBase)
  private
    FDocComment: String;
    FRefCount: LongWord;
    FName: string;
    FParent: TPasElement;
    FHints : TPasMemberHints;
    FHintMessage : String;
  protected
    procedure ProcessHints(const ASemiColonPrefix: boolean; var AResult: string); virtual;
  public
    SourceFilename: string;
    SourceLinenumber: Integer;
    Visibility: TPasMemberVisibility;
  public
    constructor Create(const AName: string; AParent: TPasElement); virtual;
    destructor Destroy; override;
    procedure AddRef;
    procedure Release;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); virtual;
    procedure ForEachChildCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer; Child: TPasElement; CheckParent: boolean); virtual;
    function FullPath: string;
    function ParentPath: string;
    function FullName: string; virtual;         // Name including parent's names
    function PathName: string; virtual;         // = Module.Name + FullName
    function GetModule: TPasModule;
    function ElementTypeName: string; virtual;
    Function HintsString : String;
    function GetDeclaration(full : Boolean) : string; virtual;
    procedure Accept(Visitor: TPassTreeVisitor); override;
    function HasParent(aParent: TPasElement): boolean;
    property RefCount: LongWord read FRefCount;
    property Name: string read FName write FName;
    property Parent: TPasElement read FParent Write FParent;
    Property Hints : TPasMemberHints Read FHints Write FHints;
    Property HintMessage : String Read FHintMessage Write FHintMessage;
    Property DocComment : String Read FDocComment Write FDocComment;
  end;

  TPasExprKind = (pekIdent, pekNumber, pekString, pekSet, pekNil, pekBoolConst,
     pekRange, pekUnary, pekBinary, pekFuncParams, pekArrayParams, pekListOfExp,
     pekInherited, pekSelf);

  TExprOpCode = (eopNone,
                 eopAdd,eopSubtract,eopMultiply,eopDivide, eopDiv,eopMod, eopPower,// arithmetic
                 eopShr,eopShl, // bit operations
                 eopNot,eopAnd,eopOr,eopXor, // logical/bit
                 eopEqual, eopNotEqual,  // Logical
                 eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual, // ordering
                 eopIn,eopIs,eopAs, eopSymmetricaldifference, // Specials
                 eopAddress, eopDeref, // Pointers
                 eopSubIdent); // SomeRec.A, A is subIdent of SomeRec

  { TPasExpr }

  TPasExpr = class(TPasElement)
    Kind      : TPasExprKind;
    OpCode    : TExprOpCode;
    format1,format2 : TPasExpr; // write, writeln, str
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; AOpCode: TExprOpCode); virtual; overload;
    destructor Destroy; override;
  end;

  { TUnaryExpr }

  TUnaryExpr = class(TPasExpr)
    Operand   : TPasExpr;
    constructor Create(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode); overload;
    function GetDeclaration(full : Boolean) : string; override;
    destructor Destroy; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TBinaryExpr }

  TBinaryExpr = class(TPasExpr)
    left      : TPasExpr;
    right     : TPasExpr;
    constructor Create(AParent : TPasElement; xleft, xright: TPasExpr; AOpCode: TExprOpCode); overload;
    constructor CreateRange(AParent : TPasElement; xleft, xright: TPasExpr); overload;
    function GetDeclaration(full : Boolean) : string; override;
    destructor Destroy; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPrimitiveExpr }

  TPrimitiveExpr = class(TPasExpr)
    Value     : AnsiString;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; const AValue : Ansistring); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;
  
  { TBoolConstExpr }

  TBoolConstExpr = class(TPasExpr)
    Value     : Boolean;
    constructor Create(AParent : TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TNilExpr }

  TNilExpr = class(TPasExpr)
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TInheritedExpr }

  TInheritedExpr = class(TPasExpr)
  Public
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;

  { TSelfExpr }

  TSelfExpr = class(TPasExpr)
    constructor Create(AParent : TPasElement); overload;
    function GetDeclaration(full : Boolean) : string; override;
  end;

  TPasExprArray = array of TPasExpr;

  { TParamsExpr - source position is the opening bracket }

  TParamsExpr = class(TPasExpr)
    Value     : TPasExpr;
    Params    : TPasExprArray;
    // Kind: pekArrayParams, pekFuncParams, pekSet
    constructor Create(AParent : TPasElement; AKind: TPasExprKind); overload;
    function GetDeclaration(full : Boolean) : string; override;
    destructor Destroy; override;
    procedure AddParam(xp: TPasExpr);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TRecordValues }

  TRecordValuesItem = record
    Name      : AnsiString;
    ValueExp  : TPasExpr;
  end;

  TRecordValues = class(TPasExpr)
    Fields    : array of TRecordValuesItem;
    constructor Create(AParent : TPasElement); overload;
    destructor Destroy; override;
    procedure AddField(const AName: AnsiString; Value: TPasExpr);
    function GetDeclaration(full : Boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TArrayValues }

  TArrayValues = class(TPasExpr)
    Values    : TPasExprArray;
    constructor Create(AParent : TPasElement); overload;
    destructor Destroy; override;
    procedure AddValues(AValue: TPasExpr);
    function GetDeclaration(full : Boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasDeclarations }

  TPasDeclarations = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Declarations: TFPList; // list of TPasElement
    // Declarations contains all the following:
    ResStrings, Types, Consts, Classes,
    Functions, Variables, Properties, ExportSymbols: TFPList;
  end;

  { TPasUsesUnit - Parent is TPasSection }

  TPasUsesUnit = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expr: TPasExpr; // name expression
    InFilename: TPrimitiveExpr; // Kind=pekString, can be nil
    Module: TPasElement; // TPasUnresolvedUnitRef or TPasModule
  end;
  TPasUsesClause = array of TPasUsesUnit;

  { TPasSection }

  TPasSection = class(TPasDeclarations)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function AddUnitToUsesList(const AUnitName: string; aName: TPasExpr = nil;
      InFilename: TPrimitiveExpr = nil; aModule: TPasElement = nil;
      UsesUnit: TPasUsesUnit = nil): TPasUsesUnit;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    UsesList: TFPList; // kept for compatibility, see TPasUsesUnit.Module
    UsesClause: TPasUsesClause;
  end;

  { TInterfaceSection }

  TInterfaceSection = class(TPasSection)
  public
    function ElementTypeName: string; override;
  end;

  { TImplementationSection }

  TImplementationSection = class(TPasSection)
  public
    function ElementTypeName: string; override;
  end;

  { TProgramSection }

  TProgramSection = class(TImplementationSection)
  public
    function ElementTypeName: string; override;
  end;

  { TLibrarySection }

  TLibrarySection = class(TImplementationSection)
  public
    function ElementTypeName: string; override;
  end;

  TInitializationSection = class;
  TFinalizationSection = class;

  { TPasModule }

  TPasModule = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    InterfaceSection: TInterfaceSection;
    ImplementationSection: TImplementationSection;
    InitializationSection: TInitializationSection; // in TPasProgram the begin..end.
    FinalizationSection: TFinalizationSection;
    PackageName: string;
    Filename   : String;  // the IN filename, only written when not empty.
  end;

  { TPasUnitModule }

  TPasUnitModule = Class(TPasModule)
    function ElementTypeName: string; override;
  end;

  { TPasProgram }

  TPasProgram = class(TPasModule)
  Public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  Public
    ProgramSection: TProgramSection;
    InputFile,OutPutFile : String;
    // Note: the begin..end. block is in the InitializationSection
  end;

  { TPasLibrary }

  TPasLibrary = class(TPasModule)
  Public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  Public
    LibrarySection: TLibrarySection;
    InputFile,OutPutFile : String;
  end;

  { TPasPackage }

  TPasPackage = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Modules: TFPList;     // List of TPasModule objects
  end;

  { TPasResString }

  TPasResString = class(TPasElement)
  public
    Destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean) : string; Override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expr: TPasExpr;
  end;

  { TPasType }

  TPasType = class(TPasElement)
  public
    function ElementTypeName: string; override;
  end;

  { TPasAliasType }

  TPasAliasType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean): string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    DestType: TPasType;
    Expr: TPasExpr;
  end;

  { TPasPointerType - todo: change it TPasAliasType }

  TPasPointerType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean): string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    DestType: TPasType;
  end;

  { TPasTypeAliasType }

  TPasTypeAliasType = class(TPasAliasType)
  public
    function ElementTypeName: string; override;
  end;

  { TPasClassOfType }

  TPasClassOfType = class(TPasAliasType)
  public
    function ElementTypeName: string; override;
    function GetDeclaration(full: boolean) : string; override;
  end;

  { TPasSpecializeType }

  TPasSpecializeType = class(TPasAliasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full: boolean) : string; override;
    procedure AddParam(El: TPasElement);
  public
    Params: TFPList; // list of TPasType or TPasExpr
  end;

  { TInlineTypeExpr }

  TInlineTypeExpr = class(TPasExpr)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : Boolean): string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    DestType: TPasType;
  end;

  { TInlineSpecializeExpr }

  TInlineSpecializeExpr = class(TInlineTypeExpr)
  end;

  { TPasRangeType }

  TPasRangeType = class(TPasType)
  public
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    RangeExpr : TBinaryExpr; // Kind=pekRange
    Destructor Destroy; override;
    Function RangeStart : String;
    Function RangeEnd : String;
  end;

  { TPasArrayType }

  TPasArrayType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    IndexRange : string; // only valid if Parser po_arrayrangeexpr disabled
    Ranges: TPasExprArray; // only valid if Parser po_arrayrangeexpr enabled
    PackMode : TPackMode;
    ElType: TPasType;
    Function IsGenericArray : Boolean;
    Function IsPacked : Boolean;
    procedure AddRange(Range: TPasExpr);
  end;

  { TPasFileType }

  TPasFileType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ElType: TPasType;
  end;

  { TPasEnumValue - Parent is TPasEnumType }

  TPasEnumValue = class(TPasElement)
  public
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Value: TPasExpr;
    Destructor Destroy; override;
    Function AssignedValue : string;
  end;

  { TPasEnumType }

  TPasEnumType = class(TPasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    Procedure GetEnumNames(Names : TStrings);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Values: TFPList;      // List of TPasEnumValue
  end;

  { TPasSetType }

  TPasSetType = class(TPasType)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    EnumType: TPasType;
    IsPacked : Boolean;
  end;

  TPasRecordType = class;

  { TPasVariant }

  TPasVariant = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Values: TFPList; // list of TPasElement
    Members: TPasRecordType;
  end;

  { TPasRecordType }

  TPasRecordType = class(TPasType)
  private
    procedure GetMembers(S: TStrings);
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    PackMode: TPackMode;
    Members: TFPList;     // array of TPasVariable elements
    VariantEl: TPasElement; // TPasVariable or TPasType
    Variants: TFPList;	// array of TPasVariant elements, may be nil!
    GenericTemplateTypes: TFPList; // list of TPasGenericTemplateType
    Function IsPacked: Boolean;
    Function IsBitPacked : Boolean;
    Function IsAdvancedRecord : Boolean;
    Procedure SetGenericTemplates(AList : TFPList);
  end;

  TPasGenericTemplateType = Class(TPasType);
  TPasObjKind = (okObject, okClass, okInterface, okGeneric, okSpecialize,
                 okClassHelper,okRecordHelper,okTypeHelper, okDispInterface);

  { TPasClassType }

  TPasClassType = class(TPasType)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    PackMode: TPackMode;
    ObjKind: TPasObjKind;
    AncestorType: TPasType;     // TPasClassType or TPasUnresolvedTypeRef or TPasAliasType or TPasTypeAliasType
    HelperForType: TPasType;     // TPasClassType or TPasUnresolvedTypeRef
    IsForward: Boolean;
    IsExternal : Boolean;
    IsShortDefinition: Boolean;//class(anchestor); without end
    GUIDExpr : TPasExpr;
    Members: TFPList;     // list of TPasElement
    Modifiers: TStringList;
    Interfaces : TFPList; // list of TPasElement
    GenericTemplateTypes: TFPList; // list of TPasGenericTemplateType
    ExternalNameSpace : String;
    ExternalName : String;
    Procedure SetGenericTemplates(AList : TFPList);
    Function FindMember(MemberClass : TPTreeElement; Const MemberName : String) : TPasElement;
    Function FindMemberInAncestors(MemberClass : TPTreeElement; Const MemberName : String) : TPasElement;
    Function IsPacked : Boolean;
    Function InterfaceGUID : string;
    Function IsSealed : Boolean;
    Function IsAbstract : Boolean;
    Function HasModifier(const aModifier: String): Boolean;
  end;



  TArgumentAccess = (argDefault, argConst, argVar, argOut, argConstRef);

  { TPasArgument }

  TPasArgument = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Access: TArgumentAccess;
    ArgType: TPasType; // can be nil, when Access<>argDefault
    ValueExpr: TPasExpr; // the default value
    Function Value : String;
  end;

  { TPasProcedureType }

  TPasProcedureType = class(TPasType)
  private
    function GetIsNested: Boolean;
    function GetIsOfObject: Boolean;
    function GetIsReference: Boolean;
    procedure SetIsNested(const AValue: Boolean);
    procedure SetIsOfObject(const AValue: Boolean);
    procedure SetIsReference(AValue: Boolean);
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    class function TypeName: string; virtual;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure GetArguments(List : TStrings);
    function CreateArgument(const AName, AUnresolvedTypeName: string):TPasArgument;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Args: TFPList;        // List of TPasArgument objects
    CallingConvention: TCallingConvention;
    Modifiers: TProcTypeModifiers;
    property IsOfObject: Boolean read GetIsOfObject write SetIsOfObject;
    property IsNested : Boolean read GetIsNested write SetIsNested;
    property IsReferenceTo : Boolean Read GetIsReference write SetIsReference;
  end;

  { TPasResultElement }

  TPasResultElement = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ResultType: TPasType;
  end;

  { TPasFunctionType }

  TPasFunctionType = class(TPasProcedureType)
  public
    destructor Destroy; override;
    class function TypeName: string; override;
    function ElementTypeName: string; override;
    function GetDeclaration(Full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ResultEl: TPasResultElement;
  end;

  TPasUnresolvedSymbolRef = class(TPasType)
  end;

  TPasUnresolvedTypeRef = class(TPasUnresolvedSymbolRef)
  public
    // Typerefs cannot be parented! -> AParent _must_ be NIL
    constructor Create(const AName: string; AParent: TPasElement); override;
    function ElementTypeName: string; override;
  end;

  { TPasUnresolvedUnitRef }

  TPasUnresolvedUnitRef = Class(TPasUnresolvedSymbolRef)
  public
    FileName : string;
    function ElementTypeName: string; override;
  end;

  { TPasStringType - e.g. string[len] }

  TPasStringType = class(TPasUnresolvedTypeRef)
  public
    LengthExpr : String;
    function ElementTypeName: string; override;
  end;

  { TPasTypeRef  - not used by TPasParser }

  TPasTypeRef = class(TPasUnresolvedTypeRef)
  public
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    RefType: TPasType;
  end;

  { TPasVariable }
  TVariableModifier = (vmCVar, vmExternal, vmPublic, vmExport, vmClass, vmStatic);
  TVariableModifiers = set of TVariableModifier;

  TPasVariable = class(TPasElement)
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    VarType: TPasType;
    VarModifiers : TVariableModifiers;
    LibraryName : TPasExpr; // libname of modifier external
    ExportName : TPasExpr; // symbol name of modifier external, export and public
    Modifiers : string;
    AbsoluteLocation : String;
    Expr: TPasExpr;
    Function Value : String;
  end;

  { TPasExportSymbol }

  TPasExportSymbol = class(TPasElement)
  public
    ExportName : TPasExpr;
    ExportIndex : TPasExpr;
    Destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasConst }

  TPasConst = class(TPasVariable)
  public
    IsConst: boolean; // e.g. $WritableConst off
    function ElementTypeName: string; override;
  end;

  { TPasProperty }

  TPasProperty = class(TPasVariable)
  private
    FResolvedType : TPasType;
    function GetIsClass: boolean;
    procedure SetIsClass(AValue: boolean);
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function GetDeclaration(full : boolean) : string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    IndexExpr: TPasExpr;
    ReadAccessor: TPasExpr;
    WriteAccessor: TPasExpr;
    ImplementsFunc: TPasExpr;
    DispIDExpr : TPasexpr;   // Can be nil.

    StoredAccessor: TPasExpr; // can be nil, if StoredAccessorName is 'True' or 'False'
    DefaultExpr: TPasExpr;
    Args: TFPList;        // List of TPasArgument objects
    ReadAccessorName, WriteAccessorName, ImplementsName,
      StoredAccessorName: string;
    DispIDReadOnly,
    IsDefault, IsNodefault: Boolean;
    property IsClass: boolean read GetIsClass write SetIsClass;
    Function ResolvedType : TPasType;
    Function IndexValue : String;
    Function DefaultValue : string;
  end;

  { TPasProcedureBase }

  TPasProcedureBase = class(TPasElement)
  public
    function TypeName: string; virtual; abstract;
  end;

  { TPasOverloadedProc }

  TPasOverloadedProc = class(TPasProcedureBase)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function TypeName: string; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Overloads: TFPList;           // List of TPasProcedure nodes
  end;

  { TPasProcedure }

  TProcedureModifier = (pmVirtual, pmDynamic, pmAbstract, pmOverride,
                        pmExport, pmOverload, pmMessage, pmReintroduce,
                        pmInline,pmAssembler, pmPublic,
                        pmCompilerProc,pmExternal,pmForward, pmDispId, 
                        pmNoReturn, pmfar, pmFinal);
  TProcedureModifiers = Set of TProcedureModifier;
  TProcedureMessageType = (pmtNone,pmtInteger,pmtString);
                        
  TProcedureBody = class;

  TPasProcedure = class(TPasProcedureBase)
  Private
    FModifiers : TProcedureModifiers;
    FMessageName : String;
    FMessageType : TProcedureMessageType;
    function GetCallingConvention: TCallingConvention;
    procedure SetCallingConvention(AValue: TCallingConvention);
  public
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function TypeName: string; override;
    function GetDeclaration(full: Boolean): string; override;
    procedure GetModifiers(List: TStrings);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ProcType : TPasProcedureType;
    Body : TProcedureBody;
    PublicName, // e.g. public PublicName;
    LibrarySymbolName,
    LibraryExpr : TPasExpr; // e.g. external LibraryExpr name LibrarySymbolName;
    DispIDExpr :  TPasExpr;
    AliasName : String;
    Procedure AddModifier(AModifier : TProcedureModifier);
    Function IsVirtual : Boolean;
    Function IsDynamic : Boolean;
    Function IsAbstract : Boolean;
    Function IsOverride : Boolean;
    Function IsExported : Boolean;
    Function IsExternal : Boolean;
    Function IsOverload : Boolean;
    Function IsMessage: Boolean;
    Function IsReintroduced : Boolean;
    Function IsStatic : Boolean;
    Function IsForward: Boolean;
    Property Modifiers : TProcedureModifiers Read FModifiers Write FModifiers;
    Property CallingConvention : TCallingConvention Read GetCallingConvention Write SetCallingConvention;
    Property MessageName : String Read FMessageName Write FMessageName;
    property MessageType : TProcedureMessageType Read FMessageType Write FMessageType;
  end;

  TPasFunction = class(TPasProcedure)
  private
    function GetFT: TPasFunctionType; inline;
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
    function GetDeclaration (full : boolean) : string; override;
    Property FuncType : TPasFunctionType Read GetFT;
  end;

  { TPasOperator }
  TOperatorType = (otUnknown,otImplicit,otExplicit,otMul,otPlus, otMinus, otDivision,otLessThan, otEqual,
                   otGreaterThan, otAssign,otNotEqual,otLessEqualThan,otGreaterEqualThan,otPower,
                   otSymmetricalDifference, otInc, otDec, otMod, otNegative, otPositive, otBitWiseOr, otDiv,
                   otLeftShift, otLogicalOr, otBitwiseAnd, otbitwiseXor,otLogicalAnd,otLogicalNot,otLogicalXor,
                   otRightShift,otEnumerator);
  TOperatorTypes = set of TOperatorType;

  TPasOperator = class(TPasFunction)
  private
    FOperatorType: TOperatorType;
    FTokenBased: Boolean;
    function NameSuffix: String;
  public
    Class Function OperatorTypeToToken(T : TOperatorType) : String;
    Class Function OperatorTypeToOperatorName(T: TOperatorType) : String;
    Class Function TokenToOperatorType(S : String) : TOperatorType;
    Class Function NameToOperatorType(S : String) : TOperatorType;
    Procedure CorrectName;
    // For backwards compatibility the old name can still be used to search on.
    function GetOperatorDeclaration(Full: Boolean): string;
    Function OldName(WithPath : Boolean) : String;
    function ElementTypeName: string; override;
    function TypeName: string; override;
    function GetDeclaration (full : boolean) : string; override;
    Property OperatorType : TOperatorType Read FOperatorType Write FOperatorType;
    // True if the declaration was using a token instead of an identifier
    Property TokenBased : Boolean Read FTokenBased Write FTokenBased;
  end;

Type
  { TPasClassOperator }

  TPasClassOperator = class(TPasOperator)
    function TypeName: string; override;
  end;


  { TPasConstructor }

  TPasConstructor = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasClassConstructor }

  TPasClassConstructor  = class(TPasConstructor)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasDestructor }

  TPasDestructor = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasClassDestructor }

  TPasClassDestructor  = class(TPasDestructor)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasClassProcedure }

  TPasClassProcedure = class(TPasProcedure)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasClassFunction }

  TPasClassFunction = class(TPasFunction)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  TPasImplBlock = class;

  { TProcedureBody - the var+type+const+begin, without the header, child of TPasProcedure }

  TProcedureBody = class(TPasDeclarations)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Body: TPasImplBlock;
  end;

  { TPasProcedureImpl - used by mkxmlrpc, not by pparser }

  TPasProcedureImpl = class(TPasElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    function ElementTypeName: string; override;
    function TypeName: string; virtual;
  public
    ProcType: TPasProcedureType;
    Locals: TFPList;
    Body: TPasImplBlock;
  end;

  { TPasConstructorImpl - used by mkxmlrpc, not by pparser }

  TPasConstructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasDestructorImpl - used by mkxmlrpc, not by pparser }

  TPasDestructorImpl = class(TPasProcedureImpl)
  public
    function ElementTypeName: string; override;
    function TypeName: string; override;
  end;

  { TPasImplElement - implementation element }

  TPasImplElement = class(TPasElement)
  end;

  { TPasImplCommand - currently used as empty statement, e.g. if then else ; }

  TPasImplCommand = class(TPasImplElement)
  public
    Command: string; // never set by TPasParser
  end;

  { TPasImplCommands - used by mkxmlrpc, not used by pparser }

  TPasImplCommands = class(TPasImplElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
  public
    Commands: TStrings;
  end;

  { TPasLabels }

  TPasLabels = class(TPasImplElement)
  public
    Labels: TStrings;
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
  end;

  TPasImplBeginBlock = class;
  TPasImplRepeatUntil = class;
  TPasImplIfElse = class;
  TPasImplWhileDo = class;
  TPasImplWithDo = class;
  TPasImplCaseOf = class;
  TPasImplForLoop = class;
  TPasImplTry = class;
  TPasImplExceptOn = class;
  TPasImplRaise = class;
  TPasImplAssign = class;
  TPasImplSimple = class;
  TPasImplLabelMark = class;

  { TPasImplBlock }

  TPasImplBlock = class(TPasImplElement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); virtual;
    function AddCommand(const ACommand: string): TPasImplCommand;
    function AddCommands: TPasImplCommands; // used by mkxmlrpc, not by pparser
    function AddBeginBlock: TPasImplBeginBlock;
    function AddRepeatUntil: TPasImplRepeatUntil;
    function AddIfElse(const ACondition: TPasExpr): TPasImplIfElse;
    function AddWhileDo(const ACondition: TPasExpr): TPasImplWhileDo;
    function AddWithDo(const Expression: TPasExpr): TPasImplWithDo;
    function AddCaseOf(const Expression: TPasExpr): TPasImplCaseOf;
    function AddForLoop(AVar: TPasVariable;
      const AStartValue, AEndValue: TPasExpr): TPasImplForLoop;
    function AddForLoop(AVarName : TPasExpr; AStartValue, AEndValue: TPasExpr;
      ADownTo: Boolean = false): TPasImplForLoop;
    function AddTry: TPasImplTry;
    function AddExceptOn(const VarName, TypeName: string): TPasImplExceptOn;
    function AddExceptOn(const VarName: string; VarType: TPasType): TPasImplExceptOn;
    function AddExceptOn(const VarEl: TPasVariable): TPasImplExceptOn;
    function AddExceptOn(const TypeEl: TPasType): TPasImplExceptOn;
    function AddRaise: TPasImplRaise;
    function AddLabelMark(const Id: string): TPasImplLabelMark;
    function AddAssign(left, right: TPasExpr): TPasImplAssign;
    function AddSimple(exp: TPasExpr): TPasImplSimple;
    function CloseOnSemicolon: boolean; virtual;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Elements: TFPList;    // list of TPasImplElement and maybe one TPasImplCaseElse
  end;

  { TPasImplStatement }

  TPasImplStatement = class(TPasImplBlock)
  public
    function CloseOnSemicolon: boolean; override;
  end;

  { TPasImplBeginBlock }

  TPasImplBeginBlock = class(TPasImplBlock)
  end;

  { TInitializationSection }

  TInitializationSection = class(TPasImplBlock)
  end;

  { TFinalizationSection }

  TFinalizationSection = class(TPasImplBlock)
  end;

  { TPasImplAsmStatement }

  TPasImplAsmStatement = class (TPasImplStatement)
  private
    FTokens: TStrings;
  Public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    Property Tokens : TStrings Read FTokens;
  end;

  { TPasImplRepeatUntil }

  TPasImplRepeatUntil = class(TPasImplBlock)
  public
    ConditionExpr : TPasExpr;
    destructor Destroy; override;
    Function Condition: string;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasImplIfElse }

  TPasImplIfElse = class(TPasImplBlock)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    function CloseOnSemicolon: boolean; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ConditionExpr: TPasExpr;
    IfBranch: TPasImplElement;
    ElseBranch: TPasImplElement; // can be nil
    Function Condition: string;
  end;

  { TPasImplWhileDo }

  TPasImplWhileDo = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    ConditionExpr : TPasExpr;
    Body: TPasImplElement;
    function Condition: string;
  end;

  { TPasImplWithDo }

  TPasImplWithDo = class(TPasImplStatement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure AddExpression(const Expression: TPasExpr);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expressions: TFPList; // list of TPasExpr
    Body: TPasImplElement;
  end;

  TPasImplCaseStatement = class;
  TPasImplCaseElse = class;

  { TPasImplCaseOf - Elements are TPasImplCaseStatement }

  TPasImplCaseOf = class(TPasImplBlock)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    function AddCase(const Expression: TPasExpr): TPasImplCaseStatement;
    function AddElse: TPasImplCaseElse;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    CaseExpr : TPasExpr;
    ElseBranch: TPasImplCaseElse; // this is also in Elements
    function Expression: string;
  end;

  { TPasImplCaseStatement }

  TPasImplCaseStatement = class(TPasImplStatement)
  public
    constructor Create(const AName: string; AParent: TPasElement); override;
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure AddExpression(const Expr: TPasExpr);
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    Expressions: TFPList; // list of TPasExpr
    Body: TPasImplElement;
  end;

  { TPasImplCaseElse }

  TPasImplCaseElse = class(TPasImplBlock)
  end;

  { TPasImplForLoop }

  TLoopType = (ltNormal,ltDown,ltIn);
  TPasImplForLoop = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    VariableName : TPasExpr;
    LoopType : TLoopType;
    StartExpr : TPasExpr;
    EndExpr : TPasExpr;
    Body: TPasImplElement;
    Variable: TPasVariable; // not used by TPasParser
    Function Down: boolean; // downto, backward compatibility
    Function StartValue : String;
    Function EndValue: string;
  end;

  { TPasImplAssign }
  TAssignKind = (akDefault,akAdd,akMinus,akMul,akDivision);
  TPasImplAssign = class (TPasImplStatement)
  public
    left  : TPasExpr;
    right : TPasExpr;
    Kind : TAssignKind;
    Destructor Destroy; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  { TPasImplSimple }

  TPasImplSimple = class (TPasImplStatement)
  public
    expr  : TPasExpr;
    Destructor Destroy; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  end;

  TPasImplTryHandler = class;
  TPasImplTryFinally = class;
  TPasImplTryExcept = class;
  TPasImplTryExceptElse = class;

  { TPasImplTry }

  TPasImplTry = class(TPasImplBlock)
  public
    destructor Destroy; override;
    function AddFinally: TPasImplTryFinally;
    function AddExcept: TPasImplTryExcept;
    function AddExceptElse: TPasImplTryExceptElse;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    FinallyExcept: TPasImplTryHandler;
    ElseBranch: TPasImplTryExceptElse;
  end;

  TPasImplTryHandler = class(TPasImplBlock)
  end;

  { TPasImplTryFinally }

  TPasImplTryFinally = class(TPasImplTryHandler)
  end;

  { TPasImplTryExcept }

  TPasImplTryExcept = class(TPasImplTryHandler)
  end;

  { TPasImplTryExceptElse }

  TPasImplTryExceptElse = class(TPasImplTryHandler)
  end;

  { TPasImplExceptOn }

  TPasImplExceptOn = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure AddElement(Element: TPasImplElement); override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  public
    VarEl: TPasVariable; // can be nil
    TypeEl : TPasType;
    Body: TPasImplElement;
    Function VariableName : String;
    Function TypeName: string;
  end;

  { TPasImplRaise }

  TPasImplRaise = class(TPasImplStatement)
  public
    destructor Destroy; override;
    procedure ForEachCall(const aMethodCall: TOnForEachPasElement;
      const Arg: Pointer); override;
  Public
    ExceptObject,
    ExceptAddr : TPasExpr;
  end;

  { TPasImplLabelMark }

  TPasImplLabelMark = class(TPasImplElement)
  public
    LabelId: AnsiString;
  end;

  { TPassTreeVisitor }

  TPassTreeVisitor = class
  public
    procedure Visit(obj: TPasElement); virtual;
  end;

const
  AccessNames: array[TArgumentAccess] of string[9] = ('', 'const ', 'var ', 'out ','constref ');
  AccessDescriptions: array[TArgumentAccess] of string[9] = ('default', 'const', 'var', 'out','constref');
  AllVisibilities: TPasMemberVisibilities =
     [visDefault, visPrivate, visProtected, visPublic,
      visPublished, visAutomated];

  VisibilityNames: array[TPasMemberVisibility] of string = (
    'default','private', 'protected', 'public', 'published', 'automated',
    'strict private', 'strict protected');

  ObjKindNames: array[TPasObjKind] of string = (
    'object', 'class', 'interface','class','class','class helper','record helper','type helper','dispinterface');

  ExprKindNames : Array[TPasExprKind] of string = (
      'Ident',
      'Number',
      'String',
      'Set',
      'Nil',
      'BoolConst',
      'Range',
      'Unary',
      'Binary',
      'FuncParams',
      'ArrayParams',
      'ListOfExp',
      'Inherited',
      'Self');

  OpcodeStrings : Array[TExprOpCode] of string = (
        '','+','-','*','/','div','mod','**',
        'shr','shl',
        'not','and','or','xor',
        '=','<>',
        '<','>','<=','>=',
        'in','is','as','><',
        '@','^',
        '.');


  UnaryOperators = [otImplicit,otExplicit,otAssign,otNegative,otPositive,otEnumerator];

  OperatorTokens : Array[TOperatorType] of string
       =  ('','','','*','+','-','/','<','=',
           '>',':=','<>','<=','>=','**',
           '><','Inc','Dec','mod','-','+','Or','div',
           'shl','or','and','xor','and','not','xor',
           'shr','enumerator');
  OperatorNames : Array[TOperatorType] of string
       =  ('','implicit','explicit','multiply','add','subtract','divide','lessthan','equal',
           'greaterthan','assign','notequal','lessthanorequal','greaterthanorequal','power',
           'symmetricaldifference','inc','dec','modulus','negative','positive','bitwiseor','intdivide',
           'leftshift','logicalor','bitwiseand','bitwisexor','logicaland','logicalnot','logicalxor',
           'rightshift','enumerator');

  AssignKindNames : Array[TAssignKind] of string = (':=','+=','-=','*=','/=' );

  cPasMemberHint : Array[TPasMemberHint] of string =
      ( 'deprecated', 'library', 'platform', 'experimental', 'unimplemented' );
  cCallingConventions : Array[TCallingConvention] of string =
      ( '', 'Register','Pascal','CDecl','StdCall','OldFPCCall','SafeCall','SysCall');
  ProcTypeModifiers : Array[TProcTypeModifier] of string =
      ('of Object', 'is nested','static','varargs','reference to');

  ModifierNames : Array[TProcedureModifier] of string
                = ('virtual', 'dynamic','abstract', 'override',
                   'export', 'overload', 'message', 'reintroduce',
                   'inline','assembler','public',
                   'compilerproc','external','forward','dispid',
                   'noreturn','far','final');

  VariableModifierNames : Array[TVariableModifier] of string
     = ('cvar', 'external', 'public', 'export', 'class', 'static');

procedure ReleaseAndNil(var El: TPasElement); overload;

implementation

uses SysUtils;

procedure ReleaseAndNil(var El: TPasElement);
begin
  if El=nil then exit;
  {$IFDEF VerbosePasTreeMem}writeln('ReleaseAndNil ',El.Name,' ',El.ClassName);{$ENDIF}
  El.Release;
  El:=nil;
end;

{ TInlineTypeExpr }

destructor TInlineTypeExpr.Destroy;
begin
  ReleaseAndNil(TPasElement(DestType));
  inherited Destroy;
end;

function TInlineTypeExpr.ElementTypeName: string;
begin
  Result := DestType.ElementTypeName;
end;

function TInlineTypeExpr.GetDeclaration(full: Boolean): string;
begin
  Result:=DestType.GetDeclaration(full);
end;

procedure TInlineTypeExpr.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
begin
  DestType.ForEachChildCall(aMethodCall,Arg,DestType,true);
end;

{ TPasSpecializeType }

constructor TPasSpecializeType.Create(const AName: string; AParent: TPasElement
  );
begin
  inherited Create(AName, AParent);
  Params:=TFPList.Create;
end;

destructor TPasSpecializeType.Destroy;
var
  i: Integer;
begin
  for i:=0 to Params.Count-1 do
    TPasElement(Params[i]).Release;
  FreeAndNil(Params);
  inherited Destroy;
end;

function TPasSpecializeType.ElementTypeName: string;
begin
  Result:=SPasTreeSpecializedType;
end;

function TPasSpecializeType.GetDeclaration(full: boolean): string;
var
  i: Integer;
begin
  Result:='specialize '+DestType.Name+'<';
  for i:=0 to Params.Count-1 do
    begin
    if i>0 then
      Result:=Result+',';
    Result:=Result+TPasElement(Params[i]).GetDeclaration(false);
    end;
  If Full then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasSpecializeType.AddParam(El: TPasElement);
begin
  Params.Add(El);
end;

{ TInterfaceSection }

function TInterfaceSection.ElementTypeName: string;
begin
  Result:=SPasTreeInterfaceSection;
end;

{ TLibrarySection }

function TLibrarySection.ElementTypeName: string;
begin
  Result:=SPasTreeLibrarySection;
end;

{ TProgramSection }

function TProgramSection.ElementTypeName: string;
begin
  Result:=SPasTreeProgramSection;
end;

{ TImplementationSection }

function TImplementationSection.ElementTypeName: string;
begin
  Result:=SPasTreeImplementationSection;
end;

{ TPasUsesUnit }

destructor TPasUsesUnit.Destroy;
begin
  ReleaseAndNil(TPasElement(Expr));
  ReleaseAndNil(TPasElement(InFilename));
  ReleaseAndNil(TPasElement(Module));
  inherited Destroy;
end;

function TPasUsesUnit.ElementTypeName: string;
begin
  Result := SPasTreeUsesUnit;
end;

procedure TPasUsesUnit.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
  ForEachChildCall(aMethodCall,Arg,InFilename,false);
  ForEachChildCall(aMethodCall,Arg,Module,true);
end;

{ TPasElementBase }

procedure TPasElementBase.Accept(Visitor: TPassTreeVisitor);
begin
  if Visitor=nil then ;
end;

{ TPasTypeRef }

procedure TPasTypeRef.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,RefType,true);
end;

{ TPasClassOperator }

function TPasClassOperator.TypeName: string;
begin
  Result:='class operator';
end;

{ TPasImplAsmStatement }

constructor TPasImplAsmStatement.Create(const AName: string;
  AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  FTokens:=TStringList.Create;
end;

destructor TPasImplAsmStatement.Destroy;
begin
  FreeAndNil(FTokens);
  inherited Destroy;
end;

{ TPasClassConstructor }

function TPasClassConstructor.TypeName: string;
begin
  Result:='class '+ inherited TypeName;
end;

{ TPasImplRaise }

destructor TPasImplRaise.Destroy;
begin
  ReleaseAndNil(TPasElement(ExceptObject));
  ReleaseAndNil(TPasElement(ExceptAddr));
  Inherited;
end;

procedure TPasImplRaise.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ExceptObject,false);
  ForEachChildCall(aMethodCall,Arg,ExceptAddr,false);
end;

{ TPasImplRepeatUntil }

destructor TPasImplRepeatUntil.Destroy;
begin
  ReleaseAndNil(TPasElement(ConditionExpr));
  inherited Destroy;
end;

function TPasImplRepeatUntil.Condition: string;
begin
  If Assigned(ConditionExpr) then
    Result:=ConditionExpr.GetDeclaration(True)
  else
    Result:='';
end;

procedure TPasImplRepeatUntil.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ConditionExpr,false);
end;

{ TPasImplSimple }

destructor TPasImplSimple.Destroy;
begin
  ReleaseAndNil(TPasElement(Expr));
  inherited Destroy;
end;

procedure TPasImplSimple.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
end;

{ TPasImplAssign }

destructor TPasImplAssign.Destroy;
begin
  ReleaseAndNil(TPasElement(Left));
  ReleaseAndNil(TPasElement(Right));
  inherited Destroy;
end;

procedure TPasImplAssign.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,left,false);
  ForEachChildCall(aMethodCall,Arg,right,false);
end;

{ TPasExportSymbol }

destructor TPasExportSymbol.Destroy;
begin
  ReleaseAndNil(TPasElement(ExportName));
  ReleaseAndNil(TPasElement(ExportIndex));
  inherited Destroy;
end;

function TPasExportSymbol.ElementTypeName: string;
begin
  Result:='Export'
end;

function TPasExportSymbol.GetDeclaration(full: boolean): string;
begin
  Result:=Name;
  if (ExportName<>Nil) then
    Result:=Result+' name '+ExportName.GetDeclaration(Full)
  else if (ExportIndex<>Nil) then
    Result:=Result+' index '+ExportIndex.GetDeclaration(Full);
end;

procedure TPasExportSymbol.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ExportName,false);
  ForEachChildCall(aMethodCall,Arg,ExportIndex,false);
end;

{ TPasUnresolvedUnitRef }

function TPasUnresolvedUnitRef.ElementTypeName: string;
begin
  Result:=SPasTreeUnit;
end;

{ TPasLibrary }

destructor TPasLibrary.Destroy;
begin
  ReleaseAndNil(TPasElement(LibrarySection));
  inherited Destroy;
end;

function TPasLibrary.ElementTypeName: string;
begin
  Result:=inherited ElementTypeName;
end;

procedure TPasLibrary.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,LibrarySection,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasProgram }

destructor TPasProgram.Destroy;
begin
  {$IFDEF VerbosePasTreeMem}writeln('TPasProgram.Destroy ProgramSection');{$ENDIF}
  ReleaseAndNil(TPasElement(ProgramSection));
  {$IFDEF VerbosePasTreeMem}writeln('TPasProgram.Destroy inherited');{$ENDIF}
  inherited Destroy;
  {$IFDEF VerbosePasTreeMem}writeln('TPasProgram.Destroy END');{$ENDIF}
end;

function TPasProgram.ElementTypeName: string;
begin
  Result:=inherited ElementTypeName;
end;

procedure TPasProgram.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,ProgramSection,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasUnitModule }

function TPasUnitModule.ElementTypeName: string;
begin
  Result:=SPasTreeUnit;
end;

{ TPasStringType }


{$IFNDEF FPC}
  const
    LineEnding = sLineBreak;
{$ENDIF}

{ Parse tree element type name functions }

function TPasElement.ElementTypeName: string; begin Result := SPasTreeElement end;

function TPasElement.HintsString: String;

Var
  H : TPasmemberHint;

begin
  Result:='';
  For H := Low(TPasmemberHint) to High(TPasMemberHint) do
    if H in Hints then
      begin
      If (Result<>'') then
        Result:=Result+'; ';
      Result:=Result+cPasMemberHint[h];
      end;
end;

function TPasDeclarations.ElementTypeName: string; begin Result := SPasTreeSection end;

procedure TPasDeclarations.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Declarations.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Declarations[i]),false);
end;

function TPasModule.ElementTypeName: string; begin Result := SPasTreeModule end;
function TPasPackage.ElementTypeName: string; begin Result := SPasTreePackage end;

procedure TPasPackage.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Modules.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasModule(Modules[i]),true);
end;

function TPasResString.ElementTypeName: string; begin Result := SPasTreeResString; end;
function TPasType.ElementTypeName: string; begin Result := SPasTreeType; end;
function TPasPointerType.ElementTypeName: string; begin Result := SPasTreePointerType; end;
function TPasAliasType.ElementTypeName: string; begin Result := SPasTreeAliasType; end;
function TPasTypeAliasType.ElementTypeName: string; begin Result := SPasTreeTypeAliasType; end;
function TPasClassOfType.ElementTypeName: string; begin Result := SPasTreeClassOfType; end;
function TPasRangeType.ElementTypeName: string; begin Result := SPasTreeRangeType; end;
function TPasArrayType.ElementTypeName: string; begin Result := SPasTreeArrayType; end;
function TPasFileType.ElementTypeName: string; begin Result := SPasTreeFileType; end;
function TPasEnumValue.ElementTypeName: string; begin Result := SPasTreeEnumValue; end;

procedure TPasEnumValue.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Value,false);
end;

destructor TPasEnumValue.Destroy;
begin
  ReleaseAndNil(TPasElement(Value));
  inherited Destroy;
end;

function TPasEnumValue.AssignedValue: string;
begin
  If Assigned(Value) then
    Result:=Value.GetDeclaration(True)
  else
    Result:='';
end;

function TPasEnumType.ElementTypeName: string; begin Result := SPasTreeEnumType end;
function TPasSetType.ElementTypeName: string; begin Result := SPasTreeSetType end;
function TPasRecordType.ElementTypeName: string; begin Result := SPasTreeRecordType end;
function TPasArgument.ElementTypeName: string; begin Result := SPasTreeArgument end;
function TPasProcedureType.ElementTypeName: string; begin Result := SPasTreeProcedureType end;
function TPasResultElement.ElementTypeName: string; begin Result := SPasTreeResultElement end;

procedure TPasResultElement.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ResultType,true);
end;

function TPasFunctionType.ElementTypeName: string; begin Result := SPasTreeFunctionType end;
function TPasUnresolvedTypeRef.ElementTypeName: string; begin Result := SPasTreeUnresolvedTypeRef end;
function TPasVariable.ElementTypeName: string; begin Result := SPasTreeVariable end;
function TPasConst.ElementTypeName: string; begin Result := SPasTreeConst end;
function TPasProperty.ElementTypeName: string; begin Result := SPasTreeProperty end;
function TPasOverloadedProc.ElementTypeName: string; begin Result := SPasTreeOverloadedProcedure end;
function TPasProcedure.ElementTypeName: string; begin Result := SPasTreeProcedure end;

function TPasFunction.GetFT: TPasFunctionType;
begin
  Result:=ProcType as TPasFunctionType;
end;

function TPasFunction.ElementTypeName: string; begin Result := SPasTreeFunction end;
function TPasClassProcedure.ElementTypeName: string; begin Result := SPasTreeClassProcedure; end;
function TPasClassConstructor.ElementTypeName: string; begin Result := SPasTreeClassConstructor; end;
function TPasClassDestructor.ElementTypeName: string; begin Result := SPasTreeClassDestructor; end;

function TPasClassDestructor.TypeName: string;
begin
  Result:='destructor';
end;

function TPasClassFunction.ElementTypeName: string; begin Result := SPasTreeClassFunction; end;

class function TPasOperator.OperatorTypeToToken(T: TOperatorType): String;
begin
  Result:=OperatorTokens[T];
end;

class function TPasOperator.OperatorTypeToOperatorName(T: TOperatorType
  ): String;
begin
  Result:=OperatorNames[T];
end;

class function TPasOperator.TokenToOperatorType(S: String): TOperatorType;
begin
  Result:=High(TOperatorType);
  While (Result>otUnknown) and (CompareText(S,OperatorTokens[Result])<>0) do
    Result:=Pred(Result);
end;

class function TPasOperator.NameToOperatorType(S: String): TOperatorType;
begin
  Result:=High(TOperatorType);
  While (Result>otUnknown) and (CompareText(S,OperatorNames[Result])<>0) do
    Result:=Pred(Result);
end;

Function TPasOperator.NameSuffix : String;

Var
  I : Integer;

begin
  Result:='(';
  if Assigned(ProcType) and Assigned(ProcType.Args) then
  for i:=0 to ProcType.Args.Count-1 do
    begin
    if i>0 then
      Result:=Result+',';
    Result:=Result+TPasArgument(ProcType.Args[i]).ArgType.Name;
    end;
  Result:=Result+')';
  if Assigned(TPasFunctionType(ProcType)) and
     Assigned(TPasFunctionType(ProcType).ResultEl) and
     Assigned(TPasFunctionType(ProcType).ResultEl.ResultType) then
    Result:=Result+':'+TPasFunctionType(ProcType).ResultEl.ResultType.Name;
end;

procedure TPasOperator.CorrectName;

begin
  Name:=OperatorNames[OperatorType]+NameSuffix;
end;

function TPasOperator.OldName(WithPath : Boolean): String;

Var
  I : Integer;
  S : String;
begin
  Result:=TypeName+' '+OperatorTokens[OperatorType];
  Result := Result + '(';
  if Assigned(ProcType) then
    begin
    for i := 0 to ProcType.Args.Count - 1 do
      begin
      if i > 0 then
        Result := Result + ', ';
      Result := Result + TPasArgument(ProcType.Args[i]).ArgType.Name;
      end;
    Result := Result + '): ' + TPasFunctionType(ProcType).ResultEl.ResultType.Name;
    If WithPath then
      begin
      S:=Self.ParentPath;
      if (S<>'') then
        Result:=S+'.'+Result;
      end;
    end;
end;

function TPasOperator.ElementTypeName: string;
begin
  Result := SPasTreeOperator
end;

function TPasConstructor.ElementTypeName: string; begin Result := SPasTreeConstructor end;
function TPasDestructor.ElementTypeName: string; begin Result := SPasTreeDestructor end;
function TPasProcedureImpl.ElementTypeName: string; begin Result := SPasTreeProcedureImpl end;
function TPasConstructorImpl.ElementTypeName: string; begin Result := SPasTreeConstructorImpl end;
function TPasDestructorImpl.ElementTypeName: string; begin Result := SPasTreeDestructorImpl end;
function TPasStringType.ElementTypeName: string; begin Result:=SPasStringType;end;


{ All other stuff: }

procedure TPasElement.ProcessHints(const ASemiColonPrefix: boolean; var AResult: string);
var
  S : String;
begin
  if Hints <> [] then
    begin
    if ASemiColonPrefix then
      AResult := AResult + ';';
    S:=HintsString;
    if (S<>'') then
      AResult:=AResult+' '+S;
    if ASemiColonPrefix then
      AResult:=AResult+';';
    end;
end;

constructor TPasElement.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
end;

destructor TPasElement.Destroy;
begin
  if (FRefCount>0) and (FRefCount<high(FRefCount)) then
    begin
    {$if defined(debugrefcount) or defined(VerbosePasTreeMem)}writeln('TPasElement.Destroy ',Name,':',ClassName);{$ENDIF}
    raise Exception.Create('');
    end;
  FParent:=nil;
  inherited Destroy;
end;

procedure TPasElement.AddRef;
begin
  Inc(FRefCount);
end;

{ $define debugrefcount}

procedure TPasElement.Release;

{$if defined(debugrefcount) or defined(VerbosePasTreeMem)}
Var
  Cn : String;
  {$endif}

begin
{$if defined(debugrefcount) or defined(VerbosePasTreeMem)}
  CN:=ClassName+' '+Name;
  CN:=CN+' '+IntToStr(FRefCount);
  //If Assigned(Parent) then
  //  CN:=CN+' ('+Parent.ClassName+')';
  Writeln('TPasElement.Release : ',Cn);
{$endif}
  if FRefCount = 0 then
    begin
    FRefCount:=High(FRefCount);
    Free;
    end
  else if FRefCount=High(FRefCount) then
    begin
    {$if defined(debugrefcount) or defined(VerbosePasTreeMem)}  Writeln('TPasElement.Released OUCH: ',Cn); {$endif}
    raise Exception.Create('');
    end
  else
    Dec(FRefCount);
{$if defined(debugrefcount) or defined(VerbosePasTreeMem)}  Writeln('TPasElement.Released : ',Cn); {$endif}
end;

procedure TPasElement.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  aMethodCall(Self,Arg);
end;

procedure TPasElement.ForEachChildCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer; Child: TPasElement; CheckParent: boolean);
begin
  if (Child=nil) then exit;
  if CheckParent and (not Child.HasParent(Self)) then exit;
  Child.ForEachCall(aMethodCall,Arg);
end;

function TPasElement.FullPath: string;

var
  p: TPasElement;

begin
  Result := '';
  p := Parent;
  while Assigned(p) and not p.InheritsFrom(TPasDeclarations) do
  begin
    if (not (p is TPasOverloadedProc)) and (Length(p.Name) > 0) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.FullName: string;


begin
  Result := FullPath;
  if Result<>'' then
    Result:=Result+'.'+Name
  else
    Result:=Name;
end;

function TPasElement.ParentPath: string;

var
  p: TPasElement;
begin
  Result:='';
  p := Parent;
  while Assigned(p) do
  begin
    if (Not (p is TPasOverloadedProc)) and (Length(p.Name) > 0) then
      if Length(Result) > 0 then
        Result := p.Name + '.' + Result
      else
        Result := p.Name;
    p := p.Parent;
  end;
end;

function TPasElement.PathName: string;

begin
  Result := ParentPath;
  if Result<>'' then
    Result:=Result+'.'+Name
  else
    Result:=Name;
end;

function TPasElement.GetModule: TPasModule;

Var
  p : TPaselement;
begin
  if self is  TPasPackage then
    Result := nil
  else
    begin
    P:=Self;
    While (P<>Nil) and Not (P is TPasModule) do
      P:=P.Parent;
    Result:=TPasModule(P);
    end;
end;

function TPasElement.GetDeclaration(full: Boolean): string;

begin
  if Full then
    Result := Name
  else
    Result := '';
end;

procedure TPasElement.Accept(Visitor: TPassTreeVisitor);
begin
  Visitor.Visit(Self);
end;

function TPasElement.HasParent(aParent: TPasElement): boolean;
var
  El: TPasElement;
begin
  El:=Parent;
  while El<>nil do
    begin
    if El=aParent then exit(true);
    El:=El.Parent;
    end;
  Result:=false;
end;

constructor TPasDeclarations.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Declarations := TFPList.Create;
  ResStrings := TFPList.Create;
  Types := TFPList.Create;
  Consts := TFPList.Create;
  Classes := TFPList.Create;
  Functions := TFPList.Create;
  Variables := TFPList.Create;
  Properties := TFPList.Create;
  ExportSymbols := TFPList.Create;
end;

destructor TPasDeclarations.Destroy;
var
  i: Integer;
begin
  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy START');{$ENDIF}
  FreeAndNil(ExportSymbols);
  FreeAndNil(Properties);
  FreeAndNil(Variables);
  FreeAndNil(Functions);
  FreeAndNil(Classes);
  FreeAndNil(Consts);
  FreeAndNil(Types);
  FreeAndNil(ResStrings);
  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy Declarations');{$ENDIF}
  for i := 0 to Declarations.Count - 1 do
    TPasElement(Declarations[i]).Release;
  FreeAndNil(Declarations);

  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy inherited');{$ENDIF}
  inherited Destroy;
  {$IFDEF VerbosePasTreeMem}writeln('TPasDeclarations.Destroy END');{$ENDIF}
end;

destructor TPasModule.Destroy;
begin
  {$IFDEF VerbosePasTreeMem}writeln('TPasModule.Destroy interface');{$ENDIF}
  ReleaseAndNil(TPasElement(InterfaceSection));
  {$IFDEF VerbosePasTreeMem}writeln('TPasModule.Destroy implementation');{$ENDIF}
  ReleaseAndNil(TPasElement(ImplementationSection));
  {$IFDEF VerbosePasTreeMem}writeln('TPasModule.Destroy initialization');{$ENDIF}
  ReleaseAndNil(TPasElement(InitializationSection));
  {$IFDEF VerbosePasTreeMem}writeln('TPasModule.Destroy finalization');{$ENDIF}
  ReleaseAndNil(TPasElement(FinalizationSection));
  {$IFDEF VerbosePasTreeMem}writeln('TPasModule.Destroy inherited');{$ENDIF}
  inherited Destroy;
  {$IFDEF VerbosePasTreeMem}writeln('TPasModule.Destroy END');{$ENDIF}
end;


constructor TPasPackage.Create(const AName: string; AParent: TPasElement);
begin
  if (Length(AName) > 0) and (AName[1] <> '#') then
    inherited Create('#' + AName, AParent)
  else
    inherited Create(AName, AParent);
  Modules := TFPList.Create;
end;

destructor TPasPackage.Destroy;
var
  i: Integer;
begin
  for i := 0 to Modules.Count - 1 do
    TPasModule(Modules[i]).Release;
  FreeAndNil(Modules);
  inherited Destroy;
end;


destructor TPasPointerType.Destroy;
begin
  if Assigned(DestType) then
    DestType.Release;
  inherited Destroy;
end;


destructor TPasAliasType.Destroy;
begin
  ReleaseAndNil(TPasElement(DestType));
  ReleaseAndNil(TPasElement(Expr));
  inherited Destroy;
end;


destructor TPasArrayType.Destroy;
var
  i: Integer;
begin
  for i:=0 to length(Ranges)-1 do
    Ranges[i].Release;
  if Assigned(ElType) then
    ElType.Release;
  inherited Destroy;
end;

destructor TPasFileType.Destroy;
begin
  if Assigned(ElType) then
    ElType.Release;
  inherited Destroy;
end;


constructor TPasEnumType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TFPList.Create;
end;

destructor TPasEnumType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Values.Count - 1 do
    TPasEnumValue(Values[i]).Release;
  FreeAndNil(Values);
  inherited Destroy;
end;

procedure TPasEnumType.GetEnumNames(Names: TStrings);
var
  i: Integer;
begin
  with Values do
  begin
    for i := 0 to Count - 2 do
      Names.Add(TPasEnumValue(Items[i]).Name + ',');
    if Count > 0 then
      Names.Add(TPasEnumValue(Items[Count - 1]).Name);
  end;
end;

procedure TPasEnumType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Values.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasEnumValue(Values[i]),false);
end;


constructor TPasVariant.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Values := TFPList.Create;
end;

destructor TPasVariant.Destroy;

Var
  I : Integer;

begin
  For I:=0 to Values.Count-1 do
    TPasElement(Values[i]).Release;
  FreeAndNil(Values);
  if Assigned(Members) then
    ReleaseAndNil(TpasElement(Members));
  inherited Destroy;
end;

function TPasVariant.GetDeclaration(full: boolean): string;

Var
  i : Integer;
  S : TStrings;

begin
  Result:='';
  For I:=0 to Values.Count-1 do
    begin
    if (Result<>'') then
      Result:=Result+', ';
    Result:=Result+TPasElement(Values[i]).GetDeclaration(False);
    Result:=Result+': ('+sLineBreak;
    S:=TStringList.Create;
    try
      Members.GetMembers(S);
      Result:=Result+S.Text;
    finally
      S.Free;
    end;
    Result:=Result+');';
    if Full then ;
    end;
end;

procedure TPasVariant.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Values.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Values[i]),false);
  ForEachChildCall(aMethodCall,Arg,Members,false);
end;

{ TPasRecordType }

constructor TPasRecordType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Members := TFPList.Create;
  GenericTemplateTypes:=TFPList.Create;
end;

destructor TPasRecordType.Destroy;
var
  i: Integer;
begin
  for i := 0 to GenericTemplateTypes.Count - 1 do
    TPasElement(GenericTemplateTypes[i]).Release;
  FreeAndNil(GenericTemplateTypes);

  for i := 0 to Members.Count - 1 do
    TPasVariable(Members[i]).Release;
  FreeAndNil(Members);

  if Assigned(VariantEl) then
    ReleaseAndNil(TPasElement(VariantEl));

  if Assigned(Variants) then
  begin
    for i := 0 to Variants.Count - 1 do
      TPasVariant(Variants[i]).Release;
    FreeAndNil(Variants);
  end;

  inherited Destroy;
end;

{ TPasClassType }

constructor TPasClassType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  PackMode:=pmNone;                     // 12/04/04 - Dave - Added
  IsShortDefinition := False;
  Members := TFPList.Create;
  Modifiers := TStringList.Create;
  Interfaces:= TFPList.Create;
  GenericTemplateTypes:=TFPList.Create;
end;

destructor TPasClassType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Members.Count - 1 do
    TPasElement(Members[i]).Release;
  for i := 0 to Interfaces.Count - 1 do
    TPasElement(Interfaces[i]).Release;
  FreeAndNil(Members);
  if Assigned(AncestorType) then
    AncestorType.Release;
  if Assigned(HelperForType) then
    HelperForType.Release;
  ReleaseAndNil(TPasElement(GUIDExpr));
  FreeAndNil(Modifiers);
  FreeAndNil(Interfaces);
  for i := 0 to GenericTemplateTypes.Count - 1 do
    TPasElement(GenericTemplateTypes[i]).Release;
  FreeAndNil(GenericTemplateTypes);
  inherited Destroy;
end;

function TPasClassType.ElementTypeName: string;
begin
  case ObjKind of
    okObject: Result := SPasTreeObjectType;
    okClass: Result := SPasTreeClassType;
    okInterface: Result := SPasTreeInterfaceType;
    okGeneric : Result := SPasTreeGenericType;
    okSpecialize : Result := SPasTreeSpecializedType;
    okClassHelper : Result:=SPasClassHelperType;
    okRecordHelper : Result:=SPasRecordHelperType;
  else
    Result:='ObjKind('+IntToStr(ord(ObjKind))+')';
  end;
end;

procedure TPasClassType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);

  ForEachChildCall(aMethodCall,Arg,AncestorType,true);
  for i:=0 to Interfaces.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Interfaces[i]),true);
  ForEachChildCall(aMethodCall,Arg,HelperForType,true);
  ForEachChildCall(aMethodCall,Arg,GUIDExpr,false);
  for i:=0 to Members.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Members[i]),false);
  for i:=0 to GenericTemplateTypes.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(GenericTemplateTypes[i]),false);
end;

procedure TPasClassType.SetGenericTemplates(AList: TFPList);

Var
  I : Integer;

begin
  ObjKind:=okGeneric;
  For I:=0 to AList.Count-1 do
    begin
    TPasElement(AList[i]).Parent:=Self;
    GenericTemplateTypes.Add(AList[i]);
    end;
  ObjKind:=okGeneric;
end;

function TPasClassType.FindMember(MemberClass: TPTreeElement; const MemberName: String): TPasElement;

Var
  I : Integer;

begin
//  Writeln('Looking for ',MemberName,'(',MemberClass.ClassName,') in ',Name);
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<Members.Count) do
    begin
    Result:=TPasElement(Members[i]);
    if (Result.ClassType<>MemberClass) or (CompareText(Result.Name,MemberName)<>0) then
      Result:=Nil;
    Inc(I);
    end;
end;

function TPasClassType.FindMemberInAncestors(MemberClass: TPTreeElement;
  const MemberName: String): TPasElement;

  Function A (C : TPasClassType) : TPasClassType;

  begin
    if C.AncestorType is TPasClassType then
      result:=TPasClassType(C.AncestorType)
    else
      result:=Nil;
  end;

Var
  C : TPasClassType;

begin
  Result:=Nil;
  C:=A(Self);
  While (Result=Nil) and (C<>Nil) do
    begin
    Result:=C.FindMember(MemberClass,MemberName);
    C:=A(C);
    end;
end;

function TPasClassType.InterfaceGUID: string;
begin
  If Assigned(GUIDExpr) then
    Result:=GUIDExpr.GetDeclaration(True)
  else
    Result:=''
end;

function TPasClassType.IsSealed: Boolean;
begin
  Result:=HasModifier('sealed');
end;

function TPasClassType.IsAbstract: Boolean;
begin
  Result:=HasModifier('abstract');
end;

function TPasClassType.HasModifier(const aModifier: String): Boolean;
var
  i: Integer;
begin
  for i:=0 to Modifiers.Count-1 do
    if CompareText(aModifier,Modifiers[i])=0 then
      exit(true);
  Result:=false;
end;

function TPasClassType.IsPacked: Boolean;
begin
  Result:=PackMode<>pmNone;
end;


{ TPasArgument }

destructor TPasArgument.Destroy;
begin
  ReleaseAndNil(TPasElement(ArgType));
  ReleaseAndNil(TPasElement(ValueExpr));
  inherited Destroy;
end;

{ TPasProcedureType }

function TPasProcedureType.GetIsNested: Boolean;
begin
  Result:=ptmIsNested in Modifiers;
end;

function TPasProcedureType.GetIsOfObject: Boolean;
begin
  Result:=ptmOfObject in Modifiers;
end;

function TPasProcedureType.GetIsReference: Boolean;
begin
  Result:=ptmReferenceTo in Modifiers;
end;

procedure TPasProcedureType.SetIsNested(const AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmIsNested)
  else
    Exclude(Modifiers,ptmIsNested);
end;

procedure TPasProcedureType.SetIsOfObject(const AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmOfObject)
  else
    Exclude(Modifiers,ptmOfObject);
end;

procedure TPasProcedureType.SetIsReference(AValue: Boolean);
begin
  if AValue then
    Include(Modifiers,ptmReferenceTo)
  else
    Exclude(Modifiers,ptmReferenceTo);
end;

constructor TPasProcedureType.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TFPList.Create;
end;

destructor TPasProcedureType.Destroy;
var
  i: Integer;
begin
  for i := 0 to Args.Count - 1 do
    TPasArgument(Args[i]).Release;
  FreeAndNil(Args);
  inherited Destroy;
end;

class function TPasProcedureType.TypeName: string;
begin
  Result := 'procedure';
end;

function TPasProcedureType.CreateArgument(const AName,
  AUnresolvedTypeName: string): TPasArgument;
begin
  Result := TPasArgument.Create(AName, Self);
  Args.Add(Result);
  Result.ArgType := TPasUnresolvedTypeRef.Create(AUnresolvedTypeName, Result);
end;

procedure TPasProcedureType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Args.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Args[i]),false);
end;

{ TPasResultElement }

destructor TPasResultElement.Destroy;
begin
  if Assigned(ResultType) then
    ResultType.Release;
  inherited Destroy;
end;


destructor TPasFunctionType.Destroy;
begin
  if Assigned(ResultEl) then
    ResultEl.Release;
  inherited Destroy;
end;


class function TPasFunctionType.TypeName: string;
begin
  Result := 'function';
end;


constructor TPasUnresolvedTypeRef.Create(const AName: string; AParent: TPasElement);
begin
  if AParent=nil then ;
  inherited Create(AName, nil);
end;


destructor TPasVariable.Destroy;
begin
//  FreeAndNil(Expr);
  { Attention, in derived classes, VarType isn't necessarily set!
    (e.g. in Constants) }
  ReleaseAndNil(TPasElement(VarType));
  ReleaseAndNil(TPasElement(Expr));
  ReleaseAndNil(TPasElement(LibraryName));
  ReleaseAndNil(TPasElement(ExportName));
  inherited Destroy;
end;

function TPasProperty.GetIsClass: boolean;
begin
  Result:=vmClass in VarModifiers;
end;

procedure TPasProperty.SetIsClass(AValue: boolean);
begin
   if AValue then
    Include(VarModifiers,vmClass)
  else
    Exclude(VarModifiers,vmClass);
end;

constructor TPasProperty.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Args := TFPList.Create;
end;

destructor TPasProperty.Destroy;
var
  i: Integer;
begin
  for i := 0 to Args.Count - 1 do
    TPasArgument(Args[i]).Release;
  FreeAndNil(Args);
  ReleaseAndNil(TPasElement(IndexExpr));
  ReleaseAndNil(TPasElement(ReadAccessor));
  ReleaseAndNil(TPasElement(WriteAccessor));
  ReleaseAndNil(TPasElement(ImplementsFunc));
  ReleaseAndNil(TPasElement(StoredAccessor));
  ReleaseAndNil(TPasElement(DefaultExpr));
  ReleaseAndNil(TPasElement(DispIDExpr));
  inherited Destroy;
end;


constructor TPasOverloadedProc.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Overloads := TFPList.Create;
end;

destructor TPasOverloadedProc.Destroy;
var
  i: Integer;
begin
  for i := 0 to Overloads.Count - 1 do
    TPasProcedure(Overloads[i]).Release;
  FreeAndNil(Overloads);
  inherited Destroy;
end;

function TPasOverloadedProc.TypeName: string;
begin
  if Assigned(TPasProcedure(Overloads[0]).ProcType) then
    Result := TPasProcedure(Overloads[0]).ProcType.TypeName
  else
    SetLength(Result, 0);
end;

procedure TPasOverloadedProc.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Overloads.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasProcedure(Overloads[i]),false);
end;

function TPasProcedure.GetCallingConvention: TCallingConvention;
begin
  Result:=ccDefault;
  if Assigned(ProcType) then
    Result:=ProcType.CallingConvention;
end;

procedure TPasProcedure.SetCallingConvention(AValue: TCallingConvention);
begin
  if Assigned(ProcType) then
    ProcType.CallingConvention:=AValue;
end;

destructor TPasProcedure.Destroy;
begin
  if Assigned(ProcType) then
    ProcType.Release;
  if Assigned(Body) then
    Body.Release;
  ReleaseAndNil(TPasElement(PublicName));
  ReleaseAndNil(TPasElement(LibraryExpr));
  ReleaseAndNil(TPasElement(LibrarySymbolName));
  inherited Destroy;
end;

function TPasProcedure.TypeName: string;
begin
  Result := 'procedure';
end;

constructor TPasProcedureImpl.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Locals := TFPList.Create;
end;

destructor TPasProcedureImpl.Destroy;
var
  i: Integer;
begin
  if Assigned(Body) then
    Body.Release;

  for i := 0 to Locals.Count - 1 do
    TPasElement(Locals[i]).Release;
  FreeAndNil(Locals);

  if Assigned(ProcType) then
    ProcType.Release;

  inherited Destroy;
end;

function TPasProcedureImpl.TypeName: string;
begin
  Result := ProcType.TypeName;
end;


function TPasConstructorImpl.TypeName: string;
begin
  Result := 'constructor';
end;

function TPasDestructorImpl.TypeName: string;
begin
  Result := 'destructor';
end;


constructor TPasImplCommands.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Commands := TStringList.Create;
end;

destructor TPasImplCommands.Destroy;
begin
  FreeAndNil(Commands);
  inherited Destroy;
end;


destructor TPasImplIfElse.Destroy;
begin
  ReleaseAndNil(TPasElement(ConditionExpr));
  ReleaseAndNil(TPasElement(IfBranch));
  ReleaseAndNil(TPasElement(ElseBranch));
  inherited Destroy;
end;

procedure TPasImplIfElse.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if IfBranch=nil then
    begin
    IfBranch:=Element;
    Element.AddRef;
    end
  else if ElseBranch=nil then
    begin
    ElseBranch:=Element;
    Element.AddRef;
    end
  else
    raise Exception.Create('TPasImplIfElse.AddElement if and else already set - please report this bug');
end;

function TPasImplIfElse.CloseOnSemicolon: boolean;
begin
  Result:=ElseBranch<>nil;
end;

procedure TPasImplIfElse.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,ConditionExpr,false);
  if Elements.IndexOf(IfBranch)<0 then
    ForEachChildCall(aMethodCall,Arg,IfBranch,false);
  if Elements.IndexOf(ElseBranch)<0 then
    ForEachChildCall(aMethodCall,Arg,ElseBranch,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplIfElse.Condition: string;
begin
  If Assigned(ConditionExpr) then
    Result:=ConditionExpr.GetDeclaration(True);
end;

destructor TPasImplForLoop.Destroy;
begin
  ReleaseAndNil(TPasElement(VariableName));
  ReleaseAndNil(TPasElement(StartExpr));
  ReleaseAndNil(TPasElement(EndExpr));
  ReleaseAndNil(TPasElement(Variable));
  ReleaseAndNil(TPasElement(Body));
  inherited Destroy;
end;

procedure TPasImplForLoop.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    Body.AddRef;
    end
  else
    raise Exception.Create('TPasImplForLoop.AddElement body already set - please report this bug');
end;

procedure TPasImplForLoop.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,VariableName,false);
  ForEachChildCall(aMethodCall,Arg,Variable,false);
  ForEachChildCall(aMethodCall,Arg,StartExpr,false);
  ForEachChildCall(aMethodCall,Arg,EndExpr,false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplForLoop.Down: boolean;
begin
  Result:=(LoopType=ltDown);
end;

function TPasImplForLoop.StartValue: String;
begin
  If Assigned(StartExpr) then
    Result:=StartExpr.GetDeclaration(true)
  else
    Result:='';
end;

function TPasImplForLoop.EndValue: string;
begin
  If Assigned(EndExpr) then
    Result:=EndExpr.GetDeclaration(true)
  else
    Result:='';
end;

constructor TPasImplBlock.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Elements := TFPList.Create;
end;

destructor TPasImplBlock.Destroy;
var
  i: Integer;
begin
  for i := 0 to Elements.Count - 1 do
    TPasImplElement(Elements[i]).Release;
  FreeAndNil(Elements);
  inherited Destroy;
end;

procedure TPasImplBlock.AddElement(Element: TPasImplElement);
begin
  Elements.Add(Element);
end;

function TPasImplBlock.AddCommand(const ACommand: string): TPasImplCommand;
begin
  Result := TPasImplCommand.Create('', Self);
  Result.Command := ACommand;
  AddElement(Result);
end;

function TPasImplBlock.AddCommands: TPasImplCommands;
begin
  Result := TPasImplCommands.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddBeginBlock: TPasImplBeginBlock;
begin
  Result := TPasImplBeginBlock.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddRepeatUntil: TPasImplRepeatUntil;
begin
  Result := TPasImplRepeatUntil.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddIfElse(const ACondition: TPasExpr): TPasImplIfElse;
begin
  Result := TPasImplIfElse.Create('', Self);
  Result.ConditionExpr := ACondition;
  AddElement(Result);
end;

function TPasImplBlock.AddWhileDo(const ACondition: TPasExpr): TPasImplWhileDo;
begin
  Result := TPasImplWhileDo.Create('', Self);
  Result.ConditionExpr := ACondition;
  AddElement(Result);
end;

function TPasImplBlock.AddWithDo(const Expression: TPasExpr): TPasImplWithDo;
begin
  Result := TPasImplWithDo.Create('', Self);
  Result.AddExpression(Expression);
  AddElement(Result);
end;

function TPasImplBlock.AddCaseOf(const Expression: TPasExpr): TPasImplCaseOf;
begin
  Result := TPasImplCaseOf.Create('', Self);
  Result.CaseExpr:= Expression;
  AddElement(Result);
end;

function TPasImplBlock.AddForLoop(AVar: TPasVariable; const AStartValue,
  AEndValue: TPasExpr): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Result.Variable := AVar;
  Result.StartExpr := AStartValue;
  Result.EndExpr:= AEndValue;
  AddElement(Result);
end;

function TPasImplBlock.AddForLoop(AVarName: TPasExpr; AStartValue,
  AEndValue: TPasExpr; ADownTo: Boolean): TPasImplForLoop;
begin
  Result := TPasImplForLoop.Create('', Self);
  Result.VariableName := AVarName;
  Result.StartExpr := AStartValue;
  Result.EndExpr := AEndValue;
  if ADownto then
    Result.Looptype := ltDown;
  AddElement(Result);
end;

function TPasImplBlock.AddTry: TPasImplTry;
begin
  Result := TPasImplTry.Create('', Self);
  AddElement(Result);
end;

function TPasImplBlock.AddExceptOn(const VarName, TypeName: string
  ): TPasImplExceptOn;
begin
  Result:=AddExceptOn(VarName,TPasUnresolvedTypeRef.Create(TypeName,nil));
end;

function TPasImplBlock.AddExceptOn(const VarName: string; VarType: TPasType
  ): TPasImplExceptOn;
var
  V: TPasVariable;
begin
  V:=TPasVariable.Create(VarName,nil);
  V.VarType:=VarType;
  Result:=AddExceptOn(V);
end;

function TPasImplBlock.AddExceptOn(const VarEl: TPasVariable): TPasImplExceptOn;
begin
  Result:=TPasImplExceptOn.Create('',Self);
  Result.VarEl:=VarEl;
  Result.TypeEl:=VarEl.VarType;
  Result.TypeEl.AddRef;
  AddElement(Result);
end;

function TPasImplBlock.AddExceptOn(const TypeEl: TPasType): TPasImplExceptOn;
begin
  Result:=TPasImplExceptOn.Create('',Self);
  Result.TypeEl:=TypeEl;
  AddElement(Result);
end;

function TPasImplBlock.AddRaise: TPasImplRaise;
begin
  Result:=TPasImplRaise.Create('',Self);
  AddElement(Result);
end;

function TPasImplBlock.AddLabelMark(const Id: string): TPasImplLabelMark;
begin
  Result:=TPasImplLabelMark.Create('', Self);
  Result.LabelId:=Id;
  AddElement(Result);
end;

function TPasImplBlock.AddAssign(left,right:TPasExpr):TPasImplAssign;
begin
  Result:=TPasImplAssign.Create('', Self);
  Result.left:=left;
  Result.right:=right;
  AddElement(Result);
end;

function TPasImplBlock.AddSimple(exp:TPasExpr):TPasImplSimple;
begin
  Result:=TPasImplSimple.Create('', Self);
  Result.expr:=exp;
  AddElement(Result);
end;

function TPasImplBlock.CloseOnSemicolon: boolean;
begin
  Result:=false;
end;

procedure TPasImplBlock.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Elements.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Elements[i]),false);
end;



{ ---------------------------------------------------------------------

  ---------------------------------------------------------------------}

function TPasModule.GetDeclaration(full : boolean): string;
begin
  Result := 'Unit ' + Name;
  if full then ;
end;

procedure TPasModule.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,InterfaceSection,false);
  ForEachChildCall(aMethodCall,Arg,ImplementationSection,false);
  ForEachChildCall(aMethodCall,Arg,InitializationSection,false);
  ForEachChildCall(aMethodCall,Arg,FinalizationSection,false);
end;

{
function TPas.GetDeclaration : string;
begin
  Result:=Name;
end;
}

function TPasResString.GetDeclaration(full: Boolean): string;
begin
  Result:=Expr.GetDeclaration(true);
  If Full Then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasResString.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
end;

destructor TPasResString.Destroy;
begin
  If Assigned(Expr) then
    Expr.Release;
  inherited Destroy;
end;

function TPasPointerType.GetDeclaration(full: Boolean): string;
begin
  Result:='^'+DestType.Name;
  If Full then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasPointerType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,DestType,true);
end;

function TPasAliasType.GetDeclaration(full: Boolean): string;
begin
  Result:=DestType.Name;
  If Full then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasAliasType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,DestType,true);
end;

function TPasClassOfType.GetDeclaration (full : boolean) : string;
begin
  Result:='Class of '+DestType.Name;
  If Full then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

function TPasRangeType.GetDeclaration (full : boolean) : string;
begin
  Result:=RangeStart+'..'+RangeEnd;
  If Full then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasRangeType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,RangeExpr,false);
end;

destructor TPasRangeType.Destroy;
begin
  ReleaseAndNil(TPasElement(RangeExpr));
  inherited Destroy;
end;

function TPasRangeType.RangeStart: String;
begin
  Result:=RangeExpr.Left.GetDeclaration(False);
end;

function TPasRangeType.RangeEnd: String;
begin
  Result:=RangeExpr.Right.GetDeclaration(False);
end;

function TPasArrayType.GetDeclaration (full : boolean) : string;
begin
  Result:='Array';
  If (IndexRange<>'') then
    Result:=Result+'['+IndexRange+']';
  Result:=Result+' of ';
  If IsPacked then
     Result := 'packed '+Result;      // 12/04/04 Dave - Added
  If Assigned(Eltype) then
    Result:=Result+ElType.Name
  else
    Result:=Result+'const';
  If Full Then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasArrayType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ElType,true);
end;

function TPasArrayType.IsGenericArray: Boolean;
begin
  Result:=elType is TPasGenericTemplateType;
end;

function TPasArrayType.IsPacked: Boolean;
begin
  Result:=PackMode=pmPacked;
end;

procedure TPasArrayType.AddRange(Range: TPasExpr);
var
  i: Integer;
begin
  i:=Length(Ranges);
  SetLength(Ranges, i+1);
  Ranges[i]:=Range;
end;

function TPasFileType.GetDeclaration (full : boolean) : string;
begin
  Result:='File';
  If Assigned(Eltype) then
    Result:=Result+' of '+ElType.Name;
  If Full Then
    begin
    Result:=Name+' = '+Result;
    ProcessHints(False,Result);
    end;
end;

procedure TPasFileType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ElType,true);
end;

Function IndentStrings(S : TStrings; indent : Integer) : string;

Var
  I,CurrLen,CurrPos : Integer;


begin
  Result:='';
  CurrLen:=0;
  CurrPos:=0;
  For I:=0 to S.Count-1 do
    begin
    CurrLen:=Length(S[i]);
    If (CurrLen+CurrPos)>72 then
      begin
      Result:=Result+LineEnding+StringOfChar(' ',Indent);
      CurrPos:=Indent;
      end;
    Result:=Result+S[i];
    CurrPos:=CurrPos+CurrLen;
    end;
end;

function TPasEnumType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Name+' = (')
    else
      S.Add('(');
    GetEnumNames(S);
    S[S.Count-1]:=S[S.Count-1]+')';
    If Full then
      Result:=IndentStrings(S,Length(Name)+4)
    else
      Result:=IndentStrings(S,1);
    if Full then
      ProcessHints(False,Result);
  finally
    S.Free;
  end;
end;

destructor TPasSetType.Destroy;
begin
  ReleaseAndNil(TPasElement(EnumType));
  inherited Destroy;
end;

function TPasSetType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  i : Integer;

begin
  If (EnumType is TPasEnumType) and (EnumType.Name='') then
    begin
    S:=TStringList.Create;
    Try
      If Full then
        S.Add(Name+'= Set of (')
      else
        S.Add('Set of (');
      TPasEnumType(EnumType).GetEnumNames(S);
      S[S.Count-1]:=S[S.Count-1]+')';
      I:=Pos('(',S[0]);
      Result:=IndentStrings(S,i);
    finally
      S.Free;
    end;
    end
  else
    begin
    Result:='Set of '+EnumType.Name;
    If Full then
      Result:=Name+' = '+Result;
    end;
  If Full then
    ProcessHints(False,Result);
end;

procedure TPasSetType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,EnumType,true);
end;

procedure TPasRecordType.GetMembers(S: TStrings);

Var
  T : TStringList;
  temp : string;
  I,J : integer;
  E : TPasElement;
  CV : TPasMemberVisibility ;

begin
  T:=TStringList.Create;
  try

  CV:=visDefault;
  For I:=0 to Members.Count-1 do
    begin
    E:=TPasElement(Members[i]);
    if E.Visibility<>CV then
      begin
      CV:=E.Visibility;
      if CV<>visDefault then
        S.Add(VisibilityNames[CV]);
      end;
    Temp:=E.GetDeclaration(True);
    If E is TPasProperty then
      Temp:='property '+Temp;
    If Pos(LineEnding,Temp)>0 then
      begin
      T.Text:=Temp;
      For J:=0 to T.Count-1 do
        if J=T.Count-1 then
          S.Add('  '+T[J]+';')
        else
          S.Add('  '+T[J])
      end
    else
      S.Add('  '+Temp+';');
    end;
  if Variants<>nil then
    begin
    temp:='case ';
    if (VariantEl is TPasVariable) then
      temp:=Temp+VariantEl.Name+' : '+TPasVariable(VariantEl).VarType.Name
    else if (VariantEl<>Nil) then
      temp:=temp+VariantEl.Name;
    S.Add(temp+' of');
    T.Clear;
    For I:=0 to Variants.Count-1 do
      T.Add(TPasVariant(Variants[i]).GetDeclaration(True));
    S.AddStrings(T);
    end;
  finally
    T.Free;
  end;
end;

function TPasRecordType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  temp : string;

begin
  S:=TStringList.Create;
  Try
    Temp:='record';
    If IsPacked then
      if IsBitPacked then
        Temp:='bitpacked '+Temp
      else
        Temp:='packed '+Temp;
    If Full then
      Temp:=Name+' = '+Temp;
    S.Add(Temp);
    GetMembers(S);
    S.Add('end');
    Result:=S.Text;
    ProcessHints(False, Result);
  finally
    S.free;
  end;
end;

procedure TPasRecordType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to Members.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Members[i]),false);
  ForEachChildCall(aMethodCall,Arg,VariantEl,false);
  if Variants<>nil then
    for i:=0 to Variants.Count-1 do
      ForEachChildCall(aMethodCall,Arg,TPasElement(Variants[i]),false);
  for i:=0 to GenericTemplateTypes.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(GenericTemplateTypes[i]),false);
end;

function TPasRecordType.IsPacked: Boolean;
begin
  Result:=(PackMode <> pmNone);
end;

function TPasRecordType.IsBitPacked: Boolean;
begin
  Result:=(PackMode=pmBitPacked)
end;

function TPasRecordType.IsAdvancedRecord: Boolean;

Var
  I : Integer;

begin
  Result:=False;
  I:=0;
  While (Not Result) and (I<Members.Count) do
    begin
    Result:=TPasElement(Members[i]).InheritsFrom(TPasProcedureBase) or
            TPasElement(Members[i]).InheritsFrom(TPasProperty);
    Inc(I);
    end;
end;

procedure TPasRecordType.SetGenericTemplates(AList: TFPList);
var
  I: Integer;
begin
  For I:=0 to AList.Count-1 do
    begin
    TPasElement(AList[i]).Parent:=Self;
    GenericTemplateTypes.Add(AList[i]);
    end;
end;

procedure TPasProcedureType.GetArguments(List : TStrings);

Var
  T : string;
  I : Integer;

begin
  For I:=0 to Args.Count-1 do
    begin
    T:=AccessNames[TPasArgument(Args[i]).Access];
    T:=T+TPasArgument(Args[i]).GetDeclaration(True);
    If I=0 then
      T:='('+T;
    If I<Args.Count-1 then
      List.Add(T+'; ')
    else
      List.Add(T+')');
    end;
end;

function TPasProcedureType.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Format('%s = ',[Name]));
    S.Add(TypeName);
    GetArguments(S);
    If IsOfObject then
      S.Add(' of object')
    else if IsNested then
      S.Add(' is nested');
    If Full then
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);
  finally
    S.Free;
  end;
end;

function TPasFunctionType.GetDeclaration(Full: boolean): string;

Var
  S : TStringList;
  T : string;

begin
  S:=TStringList.Create;
  Try
    If Full then
      S.Add(Format('%s = ',[Name]));
    S.Add(TypeName);
    GetArguments(S);
    If Assigned(ResultEl) then
      begin
      T:=' : ';
      If (ResultEl.ResultType.Name<>'') then
        T:=T+ResultEl.ResultType.Name
      else
        T:=T+ResultEl.ResultType.GetDeclaration(False);
      S.Add(T);
      end;
    If IsOfObject then
      S.Add(' of object');
    If Full then
      Result:=IndentStrings(S,Length(S[0])+Length(S[1])+1)
    else
      Result:=IndentStrings(S,Length(S[0])+1);
  finally
    S.Free;
  end;
end;

procedure TPasFunctionType.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ResultEl,false);
end;

function TPasVariable.GetDeclaration (full : boolean) : string;

Const
 Seps : Array[Boolean] of Char = ('=',':');

begin
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.Name;
    Result:=Result+Modifiers;
    if (Value<>'') then
      Result:=Result+' = '+Value;
    end
  else
    Result:=Value;
  If Full then
    begin
    Result:=Name+' '+Seps[Assigned(VarType)]+' '+Result;
    Result:=Result+HintsString;
    end;
end;

procedure TPasVariable.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,VarType,true);
  ForEachChildCall(aMethodCall,Arg,Expr,false);
end;


function TPasVariable.Value: String;
begin
  If Assigned(Expr) then
    Result:=Expr.GetDeclaration(True)
end;

function TPasProperty.GetDeclaration (full : boolean) : string;

Var
  S : string;
  I : Integer;

begin
  If Assigned(VarType) then
    begin
    If VarType.Name='' then
      Result:=VarType.GetDeclaration(False)
    else
      Result:=VarType.Name;
    end
  else if Assigned(Expr) then
    Result:=Expr.GetDeclaration(True);
  S:='';
  If Assigned(Args) and (Args.Count>0) then
    begin
    For I:=0 to Args.Count-1 do
      begin
      If (S<>'') then
        S:=S+';';
      S:=S+TPasElement(Args[i]).GetDeclaration(true);
      end;
    end;
  If S<>'' then
    S:='['+S+']'
  else
    S:=' ';
  If Full then
    begin
    Result:=Name+S+': '+Result;
    If (ImplementsName<>'') then
       Result:=Result+' implements '+ImplementsName;
    end;   
  If IsDefault then
    Result:=Result+'; default';
  ProcessHints(True, Result);
end;

procedure TPasProperty.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,IndexExpr,false);
  for i:=0 to Args.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Args[i]),false);
  ForEachChildCall(aMethodCall,Arg,ReadAccessor,false);
  ForEachChildCall(aMethodCall,Arg,WriteAccessor,false);
  ForEachChildCall(aMethodCall,Arg,ImplementsFunc,false);
  ForEachChildCall(aMethodCall,Arg,StoredAccessor,false);
  ForEachChildCall(aMethodCall,Arg,DefaultExpr,false);
end;

function TPasProperty.ResolvedType: TPasType;

  Function GC(P : TPasProperty) : TPasClassType;

  begin
    if Assigned(P) and Assigned(P.Parent) and (P.Parent is TPasClassType) then
      Result:=P.Parent as TPasClassType
    else
      Result:=Nil;
  end;


Var
  P : TPasProperty;
  C : TPasClassType;

begin
  Result:=FResolvedType;
  if Result=Nil then
    Result:=VarType;
  P:=Self;
  While (Result=Nil) and (P<>Nil) do
    begin
    C:=GC(P);
//    Writeln('Looking for ',Name,' in ancestor ',C.Name);
    P:=TPasProperty(C.FindMemberInAncestors(TPasProperty,Name));
    if Assigned(P) then
      begin
//      Writeln('Found ',Name,' in ancestor : ',P.Name);
      Result:=P.ResolvedType;
      end
    end;
end;

function TPasProperty.IndexValue: String;
begin
  If Assigned(IndexExpr) then
    Result:=IndexExpr.GetDeclaration(true)
  else
    Result:='';
end;

function TPasProperty.DefaultValue: string;
begin
  If Assigned(DefaultExpr) then
    Result:=DefaultExpr.GetDeclaration(true)
  else
    Result:='';
end;

procedure TPasProcedure.GetModifiers(List: TStrings);

  Procedure DoAdd(B : Boolean; S : string);

  begin
    if B then
      List.add('; '+S);
  end;

begin
  Doadd(IsVirtual,' Virtual');
  DoAdd(IsDynamic,' Dynamic');
  DoAdd(IsOverride,' Override');
  DoAdd(IsAbstract,' Abstract');
  DoAdd(IsOverload,' Overload');
  DoAdd(IsReintroduced,' Reintroduce');
  DoAdd(IsStatic,' Static');
  DoAdd(IsMessage,' Message');
end;

procedure TPasProcedure.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,PublicName,false);
  ForEachChildCall(aMethodCall,Arg,ProcType,false);
  ForEachChildCall(aMethodCall,Arg,LibraryExpr,false);
  ForEachChildCall(aMethodCall,Arg,LibrarySymbolName,false);
  ForEachChildCall(aMethodCall,Arg,Body,false);
end;

procedure TPasProcedure.AddModifier(AModifier: TProcedureModifier);

begin
  Include(FModifiers,AModifier);
end;

function TPasProcedure.IsVirtual: Boolean;
begin
  Result:=pmVirtual in FModifiers;
end;

function TPasProcedure.IsDynamic: Boolean;
begin
  Result:=pmDynamic in FModifiers;
end;

function TPasProcedure.IsAbstract: Boolean;
begin
  Result:=pmAbstract in FModifiers;
end;

function TPasProcedure.IsOverride: Boolean;
begin
  Result:=pmOverride in FModifiers;
end;

function TPasProcedure.IsExported: Boolean;
begin
  Result:=pmExport in FModifiers;
end;

function TPasProcedure.IsExternal: Boolean;
begin
  Result:=pmExternal in FModifiers;
end;

function TPasProcedure.IsOverload: Boolean;
begin
  Result:=pmOverload in FModifiers;
end;

function TPasProcedure.IsMessage: Boolean;
begin
  Result:=pmMessage in FModifiers;
end;

function TPasProcedure.IsReintroduced: Boolean;
begin
  Result:=pmReintroduce in FModifiers;
end;

function TPasProcedure.IsStatic: Boolean;

begin
  Result:=ptmStatic in ProcType.Modifiers;
end;

function TPasProcedure.IsForward: Boolean;
begin
  Result:=pmForward in FModifiers;
end;

function TPasProcedure.GetDeclaration(full: Boolean): string;

Var
  S : TStringList;
begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasFunction.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : string;

begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(TypeName+' '+Name);
    ProcType.GetArguments(S);
    If Assigned((Proctype as TPasFunctionType).ResultEl) then
      With TPasFunctionType(ProcType).ResultEl.ResultType do
        begin
        T:=' : ';
        If (Name<>'') then
          T:=T+Name
        else
          T:=T+GetDeclaration(False);
        S.Add(T);
        end;
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));
  finally
    S.Free;
  end;
end;

function TPasFunction.TypeName: string;
begin
  Result:='function';
end;

function TPasOperator.GetOperatorDeclaration(Full : Boolean) : string;

begin
  if Full then
    begin
    Result:=FullPath;
    if (Result<>'') then
      Result:=Result+'.';
    end
  else
    Result:='';
  if TokenBased then
    Result:=Result+TypeName+' '+OperatorTypeToToken(OperatorType)
  else
    Result:=Result+TypeName+' '+OperatorTypeToOperatorName(OperatorType);
end;

function TPasOperator.GetDeclaration (full : boolean) : string;

Var
  S : TStringList;
  T : string;

begin
  S:=TStringList.Create;
  try
    If Full then
      S.Add(GetOperatorDeclaration(Full));
    ProcType.GetArguments(S);
    If Assigned((Proctype as TPasFunctionType).ResultEl) then
      With TPasFunctionType(ProcType).ResultEl.ResultType do
        begin
        T:=' : ';
        If (Name<>'') then
          T:=T+Name
        else
          T:=T+GetDeclaration(False);
        S.Add(T);
        end;
    GetModifiers(S);
    Result:=IndentStrings(S,Length(S[0]));

  finally
    S.Free;
  end;
end;

function TPasOperator.TypeName: string;
begin
  Result:='operator';
end;

function TPasClassProcedure.TypeName: string;
begin
  Result:='class procedure';
end;

function TPasClassFunction.TypeName: string;
begin
  Result:='class function';
end;

function TPasConstructor.TypeName: string;
begin
  Result:='constructor';
end;

function TPasDestructor.TypeName: string;
begin
  Result:='destructor';
end;

function TPasArgument.GetDeclaration (full : boolean) : string;
begin
  If Assigned(ArgType) then
    begin
    If ArgType.Name<>'' then
      Result:=ArgType.Name
    else
      Result:=ArgType.GetDeclaration(False);
    If Full then
      Result:=Name+': '+Result;
    end
  else If Full then
    Result:=Name
  else
    Result:='';
end;

procedure TPasArgument.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,ArgType,true);
  ForEachChildCall(aMethodCall,Arg,ValueExpr,false);
end;

function TPasArgument.Value: String;
begin
  If Assigned(ValueExpr) then
    Result:=ValueExpr.GetDeclaration(true)
  else
    Result:='';
end;

{ TPassTreeVisitor }

procedure TPassTreeVisitor.Visit(obj: TPasElement);
begin
  // Needs to be implemented by descendents.
  if Obj=nil then ;
end;

{ TPasSection }

constructor TPasSection.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  UsesList := TFPList.Create;
end;

destructor TPasSection.Destroy;
var
  i: Integer;
begin
  {$IFDEF VerbosePasTreeMem}writeln('TPasSection.Destroy UsesList');{$ENDIF}
  for i := 0 to UsesList.Count - 1 do
    TPasType(UsesList[i]).Release;
  FreeAndNil(UsesList);
  {$IFDEF VerbosePasTreeMem}writeln('TPasSection.Destroy UsesClause');{$ENDIF}
  for i := 0 to length(UsesClause) - 1 do
    UsesClause[i].Release;
  SetLength(UsesClause,0);

  {$IFDEF VerbosePasTreeMem}writeln('TPasSection.Destroy inherited');{$ENDIF}
  inherited Destroy;
  {$IFDEF VerbosePasTreeMem}writeln('TPasSection.Destroy END');{$ENDIF}
end;

function TPasSection.AddUnitToUsesList(const AUnitName: string;
  aName: TPasExpr; InFilename: TPrimitiveExpr; aModule: TPasElement;
  UsesUnit: TPasUsesUnit): TPasUsesUnit;
var
  l: Integer;
begin
  if (InFilename<>nil) and (InFilename.Kind<>pekString) then
    raise Exception.Create('');
  if aModule=nil then
    aModule:=TPasUnresolvedUnitRef.Create(AUnitName, Self);
  l:=length(UsesClause);
  SetLength(UsesClause,l+1);
  if UsesUnit=nil then
    begin
    UsesUnit:=TPasUsesUnit.Create(AUnitName,Self);
    if aName<>nil then
      begin
      Result.SourceFilename:=aName.SourceFilename;
      Result.SourceLinenumber:=aName.SourceLinenumber;
      end;
    end;
  UsesClause[l]:=UsesUnit;
  UsesUnit.Expr:=aName;
  UsesUnit.InFilename:=InFilename;
  UsesUnit.Module:=aModule;
  Result:=UsesUnit;

  UsesList.Add(aModule);
  aModule.AddRef;
end;

function TPasSection.ElementTypeName: string;
begin
  Result := SPasTreeSection;
end;

procedure TPasSection.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(UsesClause)-1 do
    ForEachChildCall(aMethodCall,Arg,UsesClause[i],false);
end;

{ TProcedureBody }

constructor TProcedureBody.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
end;

destructor TProcedureBody.Destroy;
begin
  if Assigned(Body) then
    Body.Release;
  inherited Destroy;
end;

procedure TProcedureBody.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Body,false);
end;

{ TPasImplWhileDo }

destructor TPasImplWhileDo.Destroy;
begin
  ReleaseAndNil(TPasElement(ConditionExpr));
  ReleaseAndNil(TPasElement(Body));
  inherited Destroy;
end;

procedure TPasImplWhileDo.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    Body.AddRef;
    end
  else
    raise Exception.Create('TPasImplWhileDo.AddElement body already set');
end;

procedure TPasImplWhileDo.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,ConditionExpr,false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplWhileDo.Condition: string;
begin
  If Assigned(ConditionExpr) then
    Result:=ConditionExpr.GetDeclaration(True);
end;

{ TPasImplCaseOf }

destructor TPasImplCaseOf.Destroy;
begin
  ReleaseAndNil(TPasElement(CaseExpr));
  ReleaseAndNil(TPasElement(ElseBranch));
  inherited Destroy;
end;

procedure TPasImplCaseOf.AddElement(Element: TPasImplElement);
begin
  if (ElseBranch<>Nil) and (Element=ElseBranch) then
    ElseBranch.AddRef;
  inherited AddElement(Element);
end;

function TPasImplCaseOf.AddCase(const Expression: TPasExpr
  ): TPasImplCaseStatement;
begin
  Result:=TPasImplCaseStatement.Create('',Self);
  Result.AddExpression(Expression);
  AddElement(Result);
end;

function TPasImplCaseOf.AddElse: TPasImplCaseElse;
begin
  Result:=TPasImplCaseElse.Create('',Self);
  ElseBranch:=Result;
  AddElement(Result);
end;

procedure TPasImplCaseOf.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,CaseExpr,false);
  if Elements.IndexOf(ElseBranch)<0 then
    ForEachChildCall(aMethodCall,Arg,ElseBranch,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplCaseOf.Expression: string;
begin
  if Assigned(CaseExpr) then
    Result:=CaseExpr.GetDeclaration(True)
  else
    Result:='';
end;

{ TPasImplCaseStatement }

constructor TPasImplCaseStatement.Create(const AName: string;
  AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Expressions:=TFPList.Create;
end;

destructor TPasImplCaseStatement.Destroy;

Var
  I : integer;

begin
  For I:=0 to Expressions.Count-1 do
    TPasExpr(Expressions[i]).Release;
  FreeAndNil(Expressions);
  ReleaseAndNil(TPasElement(Body));
  inherited Destroy;
end;

procedure TPasImplCaseStatement.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    Body.AddRef;
    end
  else
    raise Exception.Create('TPasImplCaseStatement.AddElement body already set');
end;

procedure TPasImplCaseStatement.AddExpression(const Expr: TPasExpr);
begin
  Expressions.Add(Expr);
  Expr.Parent:=Self;
end;

procedure TPasImplCaseStatement.ForEachCall(
  const aMethodCall: TOnForEachPasElement; const Arg: Pointer);
var
  i: Integer;
begin
  for i:=0 to Expressions.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Expressions[i]),false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasImplWithDo }

constructor TPasImplWithDo.Create(const AName: string; AParent: TPasElement);
begin
  inherited Create(AName, AParent);
  Expressions:=TFPList.Create;
end;

destructor TPasImplWithDo.Destroy;
Var
  I : Integer;
begin
  if Assigned(Body) then
    Body.Release;
  For I:=0 to Expressions.Count-1 do
    TPasExpr(Expressions[i]).Release;
  FreeAndNil(Expressions);
  inherited Destroy;
end;

procedure TPasImplWithDo.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    Body.AddRef;
    end
  else
    raise Exception.Create('TPasImplWithDo.AddElement body already set');
end;

procedure TPasImplWithDo.AddExpression(const Expression: TPasExpr);
begin
  Expressions.Add(Expression);
end;

procedure TPasImplWithDo.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  for i:=0 to Expressions.Count-1 do
    ForEachChildCall(aMethodCall,Arg,TPasElement(Expressions[i]),false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

{ TPasImplTry }

destructor TPasImplTry.Destroy;
begin
  if Assigned(FinallyExcept) then
    FinallyExcept.Release;
  if Assigned(ElseBranch) then
    ElseBranch.Release;
  inherited Destroy;
end;

function TPasImplTry.AddFinally: TPasImplTryFinally;
begin
  Result:=TPasImplTryFinally.Create('',Self);
  FinallyExcept:=Result;
end;

function TPasImplTry.AddExcept: TPasImplTryExcept;
begin
  Result:=TPasImplTryExcept.Create('',Self);
  FinallyExcept:=Result;
end;

function TPasImplTry.AddExceptElse: TPasImplTryExceptElse;
begin
  Result:=TPasImplTryExceptElse.Create('',Self);
  ElseBranch:=Result;
end;

procedure TPasImplTry.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,FinallyExcept,false);
  ForEachChildCall(aMethodCall,Arg,ElseBranch,false);
end;

{ TPasImplExceptOn }

destructor TPasImplExceptOn.Destroy;
begin
  ReleaseAndNil(TPasElement(VarEl));
  ReleaseAndNil(TPasElement(TypeEl));
  ReleaseAndNil(TPasElement(Body));
  inherited Destroy;
end;

procedure TPasImplExceptOn.AddElement(Element: TPasImplElement);
begin
  inherited AddElement(Element);
  if Body=nil then
    begin
    Body:=Element;
    Body.AddRef;
    end;
end;

procedure TPasImplExceptOn.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  ForEachChildCall(aMethodCall,Arg,VarEl,false);
  ForEachChildCall(aMethodCall,Arg,TypeEl,false);
  if Elements.IndexOf(Body)<0 then
    ForEachChildCall(aMethodCall,Arg,Body,false);
  inherited ForEachCall(aMethodCall, Arg);
end;

function TPasImplExceptOn.VariableName: String;
begin
  If assigned(VarEl) then
    Result:=VarEl.Name
  else
    Result:='';
end;

function TPasImplExceptOn.TypeName: string;
begin
  If assigned(TypeEl) then
    Result:=TypeEl.GetDeclaration(True)
  else
    Result:='';
end;

{ TPasImplStatement }

function TPasImplStatement.CloseOnSemicolon: boolean;
begin
  Result:=true;
end;

{ TPasExpr }

constructor TPasExpr.Create(AParent: TPasElement; AKind: TPasExprKind;
  AOpCode: TExprOpCode);
begin
  inherited Create(ClassName, AParent);
  Kind:=AKind;
  OpCode:=AOpCode;
end;

destructor TPasExpr.Destroy;
begin
  ReleaseAndNil(TPasElement(Format1));
  ReleaseAndNil(TPasElement(Format2));
  inherited Destroy;
end;

{ TPrimitiveExpr }

function TPrimitiveExpr.GetDeclaration(full: Boolean): string;
begin
  Result:=Value;
  if full then ;
end;

constructor TPrimitiveExpr.Create(AParent : TPasElement; AKind: TPasExprKind; const AValue : Ansistring);
begin
  inherited Create(AParent,AKind, eopNone);
  Value:=AValue;
end;

{ TBoolConstExpr }

constructor TBoolConstExpr.Create(AParent : TPasElement; AKind: TPasExprKind; const ABoolValue : Boolean);
begin
  inherited Create(AParent,AKind, eopNone);
  Value:=ABoolValue;
end;

function TBoolConstExpr.GetDeclaration(full: Boolean): string;

begin
  If Value then
    Result:='True'
  else
    Result:='False';
  if full then ;
end;



{ TUnaryExpr }

function TUnaryExpr.GetDeclaration(full: Boolean): string;

begin
  Result:=OpCodeStrings[Opcode];
  If Assigned(Operand) then
    Result:=Result+Operand.GetDeclaration(Full);
end;

constructor TUnaryExpr.Create(AParent : TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode);
begin
  inherited Create(AParent,pekUnary, AOpCode);
  Operand:=AOperand;
end;

destructor TUnaryExpr.Destroy;
begin
  if Assigned(Operand) then
    Operand.Release;
end;

procedure TUnaryExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Operand,false);
end;

{ TBinaryExpr }

function TBinaryExpr.GetDeclaration(full: Boolean): string;
  function OpLevel(op: TPasExpr): Integer;
  begin
    case op.OpCode of
      eopNot,eopAddress:
        Result := 4;
      eopMultiply, eopDivide, eopDiv, eopMod, eopAnd, eopShl,
      eopShr, eopAs, eopPower:
        Result := 3;
      eopAdd, eopSubtract, eopOr, eopXor:
        Result := 2;
      eopEqual, eopNotEqual, eopLessThan, eopLessthanEqual, eopGreaterThan,
      eopGreaterThanEqual, eopIn, eopIs:
        Result := 1;
    else
      Result := 5; // Numbers and Identifiers
    end;
  end;
var op: string;
begin
  If Kind=pekRange then
    Result:='..'
  else
    begin
    Result:=OpcodeStrings[Opcode];
    if Not (OpCode in [eopAddress,eopDeref,eopSubIdent]) then
      Result:=' '+Result+' ';
    end;
  If Assigned(Left) then
  begin
    op := Left.GetDeclaration(Full);
    if OpLevel(Left) < OpLevel(Self) then
      Result := '(' + op + ')' + Result
    else
      Result := op + Result;
  end;
  If Assigned(Right) then
  begin
    op := Right.GetDeclaration(Full);
    if (OpLevel(Right) < 5) and (OpLevel(Right) >= OpLevel(Self)) then
      Result := Result + '(' + op + ')'
    else
      Result := Result + op;
  end;
end;


constructor TBinaryExpr.Create(AParent : TPasElement; xleft,xright:TPasExpr; AOpCode:TExprOpCode);
begin
  inherited Create(AParent,pekBinary, AOpCode);
  left:=xleft;
  left.Parent:=Self;
  right:=xright;
  right.Parent:=Self;
end;

constructor TBinaryExpr.CreateRange(AParent : TPasElement; xleft,xright:TPasExpr);
begin
  inherited Create(AParent,pekRange, eopNone);
  left:=xleft;
  left.Parent:=Self;
  right:=xright;
  right.Parent:=Self;
end;

destructor TBinaryExpr.Destroy;
begin
  if Assigned(left) then left.Release;
  left:=nil;
  if Assigned(right) then right.Release;
  right:=nil;
  inherited Destroy;
end;

procedure TBinaryExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,left,false);
  ForEachChildCall(aMethodCall,Arg,right,false);
end;

{ TParamsExpr }

function TParamsExpr.GetDeclaration(full: Boolean): string;

Var
  I : Integer;

begin
  Result := '';
  For I:=0 to High(Params) do
    begin
    If (Result<>'')  then
      Result:=Result+', ';
    Result:=Result+Params[I].GetDeclaration(Full);  
    end;
  if Kind = pekSet then
    Result := '[' + Result + ']'
  else
    Result := '(' + Result + ')';
end;

procedure TParamsExpr.AddParam(xp:TPasExpr);
var
  i : Integer;
begin
  i:=Length(Params);
  SetLength(Params, i+1);
  Params[i]:=xp;
end;

procedure TParamsExpr.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  ForEachChildCall(aMethodCall,Arg,Value,false);
  for i:=0 to Length(Params)-1 do
    ForEachChildCall(aMethodCall,Arg,Params[i],false);
end;

constructor TParamsExpr.Create(AParent : TPasElement; AKind: TPasExprKind);
begin
  inherited Create(AParent,AKind, eopNone)
end;

destructor TParamsExpr.Destroy;
var
  i : Integer;
begin
  ReleaseAndNil(TPasElement(Value));
  for i:=0 to length(Params)-1 do Params[i].Release;
  inherited Destroy;
end;

{ TRecordValues }

function TRecordValues.GetDeclaration(full: Boolean): string;

Var
  I : Integer;
begin
  Result := '';
  For I:=0 to High(Fields) do
    begin
    If Result<>'' then
      Result:=Result+'; ';
    Result:=Result+Fields[I].Name+': '+Fields[i].ValueExp.getDeclaration(Full);
    end;
  Result:='('+Result+')';
end;

procedure TRecordValues.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(Fields)-1 do
    with Fields[i] do
      if ValueExp<>nil then
        ForEachChildCall(aMethodCall,Arg,ValueExp,false);
end;

constructor TRecordValues.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekListOfExp, eopNone);
end;

destructor TRecordValues.Destroy;
var
  i : Integer;
begin
  for i:=0 to length(Fields)-1 do
    Fields[i].ValueExp.Release;
  Fields:=nil;
  inherited Destroy;
end;

procedure TRecordValues.AddField(const AName:AnsiString;Value:TPasExpr);
var
  i : Integer;
begin
  i:=length(Fields);
  SetLength(Fields, i+1);
  Fields[i].Name:=AName;
  Fields[i].ValueExp:=Value;
  Value.Parent:=Self;
end;

{ TNilExpr }

function TNilExpr.GetDeclaration(full: Boolean): string;
begin
  Result:='Nil';
  if full then ;
end;

{ TInheritedExpr }

function TInheritedExpr.GetDeclaration(full: Boolean): string;
begin
  Result:='Inherited';
  if full then ;
end;

{ TSelfExpr }

function TSelfExpr.GetDeclaration(full: Boolean): string;
begin
  Result:='Self';
  if full then ;
end;

{ TArrayValues }

function TArrayValues.GetDeclaration(full: Boolean): string;

Var
  I : Integer;

begin
  Result := '';
  For I:=0 to High(Values) do
    begin
    If Result<>'' then
      Result:=Result+', ';
    Result:=Result+Values[i].getDeclaration(Full);
    end;
  Result:='('+Result+')';
end;

procedure TArrayValues.ForEachCall(const aMethodCall: TOnForEachPasElement;
  const Arg: Pointer);
var
  i: Integer;
begin
  inherited ForEachCall(aMethodCall, Arg);
  for i:=0 to length(Values)-1 do
    ForEachChildCall(aMethodCall,Arg,Values[i],false);
end;

constructor TArrayValues.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekListOfExp, eopNone)
end;

destructor TArrayValues.Destroy;
var
  i : Integer;
begin
  for i:=0 to length(Values)-1 do
    Values[i].Release;
  Values:=nil;
  inherited Destroy;
end;

procedure TArrayValues.AddValues(AValue:TPasExpr);
var
  i : Integer;
begin
  i:=length(Values);
  SetLength(Values, i+1);
  Values[i]:=AValue;
  AValue.Parent:=Self;
end;

{ TNilExpr }

constructor TNilExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekNil, eopNone);
end;

{ TInheritedExpr }

constructor TInheritedExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekInherited, eopNone);
end;


{ TSelfExpr }

constructor TSelfExpr.Create(AParent : TPasElement);
begin
  inherited Create(AParent,pekSelf, eopNone);
end;

{ TPasLabels }

constructor TPasLabels.Create(const AName:string;AParent:TPasElement);
begin
  inherited Create(AName,AParent);
  Labels := TStringList.Create;
end;

destructor TPasLabels.Destroy;
begin
  FreeAndNil(Labels);
  inherited Destroy;
end;

end.

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tolerant SEM-tier resolver wrapper over the fcl-passrc resolver engine

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Resolver;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree, Pascal.Resolver, Pascal.ResolveEval, Pascal.Resolver.Native,
{$ELSE}
  Classes, SysUtils, PasTree, pasresolver, pasresolveeval, pasnativeresolve,
{$ENDIF}
  FpSonar.Ingest, FpSonar.Types,
  FpSonar.PpuStub;

type
  { A FpSonar-owned, coarse classification of a resolved type. }
  TFpSonarTypeKind = (ltkOther, ltkChar, ltkString, ltkInteger, ltkFloat,
    ltkBool, ltkEnum, ltkPointer, ltkClass, ltkClassRef, ltkInterface,
    ltkRecord, ltkArray);

  { A FpSonar-owned codepage classification of a resolved string/char type
    lseNone = not a string/char OR an indeterminate encoding }
  TFpSonarStrEncoding = (lseNone, lseAnsi, lseWide);

  { A FpSonar-owned integer-portability classification of a resolved type
    (for a platform-dependent cast).
    * liwNone = not an integer OR an indeterminate width;
    * liwFixed = a fixed <=32-bit integer (Integer/Cardinal/SmallInt/Word/...);
    * liwPointerSized = a PtrInt/NativeInt-family integer;
    * liwWide = Int64/QWord (wide, portable). }
  TFpSonarIntWidth = (liwNone, liwFixed, liwPointerSized, liwWide);

  { What kind of object-disposal call a candidate node is.
    lfkNone = not a resolved Free/FreeAndNil disposal call;
    the other two name the two shapes whose inner cast TryFreeCall hands back. }
  TFpSonarFreeKind = (lfkNone, lfkFreeMethod, lfkFreeAndNil);

  { Which heap-management routine a resolved call site is:
    lmoNew / lmoGetMem allocate, lmoDispose / lmoFreeMem release. }
  TFpSonarMemoryOp = (lmoNew, lmoGetMem, lmoDispose, lmoFreeMem);

  { The resolved control-flow built-in a loop-body statement binds to (for
    SingleIterationLoop).
    lekNone = not a resolved break/continue/exit built-in (a shadowing user symbol or any unresolved candidate);
    the other three name the three loop-exit intrinsics, decided by the built-in proc data }
  TFpSonarLoopExitKind = (lekNone, lekBreak, lekContinue, lekExit);

  { The minimal FpSonar-owned re-exposure of a resolved type fact }
  TFpSonarResolvedType = record
    // The alias-collapsed resolved type element (LoTypeEl), or nil when
    // unresolved. Pointer-identity of this element is the redundant-cast test.
    TypeEl: TPasType;
    // The collapsed type's name (e.g. 'Longint'), '' when unresolved — a
    // string-only fact so rules need no pasresolver enum.
    TypeName: string;
    // The FpSonar-owned classification of the collapsed type.
    Kind: TFpSonarTypeKind;
    { The alias-preserved named type, '' when unresolved.
      The redundant-cast rule uses this to keep a distinct-named alias cast silent. }
    NamedTypeName: string;
    // The FpSonar-owned codepage class of a string/char type.
    Encoding: TFpSonarStrEncoding;
    // The FpSonar-owned integer-portability class, liwNone for anything else.
    IntWidth: TFpSonarIntWidth;
  end;

  { A FpSonar-owned re-exposure of a resolved RTL Format-family call. }
  TFpSonarFormatCall = record
    // arg[0], the format-string expression — the anchor row for the rules.
    FmtNode: TPasExpr;
    // The const-folded format string (meaningful iff FmtResolved).
    FmtText: string;
    // arg[0] const-folded to a string-kind value (literal OR const).
    FmtResolved: boolean;
    // arg[1] is an inline '[...]' array literal (a pekSet TParamsExpr).
    ArgsInline: boolean;
    // Element count of that literal (meaningful iff ArgsInline).
    ArgCount: integer;
    // The element expressions (meaningful iff ArgsInline) — for per-arg typing,
    // queried back through TryResolvedType.
    Args: TPasExprArray;
  end;

  { A FpSonar-owned re-exposure of one use site of a hint-carrying declaration. }
  TFpSonarHintedUse = record
    Node: TPasElement;
    Name: string;
    // The referenced declaration's hint modifiers; never empty.
    Hints: TPasMemberHints;
  end;

  TFpSonarHintedUseArray = array of TFpSonarHintedUse;

  { A dynamic array of measured generic-specialization nesting depths. }
  TFpSonarDepthArray = array of integer;

  TFpSonarResolver = class;

  { The resolver engine: the ONLY TPasResolver subclass }
  TFpSonarResolverEngine = class(TPasNativeResolver)
  private
    FOwner: TFpSonarResolver;
    // The stock range-check handler, displaced for the engine's whole life.
    FSavedRangeCheckEl: TPasResEvalRangeCheckElHandler;
    { The two cond-directive answers, each behind its own tolerant try/except
      (kept out of EvalCondDirective so it stays a thin dispatcher).
      declared(): '1' visible / '0' definitely-absent -> True; other failure ->
      False. sizeof(): the target-correct byte size -> True; un-sizable -> False. }
    function TryEvalDeclared(const aArgument: string; aGenArity: integer;
      out aValue: string): boolean;
    function TryEvalSizeOf(const aArgument: string; out aValue: string): boolean;
  public
    { Cross-unit resolution: delegate to the wrapper, which locates + parses +
      resolves the used unit (or returns nil to degrade). Never raises out of
      FindUnit — a nil result makes FindModule raise EPasResolve, folded to
      dkResolveError by the wrapper, preserving degrade-to-silence. }
    function FindUnit(const AName, InFilename: string;
      NameExpr, InFileExpr: TPasExpr): TPasModule; override;
    { Answers a scanner declared()/sizeof() conditional directive from this
      engine's live resolve scope.
      Returns True with aValue set to '1'/'0' or the target-correct byte size (sizeof) when the
      scope can decide; returns False to fail the unit whenever it cannot confidently decide}
    function EvalCondDirective(const aFuncName, aArgument: string;
      aGenArity: integer; out aValue: string): boolean;
  end;

  { The tolerant wrapper: owns the shared hub, every sub-engine, every kept-alive
    sub-parse bundle, the module cache and the synthetic-unit temp files;
    drives a resolved parse and exposes the resolved  module + the minimal query API. }
  TFpSonarResolver = class
  private
    FHub: TPasResolverHub;
    FEngines: TFPList;         // owns all TFpSonarResolverEngine (top + deps)
    FParses: TFPList;          // owns all TFpSonarResolvedParse (kept alive)
    FCacheNames: TStringList;  // normalized unit name -> resolved module (Objects)
    FFailed: TStringList;      // unit names that already failed (no re-parse)
    FInProgress: TStringList;  // cycle guard for circular uses
    FTopEngine: TFpSonarResolverEngine;
    FSucceeded: boolean;
    FDependencyInterfaceOnly: boolean;
    FIntrinsicConstEval: boolean;
    FCondDirectiveEval: boolean;
    FIntrinsicTargetPointerSize: integer;
    FRealRtlPreferred: boolean;
    FDialect: TFpSonarDialect;
    FPpuAutoDetect: boolean;
    FPpuCacheDir: string;
    // Config carried for FindUnit (BuildFor copies the open arrays in).
    FMode: string;
    FDefines: array of string;
    FUnitPaths: array of string;
    FIncludePaths: array of string;
    FSynthFiles: TStringList;  // synthetic unit name -> its temp file path
    function GetResolvedModule: TPasModule;
    // The named const declared in the resolved module's interface section, or nil.
    // Backs TryEvalConstInt.
    function FindInterfaceConst(const aConstName: string): TPasConst;
    // Creates a fresh sub-engine sharing the hub, registered for teardown.
    function NewEngine: TFpSonarResolverEngine;
    { The extra implicit-uses units for a parse of aUnitName under the real-RTL
      mode: ['objpas'] for every unit except objpas itself and System (which
      objpas depends on). Empty when the mode is off — byte-identical default. }
    function ImplicitUsesFor(const aUnitName: string): TFpSonarStringArray;
    { The scope-backed declared()/sizeof() cond-directive evaluator bound to
      aEngine, or nil when CondDirectiveEval is off — nil leaves the scanner's
      OnEvalFunction unassigned. }
    function CondEvalQueryFor(aEngine: TFpSonarResolverEngine): TFpSonarCondEvalQuery;
    // Searches the unit search paths for <name>.pas/.pp (case-insensitive); ''
    // if not found.
    function LocateUnit(const aName: string): string;
    { The on-demand auto-detect stub FILE for aName (materialised + cached) }
    function AutoDetectFile(const aName: string): string;
    // The embedded synthetic source for an RTL unit name (System/SysUtils/
    // Classes), or '' for any other name (not a synthetic unit).
    function SyntheticSource(const aName: string): string;
    { Lazily materialises a synthetic unit's embedded source to a temp file
      (one per name, reused across builds) and returns its path; '' when aName
      is not a synthetic unit or the write fails (that unit then degrades). }
    function SyntheticUnitFile(const aName: string): string;
    // Folds aExpr behind a catch-all; nil on a non-constant expression or on
    // any raise from the evaluator.
    function EvalGuarded(aExpr: TPasExpr): TResEvalValue;
    { The evaluator's own range value for aType behind a catch-all;
      nil for a type it models no range for and on any raise. }
    function TypeRangeGuarded(aType: TPasType): TResEvalValue;
    { Asks the evaluator whether aValue lies in aRange, behind a catch-all.
      Result=False when the pair is undecidable: the evaluator raises on kind
      combinations it does not model. }
    function TryInRangeGuarded(aValue: TResEvalValue; aValueExpr: TPasExpr;
      aRange: TResEvalValue; aRangeExpr: TPasExpr;
      out aInRange: boolean): boolean;
    // Resolves aElement's type behind a catch-all; False when it does not resolve.
    function ComputeGuarded(aElement: TPasElement;
      out aResolved: TPasResolverResult): boolean;
    { True iff aExpr is a call to the compiler built-in aBuiltIn with aCount
      parameters; aParams is that call node. }
    function BuiltInCall(aExpr: TPasExpr; aBuiltIn: TResolverBuiltInProc;
      aCount: integer; out aParams: TParamsExpr): boolean;
    { The integer width and signedness of aBaseType; False for a base type the
      resolver models no integer properties for. }
    function IntegerPropsGuarded(aBaseType: TResolverBaseType;
      out aPrecision: word; out aSigned: boolean): boolean;
    // Frees the per-build state (bundles -> engines), clears the caches and
    // resets the hub — keeps the hub object and the lists for reuse.
    procedure ClearBuild;
  public
    constructor Create;
    destructor Destroy; override;
    { Runs a resolved parse of aFileName through the parser adapter with a
      freshly-owned top engine, resolving cross-unit dependencies via the unit
      search paths.
      Returns True on a fully resolved module; on a parse OR resolution failure returns False,
      and fills aDiag (dkParseError / dkResolveError) and leaves ResolvedModule nil }
    function BuildFor(const aFileName, aCompilerMode: string;
      const aDefines, aUnitPaths, aIncludePaths: array of string;
      out aDiag: TFpSonarDiagnostic): boolean;
    { Cross-unit FindUnit body: returns a fully resolved module for aName,
      or nil on any not-found / parse / resolve failure or a circular-uses re-entry. }
    function ResolveDependency(const aName: string): TPasModule;
    { The source files to try for aName, in resolution-order: under the real-RTL
      mode real-source-first (LocateUnit) then the synthetic stub; otherwise the
      committed synthetic-first order. Empty entries are dropped. }
    function OrderedCandidates(const aName: string): TFpSonarStringArray;
    { Resolves a single candidate file into a fully resolved module with its own
      kept-alive sub-engine; nil on any parse/resolve failure without touching
      FFailed, so the caller can try the next candidate }
    function TryResolveModule(const aFile, aDepName: string): TPasModule;
    { Tolerant type query: the resolved type of aElement, or the unresolved
      sentinel (Result=False, TypeEl=nil) on an unresolved/untyped element or ANY
      caught exception.}
    function TryResolvedType(aElement: TPasElement;
      out aType: TFpSonarResolvedType): boolean;
    // The declared byte size of aType; False leaves aSize 0 and means no known size.
    function TryTypeByteSize(aType: TPasType; out aSize: integer): boolean;
    { Tolerant typecast query:
      True iff aCandidate is a CONFIRMED typecast `T(x)`
      (a single-arg pekFuncParams whose callee alias-resolves to a type, NOT a
      function call) with BOTH sides resolved — aTarget = the cast's result
      (target) type, aSource = the operand's type. }
    function TryTypecast(aCandidate: TPasElement;
      out aTarget, aSource: TFpSonarResolvedType): boolean;
    { Tolerant class-relation query: Result=True means the relation was
      determined and aRelated tells whether either class is an
      ancestor of the other (same class counts as related). Result=False means
      undeterminable. the caller emits nothing. }
    function TryClassRelation(aClassA, aClassB: TPasType;
      out aRelated: boolean): boolean;
    { Tolerant Free/FreeAndNil-call query:
      Result is
      * lfkFreeMethod iff aNode is a  resolved `<expr>.Free` member access whose callee
        is TObject.Free;
      * lfkFreeAndNil iff aNode is a RESOLVED single-arg call to SysUtils.FreeAndNil.
        aInner is set to the inner candidate expression .
      *  lfkNone (aInner=nil) on any non-match, unresolved callee, or caught exception.
      }
    function TryFreeCall(aNode: TPasElement; out aInner: TPasExpr): TFpSonarFreeKind;
    { Tolerant Format-call query:
      True iff aNode is a confirmed call to the RTL SysUtils.Format.
      On True, aCall carries the format-string node, the const-folded format string
      (FmtResolved iff arg[0] folds to a string), and the inline `[...]` args + element list }
    function TryFormatCall(aNode: TPasElement; out aCall: TFpSonarFormatCall): boolean;
    { Tolerant reference-declaration query: the resolved declaration TPasElement an
      expression's reference points to (a TPasProperty,
      TPasVariable, TPasArgument, TPasEnumValue, ...), or nil when the expression
      carries no resolved reference or on any caught exception.}
    function ReferencedDecl(aExpr: TPasElement): TPasElement;
    { Tolerant constructor-call query: Result=True iff aNode is a confirmed call
      whose resolved callee declaration is a TPasConstructor. }
    function TryConstructorCall(aNode: TPasElement; out aOnInstance: boolean;
      out aCtorName: string): boolean;
    { Tolerant destructor-contract query: Result=True iff aDtor resolves to a
      destructor whose owning class ancestry is fully resolvable }
    function TryDestructorContract(aDtor: TPasElement;
      out aDescendsTObject: boolean; out aInheritedVirtualDestroy: boolean): boolean;
    { Tolerant "override method only forwards to inherited" query: Result=True iff
      aProc is an ORDINARY override method (its declaration carries pmOverride }
    function TryOverrideOnlyForwards(aProc: TPasElement;
      aInheritedExpr: TPasExpr): boolean;
    { Tolerant redundant-inherited query (Case A): Result=True iff aProc is an
      ordinary method (NOT a constructor/destructor) whose resolved
      declaration scope has NO same-signature ancestor proc (OverriddenProc = nil).}
    function TryRedundantInherited(aProc: TPasElement): boolean;
    { Tolerant hidden-virtual query (MethodHidesVirtualWithoutOverride):
      Result=True iff aProc's resolved declaration has a same-signature ancestor
      proc that is overridable, while the declaration itself carries none of
      override, reintroduce or overload. }
    function TryMethodHidesVirtual(aProc: TPasElement): boolean;
    { Tolerant default-parameter-change query
      (OverrideChangesDefaultParameterValue): Result=True iff aProc's resolved
      declaration is an override with a same-signature ancestor, and some
      argument's default folds on both sides to different values. }
    function TryOverrideDefaultParamChange(aProc: TPasElement): boolean;
    { Tolerant abstract-instantiation query
      (InstantiatesClassWithAbstractMethods): a whole-module collector over the
      resolved reference sites. aNodes returns every `T.Create` site whose
      qualifier NAMES a class carrying unimplemented abstract methods,
      aClassNames that class. }
    function TryAbstractInstantiation(out aNodes: TPasElementArray;
      out aClassNames: TFpSonarStringArray): boolean;
    { Tolerant class-helper shadowing query (ClassHelperHidesAncestorMethod): a
      whole-module collector over the class helpers declared in the analysed
      module. aNodes returns every helper method whose name is also declared by
      the extended class or one of its resolved ancestors, aNames that name. }
    function TryClassHelperHiddenMethods(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant public-field-backed-property query
      (PublicFieldAndPropertyForSameStorage): a whole-module collector over the
      properties declared in the analysed module. aNodes returns every property
      whose read or write accessor resolves to a field of public or published
      visibility, aNames that field's name. }
    function TryPublicFieldBackedProperties(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant accessor-visibility query
      (PropertyAccessorVisibilityWiderThanProperty): a whole-module collector
      over the properties declared in the analysed module. aNodes returns every
      property whose read or write accessor resolves to a routine declared more
      visible than the property itself, aNames the property's name. }
    function TryWiderAccessorProperties(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant getter-side-effect query (PropertyGetterWithSideEffect): a
      whole-module collector over the properties declared in the analysed
      module. aNodes returns every property whose read accessor has an
      implementation that writes a field of its own members type, aNames the
      property's name. }
    function TryPropertyGetterFieldWrites(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant hidden-constructor query
      (ConstructorNotVirtualInPolymorphicHierarchy): a whole-module collector
      over the classes declared in the analysed module. aNodes returns every
      non-overridable constructor of a class that declares a virtual or dynamic
      method and that another class of the module descends while declaring a
      constructor of the same name, aNames the qualified <Class>.<Ctor>. }
    function TryHiddenNonVirtualConstructors(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant IfThen-misuse query: Result=True iff aNode is a RESOLVED call to
      Math.IfThen or StrUtils.IfThen whose condition (Params[0]) guards a subject }
    function TryIfThenMisuse(aNode: TPasElement; out aSubject: string): boolean;
    { Tolerant built-in-Assert query: Result=True iff aNode is a RESOLVED call to
      the compiler built-in Assert — identity taken from the
      resolved reference's built-in proc data }
    function TryAssertCall(aNode: TPasElement; out aArgCount: integer): boolean;
    { Tolerant date-format default-settings query: Result=True iff aNode is a
      RESOLVED call to a SysUtils locale-sensitive date/time routine
      (module identity via GetModule = SysUtils. }
    function TryDefaultFormatSettingsDateCall(aNode: TPasElement;
      out aRoutineName: string): boolean;
    { Tolerant explicit-default-array-property query: Result=True iff aNode is a
      pekArrayParams access whose Value EXPLICITLY names a property that resolves to a class
      DEFAULT array property }
    function TryExplicitDefaultArrayProperty(aNode: TPasElement;
      out aPropName: string): boolean;
    { Tolerant string-first-char-by-index query: Result=True iff aNode is a
      single-index pekArrayParams access whose Value resolves to a
      string type AND the index constant-folds (via the evaluator) to an integer
      equal to the low index (cLowIndex, default 1). }
    function TryStringFirstCharByIndex(aNode: TPasElement): boolean;
    { Tolerant TList-last-by-index query: Result=True iff aNode is a single-index
      pekArrayParams access on a CONFIRMED list type (a
      class named TList in module Classes, or a descendant — ancestor-scope walk;
      the class also exposes a Last member) whose index is the exact Count-1
      pattern }
    function TryListLastByIndex(aNode: TPasElement): boolean;
    { Tolerant implicit-TEncoding.Default query: Result=True iff aNode is a call
      (pekFuncParams) that binds a curated stdlib byte/string-
      conversion overload}
    function TryImplicitTEncodingDefault(aNode: TPasElement): boolean;
    { Tolerant single-precision math-overload query: Result=True iff aNode is a
      call  that binds a curated standard math routine whose bound overload's
      first argument resolves to the built-in Single type,
      AND a sibling overload of the same name (visible in the same unit
      section scope) declares a Double/Extended first argument.}
    function TrySingleOverloadOfMathFunction(aNode: TPasElement): boolean;
    { Tolerant non-exhaustive enum-case query: Result=True iff aNode is a `case`
      whose selector resolves to an enumerated type }
    function TryNonExhaustiveEnumCase(aNode: TPasElement;
      out aMissing: TFpSonarStringArray): boolean;
    { Tolerant discarded-exception-construction query: Result=True iff aNode is a
      constructor call whose constructed class provably descends from the exception
      base class  }
    function TryDiscardedExceptionConstruction(aNode: TPasElement;
      out aClassName: string): boolean;
    { Tolerant raw-exception-catch query (NoCatchRawException):
      Result=True iff aNode is a TPasImplExceptOn whose caught TypeEl, alias-resolved
      through the chokepoint, terminates in a TPasClassType whose name is EXACTLY the
      root Exception (or TObject)}
    function TryHandlerCatchesRawException(aNode: TPasElement): boolean;
    { Tolerant raw-exception-raise query (NoRaiseRawException):
      Result=True iff aNode is a TPasImplRaise whose ExceptObject constructs an
      instance of EXACTLY the root Exception (or TObject) class. }
    function TryRaisesRawException(aNode: TPasElement): boolean;
    { Tolerant handler-order query (HandlerOrderShadowsDerived):
      Result=True iff aNode is a TPasImplExceptOn in a TPasImplTryExcept that has an
      EARLIER sibling handler whose caught class, alias-collapsed, aNode's caught
      class equals or descends from. aHandlerName / aAncestorName are the two class
      names. }
    function TryHandlerShadowedByEarlier(aNode: TPasElement;
      out aHandlerName, aAncestorName: string): boolean;
    { Tolerant non-Exception-raise query (ExceptionClassNotDerivedFromException):
      Result=True iff aNode is a TPasImplRaise whose ExceptObject constructs an
      instance of a class other than TObject whose resolved ancestor-scope chain
      TERMINATES at TObject without passing through Exception. aClassName is the
      constructed class name; a chain ending anywhere else abstains. }
    function TryRaisedClassNotException(aNode: TPasElement;
      out aClassName: string): boolean;
    { Tolerant load-bearing-Assert query (AssertUsedForControlFlow):
      Result=True iff aNode is a call to the built-in Assert whose argument 0
      subtree calls a user-declared routine. aRoutineName is that routine's name,
      the first one a depth-first walk of the argument reaches; the message
      argument is not examined. }
    function TryAssertControlFlowOperand(aNode: TPasElement;
      out aRoutineName: string): boolean;
    { Tolerant loop-control-flow query: classifies a statement that may be a
      break/continue/exit by the RESOLVED built-in proc}
    function TryLoopControlFlow(aNode: TPasElement;
      out aKind: TFpSonarLoopExitKind): boolean;
    { Tolerant Pascal-style result-assignment query: Result=True iff aNode is a
      TPasImplAssign whose left-hand side resolves }
    function TryPascalStyleResultAssign(aNode: TPasElement;
      out aFuncName: string): boolean;
    { Tolerant redundant-Assigned-before-Free query: Result=True iff aNode is a
      TPasImplIfElse with NO else whose single if-branch
      statement is exactly a `Free`/`FreeAndNil` disposal }
    function TryRedundantAssignedCheckBeforeFree(aNode: TPasElement): boolean;
    { Tolerant self-disposal query: Result=True iff aNode is a statement holding a
      resolved `Free`/`FreeAndNil` disposal whose operand is the enclosing
      routine's own Self argument, the unqualified `Free` form included.
      False outside a method body and whenever the routine, its scope or its Self
      argument is not recovered. }
    function TryDisposesSelf(aNode: TPasElement): boolean;
    { Tolerant heap-management-call query: Result=True iff aNode is a statement
      whose expression is a resolved call to New/Dispose (compiler built-ins) or
      to the RTL GetMem/FreeMem, and whose first argument carries a resolved
      declaration. aOp names the routine, aOperand is that declaration and
      aOperandName its name. }
    function TryMemoryOpCall(aNode: TPasElement; out aOp: TFpSonarMemoryOp;
      out aOperand: TPasElement; out aOperandName: string): boolean;
    { Tolerant loop-beyond-collection-end query: Result=True iff aNode is a counted
      for-loop whose body indexes a FIXED-LENGTH collection — a static array
      - with a loop-variable index (loopvar or loopvar +/- a constant) whose
      reachable interval provably exceeds the array's valid index
      range. }
    function TryLoopBeyondCollectionEnd(aNode: TPasElement;
      out aOverrun: TPasElement): boolean;
    { Tolerant loop-variable-read-after-loop query: Result=True iff aNode is a
      counted for-loop whose control variable is a routine local that some
      reference site READS on a later row, outside aNode, outside every other
      for-loop counting that same declaration and with no such loop between
      aNode and it. aSite is the lowest such row's node, aVarName the name. }
    function TryLoopVarReadAfterLoop(aNode: TPasElement; out aVarName: string;
      out aSite: TPasElement): boolean;
    { Tolerant shadowable-unqualified-reference query (FullyQualifiedImports): a
      whole-module collector. aNodes returns every
      offending identifier-reference node that is unqualified, resolves to a declaration in a different
      module, and whose simple name is exported by >= 2 distinct in-scope source modules }
    function TryShadowedUnqualifiedRefs(out aNodes: TPasElementArray): boolean;
    { Tolerant movable-interface-import query (MoveImportToImplementation): a
      whole-section collector. aNodes returns every movable interface-`uses`
      unit node — a unit listed in the INTERFACE uses clause whose
      module is referenced by NO interface declaration/signature, so it can move
      to the implementation uses clause. }
    function TryMovableInterfaceUnits(out aNodes: TPasElementArray): boolean;
    { Tolerant object-assigned-to-COM-interface query (NoObjectAsInterface): a
      whole-module collector. aNodes returns every source
      expression node where a concrete class INSTANCE is assigned
      to, or passed where the declared type is, a COM interface — the implicit
      IUnknown reference-count acquisition hazard on a manually-managed object.
      CORBA interface targets carry no refcounting and are exempt
      }
    function TryObjectAsComInterfaceSites(out aNodes: TPasElementArray): boolean;
    { Tolerant nested-routine-address-escape query (NoNestedRoutineAsProcValue): a
      whole-module collector. aNodes returns every
      `@`-expression (the SourceRow anchor) that takes the address of a nested
      routine and stores it into a slot that outlives the enclosing routine frame — a
      global variable or a record/class field. }
    function TryNestedRoutineAsProcValueSites(out aNodes: TPasElementArray): boolean;
    { Tolerant inline-var-captured-by-anonymous-method query
      (NoInlineVarCapturedByAnonMethod): a whole-module collector. aNodes returns
      every capturing reference site at which an anonymous  method captures a
      free variable whose lifetime is less than the closure's }
    function TryInlineVarCapturedByAnonMethodSites(
      out aNodes: TPasElementArray): boolean;
    { Tolerant idle-generic-constraint query (GenericConstraintUnused): a
      whole-module collector over the generic types declared in the analysed
      module. aNodes returns every type-constrained template parameter of a
      generic that has an implemented routine and on which none of those
      routines rely — no member selection on the parameter or on a declaration
      typed by it, no `is`/`as` naming it, and no nil assignment or comparison
      against it. }
    function TryUnusedGenericConstraints(out aNodes: TPasElementArray): boolean;
    { Tolerant unconstrained-specialization query
      (SpecializationOfUnconstrainedGeneric): a whole-module collector over the
      declaration-site specializations of the analysed module. aNodes returns
      every specialization node whose generic declares at least one type
      parameter and constrains none of them; const parameters are not judged. }
    function TryUnconstrainedSpecializations(
      out aNodes: TPasElementArray): boolean;
    { Tolerant specialization-nesting query (NestedGenericSpecializationDepth): a
      whole-module collector over the declaration-site specializations of the
      analysed module. aNodes returns every specialization node nesting more than
      aMaxDepth levels of specialization, aDepths the measured depth of each. }
    function TryDeepNestedSpecializations(aMaxDepth: integer;
      out aNodes: TPasElementArray; out aDepths: TFpSonarDepthArray): boolean;
    { Tolerant anonymous-method loop-variable-capture query
      (AnonymousMethodCapturesLoopVariable): a whole-module collector. aNodes
      returns every reference expression inside an anonymous method that names
      the control variable of an enclosing classic for loop, one per captured
      declaration. }
    function TryAnonMethodLoopVarCaptures(
      out aNodes: TPasElementArray): boolean;
    { Tolerant anonymous-method Self-capture query
      (AnonymousMethodCapturesSelf): a whole-module collector. aNodes returns
      every anonymous procedure whose body reaches the enclosing instance —
      the Self argument of an enclosing method, or an instance member named
      without a qualifier. }
    function TryAnonMethodSelfCaptures(out aNodes: TPasElementArray): boolean;
    { Tolerant inconsistent-name-casing query (ConsistentNameCasing): a
      whole-module collector. For every resolved  identifier reference in the
      module's statement bodies, aNodes returns the  offending reference node }
    function TryInconsistentCasingSites(out aNodes: TPasElementArray;
      out aUsed, aCanonical: TFpSonarStringArray): boolean;
    { Tolerant descendant-naming query (DescendantNamingConvention): a whole-module collector.
      For every class or  interface declaration in the module that transitively descends a configured
      base type, aNodes returns the  declaring node and aBases the matched base key.}
    function TryDescendantNamingViolationSites(out aNodes: TPasElementArray;
      out aBases: TFpSonarStringArray): boolean;
    { Tolerant uses-clause-unit -> resolved-path query (DisallowedImportByPath):
      a whole-module collector over both the interface and implementation uses clauses.  }
    function TryUsesUnitPaths(out aNodes: TPasElementArray;
      out aPaths: TFpSonarStringArray): boolean;
    { Tolerant resolved-reference-site -> declaration query (DisallowedConstant / DisallowedEnumValue).
      A whole-module collector over every identifier-reference expression in the
      resolved module — declaration initializers and statement-tree expressions;
      plus the module's init/finalization block.
      The two arrays are parallel: aNodes[i] is the reference expr,
      aDecls[i] the resolved declaration it refers to.  }
    function TryReferenceSites(out aNodes: TPasElementArray;
      out aDecls: TPasElementArray): boolean;
    { Tolerant hint-modifier query (DeprecatedSymbolUsed / ExperimentalSymbolUsed /
      PlatformSymbolUsedInPortableUnit): the TryReferenceSites pairs whose
      declaration carries hint modifiers, plus the analysed module's own hints. }
    function TryHintedSymbolUses(out aUses: TFpSonarHintedUseArray;
      out aModuleHints: TPasMemberHints): boolean;
    { Tolerant identifier-name-site query.
      Same whole-module tree walk as TryReferenceSites, but collects every identifier leaf in
      expression position. aNodes[i] is the leaf, aNames[i] its textual spelling }
    function TryIdentifierNameSites(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant statement read/write query (the forward-dataflow lattice).
      Splits the resolved references held by aStmt's OWN value and control
      expressions -- its sub-statements excluded -- by access kind.
      aReadNodes[i]/aReadDecls[i] are parallel: the reference node and the
      declaration it reads. aWriteDecls holds the declarations written.
      An out argument counts as a read as well as a write.
      Result=False means the statement cannot be answered for: inline
      assembler, an identifier leaf carrying no resolved reference, an
      anonymous-method operand, an unlisted statement kind, or a caught
      resolver failure. A listed statement kind carrying no expressions answers
      True with empty arrays. }
    function TryStatementAccess(aStmt: TPasElement;
      out aReadNodes: TPasElementArray; out aReadDecls: TPasElementArray;
      out aWriteDecls: TPasElementArray): boolean;
    { The 1-based source row of aElement in the resolved tree.
      Resolution stores columns, so the resolver packs row+column into the raw
      SourceLinenumber }
    function SourceRow(aElement: TPasElement): integer;
    { Tolerant const-int query: folds the named const's initializer in the resolved module's interface.
      Result=True iff aConstName names a const that  resolved;
      aKnown=True with aValue set iff its initializer folds to an integer;
      aKnown=Falsewhen the const resolved but its value is unknown }
    function TryEvalConstInt(const aConstName: string; out aValue: int64;
      out aKnown: boolean): boolean;
    // Tolerant constant-zero-divisor query: True iff aExpr is a /, div or mod
    // whose right operand folds to zero; aOperator is the operator text.
    function TryConstDivisionByZero(aExpr: TPasExpr;
      out aOperator: string): boolean;
    // Tolerant constant-boolean query: True iff aExpr folds to a boolean value.
    function TryConstBooleanValue(aExpr: TPasExpr; out aValue: boolean): boolean;
    { Tolerant type-fixed-comparison query (ComparisonAlwaysTrueForType):
      True iff aExpr compares an operand against a folding integer constant and
      the verdict is the same for every value of that operand's type range.
      aTypeName is the operand's type, aConstant the constant, aValue the verdict. }
    function TryComparisonAlwaysTrueForType(aExpr: TPasExpr;
      out aTypeName, aConstant: string; out aValue: boolean): boolean;
    { Tolerant assignment-range query (ConstantOutOfRangeForTarget):
      True iff aAssign is a `:=` whose right side folds to a value outside the
      range of the left side's type. }
    function TryConstantOutOfRangeForTarget(aAssign: TPasElement;
      out aConstant, aTypeName: string): boolean;
    { Tolerant constant-overflow query (ConstantOverflowInExpression):
      True iff aExpr adds, subtracts or multiplies two folding integers and the
      whole expression does not fold. aOperator is the operator text. }
    function TryConstantOverflowInExpression(aExpr: TPasExpr;
      out aOperator: string): boolean;
    { Tolerant shift-width query (ShiftCountExceedsWidth):
      True iff aExpr is a shl/shr by a folding non-negative count at or above the
      width FPC computes the shift in — the shifted operand's integer type, or
      Longint when that type is narrower. aTypeName is that width's type. }
    function TryShiftCountExceedsWidth(aExpr: TPasExpr;
      out aCount, aTypeName: string): boolean;
    { Tolerant set-membership query (SetElementOutOfRange):
      True iff aExpr is an `in` whose right side folds to a set holding an
      element outside the range of the left operand's type. }
    function TrySetElementOutOfRange(aExpr: TPasExpr;
      out aTypeName: string): boolean;
    { Tolerant enum-cast query (EnumOrdinalOutOfRange):
      True iff aExpr casts a folding integer to an enumeration whose ordinal
      range does not hold it. }
    function TryEnumOrdinalOutOfRange(aExpr: TPasExpr;
      out aOrdinal, aEnumName: string): boolean;
    { Tolerant array-index query (ArrayIndexConstantOutOfBounds):
      True iff aExpr indexes a statically ranged array with a folding index
      outside the declared bounds of that dimension. }
    function TryArrayIndexConstantOutOfBounds(aExpr: TPasExpr;
      out aIndex, aArrayName: string): boolean;
    { Tolerant SizeOf-argument query (SizeOfOnReferenceType):
      True iff aExpr is a built-in SizeOf whose argument is a class, interface,
      dynamic array or long string. aTypeName is that argument's type. }
    function TrySizeOfOnReferenceType(aExpr: TPasExpr;
      out aTypeName: string): boolean;
    { Tolerant byte-count query (MoveFillCharSizeMismatch):
      True iff aExpr is a three-argument Move/FillChar whose count is a Length
      of a multi-byte-element array or a SizeOf of a pointer.
      aCallee is the routine name, aCount the count routine's name. }
    function TryMoveFillCharSizeMismatch(aExpr: TPasExpr;
      out aCallee, aCount: string): boolean;
    { Tolerant float-equality query (FloatEqualityComparison):
      True iff aExpr is an = or <> with a floating-point operand.
      aTypeName is that operand's type. }
    function TryFloatEqualityComparison(aExpr: TPasExpr;
      out aTypeName: string): boolean;
    { Tolerant truncating-assignment query (IntegerDivisionAssignedToFloat):
      True iff aAssign is a `:=` of an integer `div` to a floating-point target.
      aTypeName is the target type. }
    function TryIntegerDivisionAssignedToFloat(aAssign: TPasElement;
      out aTypeName: string): boolean;
    { Tolerant signedness query (MixedSignedUnsignedComparison):
      True iff aExpr compares a signed against an unsigned integer of the same
      width, at 32 bits or wider, naming each operand's type by its role. }
    function TryMixedSignedUnsignedComparison(aExpr: TPasExpr;
      out aSignedType, aUnsignedType: string): boolean;
    { Tolerant temporary-pointer query (PCharOfTemporaryString):
      True iff aExpr casts a string to a pointer-to-char whose operand is a
      function-call result or a concatenation. aTargetName is the target type. }
    function TryPCharOfTemporaryString(aExpr: TPasExpr;
      out aTargetName: string): boolean;
    { Tolerant implicit-conversion query (ImplicitStringConversionWithDataLoss):
      True iff aNode is an assignment whose target resolves to an ANSI-encoded
      string and whose source resolves to a wide-encoded one.
      aSourceName and aTargetName are the two type names. }
    function TryImplicitStringConversion(aNode: TPasElement;
      out aSourceName, aTargetName: string): boolean;
    { Tolerant character-count query (LengthUsedAsByteCount):
      True iff aExpr is a three-argument Move/FillChar whose count is a Length
      of a wide-encoded string. aStringName is that string's type,
      aCallee the routine name. }
    function TryLengthUsedAsByteCount(aExpr: TPasExpr;
      out aStringName, aCallee: string): boolean;
    { Tolerant zero-index query (CopyWithZeroIndex):
      True iff aExpr is the string Copy, in its two- or three-argument form,
      whose start index folds to exactly 0. aStringName is the copied type. }
    function TryCopyWithZeroIndex(aExpr: TPasExpr;
      out aStringName: string): boolean;
    { Tolerant zero-based-result query (PosResultComparedToZeroBased):
      True iff aExpr compares a unit-level Pos call against a folding 0 or -1
      in one of the six shapes that read a miss as negative.
      aCallee is the routine name, aConstText the constant. }
    function TryPosResultComparedToZeroBased(aExpr: TPasExpr;
      out aCallee, aConstText: string): boolean;
    { Tolerant shortstring-capacity query (ShortStringTruncation):
      True iff aNode assigns a folding string constant longer than the length
      declared on the target's string[N] declaration.
      aTargetName is the target, aCapacity that declared length. }
    function TryShortStringTruncation(aNode: TPasElement;
      out aTargetName, aCapacity: string): boolean;
    { Tolerant char-comparison query (CharComparedToString):
      True iff aExpr is an = or <> between a char operand and a string constant
      folding to a text whose length is not one.
      aCharTypeName is the char operand's type, aLength the constant's length. }
    function TryCharComparedToString(aExpr: TPasExpr;
      out aCharTypeName, aLength: string): boolean;
    { Tolerant code-page-mix query (RawByteStringCodePageMix):
      True iff aNode is a `:=` between a RawByteString operand and one whose
      declaration writes a code page, with no cast written on either side.
      aRawName is the RawByteString declaration, aCodePage that written page. }
    function TryRawByteStringCodePageMix(aNode: TPasElement;
      out aRawName, aCodePage: string): boolean;
    { Tolerant concatenation query (StringConcatInLoop):
      True iff aNode assigns to a string declaration the sum of that same
      declaration and another operand, inside a for, while or repeat loop.
      aTargetName is the accumulated declaration. }
    function TryStringConcatInLoop(aNode: TPasElement;
      out aTargetName: string): boolean;
    { Tolerant conversion-guard query (StrToIntWithoutGuard):
      True iff aExpr is a unit-level StrToInt call whose argument does not fold
      to a constant and which no enclosing try..except protects.
      aArgName is that argument's declaration. }
    function TryStrToIntWithoutGuard(aExpr: TPasExpr;
      out aArgName: string): boolean;
    { Tolerant wide-string-declaration query (WideStringOnNonWindows):
      True iff aDecl is a variable or an argument whose type resolves
      WideString. aDeclName is the declaration's name. }
    function TryWideStringDeclaration(aDecl: TPasElement;
      out aDeclName: string): boolean;
    // The resolved module after a successful BuildFor, else nil.
    property ResolvedModule: TPasModule read GetResolvedModule;
    // The top engine of the last BuildFor, succeeded or not; nil before the first.
    property Engine: TFpSonarResolverEngine read FTopEngine;
    // True iff the last BuildFor produced a fully resolved module.
    property Succeeded: boolean read FSucceeded;
    { When True, dependency units resolve interface-only;
      the unit-under-analysis always resolves in full. Default False. }
    property DependencyInterfaceOnly: boolean
      read FDependencyInterfaceOnly write FDependencyInterfaceOnly;
    { When True, every engine registers the const-eval intrinsics
      and folds pointer typecasts of constant integers
      and  accepts address-of-const as an opaque pointer constant. Default False }
    property IntrinsicConstEval: boolean read FIntrinsicConstEval
      write FIntrinsicConstEval;
    { Target pointer width in bytes used by SizeOf(Pointer)/^T/PChar folds.
      Defaults to the host pointer size; set from the configured TargetCPU so the
      fold is target-correct rather than a hard-coded guess. }
    property IntrinsicTargetPointerSize: integer
      read FIntrinsicTargetPointerSize write FIntrinsicTargetPointerSize;
    { The opt-in real-RTL-preferred master switch.
      When True the RTL chain resolves the real source first with a clean
      synthetic fallback.  }
    property RealRtlPreferred: boolean read FRealRtlPreferred
      write FRealRtlPreferred default False;
    { The source dialect. dlPas2js threads the pas2js parse-relevant switches
      into every dependency parse. Default dlDefault (byte-identical). }
    property Dialect: TFpSonarDialect read FDialect write FDialect;
    { Auto-detect ppudump-from-live-.ppu resolution.
      When True, each dependency resolves first against an on-demand, cached stub:
      the host-independent hybrid RTL (synthetic + faithful gap) for SysUtils/Classes,
      and a faithful ppudump  stub generated from the host's own version-matched .ppu when it exists}
    property PpuAutoDetect: boolean
      read FPpuAutoDetect write FPpuAutoDetect default False;
    { Persistent ppudump-stub cache directory (--ppu-cache).
      '' (default) keeps the per-run temp behaviour.
      When set (and PpuAutoDetect is on), each non-synthetic dependency stub is cached here across runs,
      keyed by the .ppu's path+size+mtime, so a warm cache resolves without running ppudump }
    property PpuCacheDir: string read FPpuCacheDir write FPpuCacheDir;
    (* When True, the scope-backed declared()/sizeof() cond-directive evaluator is
       bound to every engine's scanner, so {$if declared(X)} / {$if sizeof(T)=N} *)

    property CondDirectiveEval: boolean read FCondDirectiveEval
      write FCondDirectiveEval default False;
  end;


implementation

uses
  FpSonar.Consts;

type
  { A dynamic array of statement elements }
  TImplStmtArray = array of TPasImplElement;

  { A dynamic array of source rows }
  TImplRowArray = array of integer;

const
  { The pointer-sized integer type names }
  allowedPtrIntTypes: array[0..5] of string =
    ('PtrInt', 'PtrUInt', 'NativeInt', 'NativeUInt', 'IntPtr', 'UIntPtr');

  // The string low bound. Low(string)=1 for FPC {$H+} strings.
  // {$ZEROBASEDSTRINGS} is not supported.
  cLowIndex = 1;

  { The curated list-type set: the wrapper confirms a receiver is or descends from this  }
  cListClassName = 'TList';
  cListModuleName = 'Classes';

  { The enriched synthetic System unit. }
  cSyntheticSystemSource =
    'unit System;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TObject = class;' + LineEnding +
    '  TClass = class of TObject;' + LineEnding +
    '  IUnknown = interface' + LineEnding +
    '  end;' + LineEnding +
    '  IInterface = IUnknown;' + LineEnding +
    '  Integer = Longint;' + LineEnding +
    '  Cardinal = LongWord;' + LineEnding +
    '  PtrInt = Int64;' + LineEnding +
    '  PtrUInt = QWord;' + LineEnding +
    '  SizeInt = Int64;' + LineEnding +
    '  SizeUInt = QWord;' + LineEnding +
    '  NativeInt = Int64;' + LineEnding +
    '  NativeUInt = QWord;' + LineEnding +
    '  UInt64 = QWord;' + LineEnding +
    '  Real = Double;' + LineEnding +
    '  PChar = ^AnsiChar;' + LineEnding +
    '  PAnsiChar = ^AnsiChar;' + LineEnding +
    '  PWideChar = ^WideChar;' + LineEnding +
    '  PByte = ^Byte;' + LineEnding +
    '  UTF8String = AnsiString;' + LineEnding +
    '  TStringArray = array of AnsiString;' + LineEnding +
    '  TVarRec = record' + LineEnding +
    '    VType: Byte;' + LineEnding +
    '    VInteger: Longint;' + LineEnding +
    '  end;' + LineEnding +
    '  TObject = class' + LineEnding +
    '    constructor Create;' + LineEnding +
    '    destructor Destroy; virtual;' + LineEnding +
    '    procedure Free;' + LineEnding +
    '    function ClassName: ShortString;' + LineEnding +
    '    function ClassType: TClass;' + LineEnding +
    '  end;' + LineEnding +
    '  TInterfacedObject = class(TObject, IInterface)' + LineEnding +
    '  end;' + LineEnding +
    'const' + LineEnding +
    '  LineEnding = #10;' + LineEnding +
    'function Pos(const aSub, aStr: AnsiString): Longint;' + LineEnding +
    'procedure FreeMem(aPtr: Pointer);' + LineEnding +
    'procedure GetMem(out aPtr: Pointer; aSize: PtrUInt);' + LineEnding +
    'implementation' + LineEnding +
    'function Pos(const aSub, aStr: AnsiString): Longint;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure FreeMem(aPtr: Pointer);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure GetMem(out aPtr: Pointer; aSize: PtrUInt);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'constructor TObject.Create;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'destructor TObject.Destroy;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure TObject.Free;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TObject.ClassName: ShortString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TObject.ClassType: TClass;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  cSyntheticSysUtilsSource =
    'unit SysUtils;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  Exception = class(TObject)' + LineEnding +
    '  private' + LineEnding +
    '    FMessage: AnsiString;' + LineEnding +
    '  public' + LineEnding +
    '    constructor Create(const aMsg: AnsiString);' + LineEnding +
    '    property Message: AnsiString read FMessage write FMessage;' + LineEnding +
    '  end;' + LineEnding +
    '  EConvertError = class(Exception);' + LineEnding +
    '  TEncoding = class(TObject)' + LineEnding +
    '  end;' + LineEnding +
    '  TDateTime = type Double;' + LineEnding +
    '  TFormatSettings = record' + LineEnding +
    '    DecimalSeparator: AnsiChar;' + LineEnding +
    '  end;' + LineEnding +
    '  TReplaceFlag = (rfReplaceAll, rfIgnoreCase);' + LineEnding +
    '  TReplaceFlags = set of TReplaceFlag;' + LineEnding +
    'const' + LineEnding +
    '  PathDelim = ''/'';' + LineEnding +
    'procedure FreeAndNil(var aObj);' + LineEnding +
    'function Format(const aFmt: AnsiString;' + LineEnding +
    '  const aArgs: array of const): AnsiString;' + LineEnding +
    'function IntToStr(aValue: Int64): AnsiString;' + LineEnding +
    'function StrToInt(const aValue: AnsiString): Longint;' + LineEnding +
    'function StrToIntDef(const aValue: AnsiString;' + LineEnding +
    '  aDefault: Longint): Longint;' + LineEnding +
    'function IntToHex(aValue: Int64; aDigits: Longint): AnsiString;' + LineEnding +
    'function LowerCase(const aValue: AnsiString): AnsiString;' + LineEnding +
    'function UpperCase(const aValue: AnsiString): AnsiString;' + LineEnding +
    'function Trim(const aValue: AnsiString): AnsiString;' + LineEnding +
    'function CompareText(const a, b: AnsiString): Longint;' + LineEnding +
    'function SameText(const a, b: AnsiString): Boolean;' + LineEnding +
    'function UTF8Encode(const aValue: AnsiString): UTF8String;' + LineEnding +
    'function Now: TDateTime;' + LineEnding +
    'function FormatDateTime(const aFmt: AnsiString;' + LineEnding +
    '  aValue: TDateTime): AnsiString; overload;' + LineEnding +
    'function FormatDateTime(const aFmt: AnsiString; aValue: TDateTime;' + LineEnding +
    '  const aSettings: TFormatSettings): AnsiString; overload;' + LineEnding +
    'function TrimLeft(const aValue: AnsiString): AnsiString;' + LineEnding +
    'function TrimRight(const aValue: AnsiString): AnsiString;' + LineEnding +
    'function CompareStr(const a, b: AnsiString): Longint;' + LineEnding +
    'function ExtractFilePath(const aFileName: AnsiString): AnsiString;' + LineEnding +
    'function ExtractFileName(const aFileName: AnsiString): AnsiString;' + LineEnding +
    'function IncludeTrailingPathDelimiter(const aPath: AnsiString): AnsiString;' +
    LineEnding +
    'function StringReplace(const aSource, aOld, aNew: AnsiString;' + LineEnding +
    '  aFlags: TReplaceFlags): AnsiString;' + LineEnding +
    'implementation' + LineEnding +
    'constructor Exception.Create(const aMsg: AnsiString);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure FreeAndNil(var aObj);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function Format(const aFmt: AnsiString;' + LineEnding +
    '  const aArgs: array of const): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function IntToStr(aValue: Int64): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function StrToInt(const aValue: AnsiString): Longint;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function StrToIntDef(const aValue: AnsiString;' + LineEnding +
    '  aDefault: Longint): Longint;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function IntToHex(aValue: Int64; aDigits: Longint): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function LowerCase(const aValue: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function UpperCase(const aValue: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function Trim(const aValue: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function CompareText(const a, b: AnsiString): Longint;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function SameText(const a, b: AnsiString): Boolean;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function UTF8Encode(const aValue: AnsiString): UTF8String;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function Now: TDateTime;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function FormatDateTime(const aFmt: AnsiString;' + LineEnding +
    '  aValue: TDateTime): AnsiString; overload;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function FormatDateTime(const aFmt: AnsiString; aValue: TDateTime;' + LineEnding +
    '  const aSettings: TFormatSettings): AnsiString; overload;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TrimLeft(const aValue: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TrimRight(const aValue: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function CompareStr(const a, b: AnsiString): Longint;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function ExtractFilePath(const aFileName: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function ExtractFileName(const aFileName: AnsiString): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function IncludeTrailingPathDelimiter(const aPath: AnsiString): AnsiString;' +
    LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function StringReplace(const aSource, aOld, aNew: AnsiString;' + LineEnding +
    '  aFlags: TReplaceFlags): AnsiString;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  { Synthetic Classes stub }
  cSyntheticClassesSource =
    'unit Classes;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses SysUtils;' + LineEnding +
    'type' + LineEnding +
    '  TDuplicates = (dupIgnore, dupError, dupAccept);' + LineEnding +
    '  TPersistent = class(TObject)' + LineEnding +
    '  end;' + LineEnding +
    '  TStream = class(TObject)' + LineEnding +
    '  end;' + LineEnding +
    '  TFPList = class(TObject)' + LineEnding +
    '  private' + LineEnding +
    '    function GetCount: Integer;' + LineEnding +
    '    function GetItem(aIndex: Integer): Pointer;' + LineEnding +
    '  public' + LineEnding +
    '    function Add(aItem: Pointer): Integer;' + LineEnding +
    '    procedure Clear;' + LineEnding +
    '    property Count: Integer read GetCount;' + LineEnding +
    '    property Items[aIndex: Integer]: Pointer read GetItem; default;' + LineEnding +
    '  end;' + LineEnding +
    '  THandleStream = class(TStream)' + LineEnding +
    '  end;' + LineEnding +
    '  TFileStream = class(THandleStream)' + LineEnding +
    '  public' + LineEnding +
    '    constructor Create(const aFileName: AnsiString; aMode: Word);' + LineEnding +
    '  end;' + LineEnding +
    '  TList = class(TObject)' + LineEnding +
    '  private' + LineEnding +
    '    function GetCount: Integer;' + LineEnding +
    '    function GetItem(aIndex: Integer): Pointer;' + LineEnding +
    '  public' + LineEnding +
    '    property Count: Integer read GetCount;' + LineEnding +
    '    property Items[aIndex: Integer]: Pointer read GetItem; default;' + LineEnding +
    '    function Last: Pointer;' + LineEnding +
    '  end;' + LineEnding +
    '  TStrings = class(TPersistent)' + LineEnding +
    '  public' + LineEnding +
    '    procedure LoadFromFile(const aFileName: AnsiString); overload;' + LineEnding +
    '    procedure LoadFromFile(const aFileName: AnsiString;' + LineEnding +
    '      aEncoding: TEncoding); overload;' + LineEnding +
    '    function Add(const aValue: AnsiString): Integer;' + LineEnding +
    '    procedure Clear;' + LineEnding +
    '  end;' + LineEnding +
    '  TStringList = class(TStrings)' + LineEnding +
    '  private' + LineEnding +
    '    FDuplicates: TDuplicates;' + LineEnding +
    '    FSorted: Boolean;' + LineEnding +
    '    FCaseSensitive: Boolean;' + LineEnding +
    '  public' + LineEnding +
    '    property Duplicates: TDuplicates read FDuplicates write FDuplicates;' +
    LineEnding +
    '    property Sorted: Boolean read FSorted write FSorted;' + LineEnding +
    '    property CaseSensitive: Boolean read FCaseSensitive' + LineEnding +
    '      write FCaseSensitive;' + LineEnding +
    '  end;' + LineEnding +
    '  TComponent = class(TPersistent)' + LineEnding +
    '  end;' + LineEnding +
    'const' + LineEnding +
    '  fmOpenRead = 0;' + LineEnding +
    'implementation' + LineEnding +
    'procedure TStrings.LoadFromFile(const aFileName: AnsiString);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure TStrings.LoadFromFile(const aFileName: AnsiString;' + LineEnding +
    '  aEncoding: TEncoding);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TStrings.Add(const aValue: AnsiString): Integer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure TStrings.Clear;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'constructor TFileStream.Create(const aFileName: AnsiString;' + LineEnding +
    '  aMode: Word);' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TFPList.GetCount: Integer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TFPList.GetItem(aIndex: Integer): Pointer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TFPList.Add(aItem: Pointer): Integer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure TFPList.Clear;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TList.GetCount: Integer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TList.GetItem(aIndex: Integer): Pointer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TList.Last: Pointer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  { Synthetic Math stub }
  cSyntheticMathSource =
    'unit Math;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TValueSign = -1..1;' + LineEnding +
    'function IfThen(aCondition: Boolean; const aTrue: Integer;' + LineEnding +
    '  const aFalse: Integer): Integer;' + LineEnding +
    'function Sqrt(aValue: Single): Single; overload;' + LineEnding +
    'function Sqrt(aValue: Double): Double; overload;' + LineEnding +
    'implementation' + LineEnding +
    'function IfThen(aCondition: Boolean; const aTrue: Integer;' + LineEnding +
    '  const aFalse: Integer): Integer;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function Sqrt(aValue: Single): Single; overload;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function Sqrt(aValue: Double): Double; overload;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  { Synthetic Types stub  }
  cSyntheticTypesSource =
    'unit Types;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TPoint = record' + LineEnding +
    '    x: Longint;' + LineEnding +
    '    y: Longint;' + LineEnding +
    '  end;' + LineEnding +
    '  TSize = record' + LineEnding +
    '    cx: Longint;' + LineEnding +
    '    cy: Longint;' + LineEnding +
    '  end;' + LineEnding +
    '  TRect = record' + LineEnding +
    '    Left: Longint;' + LineEnding +
    '    Top: Longint;' + LineEnding +
    '    Right: Longint;' + LineEnding +
    '    Bottom: Longint;' + LineEnding +
    '  end;' + LineEnding +
    '  TIntegerDynArray = array of Longint;' + LineEnding +
    '  TStringDynArray = array of AnsiString;' + LineEnding +
    'implementation' + LineEnding +
    'end.' + LineEnding;

  { Synthetic contnrs stub }
  cSyntheticContnrsSource =
    'unit contnrs;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'type' + LineEnding +
    '  TFPObjectList = class(TObject)' + LineEnding +
    '  end;' + LineEnding +
    '  TFPHashList = class(TObject)' + LineEnding +
    '  end;' + LineEnding +
    '  TObjectList = class(TFPObjectList)' + LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'end.' + LineEnding;

  { Synthetic regexpr stub }
  cSyntheticRegexprSource =
    'unit regexpr;' + LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    'interface' + LineEnding +
    'uses SysUtils;' + LineEnding +
    'type' + LineEnding +
    '  ERegExpr = class(Exception);' + LineEnding +
    '  TRegExpr = class(TObject)' + LineEnding +
    '  private' + LineEnding +
    '    FExpression: AnsiString;' + LineEnding +
    '  public' + LineEnding +
    '    constructor Create;' + LineEnding +
    '    procedure Compile;' + LineEnding +
    '    function Exec(const aInput: AnsiString): Boolean;' + LineEnding +
    '    property Expression: AnsiString read FExpression write FExpression;' +
    LineEnding +
    '  end;' + LineEnding +
    'implementation' + LineEnding +
    'constructor TRegExpr.Create;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'procedure TRegExpr.Compile;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'function TRegExpr.Exec(const aInput: AnsiString): Boolean;' + LineEnding +
    'begin' + LineEnding +
    'end;' + LineEnding +
    'end.' + LineEnding;

  { TFpSonarResolverEngine }

function TFpSonarResolverEngine.FindUnit(const AName, InFilename: string;
  NameExpr, InFileExpr: TPasExpr): TPasModule;
begin
  // Cross-unit: delegate to the wrapper (tolerant — nil degrades, never raises).
  Result := FOwner.ResolveDependency(AName);
end;


function TFpSonarResolverEngine.TryEvalDeclared(const aArgument: string;
  aGenArity: integer; out aValue: string): boolean;
var
  lEl: TPasElement;
begin
  aValue := '0';
  try
    { FindElement/FindElementFor query the live scope the parser has built to this point.
     A generic arity selects the arity-aware lookup; otherwise the plain one. }
    if aGenArity >= 0 then
      lEl := FindElementFor(aArgument, nil, aGenArity)
    else
      lEl := FindElement(aArgument);
    if lEl <> nil then
      aValue := '1';
    Result := True;
  except
    { A definite identifier-not-found IS the confident answer }
    on E: EPasResolve do
      Result := True;
    // Any other resolver failure is NOT a confident answer.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolverEngine.TryEvalSizeOf(const aArgument: string;
  out aValue: string): boolean;
var
  lEl: TPasElement;
  lSize: integer;
begin
  aValue := '0';
  Result := False;
  try
    lEl := FindElement(aArgument);
    if (lEl = nil) or not (lEl is TPasType) then
      Exit; // not a type
    // Reuse the const-eval size helper: target-correct bytes, or -1 when the type cannot be sized
    lSize := TypeByteSize(TPasType(lEl));
    if lSize > 0 then
    begin
      aValue := IntToStr(lSize);
      Result := True;
    end;
  except
    // Not-found or any resolver failure -> cannot decide
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolverEngine.EvalCondDirective(const aFuncName,
  aArgument: string; aGenArity: integer; out aValue: string): boolean;
begin
  aValue := '0';
  if aArgument = '' then
    Exit(False); // no operand -> cannot decide
  if aFuncName = 'declared' then
    Result := TryEvalDeclared(aArgument, aGenArity, aValue)
  else if aFuncName = 'sizeof' then
    Result := TryEvalSizeOf(aArgument, aValue)
  else
    Result := False; // any other cond-function
end;


{ TFpSonarResolver }

constructor TFpSonarResolver.Create;
begin
  inherited Create;
  FHub := TPasResolverHub.Create(Self);
  FEngines := TFPList.Create;
  FParses := TFPList.Create;
  FCacheNames := TStringList.Create;
  FCacheNames.CaseSensitive := False;
  FFailed := TStringList.Create;
  FFailed.CaseSensitive := False;
  FInProgress := TStringList.Create;
  FInProgress.CaseSensitive := False;
  FSynthFiles := TStringList.Create;
  FSynthFiles.CaseSensitive := False;
  FDependencyInterfaceOnly := False;
  FPpuAutoDetect := False;
  FPpuCacheDir := '';
  FIntrinsicConstEval := False;
  FIntrinsicTargetPointerSize := SizeOf(Pointer);
  FRealRtlPreferred := False;
  FDialect := dlDefault;
  FCondDirectiveEval := False;
end;


procedure TFpSonarResolver.ClearBuild;
var
  i: integer;
begin
  { each parser's destructor nils its engine's CurrentParser while the engine
    is still alive — never the reverse, or the parser could use a freed engine. }
  for i := 0 to FParses.Count - 1 do
    TFpSonarResolvedParse(FParses[i]).Free;
  FParses.Clear;
  // Each engine frees its resolved tree.
  for i := 0 to FEngines.Count - 1 do
  begin
    TFpSonarResolverEngine(FEngines[i]).ExprEvaluator.OnRangeCheckEl :=
      TFpSonarResolverEngine(FEngines[i]).FSavedRangeCheckEl;
    TFpSonarResolverEngine(FEngines[i]).Free;
  end;
  FEngines.Clear;
  FTopEngine := nil;
  FCacheNames.Clear;
  FFailed.Clear;
  FInProgress.Clear;
  // The hub survives across builds;
  FHub.Reset;
end;


destructor TFpSonarResolver.Destroy;
var
  i: integer;
  lPath: string;
begin
  ClearBuild;
  // The shared hub is freed only after every engine that referenced it is gone.
  FHub.Free;
  FEngines.Free;
  FParses.Free;
  FCacheNames.Free;
  FFailed.Free;
  FInProgress.Free;
  // Remove synthetic-unit temp file.
  for i := 0 to FSynthFiles.Count - 1 do
  begin
    lPath := FSynthFiles.ValueFromIndex[i];
    if (lPath <> '') and FileExists(lPath) then
      DeleteFile(lPath);
  end;
  FSynthFiles.Free;
  inherited Destroy;
end;


function TFpSonarResolver.GetResolvedModule: TPasModule;
begin
  if (FTopEngine <> nil) and FSucceeded then
    Result := FTopEngine.RootElement
  else
    Result := nil;
end;


function TFpSonarResolver.NewEngine: TFpSonarResolverEngine;
begin
  Result := TFpSonarResolverEngine.Create;
  Result.FOwner := Self;
  // Share the hub before adding builtins
  Result.Hub := FHub;
  // Without this even Longint/Boolean/Char are unresolved identifiers.
  Result.AddObjFPCBuiltInIdentifiers;
  Result.FSavedRangeCheckEl := Result.ExprEvaluator.OnRangeCheckEl;
  Result.ExprEvaluator.OnRangeCheckEl := nil;
  Result.TargetPointerSize := FIntrinsicTargetPointerSize;
  Result.StoreSrcColumns := True;
  FEngines.Add(Result);
end;


function TFpSonarResolver.ImplicitUsesFor(const aUnitName: string): TFpSonarStringArray;
const
  cObjPasUnit = 'objpas';
begin
  SetLength(Result, 0);
  { objpas rides the implicit uses-chain under the real-RTL
    mode — for every unit except objpas itself and System.
    The unit-under-analysis (aUnitName='') gets it too.
    Off => empty, leaving the stock implicit 'System' alone. }
  if FRealRtlPreferred
    and not SameText(aUnitName, cObjPasUnit)
    and not SameText(aUnitName, 'System') then
  begin
    SetLength(Result, 1);
    Result[0] := cObjPasUnit;
  end;
end;


function TFpSonarResolver.CondEvalQueryFor(
  aEngine: TFpSonarResolverEngine): TFpSonarCondEvalQuery;
begin
  if FCondDirectiveEval then
    Result := @aEngine.EvalCondDirective
  else
    Result := nil;
end;


function TFpSonarResolver.LocateUnit(const aName: string): string;

  // Tries <stem>.pas then <stem>.pp in aDir; '' if neither exists.
  function TryStem(const aDir, aStem: string): string;
  begin
    Result := aDir + aStem + '.pas';
    if FileExists(Result) then
      Exit;
    Result := aDir + aStem + '.pp';
    if FileExists(Result) then
      Exit;
    Result := '';
  end;

var
  lLow, lDir: string;
  i: integer;
begin
  Result := '';
  { FPC accepts both the unit name as-written (e.g. dotted/mixed-case
    'FpSonar.Types' on a case-sensitive filesystem) and the all-lowercase
    form. Try as-written first, then lowercase. }
  lLow := LowerCase(aName);
  for i := Low(FUnitPaths) to High(FUnitPaths) do
  begin
    if FUnitPaths[i] = '' then
      Continue;
    lDir := IncludeTrailingPathDelimiter(FUnitPaths[i]);
    Result := TryStem(lDir, aName);
    if Result <> '' then
      Exit;
    if lLow <> aName then
    begin
      Result := TryStem(lDir, lLow);
      if Result <> '' then
        Exit;
    end;
  end;
  Result := '';
end;


{ Auto-detect ppudump generator: a global singleton
  A resolver is created per analyzed file, but the .ppu index, the dumped-ppu
  cache and the generated stub files must survive across all files }
var
  GPpuGen: TFpSonarPpuStubGen = nil;
  GPpuFiles: TStringList = nil;

const
  cPpuCachePrefix = 'fps1-';


{ A cheap content-change fingerprint for a .ppu }
function PpuFingerprint(const aPath: string): string;
var
  lInfo: TSearchRec;
begin
  Result := '';
  if FindFirst(aPath, faAnyFile, lInfo) = 0 then
  begin
    Result := IntToStr(lInfo.Size) + '-' + IntToStr(lInfo.Time);
    FindClose(lInfo);
  end;
end;


{ Writes aSource to aFile atomically }
function WritePersistentStub(const aFile, aSource: string): string;
var
  lDir, lTmp: string;
  lStream: TFileStream;
begin
  Result := '';
  lDir := ExtractFileDir(aFile);
  if (lDir <> '') and not ForceDirectories(lDir) then
    Exit;
  lTmp := '';
  try
    lTmp := GetTempFileName(lDir, 'fpswr');
    lStream := TFileStream.Create(lTmp, fmCreate);
    try
      if Length(aSource) > 0 then
        lStream.WriteBuffer(aSource[1], Length(aSource));
    finally
      lStream.Free;
    end;
    // Atomic publish. rename(2) overwrites.
    if RenameFile(lTmp, aFile) then
      Result := aFile
    else if FileExists(aFile) then
    begin
      DeleteFile(lTmp);
      Result := aFile;
    end
    else
      DeleteFile(lTmp);
  except
    on E: Exception do
    begin
      if (lTmp <> '') and FileExists(lTmp) then
        DeleteFile(lTmp);
      Result := '';
    end;
  end;
end;


{ The host FPC .ppu unit dir (<prefix>/lib/fpc/<ver>/units/<cpu>-<os>) for the
  one version that matches this toolchain, derived from the `ppudump` location +
  the build-host version/target macros. Empty when ppudump is not found or the
  version dir is absent }
function DiscoverPpuUnitDirs: TFpSonarStringArray;
var
  lPd, lPrefix, lTarget, lVer, lCand: string;
begin
  SetLength(Result, 0);
  lPd := FindPpudump;
  if lPd = '' then
    Exit;
  // /usr/local/bin/ppudump -> /usr/local
  lPrefix := ExtractFileDir(ExtractFileDir(lPd));
  // FPC unit dirs are lowercase (units/x86_64-linux); %FPCTARGETOS% is 'Linux'.
  lTarget := LowerCase({$I %FPCTARGETCPU%} + '-' + {$I %FPCTARGETOS%});
  lVer := {$I %FPCVERSION%};
  lCand := IncludeTrailingPathDelimiter(lPrefix) + 'lib' + PathDelim + 'fpc' +
    PathDelim + lVer + PathDelim + 'units' + PathDelim + lTarget;
  if DirectoryExists(lCand) then
  begin
    SetLength(Result, 1);
    Result[0] := lCand;
  end;
end;


// Lazily builds the global generator, indexing the host .ppu unit dirs (once)
// plus the analysed project's own -Fu paths (its compiled output may hold .ppu).
procedure EnsureGlobalPpuGen(const aUnitPaths: array of string);
var
  lDirs: TFpSonarStringArray;
  i: integer;
begin
  if GPpuGen = nil then
  begin
    GPpuGen := TFpSonarPpuStubGen.Create;
    GPpuFiles := TStringList.Create;
    lDirs := DiscoverPpuUnitDirs;
    for i := 0 to High(lDirs) do
      GPpuGen.AddSearchDir(lDirs[i]);
  end;
  for i := 0 to High(aUnitPaths) do
    GPpuGen.AddSearchDir(aUnitPaths[i]);
end;


// Materialises aSource to a temp file, cached by aKey (generated once); '' on
// a write failure (=> the caller degrades to synthetic). Tracked for deletion.
function MaterializeAutoStub(const aKey, aSource: string): string;
var
  lIdx: integer;
  lStream: TFileStream;
begin
  Result := '';
  lIdx := GPpuFiles.IndexOfName(aKey);
  if lIdx >= 0 then
    Exit(GPpuFiles.ValueFromIndex[lIdx]);
  try
    Result := GetTempFileName('', 'fpsppu');
    lStream := TFileStream.Create(Result, fmCreate);
    try
      if Length(aSource) > 0 then
        lStream.WriteBuffer(aSource[1], Length(aSource));
    finally
      lStream.Free;
    end;
    GPpuFiles.Add(aKey + '=' + Result);
  except
    on E: Exception do
    begin
      if (Result <> '') and FileExists(Result) then
        DeleteFile(Result);
      Result := '';
    end;
  end;
end;


function TFpSonarResolver.AutoDetectFile(const aName: string): string;
var
  lSyn, lHyb, lKey, lCanon, lCrc, lSrc, lPpuPath, lFp, lCacheFile: string;
  lIdx: integer;
begin
  Result := '';
  if not FPpuAutoDetect then
    Exit;
  EnsureGlobalPpuGen(FUnitPaths);
  lSyn := SyntheticSource(aName);
  if lSyn <> '' then
  begin
    { A synthetic RTL name }
    lHyb := HybridRtlSource(aName, lSyn);
    if lHyb = '' then
      Exit;
    Result := MaterializeAutoStub('hyb:' + LowerCase(aName), lHyb);
    Exit;
  end;
  { A non-synthetic dependency }
  lPpuPath := GPpuGen.LocatePpu(aName);
  if lPpuPath = '' then
    Exit;
  { Persistent --ppu-cache hit?  }
  lCacheFile := '';
  if FPpuCacheDir <> '' then
  begin
    lFp := PpuFingerprint(lPpuPath);
    if lFp <> '' then
    begin
      lCacheFile := IncludeTrailingPathDelimiter(FPpuCacheDir) +
        cPpuCachePrefix + LowerCase(aName) + '-' + lFp + '.stub';
      if FileExists(lCacheFile) then
        Exit(lCacheFile);
    end;
  end;
  // MISS: generation needs ppudump.
  if not GPpuGen.PpudumpAvailable then
    Exit;
  lCrc := GPpuGen.InterfaceCrc(aName);
  lKey := 'ppu:' + LowerCase(aName) + '#' + lCrc;
  lIdx := GPpuFiles.IndexOfName(lKey);
  if lIdx >= 0 then
    Exit(GPpuFiles.ValueFromIndex[lIdx]);
  if GPpuGen.GenerateUnit(aName, lCanon, lCrc, lSrc) then
  begin
    // Persist across runs when --ppu-cache is usable; else a per-run temp file.
    if lCacheFile <> '' then
      Result := WritePersistentStub(lCacheFile, lSrc);
    if Result = '' then
      Result := MaterializeAutoStub(lKey, lSrc);
  end;
end;


function TFpSonarResolver.SyntheticSource(const aName: string): string;
begin
  // The synthetic-unit registry: name -> embedded source.
  if SameText(aName, 'System') then
    Result := cSyntheticSystemSource
  else if SameText(aName, 'SysUtils') then
    Result := cSyntheticSysUtilsSource
  else if SameText(aName, 'Classes') then
    Result := cSyntheticClassesSource
  else if SameText(aName, 'Math') then
    Result := cSyntheticMathSource
  else if SameText(aName, 'Types') then
    Result := cSyntheticTypesSource
  else if SameText(aName, 'contnrs') then
    Result := cSyntheticContnrsSource
  else if SameText(aName, 'regexpr') then
    Result := cSyntheticRegexprSource
  else
    Result := '';
end;


function TFpSonarResolver.SyntheticUnitFile(const aName: string): string;
var
  lSource: string;
  lStream: TFileStream;
  lIdx: integer;
begin
  // Reuse the temp file already written for this name.
  lIdx := FSynthFiles.IndexOfName(aName);
  if lIdx >= 0 then
    Exit(FSynthFiles.ValueFromIndex[lIdx]);
  lSource := SyntheticSource(aName);
  if lSource = '' then
    Exit('');
  Result := '';
  try
    // A unique temp path; the unit NAME comes from the source
    Result := GetTempFileName('', 'fpssyn');
    lStream := TFileStream.Create(Result, fmCreate);
    try
      lStream.WriteBuffer(lSource[1], Length(lSource));
    finally
      lStream.Free;
    end;
    FSynthFiles.Values[aName] := Result;
  except
    { Could not materialise — clean up }
    on E: Exception do
    begin
      if (Result <> '') and FileExists(Result) then
        DeleteFile(Result);
      Result := '';
    end;
  end;
end;


function TFpSonarResolver.OrderedCandidates(const aName: string)
: TFpSonarStringArray;

  procedure AddNonEmpty(const aFile: string);
  begin
    if aFile = '' then
      Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := aFile;
  end;

begin
  SetLength(Result, 0);
  if SyntheticSource(aName) <> '' then
    AddNonEmpty(AutoDetectFile(aName));
  if FRealRtlPreferred then
  begin
    AddNonEmpty(LocateUnit(aName));
    AddNonEmpty(SyntheticUnitFile(aName));
  end
  else
  begin
    AddNonEmpty(SyntheticUnitFile(aName));
    if Length(Result) = 0 then
      AddNonEmpty(LocateUnit(aName));
  end;
  { A NON-synthetic dependency's ppudump stub is the LAST resort — used ONLY when
    no synthetic stub and no real source resolved a candidate }
  if (Length(Result) = 0) and (SyntheticSource(aName) = '') then
    AddNonEmpty(AutoDetectFile(aName));
end;


function TFpSonarResolver.TryResolveModule(const aFile, aDepName: string)
: TPasModule;
var
  lEngine: TFpSonarResolverEngine;
  lModule: TPasModule;
  lDiag: TFpSonarDiagnostic;
begin
  Result := nil;
  lEngine := NewEngine;
  try
    { Opt-in: a dependency resolves from its interface only}
    if FDependencyInterfaceOnly then
      lEngine.InterfaceOnly := True;
    // Re-entrant: this runs a nested resolved parse while the outer parse is suspended mid-uses-clause.
    if ParseResolvedKeepAlive(lEngine, aFile, FMode, FDefines, FIncludePaths,
      ImplicitUsesFor(aDepName), CondEvalQueryFor(lEngine), FDialect, FParses,
      lModule, lDiag) and (lModule <> nil) then
      Result := lModule;
  except
    on E: Exception do
      Result := nil;
  end;
end;


function TFpSonarResolver.ResolveDependency(const aName: string): TPasModule;
var
  lIdx, i: integer;
  lCandidates: TFpSonarStringArray;
begin
  Result := nil;
  // Resolve-once cache.
  lIdx := FCacheNames.IndexOf(aName);
  if lIdx >= 0 then
  begin
    Result := TPasModule(FCacheNames.Objects[lIdx]);
    Exit;
  end;
  // A dependency that already failed: degrade without re-parsing.
  if FFailed.IndexOf(aName) >= 0 then
    Exit;
  // Cycle guard: a unit currently being resolved (circular uses) -> nil.
  if FInProgress.IndexOf(aName) >= 0 then
    Exit;

  FInProgress.Add(aName);
  try
    // Try each candidate source in resolution-order until one resolves.
    lCandidates := OrderedCandidates(aName);
    for i := 0 to High(lCandidates) do
      if Result = nil then
        Result := TryResolveModule(lCandidates[i], aName);

    if Result <> nil then
      FCacheNames.AddObject(aName, Result)
    else if FFailed.IndexOf(aName) < 0 then
      FFailed.Add(aName);
  finally
    lIdx := FInProgress.IndexOf(aName);
    if lIdx >= 0 then
      FInProgress.Delete(lIdx);
  end;
end;


function TFpSonarResolver.BuildFor(const aFileName, aCompilerMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string;
  out aDiag: TFpSonarDiagnostic): boolean;
var
  lModule: TPasModule;
  i: integer;
begin
  // Fresh per build so a reused wrapper never carries a prior tree/cache.
  ClearBuild;
  FSucceeded := False;

  // Carry the config so FindUnit (cross-unit) can use it.
  FMode := aCompilerMode;
  SetLength(FDefines, Length(aDefines));
  for i := Low(aDefines) to High(aDefines) do
    FDefines[i] := aDefines[i];
  // Real-RTL target CPU auto-defines. fcl-passrc's own ParseSource adds
  // CPU<target>/CPU64 for the compile target;
  if FRealRtlPreferred and (FIntrinsicTargetPointerSize = 8) then
  begin
    i := Length(FDefines);
    SetLength(FDefines, i + 3);
    FDefines[i] := 'CPU64';
    FDefines[i + 1] := 'CPUAMD64';
    FDefines[i + 2] := 'CPUX86_64';
  end;
  SetLength(FUnitPaths, Length(aUnitPaths));
  for i := Low(aUnitPaths) to High(aUnitPaths) do
    FUnitPaths[i] := aUnitPaths[i];
  SetLength(FIncludePaths, Length(aIncludePaths));
  for i := Low(aIncludePaths) to High(aIncludePaths) do
    FIncludePaths[i] := aIncludePaths[i];

  // Default the diagnostic to a resolve failure anchored at the file; the
  // success / syntax-error paths overwrite it below.
  aDiag.FileName := aFileName;
  aDiag.Row := 0;
  aDiag.Col := 0;
  aDiag.Kind := dkResolveError;
  aDiag.Message := '';

  FTopEngine := NewEngine;
  try
    { The adapter folds a SYNTAX error into aDiag (dkParseError) and returns False }
    Result := ParseResolvedKeepAlive(FTopEngine, aFileName, FMode, FDefines,
      FIncludePaths, ImplicitUsesFor(''), CondEvalQueryFor(FTopEngine), FDialect,
      FParses, lModule, aDiag);
  except
    on E: EPasResolve do
    begin
      aDiag.FileName := E.SourcePos.FileName;
      aDiag.Row := E.SourcePos.Row;
      aDiag.Col := E.SourcePos.Column;
      aDiag.Kind := dkResolveError;
      aDiag.Message := E.Message;
      Result := False;
    end;
    // Any other failure (IO, internal) is a resolve-build failure with no typed position.
    on E: Exception do
    begin
      aDiag.FileName := aFileName;
      aDiag.Row := 0;
      aDiag.Col := 0;
      aDiag.Kind := dkResolveError;
      aDiag.Message := E.Message;
      Result := False;
    end;
  end;

  // Success requires both a clean parse+resolve AND a resolved root module.
  FSucceeded := Result and (FTopEngine.RootElement <> nil);
  Result := FSucceeded;
end;


{ The FpSonar-owned classification of a resolved fact:
  computed from the vendored base-type sets / element classes so the kind never leaks a
  pasresolver enum. }
function ClassifyResolvedKind(const aResolved: TPasResolverResult): TFpSonarTypeKind;
begin
  if aResolved.BaseType in btAllChars then
    Result := ltkChar
  else if aResolved.BaseType in btAllStrings then
    Result := ltkString
  else if aResolved.BaseType in btAllInteger then
    Result := ltkInteger
  else if aResolved.BaseType in btAllFloats then
    Result := ltkFloat
  else if aResolved.BaseType in btAllBooleans then
    Result := ltkBool
  else if (aResolved.BaseType = btPointer) or (aResolved.BaseType = btNil) then
    Result := ltkPointer
  else if aResolved.BaseType = btContext then
  begin
    // A source-declared type: classify by the collapsed element's class.
    if aResolved.LoTypeEl is TPasClassOfType then
      Result := ltkClassRef
    else if aResolved.LoTypeEl is TPasClassType then
    begin
      if TPasClassType(aResolved.LoTypeEl).ObjKind in
        [okInterface, okDispInterface] then
        Result := ltkInterface
      else
        Result := ltkClass;
    end
    else if aResolved.LoTypeEl is TPasPointerType then
      Result := ltkPointer
    else if aResolved.LoTypeEl is TPasArrayType then
      Result := ltkArray
    else if aResolved.LoTypeEl is TPasRecordType then
      Result := ltkRecord
    else if aResolved.LoTypeEl is TPasEnumType then
      Result := ltkEnum
    else
      Result := ltkOther;
  end
  else
    Result := ltkOther;
end;


{ The FpSonar-owned codepage classification of a resolved string/char }
function ClassifyEncoding(const aResolved: TPasResolverResult;
  aBaseTypeString, aBaseTypeChar: TResolverBaseType): TFpSonarStrEncoding;
begin
  case aResolved.BaseType of
    btUnicodeString, btWideString, btWideChar:
      Result := lseWide;
    btAnsiString, btShortString, btRawByteString, btAnsiChar:
      Result := lseAnsi;
    btString:
      if aBaseTypeString = btAnsiString then
        Result := lseAnsi
      else if aBaseTypeString in [btUnicodeString, btWideString] then
        Result := lseWide
      else
        Result := lseNone;
    btChar:
      if aBaseTypeChar = btAnsiChar then
        Result := lseAnsi
      else if aBaseTypeChar = btWideChar then
        Result := lseWide
      else
        Result := lseNone;
    else
      Result := lseNone;
  end;
end;


{ The FpSonar-owned integer-portability classification of a resolved fact: }
function ClassifyIntWidth(const aResolved: TPasResolverResult;
  const aNamedName: string): TFpSonarIntWidth;
var
  i: integer;
begin
  Result := liwNone;
  if not (aResolved.BaseType in btAllInteger) then
    Exit;
  // Name check FIRST: portability is the named alias, robust to the host build.
  for i := Low(allowedPtrIntTypes) to High(allowedPtrIntTypes) do
    if SameText(aNamedName, allowedPtrIntTypes[i]) then
    begin
      Result := liwPointerSized;
      Exit;
    end;
  case aResolved.BaseType of
    btInt64, btQWord, btComp, btIntDouble, btUIntDouble:
      Result := liwWide;
    btLongint, btLongWord, btSmallInt, btWord, btShortInt, btByte,
    btIntSingle, btUIntSingle:
      Result := liwFixed;
    else
      Result := liwNone;
  end;
end;


// The unresolved sentinel — no concrete type fact (degrade-to-silence).
procedure ClearResolvedType(out aType: TFpSonarResolvedType);
begin
  aType.TypeEl := nil;
  aType.TypeName := '';
  aType.Kind := ltkOther;
  aType.NamedTypeName := '';
  aType.Encoding := lseNone;
  aType.IntWidth := liwNone;
end;


{ Fills aType from a computed resolver result, or leaves the unresolved sentinel
  when there is no concrete type element or the base type is the none/untyped sentinel. }
function FillResolvedType(const aResolved: TPasResolverResult;
  aBaseTypeString, aBaseTypeChar: TResolverBaseType;
  out aType: TFpSonarResolvedType): boolean;
begin
  ClearResolvedType(aType);
  Result := False;
  if (aResolved.LoTypeEl = nil)
    or (aResolved.BaseType = btNone)
    or (aResolved.BaseType = btUntyped) then
    Exit;
  aType.TypeEl := aResolved.LoTypeEl;
  aType.TypeName := aResolved.LoTypeEl.Name;
  aType.Kind := ClassifyResolvedKind(aResolved);
  aType.Encoding := ClassifyEncoding(aResolved, aBaseTypeString, aBaseTypeChar);
  // HiTypeEl preserves the written alias name; fall back to the collapsed name.
  if aResolved.HiTypeEl <> nil then
    aType.NamedTypeName := aResolved.HiTypeEl.Name
  else
    aType.NamedTypeName := aResolved.LoTypeEl.Name;
  aType.IntWidth := ClassifyIntWidth(aResolved, aType.NamedTypeName);
  Result := True;
end;


function TFpSonarResolver.TryResolvedType(aElement: TPasElement;
  out aType: TFpSonarResolvedType): boolean;
var
  lResolved: TPasResolverResult;
begin
  ClearResolvedType(aType);
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aElement = nil) then
    Exit;

  try
    FTopEngine.ComputeElement(aElement, lResolved, []);
  except
    on E: Exception do
      Exit;
  end;

  Result := FillResolvedType(lResolved, FTopEngine.BaseTypeString,
    FTopEngine.BaseTypeChar, aType);
end;


function TFpSonarResolver.TryTypeByteSize(aType: TPasType;
  out aSize: integer): boolean;
var
  lSize: integer;
begin
  aSize := 0;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aType = nil) then
    Exit;
  try
    lSize := FTopEngine.TypeByteSize(aType);
    if lSize > 0 then
    begin
      aSize := lSize;
      Result := True;
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryTypecast(aCandidate: TPasElement;
  out aTarget, aSource: TFpSonarResolvedType): boolean;
var
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lTargetRes, lSrcRes: TPasResolverResult;
begin
  // Both sentinels first (unresolved leaves them so the caller can read either).
  ClearResolvedType(aTarget);
  ClearResolvedType(aSource);
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aCandidate = nil) then
    Exit;
  // A typecast is the call-shaped node with exactly one argument.
  if not (aCandidate is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aCandidate);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) <> 1) then
    Exit;

  try
    { Generic typecast detection }
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or (lRef.Declaration = nil)
      or not (lRef.Declaration is TPasType) then
      Exit;
    { Reject a built-in proc callee: fcl-passrc represents intrinsics like
      Low/High/SizeOf/Ord as a TPasUnresolvedSymbolRef }
    if lRef.Declaration.CustomData is TResElDataBuiltInProc then
      Exit;
    if not (FTopEngine.ResolveAliasType(TPasType(lRef.Declaration)) is TPasType) then
      Exit;
    // We have a cast: the node itself computes to the result type
    FTopEngine.ComputeElement(lParams, lTargetRes, []);
    FTopEngine.ComputeElement(lParams.Params[0], lSrcRes, []);
  except
    // Tolerant-access: any resolver failure => not a usable typecast fact.
    on E: Exception do
      Exit;
  end;

  if not FillResolvedType(lTargetRes,
    FTopEngine.BaseTypeString, FTopEngine.BaseTypeChar, aTarget) then
    Exit;
  if not FillResolvedType(lSrcRes,
    FTopEngine.BaseTypeString, FTopEngine.BaseTypeChar, aSource) then
    Exit;
  Result := True;
end;


function TFpSonarResolver.TryClassRelation(aClassA, aClassB: TPasType;
  out aRelated: boolean): boolean;

  { Walks aChild's ancestor chain looking for aAncestor }
  function AncestorWalk(aChild, aAncestor: TPasType;
    out aFound: boolean): boolean;
  var
    lCls: TPasClassType;
    lNext: TPasType;
    lGuard: integer;
  begin
    aFound := False;
    Result := True;
    if not (aChild is TPasClassType) then
      Exit(False);
    lCls := TPasClassType(aChild);
    lGuard := 0;
    while (lCls <> nil) and (lGuard < 100) do
    begin
      if lCls = aAncestor then
      begin
        aFound := True;
        Exit(True);
      end;
      lNext := lCls.AncestorType;
      if lNext = nil then
        Exit(True);                  // reached the root cleanly
      if not (lNext is TPasClassType) then
        Exit(False);                 // incomplete/forward chain — undeterminable
      lCls := TPasClassType(lNext);
      Inc(lGuard);
    end;
    { Falling out of the loop means the guard cap was hit }
    Result := False;
  end;

var
  lAinB, lBinA: boolean;
begin
  aRelated := False;
  Result := False;
  if (aClassA = nil) or (aClassB = nil)
    or not (aClassA is TPasClassType) or not (aClassB is TPasClassType) then
    Exit;
  if aClassA = aClassB then
  begin
    aRelated := True;
    Result := True;
    Exit;
  end;
  // Either direction must be fully determinable.
  if not AncestorWalk(aClassA, aClassB, lAinB) then
    Exit;
  if not AncestorWalk(aClassB, aClassA, lBinA) then
    Exit;
  aRelated := lAinB or lBinA;
  Result := True;
end;


function TFpSonarResolver.TryFreeCall(aNode: TPasElement;
  out aInner: TPasExpr): TFpSonarFreeKind;

  // The TResolvedReference attached to an expression, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // The enclosing module of aEl (walks Parent), or nil.
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

  { True iff aProc is a method whose owning class is TObject or a descendant of it  }
  function OwnerIsTObjectClass(aProc: TPasProcedure): boolean;
  var
    lCls: TPasClassType;
    lNext: TPasType;
    lGuard: integer;
  begin
    Result := False;
    if (aProc = nil) or not (aProc.Parent is TPasClassType) then
      Exit;
    lCls := TPasClassType(aProc.Parent);
    lGuard := 0;
    while (lCls <> nil) and (lGuard < 100) do
    begin
      if SameText(lCls.Name, 'TObject') then
        Exit(True);
      lNext := lCls.AncestorType;
      if not (lNext is TPasClassType) then
        Exit;
      lCls := TPasClassType(lNext);
      Inc(lGuard);
    end;
  end;

var
  lBin: TBinaryExpr;
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lProc: TPasProcedure;
  lMod: TPasModule;
begin
  aInner := nil;
  Result := lfkNone;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;

  try
    // '<expr>.Free' — a member access; the Right member resolves to TObject.Free.
    if (aNode is TBinaryExpr)
      and (TBinaryExpr(aNode).OpCode = eopSubIdent) then
    begin
      lBin := TBinaryExpr(aNode);
      lRef := RefOf(lBin.Right);
      if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
        Exit;
      lProc := TPasProcedure(lRef.Declaration);
      if SameText(lProc.Name, 'Free') and OwnerIsTObjectClass(lProc) then
      begin
        aInner := lBin.Left;
        Result := lfkFreeMethod;
      end;
      Exit;
    end;

    { `FreeAndNil(<arg>)` — a single-arg call whose callee resolves to the RTL SysUtils.FreeAndNil }
    if (aNode is TParamsExpr)
      and (TParamsExpr(aNode).Kind = pekFuncParams)
      and (Length(TParamsExpr(aNode).Params) = 1) then
    begin
      lParams := TParamsExpr(aNode);
      lRef := FTopEngine.GetParamsValueRef(lParams);
      if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
        Exit;
      lProc := TPasProcedure(lRef.Declaration);
      if not SameText(lProc.Name, 'FreeAndNil') then
        Exit;
      lMod := OwningModule(lProc);
      if (lMod <> nil) and SameText(lMod.Name, 'SysUtils') then
      begin
        aInner := lParams.Params[0];
        Result := lfkFreeAndNil;
      end;
    end;
  except
    on E: Exception do
    begin
      aInner := nil;
      Result := lfkNone;
    end;
  end;
end;


function TFpSonarResolver.TryFormatCall(aNode: TPasElement;
  out aCall: TFpSonarFormatCall): boolean;

  // The enclosing module of aEl (walks Parent), or nil.)
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

var
  lParams, lArgs: TParamsExpr;
  lArg0: TPasExpr;
  lRef: TResolvedReference;
  lProc: TPasProcedure;
  lMod: TPasModule;
  lVal: TResEvalValue;
  i: integer;
begin
  // Unresolved sentinel first (the caller may read sub-facts on a True return).
  aCall.FmtNode := nil;
  aCall.FmtText := '';
  aCall.FmtResolved := False;
  aCall.ArgsInline := False;
  aCall.ArgCount := 0;
  SetLength(aCall.Args, 0);
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  // A Format call is the call-shaped node with exactly two arguments.
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) <> 2) then
    Exit;

  try
    { Callee identity (NOT spelling): the callee must resolve to a TPasProcedure
      named Format/WideFormat/UnicodeFormat owned by the SysUtils module. }
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(lRef.Declaration);
    if not (SameText(lProc.Name, 'Format')
      or SameText(lProc.Name, 'WideFormat')
      or SameText(lProc.Name, 'UnicodeFormat')) then
      Exit;
    lMod := OwningModule(lProc);
    if (lMod = nil) or not SameText(lMod.Name, 'SysUtils') then
      Exit;

    // Confirmed Format call — the rules will read the sub-facts below.
    Result := True;
    lArg0 := lParams.Params[0];
    aCall.FmtNode := lArg0;

    { Const-fold arg[0] via the evaluator, isolated in its own try so a
      runtime-variable / non-const format string only clears. }
    try
      lVal := FTopEngine.ExprEvaluator.Eval(lArg0, [refConst]);
      try
        if (lVal <> nil) and (lVal.Kind in revkAllStrings) then
        begin
          case lVal.Kind of
            {$ifdef FPC_HAS_CPSTRING}
              revkString:
                aCall.FmtText := FTopEngine.ExprEvaluator.GetUTF8Str(
                  TResEvalString(lVal).S, lArg0);
            {$endif}
            revkUnicodeString:
              aCall.FmtText := UTF8Encode(TResEvalUTF16(lVal).S);
          end;
          aCall.FmtResolved := True;
        end;
      finally
        ReleaseEvalValue(lVal);
      end;
    except
      on E: Exception do
      begin
        aCall.FmtResolved := False;
        aCall.FmtText := '';
      end;
    end;

    { Inline-args: arg[1] is the `[...]` }
    if (lParams.Params[1] is TParamsExpr)
      and (TParamsExpr(lParams.Params[1]).Kind = pekSet) then
    begin
      lArgs := TParamsExpr(lParams.Params[1]);
      aCall.ArgsInline := True;
      aCall.ArgCount := Length(lArgs.Params);
      SetLength(aCall.Args, Length(lArgs.Params));
      for i := 0 to High(lArgs.Params) do
        aCall.Args[i] := lArgs.Params[i];
    end;
  except
    // Tolerant-access: any resolver failure => not a usable Format call.
    on E: Exception do
    begin
      aCall.FmtNode := nil;
      aCall.FmtText := '';
      aCall.FmtResolved := False;
      aCall.ArgsInline := False;
      aCall.ArgCount := 0;
      SetLength(aCall.Args, 0);
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.FindInterfaceConst(
  const aConstName: string): TPasConst;
var
  lModule: TPasModule;
  lSection: TPasSection;
  lDecl: TPasElement;
  i: integer;
begin
  Result := nil;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lModule := GetResolvedModule;
  if lModule = nil then
    Exit;
  lSection := lModule.InterfaceSection;
  if lSection = nil then
    Exit;
  for i := 0 to lSection.Declarations.Count - 1 do
  begin
    lDecl := TPasElement(lSection.Declarations[i]);
    if (lDecl is TPasConst) and SameText(lDecl.Name, aConstName) then
      Exit(TPasConst(lDecl));
  end;
end;


function TFpSonarResolver.TryEvalConstInt(const aConstName: string;
  out aValue: int64; out aKnown: boolean): boolean;
var
  lConst: TPasConst;
  lVal: TResEvalValue;
begin
  Result := False;
  aValue := 0;
  aKnown := False;
  lConst := FindInterfaceConst(aConstName);
  if lConst = nil then
    Exit;

  { The const is present in the resolved interface so it resolved }
  Result := True;
  if lConst.Expr = nil then
    Exit;
  try
    { Const-fold the initializer via the evaluator and release the eval value in a finally}
    lVal := FTopEngine.ExprEvaluator.Eval(lConst.Expr, [refConst]);
    try
      if lVal <> nil then
        case lVal.Kind of
          revkInt:
          begin
            aValue := int64(TResEvalInt(lVal).Int);
            aKnown := True;
          end;
          revkUInt:
          begin
            aValue := int64(TResEvalUInt(lVal).UInt);
            aKnown := True;
          end;
        end;
    finally
      ReleaseEvalValue(lVal);
    end;
  except
    on E: Exception do
      aKnown := False;
  end;
end;


function TFpSonarResolver.EvalGuarded(aExpr: TPasExpr): TResEvalValue;

begin
  Result := nil;
  if (FTopEngine = nil) or (not FSucceeded) or (aExpr = nil) then
    Exit;
  try
    Result := FTopEngine.ExprEvaluator.Eval(aExpr, []);
  except
    on E: Exception do
      Result := nil;
  end;
end;


function TFpSonarResolver.TryConstDivisionByZero(aExpr: TPasExpr;
  out aOperator: string): boolean;

var
  lBinary: TBinaryExpr;
  lValue: TResEvalValue;

begin
  Result := False;
  aOperator := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopDivide, eopDiv, eopMod]) then
    Exit;
  lValue := EvalGuarded(lBinary.Right);
  try
    if lValue = nil then
      Exit;
    case lValue.Kind of
      revkInt: Result := TResEvalInt(lValue).Int = 0;
      revkUInt: Result := TResEvalUInt(lValue).UInt = 0;
      revkFloat: Result := TResEvalFloat(lValue).FloatValue = 0;
      revkCurrency: Result := TResEvalCurrency(lValue).Value = 0;
    end;
  finally
    ReleaseEvalValue(lValue);
  end;
  if Result then
    aOperator := OpcodeStrings[lBinary.OpCode];
end;


function TFpSonarResolver.TryConstBooleanValue(aExpr: TPasExpr;
  out aValue: boolean): boolean;

var
  lValue: TResEvalValue;

begin
  Result := False;
  aValue := False;
  lValue := EvalGuarded(aExpr);
  try
    if (lValue <> nil) and (lValue.Kind = revkBool) then
    begin
      aValue := TResEvalBool(lValue).B;
      Result := True;
    end;
  finally
    ReleaseEvalValue(lValue);
  end;
end;


function TFpSonarResolver.TypeRangeGuarded(aType: TPasType): TResEvalValue;

begin
  Result := nil;
  if (FTopEngine = nil) or (not FSucceeded) or (aType = nil) then
    Exit;
  try
    Result := FTopEngine.EvalTypeRange(aType, []);
  except
    on E: Exception do
      Result := nil;
  end;
end;


function TFpSonarResolver.TryInRangeGuarded(aValue: TResEvalValue;
  aValueExpr: TPasExpr; aRange: TResEvalValue; aRangeExpr: TPasExpr;
  out aInRange: boolean): boolean;

begin
  Result := False;
  aInRange := False;
  if (FTopEngine = nil) or (not FSucceeded)
    or (aValue = nil) or (aRange = nil) then
    Exit;
  try
    aInRange := FTopEngine.ExprEvaluator.IsInRange(aValue, aValueExpr, aRange,
      aRangeExpr, False);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.ComputeGuarded(aElement: TPasElement;
  out aResolved: TPasResolverResult): boolean;

begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aElement = nil) then
    Exit;
  try
    FTopEngine.ComputeElement(aElement, aResolved, []);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.BuiltInCall(aExpr: TPasExpr;
  aBuiltIn: TResolverBuiltInProc; aCount: integer;
  out aParams: TParamsExpr): boolean;

var
  lRef: TResolvedReference;

begin
  Result := False;
  aParams := nil;
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TParamsExpr) then
    Exit;
  if (TParamsExpr(aExpr).Kind <> pekFuncParams)
    or (Length(TParamsExpr(aExpr).Params) <> aCount) then
    Exit;
  try
    lRef := FTopEngine.GetParamsValueRef(TParamsExpr(aExpr));
  except
    on E: Exception do
      Exit;
  end;
  if (lRef = nil) or not (lRef.Declaration is TPasUnresolvedSymbolRef)
    or not (lRef.Declaration.CustomData is TResElDataBuiltInProc) then
    Exit;
  if TResElDataBuiltInProc(lRef.Declaration.CustomData).BuiltIn <> aBuiltIn then
    Exit;
  aParams := TParamsExpr(aExpr);
  Result := True;
end;


function TFpSonarResolver.IntegerPropsGuarded(aBaseType: TResolverBaseType;
  out aPrecision: word; out aSigned: boolean): boolean;

begin
  Result := False;
  aPrecision := 0;
  aSigned := False;
  if (FTopEngine = nil) or not (aBaseType in btAllInteger) then
    Exit;
  try
    FTopEngine.GetIntegerProps(aBaseType, aPrecision, aSigned);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryComparisonAlwaysTrueForType(aExpr: TPasExpr;
  out aTypeName, aConstant: string; out aValue: boolean): boolean;

  // aOp read with its two operands swapped.
  function Mirror(aOp: TExprOpCode): TExprOpCode;
  begin
    case aOp of
      eopLessThan: Result := eopGreaterThan;
      eopGreaterThan: Result := eopLessThan;
      eopLessthanEqual: Result := eopGreaterThanEqual;
      eopGreaterThanEqual: Result := eopLessthanEqual;
    else
      Result := aOp;
    end;
  end;

  function Holds(aOp: TExprOpCode; aLeft, aRight: TMaxPrecInt): boolean;
  begin
    case aOp of
      eopLessThan: Result := aLeft < aRight;
      eopGreaterThan: Result := aLeft > aRight;
      eopLessthanEqual: Result := aLeft <= aRight;
    else
      Result := aLeft >= aRight;
    end;
  end;

var
  lBinary: TBinaryExpr;
  lOp: TExprOpCode;
  lOperand: TPasExpr;
  lLeft, lRight, lConst, lRange: TResEvalValue;
  lRes: TPasResolverResult;
  lRangeInt: TResEvalRangeInt;
  lLimit: TMaxPrecInt;
  lAtStart: boolean;

begin
  Result := False;
  aTypeName := '';
  aConstant := '';
  aValue := False;
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopEqual, eopNotEqual, eopLessThan,
    eopGreaterThan, eopLessthanEqual, eopGreaterThanEqual]) then
    Exit;
  lLeft := EvalGuarded(lBinary.Left);
  lRight := EvalGuarded(lBinary.Right);
  lRange := nil;
  try
    // Both sides constant is ConstantConditionAlwaysTrueOrFalse's territory.
    if (lLeft = nil) = (lRight = nil) then
      Exit;
    if lRight <> nil then
    begin
      lConst := lRight;
      lOperand := lBinary.Left;
      lOp := lBinary.OpCode;
    end
    else
    begin
      lConst := lLeft;
      lOperand := lBinary.Right;
      lOp := Mirror(lBinary.OpCode);
    end;
    case lConst.Kind of
      revkInt: lLimit := TResEvalInt(lConst).Int;
      revkUInt:
        begin
          if TResEvalUInt(lConst).UInt > High(TMaxPrecInt) then
            Exit;
          lLimit := TMaxPrecInt(TResEvalUInt(lConst).UInt);
        end;
    else
      Exit;
    end;
    { FPC computes integer arithmetic at 32 bits or wider while ComputeElement
      keeps the operand's own type, so a computed operand's reachable values are
      not its type's range. }
    if ((lOperand is TBinaryExpr)
        and (TBinaryExpr(lOperand).OpCode in [eopAdd, eopSubtract, eopMultiply,
          eopDiv, eopPower, eopShl]))
      or ((lOperand is TUnaryExpr)
        and (TUnaryExpr(lOperand).OpCode in [eopAdd, eopSubtract])) then
      Exit;
    if not ComputeGuarded(lOperand, lRes) then
      Exit;
    if (lRes.LoTypeEl = nil) or (lRes.LoTypeEl.Name = '') then
      Exit;
    { Currency carries an integer range but fractional values, so a limit at
      the bound leaves the comparison satisfiable. }
    if lRes.BaseType = btCurrency then
      Exit;
    lRange := TypeRangeGuarded(lRes.LoTypeEl);
    if (lRange = nil) or (lRange.Kind <> revkRangeInt) then
      Exit;
    lRangeInt := TResEvalRangeInt(lRange);
    if lRangeInt.ElKind <> revskInt then
      Exit;
    if lOp in [eopEqual, eopNotEqual] then
    begin
      if (lLimit < lRangeInt.RangeStart) or (lLimit > lRangeInt.RangeEnd) then
        aValue := lOp = eopNotEqual
      else if lRangeInt.RangeStart = lRangeInt.RangeEnd then
        aValue := lOp = eopEqual
      else
        Exit;
    end
    else
    begin
      // An ordering test is monotone, so the two bounds decide it.
      lAtStart := Holds(lOp, lRangeInt.RangeStart, lLimit);
      if lAtStart <> Holds(lOp, lRangeInt.RangeEnd, lLimit) then
        Exit;
      aValue := lAtStart;
    end;
    aTypeName := lRes.LoTypeEl.Name;
    aConstant := lConst.AsString;
    Result := True;
  finally
    ReleaseEvalValue(lLeft);
    ReleaseEvalValue(lRight);
    ReleaseEvalValue(lRange);
  end;
end;


function TFpSonarResolver.TryConstantOutOfRangeForTarget(aAssign: TPasElement;
  out aConstant, aTypeName: string): boolean;

var
  lAssign: TPasImplAssign;
  lValue, lRange: TResEvalValue;
  lRes: TPasResolverResult;
  lInRange: boolean;

begin
  Result := False;
  aConstant := '';
  aTypeName := '';
  if not (aAssign is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aAssign);
  if (lAssign.Kind <> akDefault) or (lAssign.Left = nil) then
    Exit;
  if not ComputeGuarded(lAssign.Left, lRes) then
    Exit;
  if (lRes.LoTypeEl = nil) or (lRes.LoTypeEl.Name = '') then
    Exit;
  lValue := EvalGuarded(lAssign.Right);
  lRange := TypeRangeGuarded(lRes.LoTypeEl);
  try
    if not TryInRangeGuarded(lValue, lAssign.Right, lRange, lAssign.Left,
      lInRange) then
      Exit;
    if lInRange then
      Exit;
    aConstant := lValue.AsString;
    aTypeName := lRes.LoTypeEl.Name;
    Result := True;
  finally
    ReleaseEvalValue(lValue);
    ReleaseEvalValue(lRange);
  end;
end;


function TFpSonarResolver.TryConstantOverflowInExpression(aExpr: TPasExpr;
  out aOperator: string): boolean;

var
  lBinary: TBinaryExpr;
  lLeft, lRight, lWhole: TResEvalValue;

begin
  Result := False;
  aOperator := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopAdd, eopSubtract, eopMultiply]) then
    Exit;
  lLeft := EvalGuarded(lBinary.Left);
  lRight := EvalGuarded(lBinary.Right);
  lWhole := nil;
  try
    if (lLeft = nil) or (lRight = nil) then
      Exit;
    if not (lLeft.Kind in [revkInt, revkUInt])
      or not (lRight.Kind in [revkInt, revkUInt]) then
      Exit;
    { Both operands fold, so a whole that does not fold is the evaluator
      refusing the result as unrepresentable. }
    lWhole := EvalGuarded(aExpr);
    if lWhole <> nil then
      Exit;
    aOperator := OpcodeStrings[lBinary.OpCode];
    Result := True;
  finally
    ReleaseEvalValue(lLeft);
    ReleaseEvalValue(lRight);
    ReleaseEvalValue(lWhole);
  end;
end;


function TFpSonarResolver.TryShiftCountExceedsWidth(aExpr: TPasExpr;
  out aCount, aTypeName: string): boolean;

var
  lBinary: TBinaryExpr;
  lShifted, lCount: TResEvalValue;
  lRes: TPasResolverResult;
  lShiftType: TPasType;
  lPrecision: word;
  lSigned: boolean;
  lWidth: TMaxPrecInt;

begin
  Result := False;
  aCount := '';
  aTypeName := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopShl, eopShr]) then
    Exit;
  lShifted := EvalGuarded(lBinary.Left);
  lCount := EvalGuarded(lBinary.Right);
  try
    { A folding left operand is shifted at the evaluator's own widest
      precision, not at the declared width of its inferred type. }
    if (lShifted <> nil) or (lCount = nil) then
      Exit;
    case lCount.Kind of
      revkInt: lWidth := TResEvalInt(lCount).Int;
      revkUInt:
        begin
          if TResEvalUInt(lCount).UInt > High(TMaxPrecInt) then
            Exit;
          lWidth := TMaxPrecInt(TResEvalUInt(lCount).UInt);
        end;
    else
      Exit;
    end;
    if lWidth < 0 then
      Exit;
    if not ComputeGuarded(lBinary.Left, lRes) then
      Exit;
    if (lRes.LoTypeEl = nil) or (lRes.LoTypeEl.Name = '') then
      Exit;
    try
      FTopEngine.GetIntegerProps(lRes.BaseType, lPrecision, lSigned);
    except
      on E: Exception do
        Exit;
    end;
    { FPC computes a shift of a sub-32-bit ordinal in 32 bits
      (compiler/nmat.pas, tshlshrnode.pass_typecheck), so the width that decides
      the verdict is that type's, not the operand's. }
    lShiftType := lRes.LoTypeEl;
    if lPrecision < 32 then
    begin
      lPrecision := 32;
      lShiftType := FTopEngine.BaseTypes[btLongint];
    end;
    if lWidth < lPrecision then
      Exit;
    if (lShiftType = nil) or (lShiftType.Name = '') then
      Exit;
    aCount := lCount.AsString;
    aTypeName := lShiftType.Name;
    Result := True;
  finally
    ReleaseEvalValue(lShifted);
    ReleaseEvalValue(lCount);
  end;
end;


function TFpSonarResolver.TrySetElementOutOfRange(aExpr: TPasExpr;
  out aTypeName: string): boolean;

var
  lBinary: TBinaryExpr;
  lSet, lRange: TResEvalValue;
  lRes: TPasResolverResult;
  lFits: boolean;

begin
  Result := False;
  aTypeName := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if lBinary.OpCode <> eopIn then
    Exit;
  if not ComputeGuarded(lBinary.Left, lRes) then
    Exit;
  if (lRes.LoTypeEl = nil) or (lRes.LoTypeEl.Name = '') then
    Exit;
  lSet := EvalGuarded(lBinary.Right);
  lRange := TypeRangeGuarded(lRes.LoTypeEl);
  try
    if (lSet = nil) or (lSet.Kind <> revkSetOfInt) or (lRange = nil) then
      Exit;
    try
      lFits := FTopEngine.ExprEvaluator.IsSetCompatible(lSet, lBinary.Right,
        lRange, False);
    except
      on E: Exception do
        Exit;
    end;
    if lFits then
      Exit;
    aTypeName := lRes.LoTypeEl.Name;
    Result := True;
  finally
    ReleaseEvalValue(lSet);
    ReleaseEvalValue(lRange);
  end;
end;


function TFpSonarResolver.TryEnumOrdinalOutOfRange(aExpr: TPasExpr;
  out aOrdinal, aEnumName: string): boolean;

var
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lTarget: TPasType;
  lValue, lRange: TResEvalValue;
  lRangeInt: TResEvalRangeInt;
  lOrdinal: TMaxPrecInt;

begin
  Result := False;
  aOrdinal := '';
  aEnumName := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aExpr);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) <> 1) then
    Exit;
  lTarget := nil;
  try
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (lRef.Declaration is TPasType) then
      Exit;
    if lRef.Declaration.CustomData is TResElDataBuiltInProc then
      Exit;
    lTarget := FTopEngine.ResolveAliasType(TPasType(lRef.Declaration));
  except
    on E: Exception do
      Exit;
  end;
  if not (lTarget is TPasEnumType) then
    Exit;
  lValue := EvalGuarded(lParams.Params[0]);
  lRange := TypeRangeGuarded(lTarget);
  try
    if (lValue = nil) or (lRange = nil) or (lRange.Kind <> revkRangeInt) then
      Exit;
    case lValue.Kind of
      revkInt: lOrdinal := TResEvalInt(lValue).Int;
      revkUInt:
        begin
          if TResEvalUInt(lValue).UInt > High(TMaxPrecInt) then
            Exit;
          lOrdinal := TMaxPrecInt(TResEvalUInt(lValue).UInt);
        end;
    else
      Exit;
    end;
    lRangeInt := TResEvalRangeInt(lRange);
    if (lOrdinal >= lRangeInt.RangeStart)
      and (lOrdinal <= lRangeInt.RangeEnd) then
      Exit;
    aOrdinal := lValue.AsString;
    aEnumName := lRef.Declaration.Name;
    Result := True;
  finally
    ReleaseEvalValue(lValue);
    ReleaseEvalValue(lRange);
  end;
end;


function TFpSonarResolver.TryArrayIndexConstantOutOfBounds(aExpr: TPasExpr;
  out aIndex, aArrayName: string): boolean;

var
  lParams: TParamsExpr;
  lRes: TPasResolverResult;
  lArray: TPasArrayType;
  lValue, lRange: TResEvalValue;
  lDecl: TPasElement;
  lInRange: boolean;
  i, lDims: integer;

begin
  Result := False;
  aIndex := '';
  aArrayName := '';
  if not (aExpr is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aExpr);
  if (lParams.Kind <> pekArrayParams) or (Length(lParams.Params) = 0) then
    Exit;
  if not ComputeGuarded(lParams.Value, lRes) then
    Exit;
  if not (lRes.LoTypeEl is TPasArrayType) then
    Exit;
  lArray := TPasArrayType(lRes.LoTypeEl);
  lDims := Length(lArray.Ranges);
  if lDims = 0 then
    Exit;
  if lDims > Length(lParams.Params) then
    lDims := Length(lParams.Params);
  for i := 0 to lDims - 1 do
  begin
    lValue := EvalGuarded(lParams.Params[i]);
    lRange := EvalGuarded(lArray.Ranges[i]);
    try
      if TryInRangeGuarded(lValue, lParams.Params[i], lRange,
        lArray.Ranges[i], lInRange) and not lInRange then
      begin
        lDecl := ReferencedDecl(lParams.Value);
        if (lDecl <> nil) and (lDecl.Name <> '') then
          aArrayName := lDecl.Name
        else if lParams.Value is TPrimitiveExpr then
          aArrayName := TPrimitiveExpr(lParams.Value).Value;
        if aArrayName <> '' then
        begin
          aIndex := lValue.AsString;
          Result := True;
        end;
      end;
    finally
      ReleaseEvalValue(lValue);
      ReleaseEvalValue(lRange);
    end;
    if Result then
      Exit;
  end;
end;


function TFpSonarResolver.TrySizeOfOnReferenceType(aExpr: TPasExpr;
  out aTypeName: string): boolean;

var
  lParams: TParamsExpr;
  lRes: TPasResolverResult;
  lReference: boolean;

begin
  Result := False;
  aTypeName := '';
  if not BuiltInCall(aExpr, bfSizeOf, 1, lParams) then
    Exit;
  if not ComputeGuarded(lParams.Params[0], lRes) then
    Exit;
  if (lRes.LoTypeEl = nil) or (lRes.LoTypeEl.Name = '') then
    Exit;
  if (lRes.IdentEl = nil) or (lRes.IdentEl is TPasType) then
    Exit;
  lReference := (lRes.LoTypeEl is TPasClassType)
    and (TPasClassType(lRes.LoTypeEl).ObjKind
      in [okClass, okInterface, okDispInterface]);
  if not lReference then
    try
      lReference := FTopEngine.IsDynArray(lRes.LoTypeEl, False);
    except
      on E: Exception do
        Exit;
    end;
  if not lReference then
    lReference := lRes.BaseType in btAllStringPointer;
  if not lReference then
    Exit;
  aTypeName := lRes.LoTypeEl.Name;
  Result := True;
end;


function TFpSonarResolver.TryMoveFillCharSizeMismatch(aExpr: TPasExpr;
  out aCallee, aCount: string): boolean;

var
  lParams, lInner: TParamsExpr;
  lRef: TResolvedReference;
  lProcType: TPasProcedureType;
  lRes, lElem: TPasResolverResult;
  lPrecision: word;
  lSigned: boolean;
  lCountAt: integer;

begin
  Result := False;
  aCallee := '';
  aCount := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aExpr);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) <> 3) then
    Exit;
  try
    lRef := FTopEngine.GetParamsValueRef(lParams);
  except
    on E: Exception do
      Exit;
  end;
  if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
    Exit;
  // The RTL pair is declared at unit level, with an untyped first argument.
  if not (lRef.Declaration.Parent is TPasSection) then
    Exit;
  lProcType := TPasProcedure(lRef.Declaration).ProcType;
  if (lProcType = nil) or (lProcType.Args = nil) or (lProcType.Args.Count <> 3)
    or not (TObject(lProcType.Args[0]) is TPasArgument)
    or (TPasArgument(lProcType.Args[0]).ArgType <> nil) then
    Exit;
  if SameText(lRef.Declaration.Name, 'Move') then
    lCountAt := 2
  else if SameText(lRef.Declaration.Name, 'FillChar') then
    lCountAt := 1
  else
    Exit;
  if BuiltInCall(lParams.Params[lCountAt], bfLength, 1, lInner) then
  begin
    if not ComputeGuarded(lInner.Params[0], lRes) then
      Exit;
    if not (lRes.LoTypeEl is TPasArrayType) then
      Exit;
    if not ComputeGuarded(TPasArrayType(lRes.LoTypeEl).ElType, lElem) then
      Exit;
    if not IntegerPropsGuarded(lElem.BaseType, lPrecision, lSigned)
      or (lPrecision <= 8) then
      Exit;
    aCount := 'Length';
  end
  else if BuiltInCall(lParams.Params[lCountAt], bfSizeOf, 1, lInner) then
  begin
    if not ComputeGuarded(lInner.Params[0], lRes) then
      Exit;
    if not (lRes.LoTypeEl is TPasPointerType) then
      Exit;
    if (lRes.IdentEl = nil) or (lRes.IdentEl is TPasType) then
      Exit;
    // Sizing the very variable that carries the data asks for the pointer size.
    if (ReferencedDecl(lParams.Params[0]) = lRes.IdentEl)
      or ((lCountAt = 2)
        and (ReferencedDecl(lParams.Params[1]) = lRes.IdentEl)) then
      Exit;
    aCount := 'SizeOf';
  end
  else
    Exit;
  aCallee := lRef.Declaration.Name;
  Result := True;
end;


function TFpSonarResolver.TryFloatEqualityComparison(aExpr: TPasExpr;
  out aTypeName: string): boolean;

  function FloatOperand(aOperand: TPasExpr; out aName: string): boolean;
  var
    lRes: TPasResolverResult;
  begin
    aName := '';
    // Currency is a scaled Int64, not a binary float.
    Result := ComputeGuarded(aOperand, lRes)
      and (lRes.BaseType in btAllFloats - [btCurrency])
      and (lRes.LoTypeEl <> nil) and (lRes.LoTypeEl.Name <> '');
    if Result then
      aName := lRes.LoTypeEl.Name;
  end;

var
  lBinary: TBinaryExpr;

begin
  Result := False;
  aTypeName := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopEqual, eopNotEqual]) then
    Exit;
  Result := FloatOperand(lBinary.Left, aTypeName)
    or FloatOperand(lBinary.Right, aTypeName);
end;


function TFpSonarResolver.TryIntegerDivisionAssignedToFloat(
  aAssign: TPasElement; out aTypeName: string): boolean;

  function IsInteger(aOperand: TPasExpr): boolean;
  var
    lRes: TPasResolverResult;
  begin
    Result := ComputeGuarded(aOperand, lRes)
      and (lRes.BaseType in btAllInteger);
  end;

var
  lAssign: TPasImplAssign;
  lDiv: TBinaryExpr;
  lRes: TPasResolverResult;

begin
  Result := False;
  aTypeName := '';
  if not (aAssign is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aAssign);
  if (lAssign.Kind <> akDefault) or (lAssign.Left = nil)
    or not (lAssign.Right is TBinaryExpr) then
    Exit;
  lDiv := TBinaryExpr(lAssign.Right);
  if lDiv.OpCode <> eopDiv then
    Exit;
  if not ComputeGuarded(lAssign.Left, lRes) then
    Exit;
  if not (lRes.BaseType in btAllFloats) then
    Exit;
  if (lRes.LoTypeEl = nil) or (lRes.LoTypeEl.Name = '') then
    Exit;
  if not IsInteger(lDiv.Left) or not IsInteger(lDiv.Right) then
    Exit;
  aTypeName := lRes.LoTypeEl.Name;
  Result := True;
end;


function TFpSonarResolver.TryMixedSignedUnsignedComparison(aExpr: TPasExpr;
  out aSignedType, aUnsignedType: string): boolean;

  function Promoted(aOperand: TPasExpr): boolean;
  begin
    Result := ((aOperand is TBinaryExpr)
        and (TBinaryExpr(aOperand).OpCode in [eopAdd, eopSubtract, eopMultiply,
          eopDiv, eopMod, eopAnd, eopOr, eopXor]))
      or ((aOperand is TUnaryExpr)
        and (TUnaryExpr(aOperand).OpCode in [eopAdd, eopSubtract]));
  end;

  function Folds(aOperand: TPasExpr): boolean;
  var
    lValue: TResEvalValue;
  begin
    lValue := EvalGuarded(aOperand);
    try
      Result := lValue <> nil;
    finally
      ReleaseEvalValue(lValue);
    end;
  end;

var
  lBinary: TBinaryExpr;
  lLeft, lRight: TPasResolverResult;
  lLeftPrec, lRightPrec: word;
  lLeftSigned, lRightSigned: boolean;

begin
  Result := False;
  aSignedType := '';
  aUnsignedType := '';
  if not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopEqual, eopNotEqual, eopLessThan,
    eopGreaterThan, eopLessthanEqual, eopGreaterThanEqual]) then
    Exit;
  if not ComputeGuarded(lBinary.Left, lLeft)
    or not ComputeGuarded(lBinary.Right, lRight) then
    Exit;
  if not IntegerPropsGuarded(lLeft.BaseType, lLeftPrec, lLeftSigned)
    or not IntegerPropsGuarded(lRight.BaseType, lRightPrec, lRightSigned) then
    Exit;
  if (lLeftPrec <> lRightPrec) or (lLeftPrec < 32)
    or (lLeftSigned = lRightSigned) then
    Exit;
  if Promoted(lBinary.Left) or Promoted(lBinary.Right) then
    Exit;
  if Folds(lBinary.Left) or Folds(lBinary.Right) then
    Exit;
  if (lLeft.LoTypeEl = nil) or (lLeft.LoTypeEl.Name = '')
    or (lRight.LoTypeEl = nil) or (lRight.LoTypeEl.Name = '') then
    Exit;
  if lLeftSigned then
  begin
    aSignedType := lLeft.LoTypeEl.Name;
    aUnsignedType := lRight.LoTypeEl.Name;
  end
  else
  begin
    aSignedType := lRight.LoTypeEl.Name;
    aUnsignedType := lLeft.LoTypeEl.Name;
  end;
  Result := True;
end;


function TFpSonarResolver.TryPCharOfTemporaryString(aExpr: TPasExpr;
  out aTargetName: string): boolean;

  { The declaration aOperand binds to. A call carries its reference on the
    callee, and a dotted callee carries it on the rightmost name. }
  function OperandDecl(aOperand: TPasExpr): TPasElement;
  var
    lRef: TResolvedReference;
  begin
    Result := nil;
    if (aOperand is TParamsExpr)
      and (TParamsExpr(aOperand).Kind = pekFuncParams) then
    begin
      try
        lRef := FTopEngine.GetParamsValueRef(TParamsExpr(aOperand));
      except
        on E: Exception do
          Exit;
      end;
      if lRef <> nil then
        Result := lRef.Declaration;
    end
    else if (aOperand is TBinaryExpr)
      and (TBinaryExpr(aOperand).OpCode = eopSubIdent) then
      Result := ReferencedDecl(TBinaryExpr(aOperand).Right)
    else
      Result := ReferencedDecl(aOperand);
  end;

  { True when aDecl is a built-in whose result is a freshly built string. }
  function StringBuiltIn(aDecl: TPasElement): boolean;
  begin
    Result := (aDecl is TPasUnresolvedSymbolRef)
      and (aDecl.CustomData is TResElDataBuiltInProc)
      and (TResElDataBuiltInProc(aDecl.CustomData).BuiltIn
        in [bfCopyString, bfConcatString]);
  end;

var
  lTarget, lSource, lDest: TFpSonarResolvedType;
  lOperand: TPasExpr;
  lDecl: TPasElement;

begin
  Result := False;
  aTargetName := '';
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  if not TryTypecast(aExpr, lTarget, lSource) then
    Exit;
  if (lSource.Kind <> ltkString) or (lTarget.Kind <> ltkPointer)
    or not (lTarget.TypeEl is TPasPointerType) then
    Exit;
  if not TryResolvedType(TPasPointerType(lTarget.TypeEl).DestType, lDest)
    or (lDest.Kind <> ltkChar) then
    Exit;
  lOperand := TParamsExpr(aExpr).Params[0];
  if (lOperand is TBinaryExpr) and (TBinaryExpr(lOperand).OpCode = eopAdd) then
    Result := True
  else
  begin
    lDecl := OperandDecl(lOperand);
    Result := ((lDecl is TPasProcedure)
      and (TPasProcedure(lDecl).ProcType is TPasFunctionType))
      or StringBuiltIn(lDecl);
  end;
  if Result then
    aTargetName := lTarget.NamedTypeName;
end;


function TFpSonarResolver.TryImplicitStringConversion(aNode: TPasElement;
  out aSourceName, aTargetName: string): boolean;

var
  lAssign: TPasImplAssign;
  lLeft, lRight: TFpSonarResolvedType;

begin
  Result := False;
  aSourceName := '';
  aTargetName := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aNode is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aNode);
  if not TryResolvedType(lAssign.Left, lLeft)
    or not TryResolvedType(lAssign.Right, lRight) then
    Exit;
  if (lLeft.Kind <> ltkString) or (lRight.Kind <> ltkString) then
    Exit;
  if (lRight.Encoding <> lseWide) or (lLeft.Encoding <> lseAnsi) then
    Exit;
  aSourceName := lRight.NamedTypeName;
  aTargetName := lLeft.NamedTypeName;
  Result := True;
end;


function TFpSonarResolver.TryLengthUsedAsByteCount(aExpr: TPasExpr;
  out aStringName, aCallee: string): boolean;

var
  lParams, lInner: TParamsExpr;
  lRef: TResolvedReference;
  lProcType: TPasProcedureType;
  lRes: TPasResolverResult;
  lString: TFpSonarResolvedType;
  lCountAt: integer;

begin
  Result := False;
  aStringName := '';
  aCallee := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aExpr);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) <> 3) then
    Exit;
  try
    lRef := FTopEngine.GetParamsValueRef(lParams);
  except
    on E: Exception do
      Exit;
  end;
  if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
    Exit;
  // The RTL pair is declared at unit level, with an untyped first argument.
  if not (lRef.Declaration.Parent is TPasSection) then
    Exit;
  lProcType := TPasProcedure(lRef.Declaration).ProcType;
  if (lProcType = nil) or (lProcType.Args = nil) or (lProcType.Args.Count <> 3)
    or not (TObject(lProcType.Args[0]) is TPasArgument)
    or (TPasArgument(lProcType.Args[0]).ArgType <> nil) then
    Exit;
  if SameText(lRef.Declaration.Name, 'Move') then
    lCountAt := 2
  else if SameText(lRef.Declaration.Name, 'FillChar') then
    lCountAt := 1
  else
    Exit;
  if not BuiltInCall(lParams.Params[lCountAt], bfLength, 1, lInner) then
    Exit;
  if not ComputeGuarded(lInner.Params[0], lRes) then
    Exit;
  if not FillResolvedType(lRes, FTopEngine.BaseTypeString,
    FTopEngine.BaseTypeChar, lString) then
    Exit;
  if (lString.Kind <> ltkString) or (lString.Encoding <> lseWide) then
    Exit;
  aStringName := lString.NamedTypeName;
  aCallee := lRef.Declaration.Name;
  Result := True;
end;


function TFpSonarResolver.TryCopyWithZeroIndex(aExpr: TPasExpr;
  out aStringName: string): boolean;

var
  lParams: TParamsExpr;
  lString: TFpSonarResolvedType;
  lValue: TResEvalValue;

begin
  Result := False;
  aStringName := '';
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  // bfCopyString is the string overload; the array Copy binds bfCopyArray.
  if not BuiltInCall(aExpr, bfCopyString, 3, lParams)
    and not BuiltInCall(aExpr, bfCopyString, 2, lParams) then
    Exit;
  if not TryResolvedType(lParams.Params[0], lString)
    or (lString.Kind <> ltkString) then
    Exit;
  lValue := EvalGuarded(lParams.Params[1]);
  try
    if lValue = nil then
      Exit;
    case lValue.Kind of
      revkInt: Result := TResEvalInt(lValue).Int = 0;
      revkUInt: Result := TResEvalUInt(lValue).UInt = 0;
    end;
  finally
    ReleaseEvalValue(lValue);
  end;
  if Result then
    aStringName := lString.NamedTypeName;
end;


function TFpSonarResolver.TryPosResultComparedToZeroBased(aExpr: TPasExpr;
  out aCallee, aConstText: string): boolean;

  // aOp read with its two operands swapped.
  function Mirror(aOp: TExprOpCode): TExprOpCode;
  begin
    case aOp of
      eopLessThan: Result := eopGreaterThan;
      eopGreaterThan: Result := eopLessThan;
      eopLessthanEqual: Result := eopGreaterThanEqual;
      eopGreaterThanEqual: Result := eopLessthanEqual;
    else
      Result := aOp;
    end;
  end;

  // The unit-level routine named Pos that aOperand calls, or nil.
  function PosCallee(aOperand: TPasExpr): TPasElement;
  var
    lRef: TResolvedReference;
    lType: TPasProcedureType;
  begin
    Result := nil;
    if not (aOperand is TParamsExpr)
      or (TParamsExpr(aOperand).Kind <> pekFuncParams) then
      Exit;
    try
      lRef := FTopEngine.GetParamsValueRef(TParamsExpr(aOperand));
    except
      on E: Exception do
        Exit;
    end;
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure)
      or not (lRef.Declaration.Parent is TPasSection)
      or not SameText(lRef.Declaration.Name, 'Pos') then
      Exit;
    lType := TPasProcedure(lRef.Declaration).ProcType;
    if not (lType is TPasFunctionType) or (lType.Args = nil)
      or (lType.Args.Count < 2) then
      Exit;
    Result := lRef.Declaration;
  end;

var
  lBinary: TBinaryExpr;
  lOp: TExprOpCode;
  lDecl: TPasElement;
  lValue: TResEvalValue;
  lLimit: TMaxPrecInt;

begin
  Result := False;
  aCallee := '';
  aConstText := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopEqual, eopNotEqual, eopLessThan,
    eopGreaterThan, eopLessthanEqual, eopGreaterThanEqual]) then
    Exit;
  lDecl := PosCallee(lBinary.Left);
  if lDecl <> nil then
  begin
    lOp := lBinary.OpCode;
    lValue := EvalGuarded(lBinary.Right);
  end
  else
  begin
    lDecl := PosCallee(lBinary.Right);
    if lDecl = nil then
      Exit;
    lOp := Mirror(lBinary.OpCode);
    lValue := EvalGuarded(lBinary.Left);
  end;
  lLimit := 0;
  try
    if lValue = nil then
      Exit;
    case lValue.Kind of
      revkInt: lLimit := TResEvalInt(lValue).Int;
      revkUInt:
        begin
          if TResEvalUInt(lValue).UInt > High(TMaxPrecInt) then
            Exit;
          lLimit := TMaxPrecInt(TResEvalUInt(lValue).UInt);
        end;
    else
      Exit;
    end;
  finally
    ReleaseEvalValue(lValue);
  end;
  if (lLimit = 0) and (lOp in [eopGreaterThanEqual, eopLessThan]) then
    Result := True
  else if (lLimit = -1) and (lOp in [eopGreaterThan, eopLessthanEqual,
    eopEqual, eopNotEqual]) then
    Result := True;
  if Result then
  begin
    aCallee := lDecl.Name;
    aConstText := IntToStr(lLimit);
  end;
end;


function TFpSonarResolver.TryShortStringTruncation(aNode: TPasElement;
  out aTargetName, aCapacity: string): boolean;

  { The length written in aType's string[N] declaration, or -1. It is read
    from TPasStringType.LengthExpr, which the parser stores as raw source
    text rather than as an expression tree. }
  function DeclaredLength(aType: TPasType): integer;
  var
    lType: TPasType;
    lGuard: integer;
  begin
    Result := -1;
    lType := aType;
    lGuard := 0;
    while (lType is TPasAliasType) and (lGuard < 100) do
    begin
      lType := TPasAliasType(lType).DestType;
      Inc(lGuard);
    end;
    if not (lType is TPasStringType) then
      Exit;
    if not TryStrToInt(TPasStringType(lType).LengthExpr, Result) then
      Result := -1;
  end;

  // The byte count aValue's string constant folds to, or -1.
  function FoldedByteCount(aValue: TPasExpr): integer;
  var
    lVal: TResEvalValue;
  begin
    Result := -1;
    lVal := EvalGuarded(aValue);
    try
      try
        if (lVal <> nil) and (lVal.Kind in revkAllStrings) then
          case lVal.Kind of
            {$ifdef FPC_HAS_CPSTRING}
            revkString: Result := Length(FTopEngine.ExprEvaluator.GetUTF8Str(
              TResEvalString(lVal).S, aValue));
            {$endif}
            revkUnicodeString:
              Result := Length(UTF8Encode(TResEvalUTF16(lVal).S));
          end;
      except
        on E: Exception do
          Result := -1;
      end;
    finally
      ReleaseEvalValue(lVal);
    end;
  end;

var
  lAssign: TPasImplAssign;
  lDecl: TPasElement;
  lCapacity, lLength: integer;

begin
  Result := False;
  aTargetName := '';
  aCapacity := '';
  if (FTopEngine = nil) or (not FSucceeded)
    or not (aNode is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aNode);
  if lAssign.Kind <> akDefault then
    Exit;
  lDecl := ReferencedDecl(lAssign.Left);
  if not (lDecl is TPasVariable) then
    Exit;
  try
    lCapacity := DeclaredLength(TPasVariable(lDecl).VarType);
  except
    on E: Exception do
      Exit;
  end;
  if lCapacity < 0 then
    Exit;
  lLength := FoldedByteCount(lAssign.Right);
  if (lLength < 0) or (lLength <= lCapacity) then
    Exit;
  aTargetName := lDecl.Name;
  aCapacity := IntToStr(lCapacity);
  Result := True;
end;


function TFpSonarResolver.TryCharComparedToString(aExpr: TPasExpr;
  out aCharTypeName, aLength: string): boolean;

  { The character count aValue's string constant folds to, or -1. The UTF-8
    form is decoded, so a non-ASCII character counts once and not per byte. }
  function FoldedLength(aValue: TPasExpr): integer;
  var
    lVal: TResEvalValue;
  begin
    Result := -1;
    lVal := EvalGuarded(aValue);
    try
      try
        if (lVal <> nil) and (lVal.Kind in revkAllStrings) then
          case lVal.Kind of
            {$ifdef FPC_HAS_CPSTRING}
            revkString: Result := Length(UTF8Decode(
              FTopEngine.ExprEvaluator.GetUTF8Str(TResEvalString(lVal).S,
              aValue)));
            {$endif}
            revkUnicodeString:
              Result := Length(TResEvalUTF16(lVal).S);
          end;
      except
        on E: Exception do
          Result := -1;
      end;
    finally
      ReleaseEvalValue(lVal);
    end;
  end;

  { True when aChar classifies as a char and aText as a string constant of a
    length other than one. }
  function CharAgainstText(aChar, aText: TPasExpr; out aName: string;
    out aCount: integer): boolean;
  var
    lChar, lText: TFpSonarResolvedType;
  begin
    Result := False;
    aName := '';
    aCount := -1;
    if not TryResolvedType(aChar, lChar) or (lChar.Kind <> ltkChar) then
      Exit;
    if not TryResolvedType(aText, lText) or (lText.Kind <> ltkString) then
      Exit;
    aCount := FoldedLength(aText);
    if (aCount < 0) or (aCount = 1) then
      Exit;
    aName := lChar.NamedTypeName;
    Result := True;
  end;

var
  lBinary: TBinaryExpr;
  lName: string;
  lCount: integer;

begin
  Result := False;
  aCharTypeName := '';
  aLength := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TBinaryExpr) then
    Exit;
  lBinary := TBinaryExpr(aExpr);
  if not (lBinary.OpCode in [eopEqual, eopNotEqual]) then
    Exit;
  if not CharAgainstText(lBinary.Left, lBinary.Right, lName, lCount)
    and not CharAgainstText(lBinary.Right, lBinary.Left, lName, lCount) then
    Exit;
  aCharTypeName := lName;
  aLength := IntToStr(lCount);
  Result := True;
end;


function TFpSonarResolver.ReferencedDecl(aExpr: TPasElement): TPasElement;
begin
  Result := nil;
  if (FTopEngine = nil) or (not FSucceeded) or (aExpr = nil) then
    Exit;
  try
    if aExpr.CustomData is TResolvedReference then
      Result := TResolvedReference(aExpr.CustomData).Declaration;
  except
    // Tolerant-access: any failure => no resolved declaration.
    on E: Exception do
      Result := nil;
  end;
end;


function TFpSonarResolver.TryConstructorCall(aNode: TPasElement;
  out aOnInstance: boolean; out aCtorName: string): boolean;

  // The TResolvedReference attached to an expression, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

var
  lBin: TBinaryExpr;
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lQualifier: TPasExpr;
  lRes: TPasResolverResult;
begin
  aOnInstance := False;
  aCtorName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;

  try
    lRef := nil;
    lQualifier := nil;
    { Shape 1: `Obj.Create` (no parens) — a member access; the ctor ref is on the Right ident, the qualifier is Left. }
    if (aNode is TBinaryExpr) and (TBinaryExpr(aNode).OpCode = eopSubIdent) then
    begin
      lBin := TBinaryExpr(aNode);
      lRef := RefOf(lBin.Right);
      lQualifier := lBin.Left;
    end
    { Shape 2: `Obj.Create()` (parens) — a call whose Value is that member access. }
    else if (aNode is TParamsExpr)
      and (TParamsExpr(aNode).Kind = pekFuncParams) then
    begin
      lParams := TParamsExpr(aNode);
      lRef := FTopEngine.GetParamsValueRef(lParams);
      if (lParams.Value is TBinaryExpr)
        and (TBinaryExpr(lParams.Value).OpCode = eopSubIdent) then
        lQualifier := TBinaryExpr(lParams.Value).Left;
    end
    else
      Exit;

    if (lRef = nil) or not (lRef.Declaration is TPasConstructor) then
      Exit;
    Result := True;
    aCtorName := lRef.Declaration.Name;

    { aOnInstance: the resolver did not flag a new-instance call }
    if (rrfNewInstance in lRef.Flags) or (lQualifier = nil) then
      Exit;
    FTopEngine.ComputeElement(lQualifier, lRes, []);
    aOnInstance := ClassifyResolvedKind(lRes) = ltkClass;
  except
    on E: Exception do
    begin
      aOnInstance := False;
      aCtorName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryDestructorContract(aDtor: TPasElement;
  out aDescendsTObject: boolean; out aInheritedVirtualDestroy: boolean): boolean;

  { Walks the owning class's RESOLVED ancestor scope chain. aIsDescendant is set
    True iff a class named 'TObject' is reached (the resolved chain handles the
    implicit-`class` root whose AncestorType is nil). Result=False (undeterminable)
    when the class carries no resolved class scope; True with aIsDescendant=False
    when the chain walks cleanly to the top without reaching TObject. }
  function DescendsTObject(aClass: TPasClassType; out aIsDescendant: boolean): boolean;
  var
    lScope: TPasClassScope;
    lGuard: integer;
  begin
    aIsDescendant := False;
    Result := False;
    if not (aClass.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      if (lScope.Element <> nil) and SameText(lScope.Element.Name, 'TObject') then
      begin
        aIsDescendant := True;
        Exit(True);
      end;
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
    // Walked the resolved chain to the top without reaching TObject: determined.
    Result := True;
  end;

var
  lProc: TPasProcedure;
  lScope: TPasProcedureScope;
begin
  aDescendsTObject := False;
  aInheritedVirtualDestroy := False;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aDtor = nil) then
    Exit;

  try
    if not (aDtor is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(aDtor);
    // The owning class (the destructor declaration's Parent).
    if not (lProc.Parent is TPasClassType) then
      Exit;
    // TObject-descent: undeterminable ancestry => degrade (Result stays False).
    if not DescendsTObject(TPasClassType(lProc.Parent), aDescendsTObject) then
      Exit;
    { Inherited overridable Destroy: the proc scope's same-signature ancestor link.
      The virtual root TObject.Destroy has none, so it is naturally exempt. }
    if aDtor.CustomData is TPasProcedureScope then
    begin
      lScope := TPasProcedureScope(aDtor.CustomData);
      aInheritedVirtualDestroy := lScope.OverriddenProc <> nil;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      aDescendsTObject := False;
      aInheritedVirtualDestroy := False;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryOverrideOnlyForwards(aProc: TPasElement;
  aInheritedExpr: TPasExpr): boolean;
var
  lProc, lDecl: TPasProcedure;
  lScope, lDeclScope: TPasProcedureScope;
  lBin: TBinaryExpr;
  lCall: TPasExpr;
  lActuals: TPasExprArray;
  lFormals: TFPList;
  lRes: TPasResolverResult;
  i: integer;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aProc = nil)
    or (aInheritedExpr = nil) then
    Exit;

  try
    if not (aProc is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(aProc);
    // Constructors/destructors are out of scope (this targets ordinary methods).
    if (lProc is TPasConstructor) or (lProc is TPasDestructor) then
      Exit;
    { The override modifier and the same-signature-ancestor link both live on the declaration. }
    if not (aProc.CustomData is TPasProcedureScope) then
      Exit;
    lScope := TPasProcedureScope(aProc.CustomData);
    lDecl := lScope.DeclarationProc;
    if lDecl = nil then
      lDecl := lProc;
    if not (pmOverride in lDecl.Modifiers) then
      Exit;
    if lDecl.CustomData is TPasProcedureScope then
      lDeclScope := TPasProcedureScope(lDecl.CustomData)
    else
      lDeclScope := lScope;
    // The inherited call must bind to the immediate same-signature ancestor, and
    // that ancestor must not be abstract.
    if lDeclScope.OverriddenProc = nil then
      Exit;
    if pmAbstract in lDeclScope.OverriddenProc.Modifiers then
      Exit;

    // The bare 'inherited;' form (a lone TInheritedExpr) forwards every argument
    // implicitly and unchanged — accept.
    if aInheritedExpr is TInheritedExpr then
    begin
      Result := True;
      Exit;
    end;
    // The named 'inherited Name(args)' form parses as a TBinaryExpr(eopNone) whose
    // Left is the TInheritedExpr and whose Right is the call.
    if not ((aInheritedExpr is TBinaryExpr)
      and (TBinaryExpr(aInheritedExpr).OpCode = eopNone)
      and (TBinaryExpr(aInheritedExpr).Left is TInheritedExpr)) then
      Exit;
    lBin := TBinaryExpr(aInheritedExpr);
    lCall := lBin.Right;
    SetLength(lActuals, 0);
    if (lCall is TParamsExpr) and (TParamsExpr(lCall).Kind = pekFuncParams) then
      lActuals := TParamsExpr(lCall).Params;
    { Match each actual to the current method's formal argument, in order }
    lFormals := lDecl.ProcType.Args;
    if lFormals = nil then
      Exit;
    if Length(lActuals) <> lFormals.Count then
      Exit;
    for i := 0 to High(lActuals) do
    begin
      FTopEngine.ComputeElement(lActuals[i], lRes, []);
      if lRes.IdentEl <> TPasElement(lFormals[i]) then
        Exit;
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable forward fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryRedundantInherited(aProc: TPasElement): boolean;

  { Walks the owning class's resolved ancestor scope chain. Result=True iff a
    class named 'TObject' is reached }
  function AncestryResolvable(aClass: TPasClassType): boolean;
  var
    lScope: TPasClassScope;
    lGuard: integer;
  begin
    Result := False;
    if not (aClass.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      if (lScope.Element <> nil) and SameText(lScope.Element.Name, 'TObject') then
        Exit(True);
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
  end;

var
  lProc, lDecl: TPasProcedure;
  lScope, lDeclScope: TPasProcedureScope;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aProc = nil) then
    Exit;

  try
    if not (aProc is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(aProc);
    { Constructors/destructors are excluded outright: a simple `inherited;` }
    if (lProc is TPasConstructor) or (lProc is TPasDestructor) then
      Exit;
    // The same-signature-ancestor link lives on the declaration proc's scope —
    if not (aProc.CustomData is TPasProcedureScope) then
      Exit;
    lScope := TPasProcedureScope(aProc.CustomData);
    lDecl := lScope.DeclarationProc;
    if lDecl = nil then
      lDecl := lProc;
    if lDecl.CustomData is TPasProcedureScope then
      lDeclScope := TPasProcedureScope(lDecl.CustomData)
    else
      lDeclScope := lScope;
    { The inverse of the override-forwards check: require NO overridable
      same-signature ancestor, so a simple `inherited;` in this method's body binds
      to nothing. }
    if lDeclScope.OverriddenProc <> nil then
      Exit;
    { The owning class (the declaration's Parent) must descend resolvably to
      TObject — only then is `OverriddenProc = nil` provably "no parent member"
      rather than "unresolved ancestry". }
    if not (lDecl.Parent is TPasClassType) then
      Exit;
    if not AncestryResolvable(TPasClassType(lDecl.Parent)) then
      Exit;
    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable fact.
    on E: Exception do
      Result := False;
  end;
end;


{ The declaration proc behind aProc's scope and that declaration's own scope;
  False when aProc carries no resolved procedure scope. }
function DeclarationOf(aProc: TPasProcedure; out aDecl: TPasProcedure;
  out aDeclScope: TPasProcedureScope): boolean;
var
  lScope: TPasProcedureScope;
begin
  aDecl := nil;
  aDeclScope := nil;
  Result := False;
  if not (aProc.CustomData is TPasProcedureScope) then
    Exit;
  lScope := TPasProcedureScope(aProc.CustomData);
  aDecl := lScope.DeclarationProc;
  if aDecl = nil then
    aDecl := aProc;
  if aDecl.CustomData is TPasProcedureScope then
    aDeclScope := TPasProcedureScope(aDecl.CustomData)
  else
    aDeclScope := lScope;
  Result := True;
end;


function TFpSonarResolver.TryMethodHidesVirtual(aProc: TPasElement): boolean;
var
  lDecl: TPasProcedure;
  lDeclScope: TPasProcedureScope;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aProc = nil) then
    Exit;

  try
    if not (aProc is TPasProcedure) then
      Exit;
    { A destructor's hiding shape is DestructorShouldOverrideDestroy's. }
    if aProc is TPasDestructor then
      Exit;
    if not DeclarationOf(TPasProcedure(aProc), lDecl, lDeclScope) then
      Exit;
    if [pmOverride, pmReintroduce, pmOverload] * lDecl.Modifiers <> [] then
      Exit;
    if lDeclScope.OverriddenProc = nil then
      Exit;
    { dynamic counts: such a method is virtual through the DMT. }
    Result := [pmVirtual, pmDynamic, pmOverride, pmAbstract]
      * lDeclScope.OverriddenProc.Modifiers <> [];
  except
    // Tolerant-access: any resolver failure => not a usable fact.
    on E: Exception do
      Result := False;
  end;
end;


{ The magnitude of aValue; False when its kind carries no number. }
function NumericEvalValue(aValue: TResEvalValue;
  out aNumber: TMaxPrecFloat): boolean;
begin
  Result := True;
  aNumber := 0;
  case aValue.Kind of
    revkInt: aNumber := TResEvalInt(aValue).Int;
    revkUInt: aNumber := TResEvalUInt(aValue).UInt;
    revkFloat: aNumber := TResEvalFloat(aValue).FloatValue;
    revkCurrency: aNumber := TResEvalCurrency(aValue).Value;
    else
      Result := False;
  end;
end;


function TFpSonarResolver.TryOverrideDefaultParamChange(
  aProc: TPasElement): boolean;

  { True iff exactly one side declares a default, or both fold to different
    values. Two numeric folds compare by magnitude, so 1 and 1.0 are equal. }
  function DefaultChanged(aMine, aTheirs: TPasExpr): boolean;
  var
    lMine, lTheirs: TResEvalValue;
    lNumMine, lNumTheirs: TMaxPrecFloat;
  begin
    Result := False;
    if (aMine = nil) and (aTheirs = nil) then
      Exit;
    if (aMine = nil) or (aTheirs = nil) then
      Exit(True);
    lMine := EvalGuarded(aMine);
    lTheirs := EvalGuarded(aTheirs);
    try
      if (lMine <> nil) and (lTheirs <> nil) then
        if NumericEvalValue(lMine, lNumMine)
          and NumericEvalValue(lTheirs, lNumTheirs) then
          Result := lNumMine <> lNumTheirs
        else
          Result := (lMine.Kind <> lTheirs.Kind)
            or (lMine.AsString <> lTheirs.AsString);
    finally
      ReleaseEvalValue(lMine);
      ReleaseEvalValue(lTheirs);
    end;
  end;

var
  lDecl, lAncestor: TPasProcedure;
  lDeclScope: TPasProcedureScope;
  lMine, lTheirs: TFPList;
  i: integer;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aProc = nil) then
    Exit;

  try
    if not (aProc is TPasProcedure) then
      Exit;
    if not DeclarationOf(TPasProcedure(aProc), lDecl, lDeclScope) then
      Exit;
    if not (pmOverride in lDecl.Modifiers) then
      Exit;
    lAncestor := lDeclScope.OverriddenProc;
    if (lAncestor = nil) or (lDecl.ProcType = nil)
      or (lAncestor.ProcType = nil) then
      Exit;
    lMine := lDecl.ProcType.Args;
    lTheirs := lAncestor.ProcType.Args;
    if (lMine = nil) or (lTheirs = nil) or (lMine.Count <> lTheirs.Count) then
      Exit;
    for i := 0 to lMine.Count - 1 do
      if DefaultChanged(TPasArgument(lMine[i]).ValueExpr,
        TPasArgument(lTheirs[i]).ValueExpr) then
        Exit(True);
  except
    // Tolerant-access: any resolver failure => not a usable fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryAbstractInstantiation(out aNodes: TPasElementArray;
  out aClassNames: TFpSonarStringArray): boolean;

  { True when aClass has an unimplemented abstract method and every one of them
    is declared in the analysed module. }
  function AbstractSetIsLocal(aClass: TPasClassType): boolean;
  var
    lProcs: TArrayOfPasProcedure;
    lModule: TPasModule;
    i: integer;
  begin
    Result := False;
    lProcs := TPasClassScope(aClass.CustomData).AbstractProcs;
    if Length(lProcs) = 0 then
      Exit;
    lModule := GetResolvedModule;
    for i := 0 to High(lProcs) do
      if lProcs[i].GetModule <> lModule then
        Exit;
    Result := True;
  end;

  { The class NAMED as the qualifier of a `T.Create` member access; nil for a
    bare, an instance or a class-of construction. }
  function NamedClassOf(aSite: TPasElement): TPasClassType;
  var
    lBin: TBinaryExpr;
    lDecl: TPasElement;
  begin
    Result := nil;
    if not (aSite.Parent is TBinaryExpr) then
      Exit;
    lBin := TBinaryExpr(aSite.Parent);
    if (lBin.OpCode <> eopSubIdent) or (lBin.Right <> aSite) then
      Exit;
    lDecl := ReferencedDecl(lBin.Left);
    if lDecl is TPasClassType then
      Result := TPasClassType(lDecl);
  end;

var
  lSites, lDecls: TPasElementArray;
  lClass: TPasClassType;
  i: integer;
begin
  SetLength(aNodes, 0);
  SetLength(aClassNames, 0);
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;

  try
    if not TryReferenceSites(lSites, lDecls) then
      Exit;
    for i := 0 to High(lSites) do
    begin
      if not (lDecls[i] is TPasConstructor)
        or not (lSites[i].CustomData is TResolvedReference) then
        Continue;
      if not (rrfNewInstance
        in TResolvedReference(lSites[i].CustomData).Flags) then
        Continue;
      lClass := NamedClassOf(lSites[i]);
      if (lClass = nil) or not (lClass.CustomData is TPasClassScope)
        or not AbstractSetIsLocal(lClass) then
        Continue;
      SetLength(aNodes, Length(aNodes) + 1);
      SetLength(aClassNames, Length(aClassNames) + 1);
      aNodes[High(aNodes)] := lSites[i];
      aClassNames[High(aClassNames)] := lClass.Name;
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => no usable site set.
    on E: Exception do
    begin
      SetLength(aNodes, 0);
      SetLength(aClassNames, 0);
      Result := False;
    end;
  end;
end;


// True when aClass declares a method named aName a helper in aHelperModule
// could hide.
function HidableMethodIn(aClass: TPasClassType; const aName: string;
  aHelperModule: TPasModule): boolean;
var
  lProc: TPasProcedure;
  m: integer;
begin
  Result := False;
  if aClass.Members = nil then
    Exit;
  for m := 0 to aClass.Members.Count - 1 do
  begin
    if not (TObject(aClass.Members[m]) is TPasProcedure) then
      Continue;
    lProc := TPasProcedure(aClass.Members[m]);
    if not SameText(lProc.Name, aName) then
      Continue;
    { A private member of another unit cannot be named there, so nothing is
      hidden; in the helper's own unit both private forms are. }
    if (lProc.Visibility in [visPrivate, visStrictPrivate])
      and (lProc.GetModule <> aHelperModule) then
      Continue;
    Exit(True);
  end;
end;


{ True when aClass or one of the classes on its resolved ancestor-scope chain
  declares a method named aName that a helper in aHelperModule hides. }
function ChainHidesMethod(aClass: TPasClassType; const aName: string;
  aHelperModule: TPasModule): boolean;
var
  lScope: TPasClassScope;
  lGuard: integer;
begin
  Result := False;
  if not (aClass.CustomData is TPasClassScope) then
    Exit;
  lScope := TPasClassScope(aClass.CustomData);
  lGuard := 0;
  while (lScope <> nil) and (lGuard < 100) do
  begin
    if (lScope.Element is TPasClassType)
      and HidableMethodIn(TPasClassType(lScope.Element), aName,
      aHelperModule) then
      Exit(True);
    lScope := lScope.AncestorScope;
    Inc(lGuard);
  end;
end;


// The class a class helper extends, or nil when it does not resolve to one.
function HelperExtendedClass(aEngine: TFpSonarResolverEngine;
  aHelper: TPasClassType): TPasClassType;
var
  lType: TPasType;
begin
  Result := nil;
  if aHelper.HelperForType = nil then
    Exit;
  lType := aEngine.ResolveAliasType(aHelper.HelperForType);
  if lType is TPasClassType then
    Result := TPasClassType(lType);
end;


function TFpSonarResolver.TryClassHelperHiddenMethods(
  out aNodes: TPasElementArray; out aNames: TFpSonarStringArray): boolean;
var
  lMod: TPasModule;

  procedure Add(aEl: TPasElement; const aName: string);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
    SetLength(aNames, Length(aNames) + 1);
    aNames[High(aNames)] := aName;
  end;

  procedure Consider(aDecl: TPasElement);
  var
    lHelper, lExtended: TPasClassType;
    lMember: TPasElement;
    m: integer;
  begin
    if not (aDecl is TPasClassType) then
      Exit;
    lHelper := TPasClassType(aDecl);
    if lHelper.ObjKind <> okClassHelper then
      Exit;
    lExtended := HelperExtendedClass(FTopEngine, lHelper);
    if (lExtended = nil) or (lHelper.Members = nil) then
      Exit;
    for m := 0 to lHelper.Members.Count - 1 do
    begin
      lMember := TPasElement(lHelper.Members[m]);
      if not (lMember is TPasProcedure) then
        Continue;
      { An overload joins the extended type's overload set instead of hiding it. }
      if pmOverload in TPasProcedure(lMember).Modifiers then
        Continue;
      if ChainHidesMethod(lExtended, lMember.Name, lHelper.GetModule) then
        Add(lMember, lMember.Name);
    end;
  end;

  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
      Consider(TPasElement(aDecls[k]));
  end;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      Result := False;
    end;
  end;
end;


// True when aEl is a data field.
function IsStorageField(aEl: TPasElement): boolean;
begin
  Result := (aEl is TPasVariable) and not (aEl is TPasProperty)
    and not (aEl is TPasConst);
end;


{ Appends every TPasProperty aType declares, descending into the member types
  it nests. }
procedure CollectTypeProperties(aType: TPasElement;
  var aProps: TPasElementArray);
var
  lMember: TPasElement;
  m: integer;
begin
  if not (aType is TPasMembersType) then
    Exit;
  if TPasMembersType(aType).Members = nil then
    Exit;
  for m := 0 to TPasMembersType(aType).Members.Count - 1 do
  begin
    lMember := TPasElement(TPasMembersType(aType).Members[m]);
    if lMember is TPasProperty then
    begin
      SetLength(aProps, Length(aProps) + 1);
      aProps[High(aProps)] := lMember;
    end
    else
      CollectTypeProperties(lMember, aProps);
  end;
end;


// Every property declared by a members type in either section of aModule.
function CollectModuleProperties(aModule: TPasModule): TPasElementArray;

  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
      CollectTypeProperties(TPasElement(aDecls[k]), Result);
  end;

begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  if aModule.InterfaceSection <> nil then
    WalkDecls(aModule.InterfaceSection.Declarations);
  if aModule.ImplementationSection <> nil then
    WalkDecls(aModule.ImplementationSection.Declarations);
end;


{ The routine carrying aProc's statement body — aProc itself or the
  implementation its declaration scope links to; nil when there is none. }
function BodyProcOf(aProc: TPasProcedure): TPasProcedure;
begin
  Result := aProc;
  if (aProc.CustomData is TPasProcedureScope)
    and (TPasProcedureScope(aProc.CustomData).ImplProc <> nil) then
    Result := TPasProcedureScope(aProc.CustomData).ImplProc;
  if (Result.Body = nil) or (Result.Body.Body = nil) then
    Result := nil;
end;


{ True when aExpr names its target plainly or through Self, rather than through
  another object's reference or a with block. }
function IsUnqualifiedOrSelf(aExpr: TPasExpr): boolean;
var
  lBin: TBinaryExpr;
begin
  if (aExpr.CustomData is TResolvedReference)
    and (TResolvedReference(aExpr.CustomData).WithExprScope <> nil) then
    Exit(False);
  if not (aExpr.Parent is TBinaryExpr) then
    Exit(True);
  lBin := TBinaryExpr(aExpr.Parent);
  if (lBin.OpCode <> eopSubIdent) or (lBin.Right <> aExpr) then
    Exit(True);
  Result := (lBin.Left is TPrimitiveExpr)
    and SameText(TPrimitiveExpr(lBin.Left).Value, 'Self');
end;


{ True when aExpr itself carries a resolved write reference to a field aOwner
  declares, named plainly or through Self. }
function IsOwnFieldWrite(aExpr: TPasExpr; aOwner: TPasElement): boolean;
var
  lRef: TResolvedReference;
begin
  Result := False;
  if not (aExpr.CustomData is TResolvedReference) then
    Exit;
  lRef := TResolvedReference(aExpr.CustomData);
  Result := (lRef.Access in rraAllWrite) and IsStorageField(lRef.Declaration)
    and (lRef.Declaration.Parent = aOwner) and IsUnqualifiedOrSelf(aExpr);
end;


{ True when aExpr, or one of its sub-expressions, is a write reference to a
  field aOwner declares. }
function ExprWritesOwnField(aExpr: TPasExpr; aOwner: TPasElement): boolean;
var
  k: integer;
begin
  Result := False;
  if aExpr = nil then
    Exit;
  if IsOwnFieldWrite(aExpr, aOwner) then
    Exit(True);
  if aExpr is TParamsExpr then
  begin
    if ExprWritesOwnField(TParamsExpr(aExpr).Value, aOwner) then
      Exit(True);
    for k := 0 to High(TParamsExpr(aExpr).Params) do
      if ExprWritesOwnField(TParamsExpr(aExpr).Params[k], aOwner) then
        Exit(True);
  end
  else if aExpr is TBinaryExpr then
    Result := ExprWritesOwnField(TBinaryExpr(aExpr).Left, aOwner)
      or ExprWritesOwnField(TBinaryExpr(aExpr).Right, aOwner)
  else if aExpr is TUnaryExpr then
    Result := ExprWritesOwnField(TUnaryExpr(aExpr).Operand, aOwner);
end;


// True when one of aWith's subject expressions writes a field aOwner declares.
function WithExprsWrite(aWith: TPasImplWithDo; aOwner: TPasElement): boolean;
var
  k: integer;
begin
  Result := False;
  if aWith.Expressions = nil then
    Exit;
  for k := 0 to aWith.Expressions.Count - 1 do
    if (TObject(aWith.Expressions[k]) is TPasExpr)
      and ExprWritesOwnField(TPasExpr(aWith.Expressions[k]), aOwner) then
      Exit(True);
end;


{ True when some expression in aStmt's subtree writes a field aOwner declares,
  named plainly or through Self. }
function StmtWritesOwnField(aStmt: TPasImplElement;
  aOwner: TPasElement): boolean;
var
  lKids: TImplStmtArray;
  i: integer;

  // The direct child sub-statements of aNode.
  function ChildStmts(aNode: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aNode = nil then
      Exit;
    if aNode is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aNode).IfBranch);
      AddStmt(TPasImplIfElse(aNode).ElseBranch);
    end
    else if aNode is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aNode).Body)
    else if aNode is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aNode).Body)
    else if aNode is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aNode).Body)
    else if aNode is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aNode).Body)
    else if aNode is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aNode).Body)
    else if aNode is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aNode).Elements);
      AddStmt(TPasImplTry(aNode).FinallyExcept);
      AddStmt(TPasImplTry(aNode).ElseBranch);
    end
    else if aNode is TPasImplBlock then
      AddElems(TPasImplBlock(aNode).Elements);
  end;

  // True when one of aNode's own expressions writes such a field.
  function OwnExprsWrite(aNode: TPasImplElement): boolean;
  begin
    Result := False;
    if aNode is TPasImplAssign then
      Result := ExprWritesOwnField(TPasImplAssign(aNode).Left, aOwner)
        or ExprWritesOwnField(TPasImplAssign(aNode).Right, aOwner)
    else if aNode is TPasImplSimple then
      Result := ExprWritesOwnField(TPasImplSimple(aNode).Expr, aOwner)
    else if aNode is TPasImplIfElse then
      Result := ExprWritesOwnField(TPasImplIfElse(aNode).ConditionExpr, aOwner)
    else if aNode is TPasImplWhileDo then
      Result := ExprWritesOwnField(TPasImplWhileDo(aNode).ConditionExpr, aOwner)
    else if aNode is TPasImplRepeatUntil then
      Result := ExprWritesOwnField(TPasImplRepeatUntil(aNode).ConditionExpr,
        aOwner)
    else if aNode is TPasImplForLoop then
      Result := ExprWritesOwnField(TPasImplForLoop(aNode).VariableName, aOwner)
        or ExprWritesOwnField(TPasImplForLoop(aNode).StartExpr, aOwner)
        or ExprWritesOwnField(TPasImplForLoop(aNode).EndExpr, aOwner)
    else if aNode is TPasImplCaseOf then
      Result := ExprWritesOwnField(TPasImplCaseOf(aNode).CaseExpr, aOwner)
    else if aNode is TPasImplWithDo then
      Result := WithExprsWrite(TPasImplWithDo(aNode), aOwner);
  end;

begin
  Result := False;
  if aStmt = nil then
    Exit;
  if OwnExprsWrite(aStmt) then
    Exit(True);
  lKids := ChildStmts(aStmt);
  for i := 0 to High(lKids) do
    if StmtWritesOwnField(lKids[i], aOwner) then
      Exit(True);
end;


function TFpSonarResolver.TryPublicFieldBackedProperties(
  out aNodes: TPasElementArray; out aNames: TFpSonarStringArray): boolean;
var
  lMod: TPasModule;
  lProps: TPasElementArray;
  lField: TPasElement;
  i: integer;

  { The public or published field one of aProp's accessors resolves to, or nil
    when neither does; classes and objects only. }
  function BackingField(aProp: TPasProperty): TPasElement;
  var
    lAcc: array[0..1] of TPasElement;
    k: integer;
  begin
    Result := nil;
    if not (aProp.Parent is TPasClassType)
      or not (TPasClassType(aProp.Parent).ObjKind in [okClass, okObject]) then
      Exit;
    lAcc[0] := FTopEngine.GetPasPropertyGetter(aProp);
    lAcc[1] := FTopEngine.GetPasPropertySetter(aProp);
    for k := 0 to 1 do
      if IsStorageField(lAcc[k])
        and (lAcc[k].Visibility in [visPublic, visPublished]) then
        Exit(lAcc[k]);
  end;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    lProps := CollectModuleProperties(lMod);
    for i := 0 to High(lProps) do
    begin
      lField := BackingField(TPasProperty(lProps[i]));
      if lField = nil then
        Continue;
      SetLength(aNodes, Length(aNodes) + 1);
      aNodes[High(aNodes)] := lProps[i];
      SetLength(aNames, Length(aNames) + 1);
      aNames[High(aNames)] := lField.Name;
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryWiderAccessorProperties(
  out aNodes: TPasElementArray; out aNames: TFpSonarStringArray): boolean;
var
  lMod: TPasModule;
  lProps: TPasElementArray;
  i: integer;

  { Accessibility order; -1 for a member whose section carries no rank of its
    own. Private and strict protected share a rank, as do public and
    published. }
  function VisRank(aVis: TPasMemberVisibility): integer;
  begin
    case aVis of
      visStrictPrivate: Result := 0;
      visPrivate, visStrictProtected: Result := 1;
      visProtected: Result := 2;
      visPublic, visPublished: Result := 3;
      else
        Result := -1;
    end;
  end;

  // True when a routine accessor of aProp is declared wider than aProp.
  function HasWiderAccessor(aProp: TPasProperty): boolean;
  var
    lAcc: array[0..1] of TPasElement;
    lRank: integer;
    k: integer;
  begin
    Result := False;
    lRank := VisRank(aProp.Visibility);
    if lRank < 0 then
      Exit;
    lAcc[0] := FTopEngine.GetPasPropertyGetter(aProp);
    lAcc[1] := FTopEngine.GetPasPropertySetter(aProp);
    for k := 0 to 1 do
      if (lAcc[k] is TPasProcedure)
        and (VisRank(lAcc[k].Visibility) > lRank) then
        Exit(True);
  end;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    lProps := CollectModuleProperties(lMod);
    for i := 0 to High(lProps) do
    begin
      if not HasWiderAccessor(TPasProperty(lProps[i])) then
        Continue;
      SetLength(aNodes, Length(aNodes) + 1);
      aNodes[High(aNodes)] := lProps[i];
      SetLength(aNames, Length(aNames) + 1);
      aNames[High(aNames)] := lProps[i].Name;
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryPropertyGetterFieldWrites(
  out aNodes: TPasElementArray; out aNames: TFpSonarStringArray): boolean;
var
  lMod: TPasModule;
  lProps: TPasElementArray;
  i: integer;

  // True when aProp's read accessor is a routine whose body writes own state.
  function GetterWritesField(aProp: TPasProperty): boolean;
  var
    lAcc: TPasElement;
    lBody: TPasProcedure;
  begin
    Result := False;
    lAcc := FTopEngine.GetPasPropertyGetter(aProp);
    if not (lAcc is TPasProcedure) then
      Exit;
    lBody := BodyProcOf(TPasProcedure(lAcc));
    if lBody = nil then
      Exit;
    Result := StmtWritesOwnField(lBody.Body.Body, lAcc.Parent);
  end;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    lProps := CollectModuleProperties(lMod);
    for i := 0 to High(lProps) do
    begin
      if not GetterWritesField(TPasProperty(lProps[i])) then
        Continue;
      SetLength(aNodes, Length(aNodes) + 1);
      aNodes[High(aNodes)] := lProps[i];
      SetLength(aNames, Length(aNames) + 1);
      aNames[High(aNames)] := lProps[i].Name;
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryHiddenNonVirtualConstructors(
  out aNodes: TPasElementArray; out aNames: TFpSonarStringArray): boolean;
const
  cOverridable = [pmVirtual, pmDynamic, pmOverride, pmAbstract];
  cVisibleAlongside = [pmOverload, pmReintroduce];
var
  lMod: TPasModule;
  lClasses: TPasElementArray;
  i: integer;

  procedure Add(aEl: TPasElement; const aName: string);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
    SetLength(aNames, Length(aNames) + 1);
    aNames[High(aNames)] := aName;
  end;

  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
      if TObject(aDecls[k]) is TPasClassType then
      begin
        SetLength(lClasses, Length(lClasses) + 1);
        lClasses[High(lClasses)] := TPasElement(aDecls[k]);
      end;
  end;

  // True when aClass declares a virtual or dynamic method of its own.
  function HasOverridableMethod(aClass: TPasClassType): boolean;
  var
    lMember: TPasElement;
    m: integer;
  begin
    Result := False;
    for m := 0 to aClass.Members.Count - 1 do
    begin
      lMember := TPasElement(aClass.Members[m]);
      if (lMember is TPasProcedure)
        and ([pmVirtual, pmDynamic] * TPasProcedure(lMember).Modifiers <> []) then
        Exit(True);
    end;
  end;

  { Walks aDesc's resolved ancestor-scope chain; True iff aBase is reached.
    An unresolved chain never reaches it, so it is not a descendant. }
  function Descends(aDesc, aBase: TPasClassType): boolean;
  var
    lScope: TPasClassScope;
    lGuard: integer;
  begin
    Result := False;
    if not (aDesc.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aDesc.CustomData).AncestorScope;
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      if lScope.Element = aBase then
        Exit(True);
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
  end;

  // True when another class of the module descends aClass hiding its aName.
  function DescendantRedeclares(aClass: TPasClassType;
    const aName: string): boolean;
  var
    lOther: TPasClassType;
    lMember: TPasElement;
    j, m: integer;
  begin
    Result := False;
    for j := 0 to High(lClasses) do
    begin
      lOther := TPasClassType(lClasses[j]);
      if (lOther = aClass) or (lOther.Members = nil)
        or not Descends(lOther, aClass) then
        Continue;
      for m := 0 to lOther.Members.Count - 1 do
      begin
        lMember := TPasElement(lOther.Members[m]);
        if (lMember is TPasConstructor)
          and not (lMember is TPasClassConstructor)
          and SameText(lMember.Name, aName)
          // pmOverload joins the ancestor's overload set; pmReintroduce marks
          // the hiding as declared. TryMethodHidesVirtual skips both.
          and (cVisibleAlongside * TPasProcedure(lMember).Modifiers = []) then
          Exit(True);
      end;
    end;
  end;

  procedure Consider(aClass: TPasClassType);
  var
    lMember: TPasElement;
    m: integer;
  begin
    if (aClass.Members = nil) or not HasOverridableMethod(aClass) then
      Exit;
    for m := 0 to aClass.Members.Count - 1 do
    begin
      lMember := TPasElement(aClass.Members[m]);
      if not (lMember is TPasConstructor)
        or (lMember is TPasClassConstructor) then
        Continue;
      if cOverridable * TPasProcedure(lMember).Modifiers <> [] then
        Continue;
      if DescendantRedeclares(aClass, lMember.Name) then
        Add(lMember, aClass.Name + '.' + lMember.Name);
    end;
  end;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    for i := 0 to High(lClasses) do
      Consider(TPasClassType(lClasses[i]));
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryIfThenMisuse(aNode: TPasElement;
  out aSubject: string): boolean;

  // The resolved declaration an expression's reference points to, or nil.
  function DeclOf(aExpr: TPasExpr): TPasElement;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData).Declaration
    else
      Result := nil;
  end;

  { The subject expression of a condition: the argument of Assigned(x),
    or the non-nil operand of `x <> nil` / `x = nil`; nil for any other shape.}
  function GuardSubject(aCond: TPasExpr): TPasExpr;
  var
    lp: TParamsExpr;
    lb: TBinaryExpr;
  begin
    Result := nil;
    if aCond = nil then
      Exit;
    if (aCond is TParamsExpr) and (TParamsExpr(aCond).Kind = pekFuncParams)
      and (Length(TParamsExpr(aCond).Params) = 1) then
    begin
      lp := TParamsExpr(aCond);
      if (lp.Value is TPrimitiveExpr)
        and SameText(TPrimitiveExpr(lp.Value).Value, 'Assigned') then
        Exit(lp.Params[0]);
    end;
    if (aCond is TBinaryExpr)
      and (TBinaryExpr(aCond).OpCode in [eopEqual, eopNotEqual]) then
    begin
      lb := TBinaryExpr(aCond);
      if lb.Right is TNilExpr then
        Exit(lb.Left);
      if lb.Left is TNilExpr then
        Exit(lb.Right);
    end;
  end;

  { True iff aDecl appears as a base operand anywhere in aExpr, by resolved-decl
    identity. Recurses the expression tree }
  function OperandUsesDecl(aExpr: TPasExpr; aDecl: TPasElement): boolean;
  var
    i: integer;
  begin
    Result := False;
    if (aExpr = nil) or (aDecl = nil) then
      Exit;
    if DeclOf(aExpr) = aDecl then
      Exit(True);
    if aExpr is TBinaryExpr then
      Result := OperandUsesDecl(TBinaryExpr(aExpr).Left, aDecl)
        or OperandUsesDecl(TBinaryExpr(aExpr).Right, aDecl)
    else if aExpr is TUnaryExpr then
      Result := OperandUsesDecl(TUnaryExpr(aExpr).Operand, aDecl)
    else if aExpr is TParamsExpr then
    begin
      Result := OperandUsesDecl(TParamsExpr(aExpr).Value, aDecl);
      if not Result then
        for i := 0 to High(TParamsExpr(aExpr).Params) do
          if OperandUsesDecl(TParamsExpr(aExpr).Params[i], aDecl) then
            Exit(True);
    end;
  end;

var
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lProc: TPasProcedure;
  lMod: TPasModule;
  lSubjExpr: TPasExpr;
  lSubjDecl: TPasElement;
begin
  aSubject := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) < 3) then
    Exit;

  try
    // Callee identity by MODULE: the resolved function must be named IfThen AND owned by Math or StrUtils.
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(lRef.Declaration);
    if not SameText(lProc.Name, 'IfThen') then
      Exit;
    lMod := lProc.GetModule;
    if (lMod = nil)
      or not (SameText(lMod.Name, 'Math') or SameText(lMod.Name, 'StrUtils')) then
      Exit;
    // The condition's guarded subject (resolved declaration).
    lSubjExpr := GuardSubject(lParams.Params[0]);
    if lSubjExpr = nil then
      Exit;
    lSubjDecl := DeclOf(lSubjExpr);
    if lSubjDecl = nil then
      Exit;
    // The subject must reappear as a base operand in a value argument.
    if OperandUsesDecl(lParams.Params[1], lSubjDecl)
      or OperandUsesDecl(lParams.Params[2], lSubjDecl) then
    begin
      aSubject := lSubjDecl.Name;
      Result := True;
    end;
  except
    on E: Exception do
    begin
      aSubject := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryAssertCall(aNode: TPasElement;
  out aArgCount: integer): boolean;
var
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lDecl: TPasElement;
begin
  aArgCount := 0;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekFuncParams then
    Exit;

  try
    { Built-in identity: the callee must resolve to a built-in
      symbol whose built-in proc data is the Assert built-in.}
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or (lRef.Declaration = nil) then
      Exit;
    lDecl := lRef.Declaration;
    if not (lDecl is TPasUnresolvedSymbolRef) then
      Exit;
    if not (lDecl.CustomData is TResElDataBuiltInProc) then
      Exit;
    if TResElDataBuiltInProc(lDecl.CustomData).BuiltIn <> bfAssert then
      Exit;
    aArgCount := Length(lParams.Params);
    Result := True;
  except
    on E: Exception do
    begin
      aArgCount := 0;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryDefaultFormatSettingsDateCall(aNode: TPasElement;
  out aRoutineName: string): boolean;
const
  // The curated locale-sensitive date/time routines..
  cDateRoutines: array[0..9] of string =
    ('FormatDateTime', 'DateToStr', 'TimeToStr', 'DateTimeToStr', 'StrToDate',
    'StrToTime', 'StrToDateTime', 'StrToDateDef', 'StrToTimeDef',
    'StrToDateTimeDef');

// The enclosing module of aEl (walks Parent), or nil.
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

var
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lProc: TPasProcedure;
  lMod: TPasModule;
  lArgs: TFPList;
  lArg: TPasArgument;
  lRes: TPasResolverResult;
  lNameOk, lTakesSettings: boolean;
  i: integer;
begin
  aRoutineName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekFuncParams then
    Exit;

  try
    { Callee identity by resolved declaration + curated name + SysUtils module:
      a user routine of the same name resolves to a proc in a different module, so it never matches. }
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(lRef.Declaration);
    lNameOk := False;
    for i := Low(cDateRoutines) to High(cDateRoutines) do
      if SameText(lProc.Name, cDateRoutines[i]) then
      begin
        lNameOk := True;
        Break;
      end;
    if not lNameOk then
      Exit;
    lMod := OwningModule(lProc);
    if (lMod = nil) or not (SameText(lMod.Name, 'SysUtils')
      or SameText(lMod.Name, 'System.SysUtils')) then
      Exit;

    { The bound overload's authoritative fact: the resolver already selected the
      overload, so inspect its argument list for a TFormatSettings parameter }
    lTakesSettings := False;
    lArgs := lProc.ProcType.Args;
    if lArgs <> nil then
      for i := 0 to lArgs.Count - 1 do
      begin
        lArg := TPasArgument(lArgs[i]);
        if lArg.ArgType = nil then
          Continue;
        FTopEngine.ComputeElement(lArg.ArgType, lRes, [rcType]);
          { Name and owning module (SysUtils, never spelling): a user type that
            merely shares the name TFormatSettings must not be mistaken for the RTL
            record. }
        if (lRes.LoTypeEl <> nil)
          and SameText(lRes.LoTypeEl.Name, 'TFormatSettings') then
        begin
          lMod := OwningModule(lRes.LoTypeEl);
          if (lMod <> nil) and (SameText(lMod.Name, 'SysUtils')
            or SameText(lMod.Name, 'System.SysUtils')) then
          begin
            lTakesSettings := True;
            Break;
          end;
        end;
      end;
    if lTakesSettings then
      Exit;
    aRoutineName := lProc.Name;
    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable date-call fact.
    on E: Exception do
    begin
      aRoutineName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryImplicitTEncodingDefault(aNode: TPasElement): boolean;
const
  { The curated byte/string-conversion  }
  cEncRoutines: array[0..5] of string =
    ('LoadFromFile', 'SaveToFile', 'Create', 'ReadAllText', 'WriteAllText',
    'GetText');

  { The curated owning (module, class) pairs the routine must belong to:
    a user routine of the same name resolves to a proc in a different module/class, so it never matches. }
  cEncClasses: array[0..4, 0..1] of string =
    (('Classes', 'TStringList'), ('Classes', 'TStrings'),
    ('Classes', 'TStreamReader'), ('Classes', 'TStreamWriter'),
    ('IOUtils', 'TFile'));

// The enclosing module of aEl (walks Parent), or nil.
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

  { True iff aProc's bound argument list declares a TEncoding (SysUtils) parameter.}
  function TakesEncoding(aProc: TPasProcedure): boolean;
  var
    lArgs: TFPList;
    lArg: TPasArgument;
    lRes: TPasResolverResult;
    lMod: TPasModule;
    i: integer;
  begin
    Result := False;
    if (aProc = nil) or (aProc.ProcType = nil) or (aProc.ProcType.Args = nil) then
      Exit;
    lArgs := aProc.ProcType.Args;
    for i := 0 to lArgs.Count - 1 do
    begin
      lArg := TPasArgument(lArgs[i]);
      if lArg.ArgType = nil then
        Continue;
      FTopEngine.ComputeElement(lArg.ArgType, lRes, [rcType]);
      if (lRes.LoTypeEl <> nil)
        and SameText(lRes.LoTypeEl.Name, 'TEncoding') then
      begin
        lMod := OwningModule(lRes.LoTypeEl);
        if (lMod <> nil) and (SameText(lMod.Name, 'SysUtils')
          or SameText(lMod.Name, 'System.SysUtils')) then
          Exit(True);
      end;
    end;
  end;

var
  lParams: TParamsExpr;
  lResolved: TPasResolverResult;
  lProc: TPasProcedure;
  lClass: TPasClassType;
  lMod: TPasModule;
  lScope: TPasClassScope;
  lId: TPasIdentifier;
  lNameOk, lClassOk, lSiblingHasEnc: boolean;
  lGuard, i: integer;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekFuncParams then
    Exit;

  try
    { These are MEMBER calls (sl.LoadFromFile): Params.Value is a member access
      whose CustomData is nil, so the free-function ref path returns nil. }
    FTopEngine.ComputeElement(lParams.Value, lResolved, [rcNoImplicitProc]);
    if lResolved.BaseType <> btProc then
      Exit;
    if not (lResolved.IdentEl is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(lResolved.IdentEl);

    // Curated routine name (never spelling).
    lNameOk := False;
    for i := Low(cEncRoutines) to High(cEncRoutines) do
      if SameText(lProc.Name, cEncRoutines[i]) then
      begin
        lNameOk := True;
        Break;
      end;
    if not lNameOk then
      Exit;

    // The owning class+module must be a curated pair (resolved identity).
    if not (lProc.Parent is TPasClassType) then
      Exit;
    lClass := TPasClassType(lProc.Parent);
    lMod := OwningModule(lClass);
    lClassOk := False;
    if lMod <> nil then
      for i := Low(cEncClasses) to High(cEncClasses) do
        if SameText(lMod.Name, cEncClasses[i, 0])
          and SameText(lClass.Name, cEncClasses[i, 1]) then
        begin
          lClassOk := True;
          Break;
        end;
    if not lClassOk then
      Exit;

    { The bound overload must omit TEncoding — else the explicit, compliant
      overload was selected }
    if TakesEncoding(lProc) then
      Exit;

    { a sibling overload of the same name  must declare a TEncoding parameter,
      otherwise the omission is not actionable.
      Walk the class scope chain; for each scope, walk the same-name identifier
      chain enumerating the overload group. }
    if not (lClass.CustomData is TPasClassScope) then
      Exit;
    lSiblingHasEnc := False;
    lScope := TPasClassScope(lClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) and not lSiblingHasEnc do
    begin
      lId := lScope.FindIdentifier(lProc.Name);
      while (lId <> nil) and not lSiblingHasEnc do
      begin
        if (lId.Element is TPasProcedure)
          and TakesEncoding(TPasProcedure(lId.Element)) then
          lSiblingHasEnc := True;
        lId := lId.NextSameIdentifier;
      end;
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
    if not lSiblingHasEnc then
      Exit;

    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable encoding fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TrySingleOverloadOfMathFunction(aNode: TPasElement): boolean;
const
  { The curated standard math routines whose Single-precision overload silently
    loses precision when a higher-precision sibling is available.}
  cMathRoutines: array[0..12] of string =
    ('Sqrt', 'Sin', 'Cos', 'Tan', 'ArcTan', 'ArcSin', 'ArcCos', 'Ln', 'Log10',
    'Log2', 'Exp', 'Power', 'Hypot');

// The enclosing module of aEl (walks Parent), or nil.
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

  // True iff aProc's first bound argument resolves to the built-in aBase type
  function FirstArgIs(aProc: TPasProcedure; aBase: TResolverBaseType): boolean;
  var
    lArgs: TFPList;
    lArg: TPasArgument;
  begin
    Result := False;
    if (aProc = nil) or (aProc.ProcType = nil) or (aProc.ProcType.Args = nil) then
      Exit;
    lArgs := aProc.ProcType.Args;
    if lArgs.Count < 1 then
      Exit;
    lArg := TPasArgument(lArgs[0]);
    if lArg.ArgType = nil then
      Exit;
    Result := FTopEngine.IsBaseType(lArg.ArgType, aBase, True);
  end;

var
  lParams: TParamsExpr;
  lResolved: TPasResolverResult;
  lProc: TPasProcedure;
  lMod: TPasModule;
  lScopes: array[0..1] of TPasIdentifierScope;
  lScope: TPasIdentifierScope;
  lId: TPasIdentifier;
  lNameOk, lHigherSibling: boolean;
  i, j: integer;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekFuncParams then
    Exit;

  try
    { Resolve the bound overload via ComputeElement — handles the unqualified
      Sqrt(x) and the qualified Math.Power(...) forms uniformly }
    FTopEngine.ComputeElement(lParams.Value, lResolved, [rcNoImplicitProc]);
    if lResolved.BaseType <> btProc then
      Exit;
    if not (lResolved.IdentEl is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(lResolved.IdentEl);

    // Curated routine name.
    lNameOk := False;
    for i := Low(cMathRoutines) to High(cMathRoutines) do
      if SameText(lProc.Name, cMathRoutines[i]) then
      begin
        lNameOk := True;
        Break;
      end;
    if not lNameOk then
      Exit;

    // The owning module must be System or Math.
    lMod := OwningModule(lProc);
    if (lMod = nil)
      or not (SameText(lMod.Name, 'Math') or SameText(lMod.Name, 'System')
      or SameText(lMod.Name, 'System.Math')) then
      Exit;

    // The bound overload's first argument must be the built-in Single — i.e. a
    // Single value fed the call and the Single overload was selected.
    if not FirstArgIs(lProc, btSingle) then
      Exit;

    { a sibling overload of the same name, visible in the SAME unit section scope,
      must declare a Double/Extended first argument — so a higher-precision
      form was available and the Single binding loses precision for no reason.}
    lScopes[0] := nil;
    lScopes[1] := nil;
    if (lProc.Parent is TPasSection)
      and (TPasSection(lProc.Parent).CustomData is TPasIdentifierScope) then
      lScopes[0] := TPasIdentifierScope(TPasSection(lProc.Parent).CustomData);
    if (lMod.InterfaceSection <> nil)
      and (lMod.InterfaceSection.CustomData is TPasIdentifierScope) then
      lScopes[1] := TPasIdentifierScope(lMod.InterfaceSection.CustomData);
    lHigherSibling := False;
    for j := 0 to High(lScopes) do
    begin
      lScope := lScopes[j];
      if lScope = nil then
        Continue;
      lId := lScope.FindIdentifier(lProc.Name);
      while (lId <> nil) and not lHigherSibling do
      begin
        if (lId.Element is TPasProcedure)
          and (FirstArgIs(TPasProcedure(lId.Element), btDouble)
          or FirstArgIs(TPasProcedure(lId.Element), btExtended)) then
          lHigherSibling := True;
        lId := lId.NextSameIdentifier;
      end;
      if lHigherSibling then
        Break;
    end;
    if not lHigherSibling then
      Exit;

    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable precision fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryNonExhaustiveEnumCase(aNode: TPasElement;
  out aMissing: TFpSonarStringArray): boolean;

  // The index of aValue in aEnum.Values by element identity, or -1.
  function IndexOfValue(aEnum: TPasEnumType; aValue: TPasElement): integer;
  var
    k: integer;
  begin
    Result := -1;
    for k := 0 to aEnum.Values.Count - 1 do
      if TObject(aEnum.Values[k]) = aValue then
        Exit(k);
  end;

  // Resolves aLabel to one of aEnum's value indices, or -1 when it does not
  // resolve to an enum value of aEnum (an unresolvable / foreign label).
  function LabelIndex(aEnum: TPasEnumType; aLabel: TPasExpr): integer;
  var
    lRes: TPasResolverResult;
  begin
    Result := -1;
    FTopEngine.ComputeElement(aLabel, lRes, []);
    if lRes.IdentEl is TPasEnumValue then
      Result := IndexOfValue(aEnum, lRes.IdentEl);
  end;

var
  lCaseOf: TPasImplCaseOf;
  lRes: TPasResolverResult;
  lEnum: TPasEnumType;
  lCovered: array of boolean;
  lStmt: TPasImplCaseStatement;
  lLabel: TPasExpr;
  lLo, lHi, lIdx, i, j, k: integer;
begin
  aMissing := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplCaseOf) then
    Exit;
  lCaseOf := TPasImplCaseOf(aNode);

  try
    // An else/otherwise (even empty) signals author intent => never report.
    if lCaseOf.ElseBranch <> nil then
      Exit;
    { The selector must resolve to an enumerated type }
    FTopEngine.ComputeElement(lCaseOf.CaseExpr, lRes, []);
    if lRes.BaseType <> btContext then
      Exit;
    if not (lRes.LoTypeEl is TPasEnumType) then
      Exit;
    lEnum := TPasEnumType(lRes.LoTypeEl);
    if (lEnum.Values = nil) or (lEnum.Values.Count = 0) then
      Exit;

    SetLength(lCovered, lEnum.Values.Count);
    for i := 0 to High(lCovered) do
      lCovered[i] := False;

    // Walk every case branch's labels; an unresolvable label aborts the whole query.
    for i := 0 to lCaseOf.Elements.Count - 1 do
      if TObject(lCaseOf.Elements[i]) is TPasImplCaseStatement then
      begin
        lStmt := TPasImplCaseStatement(lCaseOf.Elements[i]);
        if lStmt.Expressions = nil then
          Continue;
        for j := 0 to lStmt.Expressions.Count - 1 do
        begin
          lLabel := TPasExpr(lStmt.Expressions[j]);
          // A range label (cRed..cGreen) covers the inclusive ordinal span.
          if (lLabel is TBinaryExpr)
            and (TBinaryExpr(lLabel).Kind = pekRange) then
          begin
            lLo := LabelIndex(lEnum, TBinaryExpr(lLabel).Left);
            lHi := LabelIndex(lEnum, TBinaryExpr(lLabel).Right);
            if (lLo < 0) or (lHi < 0) then
              Exit;
            if lLo > lHi then
            begin
              k := lLo;
              lLo := lHi;
              lHi := k;
            end;
            for k := lLo to lHi do
              lCovered[k] := True;
          end
          else
          begin
            lIdx := LabelIndex(lEnum, lLabel);
            if lIdx < 0 then
              Exit;
            lCovered[lIdx] := True;
          end;
        end;
      end;

    // Collect the uncovered value names in declaration order.
    for i := 0 to High(lCovered) do
      if not lCovered[i] then
      begin
        SetLength(aMissing, Length(aMissing) + 1);
        aMissing[High(aMissing)] := TPasEnumValue(lEnum.Values[i]).Name;
      end;
    Result := Length(aMissing) > 0;
    if not Result then
      aMissing := nil;
  except
    on E: Exception do
    begin
      aMissing := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryDiscardedExceptionConstruction(aNode: TPasElement;
  out aClassName: string): boolean;
const
  cExceptionBase = 'Exception';

  function AsClassType(aType: TPasType): TPasClassType;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aType <> nil) and (lGuard < 100) do
    begin
      if aType is TPasClassType then
        Exit(TPasClassType(aType));
      if aType is TPasAliasType then
        aType := TPasAliasType(aType).DestType
      else
        Exit(nil);
      Inc(lGuard);
    end;
  end;

  // True iff aClass's ancestry provably reaches the exception base.
  function DescendsFromException(aClass: TPasClassType): boolean;
  var
    lCls: TPasClassType;
    lNext: TPasType;
    lGuard: integer;
  begin
    Result := False;
    lCls := aClass;
    lGuard := 0;
    while (lCls <> nil) and (lGuard < 100) do
    begin
      if SameText(lCls.Name, cExceptionBase) then
        Exit(True);
      lNext := lCls.AncestorType;
      if lNext = nil then
        Exit(False);
      lCls := AsClassType(lNext);
      Inc(lGuard);
    end;
  end;

var
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lClass: TPasClassType;
  lDecl: TPasElement;
begin
  aClassName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekFuncParams then
    Exit;

  try
    { The call must be a constructor new-instance }
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (rrfNewInstance in lRef.Flags) then
      Exit;

    // Recover the constructed class.
    lClass := nil;
    if lRef.Context is TResolvedRefCtxConstructor then
      lClass := AsClassType(TResolvedRefCtxConstructor(lRef.Context).Typ);
    if (lClass = nil) and (lRef.Declaration is TPasConstructor) then
    begin
      lDecl := lRef.Declaration;
      while (lDecl <> nil) and not (lDecl is TPasClassType) do
        lDecl := lDecl.Parent;
      if lDecl is TPasClassType then
        lClass := TPasClassType(lDecl);
    end;
    if lClass = nil then
      Exit;

    if not DescendsFromException(lClass) then
      Exit;
    aClassName := lClass.Name;
    Result := True;
  except
    on E: Exception do
    begin
      aClassName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryHandlerCatchesRawException(
  aNode: TPasElement): boolean;
const
  cExceptionBase = 'Exception';
  cObjectBase = 'TObject';

{ Follows an alias chain to the underlying class type, or nil when the type is not a resolved class }
  function AsClassType(aType: TPasType): TPasClassType;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aType <> nil) and (lGuard < 100) do
    begin
      if aType is TPasClassType then
        Exit(TPasClassType(aType));
      if aType is TPasAliasType then
        aType := TPasAliasType(aType).DestType
      else
        Exit(nil);
      Inc(lGuard);
    end;
  end;

var
  lOn: TPasImplExceptOn;
  lClass: TPasClassType;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplExceptOn) then
    Exit;
  lOn := TPasImplExceptOn(aNode);
  if lOn.TypeEl = nil then
    Exit;

  try
    { exact-clas-match: catching the root masks every
      unrelated failure, but catching a specific subclass is correct.}
    lClass := AsClassType(lOn.TypeEl);
    Result := (lClass <> nil)
      and (SameText(lClass.Name, cExceptionBase)
      or SameText(lClass.Name, cObjectBase));
  except
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryRaisesRawException(aNode: TPasElement): boolean;
const
  cExceptionBase = 'Exception';
  cObjectBase = 'TObject';

// Alias chain to the underlying class type (copied locally, as above).
  function AsClassType(aType: TPasType): TPasClassType;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aType <> nil) and (lGuard < 100) do
    begin
      if aType is TPasClassType then
        Exit(TPasClassType(aType));
      if aType is TPasAliasType then
        aType := TPasAliasType(aType).DestType
      else
        Exit(nil);
      Inc(lGuard);
    end;
  end;

var
  lRaise: TPasImplRaise;
  lExpr: TPasExpr;
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lClass: TPasClassType;
  lDecl: TPasElement;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplRaise) then
    Exit;
  lRaise := TPasImplRaise(aNode);
  lExpr := lRaise.ExceptObject;
  if lExpr = nil then
    Exit; // bare re-raise (raise;) — never flagged

  try
    // Only the constructor new-instance form is recovered;
    if not (lExpr is TParamsExpr) then
      Exit;
    lParams := TParamsExpr(lExpr);
    if lParams.Kind <> pekFuncParams then
      Exit;
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (rrfNewInstance in lRef.Flags) then
      Exit;

    { Recover the constructed class: prefer the constructed-type context,
      else the constructor declaration's owning class. }
    lClass := nil;
    if lRef.Context is TResolvedRefCtxConstructor then
      lClass := AsClassType(TResolvedRefCtxConstructor(lRef.Context).Typ);
    if (lClass = nil) and (lRef.Declaration is TPasConstructor) then
    begin
      lDecl := lRef.Declaration;
      while (lDecl <> nil) and not (lDecl is TPasClassType) do
        lDecl := lDecl.Parent;
      if lDecl is TPasClassType then
        lClass := TPasClassType(lDecl);
    end;

    // exact-root-match: 'raise EFoo.Create' is compliant even when thin;
    Result := (lClass <> nil)
      and (SameText(lClass.Name, cExceptionBase)
      or SameText(lClass.Name, cObjectBase));
  except
    // Tolerant-access: any resolver failure => no usable raised-class fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryHandlerShadowedByEarlier(aNode: TPasElement;
  out aHandlerName, aAncestorName: string): boolean;

// Alias chain to the underlying class type (copied locally, as above).
  function AsClassType(aType: TPasType): TPasClassType;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aType <> nil) and (lGuard < 100) do
    begin
      if aType is TPasClassType then
        Exit(TPasClassType(aType));
      if aType is TPasAliasType then
        aType := TPasAliasType(aType).DestType
      else
        Exit(nil);
      Inc(lGuard);
    end;
  end;

  { True iff aChild IS aAncestor or reaches it over a fully resolved ancestor chain. }
  function DescendsOrEquals(aChild, aAncestor: TPasClassType): boolean;
  var
    lCls: TPasClassType;
    lGuard: integer;
  begin
    Result := False;
    lCls := aChild;
    lGuard := 0;
    while (lCls <> nil) and (lGuard < 100) do
    begin
      if lCls = aAncestor then
        Exit(True);
      lCls := AsClassType(lCls.AncestorType);
      Inc(lGuard);
    end;
  end;

var
  lOn, lEarlier: TPasImplExceptOn;
  lParent: TPasImplTryExcept;
  lClass, lEarlierClass: TPasClassType;
  i: integer;
begin
  aHandlerName := '';
  aAncestorName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplExceptOn) then
    Exit;
  lOn := TPasImplExceptOn(aNode);
  if (lOn.TypeEl = nil) or not (lOn.Parent is TPasImplTryExcept) then
    Exit;
  lParent := TPasImplTryExcept(lOn.Parent);
  if lParent.Elements = nil then
    Exit;

  try
    lClass := AsClassType(lOn.TypeEl);
    if lClass = nil then
      Exit;
    for i := 0 to lParent.Elements.Count - 1 do
    begin
      // The scan stops at aNode: only its earlier siblings are examined.
      if TObject(lParent.Elements[i]) = lOn then
        Exit;
      if not (TObject(lParent.Elements[i]) is TPasImplExceptOn) then
        Continue;
      lEarlier := TPasImplExceptOn(lParent.Elements[i]);
      lEarlierClass := AsClassType(lEarlier.TypeEl);
      if lEarlierClass = nil then
        Continue;
      if DescendsOrEquals(lClass, lEarlierClass) then
      begin
        // Names are reported as written at the handler, not alias-collapsed.
        aHandlerName := lOn.TypeEl.Name;
        if aHandlerName = '' then
          aHandlerName := lClass.Name;
        aAncestorName := lEarlier.TypeEl.Name;
        if aAncestorName = '' then
          aAncestorName := lEarlierClass.Name;
        Exit(True);
      end;
    end;
  except
    // Tolerant-access: any resolver failure => no usable ancestry fact.
    on E: Exception do
    begin
      aHandlerName := '';
      aAncestorName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryRaisedClassNotException(aNode: TPasElement;
  out aClassName: string): boolean;
const
  cExceptionBase = 'Exception';
  cObjectBase = 'TObject';

// Alias chain to the underlying class type (copied locally, as above).
  function AsClassType(aType: TPasType): TPasClassType;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aType <> nil) and (lGuard < 100) do
    begin
      if aType is TPasClassType then
        Exit(TPasClassType(aType));
      if aType is TPasAliasType then
        aType := TPasAliasType(aType).DestType
      else
        Exit(nil);
      Inc(lGuard);
    end;
  end;

  { True iff aClass's resolved ancestor-scope chain reaches TObject without
    passing through Exception. A chain ending anywhere else yields False. }
  function ReachesObjectOnly(aClass: TPasClassType): boolean;
  var
    lScope: TPasClassScope;
    lGuard: integer;
  begin
    Result := False;
    if not (aClass.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      if lScope.Element <> nil then
      begin
        if SameText(lScope.Element.Name, cExceptionBase) then
          Exit(False);
        if SameText(lScope.Element.Name, cObjectBase) then
          Exit(True);
      end;
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
  end;

var
  lRaise: TPasImplRaise;
  lExpr: TPasExpr;
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lClass: TPasClassType;
  lDecl: TPasElement;
begin
  aClassName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplRaise) then
    Exit;
  lRaise := TPasImplRaise(aNode);
  lExpr := lRaise.ExceptObject;
  if lExpr = nil then
    Exit; // bare re-raise (raise;) — never flagged

  try
    // Only the constructor new-instance form is recovered;
    if not (lExpr is TParamsExpr) then
      Exit;
    lParams := TParamsExpr(lExpr);
    if lParams.Kind <> pekFuncParams then
      Exit;
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or not (rrfNewInstance in lRef.Flags) then
      Exit;

    { Recover the constructed class: prefer the constructed-type context,
      else the constructor declaration's owning class. }
    lClass := nil;
    if lRef.Context is TResolvedRefCtxConstructor then
      lClass := AsClassType(TResolvedRefCtxConstructor(lRef.Context).Typ);
    if (lClass = nil) and (lRef.Declaration is TPasConstructor) then
    begin
      lDecl := lRef.Declaration;
      while (lDecl <> nil) and not (lDecl is TPasClassType) do
        lDecl := lDecl.Parent;
      if lDecl is TPasClassType then
        lClass := TPasClassType(lDecl);
    end;
    if (lClass = nil) or SameText(lClass.Name, cObjectBase) then
      Exit;

    if not ReachesObjectOnly(lClass) then
      Exit;
    aClassName := lClass.Name;
    Result := True;
  except
    // Tolerant-access: any resolver failure => no usable ancestry fact.
    on E: Exception do
    begin
      aClassName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryAssertControlFlowOperand(aNode: TPasElement;
  out aRoutineName: string): boolean;

  { The name of the first user-declared routine an expression subtree CALLS,
    or ''. A reference that is not an implicit call — the operand of @, or an
    Assigned argument — does not count. }
  function CalledRoutine(aExpr: TPasExpr; aDepth: integer): string;
  var
    lRef: TResolvedReference;
    i: integer;
  begin
    Result := '';
    if (aExpr = nil) or (aDepth >= 100) then
      Exit;
    if aExpr.CustomData is TResolvedReference then
    begin
      lRef := TResolvedReference(aExpr.CustomData);
      if (lRef.Declaration is TPasProcedure)
        and not (rrfNoImplicitCallWithoutParams in lRef.Flags) then
        Exit(lRef.Declaration.Name);
    end;
    if aExpr is TParamsExpr then
    begin
      Result := CalledRoutine(TParamsExpr(aExpr).Value, aDepth + 1);
      for i := 0 to High(TParamsExpr(aExpr).Params) do
      begin
        if Result <> '' then
          Break;
        Result := CalledRoutine(TParamsExpr(aExpr).Params[i], aDepth + 1);
      end;
    end
    else if aExpr is TBinaryExpr then
    begin
      Result := CalledRoutine(TBinaryExpr(aExpr).Left, aDepth + 1);
      if Result = '' then
        Result := CalledRoutine(TBinaryExpr(aExpr).Right, aDepth + 1);
    end
    else if aExpr is TUnaryExpr then
      Result := CalledRoutine(TUnaryExpr(aExpr).Operand, aDepth + 1);
  end;

var
  lArgCount: integer;
begin
  aRoutineName := '';
  Result := False;
  if not TryAssertCall(aNode, lArgCount) or (lArgCount = 0) then
    Exit;

  try
    aRoutineName := CalledRoutine(TParamsExpr(aNode).Params[0], 0);
    Result := aRoutineName <> '';
  except
    // Tolerant-access: any resolver failure => no usable operand fact.
    on E: Exception do
    begin
      aRoutineName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryLoopControlFlow(aNode: TPasElement;
  out aKind: TFpSonarLoopExitKind): boolean;

  // The TResolvedReference attached to an expression, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

var
  lExpr: TPasExpr;
  lRef: TResolvedReference;
  lDecl: TPasElement;
begin
  aKind := lekNone;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aNode).Expr;
  if lExpr = nil then
    Exit;

  try
    // The callee reference is on the exit(x) call's Value, or on the bare ident statement itself .
    if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
      lRef := FTopEngine.GetParamsValueRef(TParamsExpr(lExpr))
    else
      lRef := RefOf(lExpr);
    if (lRef = nil) or (lRef.Declaration = nil) then
      Exit;
    lDecl := lRef.Declaration;
    // Built-in identity: the callee must resolve to a built-in control-flow proc.
    // A user 'procedure Break;' resolves to an ordinary proc.
    if not (lDecl is TPasUnresolvedSymbolRef) then
      Exit;
    if not (lDecl.CustomData is TResElDataBuiltInProc) then
      Exit;
    case TResElDataBuiltInProc(lDecl.CustomData).BuiltIn of
      bfBreak: aKind := lekBreak;
      bfContinue: aKind := lekContinue;
      bfExit: aKind := lekExit;
      else
        Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      aKind := lekNone;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryPascalStyleResultAssign(aNode: TPasElement;
  out aFuncName: string): boolean;

  { The source spelling of an LHS identifier reference, or '' for a non-ident shape}
  function LeftName(aExpr: TPasExpr): string;
  begin
    if aExpr is TPrimitiveExpr then
      Result := TPrimitiveExpr(aExpr).Value
    else
      Result := '';
  end;

var
  lAssign: TPasImplAssign;
  lEl: TPasElement;
  lFunc: TPasFunction;
  lRes: TPasResolverResult;
  lGuard: integer;
  lName: string;
  lShort: string;
  lDot: integer;
begin
  aFuncName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aNode);

  try
    // Walk the AST Parent chain to the nearest enclosing routine.
    lEl := aNode.Parent;
    lGuard := 0;
    while (lEl <> nil) and not (lEl is TPasProcedure) and (lGuard < 200) do
    begin
      lEl := lEl.Parent;
      Inc(lGuard);
    end;
    // It must be a function, and not an operator.
    if not (lEl is TPasFunction) then
      Exit;
    if lEl is TPasOperator then
      Exit;
    lFunc := TPasFunction(lEl);
    { Identity decision: the LHS must resolve to the function's Result element.
      fcl-passrc collapses both `Result := x` and the legacy `<FuncName> := x`
      onto the result element, so the resolved declaration alone cannot tell them apart }
    FTopEngine.ComputeElement(lAssign.Left, lRes, []);
    if not (lRes.IdentEl is TPasResultElement) then
      Exit;
    { The two result-targeting forms differ ONLY in spelling: the `Result`
      pseudo-variable vs the function's own name (the legacy form). }
    lName := LeftName(lAssign.Left);
    if SameText(lName, 'Result') then
      Exit;
    lShort := lFunc.Name;
    lDot := LastDelimiter('.', lShort);
    if lDot > 0 then
      lShort := Copy(lShort, lDot + 1, MaxInt);
    if SameText(lName, lShort) then
    begin
      aFuncName := lFunc.Name;
      Result := True;
    end;
  except
    // Tolerant-access: any resolver failure => not a usable fact.
    on E: Exception do
    begin
      aFuncName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryRedundantAssignedCheckBeforeFree(
  aNode: TPasElement): boolean;
const
  // The recognizeFreeAndNil read-path is deferred: ships wrapper-owned True.
  cRecognizeFreeAndNil = True;

// The resolved declaration an expression's reference points to, or nil.
  function DeclOf(aExpr: TPasExpr): TPasElement;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData).Declaration
    else
      Result := nil;
  end;

  { The guarded subject of a condition: the argument of Assigned(X) }
  function GuardSubject(aCond: TPasExpr): TPasExpr;
  var
    lp: TParamsExpr;
    lb: TBinaryExpr;
    lRef: TResolvedReference;
  begin
    Result := nil;
    if aCond = nil then
      Exit;
    if (aCond is TParamsExpr) and (TParamsExpr(aCond).Kind = pekFuncParams)
      and (Length(TParamsExpr(aCond).Params) = 1) then
    begin
      lp := TParamsExpr(aCond);
      lRef := FTopEngine.GetParamsValueRef(lp);
      if (lRef <> nil) and (lRef.Declaration is TPasUnresolvedSymbolRef)
        and (lRef.Declaration.CustomData is TResElDataBuiltInProc)
        and (TResElDataBuiltInProc(lRef.Declaration.CustomData).BuiltIn
        = bfAssigned) then
        Exit(lp.Params[0]);
    end;
    if (aCond is TBinaryExpr) and (TBinaryExpr(aCond).OpCode = eopNotEqual) then
    begin
      lb := TBinaryExpr(aCond);
      if lb.Right is TNilExpr then
        Exit(lb.Left);
      if lb.Left is TNilExpr then
        Exit(lb.Right);
    end;
  end;

var
  lIf: TPasImplIfElse;
  lBranch: TPasImplElement;
  lBlock: TPasImplBeginBlock;
  lExpr, lInner, lSubjExpr: TPasExpr;
  lKind: TFpSonarFreeKind;
  lSubjDecl, lFreedDecl: TPasElement;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplIfElse) then
    Exit;
  lIf := TPasImplIfElse(aNode);
  if lIf.ElseBranch <> nil then
    Exit;

  try
    // The single if-branch statement: a direct TPasImplSimple, or the sole
    // statement of a begin-block (a multi-statement branch is not redundant).
    lBranch := lIf.IfBranch;
    if lBranch is TPasImplBeginBlock then
    begin
      lBlock := TPasImplBeginBlock(lBranch);
      if lBlock.Elements.Count <> 1 then
        Exit;
      lBranch := TPasImplElement(lBlock.Elements[0]);
    end;
    if not (lBranch is TPasImplSimple) then
      Exit;
    lExpr := TPasImplSimple(lBranch).Expr;

    { Reuse the existing free-call detection: it confirms `obj.Free` /
      `FreeAndNil(obj)` by TObject/SysUtils identity and hands back the freed
      object expression — do not re-derive Free/FreeAndNil identity. }
    lKind := TryFreeCall(lExpr, lInner);
    if lKind = lfkNone then
      Exit;
    if (lKind = lfkFreeAndNil) and (not cRecognizeFreeAndNil) then
      Exit;

    lSubjExpr := GuardSubject(lIf.ConditionExpr);
    if lSubjExpr = nil then
      Exit;

    // The guard subject and the freed object must be the SAME resolved
    // declaration; a mere name match ('if Assigned(a) then b.Free') is correct.
    lSubjDecl := DeclOf(lSubjExpr);
    lFreedDecl := DeclOf(lInner);
    if (lSubjDecl <> nil) and (lSubjDecl = lFreedDecl) then
      Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryDisposesSelf(aNode: TPasElement): boolean;

  // True when aExpr's reference is scoped by an enclosing with block.
  function InWithScope(aExpr: TPasExpr): boolean;
  begin
    Result := (aExpr.CustomData is TResolvedReference)
      and (TResolvedReference(aExpr.CustomData).WithExprScope <> nil);
  end;

  { True iff aProc is a method whose owning class is TObject or a descendant of it }
  function OwnerIsTObjectClass(aProc: TPasProcedure): boolean;
  var
    lCls: TPasClassType;
    lNext: TPasType;
    lGuard: integer;
  begin
    Result := False;
    if (aProc = nil) or not (aProc.Parent is TPasClassType) then
      Exit;
    lCls := TPasClassType(aProc.Parent);
    lGuard := 0;
    while (lCls <> nil) and (lGuard < 100) do
    begin
      if SameText(lCls.Name, 'TObject') then
        Exit(True);
      lNext := lCls.AncestorType;
      if not (lNext is TPasClassType) then
        Exit;
      lCls := TPasClassType(lNext);
      Inc(lGuard);
    end;
  end;

var
  lExpr, lInner: TPasExpr;
  lEl, lDecl: TPasElement;
  lSelf: TPasArgument;
  lGuard: integer;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aNode).Expr;
  if lExpr = nil then
    Exit;

  try
    // Walk the AST Parent chain to the nearest enclosing routine.
    lEl := aNode.Parent;
    lGuard := 0;
    while (lEl <> nil) and not (lEl is TPasProcedure) and (lGuard < 200) do
    begin
      lEl := lEl.Parent;
      Inc(lGuard);
    end;
    if not (lEl is TPasProcedure) then
      Exit;
    { The routine's own Self argument: the resolved identity every arm compares
      against, and nil for anything that is not a method. }
    if not (lEl.CustomData is TPasProcedureScope) then
      Exit;
    lSelf := TPasProcedureScope(lEl.CustomData).SelfArg;
    if lSelf = nil then
      Exit;

    // `Self.Free` and `FreeAndNil(Self)`: the disposal's operand must be Self.
    if TryFreeCall(lExpr, lInner) <> lfkNone then
      Exit(ReferencedDecl(lInner) = TPasElement(lSelf));

    { An unqualified `Free`: the receiver is the implicit Self, so there is no
      operand expression and the reference sits on the callee itself. }
    if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams)
      and (Length(TParamsExpr(lExpr).Params) = 0) then
      lExpr := TParamsExpr(lExpr).Value;
    if not (lExpr is TPrimitiveExpr) then
      Exit;
    // In `with aObj do Free` the receiver is the with expression, not Self.
    if InWithScope(lExpr) then
      Exit;
    lDecl := ReferencedDecl(lExpr);
    if not (lDecl is TPasProcedure) then
      Exit;
    Result := SameText(lDecl.Name, 'Free')
      and OwnerIsTObjectClass(TPasProcedure(lDecl));
  except
    // Tolerant-access: any resolver failure => not a usable fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryMemoryOpCall(aNode: TPasElement;
  out aOp: TFpSonarMemoryOp; out aOperand: TPasElement;
  out aOperandName: string): boolean;

  // The enclosing module of aEl (walks Parent), or nil.
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

var
  lExpr: TPasExpr;
  lParams: TParamsExpr;
  lRef: TResolvedReference;
  lDecl: TPasElement;
  lMod: TPasModule;
  lMatched: boolean;
begin
  aOp := lmoNew;
  aOperand := nil;
  aOperandName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplSimple) then
    Exit;
  lExpr := TPasImplSimple(aNode).Expr;
  if not (lExpr is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(lExpr);
  if (lParams.Kind <> pekFuncParams) or (Length(lParams.Params) < 1) then
    Exit;

  try
    lRef := FTopEngine.GetParamsValueRef(lParams);
    if (lRef = nil) or (lRef.Declaration = nil) then
      Exit;
    lDecl := lRef.Declaration;
    lMatched := False;
    { New and Dispose are compiler built-ins; GetMem and FreeMem have no built-in
      at all and are recognised by routine-plus-module identity. }
    if (lDecl is TPasUnresolvedSymbolRef)
      and (lDecl.CustomData is TResElDataBuiltInProc) then
    begin
      case TResElDataBuiltInProc(lDecl.CustomData).BuiltIn of
        bfNew:
          begin
            aOp := lmoNew;
            lMatched := True;
          end;
        bfDispose:
          begin
            aOp := lmoDispose;
            lMatched := True;
          end;
      end;
    end
    else if lDecl is TPasProcedure then
    begin
      lMod := OwningModule(lDecl);
      if (lMod <> nil) and (SameText(lMod.Name, 'System')
        or SameText(lMod.Name, 'SysUtils')) then
      begin
        if SameText(lDecl.Name, 'GetMem') then
        begin
          aOp := lmoGetMem;
          lMatched := True;
        end
        else if SameText(lDecl.Name, 'FreeMem') then
        begin
          aOp := lmoFreeMem;
          lMatched := True;
        end;
      end;
    end;
    if not lMatched then
      Exit;
    aOperand := ReferencedDecl(lParams.Params[0]);
    if aOperand = nil then
      Exit;
    aOperandName := aOperand.Name;
    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable fact.
    on E: Exception do
    begin
      aOp := lmoNew;
      aOperand := nil;
      aOperandName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryExplicitDefaultArrayProperty(aNode: TPasElement;
  out aPropName: string): boolean;

  // The effective default-property flag: True iff IsDefault is set on aProp or, on
  // a redeclaration that sets neither flag, on the nearest ancestor that does.
  function EffectiveIsDefault(aProp: TPasProperty): boolean;
  var
    lProp: TPasProperty;
    lGuard: integer;
  begin
    Result := False;
    lProp := aProp;
    lGuard := 0;
    while (lProp <> nil) and (lGuard < 100) do
    begin
      if lProp.IsDefault then
        Exit(True);
      if lProp.IsNodefault then
        Exit(False);
      lProp := FTopEngine.GetPasPropertyAncestor(lProp);
      Inc(lGuard);
    end;
  end;

var
  lParams: TParamsExpr;
  lRes: TPasResolverResult;
  lProp: TPasProperty;
  lBin: TBinaryExpr;
begin
  aPropName := '';
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekArrayParams then
    Exit;

  try
    // The accessed Value must resolve to a class default ARRAY property.
    FTopEngine.ComputeElement(lParams.Value, lRes, []);
    if lRes.BaseType <> btArrayProperty then
      Exit;
    if not (lRes.IdentEl is TPasProperty) then
      Exit;
    lProp := TPasProperty(lRes.IdentEl);
    if not EffectiveIsDefault(lProp) then
      Exit;
    { The access must name the property.
      A bare Obj[i] has a Value that resolves to the object,
      not the property  identifier, so it does not reach here. }
    if not ((lParams.Value is TBinaryExpr)
      and (TBinaryExpr(lParams.Value).OpCode = eopSubIdent)) then
      Exit;
    lBin := TBinaryExpr(lParams.Value);
    if not (lBin.Right.CustomData is TResolvedReference) then
      Exit;
    if TResolvedReference(lBin.Right.CustomData).Declaration <> lProp then
      Exit;
    aPropName := lProp.Name;
    Result := True;
  except
    on E: Exception do
    begin
      aPropName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryStringFirstCharByIndex(aNode: TPasElement): boolean;
var
  lParams: TParamsExpr;
  lRes: TPasResolverResult;
  lVal: TResEvalValue;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekArrayParams then
    Exit;
  if Length(lParams.Params) <> 1 then
    Exit;

  try
    // The indexed operand must resolve to a string type.
    FTopEngine.ComputeElement(lParams.Value, lRes, []);
    if not (lRes.BaseType in btAllStrings) then
      Exit;
    { The single index must const-fold to an integer equal to the low bound; a
      variable index (S[i]) folds to nil and degrades to silence. The eval value
      is released in a finally (the TryFormatCall discipline), so the leak guard
      stays at 0. }
    lVal := FTopEngine.ExprEvaluator.Eval(lParams.Params[0], [refConst]);
    try
      // The folded index may be signed or unsigned (integer kind is revkInt OR
      // revkUInt) — accept either ordinal form.
      if lVal <> nil then
        case lVal.Kind of
          revkInt: Result := TResEvalInt(lVal).Int = cLowIndex;
          revkUInt: Result := TResEvalUInt(lVal).UInt = cLowIndex;
        end;
    finally
      ReleaseEvalValue(lVal);
    end;
  except
    // Tolerant-access: any resolver failure => not a usable string-index fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryListLastByIndex(aNode: TPasElement): boolean;

  // The enclosing module of aEl (walks Parent), or nil.
  function OwningModule(aEl: TPasElement): TPasModule;
  var
    lGuard: integer;
  begin
    Result := nil;
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl is TPasModule then
        Exit(TPasModule(aEl));
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
  end;

  // The declaration aExpr resolves to (its TResolvedReference target), or nil.
  function DeclOf(aExpr: TPasElement): TPasElement;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData).Declaration
    else
      Result := nil;
  end;

  // True iff aClass IS, or descends from, a curated list type (class name +
  // owning module — never name alone). Walks the resolved ancestor-scope chain.
  function IsKnownList(aClass: TPasClassType): boolean;
  var
    lScope: TPasClassScope;
    lMod: TPasModule;
    lGuard: integer;
  begin
    Result := False;
    if not (aClass.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      if (lScope.Element <> nil)
        and SameText(lScope.Element.Name, cListClassName) then
      begin
        lMod := OwningModule(lScope.Element);
        if (lMod <> nil) and SameText(lMod.Name, cListModuleName) then
          Exit(True);
      end;
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
  end;

  // True iff a 'Last' member (a property or routine) is reachable on aClass's
  // resolved scope chain (the suggested-fix-is-valid secondary gate).
  function HasLast(aClass: TPasClassType): boolean;
  var
    lScope: TPasClassScope;
    lId: TPasIdentifier;
    lGuard: integer;
  begin
    Result := False;
    if not (aClass.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      lId := lScope.FindIdentifier('Last');
      if (lId <> nil) and (lId.Element <> nil)
        and ((lId.Element is TPasProperty) or (lId.Element is TPasProcedure)) then
        Exit(True);
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
  end;

var
  lParams: TParamsExpr;
  lRecv, lLhs: TPasResolverResult;
  lSub, lDot: TBinaryExpr;
  lVal: TResEvalValue;
  lIdxIsOne: boolean;
begin
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TParamsExpr) then
    Exit;
  lParams := TParamsExpr(aNode);
  if lParams.Kind <> pekArrayParams then
    Exit;
  if Length(lParams.Params) <> 1 then
    Exit;

  try
    // The indexed receiver must resolve to a confirmed list class (or descendant)
    // that also exposes a Last member.
    FTopEngine.ComputeElement(lParams.Value, lRecv, []);
    if not (lRecv.LoTypeEl is TPasClassType) then
      Exit;
    if not IsKnownList(TPasClassType(lRecv.LoTypeEl)) then
      Exit;
    if not HasLast(TPasClassType(lRecv.LoTypeEl)) then
      Exit;

    // The index must be the exact '<recv>.Count - 1' pattern: a subtract whose
    // RHS const-folds to integer 1.
    if not (lParams.Params[0] is TBinaryExpr) then
      Exit;
    lSub := TBinaryExpr(lParams.Params[0]);
    if lSub.OpCode <> eopSubtract then
      Exit;
    lVal := FTopEngine.ExprEvaluator.Eval(lSub.right, [refConst]);
    lIdxIsOne := False;
    try
      // The folded RHS may be signed or unsigned.
      if lVal <> nil then
        case lVal.Kind of
          revkInt: lIdxIsOne := TResEvalInt(lVal).Int = 1;
          revkUInt: lIdxIsOne := TResEvalUInt(lVal).UInt = 1;
        end;
    finally
      ReleaseEvalValue(lVal);
    end;
    if not lIdxIsOne then
      Exit;

    // The subtract LHS must be '<recv>.Count' — a member access (eopSubIdent)
    // resolving to a Count property.
    if not (lSub.left is TBinaryExpr) then
      Exit;
    lDot := TBinaryExpr(lSub.left);
    if lDot.OpCode <> eopSubIdent then
      Exit;
    FTopEngine.ComputeElement(lDot, lLhs, []);
    if not (lLhs.IdentEl is TPasProperty) then
      Exit;
    if not SameText(lLhs.IdentEl.Name, 'Count') then
      Exit;

    { Same-receiver identity: the instance the .Count
      is read on must be the SAME declaration as the indexed instance }
    if (DeclOf(lParams.Value) = nil)
      or (DeclOf(lParams.Value) <> DeclOf(lDot.left)) then
      Exit;

    Result := True;
  except
    // Tolerant-access: any resolver failure => not a usable list-index fact.
    on E: Exception do
      Result := False;
  end;
end;


function TFpSonarResolver.TryLoopBeyondCollectionEnd(aNode: TPasElement;
  out aOverrun: TPasElement): boolean;
var
  lFor: TPasImplForLoop;
  lLoopVar: TPasElement;
  lAccesses: TPasExprArray;
  lMutated: boolean;

// The declaration aExpr resolves to (ComputeElement.IdentEl), or nil.
  function DeclOf(aExpr: TPasElement): TPasElement;
  var
    lRes: TPasResolverResult;
  begin
    Result := nil;
    if aExpr = nil then
      Exit;
    try
      FTopEngine.ComputeElement(aExpr, lRes, []);
      Result := lRes.IdentEl;
    except
      on E: Exception do
        Result := nil;
    end;
  end;

  { Const-folds aExpr to an integer via the evaluator.
    False ona non-constant/non-integer/unevaluable expression.  }
  function FoldInt(aExpr: TPasExpr; out aVal: int64): boolean;
  var
    lVal: TResEvalValue;
  begin
    Result := False;
    aVal := 0;
    if aExpr = nil then
      Exit;
    try
      lVal := FTopEngine.ExprEvaluator.Eval(aExpr, [refConst]);
      try
        if lVal <> nil then
          case lVal.Kind of
            revkInt:
            begin
              aVal := TResEvalInt(lVal).Int;
              Result := True;
            end;
            revkUInt:
            begin
              aVal := int64(TResEvalUInt(lVal).UInt);
              Result := True;
            end;
          end;
      finally
        ReleaseEvalValue(lVal);
      end;
    except
      on E: Exception do
        Result := False;
    end;
  end;

  { The valid index range [aLo, aHi] of a STATIC array type via its const-folded Ranges[0] }
  function StaticArrayRange(aType: TPasType; out aLo, aHi: int64): boolean;
  var
    lArr: TPasArrayType;
    lVal: TResEvalValue;
  begin
    Result := False;
    aLo := 0;
    aHi := 0;
    if not (aType is TPasArrayType) then
      Exit;
    lArr := TPasArrayType(aType);
    if Length(lArr.Ranges) = 0 then
      Exit;
    try
      lVal := FTopEngine.ExprEvaluator.Eval(lArr.Ranges[0], [refConst]);
      try
        if (lVal <> nil) and (lVal.Kind = revkRangeInt) then
        begin
          aLo := TResEvalRangeInt(lVal).RangeStart;
          aHi := TResEvalRangeInt(lVal).RangeEnd;
          Result := True;
        end;
      finally
        ReleaseEvalValue(lVal);
      end;
    except
      on E: Exception do
        Result := False;
    end;
  end;

  // Appends every pekArrayParams indexed-access node in aExpr's tree to aList.
  procedure CollectAccesses(aExpr: TPasExpr; var aList: TPasExprArray);
  var
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TParamsExpr then
    begin
      if TParamsExpr(aExpr).Kind = pekArrayParams then
      begin
        SetLength(aList, Length(aList) + 1);
        aList[High(aList)] := aExpr;
      end;
      CollectAccesses(TParamsExpr(aExpr).Value, aList);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        CollectAccesses(TParamsExpr(aExpr).Params[k], aList);
    end
    else if aExpr is TBinaryExpr then
    begin
      CollectAccesses(TBinaryExpr(aExpr).left, aList);
      CollectAccesses(TBinaryExpr(aExpr).right, aList);
    end
    else if aExpr is TUnaryExpr then
      CollectAccesses(TUnaryExpr(aExpr).Operand, aList);
  end;

  { Descends the loop body, collecting indexed accesses and
    flagging a mutation of the loop variable into lMutated. }
  procedure ScanStmt(aStmt: TPasImplElement);
  var
    lEls: TFPList;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      if DeclOf(TPasImplAssign(aStmt).left) = lLoopVar then
        lMutated := True;
      CollectAccesses(TPasImplAssign(aStmt).left, lAccesses);
      CollectAccesses(TPasImplAssign(aStmt).right, lAccesses);
    end
    else if aStmt is TPasImplSimple then
      CollectAccesses(TPasImplSimple(aStmt).Expr, lAccesses)
    else if aStmt is TPasImplIfElse then
    begin
      CollectAccesses(TPasImplIfElse(aStmt).ConditionExpr, lAccesses);
      ScanStmt(TPasImplIfElse(aStmt).IfBranch);
      ScanStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
    begin
      CollectAccesses(TPasImplWhileDo(aStmt).ConditionExpr, lAccesses);
      ScanStmt(TPasImplWhileDo(aStmt).Body);
    end
    else if aStmt is TPasImplForLoop then
    begin
      CollectAccesses(TPasImplForLoop(aStmt).StartExpr, lAccesses);
      CollectAccesses(TPasImplForLoop(aStmt).EndExpr, lAccesses);
      ScanStmt(TPasImplForLoop(aStmt).Body);
    end
    else if aStmt is TPasImplWithDo then
      ScanStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      ScanStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplRepeatUntil then
    begin
      CollectAccesses(TPasImplRepeatUntil(aStmt).ConditionExpr, lAccesses);
      lEls := TPasImplBlock(aStmt).Elements;
      if lEls <> nil then
        for k := 0 to lEls.Count - 1 do
          if TObject(lEls[k]) is TPasImplElement then
            ScanStmt(TPasImplElement(lEls[k]));
    end
    else if aStmt is TPasImplCaseOf then
    begin
      CollectAccesses(TPasImplCaseOf(aStmt).CaseExpr, lAccesses);
      lEls := TPasImplBlock(aStmt).Elements;
      if lEls <> nil then
        for k := 0 to lEls.Count - 1 do
          if TObject(lEls[k]) is TPasImplElement then
            ScanStmt(TPasImplElement(lEls[k]));
    end
    else if aStmt is TPasImplBlock then
    begin
      lEls := TPasImplBlock(aStmt).Elements;
      if lEls <> nil then
        for k := 0 to lEls.Count - 1 do
          if TObject(lEls[k]) is TPasImplElement then
            ScanStmt(TPasImplElement(lEls[k]));
    end;
  end;

  // The reachable interval [aOutLo, aOutHi] of an index expression that is the
  // loop variable, or loopvar +/- a constant;
  function IndexInterval(aIdx: TPasExpr; aLo, aHi: int64;
    out aOutLo, aOutHi: int64): boolean;
  var
    lBin: TBinaryExpr;
    lK: int64;
  begin
    Result := False;
    aOutLo := 0;
    aOutHi := 0;
    if aIdx = nil then
      Exit;
    // bare loopvar
    if DeclOf(aIdx) = lLoopVar then
    begin
      aOutLo := aLo;
      aOutHi := aHi;
      Exit(True);
    end;
    { loopvar +/- const (loopvar must be the left operand of a subtract;}
    if aIdx is TBinaryExpr then
    begin
      lBin := TBinaryExpr(aIdx);
      if not (lBin.OpCode in [eopAdd, eopSubtract]) then
        Exit;
      if (DeclOf(lBin.left) = lLoopVar) and FoldInt(lBin.right, lK) then
      begin
        if lBin.OpCode = eopAdd then
        begin
          aOutLo := aLo + lK;
          aOutHi := aHi + lK;
        end
        else
        begin
          aOutLo := aLo - lK;
          aOutHi := aHi - lK;
        end;
        Exit(True);
      end
      else if (lBin.OpCode = eopAdd) and (DeclOf(lBin.right) = lLoopVar)
        and FoldInt(lBin.left, lK) then
      begin
        aOutLo := aLo + lK;
        aOutHi := aHi + lK;
        Exit(True);
      end;
    end;
  end;

  // The resolved base-type / type element of aExpr, never raising. Returns False
  // (and leaves the out params untouched) on any caught exception.
  function ResolveBase(aExpr: TPasExpr; out aBase: TResolverBaseType;
    out aTypeEl: TPasType): boolean;
  var
    lRes: TPasResolverResult;
  begin
    Result := False;
    aBase := btNone;
    aTypeEl := nil;
    try
      FTopEngine.ComputeElement(aExpr, lRes, []);
      aBase := lRes.BaseType;
      aTypeEl := lRes.LoTypeEl;
      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  end;

var
  lStartVal, lEndVal, lLoopLo, lLoopHi: int64;
  lAccLo, lAccHi, lArrLo, lArrHi: int64;
  lAcc: TParamsExpr;
  lBase: TResolverBaseType;
  lTypeEl: TPasType;
  i: integer;
begin
  aOverrun := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aNode = nil) then
    Exit;
  if not (aNode is TPasImplForLoop) then
    Exit;
  lFor := TPasImplForLoop(aNode);
  // A counted loop only (for..in is ltIn with EndExpr=nil => never flagged).
  if not (lFor.LoopType in [ltNormal, ltDown]) then
    Exit;
  if lFor.EndExpr = nil then
    Exit;

  try
    // The loop-variable declaration drives the same-variable index identity check.
    lLoopVar := DeclOf(lFor.VariableName);
    if lLoopVar = nil then
      Exit;

    // The reachable integer interval [lLoopLo, lLoopHi]. A bound that is neither
    // const-foldable nor a recognized constant idiom => abstain (silence).
    if not FoldInt(lFor.StartExpr, lStartVal) then
      Exit;
    if not FoldInt(lFor.EndExpr, lEndVal) then
      Exit;
    if lFor.LoopType = ltDown then
    begin
      lLoopLo := lEndVal;
      lLoopHi := lStartVal;
    end
    else
    begin
      lLoopLo := lStartVal;
      lLoopHi := lEndVal;
    end;
    { An empty interval (reversed constant bounds, e.g. `for i := 5 to 2`) means
      the body never executes, so no access can overrun => abstain (never report
      on a loop that provably never runs). }
    if lLoopLo > lLoopHi then
      Exit;

    // Collect body accesses + detect loop-variable mutation (the FP guard: a
    // mutated loop variable invalidates the syntactic interval).
    lMutated := False;
    SetLength(lAccesses, 0);
    ScanStmt(lFor.Body);
    if lMutated then
      Exit;

    for i := 0 to High(lAccesses) do
    begin
      lAcc := TParamsExpr(lAccesses[i]);
      if Length(lAcc.Params) <> 1 then
        Continue;
      // The index must be loopvar or loopvar +/- const;
      if not IndexInterval(lAcc.Params[0], lLoopLo, lLoopHi, lAccLo, lAccHi) then
        Continue;
      // The indexed base must resolve to a static array with a constant range.
      if not ResolveBase(lAcc.Value, lBase, lTypeEl) then
        Continue;
      if lBase <> btContext then
        Continue;
      if not StaticArrayRange(lTypeEl, lArrLo, lArrHi) then
        Continue;
      // The first proven overrun (high or low side) is the finding.
      if (lAccHi > lArrHi) or (lAccLo < lArrLo) then
      begin
        aOverrun := lAcc;
        Result := True;
        Exit;
      end;
    end;
  except
    on E: Exception do
    begin
      aOverrun := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryLoopVarReadAfterLoop(aNode: TPasElement;
  out aVarName: string; out aSite: TPasElement): boolean;
var
  lFor: TPasImplForLoop;
  lVar: TPasElement;
  lNodes, lDecls: TPasElementArray;
  lRef: TResolvedReference;
  lRecounts: TImplRowArray;
  lLoopRow, lRow, lBestRow: integer;
  i: integer;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // True when aEl is aRoot, lies below it, or the walk bound was exhausted.
  function Under(aEl, aRoot: TPasElement): boolean;
  var
    lGuard: integer;
  begin
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if aEl = aRoot then
        Exit(True);
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
    Result := aEl <> nil;
  end;

  { True when an enclosing for-loop counts lVar again, or the walk bound was
    exhausted. }
  function Recounted(aEl: TPasElement): boolean;
  var
    lGuard: integer;
  begin
    lGuard := 0;
    while (aEl <> nil) and (lGuard < 200) do
    begin
      if (aEl is TPasImplForLoop)
        and (ReferencedDecl(TPasImplForLoop(aEl).VariableName) = lVar) then
        Exit(True);
      aEl := aEl.Parent;
      Inc(lGuard);
    end;
    Result := aEl <> nil;
  end;

  // The direct child sub-statements of aStmt
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // Appends the row of every for-loop below aStmt that counts lVar, aNode aside.
  procedure CollectRecounts(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if (aStmt is TPasImplForLoop) and (aStmt <> aNode)
      and (ReferencedDecl(TPasImplForLoop(aStmt).VariableName) = lVar) then
    begin
      SetLength(lRecounts, Length(lRecounts) + 1);
      lRecounts[High(lRecounts)] := SourceRow(aStmt);
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      CollectRecounts(lKids[k]);
  end;

  // True when a loop counting lVar sits between the loop under test and aRow.
  function Interposed(aRow: integer): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(lRecounts) do
      if (lRecounts[k] > lLoopRow) and (lRecounts[k] < aRow) then
        Exit(True);
  end;

begin
  aVarName := '';
  aSite := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded)
    or not (aNode is TPasImplForLoop) then
    Exit;
  lFor := TPasImplForLoop(aNode);
  if not (lFor.LoopType in [ltNormal, ltDown]) then
    Exit;
  try
    lVar := ReferencedDecl(lFor.VariableName);
    if (lVar = nil) or not (lVar is TPasVariable) or (lVar is TPasProperty)
      or not (lVar.Parent is TProcedureBody) then
      Exit;
    if not TryReferenceSites(lNodes, lDecls) then
      Exit;
    lLoopRow := SourceRow(lFor);
    SetLength(lRecounts, 0);
    CollectRecounts(TProcedureBody(lVar.Parent).Body);
    lBestRow := 0;
    for i := 0 to High(lNodes) do
    begin
      if (lDecls[i] <> lVar) or Under(lNodes[i], lFor)
        or Recounted(lNodes[i]) then
        Continue;
      lRow := SourceRow(lNodes[i]);
      if (lRow <= lLoopRow) or Interposed(lRow) then
        Continue;
      lRef := RefOf(lNodes[i]);
      if (lRef = nil) or not (lRef.Access in rraAllRead) then
        Continue;
      if (aSite = nil) or (lRow < lBestRow) then
      begin
        aSite := lNodes[i];
        lBestRow := lRow;
      end;
    end;
    if aSite <> nil then
    begin
      aVarName := lVar.Name;
      Result := True;
    end;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aVarName := '';
      aSite := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryShadowedUnqualifiedRefs(
  out aNodes: TPasElementArray): boolean;
const
  cMinSources = 2;
  cIgnoreUnits: array[0..1] of string = ('System', 'SysUtils');
var
  lMod: TPasModule;
  lCands: TPasElementArray;

  procedure Add(var aArr: TPasElementArray; aEl: TPasElement);
  begin
    SetLength(aArr, Length(aArr) + 1);
    aArr[High(aArr)] := aEl;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // Appends every identifier-reference leaf in aExpr's tree to lCands, descending the shared PasTree shapes.
  procedure CollectRefs(aExpr: TPasExpr);
  var
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TPrimitiveExpr then
    begin
      if RefOf(aExpr) <> nil then
        Add(lCands, aExpr);
    end
    else if aExpr is TParamsExpr then
    begin
      CollectRefs(TParamsExpr(aExpr).Value);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        CollectRefs(TParamsExpr(aExpr).Params[k]);
    end
    else if aExpr is TBinaryExpr then
    begin
      CollectRefs(TBinaryExpr(aExpr).left);
      CollectRefs(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      CollectRefs(TUnaryExpr(aExpr).Operand);
  end;

  // The direct child sub-statements of aStmt.
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // Collects the identifier references held by aStmt's control/value expressions.
  // recurses every sub-statement.
  procedure CollectStmtRefs(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      CollectRefs(TPasImplAssign(aStmt).Left);
      CollectRefs(TPasImplAssign(aStmt).Right);
    end
    else if aStmt is TPasImplSimple then
      CollectRefs(TPasImplSimple(aStmt).Expr)
    else if aStmt is TPasImplIfElse then
      CollectRefs(TPasImplIfElse(aStmt).ConditionExpr)
    else if aStmt is TPasImplWhileDo then
      CollectRefs(TPasImplWhileDo(aStmt).ConditionExpr)
    else if aStmt is TPasImplRepeatUntil then
      CollectRefs(TPasImplRepeatUntil(aStmt).ConditionExpr)
    else if aStmt is TPasImplForLoop then
    begin
      CollectRefs(TPasImplForLoop(aStmt).StartExpr);
      CollectRefs(TPasImplForLoop(aStmt).EndExpr);
    end
    else if aStmt is TPasImplCaseOf then
      CollectRefs(TPasImplCaseOf(aStmt).CaseExpr)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            CollectRefs(TPasExpr(lWith.Expressions[k]));
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      CollectStmtRefs(lKids[k]);
  end;

  // Walks aDecls for body-bearing routines, collecting their statement
  // references and recursing into nested routines.
  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if (lEl is TPasProcedure) and (TPasProcedure(lEl).Body <> nil) then
      begin
        lProc := TPasProcedure(lEl);
        if lProc.Body.Body <> nil then
          CollectStmtRefs(lProc.Body.Body);
        WalkDecls(lProc.Body.Declarations);
      end;
    end;
  end;

  // True iff aName is in the ignoreUnits set.
  function IsIgnored(const aName: string): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := Low(cIgnoreUnits) to High(cIgnoreUnits) do
      if SameText(aName, cIgnoreUnits[k]) then
        Exit(True);
  end;

  { True iff aNode is already qualified }
  function IsQualified(aNode: TPasElement): boolean;
  var
    lRef: TResolvedReference;
    lParent: TPasElement;
  begin
    Result := False;
    lRef := RefOf(aNode);
    if (lRef <> nil) and (rrfDotScope in lRef.Flags) then
      Exit(True);
    lParent := aNode.Parent;
    if (lParent is TBinaryExpr) and (TBinaryExpr(lParent).OpCode = eopSubIdent)
      and (TBinaryExpr(lParent).right = aNode) then
      Exit(True);
  end;

  { The number of distinct in-scope source modules whose own interface exports aSimple }
  function ShadowCount(const aSimple: string): integer;
  var
    lScopes, lMods: TFPList;

    procedure AddSection(aSec: TPasSection);
    var
      lSc: TPasSectionScope;
      j: integer;
    begin
      if (aSec = nil) or not (aSec.CustomData is TPasSectionScope) then
        Exit;
      lSc := TPasSectionScope(aSec.CustomData);
      for j := 0 to lSc.UsesScopes.Count - 1 do
        if lScopes.IndexOf(lSc.UsesScopes[j]) < 0 then
          lScopes.Add(lSc.UsesScopes[j]);
    end;

  var
    j: integer;
    lUse: TPasSectionScope;
    lId: TPasIdentifier;
    lOwner: TPasModule;
  begin
    Result := 0;
    lScopes := TFPList.Create;
    lMods := TFPList.Create;
    try
      AddSection(lMod.InterfaceSection);
      AddSection(lMod.ImplementationSection);
      for j := 0 to lScopes.Count - 1 do
      begin
        if not (TObject(lScopes[j]) is TPasSectionScope) then
          Continue;
        lUse := TPasSectionScope(lScopes[j]);
        lId := lUse.FindLocalIdentifier(aSimple);
        if (lId = nil) or (lId.Element = nil) then
          Continue;
        lOwner := lId.Element.GetModule;
        if (lOwner <> nil) and (lMods.IndexOf(lOwner) < 0) then
        begin
          lMods.Add(lOwner);
          Inc(Result);
        end;
      end;
    finally
      lScopes.Free;
      lMods.Free;
    end;
  end;

var
  i: integer;
  lNode: TPasElement;
  lRef: TResolvedReference;
  lDecl: TPasElement;
  lOwner: TPasModule;
begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    SetLength(lCands, 0);
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);

    for i := 0 to High(lCands) do
    begin
      lNode := lCands[i];
      lRef := RefOf(lNode);
      if (lRef = nil) or (lRef.Declaration = nil) then
        Continue;
      lDecl := lRef.Declaration;
      // A unit-name reference (the Unit in Unit.Symbol) is not a shadowable
      // symbol.
      if lDecl is TPasModule then
        Continue;
      lOwner := lDecl.GetModule;
      { A built-in pseudo-function / operator has no real module  }
      if (lOwner = nil) or (lOwner = lMod) or IsIgnored(lOwner.Name) then
        Continue;
      // An already-qualified reference (Unit.Symbol) or a Self/instance member.
      if IsQualified(lNode) then
        Continue;
      // Shadowable only when >= cMinSources distinct in-scope units export the simple name (onlyWhenAmbiguous=True).
      if ShadowCount(TPrimitiveExpr(lNode).Value) >= cMinSources then
        Add(aNodes, lNode);
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryMovableInterfaceUnits(
  out aNodes: TPasElementArray): boolean;
const
  cTreatInlineBodiesAsInterface = True;
var
  lMod: TPasModule;
  lRefMods: TFPList;
  lUnresolved: boolean;

// True iff aName is in the (currently empty) ignoreUnits set.
  function IsIgnored(const aName: string): boolean;
  begin
    Result := False;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // Records aDecl's owning module in the interface reference set..
  procedure NoteDecl(aDecl: TPasElement);
  var
    lOwner: TPasModule;
  begin
    if aDecl = nil then
    begin
      lUnresolved := True;
      Exit;
    end;
    lOwner := aDecl.GetModule;
    if (lOwner <> nil) and (lRefMods.IndexOf(lOwner) < 0) then
      lRefMods.Add(lOwner);
  end;

  { Records a named type's owning module, following the type-node component chains:
    every one is a re-export trap through which an interface unit can supply a type.
    An unresolved type reference forces exit. }
  procedure NoteType(aType: TPasType; aDepth: integer);
  var
    lPt: TPasProcedureType;
    lParams: TFPList;
    k: integer;
  begin
    if (aType = nil) or (aDepth > 40) then
      Exit;
    if aType is TPasUnresolvedTypeRef then
    begin
      lUnresolved := True;
      Exit;
    end;
    NoteDecl(aType);
    // A generic instantiation Base<Arg,...> (TPasSpecializeType is-a alias): the
    // base destination AND every type argument are interface-visible.
    if aType is TPasSpecializeType then
    begin
      NoteType(TPasSpecializeType(aType).DestType, aDepth + 1);
      lParams := TPasSpecializeType(aType).Params;
      if lParams <> nil then
        for k := 0 to lParams.Count - 1 do
          if TObject(lParams[k]) is TPasType then
            NoteType(TPasType(lParams[k]), aDepth + 1);
    end
    else if aType is TPasArrayType then
      NoteType(TPasArrayType(aType).ElType, aDepth + 1)
    else if aType is TPasAliasType then  // also covers class-of (TPasClassOfType)
      NoteType(TPasAliasType(aType).DestType, aDepth + 1)
    else if aType is TPasPointerType then
      NoteType(TPasPointerType(aType).DestType, aDepth + 1)
    else if aType is TPasSetType then
      NoteType(TPasSetType(aType).EnumType, aDepth + 1)
    else if aType is TPasProcedureType then
    begin
      // A standalone procedural type ('procedure(...)' / 'function(...): T'):
      // its argument and result types are interface-visible references.
      lPt := TPasProcedureType(aType);
      if lPt.Args <> nil then
        for k := 0 to lPt.Args.Count - 1 do
          NoteType(TPasArgument(lPt.Args[k]).ArgType, aDepth + 1);
      if (lPt is TPasFunctionType) and (TPasFunctionType(lPt).ResultEl <> nil) then
        NoteType(TPasFunctionType(lPt).ResultEl.ResultType, aDepth + 1);
    end;
  end;

  // Records every resolved reference in aExpr's tree (inline-body references).
  procedure NoteExpr(aExpr: TPasExpr);
  var
    lRef: TResolvedReference;
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TPrimitiveExpr then
    begin
      lRef := RefOf(aExpr);
      if lRef <> nil then
        NoteDecl(lRef.Declaration);
    end
    else if aExpr is TParamsExpr then
    begin
      NoteExpr(TParamsExpr(aExpr).Value);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        NoteExpr(TParamsExpr(aExpr).Params[k]);
    end
    else if aExpr is TBinaryExpr then
    begin
      NoteExpr(TBinaryExpr(aExpr).left);
      NoteExpr(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      NoteExpr(TUnaryExpr(aExpr).Operand);
  end;

  // Records the references in an inline routine's body statements.
  procedure NoteInlineBody(aProc: TPasProcedure);

    procedure ScanStmt(aStmt: TPasImplElement);
    var
      lEls: TFPList;
      k: integer;
    begin
      if aStmt = nil then
        Exit;
      if aStmt is TPasImplAssign then
      begin
        NoteExpr(TPasImplAssign(aStmt).Left);
        NoteExpr(TPasImplAssign(aStmt).Right);
      end
      else if aStmt is TPasImplSimple then
        NoteExpr(TPasImplSimple(aStmt).Expr)
      else if aStmt is TPasImplIfElse then
        NoteExpr(TPasImplIfElse(aStmt).ConditionExpr)
      else if aStmt is TPasImplWhileDo then
        NoteExpr(TPasImplWhileDo(aStmt).ConditionExpr)
      else if aStmt is TPasImplRepeatUntil then
        NoteExpr(TPasImplRepeatUntil(aStmt).ConditionExpr)
      else if aStmt is TPasImplCaseOf then
        NoteExpr(TPasImplCaseOf(aStmt).CaseExpr);
      if aStmt is TPasImplBlock then
      begin
        lEls := TPasImplBlock(aStmt).Elements;
        if lEls <> nil then
          for k := 0 to lEls.Count - 1 do
            if TObject(lEls[k]) is TPasImplElement then
              ScanStmt(TPasImplElement(lEls[k]));
      end;
    end;

  begin
    if (aProc.Body <> nil) and (aProc.Body.Body <> nil) then
      ScanStmt(aProc.Body.Body);
  end;

  { Records the type constraints of a generic type's template parameter.
    Constraints hold TPasType or TPasExpr (a reference leaf) entries. }
  procedure NoteTemplates(aGen: TPasGenericType);
  var
    i, j: integer;
    lTpl: TPasGenericTemplateType;
    lC: TPasElement;
  begin
    if aGen.GenericTemplateTypes = nil then
      Exit;
    for i := 0 to aGen.GenericTemplateTypes.Count - 1 do
    begin
      if not (TObject(aGen.GenericTemplateTypes[i]) is TPasGenericTemplateType) then
        Continue;
      lTpl := TPasGenericTemplateType(aGen.GenericTemplateTypes[i]);
      for j := 0 to High(lTpl.Constraints) do
      begin
        lC := lTpl.Constraints[j];
        if lC is TPasType then
          NoteType(TPasType(lC), 0)
        else if lC is TPasExpr then
          NoteExpr(TPasExpr(lC));
      end;
    end;
  end;

  { Collect every interface-visible type/identifier reference of aEl into the referenced-module set. }
  procedure VisitDecl(aEl: TPasElement; aDepth: integer);
  var
    lProc: TPasProcedure;
    lFn: TPasFunctionType;
    lCls: TPasClassType;
    lRec: TPasRecordType;
    lMem, lArgs: TFPList;
    k: integer;
  begin
    if (aEl = nil) or (aDepth > 40) then
      Exit;
    // Any generic type (class/record/proc-type) may carry template constraints.
    if aEl is TPasGenericType then
      NoteTemplates(TPasGenericType(aEl));
    if aEl is TPasProcedure then
    begin
      lProc := TPasProcedure(aEl);
      if (lProc.ProcType <> nil) and (lProc.ProcType.Args <> nil) then
        for k := 0 to lProc.ProcType.Args.Count - 1 do
          NoteType(TPasArgument(lProc.ProcType.Args[k]).ArgType, aDepth + 1);
      if lProc.ProcType is TPasFunctionType then
      begin
        lFn := TPasFunctionType(lProc.ProcType);
        if lFn.ResultEl <> nil then
          NoteType(lFn.ResultEl.ResultType, aDepth + 1);
      end;
      if cTreatInlineBodiesAsInterface and (pmInline in lProc.Modifiers) then
        NoteInlineBody(lProc);
    end
    else if aEl is TPasProperty then  // the property type AND any index-arg types
    begin
      NoteType(TPasVariable(aEl).VarType, aDepth + 1);
      lArgs := TPasProperty(aEl).Args;
      if lArgs <> nil then
        for k := 0 to lArgs.Count - 1 do
          NoteType(TPasArgument(lArgs[k]).ArgType, aDepth + 1);
    end
    else if aEl is TPasVariable then  // var / const / field
      NoteType(TPasVariable(aEl).VarType, aDepth + 1)
    else if aEl is TPasClassType then
    begin
      lCls := TPasClassType(aEl);
      NoteType(lCls.AncestorType, aDepth + 1);
      if lCls.Interfaces <> nil then
        for k := 0 to lCls.Interfaces.Count - 1 do
          NoteType(TPasType(lCls.Interfaces[k]), aDepth + 1);
      lMem := lCls.Members;
      if lMem <> nil then
        for k := 0 to lMem.Count - 1 do
          VisitDecl(TPasElement(lMem[k]), aDepth + 1);
    end
    else if aEl is TPasRecordType then
    begin
      lRec := TPasRecordType(aEl);
      if lRec.Members <> nil then
        for k := 0 to lRec.Members.Count - 1 do
          VisitDecl(TPasElement(lRec.Members[k]), aDepth + 1);
    end
    else if aEl is TPasType then
      { Any other standalone type declaration: delegate to NoteType }
      NoteType(TPasType(aEl), aDepth + 1);
  end;

var
  i, k: integer;
  lDecls: TFPList;
  lUse: TPasUsesUnit;
  lUseMod: TPasModule;
  lReferenced: boolean;
begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if (lMod = nil) or (lMod.InterfaceSection = nil) then
    Exit;

  lRefMods := TFPList.Create;
  try
    lUnresolved := False;
    lDecls := lMod.InterfaceSection.Declarations;
    if lDecls <> nil then
      for i := 0 to lDecls.Count - 1 do
        VisitDecl(TPasElement(lDecls[i]), 0);

    // If even one interface reference is unresolved, the unused-ness of no unit can be proven.
    if not lUnresolved then
      for i := 0 to High(lMod.InterfaceSection.UsesClause) do
      begin
        lUse := lMod.InterfaceSection.UsesClause[i];
        // The implicit System unit has no source name expression and is never movable.
        if lUse.Expr = nil then
          Continue;
        // A used unit that did not bind to a real module cannot compute its movability => skip it.
        if not (lUse.Module is TPasModule) then
          Continue;
        if IsIgnored(lUse.Name) then
          Continue;
        lUseMod := TPasModule(lUse.Module);
        lReferenced := False;
        for k := 0 to lRefMods.Count - 1 do
          if TPasModule(lRefMods[k]) = lUseMod then
          begin
            lReferenced := True;
            Break;
          end;
        if not lReferenced then
        begin
          SetLength(aNodes, Length(aNodes) + 1);
          aNodes[High(aNodes)] := lUse;
        end;
      end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
  lRefMods.Free;
end;


function TFpSonarResolver.TryObjectAsComInterfaceSites(
  out aNodes: TPasElementArray): boolean;
const
  cCorbaInterfacesExempt = True;
  cAllowExplicitAs = True;
var
  lMod: TPasModule;

  procedure Add(aEl: TPasElement);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
  end;

  // The alias-collapsed class type a computed result denotes, or nil.
  function ClassTypeOf(const aRes: TPasResolverResult): TPasClassType;
  var
    lEl: TPasType;
  begin
    Result := nil;
    if (aRes.BaseType <> btContext) or (aRes.LoTypeEl = nil)
      or not (aRes.LoTypeEl is TPasType) then
      Exit;
    lEl := FTopEngine.ResolveAliasType(TPasType(aRes.LoTypeEl));
    if lEl is TPasClassType then
      Result := TPasClassType(lEl);
  end;

  { True iff aRes is a concrete class instance reference: a non-interface class
    whose identity is a var/field/argument/result. }
  function IsClassInstance(const aRes: TPasResolverResult): boolean;
  var
    lCls: TPasClassType;
  begin
    Result := False;
    lCls := ClassTypeOf(aRes);
    if (lCls = nil) or (lCls.ObjKind <> okClass) then
      Exit;
    Result := (aRes.IdentEl is TPasVariable) or (aRes.IdentEl is TPasArgument)
      or (aRes.IdentEl is TPasResultElement);
  end;

  // True iff aRes denotes an interface target that fires: a COM interface always,
  // a CORBA interface only when the switch is off.
  function TargetInterfaceFires(const aRes: TPasResolverResult): boolean;
  var
    lCls: TPasClassType;
  begin
    Result := False;
    lCls := ClassTypeOf(aRes);
    if (lCls = nil) or not (lCls.ObjKind in [okInterface, okDispInterface]) then
      Exit;
    if lCls.InterfaceType = citCom then
      Result := True
    else if (lCls.InterfaceType = citCorba) and (not cCorbaInterfacesExempt) then
      Result := True;
  end;

  // True iff aExpr is an explicit 'as'/cast (intentional coercion).
  function IsExplicitAs(aExpr: TPasExpr): boolean;
  begin
    Result := cAllowExplicitAs and (aExpr is TBinaryExpr)
      and (TBinaryExpr(aExpr).OpCode = eopAs);
  end;

  // Flags aSrc->aDst when a concrete class instance has a COM interface target.
  // aSrc is the anchor node appended.
  procedure CheckPair(aSrc, aDst: TPasExpr; aDstType: TPasType);
  var
    lSrcRes, lDstRes: TPasResolverResult;
  begin
    if (aSrc = nil) or IsExplicitAs(aSrc) then
      Exit;
    FTopEngine.ComputeElement(aSrc, lSrcRes, []);
    if not IsClassInstance(lSrcRes) then
      Exit;
    if aDstType <> nil then
      FTopEngine.ComputeElement(aDstType, lDstRes, [rcType])
    else if aDst <> nil then
      FTopEngine.ComputeElement(aDst, lDstRes, [])
    else
      Exit;
    if TargetInterfaceFires(lDstRes) then
      Add(aSrc);
  end;

  // Processes a call: each actual argument against its declared formal type.
  procedure ProcessCall(aParams: TParamsExpr);
  var
    lRef: TResolvedReference;
    lProc: TPasProcedure;
    lArgs: TFPList;
    k: integer;
  begin
    lRef := FTopEngine.GetParamsValueRef(aParams);
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
      Exit;
    lProc := TPasProcedure(lRef.Declaration);
    if (lProc.ProcType = nil) or (lProc.ProcType.Args = nil) then
      Exit;
    lArgs := lProc.ProcType.Args;
    for k := 0 to High(aParams.Params) do
    begin
      if k > lArgs.Count - 1 then
        Break;
      CheckPair(aParams.Params[k], nil, TPasArgument(lArgs[k]).ArgType);
    end;
  end;

  // Descends aExpr finding call nodes
  procedure ScanExpr(aExpr: TPasExpr);
  var
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TParamsExpr then
    begin
      if TParamsExpr(aExpr).Kind = pekFuncParams then
        ProcessCall(TParamsExpr(aExpr));
      ScanExpr(TParamsExpr(aExpr).Value);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        ScanExpr(TParamsExpr(aExpr).Params[k]);
    end
    else if aExpr is TBinaryExpr then
    begin
      ScanExpr(TBinaryExpr(aExpr).left);
      ScanExpr(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      ScanExpr(TUnaryExpr(aExpr).Operand);
  end;

  // The direct child sub-statements of aStmt
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  { Processes a statement's own expressions, then recurses into sub-statements. }
  procedure WalkStmt(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      CheckPair(TPasImplAssign(aStmt).Right, TPasImplAssign(aStmt).Left, nil);
      ScanExpr(TPasImplAssign(aStmt).Left);
      ScanExpr(TPasImplAssign(aStmt).Right);
    end
    else if aStmt is TPasImplSimple then
      ScanExpr(TPasImplSimple(aStmt).Expr)
    else if aStmt is TPasImplIfElse then
      ScanExpr(TPasImplIfElse(aStmt).ConditionExpr)
    else if aStmt is TPasImplWhileDo then
      ScanExpr(TPasImplWhileDo(aStmt).ConditionExpr)
    else if aStmt is TPasImplRepeatUntil then
      ScanExpr(TPasImplRepeatUntil(aStmt).ConditionExpr)
    else if aStmt is TPasImplForLoop then
    begin
      ScanExpr(TPasImplForLoop(aStmt).StartExpr);
      ScanExpr(TPasImplForLoop(aStmt).EndExpr);
    end
    else if aStmt is TPasImplCaseOf then
      ScanExpr(TPasImplCaseOf(aStmt).CaseExpr)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            ScanExpr(TPasExpr(lWith.Expressions[k]));
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      WalkStmt(lKids[k]);
  end;

  // Walks aDecls for body-bearing routines, processing their statements and
  // recursing into nested routines.
  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if (lEl is TPasProcedure) and (TPasProcedure(lEl).Body <> nil) then
      begin
        lProc := TPasProcedure(lEl);
        if lProc.Body.Body <> nil then
          WalkStmt(lProc.Body.Body);
        WalkDecls(lProc.Body.Declarations);
      end;
    end;
  end;

begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryNestedRoutineAsProcValueSites(
  out aNodes: TPasElementArray): boolean;
var
  lMod: TPasModule;

  procedure Add(aEl: TPasElement);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // True iff aProc is a nested routine — declared inside another routine's body,
  // i.e. its Parent chain passes through a TProcedureBody before the module.
  function IsNestedProc(aProc: TPasProcedure): boolean;
  var
    lEl: TPasElement;
    lGuard: integer;
  begin
    Result := False;
    lEl := aProc.Parent;
    lGuard := 0;
    while (lEl <> nil) and (lGuard < 200) do
    begin
      if lEl is TProcedureBody then
        Exit(True);
      if lEl is TPasModule then
        Exit(False);
      lEl := lEl.Parent;
      Inc(lGuard);
    end;
  end;

  { The nested routine whose address aExpr (`@Inner`) takes, or nil when aExpr is
    not an address-of a resolved nested routine.}
  function NestedAddressTarget(aExpr: TPasExpr): TPasProcedure;
  var
    lUn: TUnaryExpr;
    lRef: TResolvedReference;
  begin
    Result := nil;
    if not (aExpr is TUnaryExpr) then
      Exit;
    lUn := TUnaryExpr(aExpr);
    if lUn.OpCode <> eopAddress then
      Exit;
    lRef := RefOf(lUn.Operand);
    if lRef = nil then
      lRef := RefOf(lUn);
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure) then
      Exit;
    if IsNestedProc(TPasProcedure(lRef.Declaration)) then
      Result := TPasProcedure(lRef.Declaration);
  end;

  { True iff the assignment target aLeft is storage that outlived the enclosing
    routine frame — a global variable or a record/class field. }
  function OutlivesFrame(aLeft: TPasExpr): boolean;
  var
    lRes: TPasResolverResult;
    lEl: TPasElement;
    lGuard: integer;
  begin
    Result := False;
    FTopEngine.ComputeElement(aLeft, lRes, []);
    if not (lRes.IdentEl is TPasVariable) then
      Exit;
    lEl := lRes.IdentEl.Parent;
    lGuard := 0;
    while (lEl <> nil) and (lGuard < 200) do
    begin
      if (lEl is TProcedureBody) or (lEl is TPasProcedure) then
        Exit(False);                 // a local of a routine frame
      if lEl is TPasMembersType then
        Exit(True);                  // a class/record field
      if lEl is TPasSection then
        Exit(True);                  // a global (interface/implementation) var
      lEl := lEl.Parent;
      Inc(lGuard);
    end;
  end;

  // The direct child sub-statements of aStmt.
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // For each assignment whose RHS takes a nested routine's address and whose LHS
  // outlives the frame, append the '@'-expression; recurse into sub-statements.
  procedure WalkStmt(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    lAssign: TPasImplAssign;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      lAssign := TPasImplAssign(aStmt);
      if (NestedAddressTarget(lAssign.Right) <> nil)
        and OutlivesFrame(lAssign.Left) then
        Add(lAssign.Right);
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      WalkStmt(lKids[k]);
  end;

  // Walks aDecls for body-bearing routines, processing statements and recursing
  // into nested routines.
  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if (lEl is TPasProcedure) and (TPasProcedure(lEl).Body <> nil) then
      begin
        lProc := TPasProcedure(lEl);
        if lProc.Body.Body <> nil then
          WalkStmt(lProc.Body.Body);
        WalkDecls(lProc.Body.Declarations);
      end;
    end;
  end;

begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryInlineVarCapturedByAnonMethodSites(
  out aNodes: TPasElementArray): boolean;
const
  { The read-paths are deferred: NoInlineVarCapturedByAnonMethod's two params ship
    as wrapper consts.
     * cFlagInlineLocals=True  => flag captures of block-scoped inline `var` locals.
     * cFlagLoopVariables=True => flag captures of `for var` per-iteration loop vars. }
  cFlagInlineLocals = True;
  cFlagLoopVariables = True;
var
  lMod: TPasModule;
  lLoopVars: TFpSonarStringArray;   // names of enclosing IsVarDef for-loops in scope

  procedure Add(aEl: TPasElement);
  var
    k: integer;
  begin
    for k := 0 to High(aNodes) do
      if aNodes[k] = aEl then
        Exit;                          // de-dup the same anchor
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // True iff aDecl is the analysed anon proc's own param/local: its Parent
  // ancestry reaches aAnon before the module.
  function IsOwnLocal(aDecl, aAnon: TPasElement): boolean;
  var
    lEl: TPasElement;
    lGuard: integer;
  begin
    Result := False;
    lEl := aDecl;
    lGuard := 0;
    while (lEl <> nil) and (lGuard < 200) do
    begin
      if lEl = aAnon then
        Exit(True);
      if lEl is TPasModule then
        Exit(False);
      lEl := lEl.Parent;
      Inc(lGuard);
    end;
  end;

  { True iff aDecl is a block-scoped inline local — a variable declared by an inline
    `var` statement inside a begin..end block, not a routine var-section local.
    This Parent distinction is the block-scoped-vs-routine-local discriminator. }
  function IsBlockScopedInline(aDecl: TPasElement): boolean;
  begin
    Result := (aDecl is TPasVariable)
      and ((aDecl.Parent is TPasInlineVarDeclStatement)
      or (aDecl.Parent is TPasImplBlock));
  end;

  // True iff aName matches the control variable of an enclosing IsVarDef for-loop.
  function NameMatchesLoopVar(const aName: string): boolean;
  var
    k: integer;
  begin
    Result := False;
    if aName = '' then
      Exit;
    for k := 0 to High(lLoopVars) do
      if SameText(lLoopVars[k], aName) then
        Exit(True);
  end;

  procedure HandleAnon(aPE: TProcedureExpr); forward;

  { Walks aExpr's tree. A nested TProcedureExpr is dispatched to HandleAnon. }
  procedure ScanExpr(aExpr: TPasExpr; aAnon: TPasElement);
  var
    k: integer;
    lRef: TResolvedReference;
    lDecl: TPasElement;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TProcedureExpr then
      HandleAnon(TProcedureExpr(aExpr))
    else if aExpr is TPrimitiveExpr then
    begin
      if aAnon = nil then
        Exit;
      lRef := RefOf(aExpr);
      if lRef = nil then
        Exit;                        // unresolved capture => silence
      lDecl := lRef.Declaration;
      if lDecl = nil then
        Exit;
      if IsOwnLocal(lDecl, aAnon) then
        Exit;                        // the anon proc's own param/local
      if cFlagInlineLocals and IsBlockScopedInline(lDecl) then
        Add(aExpr)
      else if cFlagLoopVariables
        and NameMatchesLoopVar(TPrimitiveExpr(aExpr).Value) then
        Add(aExpr);
      // else routine-level local / unclassifiable => skip
    end
    else if aExpr is TParamsExpr then
    begin
      ScanExpr(TParamsExpr(aExpr).Value, aAnon);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        ScanExpr(TParamsExpr(aExpr).Params[k], aAnon);
    end
    else if aExpr is TBinaryExpr then
    begin
      ScanExpr(TBinaryExpr(aExpr).left, aAnon);
      ScanExpr(TBinaryExpr(aExpr).right, aAnon);
    end
    else if aExpr is TUnaryExpr then
      ScanExpr(TUnaryExpr(aExpr).Operand, aAnon);
  end;

  // The direct child sub-statements of aStmt (the single tree-shape encoding,
  // reproduced query-locally).
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  { Walks aStmt's control/value expressions, then recurses every sub-statement. }
  procedure WalkStmt(aStmt: TPasImplElement; aAnon: TPasElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    lFor: TPasImplForLoop;
    lPushed: boolean;
    lName: string;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      ScanExpr(TPasImplAssign(aStmt).Left, aAnon);
      ScanExpr(TPasImplAssign(aStmt).Right, aAnon);
    end
    else if aStmt is TPasImplSimple then
      ScanExpr(TPasImplSimple(aStmt).Expr, aAnon)
    else if aStmt is TPasImplIfElse then
      ScanExpr(TPasImplIfElse(aStmt).ConditionExpr, aAnon)
    else if aStmt is TPasImplWhileDo then
      ScanExpr(TPasImplWhileDo(aStmt).ConditionExpr, aAnon)
    else if aStmt is TPasImplRepeatUntil then
      ScanExpr(TPasImplRepeatUntil(aStmt).ConditionExpr, aAnon)
    else if aStmt is TPasImplCaseOf then
      ScanExpr(TPasImplCaseOf(aStmt).CaseExpr, aAnon)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            ScanExpr(TPasExpr(lWith.Expressions[k]), aAnon);
    end;

    lPushed := False;
    if aStmt is TPasImplForLoop then
    begin
      lFor := TPasImplForLoop(aStmt);
      ScanExpr(lFor.StartExpr, aAnon);
      ScanExpr(lFor.EndExpr, aAnon);
      if (aAnon = nil) and cFlagLoopVariables and lFor.IsVarDef
        and (lFor.VariableName is TPrimitiveExpr) then
      begin
        lName := TPrimitiveExpr(lFor.VariableName).Value;
        if lName <> '' then
        begin
          SetLength(lLoopVars, Length(lLoopVars) + 1);
          lLoopVars[High(lLoopVars)] := lName;
          lPushed := True;
        end;
      end;
    end;

    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      WalkStmt(lKids[k], aAnon);

    if lPushed then
      SetLength(lLoopVars, Length(lLoopVars) - 1);
  end;

  { Classifies the captures of one anonymous method: walk its body
    with the anon proc as the own-local sentinel. }
  procedure HandleAnon(aPE: TProcedureExpr);
  begin
    if (aPE = nil) or (aPE.Proc = nil) then
      Exit;
    if not (aPE.Proc.GetProcTypeEnum in
      [ptAnonymousProcedure, ptAnonymousFunction]) then
      Exit;
    if (aPE.Proc.Body <> nil) and (aPE.Proc.Body.Body <> nil) then
      WalkStmt(aPE.Proc.Body.Body, aPE.Proc);
  end;

  // Walks aDecls routines, scanning their statements for anon
  // procs and recursing into nested routines.
  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if (lEl is TPasProcedure) and (TPasProcedure(lEl).Body <> nil) then
      begin
        lProc := TPasProcedure(lEl);
        if lProc.Body.Body <> nil then
          WalkStmt(lProc.Body.Body, nil);
        WalkDecls(lProc.Body.Declarations);
      end;
    end;
  end;

begin
  aNodes := nil;
  lLoopVars := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryUnusedGenericConstraints(
  out aNodes: TPasElementArray): boolean;
var
  lMod: TPasModule;
  lTyped: TFpSonarStringArray;
  lMemberTyped: TFpSonarStringArray;

  procedure Add(aEl: TPasElement);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
  end;

  procedure AddTyped(const aName: string);
  begin
    if aName = '' then
      Exit;
    SetLength(lTyped, Length(lTyped) + 1);
    lTyped[High(lTyped)] := aName;
  end;

  // True iff aName is a declaration collected as carrying the template's type.
  function IsTyped(const aName: string): boolean;
  var
    k: integer;
  begin
    Result := False;
    if aName = '' then
      Exit;
    for k := 0 to High(lTyped) do
      if SameText(lTyped[k], aName) then
        Exit(True);
  end;

  // The written name an identifier or qualified-access expression ends in.
  function IdentName(aExpr: TPasExpr): string;
  begin
    Result := '';
    if aExpr = nil then
      Exit;
    if (aExpr is TPrimitiveExpr) and (aExpr.Kind = pekIdent) then
      Result := TPrimitiveExpr(aExpr).Value
    else if (aExpr is TBinaryExpr)
      and (TBinaryExpr(aExpr).OpCode = eopSubIdent) then
      Result := IdentName(TBinaryExpr(aExpr).right)
    else if aExpr is TParamsExpr then
      Result := IdentName(TParamsExpr(aExpr).Value);
  end;

  // True iff aExpr is the nil literal.
  function IsNilLiteral(aExpr: TPasExpr): boolean;
  begin
    Result := (aExpr <> nil) and (aExpr.Kind = pekNil);
  end;

  // True iff aExpr names aParam itself or a declaration typed by it.
  function NamesParam(aExpr: TPasExpr; const aParam: string): boolean;
  var
    lName: string;
  begin
    lName := IdentName(aExpr);
    Result := SameText(lName, aParam) or IsTyped(lName);
  end;

  // The declared type of a variable, argument or function result, or nil.
  function DeclaredType(aEl: TPasElement): TPasType;
  begin
    if aEl is TPasVariable then
      Result := TPasVariable(aEl).VarType
    else if aEl is TPasArgument then
      Result := TPasArgument(aEl).ArgType
    else if aEl is TPasResultElement then
      Result := TPasResultElement(aEl).ResultType
    else
      Result := nil;
  end;

  // Collects aEl's name when its declared type is written aTypeName.
  procedure NoteTyped(aEl: TPasElement; const aTypeName: string);
  var
    lType: TPasType;
  begin
    lType := DeclaredType(aEl);
    if (lType <> nil) and SameText(lType.Name, aTypeName) then
      AddTyped(aEl.Name);
  end;

  // Collects the arguments, result and locals of aProc typed aTypeName.
  procedure NoteRoutine(aProc: TPasProcedure; const aTypeName: string);
  var
    k: integer;
  begin
    if aProc.ProcType <> nil then
    begin
      if aProc.ProcType.Args <> nil then
        for k := 0 to aProc.ProcType.Args.Count - 1 do
          NoteTyped(TPasElement(aProc.ProcType.Args[k]), aTypeName);
      if aProc.ProcType is TPasFunctionType then
        NoteTyped(TPasFunctionType(aProc.ProcType).ResultEl, aTypeName);
    end;
    if (aProc.Body = nil) or (aProc.Body.Declarations = nil) then
      Exit;
    for k := 0 to aProc.Body.Declarations.Count - 1 do
      NoteTyped(TPasElement(aProc.Body.Declarations[k]), aTypeName);
  end;

  { True iff aExpr selects a member on aParam or on a declaration typed by it,
    tests aParam with `is`/`as`, or compares either against nil. }
  function ExprRelies(aExpr: TPasExpr; const aParam: string): boolean;
  var
    lBin: TBinaryExpr;
    k: integer;
  begin
    Result := False;
    if aExpr = nil then
      Exit;
    if aExpr is TBinaryExpr then
    begin
      lBin := TBinaryExpr(aExpr);
      if lBin.OpCode = eopSubIdent then
      begin
        if NamesParam(lBin.left, aParam) then
          Exit(True);
      end
      else if lBin.OpCode in [eopIs, eopAs] then
      begin
        if SameText(IdentName(lBin.left), aParam)
          or SameText(IdentName(lBin.right), aParam) then
          Exit(True);
      end
      else if lBin.OpCode in [eopEqual, eopNotEqual] then
        if (IsNilLiteral(lBin.left) and NamesParam(lBin.right, aParam))
          or (IsNilLiteral(lBin.right) and NamesParam(lBin.left, aParam)) then
          Exit(True);
      Result := ExprRelies(lBin.left, aParam) or ExprRelies(lBin.right, aParam);
    end
    else if aExpr is TParamsExpr then
    begin
      if ExprRelies(TParamsExpr(aExpr).Value, aParam) then
        Exit(True);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        if ExprRelies(TParamsExpr(aExpr).Params[k], aParam) then
          Exit(True);
    end
    else if aExpr is TUnaryExpr then
      Result := ExprRelies(TUnaryExpr(aExpr).Operand, aParam);
  end;

  // The direct child sub-statements of aStmt
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // True iff aStmt or any sub-statement carries reliance evidence on aParam.
  function StmtRelies(aStmt: TPasImplElement; const aParam: string): boolean;
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    lAssign: TPasImplAssign;
    k: integer;
  begin
    Result := False;
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      lAssign := TPasImplAssign(aStmt);
      Result := (IsNilLiteral(lAssign.Right) and NamesParam(lAssign.Left, aParam))
        or ExprRelies(lAssign.Left, aParam)
        or ExprRelies(lAssign.Right, aParam);
    end
    else if aStmt is TPasImplSimple then
      Result := ExprRelies(TPasImplSimple(aStmt).Expr, aParam)
    else if aStmt is TPasImplIfElse then
      Result := ExprRelies(TPasImplIfElse(aStmt).ConditionExpr, aParam)
    else if aStmt is TPasImplWhileDo then
      Result := ExprRelies(TPasImplWhileDo(aStmt).ConditionExpr, aParam)
    else if aStmt is TPasImplRepeatUntil then
      Result := ExprRelies(TPasImplRepeatUntil(aStmt).ConditionExpr, aParam)
    else if aStmt is TPasImplForLoop then
      Result := ExprRelies(TPasImplForLoop(aStmt).StartExpr, aParam)
        or ExprRelies(TPasImplForLoop(aStmt).EndExpr, aParam)
    else if aStmt is TPasImplCaseOf then
      Result := ExprRelies(TPasImplCaseOf(aStmt).CaseExpr, aParam)
    else if aStmt is TPasImplRaise then
      Result := ExprRelies(TPasImplRaise(aStmt).ExceptObject, aParam)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            // A with scope elides the selector, so naming the operand is reliance.
            if NamesParam(TPasExpr(lWith.Expressions[k]), aParam)
              or ExprRelies(TPasExpr(lWith.Expressions[k]), aParam) then
              Exit(True);
    end;
    if Result then
      Exit;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      if StmtRelies(lKids[k], aParam) then
        Exit(True);
  end;

  { True iff aProc implements a member of the generic named aOwner: the parser
    writes the qualified `<Generic>.<Member>` name without the template list. }
  function ImplementsMemberOf(aProc: TPasProcedure; const aOwner: string): boolean;
  begin
    Result := (aOwner <> '')
      and SameText(Copy(aProc.Name, 1, Length(aOwner) + 1), aOwner + '.');
  end;

  // True iff aTemplate carries at least one type constraint.
  function HasTypeConstraint(aTemplate: TPasGenericTemplateType): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(aTemplate.Constraints) do
      // The nil call yields the token standing for "not a keyword constraint".
      if FTopEngine.GetGenericConstraintKeyword(aTemplate.Constraints[k])
        = FTopEngine.GetGenericConstraintKeyword(nil) then
        Exit(True);
  end;

  { True iff aGeneric offers a surface evidence can appear on: a members type
    that is not a forward declaration and has at least one implemented routine. }
  function HasBodySurface(aGeneric: TPasGenericType): boolean;
  var
    lProc: TPasProcedure;
    k: integer;
  begin
    Result := False;
    if not (aGeneric is TPasMembersType) then
      Exit;
    if (aGeneric is TPasClassType) and TPasClassType(aGeneric).IsForward then
      Exit;
    if (lMod.ImplementationSection = nil) or (aGeneric.Name = '') then
      Exit;
    for k := 0 to lMod.ImplementationSection.Declarations.Count - 1 do
    begin
      if not (TObject(lMod.ImplementationSection.Declarations[k]) is TPasProcedure) then
        Continue;
      lProc := TPasProcedure(lMod.ImplementationSection.Declarations[k]);
      if ImplementsMemberOf(lProc, aGeneric.Name) and (lProc.Body <> nil) then
        Exit(True);
    end;
  end;

  { True iff aProc, or a routine nested in it, relies on aParam. The names typed
    by aParam are aOuter plus the routine's own, so a sibling routine's
    declarations never count here. }
  function RoutineRelies(aProc: TPasProcedure; const aParam: string;
    const aOuter: TFpSonarStringArray): boolean;
  var
    lScope: TFpSonarStringArray;
    k: integer;
  begin
    lTyped := Copy(aOuter);
    NoteRoutine(aProc, aParam);
    lScope := lTyped;
    Result := (aProc.Body <> nil) and StmtRelies(aProc.Body.Body, aParam);
    if Result or (aProc.Body = nil) or (aProc.Body.Declarations = nil) then
      Exit;
    for k := 0 to aProc.Body.Declarations.Count - 1 do
      if (TObject(aProc.Body.Declarations[k]) is TPasProcedure)
        and RoutineRelies(TPasProcedure(aProc.Body.Declarations[k]), aParam,
          lScope) then
        Exit(True);
  end;

  // True iff an implemented routine of aGeneric relies on the template aParam.
  function ReliesOn(aGeneric: TPasGenericType; const aParam: string): boolean;
  var
    lMembers: TFPList;
    lProc: TPasProcedure;
    k: integer;
  begin
    Result := False;
    lTyped := nil;
    lMembers := TPasMembersType(aGeneric).Members;
    for k := 0 to lMembers.Count - 1 do
      if not (TObject(lMembers[k]) is TPasProcedure) then
        NoteTyped(TPasElement(lMembers[k]), aParam);
    lMemberTyped := lTyped;
    for k := 0 to lMod.ImplementationSection.Declarations.Count - 1 do
    begin
      if not (TObject(lMod.ImplementationSection.Declarations[k]) is TPasProcedure) then
        Continue;
      lProc := TPasProcedure(lMod.ImplementationSection.Declarations[k]);
      if ImplementsMemberOf(lProc, aGeneric.Name)
        and RoutineRelies(lProc, aParam, lMemberTyped) then
        Exit(True);
    end;
  end;

  procedure VisitGeneric(aGeneric: TPasGenericType);
  var
    lTemplate: TPasGenericTemplateType;
    k: integer;
  begin
    if (aGeneric.GenericTemplateTypes = nil) or not HasBodySurface(aGeneric) then
      Exit;
    for k := 0 to aGeneric.GenericTemplateTypes.Count - 1 do
    begin
      lTemplate := TPasGenericTemplateType(aGeneric.GenericTemplateTypes[k]);
      if lTemplate.IsConst or (lTemplate.Name = '')
        or not HasTypeConstraint(lTemplate) then
        Continue;
      if not ReliesOn(aGeneric, lTemplate.Name) then
        Add(lTemplate);
    end;
  end;

  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
      if TObject(aDecls[k]) is TPasGenericType then
        VisitGeneric(TPasGenericType(aDecls[k]));
  end;

begin
  aNodes := nil;
  lTyped := nil;
  lMemberTyped := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


{ The declaration-site specializations of aMod, in declaration order: every
  TPasSpecializeType a declaration owns, a specialization's own parameters
  excluded. Both specialization queries collect over this. }
function SpecializationSites(aMod: TPasModule): TPasElementArray;
var
  lSites: TPasElementArray;
  lSeen: TFPList;

  // Descends into aEl's own children, collecting the specializations it owns.
  procedure Visit(aEl: TPasElement);
  var
    lChild: TPasElement;
    lProc: TPasProcedure;
    k: integer;

    // aChild, when aEl declares it inline rather than naming it elsewhere.
    function Owned(aChild: TPasElement): TPasElement;
    begin
      if (aChild <> nil) and (aChild.Parent = aEl) then
        Result := aChild
      else
        Result := nil;
    end;

  begin
    if (aEl = nil) or (lSeen.IndexOf(aEl) >= 0) then
      Exit;
    lSeen.Add(aEl);
    if aEl is TPasSpecializeType then
    begin
      SetLength(lSites, Length(lSites) + 1);
      lSites[High(lSites)] := aEl;
      Exit;
    end;
    if aEl is TPasProcedure then
    begin
      lProc := TPasProcedure(aEl);
      if lProc.ProcType <> nil then
      begin
        if lProc.ProcType.Args <> nil then
          for k := 0 to lProc.ProcType.Args.Count - 1 do
            Visit(TPasElement(lProc.ProcType.Args[k]));
        if lProc.ProcType is TPasFunctionType then
          Visit(TPasFunctionType(lProc.ProcType).ResultEl);
      end;
      if lProc.Body <> nil then
        Visit(lProc.Body);
    end
    else if aEl is TProcedureBody then
    begin
      for k := 0 to TProcedureBody(aEl).Declarations.Count - 1 do
        Visit(TPasElement(TProcedureBody(aEl).Declarations[k]));
    end
    else if aEl is TPasArgument then
      Visit(Owned(TPasArgument(aEl).ArgType))
    else if aEl is TPasResultElement then
      Visit(Owned(TPasResultElement(aEl).ResultType))
    else if aEl is TPasVariable then
      Visit(Owned(TPasVariable(aEl).VarType))
    else if aEl is TPasMembersType then
    begin
      if aEl is TPasClassType then
        Visit(Owned(TPasClassType(aEl).AncestorType));
      for k := 0 to TPasMembersType(aEl).Members.Count - 1 do
      begin
        lChild := TPasElement(TPasMembersType(aEl).Members[k]);
        if (lChild is TPasProcedure) or (lChild is TPasVariable) then
          Visit(lChild);
      end;
    end
    else if aEl is TPasArrayType then
      Visit(Owned(TPasArrayType(aEl).ElType))
    else if aEl is TPasSetType then
      Visit(Owned(TPasSetType(aEl).EnumType))
    else if aEl is TPasPointerType then
      Visit(Owned(TPasPointerType(aEl).DestType))
    else if aEl is TPasAliasType then
      Visit(Owned(TPasAliasType(aEl).DestType));
  end;

  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
      Visit(TPasElement(aDecls[k]));
  end;

begin
  lSites := nil;
  lSeen := TFPList.Create;
  try
    if aMod.InterfaceSection <> nil then
      WalkDecls(aMod.InterfaceSection.Declarations);
    if aMod.ImplementationSection <> nil then
      WalkDecls(aMod.ImplementationSection.Declarations);
  finally
    lSeen.Free;
  end;
  Result := lSites;
end;


function TFpSonarResolver.TryUnconstrainedSpecializations(
  out aNodes: TPasElementArray): boolean;
var
  lMod: TPasModule;
  lSites: TPasElementArray;
  i: integer;

  procedure Add(aEl: TPasElement);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
  end;

  // Flags aSite when its generic declares type parameters and constrains none.
  procedure Judge(aSite: TPasSpecializeType);
  var
    lGeneric: TPasGenericType;
    lTemplate: TPasGenericTemplateType;
    lTypeParams, k: integer;
  begin
    if not (aSite.DestType is TPasGenericType) then
      Exit;
    lGeneric := TPasGenericType(aSite.DestType);
    if lGeneric.GenericTemplateTypes = nil then
      Exit;
    lTypeParams := 0;
    for k := 0 to lGeneric.GenericTemplateTypes.Count - 1 do
    begin
      lTemplate := TPasGenericTemplateType(lGeneric.GenericTemplateTypes[k]);
      // A const parameter's type annotation is not a type constraint.
      if lTemplate.IsConst then
        Continue;
      if Length(lTemplate.Constraints) > 0 then
        Exit;
      Inc(lTypeParams);
    end;
    if lTypeParams > 0 then
      Add(aSite);
  end;

begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    lSites := SpecializationSites(lMod);
    for i := 0 to High(lSites) do
      Judge(TPasSpecializeType(lSites[i]));
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryDeepNestedSpecializations(aMaxDepth: integer;
  out aNodes: TPasElementArray; out aDepths: TFpSonarDepthArray): boolean;
var
  lMod: TPasModule;
  lSites: TPasElementArray;
  i: integer;

  procedure Add(aEl: TPasElement; aDepth: integer);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
    SetLength(aDepths, Length(aDepths) + 1);
    aDepths[High(aDepths)] := aDepth;
  end;

  { 1 + the deepest specialization written inside aEl's own parameter list; 0
    for anything that is not a specialization. A parameter naming a separately
    declared type resolves to that type's own node, which is nesting in the
    resolved tree but not in the source, so only owned parameters count. }
  function SpecDepth(aEl: TPasElement): integer;
  var
    lParams: TFPList;
    lParam: TPasElement;
    lBest, lDepth, k: integer;
  begin
    Result := 0;
    if aEl is TPasSpecializeType then
      lParams := TPasSpecializeType(aEl).Params
    else if aEl is TInlineSpecializeExpr then
      lParams := TInlineSpecializeExpr(aEl).Params
    else
      Exit;
    lBest := 0;
    if lParams <> nil then
      for k := 0 to lParams.Count - 1 do
      begin
        lParam := TPasElement(lParams[k]);
        if (lParam = nil) or (lParam.Parent <> aEl) then
          Continue;
        lDepth := SpecDepth(lParam);
        if lDepth > lBest then
          lBest := lDepth;
      end;
    Result := 1 + lBest;
  end;

  procedure Judge(aSite: TPasSpecializeType);
  var
    lDepth: integer;
  begin
    lDepth := SpecDepth(aSite);
    if lDepth > aMaxDepth then
      Add(aSite, lDepth);
  end;

begin
  aNodes := nil;
  aDepths := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    lSites := SpecializationSites(lMod);
    for i := 0 to High(lSites) do
      Judge(TPasSpecializeType(lSites[i]));
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aDepths := nil;
      Result := False;
    end;
  end;
end;


{ The anonymous-method capture sites of aMod: aLoopVarSites every reference
  inside an anonymous method to the control variable of an enclosing classic
  for loop, aSelfSites every anonymous procedure whose body reaches the
  enclosing instance. }
procedure AnonMethodCaptureSites(aMod: TPasModule;
  out aLoopVarSites, aSelfSites: TPasElementArray);
var
  lLoopDecls: TPasElementArray;   // control decls of the enclosing classic loops
  lSeenAnons, lSeenDecls: TPasElementArray;   // capture pairs already recorded

  procedure Add(var aList: TPasElementArray; aEl: TPasElement);
  var
    k: integer;
  begin
    for k := 0 to High(aList) do
      if aList[k] = aEl then
        Exit;                          // de-dup the same anchor
    SetLength(aList, Length(aList) + 1);
    aList[High(aList)] := aEl;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // True iff aDecl is the analysed anon proc's own param/local: its Parent
  // ancestry reaches aAnon before the module.
  function IsOwnLocal(aDecl, aAnon: TPasElement): boolean;
  var
    lEl: TPasElement;
    lGuard: integer;
  begin
    Result := False;
    lEl := aDecl;
    lGuard := 0;
    while (lEl <> nil) and (lGuard < 200) do
    begin
      if lEl = aAnon then
        Exit(True);
      if lEl is TPasModule then
        Exit(False);
      lEl := lEl.Parent;
      Inc(lGuard);
    end;
  end;

  // True iff aDecl is the control declaration of an enclosing classic for loop.
  function IsCapturedLoopVar(aDecl: TPasElement): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(lLoopDecls) do
      if lLoopDecls[k] = aDecl then
        Exit(True);
  end;

  { True iff (aAnon, aDecl) is a capture not recorded yet, recording it if so. }
  function NewCapturePair(aAnon, aDecl: TPasElement): boolean;
  var
    k: integer;
  begin
    for k := 0 to High(lSeenAnons) do
      if (lSeenAnons[k] = aAnon) and (lSeenDecls[k] = aDecl) then
        Exit(False);
    SetLength(lSeenAnons, Length(lSeenAnons) + 1);
    SetLength(lSeenDecls, Length(lSeenDecls) + 1);
    lSeenAnons[High(lSeenAnons)] := aAnon;
    lSeenDecls[High(lSeenDecls)] := aDecl;
    Result := True;
  end;

  // The routine owning aEl, nil when none encloses it.
  function OwningRoutine(aEl: TPasElement): TPasProcedure;
  begin
    Result := nil;
    while aEl <> nil do
    begin
      if aEl is TPasProcedure then
        Exit(TPasProcedure(aEl));
      aEl := aEl.Parent;
    end;
  end;

  // True iff aProc runs on an instance rather than on a class.
  function IsInstanceRoutine(aProc: TPasProcedure): boolean;
  begin
    Result := (aProc <> nil) and not (aProc is TPasClassProcedure)
      and not (aProc is TPasClassFunction) and not aProc.IsStatic;
  end;

  { True iff aRef reaches the enclosing instance: the Self argument of an
    enclosing method, or an instance member named without a qualifier. }
  function IsSelfEvidence(aRef: TResolvedReference): boolean;
  var
    lDecl: TPasElement;
  begin
    Result := False;
    lDecl := aRef.Declaration;
    if lDecl = nil then
      Exit;
    if (lDecl is TPasArgument) and SameText(lDecl.Name, 'Self') then
      Exit(IsInstanceRoutine(OwningRoutine(lDecl)));
    if rrfDotScope in aRef.Flags then
      Exit;
    if not (lDecl.Parent is TPasMembersType) then
      Exit;
    if lDecl is TPasVariable then
      Result := not (lDecl is TPasConst)
        and (TPasVariable(lDecl).VarModifiers * [vmClass, vmStatic] = [])
    else if lDecl is TPasProcedure then
      Result := IsInstanceRoutine(TPasProcedure(lDecl));
  end;

  procedure HandleAnon(aPE: TProcedureExpr); forward;

  { Walks aExpr's tree. A nested TProcedureExpr is dispatched to HandleAnon. }
  procedure ScanExpr(aExpr: TPasExpr; aAnon: TPasElement);
  var
    k: integer;
    lRef: TResolvedReference;
    lDecl: TPasElement;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TProcedureExpr then
      HandleAnon(TProcedureExpr(aExpr))
    else if aExpr is TPrimitiveExpr then
    begin
      if aAnon = nil then
        Exit;
      lRef := RefOf(aExpr);
      if lRef = nil then
        Exit;                        // unresolved capture => silence
      lDecl := lRef.Declaration;
      if lDecl = nil then
        Exit;
      if IsOwnLocal(lDecl, aAnon) then
        Exit;                        // the anon proc's own param/local
      if IsCapturedLoopVar(lDecl) and NewCapturePair(aAnon, lDecl) then
        Add(aLoopVarSites, aExpr);
      if IsSelfEvidence(lRef) then
        Add(aSelfSites, aAnon);
    end
    else if aExpr is TParamsExpr then
    begin
      ScanExpr(TParamsExpr(aExpr).Value, aAnon);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        ScanExpr(TParamsExpr(aExpr).Params[k], aAnon);
    end
    else if aExpr is TBinaryExpr then
    begin
      ScanExpr(TBinaryExpr(aExpr).left, aAnon);
      ScanExpr(TBinaryExpr(aExpr).right, aAnon);
    end
    else if aExpr is TUnaryExpr then
      ScanExpr(TUnaryExpr(aExpr).Operand, aAnon);
  end;

  // The direct child sub-statements of aStmt (the single tree-shape encoding,
  // reproduced query-locally).
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  { Walks aStmt's control/value expressions, then recurses every sub-statement. }
  procedure WalkStmt(aStmt: TPasImplElement; aAnon: TPasElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    lFor: TPasImplForLoop;
    lRef: TResolvedReference;
    lPushed: boolean;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      ScanExpr(TPasImplAssign(aStmt).Left, aAnon);
      ScanExpr(TPasImplAssign(aStmt).Right, aAnon);
    end
    else if aStmt is TPasImplSimple then
      ScanExpr(TPasImplSimple(aStmt).Expr, aAnon)
    else if aStmt is TPasImplIfElse then
      ScanExpr(TPasImplIfElse(aStmt).ConditionExpr, aAnon)
    else if aStmt is TPasImplWhileDo then
      ScanExpr(TPasImplWhileDo(aStmt).ConditionExpr, aAnon)
    else if aStmt is TPasImplRepeatUntil then
      ScanExpr(TPasImplRepeatUntil(aStmt).ConditionExpr, aAnon)
    else if aStmt is TPasImplCaseOf then
      ScanExpr(TPasImplCaseOf(aStmt).CaseExpr, aAnon)
    else if aStmt is TPasImplRaise then
    begin
      ScanExpr(TPasImplRaise(aStmt).ExceptObject, aAnon);
      ScanExpr(TPasImplRaise(aStmt).ExceptAddr, aAnon);
    end
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            ScanExpr(TPasExpr(lWith.Expressions[k]), aAnon);
    end;

    lPushed := False;
    if aStmt is TPasImplForLoop then
    begin
      lFor := TPasImplForLoop(aStmt);
      ScanExpr(lFor.StartExpr, aAnon);
      ScanExpr(lFor.EndExpr, aAnon);
      if not lFor.IsVarDef then
      begin
        lRef := RefOf(lFor.VariableName);
        if (lRef <> nil) and (lRef.Declaration <> nil) then
        begin
          SetLength(lLoopDecls, Length(lLoopDecls) + 1);
          lLoopDecls[High(lLoopDecls)] := lRef.Declaration;
          lPushed := True;
        end;
      end;
    end;

    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      WalkStmt(lKids[k], aAnon);

    if lPushed then
      SetLength(lLoopDecls, Length(lLoopDecls) - 1);
  end;

  { Walks aDecls routines, scanning their statements for anon procs and
    recursing into nested routines. aAnon is the closure they are declared in,
    nil at module level. }
  procedure WalkDecls(aDecls: TFPList; aAnon: TPasElement);
  var
    k: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if (lEl is TPasProcedure) and (TPasProcedure(lEl).Body <> nil) then
      begin
        lProc := TPasProcedure(lEl);
        // The resolver's specialized body copies repeat one written closure.
        if (lProc.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(lProc.CustomData).SpecializedFromItem <> nil) then
          Continue;
        if lProc.Body.Body <> nil then
          WalkStmt(lProc.Body.Body, aAnon);
        WalkDecls(lProc.Body.Declarations, aAnon);
      end;
    end;
  end;

  { Classifies the captures of one anonymous method: walk its body and its own
    nested routines with the anon proc as the own-local sentinel. }
  procedure HandleAnon(aPE: TProcedureExpr);
  begin
    if (aPE = nil) or (aPE.Proc = nil) then
      Exit;
    if not (aPE.Proc.GetProcTypeEnum in
      [ptAnonymousProcedure, ptAnonymousFunction]) then
      Exit;
    if aPE.Proc.Body = nil then
      Exit;
    if aPE.Proc.Body.Body <> nil then
      WalkStmt(aPE.Proc.Body.Body, aPE.Proc);
    WalkDecls(aPE.Proc.Body.Declarations, aPE.Proc);
  end;

begin
  aLoopVarSites := nil;
  aSelfSites := nil;
  lLoopDecls := nil;
  lSeenAnons := nil;
  lSeenDecls := nil;
  if aMod.InterfaceSection <> nil then
    WalkDecls(aMod.InterfaceSection.Declarations, nil);
  if aMod.ImplementationSection <> nil then
    WalkDecls(aMod.ImplementationSection.Declarations, nil);
end;


function TFpSonarResolver.TryAnonMethodLoopVarCaptures(
  out aNodes: TPasElementArray): boolean;
var
  lMod: TPasModule;
  lSelfSites: TPasElementArray;
begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    AnonMethodCaptureSites(lMod, aNodes, lSelfSites);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryAnonMethodSelfCaptures(
  out aNodes: TPasElementArray): boolean;
var
  lMod: TPasModule;
  lLoopVarSites: TPasElementArray;
begin
  aNodes := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    AnonMethodCaptureSites(lMod, lLoopVarSites, aNodes);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryInconsistentCasingSites(
  out aNodes: TPasElementArray; out aUsed, aCanonical: TFpSonarStringArray): boolean;
const
  cCheckUnitNames = True;
  cCheckKeywords = False;
var
  lMod: TPasModule;
  lCands: TPasElementArray;

  procedure Add(aEl: TPasElement; const aUse, aCanon: string);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
    SetLength(aUsed, Length(aUsed) + 1);
    aUsed[High(aUsed)] := aUse;
    SetLength(aCanonical, Length(aCanonical) + 1);
    aCanonical[High(aCanonical)] := aCanon;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  procedure AddCand(aEl: TPasElement);
  begin
    SetLength(lCands, Length(lCands) + 1);
    lCands[High(lCands)] := aEl;
  end;

  // Appends every identifier-reference leaf (a TPrimitiveExpr carrying a resolved
  // reference) in aExpr's tree to lCands.
  procedure CollectRefs(aExpr: TPasExpr);
  var
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TPrimitiveExpr then
    begin
      if RefOf(aExpr) <> nil then
        AddCand(aExpr);
    end
    else if aExpr is TParamsExpr then
    begin
      CollectRefs(TParamsExpr(aExpr).Value);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        CollectRefs(TParamsExpr(aExpr).Params[k]);
    end
    else if aExpr is TBinaryExpr then
    begin
      CollectRefs(TBinaryExpr(aExpr).left);
      CollectRefs(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      CollectRefs(TUnaryExpr(aExpr).Operand);
  end;

  // The direct child sub-statements of aStmt.
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // Collects the identifier references held directly by aStmt's control/value
  // expressions, then recurses every sub-statement.
  procedure CollectStmtRefs(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      CollectRefs(TPasImplAssign(aStmt).Left);
      CollectRefs(TPasImplAssign(aStmt).Right);
    end
    else if aStmt is TPasImplSimple then
      CollectRefs(TPasImplSimple(aStmt).Expr)
    else if aStmt is TPasImplIfElse then
      CollectRefs(TPasImplIfElse(aStmt).ConditionExpr)
    else if aStmt is TPasImplWhileDo then
      CollectRefs(TPasImplWhileDo(aStmt).ConditionExpr)
    else if aStmt is TPasImplRepeatUntil then
      CollectRefs(TPasImplRepeatUntil(aStmt).ConditionExpr)
    else if aStmt is TPasImplForLoop then
    begin
      CollectRefs(TPasImplForLoop(aStmt).StartExpr);
      CollectRefs(TPasImplForLoop(aStmt).EndExpr);
    end
    else if aStmt is TPasImplCaseOf then
      CollectRefs(TPasImplCaseOf(aStmt).CaseExpr)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            CollectRefs(TPasExpr(lWith.Expressions[k]));
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      CollectStmtRefs(lKids[k]);
  end;

  // Walks aDecls for routines, collecting their statement references
  // and recursing into nested routines.
  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if (lEl is TPasProcedure) and (TPasProcedure(lEl).Body <> nil) then
      begin
        lProc := TPasProcedure(lEl);
        if lProc.Body.Body <> nil then
          CollectStmtRefs(lProc.Body.Body);
        WalkDecls(lProc.Body.Declarations);
      end;
    end;
  end;

var
  i: integer;
  lNode: TPasElement;
  lRef: TResolvedReference;
  lDecl: TPasElement;
  lUse, lCanon: string;
begin
  aNodes := nil;
  aUsed := nil;
  aCanonical := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    SetLength(lCands, 0);
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);

    for i := 0 to High(lCands) do
    begin
      lNode := lCands[i];
      lRef := RefOf(lNode);
      if (lRef = nil) or (lRef.Declaration = nil) then
        Continue;
      lDecl := lRef.Declaration;
      // A unit-name reference (the Unit in Unit.Symbol) is only compared when
      // cCheckUnitNames is on; the canonical spelling is the module's own Name.
      if (lDecl is TPasModule) and (not cCheckUnitNames) then
        Continue;
      lCanon := lDecl.Name;
      // An anonymous/synthetic declaration carries no canonical casing — abstain
      // rather than invent the "correct" spelling.
      if lCanon = '' then
        Continue;
      lUse := TPrimitiveExpr(lNode).Value;
      if lUse = '' then
        Continue;
      // The casing decision: same identifier case-insensitively, but a different
      // letter casing => the use-site diverges from the authoritative declaration.
      if (CompareText(lUse, lCanon) = 0) and (CompareStr(lUse, lCanon) <> 0) then
        Add(lNode, lUse, lCanon);
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aUsed := nil;
      aCanonical := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryDescendantNamingViolationSites(
  out aNodes: TPasElementArray; out aBases: TFpSonarStringArray): boolean;
const
  cBaseException = 'Exception';
  cBaseInterface = 'IInterface';
  // The COM interface root the resolver links a bare interface to
  cInterfaceRootAlias = 'IUnknown';
  cMatchInterfaces = True;
var
  lMod: TPasModule;

  procedure Add(aEl: TPasElement; const aBase: string);
  begin
    SetLength(aNodes, Length(aNodes) + 1);
    aNodes[High(aNodes)] := aEl;
    SetLength(aBases, Length(aBases) + 1);
    aBases[High(aBases)] := aBase;
  end;

  { Walks aClass's resikced ancestor-scope chain.
     aFound is set True iff a resolved ancestor whose Name equals aName1
    (or the optional alias aName2) is reached. }
  function Descends(aClass: TPasClassType; const aName1, aName2: string;
    out aFound: boolean): boolean;
  var
    lScope: TPasClassScope;
    lGuard: integer;
  begin
    aFound := False;
    Result := False;
    if not (aClass.CustomData is TPasClassScope) then
      Exit;
    lScope := TPasClassScope(aClass.CustomData);
    lGuard := 0;
    while (lScope <> nil) and (lGuard < 100) do
    begin
      if (lScope.Element <> nil)
        and (SameText(lScope.Element.Name, aName1)
        or ((aName2 <> '') and SameText(lScope.Element.Name, aName2))) then
      begin
        aFound := True;
        Exit(True);
      end;
      lScope := lScope.AncestorScope;
      Inc(lGuard);
    end;
    Result := True;
  end;

  // Tests aDecl against the kind-matching configured base and appends it when descent is proven.
  procedure Consider(aDecl: TPasElement);
  var
    lCls: TPasClassType;
    lFound: boolean;
  begin
    if not (aDecl is TPasClassType) then
      Exit;
    lCls := TPasClassType(aDecl);
    if lCls.ObjKind = okClass then
    begin
      if Descends(lCls, cBaseException, '', lFound) and lFound then
        Add(aDecl, cBaseException);
    end
    else if cMatchInterfaces and (lCls.ObjKind = okInterface) then
    begin
      if Descends(lCls, cBaseInterface, cInterfaceRootAlias, lFound) and lFound then
        Add(aDecl, cBaseInterface);
    end;
  end;

  procedure WalkDecls(aDecls: TFPList);
  var
    k: integer;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
      Consider(TPasElement(aDecls[k]));
  end;

begin
  aNodes := nil;
  aBases := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      WalkDecls(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      WalkDecls(lMod.ImplementationSection.Declarations);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aBases := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryUsesUnitPaths(out aNodes: TPasElementArray;
  out aPaths: TFpSonarStringArray): boolean;
var
  lMod: TPasModule;

{ Appends every locatable used unit in aClause as a (node, path) pair. }
  procedure ScanUses(const aClause: TPasUsesClause);
  var
    k: integer;
    lUse: TPasUsesUnit;
    lPath: string;
  begin
    for k := 0 to High(aClause) do
    begin
      lUse := aClause[k];
      // The implicit System / any synthesized entry has no source name.
      if lUse.Expr = nil then
        Continue;
      lPath := LocateUnit(lUse.Name);
      if lPath = '' then
        Continue;
      SetLength(aNodes, Length(aNodes) + 1);
      aNodes[High(aNodes)] := lUse;
      SetLength(aPaths, Length(aPaths) + 1);
      aPaths[High(aPaths)] := lPath;
    end;
  end;

begin
  aNodes := nil;
  aPaths := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    // A program/library has no interface section; guard each.
    if lMod.InterfaceSection <> nil then
      ScanUses(lMod.InterfaceSection.UsesClause);
    if lMod.ImplementationSection <> nil then
      ScanUses(lMod.ImplementationSection.UsesClause);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aPaths := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryReferenceSites(out aNodes: TPasElementArray;
  out aDecls: TPasElementArray): boolean;
var
  lMod: TPasModule;
  lCands: TPasElementArray;

  procedure AddCand(aEl: TPasElement);
  begin
    SetLength(lCands, Length(lCands) + 1);
    lCands[High(lCands)] := aEl;
  end;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // Appends every identifier-reference leaf in aExpr's tree to lCands.
  procedure CollectRefs(aExpr: TPasExpr);
  var
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TPrimitiveExpr then
    begin
      if RefOf(aExpr) <> nil then
        AddCand(aExpr);
    end
    else if aExpr is TParamsExpr then
    begin
      CollectRefs(TParamsExpr(aExpr).Value);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        CollectRefs(TParamsExpr(aExpr).Params[k]);
    end
    else if aExpr is TBinaryExpr then
    begin
      CollectRefs(TBinaryExpr(aExpr).left);
      CollectRefs(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      CollectRefs(TUnaryExpr(aExpr).Operand);
  end;

  // The direct child sub-statements of aStmt
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // Collects the identifier references held directly by aStmt's control/value
  // expressions, then recurses every sub-statement.
  procedure CollectStmtRefs(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      CollectRefs(TPasImplAssign(aStmt).Left);
      CollectRefs(TPasImplAssign(aStmt).Right);
    end
    else if aStmt is TPasImplSimple then
      CollectRefs(TPasImplSimple(aStmt).Expr)
    else if aStmt is TPasImplIfElse then
      CollectRefs(TPasImplIfElse(aStmt).ConditionExpr)
    else if aStmt is TPasImplWhileDo then
      CollectRefs(TPasImplWhileDo(aStmt).ConditionExpr)
    else if aStmt is TPasImplRepeatUntil then
      CollectRefs(TPasImplRepeatUntil(aStmt).ConditionExpr)
    else if aStmt is TPasImplForLoop then
    begin
      CollectRefs(TPasImplForLoop(aStmt).StartExpr);
      CollectRefs(TPasImplForLoop(aStmt).EndExpr);
    end
    else if aStmt is TPasImplCaseOf then
      CollectRefs(TPasImplCaseOf(aStmt).CaseExpr)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            CollectRefs(TPasExpr(lWith.Expressions[k]));
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      CollectStmtRefs(lKids[k]);
  end;

  { Walks aDecls, collecting references from each declaration's value-carrier
    expressions and each routine's statements; recurses into nested routine declarations. }
  procedure CollectDeclRefs(aDecls: TFPList);
  var
    k, m: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if lEl is TPasProcedure then
      begin
        lProc := TPasProcedure(lEl);
        if (lProc.ProcType <> nil) and (lProc.ProcType.Args <> nil) then
          for m := 0 to lProc.ProcType.Args.Count - 1 do
            if TObject(lProc.ProcType.Args[m]) is TPasArgument then
              CollectRefs(TPasArgument(lProc.ProcType.Args[m]).ValueExpr);
        if lProc.Body <> nil then
        begin
          if lProc.Body.Body <> nil then
            CollectStmtRefs(lProc.Body.Body);
          CollectDeclRefs(lProc.Body.Declarations);
        end;
      end
      // TPasConst descends TPasVariable, so this one branch covers const, var
      // and field initializer expressions (TPasVariable.Expr).
      else if lEl is TPasVariable then
        CollectRefs(TPasVariable(lEl).Expr);
    end;
  end;

var
  i: integer;
  lDecl: TPasElement;
begin
  aNodes := nil;
  aDecls := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    SetLength(lCands, 0);
    if lMod.InterfaceSection <> nil then
      CollectDeclRefs(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      CollectDeclRefs(lMod.ImplementationSection.Declarations);
    // A program's main begin..end. lives in its InitializationSection;
    // a unit's init/finalization statements are reachable here too.
    if lMod.InitializationSection <> nil then
      CollectStmtRefs(lMod.InitializationSection);
    if lMod.FinalizationSection <> nil then
      CollectStmtRefs(lMod.FinalizationSection);

    for i := 0 to High(lCands) do
    begin
      lDecl := ReferencedDecl(lCands[i]);
      if lDecl = nil then
        Continue;
      SetLength(aNodes, Length(aNodes) + 1);
      aNodes[High(aNodes)] := lCands[i];
      SetLength(aDecls, Length(aDecls) + 1);
      aDecls[High(aDecls)] := lDecl;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      aNodes := nil;
      aDecls := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryStatementAccess(aStmt: TPasElement;
  out aReadNodes: TPasElementArray; out aReadDecls: TPasElementArray;
  out aWriteDecls: TPasElementArray): boolean;
var
  lAnswered: boolean;

  // The resolved reference attached to a node, or nil when unresolved.
  function RefOf(aExpr: TPasElement): TResolvedReference;
  begin
    if (aExpr <> nil) and (aExpr.CustomData is TResolvedReference) then
      Result := TResolvedReference(aExpr.CustomData)
    else
      Result := nil;
  end;

  // Appends aEl to aList.
  procedure AddTo(var aList: TPasElementArray; aEl: TPasElement);
  begin
    SetLength(aList, Length(aList) + 1);
    aList[High(aList)] := aEl;
  end;

  { Files aExpr's own reference under its access kind: an out argument and an
    argument whose overload is not yet decided count as read and write, a
    dereferenced pointer as read only. }
  procedure Classify(aExpr: TPasExpr);
  var
    lRef: TResolvedReference;
    lDeref: boolean;
  begin
    lRef := RefOf(aExpr);
    if lRef = nil then
      Exit;
    if lRef.Declaration = nil then
    begin
      lAnswered := False;
      Exit;
    end;
    lDeref := (aExpr.Parent is TUnaryExpr)
      and (TUnaryExpr(aExpr.Parent).OpCode = eopDeref);
    if lDeref or (lRef.Access in rraAllRead)
      or (lRef.Access in [rraOutParam, rraParamToUnknownProc]) then
    begin
      AddTo(aReadNodes, aExpr);
      AddTo(aReadDecls, lRef.Declaration);
    end;
    if lDeref then
      Exit;
    if (lRef.Access in rraAllWrite) or (lRef.Access = rraParamToUnknownProc) then
      AddTo(aWriteDecls, lRef.Declaration);
  end;

  { Classifies every identifier leaf of aExpr's tree; one unresolved leaf, or
    one expression class this walker does not model, withdraws the answer. }
  procedure CollectRefs(aExpr: TPasExpr);
  var
    m: integer;
  begin
    if aExpr = nil then
      Exit;
    // Write, WriteLn and Str carry their field widths beside the value.
    CollectRefs(aExpr.Format1);
    CollectRefs(aExpr.Format2);
    if aExpr is TPrimitiveExpr then
    begin
      if RefOf(aExpr) <> nil then
        Classify(aExpr)
      else if TPrimitiveExpr(aExpr).Kind = pekIdent then
        lAnswered := False;
    end
    else if aExpr is TParamsExpr then
    begin
      CollectRefs(TParamsExpr(aExpr).Value);
      for m := 0 to High(TParamsExpr(aExpr).Params) do
        CollectRefs(TParamsExpr(aExpr).Params[m]);
    end
    else if aExpr is TBinaryExpr then
    begin
      CollectRefs(TBinaryExpr(aExpr).left);
      CollectRefs(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      CollectRefs(TUnaryExpr(aExpr).Operand)
    else if aExpr is TNamedArgExpr then
      CollectRefs(TNamedArgExpr(aExpr).ValueExpr)
    else if aExpr is TArrayValues then
    begin
      for m := 0 to High(TArrayValues(aExpr).Values) do
        CollectRefs(TArrayValues(aExpr).Values[m]);
    end
    else if aExpr is TRecordValues then
    begin
      for m := 0 to High(TRecordValues(aExpr).Fields) do
        CollectRefs(TRecordValues(aExpr).Fields[m].ValueExp);
    end
    else if aExpr is TInlineSpecializeExpr then
      CollectRefs(TInlineSpecializeExpr(aExpr).NameExpr)
    // An anonymous method body is an operand, not a declaration-list entry.
    else if aExpr is TProcedureExpr then
      lAnswered := False
    // The leaves that carry no identifier of their own.
    else if not ((aExpr is TNilExpr) or (aExpr is TBoolConstExpr)
      or (aExpr is TSelfExpr) or (aExpr is TInheritedExpr)) then
      lAnswered := False;
  end;

  // Classifies every expression of aList, ignoring any other element kind.
  procedure CollectExprList(aList: TFPList);
  var
    m: integer;
  begin
    if aList = nil then
      Exit;
    for m := 0 to aList.Count - 1 do
      if TObject(aList[m]) is TPasExpr then
        CollectRefs(TPasExpr(aList[m]));
  end;

  // Files aExpr's reference as a write whatever access kind it carries.
  procedure ClassifyControlVar(aExpr: TPasExpr);
  var
    lRef: TResolvedReference;
  begin
    lRef := RefOf(aExpr);
    if (lRef <> nil) and (lRef.Declaration <> nil) then
      AddTo(aWriteDecls, lRef.Declaration)
    else if (aExpr is TPrimitiveExpr)
      and (TPrimitiveExpr(aExpr).Kind = pekIdent) then
      lAnswered := False;
  end;

  // The statement kinds that legitimately carry no value or control expression.
  function IsExpressionFree(aEl: TPasElement): boolean;
  begin
    Result := (aEl is TPasImplTry) or (aEl is TPasImplTryHandler)
      or (aEl is TPasImplBeginBlock) or (aEl is TPasImplCaseElse)
      or (aEl is TPasImplExceptOn) or (aEl is TPasImplLabelMark)
      or (aEl is TPasImplCommandBase);
  end;

begin
  aReadNodes := nil;
  aReadDecls := nil;
  aWriteDecls := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) or (aStmt = nil) then
    Exit;
  if aStmt is TPasImplAsmStatement then
    Exit;

  lAnswered := True;
  try
    if aStmt is TPasImplAssign then
    begin
      CollectRefs(TPasImplAssign(aStmt).Left);
      CollectRefs(TPasImplAssign(aStmt).Right);
    end
    else if aStmt is TPasImplSimple then
      CollectRefs(TPasImplSimple(aStmt).Expr)
    else if aStmt is TPasImplIfElse then
      CollectRefs(TPasImplIfElse(aStmt).ConditionExpr)
    else if aStmt is TPasImplWhileDo then
      CollectRefs(TPasImplWhileDo(aStmt).ConditionExpr)
    else if aStmt is TPasImplRepeatUntil then
      CollectRefs(TPasImplRepeatUntil(aStmt).ConditionExpr)
    else if aStmt is TPasImplForLoop then
    begin
      CollectRefs(TPasImplForLoop(aStmt).StartExpr);
      CollectRefs(TPasImplForLoop(aStmt).EndExpr);
      ClassifyControlVar(TPasImplForLoop(aStmt).VariableName);
    end
    else if aStmt is TPasImplCaseOf then
      CollectRefs(TPasImplCaseOf(aStmt).CaseExpr)
    else if aStmt is TPasImplCaseStatement then
      CollectExprList(TPasImplCaseStatement(aStmt).Expressions)
    else if aStmt is TPasImplWithDo then
      CollectExprList(TPasImplWithDo(aStmt).Expressions)
    else if aStmt is TPasImplRaise then
    begin
      CollectRefs(TPasImplRaise(aStmt).ExceptObject);
      CollectRefs(TPasImplRaise(aStmt).ExceptAddr);
    end
    // An unmodelled statement kind withdraws the answer.
    else if not IsExpressionFree(aStmt) then
      lAnswered := False;
    Result := lAnswered;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
      Result := False;
  end;
  if not Result then
  begin
    aReadNodes := nil;
    aReadDecls := nil;
    aWriteDecls := nil;
  end;
end;


function TFpSonarResolver.TryHintedSymbolUses(out aUses: TFpSonarHintedUseArray;
  out aModuleHints: TPasMemberHints): boolean;
var
  lNodes, lDecls: TPasElementArray;
  lMod: TPasModule;
  i: integer;
begin
  aUses := nil;
  aModuleHints := [];
  Result := False;
  if not TryReferenceSites(lNodes, lDecls) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    aModuleHints := lMod.Hints;
    for i := 0 to High(lDecls) do
    begin
      if lDecls[i].Hints = [] then
        Continue;
      SetLength(aUses, Length(aUses) + 1);
      aUses[High(aUses)].Node := lNodes[i];
      aUses[High(aUses)].Name := lDecls[i].Name;
      aUses[High(aUses)].Hints := lDecls[i].Hints;
    end;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aUses := nil;
      aModuleHints := [];
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryIdentifierNameSites(out aNodes: TPasElementArray;
  out aNames: TFpSonarStringArray): boolean;
var
  lMod: TPasModule;

// Appends every pekIdent identifier leaf in aExpr's tree, pairing the leaf with its spelling.
  procedure CollectNames(aExpr: TPasExpr);
  var
    k: integer;
  begin
    if aExpr = nil then
      Exit;
    if aExpr is TPrimitiveExpr then
    begin
      if TPrimitiveExpr(aExpr).Kind = pekIdent then
      begin
        SetLength(aNodes, Length(aNodes) + 1);
        aNodes[High(aNodes)] := aExpr;
        SetLength(aNames, Length(aNames) + 1);
        aNames[High(aNames)] := TPrimitiveExpr(aExpr).Value;
      end;
    end
    else if aExpr is TParamsExpr then
    begin
      CollectNames(TParamsExpr(aExpr).Value);
      for k := 0 to High(TParamsExpr(aExpr).Params) do
        CollectNames(TParamsExpr(aExpr).Params[k]);
    end
    else if aExpr is TBinaryExpr then
    begin
      CollectNames(TBinaryExpr(aExpr).left);
      CollectNames(TBinaryExpr(aExpr).right);
    end
    else if aExpr is TUnaryExpr then
      CollectNames(TUnaryExpr(aExpr).Operand);
  end;

  // The direct child sub-statements of aStmt
  function ChildStmts(aStmt: TPasImplElement): TImplStmtArray;

    procedure AddStmt(aEl: TPasImplElement);
    begin
      if aEl = nil then
        Exit;
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aEl;
    end;

    procedure AddElems(aList: TFPList);
    var
      k: integer;
    begin
      if aList = nil then
        Exit;
      for k := 0 to aList.Count - 1 do
        if TObject(aList[k]) is TPasImplElement then
          AddStmt(TPasImplElement(aList[k]));
    end;

  begin
    SetLength(Result, 0);
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplIfElse then
    begin
      AddStmt(TPasImplIfElse(aStmt).IfBranch);
      AddStmt(TPasImplIfElse(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplWhileDo then
      AddStmt(TPasImplWhileDo(aStmt).Body)
    else if aStmt is TPasImplForLoop then
      AddStmt(TPasImplForLoop(aStmt).Body)
    else if aStmt is TPasImplWithDo then
      AddStmt(TPasImplWithDo(aStmt).Body)
    else if aStmt is TPasImplCaseStatement then
      AddStmt(TPasImplCaseStatement(aStmt).Body)
    else if aStmt is TPasImplExceptOn then
      AddStmt(TPasImplExceptOn(aStmt).Body)
    else if aStmt is TPasImplTry then
    begin
      AddElems(TPasImplBlock(aStmt).Elements);
      AddStmt(TPasImplTry(aStmt).FinallyExcept);
      AddStmt(TPasImplTry(aStmt).ElseBranch);
    end
    else if aStmt is TPasImplBlock then
      AddElems(TPasImplBlock(aStmt).Elements);
  end;

  // Collects identifier names held directly by aStmt's control/value expressions,
  // then recurses every sub-statement.
  procedure CollectStmtNames(aStmt: TPasImplElement);
  var
    lKids: TImplStmtArray;
    lWith: TPasImplWithDo;
    k: integer;
  begin
    if aStmt = nil then
      Exit;
    if aStmt is TPasImplAssign then
    begin
      CollectNames(TPasImplAssign(aStmt).Left);
      CollectNames(TPasImplAssign(aStmt).Right);
    end
    else if aStmt is TPasImplSimple then
      CollectNames(TPasImplSimple(aStmt).Expr)
    else if aStmt is TPasImplIfElse then
      CollectNames(TPasImplIfElse(aStmt).ConditionExpr)
    else if aStmt is TPasImplWhileDo then
      CollectNames(TPasImplWhileDo(aStmt).ConditionExpr)
    else if aStmt is TPasImplRepeatUntil then
      CollectNames(TPasImplRepeatUntil(aStmt).ConditionExpr)
    else if aStmt is TPasImplForLoop then
    begin
      CollectNames(TPasImplForLoop(aStmt).StartExpr);
      CollectNames(TPasImplForLoop(aStmt).EndExpr);
    end
    else if aStmt is TPasImplCaseOf then
      CollectNames(TPasImplCaseOf(aStmt).CaseExpr)
    else if aStmt is TPasImplWithDo then
    begin
      lWith := TPasImplWithDo(aStmt);
      if lWith.Expressions <> nil then
        for k := 0 to lWith.Expressions.Count - 1 do
          if TObject(lWith.Expressions[k]) is TPasExpr then
            CollectNames(TPasExpr(lWith.Expressions[k]));
    end;
    lKids := ChildStmts(aStmt);
    for k := 0 to High(lKids) do
      CollectStmtNames(lKids[k]);
  end;

  { Walks aDecls, collecting names from each declaration's value-carrier
    expressions and each body-bearing routine's statements;
    recurses into nested routine declarations }
  procedure CollectDeclNames(aDecls: TFPList);
  var
    k, m: integer;
    lEl: TPasElement;
    lProc: TPasProcedure;
  begin
    if aDecls = nil then
      Exit;
    for k := 0 to aDecls.Count - 1 do
    begin
      lEl := TPasElement(aDecls[k]);
      if lEl is TPasProcedure then
      begin
        lProc := TPasProcedure(lEl);
        if (lProc.ProcType <> nil) and (lProc.ProcType.Args <> nil) then
          for m := 0 to lProc.ProcType.Args.Count - 1 do
            if TObject(lProc.ProcType.Args[m]) is TPasArgument then
              CollectNames(TPasArgument(lProc.ProcType.Args[m]).ValueExpr);
        if lProc.Body <> nil then
        begin
          if lProc.Body.Body <> nil then
            CollectStmtNames(lProc.Body.Body);
          CollectDeclNames(lProc.Body.Declarations);
        end;
      end
      // TPasConst descends TPasVariable, so this one branch covers const, var
      // and field initializer expressions (TPasVariable.Expr).
      else if lEl is TPasVariable then
        CollectNames(TPasVariable(lEl).Expr);
    end;
  end;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if (FTopEngine = nil) or (not FSucceeded) then
    Exit;
  lMod := GetResolvedModule;
  if lMod = nil then
    Exit;

  try
    if lMod.InterfaceSection <> nil then
      CollectDeclNames(lMod.InterfaceSection.Declarations);
    if lMod.ImplementationSection <> nil then
      CollectDeclNames(lMod.ImplementationSection.Declarations);
    if lMod.InitializationSection <> nil then
      CollectStmtNames(lMod.InitializationSection);
    if lMod.FinalizationSection <> nil then
      CollectStmtNames(lMod.FinalizationSection);
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryRawByteStringCodePageMix(aNode: TPasElement;
  out aRawName, aCodePage: string): boolean;

  { The code page written on the declaration aExpr references, or ''. The
    resolution collapses a code-paged AnsiString and drops the page, so it is
    read from the declared type's alias chain. }
  function DeclaredCodePage(aExpr: TPasExpr): string;
  var
    lDecl: TPasElement;
    lType: TPasType;
    lGuard: integer;
  begin
    Result := '';
    lDecl := ReferencedDecl(aExpr);
    if lDecl is TPasVariable then
      lType := TPasVariable(lDecl).VarType
    else if lDecl is TPasArgument then
      lType := TPasArgument(lDecl).ArgType
    else
      Exit;
    lGuard := 0;
    while (lType is TPasAliasType) and (lGuard < 100) do
    begin
      lType := TPasAliasType(lType).DestType;
      Inc(lGuard);
    end;
    if lType is TPasStringType then
      Result := TPasStringType(lType).CodePageExpr;
  end;

var
  lAssign: TPasImplAssign;
  lLeft, lRight, lTarget, lSource: TFpSonarResolvedType;
  lRaw: TPasExpr;
  lDecl: TPasElement;
  lPage: string;

begin
  Result := False;
  aRawName := '';
  aCodePage := '';
  if (FTopEngine = nil) or (not FSucceeded)
    or not (aNode is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aNode);
  if lAssign.Kind <> akDefault then
    Exit;
  try
    if not TryResolvedType(lAssign.Left, lLeft)
      or not TryResolvedType(lAssign.Right, lRight) then
      Exit;
    if SameText(lLeft.TypeName, 'RawByteString') then
    begin
      lRaw := lAssign.Left;
      lPage := DeclaredCodePage(lAssign.Right);
    end
    else if SameText(lRight.TypeName, 'RawByteString') then
    begin
      lRaw := lAssign.Right;
      lPage := DeclaredCodePage(lAssign.Left);
    end
    else
      Exit;
    if lPage = '' then
      Exit;
    // A written cast IS the conversion the rule asks for.
    if TryTypecast(lAssign.Left, lTarget, lSource)
      or TryTypecast(lAssign.Right, lTarget, lSource) then
      Exit;
    lDecl := ReferencedDecl(lRaw);
    if lDecl = nil then
      Exit;
    aRawName := lDecl.Name;
    aCodePage := lPage;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aRawName := '';
      aCodePage := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryStringConcatInLoop(aNode: TPasElement;
  out aTargetName: string): boolean;

const
  cMaxParentDepth = 200;

  // True when a for, while or repeat loop encloses aStmt.
  function InsideLoop(aStmt: TPasElement): boolean;
  var
    lWalk: TPasElement;
    lDepth: integer;
  begin
    Result := False;
    lWalk := aStmt.Parent;
    lDepth := 0;
    while (lWalk <> nil) and (lDepth < cMaxParentDepth) do
    begin
      if (lWalk is TPasImplForLoop) or (lWalk is TPasImplWhileDo)
        or (lWalk is TPasImplRepeatUntil) then
        Exit(True);
      lWalk := lWalk.Parent;
      Inc(lDepth);
    end;
  end;

var
  lAssign: TPasImplAssign;
  lSum: TBinaryExpr;
  lTarget: TFpSonarResolvedType;
  lDecl: TPasElement;

begin
  Result := False;
  aTargetName := '';
  if (FTopEngine = nil) or (not FSucceeded)
    or not (aNode is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aNode);
  if (lAssign.Kind <> akDefault) or not (lAssign.Right is TBinaryExpr) then
    Exit;
  lSum := TBinaryExpr(lAssign.Right);
  if lSum.OpCode <> eopAdd then
    Exit;
  try
    if not TryResolvedType(lAssign.Left, lTarget)
      or (lTarget.Kind <> ltkString) then
      Exit;
    lDecl := ReferencedDecl(lAssign.Left);
    if (lDecl = nil) or (lDecl <> ReferencedDecl(lSum.Left)) then
      Exit;
    if not InsideLoop(lAssign) then
      Exit;
    aTargetName := lDecl.Name;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aTargetName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryStrToIntWithoutGuard(aExpr: TPasExpr;
  out aArgName: string): boolean;

const
  cMaxParentDepth = 200;

  // True when aCall calls the unit-level routine named StrToInt.
  function IsStrToIntCall(aCall: TParamsExpr): boolean;
  var
    lRef: TResolvedReference;
    lType: TPasProcedureType;
  begin
    Result := False;
    lRef := FTopEngine.GetParamsValueRef(aCall);
    if (lRef = nil) or not (lRef.Declaration is TPasProcedure)
      or not (lRef.Declaration.Parent is TPasSection)
      or not SameText(lRef.Declaration.Name, 'StrToInt') then
      Exit;
    lType := TPasProcedure(lRef.Declaration).ProcType;
    Result := (lType is TPasFunctionType) and (lType.Args <> nil)
      and (lType.Args.Count >= 1);
  end;

  { True when a try..except between aNode and the routine body protects aNode,
    which the handler's own body and the except-else branch do not. }
  function GuardedByHandler(aNode: TPasElement): boolean;
  var
    lNode, lParent: TPasElement;
    lTry: TPasImplTry;
    lDepth: integer;
  begin
    Result := False;
    lNode := aNode;
    lDepth := 0;
    while (lNode <> nil) and (lDepth < cMaxParentDepth) do
    begin
      lParent := lNode.Parent;
      if lParent is TPasImplTry then
      begin
        lTry := TPasImplTry(lParent);
        if (lTry.FinallyExcept is TPasImplTryExcept)
          and (lNode <> lTry.FinallyExcept) and (lNode <> lTry.ElseBranch) then
          Exit(True);
      end;
      lNode := lParent;
      Inc(lDepth);
    end;
  end;

  // True when aArg folds to a constant.
  function Folds(aArg: TPasExpr): boolean;
  var
    lValue: TResEvalValue;
  begin
    lValue := EvalGuarded(aArg);
    try
      Result := lValue <> nil;
    finally
      ReleaseEvalValue(lValue);
    end;
  end;

var
  lCall: TParamsExpr;
  lDecl: TPasElement;

begin
  Result := False;
  aArgName := '';
  if (FTopEngine = nil) or (not FSucceeded) or not (aExpr is TParamsExpr) then
    Exit;
  lCall := TParamsExpr(aExpr);
  if (lCall.Kind <> pekFuncParams) or (Length(lCall.Params) < 1) then
    Exit;
  try
    if not IsStrToIntCall(lCall) or Folds(lCall.Params[0])
      or GuardedByHandler(lCall) then
      Exit;
    lDecl := ReferencedDecl(lCall.Params[0]);
    if lDecl = nil then
      Exit;
    aArgName := lDecl.Name;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aArgName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.TryWideStringDeclaration(aDecl: TPasElement;
  out aDeclName: string): boolean;
var
  lType: TFpSonarResolvedType;

begin
  Result := False;
  aDeclName := '';
  if (FTopEngine = nil) or (not FSucceeded) or (aDecl = nil)
    or not ((aDecl is TPasVariable) or (aDecl is TPasArgument)) then
    Exit;
  try
    if not TryResolvedType(aDecl, lType)
      or not SameText(lType.TypeName, 'WideString') then
      Exit;
    aDeclName := aDecl.Name;
    Result := True;
  except
    // Tolerant-access: any resolver failure => degrade (caller silent).
    on E: Exception do
    begin
      aDeclName := '';
      Result := False;
    end;
  end;
end;


function TFpSonarResolver.SourceRow(aElement: TPasElement): integer;
var
  lRow, lCol: integer;
begin
  Result := 0;
  if aElement = nil then
    Exit;
  { Resolution runs with StoreSrcColumns on, so SourceLinenumber is a packed row+column; unpack the row. }
  TPasResolver.UnmangleSourceLineNumber(aElement.SourceLinenumber, lRow, lCol);
  Result := lRow;
end;


initialization
  RegisterMessage('rule.ResolveError.message', SResolveError);

finalization
  if GPpuFiles <> nil then
  begin
    while GPpuFiles.Count > 0 do
    begin
      if FileExists(GPpuFiles.ValueFromIndex[0]) then
        DeleteFile(GPpuFiles.ValueFromIndex[0]);
      GPpuFiles.Delete(0);
    end;
    FreeAndNil(GPpuFiles);
  end;
  FreeAndNil(GPpuGen);

end.

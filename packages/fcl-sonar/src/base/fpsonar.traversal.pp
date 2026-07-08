{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Parsed-model traversal: AST enumeration and the use-reference index

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Traversal;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree,
{$ELSE}
  Classes, SysUtils, PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Resolver;

type
  // A dynamic array of named type declarations (the EnumerateTypes result).
  TPasTypeArray = array of TPasType;

  // The kind tag of a value declaration: a constant, a class/record member
  // field, or a (section-level) variable.
  TPasValueKind = (vkConst, vkField, vkVar);

  // One value declaration: Decl is the const/field/var node (carries Name +
  // SourceLinenumber), Kind classifies it for the per-kind naming rules.
  TPasValueDecl = record
    Decl: TPasVariable;
    Kind: TPasValueKind;
  end;
  TPasValueDeclArray = array of TPasValueDecl;

  // A dynamic array of statement elements (child sub-statements of a node).
  TPasImplElementArray = array of TPasImplElement;

  // One routine with body: Decl carries its position (SourceLinenumber) and
  // Name; Block is its executable statement block.
  TAstRoutine = record
    Decl: TPasProcedure;
    Block: TPasImplBlock;
  end;
  TAstRoutineArray = array of TAstRoutine;

  // Visitor callback for WalkStatements: invoked once per statement node in the tree.
  TAstNodeProc = procedure(aStmt: TPasImplElement) of object;

{ Returns every named type declaration in aModule in deterministic declaration order :
  interface section, then implementation section }
function EnumerateTypes(aModule: TPasModule): TPasTypeArray;

{ Returns every section-level constant/variable and every class/record member
  field/constant in aModule in deterministic declaration order:
  - interface section,
  - then implementation section;
  - each member list walked right after its  enclosing type }
function EnumerateValueDecls(aModule: TPasModule): TPasValueDeclArray;

{ Returns every routine with body in aModule in deterministic declaration order:
  interface section, then implementation section. Returns an empty array when aModule is nil. }
function EnumerateRoutines(aModule: TPasModule): TAstRoutineArray;

{ Returns the direct child sub-statements of aStmt:
  the next level of the statement tree.
  Leaf statements and nil yield an empty array.}

function ChildStatements(aStmt: TPasImplElement): TPasImplElementArray;

 { Depth-first walk of every statement node strictly below aRoot:
   aRoot itself — typically a routine's begin-block — is not visited
   invoking aProc per node.}
procedure WalkStatements(aRoot: TPasImplElement; aProc: TAstNodeProc);

{ Returns every top-level executable statement block in aModule:
  each routine with body's statement block;
  then the module Initialization Section and Finalization Section when present }
function EnumerateStatementRoots(aModule: TPasModule): TPasImplElementArray;

type
  // The accessibility scope a declaration's references are sought within:
  TFpSonarUseScope = (usRoutine, // the enclosing routine body
    usUnit, //  the whole unit
    usProject // the whole project (a public/global symbol)
    );

  // The oracle verdict. rrUnknown means "could not decide" -> treat it as used.
  TFpSonarRefResult = (rrUsed, rrUnused, rrUnknown);

  { The project-wide name-reference index  }
  TFpSonarProjectIndex = class
  private
    // Union of every USE-position name across all folded units (case-insensitive).
    FUnitRefs: TStringList;
    // True once any analyzed project unit failed to parse (nil module folded in).
    FAnyUnparseable: boolean;
    // Per-unit entries keyed (sorted, case-insensitive) by unit name;
    //  Objects are owned TUnitEntry holding the interface name set + edge flags.
    FUnits: TStringList;
    // ForEachCall callback over an interface section
    procedure CollectInterface(aEl: TPasElement; aArg: Pointer);
    function FindEntry(const aUnitName: string): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    // Folds one successfully-parsed project unit into the index.
    procedure AddModule(aModule: TPasModule);
    // Records that a project unit failed to parse (its references are unknown),
    // so usProject queries degrade conservatively to rrUnknown.
    procedure MarkUnparseable;
    // Is aName referenced anywhere in the project? rrUnknown when undecidable
    function IsReferencedInProject(const aName: string): TFpSonarRefResult;
    // The interface-declared name set of aUnitName, or nil when that unit is not
    // in the index. Read-only by convention.
    function InterfaceNames(const aUnitName: string): TStringList;
    // True iff aUnitName's interface declares an operator or a helper.
    function UnitHasOperatorOrHelper(const aUnitName: string): boolean;
    // True iff aUnitName carries an initialization/finalization section.
    function UnitHasInitFinal(const aUnitName: string): boolean;
  end;

  { The per-unit name-reference index + the IsReferenced origin }
  TFpSonarUseAnalyzer = class
  private
    FModule: TPasModule;
    // Optional project-wide index for usProject queries; nil => a
    // usProject query degrades to rrUnknown (no project context, abstain).
    FProjectIndex: TFpSonarProjectIndex;
    // Case-insensitive set of every name referenced anywhere in the unit.
    FUnitRefs: TStringList;
    // Per-routine reference sets, lazily built: FRoutines[i] owns FRoutineRefs[i]
    // (parallel lists keyed by the TPasProcedure pointer).
    FRoutines: TFPList;
    FRoutineRefs: TList;
    // ForEachCall callback: records the use-position names of aEl into the TStringList passed through aArg.
    procedure CollectRef(aEl: TPasElement; aArg: Pointer);
    // Walks aRoot's element subtree (ForEachCall) into a fresh case-insensitive
    // name set the caller owns.
    function CollectInto(aRoot: TPasElement): TStringList;
    // The reference set for aProc's body, built (and cached) on first request.
    function RoutineRefs(aProc: TPasProcedure): TStringList;
    // The nearest enclosing routine of aDecl via the Parent chain, or nil.
    function EnclosingRoutine(aDecl: TPasElement): TPasProcedure;
  public
    // Builds the unit-wide reference index from aModule (nil tolerated).
    constructor Create(aModule: TPasModule);
    destructor Destroy; override;
    //  is aDecl referenced within aScope?
    function IsReferenced(aDecl: TPasElement;
      aScope: TFpSonarUseScope): TFpSonarRefResult; virtual;
    // True iff ANY name in aNames is referenced in this unit.
    function ReferencesAny(aNames: TStringList): boolean;
    // This unit's own USE-position reference set (read-only), so a project index
    // can union it across units.
    function UnitReferences: TStringList;
    // The optional project-wide index consulted for usProject queries.
    property ProjectIndex: TFpSonarProjectIndex read FProjectIndex write FProjectIndex;
  end;

  { The RESOLUTION-BACKED precision oracle: }
  TFpSonarResolvedUseAnalyzer = class(TFpSonarUseAnalyzer)
  private
    // Structural-identity keys  of every declaration the resolved module references
    FReferencedKeys: TStringList;
    // True iff the unit's closure fully resolved
    FUnitComplete: boolean;
    // Case-insensitive set of names referenced through a non-expression channel
    FNonExprRefs: TStringList;
    // ForEachCall callback recording aEl's non-expresssion reference names
    procedure CollectNonExprRef(aEl: TPasElement; aArg: Pointer);
    // Builds FReferencedKeys + FUnitComplete from the resolved module via the tolerant resolver wrapper
    procedure BuildResolvedIndex(aResolver: TFpSonarResolver);
  public
    // Builds the inherited name index, then the resolved reference index from aResolver
    constructor Create(aModule: TPasModule; aResolver: TFpSonarResolver);
    destructor Destroy; override;
    // Override .
    function IsReferenced(aDecl: TPasElement;
      aScope: TFpSonarUseScope): TFpSonarRefResult; override;
  end;

function MakeUseAnalyzer(aModule: TPasModule; aResolver: TFpSonarResolver;
  aPreferResolution: boolean): TFpSonarUseAnalyzer;

implementation

{ Walks aDecls appending every named type declaration in order,
  then recursing into each class/record's members for its nested type declarations }
procedure CollectTypesFromDeclarations(var aList: TPasTypeArray;
  aDecls: TFPList);
var
  i: integer;
  lType: TPasType;
begin
  if aDecls = nil then
    Exit;
  for i := 0 to aDecls.Count - 1 do
    if TObject(aDecls[i]) is TPasType then
    begin
      lType := TPasType(aDecls[i]);
      if (lType.Name <> '')
        and not ((lType is TPasClassType) and TPasClassType(lType).IsForward) then
      begin
        SetLength(aList, Length(aList) + 1);
        aList[High(aList)] := lType;
      end;
      if lType is TPasMembersType then
        CollectTypesFromDeclarations(aList, TPasMembersType(lType).Members);
    end;
end;


function EnumerateTypes(aModule: TPasModule): TPasTypeArray;
begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  if aModule.InterfaceSection <> nil then
    CollectTypesFromDeclarations(Result, aModule.InterfaceSection.Declarations);
  if aModule.ImplementationSection <> nil then
    CollectTypesFromDeclarations(Result,
      aModule.ImplementationSection.Declarations);
end;


{ Walks aDecls appending every value declaration in order,
  then recursing into each class/record's Members for its member fields/consts. }
procedure CollectValuesFromDeclarations(var aList: TPasValueDeclArray;
  aDecls: TFPList; aInMembers: boolean);
var
  i: integer;
  lObj: TObject;
  lKind: TPasValueKind;
  lMatched: boolean;
begin
  if aDecls = nil then
    Exit;
  for i := 0 to aDecls.Count - 1 do
  begin
    lObj := TObject(aDecls[i]);
    lMatched := False;
    if lObj is TPasProperty then
    // A property is named by no rule; skip it (and never let it
    // be miscounted as a field — it descends TPasVariable too).
    else if lObj is TPasConst then
    begin
      lKind := vkConst;
      lMatched := True;
    end
    else if lObj is TPasVariable then
    begin
      if aInMembers then
        lKind := vkField
      else
        lKind := vkVar;
      lMatched := True;
    end;
    if lMatched and (TPasVariable(lObj).Name <> '') then
    begin
      SetLength(aList, Length(aList) + 1);
      aList[High(aList)].Decl := TPasVariable(lObj);
      aList[High(aList)].Kind := lKind;
    end;
    // Recurse into class/record members for their fields/consts/nested
    // members; never into a routine body (locals deferred).
    if lObj is TPasMembersType then
      CollectValuesFromDeclarations(aList, TPasMembersType(lObj).Members, True);
  end;
end;


function EnumerateValueDecls(aModule: TPasModule): TPasValueDeclArray;
begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  if aModule.InterfaceSection <> nil then
    CollectValuesFromDeclarations(Result,
      aModule.InterfaceSection.Declarations, False);
  if aModule.ImplementationSection <> nil then
    CollectValuesFromDeclarations(Result,
      aModule.ImplementationSection.Declarations, False);
end;


{ Appends aRoutine's declaration + statement block to aList when it has a body. }
procedure CollectRoutine(var aList: TAstRoutineArray; aProc: TPasProcedure);
  forward;

{ Walks aDecls appending every TPasProcedure with a body in order, recursively }
procedure CollectFromDeclarations(var aList: TAstRoutineArray;
  aDecls: TFPList);
var
  i: integer;
begin
  if aDecls = nil then
    Exit;
  for i := 0 to aDecls.Count - 1 do
    if TObject(aDecls[i]) is TPasProcedure then
      CollectRoutine(aList, TPasProcedure(aDecls[i]));
end;


procedure CollectRoutine(var aList: TAstRoutineArray; aProc: TPasProcedure);
begin
  { A routine contributes only when it has an executable block }
  if aProc.HasNoImplementation or (aProc.Body = nil)
    or (aProc.Body.Body = nil) then
    Exit;
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)].Decl := aProc;
  aList[High(aList)].Block := aProc.Body.Body;
  // Nested routines live in the procedure body's declaration list.
  CollectFromDeclarations(aList, aProc.Body.Declarations);
end;


function EnumerateRoutines(aModule: TPasModule): TAstRoutineArray;
begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  if aModule.InterfaceSection <> nil then
    CollectFromDeclarations(Result, aModule.InterfaceSection.Declarations);
  if aModule.ImplementationSection <> nil then
    CollectFromDeclarations(Result, aModule.ImplementationSection.Declarations);
end;


// Appends aEl to aArr when non-nil.
procedure AddElem(var aArr: TPasImplElementArray; aEl: TPasImplElement);
begin
  if aEl = nil then
    Exit;
  SetLength(aArr, Length(aArr) + 1);
  aArr[High(aArr)] := aEl;
end;


// Appends every TPasImplElement in aFPList to aArr in list order.
procedure AddList(var aArr: TPasImplElementArray; aFPList: TFPList);
var
  i: integer;
begin
  if aFPList = nil then
    Exit;
  for i := 0 to aFPList.Count - 1 do
    if TObject(aFPList[i]) is TPasImplElement then
      AddElem(aArr, TPasImplElement(aFPList[i]));
end;


function ChildStatements(aStmt: TPasImplElement): TPasImplElementArray;
begin
  SetLength(Result, 0);
  if aStmt = nil then
    Exit;
  // Order matters: branches/bodies are emitted in source-evaluation order so
  // any depth/sequence-sensitive rule reads them deterministically.
  if aStmt is TPasImplIfElse then
  begin
    AddElem(Result, TPasImplIfElse(aStmt).IfBranch);
    AddElem(Result, TPasImplIfElse(aStmt).ElseBranch);
  end
  else if aStmt is TPasImplWhileDo then
    AddElem(Result, TPasImplWhileDo(aStmt).Body)
  else if aStmt is TPasImplForLoop then
    AddElem(Result, TPasImplForLoop(aStmt).Body)
  else if aStmt is TPasImplWithDo then
    AddElem(Result, TPasImplWithDo(aStmt).Body)
  else if aStmt is TPasImplCaseStatement then
    AddElem(Result, TPasImplCaseStatement(aStmt).Body)
  else if aStmt is TPasImplExceptOn then
    AddElem(Result, TPasImplExceptOn(aStmt).Body)
  else if aStmt is TPasImplTry then
  begin
    // The try body sits in Elements (inherited); the finally/except handler
    // and the optional except-else are separate fields, not in Elements.
    AddList(Result, TPasImplBlock(aStmt).Elements);
    AddElem(Result, TPasImplTry(aStmt).FinallyExcept);
    AddElem(Result, TPasImplTry(aStmt).ElseBranch);
  end
  { All the impl statements }
  else if aStmt is TPasImplBlock then
    AddList(Result, TPasImplBlock(aStmt).Elements);
end;


procedure WalkStatements(aRoot: TPasImplElement; aProc: TAstNodeProc);
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  lChildren := ChildStatements(aRoot);
  for i := 0 to High(lChildren) do
  begin
    aProc(lChildren[i]);
    WalkStatements(lChildren[i], aProc);
  end;
end;


function EnumerateStatementRoots(aModule: TPasModule): TPasImplElementArray;
var
  lRoutines: TAstRoutineArray;
  i: integer;

  procedure AddRoot(aBlock: TPasImplElement);
  begin
    if aBlock = nil then
      Exit;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := aBlock;
  end;

begin
  SetLength(Result, 0);
  if aModule = nil then
    Exit;
  lRoutines := EnumerateRoutines(aModule);
  for i := 0 to High(lRoutines) do
    AddRoot(lRoutines[i].Block);
  AddRoot(aModule.InitializationSection);
  AddRoot(aModule.FinalizationSection);
end;

// Builds a fresh case-insensitive, duplicate-ignoring name set.
function NewNameSet: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;


// Adds aName to aSet unless empty (the set ignores case + duplicates itself).
procedure AddName(aSet: TStringList; const aName: string);
begin
  if aName <> '' then
    aSet.Add(aName);
end;


// Harvests every maximal identifier substring of aText into aSet.
procedure AddIdentifiers(aSet: TStringList; const aText: string);
var
  i, lStart: integer;
begin
  i := 1;
  while i <= Length(aText) do
    if aText[i] in ['A'..'Z', 'a'..'z', '_'] then
    begin
      lStart := i;
      while (i <= Length(aText))
        and (aText[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        Inc(i);
      AddName(aSet, Copy(aText, lStart, i - lStart));
    end
    else
      Inc(i);
end;


constructor TFpSonarUseAnalyzer.Create(aModule: TPasModule);
begin
  inherited Create;
  FModule := aModule;
  FProjectIndex := nil;
  FRoutines := TFPList.Create;
  FRoutineRefs := TList.Create;
  FUnitRefs := CollectInto(FModule);
end;


destructor TFpSonarUseAnalyzer.Destroy;
var
  i: integer;
begin
  FUnitRefs.Free;
  for i := 0 to FRoutineRefs.Count - 1 do
    TStringList(FRoutineRefs[i]).Free;
  FRoutineRefs.Free;
  FRoutines.Free;
  inherited Destroy;
end;


procedure TFpSonarUseAnalyzer.CollectRef(aEl: TPasElement; aArg: Pointer);
var
  lSet: TStringList;
  lProp: TPasProperty;
  i: integer;

// Records the name of a referenced type node.
  procedure RecordType(aType: TPasType);
  begin
    if (aType <> nil) and (aType.Name <> '') then
      AddName(lSet, aType.Name);
  end;

begin
  if aEl = nil then
    Exit;
  lSet := TStringList(aArg);
  // An identifier EXPRESSION: every variable/const/property/routine reference,
  if (aEl is TPrimitiveExpr) and (TPrimitiveExpr(aEl).Kind = pekIdent) then
    AddName(lSet, TPrimitiveExpr(aEl).Value)
  // A TYPE reference surfaced as an unresolved symbol ref (some shapes do).
  else if aEl is TPasUnresolvedSymbolRef then
    AddName(lSet, aEl.Name);

  // An 'asm ... end' block: its body is a raw token stream
  if aEl is TPasImplAsmStatement then
  begin
    for i := 0 to TPasImplAsmStatement(aEl).Tokens.Count - 1 do
      AddIdentifiers(lSet, TPasImplAsmStatement(aEl).Tokens[i]);
    for i := 0 to TPasImplAsmStatement(aEl).ModifierTokens.Count - 1 do
      AddIdentifiers(lSet, TPasImplAsmStatement(aEl).ModifierTokens[i]);
  end;

  // Property accessor NAMES are plain strings in an unresolved parse
  if aEl is TPasProperty then
  begin
    lProp := TPasProperty(aEl);
    AddName(lSet, lProp.ReadAccessorName);
    AddName(lSet, lProp.WriteAccessorName);
    AddName(lSet, lProp.StoredAccessorName);
    AddName(lSet, lProp.ImplementsName);
  end;

  // TYPE-reference carriers: a named type used in a declaration position is a use of that type.
  if aEl is TPasVariable then
    RecordType(TPasVariable(aEl).VarType)
  else if aEl is TPasArgument then
    RecordType(TPasArgument(aEl).ArgType)
  else if aEl is TPasResultElement then
    RecordType(TPasResultElement(aEl).ResultType)
  else if aEl is TPasArrayType then
    RecordType(TPasArrayType(aEl).ElType)
  else if aEl is TPasPointerType then
    RecordType(TPasPointerType(aEl).DestType)
  else if aEl is TPasSetType then
    RecordType(TPasSetType(aEl).EnumType)
  // TPasClassOfType descends TPasAliasType, so its DestType is covered here too.
  else if aEl is TPasAliasType then
    RecordType(TPasAliasType(aEl).DestType)
  else if aEl is TPasClassType then
  begin
    RecordType(TPasClassType(aEl).AncestorType);
    if TPasClassType(aEl).Interfaces <> nil then
      for i := 0 to TPasClassType(aEl).Interfaces.Count - 1 do
        if TObject(TPasClassType(aEl).Interfaces[i]) is TPasType then
          RecordType(TPasType(TPasClassType(aEl).Interfaces[i]));
  end;
end;


function TFpSonarUseAnalyzer.CollectInto(aRoot: TPasElement): TStringList;
begin
  Result := NewNameSet;
  if aRoot = nil then
    Exit;
  // PasTree's own complete recursive element visitor
  aRoot.ForEachCall(@CollectRef, Pointer(Result));
end;


function TFpSonarUseAnalyzer.RoutineRefs(aProc: TPasProcedure): TStringList;
var
  i: integer;
begin
  for i := 0 to FRoutines.Count - 1 do
    if FRoutines[i] = Pointer(aProc) then
    begin
      Result := TStringList(FRoutineRefs[i]);
      Exit;
    end;
  // Collect over the complete TPasProcedure.
  Result := CollectInto(aProc);
  FRoutines.Add(Pointer(aProc));
  FRoutineRefs.Add(Result);
end;


function TFpSonarUseAnalyzer.EnclosingRoutine(aDecl: TPasElement): TPasProcedure;
var
  lEl: TPasElement;
begin
  Result := nil;
  if aDecl = nil then
    Exit;
  lEl := aDecl.Parent;
  while lEl <> nil do
  begin
    if lEl is TPasProcedure then
    begin
      Result := TPasProcedure(lEl);
      Exit;
    end;
    lEl := lEl.Parent;
  end;
end;


function TFpSonarUseAnalyzer.IsReferenced(aDecl: TPasElement;
  aScope: TFpSonarUseScope): TFpSonarRefResult;
var
  lRefs: TStringList;
  lProc: TPasProcedure;
begin
  // Undecidable inputs degrade to rrUnknown (the caller treats it as used).
  if (FModule = nil) or (aDecl = nil) or (aDecl.Name = '') then
    Exit(rrUnknown);

  // Project scope: delegate to the attached project index
  if aScope = usProject then
  begin
    if FProjectIndex = nil then
      Exit(rrUnknown);
    Exit(FProjectIndex.IsReferencedInProject(aDecl.Name));
  end;

  if aScope = usRoutine then
  begin
    lProc := EnclosingRoutine(aDecl);
    if lProc = nil then
      Exit(rrUnknown);
    lRefs := RoutineRefs(lProc);
  end
  else
    lRefs := FUnitRefs;

  if lRefs.IndexOf(aDecl.Name) >= 0 then
    Result := rrUsed
  else
    Result := rrUnused;
end;


function TFpSonarUseAnalyzer.ReferencesAny(aNames: TStringList): boolean;
var
  i: integer;
begin
  Result := False;
  if aNames = nil then
    Exit;
  for i := 0 to aNames.Count - 1 do
    if FUnitRefs.IndexOf(aNames[i]) >= 0 then
      Exit(True);
end;


function TFpSonarUseAnalyzer.UnitReferences: TStringList;
begin
  Result := FUnitRefs;
end;


{ TFpSonarProjectIndex }

// One project unit's exported surface: the interface-declared name set.
type
  TUnitEntry = class
    InterfaceNames: TStringList;
    HasOperatorOrHelper: boolean;
    HasInitFinal: boolean;
    destructor Destroy; override;
  end;


destructor TUnitEntry.Destroy;
begin
  InterfaceNames.Free;
  inherited Destroy;
end;


constructor TFpSonarProjectIndex.Create;
begin
  inherited Create;
  FUnitRefs := NewNameSet;
  FAnyUnparseable := False;
  FUnits := TStringList.Create;
  FUnits.CaseSensitive := False;
  FUnits.Sorted := True;
  FUnits.Duplicates := dupIgnore;
end;


destructor TFpSonarProjectIndex.Destroy;
var
  i: integer;
begin
  for i := 0 to FUnits.Count - 1 do
    FUnits.Objects[i].Free;
  FUnits.Free;
  FUnitRefs.Free;
  inherited Destroy;
end;


procedure TFpSonarProjectIndex.CollectInterface(aEl: TPasElement;
  aArg: Pointer);
var
  lEntry: TUnitEntry;
begin
  if aEl = nil then
    Exit;
  lEntry := TUnitEntry(aArg);
  // Every NAMED element in the interface section is part of the exported surface
  // (a SUPERSET — safe: an extra name only keeps an import "used", never flags).
  if aEl.Name <> '' then
    lEntry.InterfaceNames.Add(aEl.Name);
  // An operator or a helper contributes via symbol/method dispatch, not a plain
  // name an importer would spell out — flag it for the conservative skip.
  if aEl is TPasOperator then
    lEntry.HasOperatorOrHelper := True
  else if (aEl is TPasClassType)
    and (TPasClassType(aEl).ObjKind in okAllHelpers) then
    lEntry.HasOperatorOrHelper := True;
end;


procedure TFpSonarProjectIndex.AddModule(aModule: TPasModule);
var
  lAnalyzer: TFpSonarUseAnalyzer;
  lEntry: TUnitEntry;
  i: integer;
begin
  if aModule = nil then
  begin
    MarkUnparseable;
    Exit;
  end;

  // Union this unit's use-position references into the project-wide set.
  lAnalyzer := TFpSonarUseAnalyzer.Create(aModule);
  try
    for i := 0 to lAnalyzer.UnitReferences.Count - 1 do
      FUnitRefs.Add(lAnalyzer.UnitReferences[i]);
  finally
    lAnalyzer.Free;
  end;

  // Record this unit's exported interface surface + edge flags.
  lEntry := TUnitEntry.Create;
  lEntry.InterfaceNames := NewNameSet;
  lEntry.HasOperatorOrHelper := False;
  // A program / library has no interface section — its exported surface is empty.
  if aModule.InterfaceSection <> nil then
    aModule.InterfaceSection.ForEachCall(@CollectInterface, Pointer(lEntry));
  // An initialization / finalization section is a side effect an importer may
  // depend on without naming any of the unit's symbols.
  lEntry.HasInitFinal := (aModule.InitializationSection <> nil)
    or (aModule.FinalizationSection <> nil);

  // An anonymous module cannot be named in a uses clause.
  if (aModule.Name <> '') and (FindEntry(aModule.Name) = nil) then
    FUnits.AddObject(aModule.Name, lEntry)
  else
    lEntry.Free;
end;


procedure TFpSonarProjectIndex.MarkUnparseable;
begin
  FAnyUnparseable := True;
end;


function TFpSonarProjectIndex.FindEntry(const aUnitName: string): TObject;
var
  lIdx: integer;
begin
  Result := nil;
  if (aUnitName <> '') and FUnits.Find(aUnitName, lIdx) then
    Result := FUnits.Objects[lIdx];
end;


function TFpSonarProjectIndex.IsReferencedInProject(
  const aName: string): TFpSonarRefResult;
begin
  if aName = '' then
    Exit(rrUnknown);
  // Conservative suppression: an unparseable unit could reference anything.
  if FAnyUnparseable then
    Exit(rrUnknown);
  { A single-unit index is not a whole project:
    a public/exported symbol exists to serve other units that have not been analyzed here,
    so its non-reference cannot be concluded as "unused" }
  if FUnits.Count < 2 then
    Exit(rrUnknown);
  if FUnitRefs.IndexOf(aName) >= 0 then
    Result := rrUsed
  else
    Result := rrUnused;
end;


function TFpSonarProjectIndex.InterfaceNames(
  const aUnitName: string): TStringList;
var
  lEntry: TObject;
begin
  Result := nil;
  lEntry := FindEntry(aUnitName);
  if lEntry <> nil then
    Result := TUnitEntry(lEntry).InterfaceNames;
end;


function TFpSonarProjectIndex.UnitHasOperatorOrHelper(
  const aUnitName: string): boolean;
var
  lEntry: TObject;
begin
  Result := False;
  lEntry := FindEntry(aUnitName);
  if lEntry <> nil then
    Result := TUnitEntry(lEntry).HasOperatorOrHelper;
end;


function TFpSonarProjectIndex.UnitHasInitFinal(
  const aUnitName: string): boolean;
var
  lEntry: TObject;
begin
  Result := False;
  lEntry := FindEntry(aUnitName);
  if lEntry <> nil then
    Result := TUnitEntry(lEntry).HasInitFinal;
end;


{ TFpSonarResolvedUseAnalyzer }

// The structural identity of a declaration: name + source row + parent name, case-folded.
function DeclKey(aDecl: TPasElement; aRow: integer): string;
var
  lParent: string;
begin
  if (aDecl = nil) or (aDecl.Name = '') then
    Exit('');
  lParent := '';
  if aDecl.Parent <> nil then
    lParent := aDecl.Parent.Name;
  Result := LowerCase(aDecl.Name) + '#' + IntToStr(aRow) + '#' + LowerCase(lParent);
end;


constructor TFpSonarResolvedUseAnalyzer.Create(aModule: TPasModule;
  aResolver: TFpSonarResolver);
begin
  inherited Create(aModule);
  FReferencedKeys := NewNameSet;
  FNonExprRefs := NewNameSet;
  FUnitComplete := False;
  // The non-expression reference channels resolution cannot see
  if FModule <> nil then
    FModule.ForEachCall(@CollectNonExprRef, Pointer(FNonExprRefs));
  BuildResolvedIndex(aResolver);
end;


destructor TFpSonarResolvedUseAnalyzer.Destroy;
begin
  FNonExprRefs.Free;
  FReferencedKeys.Free;
  inherited Destroy;
end;


// Adds a referenced type's name to aSet (empty/nil ignored).
procedure RecordTypeName(aSet: TStringList; aType: TPasType);
begin
  if (aType <> nil) and (aType.Name <> '') then
    AddName(aSet, aType.Name);
end;


{ The element type a single-type carrier references (var/field/const/property
 element, argument, result, array element, pointer dest, set enum, alias dest),
 or nil for a non-carrier.
 Class carriers are handled separately because they carry several.}
function CarriedType(aEl: TPasElement): TPasType;
begin
  if aEl is TPasVariable then
    Result := TPasVariable(aEl).VarType
  else if aEl is TPasArgument then
    Result := TPasArgument(aEl).ArgType
  else if aEl is TPasResultElement then
    Result := TPasResultElement(aEl).ResultType
  else if aEl is TPasArrayType then
    Result := TPasArrayType(aEl).ElType
  else if aEl is TPasPointerType then
    Result := TPasPointerType(aEl).DestType
  else if aEl is TPasSetType then
    Result := TPasSetType(aEl).EnumType
  else if aEl is TPasAliasType then
    Result := TPasAliasType(aEl).DestType
  else
    Result := nil;
end;


// Records a class type's type-position references: its ancestor + every declared
// implemented interface.
procedure RecordClassTypeRefs(aSet: TStringList; aClass: TPasClassType);
var
  i: integer;
begin
  RecordTypeName(aSet, aClass.AncestorType);
  if aClass.Interfaces <> nil then
    for i := 0 to aClass.Interfaces.Count - 1 do
      if TObject(aClass.Interfaces[i]) is TPasType then
        RecordTypeName(aSet, TPasType(aClass.Interfaces[i]));
end;


// Records a property's accessor NAMES (read/write/stored/implements)
procedure RecordAccessorNames(aSet: TStringList; aProp: TPasProperty);
begin
  AddName(aSet, aProp.ReadAccessorName);
  AddName(aSet, aProp.WriteAccessorName);
  AddName(aSet, aProp.StoredAccessorName);
  AddName(aSet, aProp.ImplementsName);
end;


// Records every identifier in an 'asm ... end' block's raw token stream
procedure RecordAsmIdents(aSet: TStringList; aAsm: TPasImplAsmStatement);
var
  i: integer;
begin
  for i := 0 to aAsm.Tokens.Count - 1 do
    AddIdentifiers(aSet, aAsm.Tokens[i]);
  for i := 0 to aAsm.ModifierTokens.Count - 1 do
    AddIdentifiers(aSet, aAsm.ModifierTokens[i]);
end;


procedure TFpSonarResolvedUseAnalyzer.CollectNonExprRef(aEl: TPasElement;
  aArg: Pointer);
var
  lSet: TStringList;
begin
  if aEl = nil then
    Exit;
  lSet := TStringList(aArg);
  // Type positions, property accessor specs and asm identifiers
  RecordTypeName(lSet, CarriedType(aEl));
  if aEl is TPasUnresolvedSymbolRef then
    AddName(lSet, aEl.Name);
  if aEl is TPasClassType then
    RecordClassTypeRefs(lSet, TPasClassType(aEl));
  if aEl is TPasProperty then
    RecordAccessorNames(lSet, TPasProperty(aEl));
  if aEl is TPasImplAsmStatement then
    RecordAsmIdents(lSet, TPasImplAsmStatement(aEl));
end;


procedure TFpSonarResolvedUseAnalyzer.BuildResolvedIndex(
  aResolver: TFpSonarResolver);
var
  lNodes, lDecls, lIdNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  lResolvedNodes: TStringList;
  i: integer;
begin
  if aResolver = nil then
    Exit;
  // The resolved reference set, keyed by structural identity
  if not aResolver.TryReferenceSites(lNodes, lDecls) then
    Exit;
  for i := 0 to High(lDecls) do
    FReferencedKeys.Add(DeclKey(lDecls[i], aResolver.SourceRow(lDecls[i])));

  // Per-unit completeness: the unit's closure is trustworthy only if every
  // identifier-reference leaf is bound to a declaration
  lResolvedNodes := TStringList.Create;
  try
    lResolvedNodes.Sorted := True;
    lResolvedNodes.Duplicates := dupIgnore;
    for i := 0 to High(lNodes) do
      lResolvedNodes.Add(IntToHex(PtrUInt(lNodes[i]), 16));
    if aResolver.TryIdentifierNameSites(lIdNodes, lNames) then
    begin
      FUnitComplete := True;
      for i := 0 to High(lIdNodes) do
        if lResolvedNodes.IndexOf(IntToHex(PtrUInt(lIdNodes[i]), 16)) < 0 then
        begin
          FUnitComplete := False;
          Break;
        end;
    end;
  finally
    lResolvedNodes.Free;
  end;
end;


function TFpSonarResolvedUseAnalyzer.IsReferenced(aDecl: TPasElement;
  aScope: TFpSonarUseScope): TFpSonarRefResult;
var
  lName: TFpSonarRefResult;
begin
  lName := inherited IsReferenced(aDecl, aScope);
  // Monotonic: a name-engine rrUnused/rrUnknown is returned unchanged
  if lName <> rrUsed then
    Exit(lName);
  // usProject delegates to the name-based project index unchanged
  if aScope = usProject then
    Exit(rrUsed);
  // Per-unit degrade: trust the resolved set only where the whole unit resolved; undecidable inputs also stay used.
  if (not FUnitComplete) or (aDecl = nil) or (aDecl.Name = '') then
    Exit(rrUsed);
  { if the candidate's name is referenced through a non-expression path,
    the resolved expression index is structurally blind to it ⇒ degrade.
    Without this, a private getter named only by `read GetCount` would be flagged unused. }
  if FNonExprRefs.IndexOf(aDecl.Name) >= 0 then
    Exit(rrUsed);
  { The incremental finding: the candidate's identity is absent from the resolved
    reference set ⇒ resolution proves it unreferenced }
  if FReferencedKeys.IndexOf(DeclKey(aDecl, aDecl.SourceLinenumber)) < 0 then
    Result := rrUnused
  else
    Result := rrUsed;
end;


function MakeUseAnalyzer(aModule: TPasModule; aResolver: TFpSonarResolver;
  aPreferResolution: boolean): TFpSonarUseAnalyzer;
begin
  if aPreferResolution and (aResolver <> nil) and aResolver.Succeeded then
    Result := TFpSonarResolvedUseAnalyzer.Create(aModule, aResolver)
  else
    Result := TFpSonarUseAnalyzer.Create(aModule);
end;

end.

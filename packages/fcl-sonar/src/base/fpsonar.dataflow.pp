{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Wrapper over the fcl-passrc data-flow analyser and control-flow graph

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.DataFlow;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree, Pascal.DataFlow, Pascal.CFG,
{$ELSE}
  Classes, SysUtils, PasTree, pasdataflow, pascfg,
{$ENDIF}
  FpSonar.Types, FpSonar.Resolver;

type
  // The resource defect a site was found to carry.
  TFpSonarResourceVerdict = (rvUnprotectedRelease, rvEarlyExitLeak,
    rvUnprotectedStream);

  { One resource verdict: the statement to report at and the name of the
    routine local whose instance it concerns. }
  TFpSonarResourceFinding = record
    Site: TPasElement;
    Name: string;
    Verdict: TFpSonarResourceVerdict;
  end;
  TFpSonarResourceFindingArray = array of TFpSonarResourceFinding;

  // The forward-dataflow defect a site was found to carry.
  TFpSonarFlowVerdict = (fvDeadStore, fvUninitializedStrict,
    fvSelfAssignedNeverUsed, fvResultOverwritten, fvResultUnassigned);

  { One forward-dataflow verdict: the node to report at and the name of the
    declaration it concerns. }
  TFpSonarFlowFinding = record
    Site: TPasElement;
    Name: string;
    Verdict: TFpSonarFlowVerdict;
  end;
  TFpSonarFlowFindingArray = array of TFpSonarFlowFinding;

  // The free-state defect a site was found to carry.
  TFpSonarFreeStateVerdict = (fsvUseAfterFree, fsvDoubleFree,
    fsvFieldFreedNotNilled, fsvUnpairedAllocation, fsvLoopAllocationNotFreed);

  { One free-state verdict: the statement to report at and the name of the
    declaration it concerns. }
  TFpSonarFreeStateFinding = record
    Site: TPasElement;
    Name: string;
    Verdict: TFpSonarFreeStateVerdict;
  end;
  TFpSonarFreeStateFindingArray = array of TFpSonarFreeStateFinding;

  // One acquire/release vocabulary entry the pairing query matches.
  TFpSonarPairSpec = record
    Acquire: string;
    Release: string;
  end;
  TFpSonarPairSpecArray = array of TFpSonarPairSpec;

  { One unprotected pairing: the acquire statement to report at, the name of
    the receiver it acquires and the release method that pairs with it. }
  TFpSonarPairFinding = record
    Site: TPasElement;
    Name: string;
    Release: string;
  end;
  TFpSonarPairFindingArray = array of TFpSonarPairFinding;

  { One indexed read of storage a SetLength left unwritten: the statement to
    report at and the name of the declaration it reads. }
  TFpSonarSetLengthFinding = record
    Site: TPasElement;
    Name: string;
  end;
  TFpSonarSetLengthFindingArray = array of TFpSonarSetLengthFinding;

  { One I/O call whose result nothing reads: the statement to report at, the
    name of the routine it calls and the next I/O call that follows it. }
  TFpSonarIOCheckFinding = record
    Site: TPasElement;
    Name: string;
    Next: TPasElement;
  end;
  TFpSonarIOCheckFindingArray = array of TFpSonarIOCheckFinding;

  // The concurrency defect a site was found to carry.
  TFpSonarConcurrencyVerdict = (cvGlobalWrite, cvSyncWithLock,
    cvSectionNotInitialized);

  { One concurrency verdict: the statement to report at, the name of the
    declaration it concerns and, for a main-thread call, the routine called. }
  TFpSonarConcurrencyFinding = record
    Site: TPasElement;
    Name: string;
    Callee: string;
    Verdict: TFpSonarConcurrencyVerdict;
  end;
  TFpSonarConcurrencyFindingArray = array of TFpSonarConcurrencyFinding;

  { The tolerant data-flow wrapper: owns one analyser over a resolver's
    resolved module and exposes the minimal query API; the reachability query
    is a class function over a module and needs no resolver. Valid only while
    that build stands — a further BuildFor frees the tree its answers point
    into. }
  TFpSonarDataFlow = class
  private
    FResolver: TFpSonarResolver;
    FAnalyzer: TPasDataFlowAnalyzer;
    FAnalyzed: boolean;
    FUsable: boolean;
    { Runs the analysis at most once and caches its verdict: True iff the
      module analysed without raising. }
    function EnsureAnalyzed: boolean;
  public
    // Binds the wrapper to aResolver; the analysis runs on the first query.
    constructor Create(aResolver: TFpSonarResolver);
    // Frees the analyser.
    destructor Destroy; override;
    { Tolerant uninitialized-use query: True iff the analysis is usable, the
      two parallel arrays then holding each use site whose variable has no
      textually earlier definition, and that variable's name. }
    function TryUninitializedUses(out aNodes: TPasElementArray;
      out aNames: TFpSonarStringArray): boolean;
    { Tolerant reachability query: True iff every statement root of aModule
      graphed, aNodes then holding each statement no control path reaches,
      containers whose body is partly live excluded. }
    class function TryUnreachableStatements(aModule: TPasModule;
      out aNodes: TPasElementArray): boolean;
    { Tolerant resource-protection query: True iff every routine of the
      resolved module graphed, aFindings then holding one verdict per
      offending site, in EnumerateRoutines order and, within one routine, in
      statement position. }
    function TryResourceFindings(
      out aFindings: TFpSonarResourceFindingArray): boolean;
    { Tolerant forward-dataflow query: True iff every routine of the resolved
      module graphed, aFindings then holding one verdict per offending site, in
      EnumerateRoutines order and, within one routine, in statement position. }
    function TryFlowFindings(out aFindings: TFpSonarFlowFindingArray): boolean;
    { Tolerant free-state query: True iff every routine of the resolved module
      graphed, aFindings then holding one verdict per offending site, in
      EnumerateRoutines order and, within one routine, in statement position. }
    function TryFreeStateFindings(
      out aFindings: TFpSonarFreeStateFindingArray): boolean;
    { Tolerant acquire/release pairing query: True iff every routine of the
      resolved module was read, aFindings then holding one entry per acquire of
      aPairs whose release sits outside a covering finally, in
      EnumerateRoutines order and, within one routine, in statement position. }
    function TryPairFindings(const aPairs: TFpSonarPairSpecArray;
      out aFindings: TFpSonarPairFindingArray): boolean;
    { Tolerant SetLength-fill query: True iff every routine of the resolved
      module was read, aFindings then holding one entry per declaration read
      through an index after a SetLength nothing wrote to since, in
      EnumerateRoutines order and, within one routine, in statement position. }
    function TrySetLengthFindings(
      out aFindings: TFpSonarSetLengthFindingArray): boolean;
    { Tolerant IOResult query: True iff every routine of the resolved module was
      read, aFindings then holding one entry per I/O call no statement before
      the next I/O call reads the result of, in EnumerateRoutines order and,
      within one routine, in statement position. }
    function TryIOCheckFindings(
      out aFindings: TFpSonarIOCheckFindingArray): boolean;
    { Tolerant concurrency query: True iff every routine of the resolved module
      graphed, aFindings then holding one verdict per offending site, in
      EnumerateRoutines order and, within one routine, in statement position. }
    function TryConcurrencyFindings(
      out aFindings: TFpSonarConcurrencyFindingArray): boolean;
  end;

implementation

uses
  FpSonar.Traversal;

const
  // Parent-walk bound, as elsewhere in the tree.
  cMaxParentDepth = 200;

  // The classes rvUnprotectedStream matches, by written name.
  cStreamClasses: array[0..2] of string = ('TFileStream', 'TStringStream',
    'TMemoryStream');

  // The routines TryIOCheckFindings reads as file I/O, by written name.
  cIORoutines: array[0..18] of string = ('Append', 'BlockRead', 'BlockWrite',
    'ChDir', 'Close', 'CloseFile', 'Erase', 'Flush', 'MkDir', 'Read', 'ReadLn',
    'Rename', 'Reset', 'Rewrite', 'RmDir', 'Seek', 'Truncate', 'Write',
    'WriteLn');

  // The identifier a routine reads to clear a pending I/O error.
  cIOResultName = 'IOResult';

  // The routine TrySetLengthFindings reads as a resize, by written name.
  cSetLengthName = 'SetLength';

  // The RTL critical-section routines, by written name.
  cInitSectionName = 'InitCriticalSection';
  cEnterSectionName = 'EnterCriticalSection';
  cLeaveSectionName = 'LeaveCriticalSection';
  cDoneSectionName = 'DoneCriticalSection';

  // The routines that run a callback on the main thread, by written name.
  cMainThreadRoutines: array[0..1] of string = ('Synchronize', 'Queue');

  // The critical-section type, the thread class and its body, by written name.
  cSectionTypeName = 'TRTLCriticalSection';
  cThreadClassName = 'TThread';
  cThreadRoutineName = 'Execute';

  // The routine an Exit with an argument writes the result through.
  cExitName = 'Exit';

  // The type kinds a forward-dataflow slot may stand for.
  cFlowTrackedKinds = [ltkInteger, ltkFloat, ltkBool, ltkEnum, ltkChar,
    ltkPointer];

type
  { The analyser with its diagnostic channel closed: fpsonar runs after the
    scanner is gone, so the inherited routing to Resolver.LogMsg would raise. }
  TFpSonarDataFlowAnalyzer = class(TPasDataFlowAnalyzer)
  protected
    procedure EmitMessage(MsgNumber: Integer; const Fmt: String;
      const Args: array of const; PosEl: TPasElement); override;
  end;

  { Ownership of one tracked acquisition on the paths reaching a node. The
    order is the lattice order, so Merge is the maximum and a release on any
    incoming path wins over ownership on another. osNilled is a release that
    left the reference nil, osReleased one that left it dangling. }
  TFpSonarOwnership = (osUnreached, osOwned, osNilled, osReleased);

  { Ownership value per tracked acquisition of one routine. }
  TFpSonarOwnershipState = class(TObject)
  private
    FValues: array of TFpSonarOwnership;
    function GetCount: integer;
    function GetValue(aIndex: integer): TFpSonarOwnership;
    procedure SetValue(aIndex: integer; aValue: TFpSonarOwnership);
  public
    // Creates a state holding aCount unreached acquisitions.
    constructor Create(aCount: integer);
    // How many acquisitions the state holds a value for.
    property Count: integer read GetCount;
    // Ownership of the acquisition at aIndex.
    property Values[aIndex: integer]: TFpSonarOwnership read GetValue
      write SetValue; default;
  end;

  { What counts as a release of a declaration: an empty Method is the
    Free/FreeAndNil vocabulary, any other a `<recv>.<Method>` call. }
  TFpSonarReleaseSpec = record
    Resolver: TFpSonarResolver;
    Method: string;
  end;

  { One acquisition of a routine local: the assignment, the local, the class
    constructed, and the two syntactic facts the verdicts need. }
  TFpSonarAcquisition = record
    Site: TPasImplElement;
    Decl: TPasElement;
    Name: string;
    TypeName: string;
    Released: boolean;
    Covered: boolean;
  end;
  TFpSonarAcquisitionArray = array of TFpSonarAcquisition;

  { Bits one forward-dataflow slot carries. The first two belong to a local
    slot and the last three to a store slot, so a slot's kind is readable from
    its bits. }
  TFpSonarFlowBit = (bfAssigned, bfUnassigned, bfPending, bfOverwritten,
    bfRead);
  TFpSonarFlowBits = set of TFpSonarFlowBit;

  // What a forward-dataflow slot stands for.
  TFpSonarSlotKind = (skLocal, skStore, skResult);

  { One slot of the forward-dataflow state, plus the facts the verdicts read
    off the settled graph. }
  TFpSonarFlowSlot = record
    Kind: TFpSonarSlotKind;
    Decl: TPasElement;
    Name: string;
    Site: TPasImplElement;
    IsResult: boolean;
    SelfDerived: boolean;
    Reachable: boolean;
    WasRead: boolean;
    ExitOverwritten: boolean;
    ExitUnassigned: boolean;
  end;
  TFpSonarFlowSlotArray = array of TFpSonarFlowSlot;

  { Bit set per slot of one routine. }
  TFpSonarFlowState = class(TObject)
  private
    FBits: array of TFpSonarFlowBits;
    function GetCount: integer;
    function GetBits(aIndex: integer): TFpSonarFlowBits;
    procedure SetBits(aIndex: integer; aValue: TFpSonarFlowBits);
  public
    // Creates a state holding aCount empty slots.
    constructor Create(aCount: integer);
    // How many slots the state holds a value for.
    property Count: integer read GetCount;
    // Bits of the slot at aIndex.
    property Bits[aIndex: integer]: TFpSonarFlowBits read GetBits
      write SetBits; default;
  end;

  { Forward per-slot bit lattice over one routine's tracked locals and stores;
    Merge is the bit union and Transfer promotes bits only, so it converges
    without an iteration cap. }
  TFpSonarFlowLattice = class(TPasDataFlowLattice)
  private
    FResolver: TFpSonarResolver;
    FCFG: TPasCFG;
    FSlots: TFpSonarFlowSlotArray;
    FReadSites: TFPList;
    FReadNames: TStringList;
    FReporting: boolean;
    procedure ApplyRead(aDecl: TPasElement; aState: TFpSonarFlowState);
    procedure ApplyWrite(aDecl: TPasElement; aState: TFpSonarFlowState);
    procedure ReportRead(aStmt: TPasImplElement; aNode: TPasElement;
      aDecl: TPasElement; aState: TFpSonarFlowState);
    procedure StepStatement(aStmt: TPasImplElement;
      aState: TFpSonarFlowState);
  public
    // Binds the lattice to the graph and the slots of one routine.
    constructor Create(aResolver: TFpSonarResolver; aCFG: TPasCFG;
      const aSlots: TFpSonarFlowSlotArray);
    // The engine propagates definitions along the edges, so forward.
    function Direction: TPasDataFlowDirection; override;
    // A state in which every slot is empty.
    function CreateState: TObject; override;
    // An independent copy of aState.
    function CopyState(aState: TObject): TObject; override;
    // Releases a state obtained from CreateState or CopyState.
    procedure FreeState(aState: TObject); override;
    // Unions the bits of aSource into those of aTarget, slot by slot.
    procedure Merge(aTarget: TObject; aSource: TObject); override;
    // Applies aNode's reads and writes to aState, reads first, in source order.
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); override;
    // True when both states hold the same bits for every slot.
    function SameState(aLeft: TObject; aRight: TObject): Boolean; override;
    { Replays aNode over aState with reporting on, appending every read of a
      local that is assigned on one incoming path and not on another. }
    procedure Replay(aNode: TPasCFGNode; aState: TObject; aSites: TFPList;
      aNames: TStringList);
  end;

  { Forward ownership lattice over one routine's tracked acquisitions. }
  TFpSonarResourceLattice = class(TPasDataFlowLattice)
  private
    FResolver: TFpSonarResolver;
    FCFG: TPasCFG;
    FAcquisitions: TFpSonarAcquisitionArray;
  public
    // Binds the lattice to the graph and the acquisitions of one routine.
    constructor Create(aResolver: TFpSonarResolver; aCFG: TPasCFG;
      const aAcquisitions: TFpSonarAcquisitionArray);
    // The engine propagates ownership along the edges, so forward.
    function Direction: TPasDataFlowDirection; override;
    // A state in which no acquisition has been reached.
    function CreateState: TObject; override;
    // An independent copy of aState.
    function CopyState(aState: TObject): TObject; override;
    // Releases a state obtained from CreateState or CopyState.
    procedure FreeState(aState: TObject); override;
    // Raises each acquisition of aTarget to the higher of the two values.
    procedure Merge(aTarget: TObject; aSource: TObject); override;
    // Applies aNode's acquisitions and releases to aState, in source order.
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); override;
    // True when both states hold the same ownership for every acquisition.
    function SameState(aLeft: TObject; aRight: TObject): Boolean; override;
  end;

  // What the recognised sites of a tracked declaration operate on.
  TFpSonarSlotSort = (ssObject, ssPointer);

  { What a recognised statement does to the declaration it names. The first
    three are object sites and the next two pointer sites; a nil store fixes no
    operand kind. }
  TFpSonarFreeAction = (faNone, faCreate, faFree, faFreeAndNil, faAllocate,
    faDeallocate, faNilStore);

  { One tracked declaration of a routine, plus the two facts the verdicts read
    off the settled graph. }
  TFpSonarFreeSlot = record
    Decl: TPasElement;
    Name: string;
    Sort: TFpSonarSlotSort;
    IsField: boolean;
    Allocations: integer;
    ReadWhileReleased: boolean;
    ExitOwned: boolean;
  end;
  TFpSonarFreeSlotArray = array of TFpSonarFreeSlot;

  { One verdict the replay proposes, before the settled facts confirm it. }
  TFpSonarFreeCandidate = record
    Site: TPasImplElement;
    Slot: integer;
    Verdict: TFpSonarFreeStateVerdict;
  end;
  TFpSonarFreeCandidateArray = array of TFpSonarFreeCandidate;

  { Forward free-state lattice over one routine's tracked declarations; Merge is
    the pointwise maximum and Transfer sets a slot to a constant, so it
    converges without an iteration cap. }
  TFpSonarFreeStateLattice = class(TPasDataFlowLattice)
  private
    FResolver: TFpSonarResolver;
    FCFG: TPasCFG;
    FSlots: TFpSonarFreeSlotArray;
    FCandidates: TFpSonarFreeCandidateArray;
    FReporting: boolean;
    procedure AddCandidate(aStmt: TPasImplElement; aSlot: integer;
      aVerdict: TFpSonarFreeStateVerdict);
    procedure ReportSite(aStmt: TPasImplElement; aAction: TFpSonarFreeAction;
      aDecl: TPasElement; aState: TFpSonarOwnershipState);
    procedure ReportReads(aStmt: TPasImplElement;
      const aReadDecls: TPasElementArray; aAction: TFpSonarFreeAction;
      aDecl: TPasElement; aState: TFpSonarOwnershipState);
    procedure StepStatement(aStmt: TPasImplElement;
      aState: TFpSonarOwnershipState);
  public
    // Binds the lattice to the graph and the tracked declarations of one routine.
    constructor Create(aResolver: TFpSonarResolver; aCFG: TPasCFG;
      const aSlots: TFpSonarFreeSlotArray);
    // The engine propagates the free state along the edges, so forward.
    function Direction: TPasDataFlowDirection; override;
    // A state in which no declaration has been reached.
    function CreateState: TObject; override;
    // An independent copy of aState.
    function CopyState(aState: TObject): TObject; override;
    // Releases a state obtained from CreateState or CopyState.
    procedure FreeState(aState: TObject); override;
    // Raises each declaration of aTarget to the higher of the two values.
    procedure Merge(aTarget: TObject; aSource: TObject); override;
    // Applies aNode's releases, acquisitions and nil stores to aState.
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); override;
    // True when both states hold the same free state for every declaration.
    function SameState(aLeft: TObject; aRight: TObject): Boolean; override;
    { Replays aNode over its in-state aState with reporting on, recording every
      verdict the state at each of its statements proposes. }
    procedure Replay(aNode: TPasCFGNode; aState: TObject);
    // Records which declarations aState leaves owned, aState being the exit state.
    procedure HarvestExit(aState: TObject);
    // The verdicts the replay proposed, in visit order.
    property Candidates: TFpSonarFreeCandidateArray read FCandidates;
    // The tracked declarations, with the settled facts filled in.
    property Slots: TFpSonarFreeSlotArray read FSlots;
  end;

procedure TFpSonarDataFlowAnalyzer.EmitMessage(MsgNumber: Integer;
  const Fmt: String; const Args: array of const; PosEl: TPasElement);

begin
end;


{ TFpSonarOwnershipState }

constructor TFpSonarOwnershipState.Create(aCount: integer);

begin
  inherited Create;
  SetLength(FValues, aCount);
end;


function TFpSonarOwnershipState.GetCount: integer;

begin
  Result := Length(FValues);
end;


function TFpSonarOwnershipState.GetValue(aIndex: integer): TFpSonarOwnership;

begin
  Result := FValues[aIndex];
end;


procedure TFpSonarOwnershipState.SetValue(aIndex: integer;
  aValue: TFpSonarOwnership);

begin
  FValues[aIndex] := aValue;
end;


// The expression of a simple statement, nil for any other statement kind.
function SimpleExpr(aStmt: TPasImplElement): TPasExpr;

begin
  if aStmt is TPasImplSimple then
    Result := TPasImplSimple(aStmt).Expr
  else
    Result := nil;
end;


// True when aExpr is the identifier aName.
function IsNamedIdent(aExpr: TPasExpr; const aName: string): boolean;

begin
  Result := (aExpr is TPrimitiveExpr)
    and (TPrimitiveExpr(aExpr).Kind = pekIdent)
    and SameText(TPrimitiveExpr(aExpr).Value, aName);
end;


// The declaration a Free/FreeAndNil statement releases, nil for anything else.
function ReleasedDecl(aResolver: TFpSonarResolver;
  aStmt: TPasImplElement): TPasElement;

var
  lInner: TPasExpr;

begin
  Result := nil;
  if aResolver.TryFreeCall(SimpleExpr(aStmt), lInner) = lfkNone then
    Exit;
  Result := aResolver.ReferencedDecl(lInner);
end;


{ The receiver of a `<recv>.<aMethod>` statement call, nil when the statement
  is not that call or its receiver is not a plain identifier. }
function MethodCallReceiver(aStmt: TPasImplElement;
  const aMethod: string): TPasExpr;

var
  lExpr: TPasExpr;
  lBin: TBinaryExpr;

begin
  Result := nil;
  lExpr := SimpleExpr(aStmt);
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
    lExpr := TParamsExpr(lExpr).Value;
  if not (lExpr is TBinaryExpr) then
    Exit;
  lBin := TBinaryExpr(lExpr);
  if (lBin.OpCode <> eopSubIdent) or not IsNamedIdent(lBin.Right, aMethod)
    or not (lBin.Left is TPrimitiveExpr)
    or (TPrimitiveExpr(lBin.Left).Kind <> pekIdent) then
    Exit;
  Result := lBin.Left;
end;


// True when aStmt releases aDecl under aSpec's vocabulary.
function Releases(const aSpec: TFpSonarReleaseSpec; aStmt: TPasImplElement;
  aDecl: TPasElement): boolean;

var
  lRecv: TPasExpr;

begin
  if aSpec.Method = '' then
    Exit(ReleasedDecl(aSpec.Resolver, aStmt) = aDecl);
  lRecv := MethodCallReceiver(aStmt, aSpec.Method);
  Result := (lRecv <> nil) and (aSpec.Resolver.ReferencedDecl(lRecv) = aDecl);
end;


// Appends every statement strictly below aRoot to aList, in source order.
procedure CollectStatements(aRoot: TPasImplElement;
  var aList: TPasImplElementArray);

var
  lChildren: TPasImplElementArray;
  i: integer;

begin
  lChildren := ChildStatements(aRoot);
  for i := 0 to High(lChildren) do
  begin
    SetLength(aList, Length(aList) + 1);
    aList[High(aList)] := lChildren[i];
    CollectStatements(lChildren[i], aList);
  end;
end;


// True when a release of aDecl sits in aRoot or anywhere below it.
function ReleasesBelow(const aSpec: TFpSonarReleaseSpec;
  aRoot: TPasImplElement; aDecl: TPasElement): boolean;

var
  lStmts: TPasImplElementArray;
  i: integer;

begin
  Result := False;
  if aRoot = nil then
    Exit;
  SetLength(lStmts, 1);
  lStmts[0] := aRoot;
  CollectStatements(aRoot, lStmts);
  for i := 0 to High(lStmts) do
    if Releases(aSpec, lStmts[i], aDecl) then
      Exit(True);
end;


// The finally section of aStmt, nil when it is not a try..finally.
function FinallySection(aStmt: TPasElement): TPasImplElement;

begin
  Result := nil;
  if (aStmt is TPasImplTry)
    and (TPasImplTry(aStmt).FinallyExcept is TPasImplTryFinally) then
    Result := TPasImplTry(aStmt).FinallyExcept;
end;


// The position of aStmt in aStmts, -1 when it is absent.
function StmtIndex(const aStmts: TPasImplElementArray;
  aStmt: TPasElement): integer;

var
  i: integer;

begin
  Result := -1;
  for i := 0 to High(aStmts) do
    if aStmts[i] = aStmt then
      Exit(i);
end;


{ True when an acquisition of the same local as aAcqs[aIndex], but not that
  one, sits strictly between the positions aFrom and aTo. }
function ReacquiredBetween(const aStmts: TPasImplElementArray;
  const aAcqs: TFpSonarAcquisitionArray;
  aIndex: integer; aFrom: integer; aTo: integer): boolean;

var
  lAt: integer;
  i: integer;

begin
  Result := False;
  for i := 0 to High(aAcqs) do
  begin
    if (i = aIndex) or (aAcqs[i].Decl <> aAcqs[aIndex].Decl) then
      Continue;
    lAt := StmtIndex(aStmts, aAcqs[i].Site);
    if (lAt > aFrom) and (lAt < aTo) then
      Exit(True);
  end;
end;


{ True when a release of aAcqs[aIndex]'s local follows its site in statement
  order with no later acquisition of that local in between. }
function HasFollowingRelease(const aSpec: TFpSonarReleaseSpec;
  const aStmts: TPasImplElementArray;
  const aAcqs: TFpSonarAcquisitionArray; aIndex: integer): boolean;

var
  lFrom: integer;
  j: integer;

begin
  Result := False;
  lFrom := StmtIndex(aStmts, aAcqs[aIndex].Site);
  if lFrom < 0 then
    Exit;
  for j := lFrom + 1 to High(aStmts) do
    if Releases(aSpec, aStmts[j], aAcqs[aIndex].Decl)
      and not ReacquiredBetween(aStmts, aAcqs, aIndex, lFrom, j) then
      Exit(True);
end;


{ True when a finally section releasing aAcqs[aIndex]'s local covers its site:
  a try enclosing the site, or a try following the site or one of its
  ancestors with no later acquisition of that local in between. }
function CoveredByFinally(const aSpec: TFpSonarReleaseSpec;
  const aStmts: TPasImplElementArray;
  const aAcqs: TFpSonarAcquisitionArray; aIndex: integer): boolean;

var
  lWalk: TPasImplElement;
  lDecl: TPasElement;
  lSiblings: TPasImplElementArray;
  i, lAt, lFrom, lDepth: integer;

begin
  Result := False;
  lDecl := aAcqs[aIndex].Decl;
  lFrom := StmtIndex(aStmts, aAcqs[aIndex].Site);
  lWalk := aAcqs[aIndex].Site;
  lDepth := 0;
  while (lWalk.Parent is TPasImplElement) and (lDepth < cMaxParentDepth) do
  begin
    if ReleasesBelow(aSpec, FinallySection(lWalk.Parent), lDecl) then
      Exit(True);
    lSiblings := ChildStatements(TPasImplElement(lWalk.Parent));
    lAt := StmtIndex(lSiblings, lWalk);
    if lAt >= 0 then
      for i := lAt + 1 to High(lSiblings) do
        if ReleasesBelow(aSpec, FinallySection(lSiblings[i]), lDecl)
          and not ReacquiredBetween(aStmts, aAcqs, aIndex, lFrom,
            StmtIndex(aStmts, lSiblings[i])) then
          Exit(True);
    lWalk := TPasImplElement(lWalk.Parent);
    Inc(lDepth);
  end;
end;


{ True when a try..except of the same routine handles a raise at aStmt, the
  raise sitting in that statement's try body rather than in its handler. }
function RaiseIsHandled(aStmt: TPasImplElement): boolean;

var
  lPrev: TPasElement;
  lWalk: TPasElement;
  lDepth: integer;

begin
  Result := False;
  lPrev := aStmt;
  lWalk := aStmt.Parent;
  lDepth := 0;
  while (lWalk is TPasImplElement) and (lDepth < cMaxParentDepth) do
  begin
    if (lWalk is TPasImplTry)
      and (TPasImplTry(lWalk).FinallyExcept is TPasImplTryExcept)
      and (lPrev <> TPasImplTry(lWalk).FinallyExcept) then
      Exit(True);
    lPrev := lWalk;
    lWalk := lWalk.Parent;
    Inc(lDepth);
  end;
end;


// True when aStmt leaves the routine early: an Exit call or an unhandled raise.
function IsExitSite(aStmt: TPasImplElement): boolean;

var
  lExpr: TPasExpr;

begin
  if aStmt is TPasImplRaise then
    Exit(not RaiseIsHandled(aStmt));
  Result := False;
  lExpr := SimpleExpr(aStmt);
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
    lExpr := TParamsExpr(lExpr).Value;
  if (lExpr is TPrimitiveExpr) and (TPrimitiveExpr(lExpr).Kind = pekIdent) then
    Result := SameText(TPrimitiveExpr(lExpr).Value, 'exit');
end;


// True when aName is one of the stream classes rvUnprotectedStream matches.
function IsStreamClass(const aName: string): boolean;

var
  i: integer;

begin
  Result := False;
  for i := 0 to High(cStreamClasses) do
    if SameText(aName, cStreamClasses[i]) then
      Exit(True);
end;


{ TFpSonarResourceLattice }

constructor TFpSonarResourceLattice.Create(aResolver: TFpSonarResolver;
  aCFG: TPasCFG; const aAcquisitions: TFpSonarAcquisitionArray);

begin
  inherited Create;
  FResolver := aResolver;
  FCFG := aCFG;
  FAcquisitions := aAcquisitions;
end;


function TFpSonarResourceLattice.Direction: TPasDataFlowDirection;

begin
  Result := dfdForward;
end;


function TFpSonarResourceLattice.CreateState: TObject;

begin
  Result := TFpSonarOwnershipState.Create(Length(FAcquisitions));
end;


function TFpSonarResourceLattice.CopyState(aState: TObject): TObject;

var
  lCopy: TFpSonarOwnershipState;
  i: integer;

begin
  lCopy := TFpSonarOwnershipState.Create(Length(FAcquisitions));
  for i := 0 to lCopy.Count - 1 do
    lCopy[i] := TFpSonarOwnershipState(aState)[i];
  Result := lCopy;
end;


procedure TFpSonarResourceLattice.FreeState(aState: TObject);

begin
  aState.Free;
end;


procedure TFpSonarResourceLattice.Merge(aTarget: TObject; aSource: TObject);

var
  lTarget: TFpSonarOwnershipState;
  lSource: TFpSonarOwnershipState;
  i: integer;

begin
  lTarget := TFpSonarOwnershipState(aTarget);
  lSource := TFpSonarOwnershipState(aSource);
  for i := 0 to lTarget.Count - 1 do
    if lSource[i] > lTarget[i] then
      lTarget[i] := lSource[i];
end;


procedure TFpSonarResourceLattice.Transfer(aNode: TPasCFGNode;
  aState: TObject);

var
  lState: TFpSonarOwnershipState;
  lStmt: TPasImplElement;
  lDecl: TPasElement;
  i, j: integer;

begin
  { An unreachable node is still merged into its successors by the engine, so
    applying its statements would raise ownership on live paths. }
  if not FCFG.Reachable(aNode) then
    Exit;
  lState := TFpSonarOwnershipState(aState);
  for j := 0 to aNode.StatementCount - 1 do
  begin
    lStmt := aNode.Statements[j];
    for i := 0 to High(FAcquisitions) do
      if FAcquisitions[i].Site = lStmt then
        lState[i] := osOwned;
    lDecl := ReleasedDecl(FResolver, lStmt);
    if lDecl = nil then
      Continue;
    for i := 0 to High(FAcquisitions) do
      if FAcquisitions[i].Decl = lDecl then
        lState[i] := osReleased;
  end;
end;


function TFpSonarResourceLattice.SameState(aLeft: TObject;
  aRight: TObject): Boolean;

var
  i: integer;

begin
  for i := 0 to TFpSonarOwnershipState(aLeft).Count - 1 do
    if TFpSonarOwnershipState(aLeft)[i]
      <> TFpSonarOwnershipState(aRight)[i] then
      Exit(False);
  Result := True;
end;


constructor TFpSonarDataFlow.Create(aResolver: TFpSonarResolver);

begin
  inherited Create;
  FResolver := aResolver;
end;


destructor TFpSonarDataFlow.Destroy;

begin
  FreeAndNil(FAnalyzer);
  inherited Destroy;
end;


function TFpSonarDataFlow.EnsureAnalyzed: boolean;

var
  lModule: TPasModule;

begin
  Result := FUsable;
  if FAnalyzed then
    Exit;
  FAnalyzed := True;
  if (FResolver = nil) or (not FResolver.Succeeded) or (FResolver.Engine = nil) then
    Exit;
  lModule := FResolver.ResolvedModule;
  if lModule = nil then
    Exit;

  try
    FAnalyzer := TFpSonarDataFlowAnalyzer.Create(FResolver.Engine);
    FAnalyzer.AnalyzeModule(lModule);
    FUsable := True;
  except
    on E: Exception do
      FUsable := False;
  end;
  Result := FUsable;
end;


function TFpSonarDataFlow.TryUninitializedUses(out aNodes: TPasElementArray;
  out aNames: TFpSonarStringArray): boolean;

var
  lFinding: TPasDataFlowResult;
  i, lCount: integer;

begin
  aNodes := nil;
  aNames := nil;
  Result := False;
  if not EnsureAnalyzed then
    Exit;

  try
    lCount := FAnalyzer.ResultCount;
    SetLength(aNodes, lCount);
    SetLength(aNames, lCount);
    for i := 0 to lCount - 1 do
    begin
      lFinding := FAnalyzer.Results[i];
      aNodes[i] := lFinding.PosEl;
      aNames[i] := lFinding.Variable.Name;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      aNodes := nil;
      aNames := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


class function TFpSonarDataFlow.TryUnreachableStatements(aModule: TPasModule;
  out aNodes: TPasElementArray): boolean;

var
  lRoots: TPasImplElementArray;
  lLive, lDead: TFPList;
  lCFG: TPasCFG;
  lNode: TPasCFGNode;
  lEl: TPasElement;
  i, j, k: integer;

begin
  aNodes := nil;
  Result := False;
  if aModule = nil then
    Exit;

  lLive := TFPList.Create;
  lDead := TFPList.Create;
  try
    try
      lRoots := EnumerateStatementRoots(aModule);
      for i := 0 to High(lRoots) do
      begin
        lLive.Clear;
        lDead.Clear;
        lCFG := TPasCFG.Create(TPasImplBlock(lRoots[i]));
        try
          for j := 0 to lCFG.NodeCount - 1 do
          begin
            lNode := lCFG.Nodes[j];
            for k := 0 to lNode.StatementCount - 1 do
              if lCFG.Reachable(lNode) then
                lLive.Add(lNode.Statements[k])
              else
                lDead.Add(lNode.Statements[k]);
          end;
        finally
          lCFG.Free;
        end;

        for j := 0 to lLive.Count - 1 do
        begin
          lEl := TPasElement(lLive[j]).Parent;
          while lEl is TPasImplElement do
          begin
            k := lDead.IndexOf(lEl);
            if k >= 0 then
              lDead.Delete(k);
            lEl := lEl.Parent;
          end;
        end;

        for j := 0 to lDead.Count - 1 do
        begin
          SetLength(aNodes, Length(aNodes) + 1);
          aNodes[High(aNodes)] := TPasElement(lDead[j]);
        end;
      end;
      Result := True;
    except
      on E: Exception do
        aNodes := nil;
    end;
  finally
    lDead.Free;
    lLive.Free;
  end;
end;


{ The acquisitions among aStmts: an akDefault assignment of a confirmed
  constructor call to a local of the enclosing routine. }
function CollectAcquisitions(aResolver: TFpSonarResolver;
  const aStmts: TPasImplElementArray): TFpSonarAcquisitionArray;

var
  lAssign: TPasImplAssign;
  lDecl: TPasElement;
  lType: TFpSonarResolvedType;
  lCtorName: string;
  lOnInstance: boolean;
  i, n: integer;

begin
  SetLength(Result, 0);
  for i := 0 to High(aStmts) do
  begin
    if not (aStmts[i] is TPasImplAssign) then
      Continue;
    lAssign := TPasImplAssign(aStmts[i]);
    if lAssign.Kind <> akDefault then
      Continue;
    if not aResolver.TryConstructorCall(lAssign.Right, lOnInstance,
      lCtorName) then
      Continue;
    lDecl := aResolver.ReferencedDecl(lAssign.Left);
    if (lDecl = nil) or not (lDecl is TPasVariable)
      or not (lDecl.Parent is TProcedureBody) then
      Continue;
    SetLength(Result, Length(Result) + 1);
    n := High(Result);
    Result[n].Site := lAssign;
    Result[n].Decl := lDecl;
    Result[n].Name := lDecl.Name;
    if aResolver.TryResolvedType(lAssign.Right, lType) then
      Result[n].TypeName := lType.NamedTypeName
    else
      Result[n].TypeName := '';
  end;
end;


procedure AddFinding(var aFindings: TFpSonarResourceFindingArray;
  aSite: TPasElement; const aName: string;
  aVerdict: TFpSonarResourceVerdict);

begin
  SetLength(aFindings, Length(aFindings) + 1);
  aFindings[High(aFindings)].Site := aSite;
  aFindings[High(aFindings)].Name := aName;
  aFindings[High(aFindings)].Verdict := aVerdict;
end;


// Fills the two syntactic facts each verdict needs from aStmts.
procedure ClassifyAcquisitions(aResolver: TFpSonarResolver;
  const aStmts: TPasImplElementArray;
  var aAcqs: TFpSonarAcquisitionArray);

var
  lSpec: TFpSonarReleaseSpec;
  i: integer;

begin
  lSpec.Resolver := aResolver;
  lSpec.Method := '';
  for i := 0 to High(aAcqs) do
  begin
    aAcqs[i].Released := HasFollowingRelease(lSpec, aStmts, aAcqs, i);
    aAcqs[i].Covered := CoveredByFinally(lSpec, aStmts, aAcqs, i);
  end;
end;


// Records in aStmts/aNames every acquisition still owned at the exit site aAt.
procedure AddExitLeaks(aAt: TPasImplElement; aState: TFpSonarOwnershipState;
  const aAcqs: TFpSonarAcquisitionArray; aStmts: TFPList;
  aNames: TStringList);

var
  lSeen: boolean;
  i, n: integer;

begin
  for i := 0 to High(aAcqs) do
  begin
    if (aState[i] <> osOwned) or (not aAcqs[i].Released)
      or aAcqs[i].Covered then
      Continue;
    lSeen := False;
    for n := 0 to aStmts.Count - 1 do
      if (aStmts[n] = Pointer(aAt)) and (aNames[n] = aAcqs[i].Name) then
        lSeen := True;
    if lSeen then
      Continue;
    aStmts.Add(aAt);
    aNames.Add(aAcqs[i].Name);
  end;
end;


// Runs the ownership lattice over aBlock and records the exit leaks it exposes.
procedure RunResourceLattice(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; const aAcqs: TFpSonarAcquisitionArray;
  aStmts: TFPList; aNames: TStringList);

  // Walks the settled graph for reachable exit sites.
  procedure Harvest(aCFG: TPasCFG; aEngine: TPasDataFlowEngine);

  var
    lNode: TPasCFGNode;
    lState: TFpSonarOwnershipState;
    j: integer;
    k: integer;
  begin
    for j := 0 to aCFG.NodeCount - 1 do
    begin
      lNode := aCFG.Nodes[j];
      lState := TFpSonarOwnershipState(aEngine.StateOf(lNode));
      if (lState = nil) or (not aCFG.Reachable(lNode)) then
        Continue;
      for k := 0 to lNode.StatementCount - 1 do
        if IsExitSite(lNode.Statements[k]) then
          AddExitLeaks(lNode.Statements[k], lState, aAcqs, aStmts, aNames);
    end;
  end;

var
  lCFG: TPasCFG;
  lLattice: TFpSonarResourceLattice;
  lEngine: TPasDataFlowEngine;

begin
  lCFG := TPasCFG.Create(aBlock);
  try
    lLattice := TFpSonarResourceLattice.Create(aResolver, lCFG, aAcqs);
    try
      lEngine := TPasDataFlowEngine.Create;
      try
        lEngine.Run(lCFG, lLattice);
        Harvest(lCFG, lEngine);
      finally
        lEngine.Free;
      end;
    finally
      lLattice.Free;
    end;
  finally
    lCFG.Free;
  end;
end;


{ Appends the acquisition-site verdict of aAcq, if it has one. A stream owns
  its row, so the release verdict never doubles up on it. }
procedure AddSiteVerdict(const aAcq: TFpSonarAcquisition;
  var aFindings: TFpSonarResourceFindingArray);

begin
  if aAcq.Covered or not aAcq.Released then
    Exit;
  if IsStreamClass(aAcq.TypeName) then
    AddFinding(aFindings, aAcq.Site, aAcq.Name, rvUnprotectedStream)
  else
    AddFinding(aFindings, aAcq.Site, aAcq.Name, rvUnprotectedRelease);
end;


// Appends the verdicts of one routine to aFindings, in statement order.
procedure EmitRoutineFindings(const aStmts: TPasImplElementArray;
  const aAcqs: TFpSonarAcquisitionArray; aLeakStmts: TFPList;
  aLeakNames: TStringList; var aFindings: TFpSonarResourceFindingArray);

var
  lStmt: TPasImplElement;
  i, j, k: integer;

begin
  for j := 0 to High(aStmts) do
  begin
    lStmt := aStmts[j];
    for i := 0 to High(aAcqs) do
      if aAcqs[i].Site = lStmt then
        AddSiteVerdict(aAcqs[i], aFindings);
    for k := 0 to aLeakStmts.Count - 1 do
      if aLeakStmts[k] = Pointer(lStmt) then
        AddFinding(aFindings, lStmt, aLeakNames[k], rvEarlyExitLeak);
  end;
end;


// Appends the verdicts of the routine body aBlock to aFindings.
procedure AnalyzeRoutineResources(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; var aFindings: TFpSonarResourceFindingArray);

var
  lStmts: TPasImplElementArray;
  lAcqs: TFpSonarAcquisitionArray;
  lLeakStmts: TFPList;
  lLeakNames: TStringList;

begin
  if aBlock = nil then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aBlock, lStmts);
  lAcqs := CollectAcquisitions(aResolver, lStmts);
  if Length(lAcqs) = 0 then
    Exit;
  ClassifyAcquisitions(aResolver, lStmts, lAcqs);

  lLeakStmts := TFPList.Create;
  lLeakNames := TStringList.Create;
  try
    RunResourceLattice(aResolver, aBlock, lAcqs, lLeakStmts, lLeakNames);
    EmitRoutineFindings(lStmts, lAcqs, lLeakStmts, lLeakNames, aFindings);
  finally
    lLeakNames.Free;
    lLeakStmts.Free;
  end;
end;


function TFpSonarDataFlow.TryResourceFindings(
  out aFindings: TFpSonarResourceFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutineResources(FResolver, lRoutines[i].Block, aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


{ TFpSonarFlowState }

constructor TFpSonarFlowState.Create(aCount: integer);

begin
  inherited Create;
  SetLength(FBits, aCount);
end;


function TFpSonarFlowState.GetCount: integer;

begin
  Result := Length(FBits);
end;


function TFpSonarFlowState.GetBits(aIndex: integer): TFpSonarFlowBits;

begin
  Result := FBits[aIndex];
end;


procedure TFpSonarFlowState.SetBits(aIndex: integer;
  aValue: TFpSonarFlowBits);

begin
  FBits[aIndex] := aValue;
end;


{ TFpSonarFlowLattice }

constructor TFpSonarFlowLattice.Create(aResolver: TFpSonarResolver;
  aCFG: TPasCFG; const aSlots: TFpSonarFlowSlotArray);

begin
  inherited Create;
  FResolver := aResolver;
  FCFG := aCFG;
  FSlots := aSlots;
end;


function TFpSonarFlowLattice.Direction: TPasDataFlowDirection;

begin
  Result := dfdForward;
end;


function TFpSonarFlowLattice.CreateState: TObject;

begin
  Result := TFpSonarFlowState.Create(Length(FSlots));
end;


function TFpSonarFlowLattice.CopyState(aState: TObject): TObject;

var
  lCopy: TFpSonarFlowState;
  i: integer;

begin
  lCopy := TFpSonarFlowState.Create(Length(FSlots));
  for i := 0 to lCopy.Count - 1 do
    lCopy[i] := TFpSonarFlowState(aState)[i];
  Result := lCopy;
end;


procedure TFpSonarFlowLattice.FreeState(aState: TObject);

begin
  aState.Free;
end;


procedure TFpSonarFlowLattice.Merge(aTarget: TObject; aSource: TObject);

var
  lTarget: TFpSonarFlowState;
  lSource: TFpSonarFlowState;
  i: integer;

begin
  lTarget := TFpSonarFlowState(aTarget);
  lSource := TFpSonarFlowState(aSource);
  for i := 0 to lTarget.Count - 1 do
    lTarget[i] := lTarget[i] + lSource[i];
end;


procedure TFpSonarFlowLattice.ReportRead(aStmt: TPasImplElement;
  aNode: TPasElement; aDecl: TPasElement; aState: TFpSonarFlowState);

var
  k: integer;
  s: integer;

begin
  for s := 0 to High(FSlots) do
  begin
    if (FSlots[s].Kind <> skLocal) or (FSlots[s].Decl <> aDecl) then
      Continue;
    if not ([bfAssigned, bfUnassigned] <= aState[s]) then
      Exit;
    // One verdict per statement and declaration, whatever the operand count.
    for k := 0 to FReadNames.Count - 1 do
      if (FReadNames.Objects[k] = TObject(aStmt))
        and (FReadNames[k] = FSlots[s].Name) then
        Exit;
    FReadSites.Add(aNode);
    FReadNames.AddObject(FSlots[s].Name, TObject(aStmt));
    Exit;
  end;
end;


// True when aDecl occurs in aList.
function Mentions(const aList: TPasElementArray; aDecl: TPasElement): boolean;

var
  i: integer;

begin
  Result := False;
  for i := 0 to High(aList) do
    if aList[i] = aDecl then
      Exit(True);
end;


procedure TFpSonarFlowLattice.ApplyRead(aDecl: TPasElement;
  aState: TFpSonarFlowState);

var
  s: integer;

begin
  for s := 0 to High(FSlots) do
    if (FSlots[s].Kind = skStore) and (FSlots[s].Decl = aDecl)
      and (bfPending in aState[s]) then
      aState[s] := aState[s] + [bfRead];
end;


procedure TFpSonarFlowLattice.ApplyWrite(aDecl: TPasElement;
  aState: TFpSonarFlowState);

var
  s: integer;

begin
  for s := 0 to High(FSlots) do
  begin
    if FSlots[s].Decl <> aDecl then
      Continue;
    if FSlots[s].Kind in [skLocal, skResult] then
      aState[s] := [bfAssigned]
    else if bfPending in aState[s] then
      aState[s] := aState[s] - [bfPending] + [bfOverwritten];
  end;
end;


// True when aStmt is a call to Exit carrying at least one argument.
function IsExitWithArgument(aStmt: TPasImplElement): boolean;

var
  lExpr: TPasExpr;

begin
  lExpr := SimpleExpr(aStmt);
  Result := (lExpr is TParamsExpr)
    and (TParamsExpr(lExpr).Kind = pekFuncParams)
    and (Length(TParamsExpr(lExpr).Params) > 0)
    and IsNamedIdent(TParamsExpr(lExpr).Value, cExitName);
end;


procedure TFpSonarFlowLattice.StepStatement(aStmt: TPasImplElement;
  aState: TFpSonarFlowState);

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i: integer;
  s: integer;

begin
  if IsExitWithArgument(aStmt) then
    for s := 0 to High(FSlots) do
      if FSlots[s].Kind = skResult then
        aState[s] := [bfAssigned];
  if not FResolver.TryStatementAccess(aStmt, lReadNodes, lReadDecls,
    lWriteDecls) then
    Exit;
  // An assignment evaluates its right side first, so reads precede writes.
  for i := 0 to High(lReadDecls) do
  begin
    // A declaration the same statement also writes is left unjudged.
    if FReporting and not Mentions(lWriteDecls, lReadDecls[i]) then
      ReportRead(aStmt, lReadNodes[i], lReadDecls[i], aState);
    ApplyRead(lReadDecls[i], aState);
  end;
  for i := 0 to High(lWriteDecls) do
    ApplyWrite(lWriteDecls[i], aState);
  for s := 0 to High(FSlots) do
    if (FSlots[s].Kind = skStore) and (FSlots[s].Site = aStmt) then
      aState[s] := [bfPending];
end;


procedure TFpSonarFlowLattice.Transfer(aNode: TPasCFGNode; aState: TObject);

var
  lState: TFpSonarFlowState;
  i, j: integer;

begin
  // The engine merges an unreachable node into its successors regardless.
  if not FCFG.Reachable(aNode) then
    Exit;
  lState := TFpSonarFlowState(aState);
  // The entry node carries the boundary state the engine has no slot for.
  if aNode = FCFG.EntryNode then
    for i := 0 to High(FSlots) do
      if FSlots[i].Kind in [skLocal, skResult] then
        lState[i] := lState[i] + [bfUnassigned];
  for j := 0 to aNode.StatementCount - 1 do
    StepStatement(aNode.Statements[j], lState);
end;


function TFpSonarFlowLattice.SameState(aLeft: TObject;
  aRight: TObject): Boolean;

var
  i: integer;

begin
  for i := 0 to TFpSonarFlowState(aLeft).Count - 1 do
    if TFpSonarFlowState(aLeft)[i] <> TFpSonarFlowState(aRight)[i] then
      Exit(False);
  Result := True;
end;


procedure TFpSonarFlowLattice.Replay(aNode: TPasCFGNode; aState: TObject;
  aSites: TFPList; aNames: TStringList);

begin
  FReadSites := aSites;
  FReadNames := aNames;
  FReporting := True;
  try
    Transfer(aNode, aState);
  finally
    FReporting := False;
    FReadNames := nil;
    FReadSites := nil;
  end;
end;


// True when aType resolves to one of the scalars the lattice tracks.
function IsFlowTrackedType(aResolver: TFpSonarResolver;
  aType: TPasElement): boolean;

var
  lType: TFpSonarResolvedType;

begin
  Result := (aType <> nil) and aResolver.TryResolvedType(aType, lType)
    and (lType.Kind in cFlowTrackedKinds);
end;


// Appends aDecl to aEscaped when it is not already listed.
procedure AddEscaped(aEscaped: TFPList; aDecl: TPasElement);

begin
  if (aDecl <> nil) and (aEscaped.IndexOf(aDecl) < 0) then
    aEscaped.Add(aDecl);
end;


{ True when aStmt sits in the protected Elements of an enclosing try, whose
  partial state the engine propagates along the exceptional successors. }
function InTryProtectedRegion(aStmt: TPasElement): boolean;

var
  lEl: TPasElement;
  lChild: TPasElement;
  i: integer;

begin
  Result := False;
  lChild := aStmt;
  lEl := aStmt.Parent;
  i := 0;
  while (lEl <> nil) and (i < cMaxParentDepth) do
  begin
    if (lEl is TPasImplTry) and (lChild <> TPasImplTry(lEl).FinallyExcept)
      and (lChild <> TPasImplTry(lEl).ElseBranch) then
      Exit(True);
    lChild := lEl;
    lEl := lEl.Parent;
    Inc(i);
  end;
end;


{ Classifies every statement of aStmts, recording the declarations whose
  address is taken and those written inside a try's protected region. False
  when one statement cannot be classified. }
function ScanOwnStatements(aResolver: TFpSonarResolver;
  const aStmts: TPasImplElementArray; aEscaped: TFPList): boolean;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i, j: integer;

begin
  Result := True;
  for i := 0 to High(aStmts) do
  begin
    if not aResolver.TryStatementAccess(aStmts[i], lReadNodes, lReadDecls,
      lWriteDecls) then
      Exit(False);
    for j := 0 to High(lReadNodes) do
      if (lReadNodes[j].Parent is TUnaryExpr)
        and (TUnaryExpr(lReadNodes[j].Parent).OpCode = eopAddress) then
        AddEscaped(aEscaped, lReadDecls[j]);
    if not InTryProtectedRegion(aStmts[i]) then
      Continue;
    for j := 0 to High(lWriteDecls) do
      AddEscaped(aEscaped, lWriteDecls[j]);
  end;
end;


{ Records in aEscaped the declaration every absolute local of aBody aliases;
  the alias itself is untracked, but a read through it is a read of its
  target. }
procedure ScanAbsoluteAliases(aResolver: TFpSonarResolver;
  aBody: TPasDeclarations; aEscaped: TFPList);

var
  lEl: TPasElement;
  i: integer;

begin
  if aBody = nil then
    Exit;
  for i := 0 to aBody.Declarations.Count - 1 do
  begin
    lEl := TPasElement(aBody.Declarations[i]);
    if (lEl is TPasVariable) and (TPasVariable(lEl).AbsoluteExpr <> nil) then
      AddEscaped(aEscaped,
        aResolver.ReferencedDecl(TPasVariable(lEl).AbsoluteExpr));
  end;
end;


{ Records in aEscaped every declaration the statements of aBody mention.
  False when one of them cannot be classified. }
function ScanBodyMentions(aResolver: TFpSonarResolver; aBody: TPasImplBlock;
  aEscaped: TFPList): boolean;

var
  lStmts: TPasImplElementArray;
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  j: integer;
  k: integer;

begin
  Result := True;
  if aBody = nil then
    Exit;
  // An asm body yields no sub-statements at all.
  if not aResolver.TryStatementAccess(aBody, lReadNodes, lReadDecls,
    lWriteDecls) then
    Exit(False);
  SetLength(lStmts, 0);
  CollectStatements(aBody, lStmts);
  for j := 0 to High(lStmts) do
  begin
    if not aResolver.TryStatementAccess(lStmts[j], lReadNodes, lReadDecls,
      lWriteDecls) then
      Exit(False);
    for k := 0 to High(lReadDecls) do
      AddEscaped(aEscaped, lReadDecls[k]);
    for k := 0 to High(lWriteDecls) do
      AddEscaped(aEscaped, lWriteDecls[k]);
  end;
end;


{ Records in aEscaped every declaration the routines declared in aDecls
  mention, recursively. False when one nested statement cannot be classified. }
function ScanNestedRoutines(aResolver: TFpSonarResolver;
  aDecls: TPasDeclarations; aEscaped: TFPList): boolean;

var
  lProc: TPasProcedure;
  i: integer;

begin
  Result := True;
  if aDecls = nil then
    Exit;
  for i := 0 to aDecls.Declarations.Count - 1 do
  begin
    if not (TObject(aDecls.Declarations[i]) is TPasProcedure) then
      Continue;
    lProc := TPasProcedure(aDecls.Declarations[i]);
    if lProc.Body = nil then
      Continue;
    ScanAbsoluteAliases(aResolver, lProc.Body, aEscaped);
    if not ScanBodyMentions(aResolver, lProc.Body.Body, aEscaped) then
      Exit(False);
    if not ScanNestedRoutines(aResolver, lProc.Body, aEscaped) then
      Exit(False);
  end;
end;


// Appends one local slot per tracked variable declared in aBody.
procedure AddLocalSlots(aResolver: TFpSonarResolver; aBody: TPasDeclarations;
  aEscaped: TFPList; var aSlots: TFpSonarFlowSlotArray);

var
  lEl: TPasElement;
  i, n: integer;

begin
  if aBody = nil then
    Exit;
  for i := 0 to aBody.Declarations.Count - 1 do
  begin
    lEl := TPasElement(aBody.Declarations[i]);
    if (lEl.ClassType <> TPasVariable) or (TPasVariable(lEl).Expr <> nil)
      or (TPasVariable(lEl).AbsoluteExpr <> nil)
      or (aEscaped.IndexOf(lEl) >= 0)
      or not IsFlowTrackedType(aResolver, TPasVariable(lEl).VarType) then
      Continue;
    SetLength(aSlots, Length(aSlots) + 1);
    n := High(aSlots);
    aSlots[n].Kind := skLocal;
    aSlots[n].Decl := lEl;
    aSlots[n].Name := lEl.Name;
  end;
end;


{ True when aResult belongs to a routine enclosing aRoutine rather than to
  aRoutine itself, which is what a Result store in a nested routine yields. }
function IsForeignResult(const aRoutine: TAstRoutine;
  aResult: TPasElement): boolean;

var
  lOwner: TPasElement;
  lEl: TPasElement;
  i: integer;

begin
  if not (aRoutine.Decl is TPasFunction) then
    Exit(True);
  lOwner := aResult;
  i := 0;
  while (lOwner <> nil) and (i < cMaxParentDepth)
    and not (lOwner is TPasProcedure) do
  begin
    lOwner := lOwner.Parent;
    Inc(i);
  end;
  Result := lOwner = nil;
  if Result or (lOwner = aRoutine.Decl) then
    Exit;
  lEl := aRoutine.Decl.Parent;
  i := 0;
  while (lEl <> nil) and (i < cMaxParentDepth) do
  begin
    if lEl = lOwner then
      Exit(True);
    lEl := lEl.Parent;
    Inc(i);
  end;
end;


{ The declaration a tracked store of aAssign targets, nil when its left side is
  not one. The function result is recognised by class, not by identity. }
function StoreTarget(aResolver: TFpSonarResolver; const aRoutine: TAstRoutine;
  aAssign: TPasImplAssign; aEscaped: TFPList;
  const aSlots: TFpSonarFlowSlotArray): TPasElement;

var
  lDecl: TPasElement;
  j: integer;

begin
  Result := nil;
  if aAssign.Kind <> akDefault then
    Exit;
  lDecl := aResolver.ReferencedDecl(aAssign.Left);
  if lDecl = nil then
    Exit;
  if lDecl is TPasResultElement then
  begin
    if (aEscaped.IndexOf(lDecl) < 0) and not IsForeignResult(aRoutine, lDecl)
      and IsFlowTrackedType(aResolver, TPasResultElement(lDecl).ResultType) then
      Result := lDecl;
    Exit;
  end;
  for j := 0 to High(aSlots) do
    if (aSlots[j].Kind = skLocal) and (aSlots[j].Decl = lDecl) then
      Exit(lDecl);
end;


{ True when aAssign computes aDecl from itself without being a bare
  self-assignment, which is NoSelfAssignment's row rather than this lattice's. }
function IsSelfDerived(aResolver: TFpSonarResolver; aAssign: TPasImplAssign;
  aDecl: TPasElement): boolean;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;

begin
  Result := False;
  if aResolver.ReferencedDecl(aAssign.Right) = aDecl then
    Exit;
  if not aResolver.TryStatementAccess(aAssign, lReadNodes, lReadDecls,
    lWriteDecls) then
    Exit;
  Result := Mentions(lReadDecls, aDecl);
end;


{ Appends one store slot per akDefault assignment of aStmts whose left side
  resolves to a tracked local or to a tracked function result. }
procedure AddStoreSlots(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; const aStmts: TPasImplElementArray;
  aEscaped: TFPList; var aSlots: TFpSonarFlowSlotArray);

var
  lAssign: TPasImplAssign;
  lDecl: TPasElement;
  i, n: integer;

begin
  for i := 0 to High(aStmts) do
  begin
    if not (aStmts[i] is TPasImplAssign) then
      Continue;
    lAssign := TPasImplAssign(aStmts[i]);
    lDecl := StoreTarget(aResolver, aRoutine, lAssign, aEscaped, aSlots);
    if lDecl = nil then
      Continue;
    SetLength(aSlots, Length(aSlots) + 1);
    n := High(aSlots);
    aSlots[n].Kind := skStore;
    aSlots[n].Decl := lDecl;
    aSlots[n].Name := lDecl.Name;
    aSlots[n].Site := lAssign;
    aSlots[n].IsResult := lDecl is TPasResultElement;
    aSlots[n].SelfDerived := IsSelfDerived(aResolver, lAssign, lDecl);
  end;
end;


function InCallArguments(aNode: TPasElement): boolean; forward;
function InLoopBody(aStmt: TPasElement): boolean; forward;


{ True when a statement of aStmts assigns aDecl from inside a loop body or hands
  it to a call, either of which takes the result slot out whole. An Exit
  carrying an argument assigns it. }
function ResultSlotDropped(aResolver: TFpSonarResolver;
  const aStmts: TPasImplElementArray; aDecl: TPasElement): boolean;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i, j: integer;

begin
  Result := False;
  for i := 0 to High(aStmts) do
  begin
    if not aResolver.TryStatementAccess(aStmts[i], lReadNodes, lReadDecls,
      lWriteDecls) then
      Continue;
    if InLoopBody(aStmts[i]) and (Mentions(lWriteDecls, aDecl)
      or IsExitWithArgument(aStmts[i])) then
      Exit(True);
    for j := 0 to High(lReadNodes) do
      if (lReadDecls[j] = aDecl) and InCallArguments(lReadNodes[j]) then
        Exit(True);
  end;
end;


{ True when aDecl is the result element of aRoutine itself or of the separate
  declaration its body implements, which is the one references bind to. }
function IsOwnResult(const aRoutine: TAstRoutine;
  aDecl: TPasElement): boolean;

var
  lOwner: TPasElement;
  i: integer;

begin
  Result := False;
  if not (aDecl is TPasResultElement) or IsForeignResult(aRoutine, aDecl) then
    Exit;
  lOwner := aDecl;
  i := 0;
  while (lOwner <> nil) and (i < cMaxParentDepth)
    and not (lOwner is TPasProcedure) do
  begin
    lOwner := lOwner.Parent;
    Inc(i);
  end;
  if lOwner = aRoutine.Decl then
    Exit(True);
  // A routine declared inside this one owns a result of its own.
  lOwner := lOwner.Parent;
  i := 0;
  while (lOwner <> nil) and (i < cMaxParentDepth) do
  begin
    if lOwner = aRoutine.Decl then
      Exit;
    lOwner := lOwner.Parent;
    Inc(i);
  end;
  Result := True;
end;


// True when aEscaped holds the result element of aRoutine.
function EscapedResult(const aRoutine: TAstRoutine;
  aEscaped: TFPList): boolean;

var
  i: integer;

begin
  Result := False;
  for i := 0 to aEscaped.Count - 1 do
    if IsOwnResult(aRoutine, TPasElement(aEscaped[i])) then
      Exit(True);
end;


// The result element of aRoutine a statement of aStmts names, nil for none.
function BoundResult(aResolver: TFpSonarResolver; const aRoutine: TAstRoutine;
  const aStmts: TPasImplElementArray): TPasElement;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i, j: integer;

begin
  Result := nil;
  for i := 0 to High(aStmts) do
  begin
    if not aResolver.TryStatementAccess(aStmts[i], lReadNodes, lReadDecls,
      lWriteDecls) then
      Continue;
    for j := 0 to High(lReadDecls) do
      if IsOwnResult(aRoutine, lReadDecls[j]) then
        Exit(lReadDecls[j]);
    for j := 0 to High(lWriteDecls) do
      if IsOwnResult(aRoutine, lWriteDecls[j]) then
        Exit(lWriteDecls[j]);
  end;
end;


{ Appends the result slot of aRoutine when it is a function whose result the
  lattice may stand for. }
procedure AddResultSlot(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; const aStmts: TPasImplElementArray;
  aEscaped: TFPList; var aSlots: TFpSonarFlowSlotArray);

var
  lFunc: TPasFunction;
  lDecl: TPasElement;
  n: integer;

begin
  if not (aRoutine.Decl is TPasFunction) or (aRoutine.Decl is TPasOperator) then
    Exit;
  lFunc := TPasFunction(aRoutine.Decl);
  if lFunc.IsAssembler or (lFunc.FuncType = nil)
    or (lFunc.FuncType.ResultEl = nil)
    or not IsFlowTrackedType(aResolver, lFunc.FuncType.ResultEl.ResultType)
    or EscapedResult(aRoutine, aEscaped) then
    Exit;
  lDecl := BoundResult(aResolver, aRoutine, aStmts);
  if lDecl = nil then
    lDecl := lFunc.FuncType.ResultEl;
  if ResultSlotDropped(aResolver, aStmts, lDecl) then
    Exit;
  SetLength(aSlots, Length(aSlots) + 1);
  n := High(aSlots);
  aSlots[n].Kind := skResult;
  aSlots[n].Decl := lDecl;
  aSlots[n].Name := lDecl.Name;
end;


{ Fills aSlots with one slot per tracked local of aRoutine and one per tracked
  store in aStmts. False when the routine cannot be classified at all. }
function CollectFlowSlots(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; const aStmts: TPasImplElementArray;
  out aSlots: TFpSonarFlowSlotArray): boolean;

var
  lEscaped: TFPList;

begin
  SetLength(aSlots, 0);
  Result := False;
  lEscaped := TFPList.Create;
  try
    if not ScanOwnStatements(aResolver, aStmts, lEscaped) then
      Exit;
    if not ScanNestedRoutines(aResolver, aRoutine.Decl.Body, lEscaped) then
      Exit;
    ScanAbsoluteAliases(aResolver, aRoutine.Decl.Body, lEscaped);
    AddLocalSlots(aResolver, aRoutine.Decl.Body, lEscaped, aSlots);
    AddStoreSlots(aResolver, aRoutine, aStmts, lEscaped, aSlots);
    AddResultSlot(aResolver, aRoutine, aStmts, lEscaped, aSlots);
    Result := True;
  finally
    lEscaped.Free;
  end;
end;


// Marks every slot aNode carries a store site of, or a read of, as such.
procedure HarvestNode(aNode: TPasCFGNode; aState: TFpSonarFlowState;
  var aSlots: TFpSonarFlowSlotArray);

var
  k: integer;
  s: integer;

begin
  for k := 0 to aNode.StatementCount - 1 do
    for s := 0 to High(aSlots) do
      if aSlots[s].Site = aNode.Statements[k] then
        aSlots[s].Reachable := True;
  if aState = nil then
    Exit;
  for s := 0 to High(aSlots) do
    if bfRead in aState[s] then
      aSlots[s].WasRead := True;
end;


// Reads the reachability, read and exit facts off the settled graph.
procedure HarvestFlowFacts(aCFG: TPasCFG; aEngine: TPasDataFlowEngine;
  var aSlots: TFpSonarFlowSlotArray);

var
  lNode: TPasCFGNode;
  lState: TFpSonarFlowState;
  j: integer;
  s: integer;

begin
  for j := 0 to aCFG.NodeCount - 1 do
  begin
    lNode := aCFG.Nodes[j];
    if aCFG.Reachable(lNode) then
      HarvestNode(lNode, TFpSonarFlowState(aEngine.StateOf(lNode)), aSlots);
  end;
  lState := TFpSonarFlowState(aEngine.StateOf(aCFG.ExitNode));
  if lState = nil then
    Exit;
  for s := 0 to High(aSlots) do
  begin
    aSlots[s].ExitOverwritten := (bfOverwritten in lState[s])
      and not (bfPending in lState[s]);
    aSlots[s].ExitUnassigned := (aSlots[s].Kind = skResult)
      and (bfUnassigned in lState[s]);
  end;
end;


{ Rebuilds every node's in-state by pushing each out-state into its successors,
  then replays the node for the read verdicts. }
procedure ReplayFlowNodes(aCFG: TPasCFG; aEngine: TPasDataFlowEngine;
  aLattice: TFpSonarFlowLattice; aReadSites: TFPList;
  aReadNames: TStringList);

var
  lIn: TFPList;
  lNode: TPasCFGNode;
  j: integer;
  k: integer;

begin
  lIn := TFPList.Create;
  try
    for j := 0 to aCFG.NodeCount - 1 do
      lIn.Add(aLattice.CreateState);
    for j := 0 to aCFG.NodeCount - 1 do
    begin
      lNode := aCFG.Nodes[j];
      if aEngine.StateOf(lNode) = nil then
        Continue;
      for k := 0 to lNode.SuccessorCount - 1 do
        aLattice.Merge(TObject(lIn[lNode.Successors[k].Index]),
          aEngine.StateOf(lNode));
    end;
    for j := 0 to aCFG.NodeCount - 1 do
      if aCFG.Reachable(aCFG.Nodes[j]) then
        aLattice.Replay(aCFG.Nodes[j], TObject(lIn[j]), aReadSites,
          aReadNames);
  finally
    for j := 0 to lIn.Count - 1 do
      aLattice.FreeState(TObject(lIn[j]));
    lIn.Free;
  end;
end;


{ Runs the forward lattice over aBlock, filling in each slot's settled facts
  and recording every maybe-unassigned read. }
procedure RunFlowLattice(aResolver: TFpSonarResolver; aBlock: TPasImplBlock;
  var aSlots: TFpSonarFlowSlotArray; aReadSites: TFPList;
  aReadNames: TStringList);

var
  lCFG: TPasCFG;
  lLattice: TFpSonarFlowLattice;
  lEngine: TPasDataFlowEngine;

begin
  lCFG := TPasCFG.Create(aBlock);
  try
    lLattice := TFpSonarFlowLattice.Create(aResolver, lCFG, aSlots);
    try
      lEngine := TPasDataFlowEngine.Create;
      try
        lEngine.Run(lCFG, lLattice);
        HarvestFlowFacts(lCFG, lEngine, aSlots);
        ReplayFlowNodes(lCFG, lEngine, lLattice, aReadSites, aReadNames);
      finally
        lEngine.Free;
      end;
    finally
      lLattice.Free;
    end;
  finally
    lCFG.Free;
  end;
end;


// Appends one finding to aFindings.
procedure AddFlowFinding(var aFindings: TFpSonarFlowFindingArray;
  aSite: TPasElement; const aName: string; aVerdict: TFpSonarFlowVerdict);

begin
  SetLength(aFindings, Length(aFindings) + 1);
  aFindings[High(aFindings)].Site := aSite;
  aFindings[High(aFindings)].Name := aName;
  aFindings[High(aFindings)].Verdict := aVerdict;
end;


{ Appends the store-site verdict of aSlot, if it has one. A result store is
  judged by the result verdict alone, so one that fails it yields nothing. }
procedure AddStoreVerdict(const aSlot: TFpSonarFlowSlot;
  var aFindings: TFpSonarFlowFindingArray);

begin
  if (not aSlot.Reachable) or aSlot.WasRead then
    Exit;
  if aSlot.IsResult then
  begin
    if aSlot.ExitOverwritten then
      AddFlowFinding(aFindings, aSlot.Site, aSlot.Name, fvResultOverwritten);
    Exit;
  end;
  if aSlot.SelfDerived then
    AddFlowFinding(aFindings, aSlot.Site, aSlot.Name, fvSelfAssignedNeverUsed)
  else if aSlot.ExitOverwritten then
    AddFlowFinding(aFindings, aSlot.Site, aSlot.Name, fvDeadStore);
end;


// Appends the verdicts of one routine to aFindings, in statement order.
procedure EmitFlowVerdicts(const aStmts: TPasImplElementArray;
  const aSlots: TFpSonarFlowSlotArray; aReadSites: TFPList;
  aReadNames: TStringList; var aFindings: TFpSonarFlowFindingArray);

var
  lStmt: TPasImplElement;
  i, j, k: integer;

begin
  for j := 0 to High(aStmts) do
  begin
    lStmt := aStmts[j];
    for i := 0 to High(aSlots) do
      if (aSlots[i].Kind = skStore) and (aSlots[i].Site = lStmt) then
        AddStoreVerdict(aSlots[i], aFindings);
    for k := 0 to aReadNames.Count - 1 do
      if aReadNames.Objects[k] = TObject(lStmt) then
        AddFlowFinding(aFindings, TPasElement(aReadSites[k]), aReadNames[k],
          fvUninitializedStrict);
  end;
end;


// Appends the forward-dataflow verdicts of aRoutine to aFindings.
procedure AnalyzeRoutineFlow(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; var aFindings: TFpSonarFlowFindingArray);

var
  lStmts: TPasImplElementArray;
  lSlots: TFpSonarFlowSlotArray;
  lReadSites: TFPList;
  lReadNames: TStringList;
  s: integer;

begin
  if (aRoutine.Block = nil) or (aRoutine.Decl = nil)
    or (aRoutine.Decl.Body = nil) then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aRoutine.Block, lStmts);
  if not CollectFlowSlots(aResolver, aRoutine, lStmts, lSlots) then
    Exit;
  if Length(lSlots) = 0 then
    Exit;

  lReadSites := TFPList.Create;
  lReadNames := TStringList.Create;
  try
    RunFlowLattice(aResolver, aRoutine.Block, lSlots, lReadSites, lReadNames);
    for s := 0 to High(lSlots) do
      if lSlots[s].ExitUnassigned then
        AddFlowFinding(aFindings, aRoutine.Decl, lSlots[s].Name,
          fvResultUnassigned);
    EmitFlowVerdicts(lStmts, lSlots, lReadSites, lReadNames, aFindings);
  finally
    lReadNames.Free;
    lReadSites.Free;
  end;
end;


function TFpSonarDataFlow.TryFlowFindings(
  out aFindings: TFpSonarFlowFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutineFlow(FResolver, lRoutines[i], aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


// The identifier an assignment target names, unwrapping a Self./Obj. prefix.
function TargetIdent(aExpr: TPasExpr): TPasExpr;

begin
  Result := aExpr;
  if (Result is TBinaryExpr) and (TBinaryExpr(Result).OpCode = eopSubIdent) then
    Result := TBinaryExpr(Result).Right;
end;


// True when aExpr names the instance the enclosing method runs on.
function IsSelfRef(aExpr: TPasExpr): boolean;

begin
  Result := (aExpr is TSelfExpr)
    or ((aExpr is TPrimitiveExpr) and (TPrimitiveExpr(aExpr).Kind = pekIdent)
    and SameText(TPrimitiveExpr(aExpr).Value, 'Self'));
end;


// True when aNode is a member reached through a qualifier other than Self.
function ForeignQualifier(aNode: TPasElement): boolean;

var
  lOwner: TBinaryExpr;

begin
  Result := False;
  if not (aNode is TPasExpr) or not (aNode.Parent is TBinaryExpr) then
    Exit;
  lOwner := TBinaryExpr(aNode.Parent);
  Result := (lOwner.OpCode = eopSubIdent) and (lOwner.Right = aNode)
    and not IsSelfRef(lOwner.Left);
end;


// True when aNode sits in the argument list of a call expression.
function InCallArguments(aNode: TPasElement): boolean;

var
  lChild: TPasElement;
  lEl: TPasElement;
  i, k: integer;

begin
  Result := False;
  lChild := aNode;
  lEl := aNode.Parent;
  i := 0;
  while (lEl is TPasExpr) and (i < cMaxParentDepth) do
  begin
    if (lEl is TParamsExpr) and (TParamsExpr(lEl).Kind = pekFuncParams) then
      for k := 0 to High(TParamsExpr(lEl).Params) do
        if TParamsExpr(lEl).Params[k] = lChild then
          Exit(True);
    lChild := lEl;
    lEl := lEl.Parent;
    Inc(i);
  end;
end;


{ The declaration a site operates on, nil when its operand is reached through a
  qualifier other than Self. }
function SiteDecl(aResolver: TFpSonarResolver; aExpr: TPasExpr): TPasElement;

var
  lIdent: TPasExpr;

begin
  Result := nil;
  lIdent := TargetIdent(aExpr);
  if (lIdent = nil) or ForeignQualifier(lIdent) then
    Exit;
  Result := aResolver.ReferencedDecl(lIdent);
end;


// What aStmt does to the declaration aDecl it names.
function ClassifySite(aResolver: TFpSonarResolver; aStmt: TPasImplElement;
  out aDecl: TPasElement): TFpSonarFreeAction;

var
  lInner: TPasExpr;
  lAssign: TPasImplAssign;
  lOp: TFpSonarMemoryOp;
  lName: string;
  lOnInstance: boolean;

begin
  Result := faNone;
  aDecl := nil;
  case aResolver.TryFreeCall(SimpleExpr(aStmt), lInner) of
    lfkFreeMethod: Result := faFree;
    lfkFreeAndNil: Result := faFreeAndNil;
  end;
  if Result <> faNone then
  begin
    aDecl := SiteDecl(aResolver, lInner);
    if aDecl = nil then
      Result := faNone;
    Exit;
  end;
  if aResolver.TryMemoryOpCall(aStmt, lOp, aDecl, lName) then
  begin
    if lOp in [lmoNew, lmoGetMem] then
      Result := faAllocate
    else
      Result := faDeallocate;
    Exit;
  end;
  aDecl := nil;
  if not (aStmt is TPasImplAssign) then
    Exit;
  lAssign := TPasImplAssign(aStmt);
  if lAssign.Kind <> akDefault then
    Exit;
  if aResolver.TryConstructorCall(lAssign.Right, lOnInstance, lName) then
    Result := faCreate
  else if lAssign.Right is TNilExpr then
    Result := faNilStore
  else
    Exit;
  aDecl := SiteDecl(aResolver, lAssign.Left);
  if aDecl = nil then
    Result := faNone;
end;


{ True when aDecl is a declaration the free-state lattice may track: a variable
  declared in aBody or a field of a class or record, aliasing no other storage. }
function IsFreeStateTrackable(aDecl: TPasElement; aBody: TPasElement): boolean;

begin
  Result := (aDecl <> nil) and (aDecl.ClassType = TPasVariable)
    and (TPasVariable(aDecl).AbsoluteExpr = nil)
    and ((aDecl.Parent = aBody) or (aDecl.Parent is TPasClassType)
    or (aDecl.Parent is TPasRecordType));
end;


// The slot of aDecl in aSlots, -1 when it has none.
function FreeSlotIndex(const aSlots: TFpSonarFreeSlotArray;
  aDecl: TPasElement): integer;

var
  i: integer;

begin
  Result := -1;
  if aDecl = nil then
    Exit;
  for i := 0 to High(aSlots) do
    if aSlots[i].Decl = aDecl then
      Exit(i);
end;


{ The declaration an assignment hands its right-hand side over from, nil when
  the right-hand side names none. }
function HandedOverDecl(aResolver: TFpSonarResolver;
  aStmt: TPasImplElement): TPasElement;

begin
  Result := nil;
  if not (aStmt is TPasImplAssign) then
    Exit;
  Result := aResolver.ReferencedDecl(TPasImplAssign(aStmt).Right);
end;


// True when aStmt sits inside the body of a while, repeat or for loop.
function InLoopBody(aStmt: TPasElement): boolean;

var
  lWalk: TPasElement;
  i: integer;

begin
  Result := False;
  lWalk := aStmt.Parent;
  i := 0;
  while (lWalk <> nil) and (i < cMaxParentDepth) do
  begin
    if (lWalk is TPasImplWhileDo) or (lWalk is TPasImplRepeatUntil)
      or (lWalk is TPasImplForLoop) then
      Exit(True);
    lWalk := lWalk.Parent;
    Inc(i);
  end;
end;


// Appends one slot for the site of aAction on aDecl, or updates the one it has.
procedure AddFreeSlot(var aSlots: TFpSonarFreeSlotArray; aDecl: TPasElement;
  aAction: TFpSonarFreeAction; aBody: TPasElement; aMixed: TFPList);

var
  lSort: TFpSonarSlotSort;
  n: integer;

begin
  if not IsFreeStateTrackable(aDecl, aBody) then
    Exit;
  if aAction in [faCreate, faFree, faFreeAndNil] then
    lSort := ssObject
  else
    lSort := ssPointer;
  n := FreeSlotIndex(aSlots, aDecl);
  if n < 0 then
  begin
    SetLength(aSlots, Length(aSlots) + 1);
    n := High(aSlots);
    aSlots[n].Decl := aDecl;
    aSlots[n].Name := aDecl.Name;
    aSlots[n].Sort := lSort;
    aSlots[n].IsField := not (aDecl.Parent is TProcedureBody);
  end
  else if aSlots[n].Sort <> lSort then
    AddEscaped(aMixed, aDecl);
  if aAction = faAllocate then
    Inc(aSlots[n].Allocations);
end;


{ Fills aSlots with one slot per tracked declaration of aStmts. False when one
  statement cannot be classified, which takes the whole routine out. }
function CollectFreeSlots(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; const aStmts: TPasImplElementArray;
  out aSlots: TFpSonarFreeSlotArray): boolean;

var
  lActions: array of TFpSonarFreeAction;
  lSiteDecls: TPasElementArray;
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  lDropped: TFPList;
  i, j, n: integer;

begin
  SetLength(aSlots, 0);
  Result := False;
  lDropped := TFPList.Create;
  try
    if not ScanNestedRoutines(aResolver, aRoutine.Decl.Body, lDropped) then
      Exit;
    ScanAbsoluteAliases(aResolver, aRoutine.Decl.Body, lDropped);
    SetLength(lActions, Length(aStmts));
    SetLength(lSiteDecls, Length(aStmts));
    for i := 0 to High(aStmts) do
    begin
      if not aResolver.TryStatementAccess(aStmts[i], lReadNodes, lReadDecls,
        lWriteDecls) then
        Exit;
      lActions[i] := ClassifySite(aResolver, aStmts[i], lSiteDecls[i]);
      if lActions[i] = faNone then
        AddEscaped(lDropped, HandedOverDecl(aResolver, aStmts[i]));
      for j := 0 to High(lReadNodes) do
        if ((lReadNodes[j].Parent is TUnaryExpr)
          and (TUnaryExpr(lReadNodes[j].Parent).OpCode = eopAddress))
          or ForeignQualifier(lReadNodes[j])
          or ((lReadDecls[j] <> lSiteDecls[i])
          and InCallArguments(lReadNodes[j])) then
          AddEscaped(lDropped, lReadDecls[j]);
      for j := 0 to High(lWriteDecls) do
        if (lWriteDecls[j] <> lSiteDecls[i]) or (lActions[i] = faNone) then
          AddEscaped(lDropped, lWriteDecls[j]);
    end;
    for i := 0 to High(aStmts) do
      if lActions[i] in [faCreate, faFree, faFreeAndNil, faAllocate,
        faDeallocate] then
        AddFreeSlot(aSlots, lSiteDecls[i], lActions[i], aRoutine.Decl.Body,
          lDropped);
    n := 0;
    for i := 0 to High(aSlots) do
      if lDropped.IndexOf(aSlots[i].Decl) < 0 then
      begin
        aSlots[n] := aSlots[i];
        Inc(n);
      end;
    SetLength(aSlots, n);
    Result := True;
  finally
    lDropped.Free;
  end;
end;


{ TFpSonarFreeStateLattice }

constructor TFpSonarFreeStateLattice.Create(aResolver: TFpSonarResolver;
  aCFG: TPasCFG; const aSlots: TFpSonarFreeSlotArray);

begin
  inherited Create;
  FResolver := aResolver;
  FCFG := aCFG;
  FSlots := aSlots;
end;


function TFpSonarFreeStateLattice.Direction: TPasDataFlowDirection;

begin
  Result := dfdForward;
end;


function TFpSonarFreeStateLattice.CreateState: TObject;

begin
  Result := TFpSonarOwnershipState.Create(Length(FSlots));
end;


function TFpSonarFreeStateLattice.CopyState(aState: TObject): TObject;

var
  lCopy: TFpSonarOwnershipState;
  i: integer;

begin
  lCopy := TFpSonarOwnershipState.Create(Length(FSlots));
  for i := 0 to lCopy.Count - 1 do
    lCopy[i] := TFpSonarOwnershipState(aState)[i];
  Result := lCopy;
end;


procedure TFpSonarFreeStateLattice.FreeState(aState: TObject);

begin
  aState.Free;
end;


procedure TFpSonarFreeStateLattice.Merge(aTarget: TObject; aSource: TObject);

var
  lTarget: TFpSonarOwnershipState;
  lSource: TFpSonarOwnershipState;
  i: integer;

begin
  lTarget := TFpSonarOwnershipState(aTarget);
  lSource := TFpSonarOwnershipState(aSource);
  for i := 0 to lTarget.Count - 1 do
    if lSource[i] > lTarget[i] then
      lTarget[i] := lSource[i];
end;


procedure TFpSonarFreeStateLattice.AddCandidate(aStmt: TPasImplElement;
  aSlot: integer; aVerdict: TFpSonarFreeStateVerdict);

var
  i: integer;

begin
  for i := 0 to High(FCandidates) do
    if (FCandidates[i].Site = aStmt) and (FCandidates[i].Slot = aSlot)
      and (FCandidates[i].Verdict = aVerdict) then
      Exit;
  SetLength(FCandidates, Length(FCandidates) + 1);
  FCandidates[High(FCandidates)].Site := aStmt;
  FCandidates[High(FCandidates)].Slot := aSlot;
  FCandidates[High(FCandidates)].Verdict := aVerdict;
end;


procedure TFpSonarFreeStateLattice.ReportSite(aStmt: TPasImplElement;
  aAction: TFpSonarFreeAction; aDecl: TPasElement;
  aState: TFpSonarOwnershipState);

var
  n: integer;

begin
  n := FreeSlotIndex(FSlots, aDecl);
  if n < 0 then
    Exit;
  // Recorded in precedence order: the first candidate a statement gets wins.
  if (FSlots[n].Sort = ssObject) and (aAction in [faFree, faFreeAndNil])
    and (aState[n] = osReleased) then
    AddCandidate(aStmt, n, fsvDoubleFree);
  if (FSlots[n].Sort = ssObject) and FSlots[n].IsField and (aAction = faFree) then
    AddCandidate(aStmt, n, fsvFieldFreedNotNilled);
  if (FSlots[n].Sort = ssObject) and (aAction = faCreate)
    and (aState[n] = osOwned) and InLoopBody(aStmt) then
    AddCandidate(aStmt, n, fsvLoopAllocationNotFreed);
  if (FSlots[n].Sort = ssPointer) and (not FSlots[n].IsField)
    and (aAction = faAllocate) then
    AddCandidate(aStmt, n, fsvUnpairedAllocation);
end;


procedure TFpSonarFreeStateLattice.ReportReads(aStmt: TPasImplElement;
  const aReadDecls: TPasElementArray; aAction: TFpSonarFreeAction;
  aDecl: TPasElement; aState: TFpSonarOwnershipState);

var
  i, n: integer;

begin
  for i := 0 to High(aReadDecls) do
  begin
    n := FreeSlotIndex(FSlots, aReadDecls[i]);
    if (n < 0) or (FSlots[n].Sort <> ssObject)
      or (aState[n] <> osReleased) then
      Continue;
    { A release and a nil store read their own operand; that site belongs to the
      release verdicts, not to a use. }
    if (aReadDecls[i] = aDecl)
      and (aAction in [faFree, faFreeAndNil, faNilStore]) then
      Continue;
    if FSlots[n].IsField then
      FSlots[n].ReadWhileReleased := True
    else
      AddCandidate(aStmt, n, fsvUseAfterFree);
  end;
end;


procedure TFpSonarFreeStateLattice.StepStatement(aStmt: TPasImplElement;
  aState: TFpSonarOwnershipState);

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  lAction: TFpSonarFreeAction;
  lDecl: TPasElement;
  n: integer;

begin
  lAction := ClassifySite(FResolver, aStmt, lDecl);
  if not FResolver.TryStatementAccess(aStmt, lReadNodes, lReadDecls,
    lWriteDecls) then
    Exit;
  // The state a statement is judged against is the one it was reached in.
  if FReporting then
  begin
    ReportSite(aStmt, lAction, lDecl, aState);
    ReportReads(aStmt, lReadDecls, lAction, lDecl, aState);
  end;
  n := FreeSlotIndex(FSlots, lDecl);
  if n < 0 then
    Exit;
  case lAction of
    faFree, faDeallocate: aState[n] := osReleased;
    faFreeAndNil: aState[n] := osNilled;
    // Nilling a pointer neither allocates nor frees the block it named.
    faNilStore: if FSlots[n].Sort = ssObject then
                  aState[n] := osNilled;
    faCreate, faAllocate: aState[n] := osOwned;
  end;
end;


procedure TFpSonarFreeStateLattice.Transfer(aNode: TPasCFGNode;
  aState: TObject);

var
  j: integer;

begin
  // The engine merges an unreachable node into its successors regardless.
  if not FCFG.Reachable(aNode) then
    Exit;
  for j := 0 to aNode.StatementCount - 1 do
    StepStatement(aNode.Statements[j], TFpSonarOwnershipState(aState));
end;


function TFpSonarFreeStateLattice.SameState(aLeft: TObject;
  aRight: TObject): Boolean;

var
  i: integer;

begin
  for i := 0 to TFpSonarOwnershipState(aLeft).Count - 1 do
    if TFpSonarOwnershipState(aLeft)[i]
      <> TFpSonarOwnershipState(aRight)[i] then
      Exit(False);
  Result := True;
end;


procedure TFpSonarFreeStateLattice.Replay(aNode: TPasCFGNode; aState: TObject);

begin
  FReporting := True;
  try
    Transfer(aNode, aState);
  finally
    FReporting := False;
  end;
end;


procedure TFpSonarFreeStateLattice.HarvestExit(aState: TObject);

var
  s: integer;

begin
  if aState = nil then
    Exit;
  for s := 0 to High(FSlots) do
    FSlots[s].ExitOwned := TFpSonarOwnershipState(aState)[s] = osOwned;
end;


{ Rebuilds every node's in-state by pushing each out-state into its successors,
  then replays the node for the verdicts. }
procedure ReplayFreeStateNodes(aCFG: TPasCFG; aEngine: TPasDataFlowEngine;
  aLattice: TFpSonarFreeStateLattice);

var
  lIn: TFPList;
  lNode: TPasCFGNode;
  j, k: integer;

begin
  lIn := TFPList.Create;
  try
    for j := 0 to aCFG.NodeCount - 1 do
      lIn.Add(aLattice.CreateState);
    for j := 0 to aCFG.NodeCount - 1 do
    begin
      lNode := aCFG.Nodes[j];
      if aEngine.StateOf(lNode) = nil then
        Continue;
      for k := 0 to lNode.SuccessorCount - 1 do
        aLattice.Merge(TObject(lIn[lNode.Successors[k].Index]),
          aEngine.StateOf(lNode));
    end;
    for j := 0 to aCFG.NodeCount - 1 do
      if aCFG.Reachable(aCFG.Nodes[j]) then
        aLattice.Replay(aCFG.Nodes[j], TObject(lIn[j]));
  finally
    for j := 0 to lIn.Count - 1 do
      aLattice.FreeState(TObject(lIn[j]));
    lIn.Free;
  end;
end;


{ Runs the free-state lattice over aBlock, filling in each slot's settled facts
  and recording every verdict the replay proposes. }
procedure RunFreeStateLattice(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; var aSlots: TFpSonarFreeSlotArray;
  out aCandidates: TFpSonarFreeCandidateArray);

var
  lCFG: TPasCFG;
  lLattice: TFpSonarFreeStateLattice;
  lEngine: TPasDataFlowEngine;

begin
  SetLength(aCandidates, 0);
  lCFG := TPasCFG.Create(aBlock);
  try
    lLattice := TFpSonarFreeStateLattice.Create(aResolver, lCFG, aSlots);
    try
      lEngine := TPasDataFlowEngine.Create;
      try
        lEngine.Run(lCFG, lLattice);
        lLattice.HarvestExit(lEngine.StateOf(lCFG.ExitNode));
        ReplayFreeStateNodes(lCFG, lEngine, lLattice);
        aSlots := lLattice.Slots;
        aCandidates := lLattice.Candidates;
      finally
        lEngine.Free;
      end;
    finally
      lLattice.Free;
    end;
  finally
    lCFG.Free;
  end;
end;


// Appends one finding to aFindings.
procedure AddFreeStateFinding(var aFindings: TFpSonarFreeStateFindingArray;
  aSite: TPasElement; const aName: string;
  aVerdict: TFpSonarFreeStateVerdict);

begin
  SetLength(aFindings, Length(aFindings) + 1);
  aFindings[High(aFindings)].Site := aSite;
  aFindings[High(aFindings)].Name := aName;
  aFindings[High(aFindings)].Verdict := aVerdict;
end;


// Appends the verdicts of one routine to aFindings, in statement order.
procedure EmitFreeStateVerdicts(const aStmts: TPasImplElementArray;
  const aSlots: TFpSonarFreeSlotArray;
  const aCandidates: TFpSonarFreeCandidateArray;
  var aFindings: TFpSonarFreeStateFindingArray);

  // True when a verdict was already emitted for the slot aSlot.
  function Emitted(const aList: array of integer; aSlot: integer): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(aList) do
      if aList[k] = aSlot then
        Exit(True);
  end;

var
  lEmitted: array of integer;
  lStmt: TPasImplElement;
  lOk: boolean;
  i, j, n: integer;

begin
  for j := 0 to High(aStmts) do
  begin
    lStmt := aStmts[j];
    SetLength(lEmitted, 0);
    for i := 0 to High(aCandidates) do
    begin
      if aCandidates[i].Site <> lStmt then
        Continue;
      n := aCandidates[i].Slot;
      if Emitted(lEmitted, n) then
        Continue;
      case aCandidates[i].Verdict of
        fsvFieldFreedNotNilled: lOk := aSlots[n].ReadWhileReleased;
        fsvUnpairedAllocation: lOk := (aSlots[n].Allocations = 1)
          and aSlots[n].ExitOwned;
      else
        lOk := True;
      end;
      if not lOk then
        Continue;
      AddFreeStateFinding(aFindings, lStmt, aSlots[n].Name,
        aCandidates[i].Verdict);
      SetLength(lEmitted, Length(lEmitted) + 1);
      lEmitted[High(lEmitted)] := n;
    end;
  end;
end;


// Appends the free-state verdicts of aRoutine to aFindings.
procedure AnalyzeRoutineFreeState(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; var aFindings: TFpSonarFreeStateFindingArray);

var
  lStmts: TPasImplElementArray;
  lSlots: TFpSonarFreeSlotArray;
  lCandidates: TFpSonarFreeCandidateArray;

begin
  if (aRoutine.Block = nil) or (aRoutine.Decl = nil)
    or (aRoutine.Decl.Body = nil) then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aRoutine.Block, lStmts);
  if not CollectFreeSlots(aResolver, aRoutine, lStmts, lSlots) then
    Exit;
  if Length(lSlots) = 0 then
    Exit;
  RunFreeStateLattice(aResolver, aRoutine.Block, lSlots, lCandidates);
  EmitFreeStateVerdicts(lStmts, lSlots, lCandidates, aFindings);
end;


function TFpSonarDataFlow.TryFreeStateFindings(
  out aFindings: TFpSonarFreeStateFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutineFreeState(FResolver, lRoutines[i], aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


// True when aBlock and every statement below it can be classified.
function RoutineIsAnswerable(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; const aStmts: TPasImplElementArray): boolean;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i: integer;

begin
  Result := aResolver.TryStatementAccess(aBlock, lReadNodes, lReadDecls,
    lWriteDecls);
  for i := 0 to High(aStmts) do
    if not aResolver.TryStatementAccess(aStmts[i], lReadNodes, lReadDecls,
      lWriteDecls) then
      Exit(False);
end;


// True when aDecl is storage a pairing may key a slot on.
function IsPairReceiverDecl(aDecl: TPasElement): boolean;

begin
  Result := (aDecl is TPasVariable) or (aDecl is TPasArgument);
end;


// One acquisition per statement of aStmts calling aPair's acquire method.
function CollectPairAcquisitions(aResolver: TFpSonarResolver;
  const aStmts: TPasImplElementArray;
  const aPair: TFpSonarPairSpec): TFpSonarAcquisitionArray;

var
  lDecl: TPasElement;
  i, n: integer;

begin
  SetLength(Result, 0);
  for i := 0 to High(aStmts) do
  begin
    lDecl := aResolver.ReferencedDecl(
      MethodCallReceiver(aStmts[i], aPair.Acquire));
    if not IsPairReceiverDecl(lDecl) then
      Continue;
    SetLength(Result, Length(Result) + 1);
    n := High(Result);
    Result[n].Site := aStmts[i];
    Result[n].Decl := lDecl;
    Result[n].Name := lDecl.Name;
    Result[n].TypeName := '';
  end;
end;


// Appends the unprotected pairings of the routine body aBlock to aFindings.
procedure AnalyzeRoutinePairs(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; const aPairs: TFpSonarPairSpecArray;
  var aFindings: TFpSonarPairFindingArray);

var
  lStmts: TPasImplElementArray;
  lAcqs: TFpSonarAcquisitionArray;
  lCands: TFpSonarPairFindingArray;
  lSpec: TFpSonarReleaseSpec;
  i, j, p, n: integer;

begin
  if aBlock = nil then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aBlock, lStmts);
  if not RoutineIsAnswerable(aResolver, aBlock, lStmts) then
    Exit;
  lSpec.Resolver := aResolver;
  SetLength(lCands, 0);
  for p := 0 to High(aPairs) do
  begin
    if (aPairs[p].Acquire = '') or (aPairs[p].Release = '') then
      Continue;
    lAcqs := CollectPairAcquisitions(aResolver, lStmts, aPairs[p]);
    lSpec.Method := aPairs[p].Release;
    for i := 0 to High(lAcqs) do
    begin
      if not HasFollowingRelease(lSpec, lStmts, lAcqs, i)
        or CoveredByFinally(lSpec, lStmts, lAcqs, i) then
        Continue;
      SetLength(lCands, Length(lCands) + 1);
      n := High(lCands);
      lCands[n].Site := lAcqs[i].Site;
      lCands[n].Name := lAcqs[i].Name;
      lCands[n].Release := aPairs[p].Release;
    end;
  end;

  // One finding per acquire statement, whichever pair matched it first.
  for j := 0 to High(lStmts) do
    for i := 0 to High(lCands) do
      if lCands[i].Site = lStmts[j] then
      begin
        SetLength(aFindings, Length(aFindings) + 1);
        aFindings[High(aFindings)] := lCands[i];
        Break;
      end;
end;


function TFpSonarDataFlow.TryPairFindings(const aPairs: TFpSonarPairSpecArray;
  out aFindings: TFpSonarPairFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutinePairs(FResolver, lRoutines[i].Block, aPairs, aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


type
  { One declaration of a routine with its SetLength sites: Site is the position
    of the first, Sites how many the routine holds. }
  TFpSonarFillSlot = record
    Decl: TPasElement;
    Name: string;
    Site: integer;
    Sites: integer;
  end;
  TFpSonarFillSlotArray = array of TFpSonarFillSlot;


// The declaration a SetLength statement resizes, nil for anything else.
function ResizedDecl(aResolver: TFpSonarResolver;
  aStmt: TPasImplElement): TPasElement;

var
  lCall: TPasExpr;

begin
  Result := nil;
  lCall := SimpleExpr(aStmt);
  if not (lCall is TParamsExpr)
    or (TParamsExpr(lCall).Kind <> pekFuncParams)
    or (Length(TParamsExpr(lCall).Params) < 2)
    or not IsNamedIdent(TParamsExpr(lCall).Value, cSetLengthName) then
    Exit;
  Result := aResolver.ReferencedDecl(TParamsExpr(lCall).Params[0]);
end;


// True when aNode is the operand an indexed access indexes.
function IsIndexedBase(aNode: TPasElement): boolean;

begin
  Result := (aNode.Parent is TParamsExpr)
    and (TParamsExpr(aNode.Parent).Kind = pekArrayParams)
    and (TParamsExpr(aNode.Parent).Value = aNode);
end;


// True when aNode sits in the left-hand side of an assignment.
function UnderAssignTarget(aNode: TPasElement): boolean;

var
  lChild: TPasElement;
  lEl: TPasElement;
  i: integer;

begin
  Result := False;
  lChild := aNode;
  lEl := aNode.Parent;
  i := 0;
  while (lEl <> nil) and (i < cMaxParentDepth) do
  begin
    if lEl is TPasImplAssign then
      Exit(TPasImplAssign(lEl).Left = lChild);
    lChild := lEl;
    lEl := lEl.Parent;
    Inc(i);
  end;
end;


{ True when aNode, or the indexed access over it, is the operand of an
  address-of. }
function UnderAddressOf(aNode: TPasElement): boolean;

var
  lEl: TPasElement;

begin
  lEl := aNode;
  if IsIndexedBase(lEl) then
    lEl := lEl.Parent;
  Result := (lEl.Parent is TUnaryExpr)
    and (TUnaryExpr(lEl.Parent).OpCode = eopAddress);
end;


// True when aDecl is a string variable of aBody the fill query may track.
function IsFillTrackable(aResolver: TFpSonarResolver; aDecl: TPasElement;
  aBody: TPasElement): boolean;

var
  lType: TFpSonarResolvedType;

begin
  Result := (aDecl <> nil) and (aDecl.ClassType = TPasVariable)
    and (TPasVariable(aDecl).AbsoluteExpr = nil)
    and (TPasVariable(aDecl).Expr = nil) and (aDecl.Parent = aBody)
    and aResolver.TryResolvedType(TPasVariable(aDecl).VarType, lType)
    and (lType.Kind = ltkString);
end;


// Appends the unfilled indexed reads of aRoutine to aFindings.
procedure AnalyzeRoutineFill(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; var aFindings: TFpSonarSetLengthFindingArray);

var
  lStmts: TPasImplElementArray;
  lReadNodes: array of TPasElementArray;
  lReadDecls: array of TPasElementArray;
  lWriteDecls: array of TPasElementArray;
  lSlots: TFpSonarFillSlotArray;
  lCands: TFpSonarSetLengthFindingArray;
  lDropped: TFPList;
  lDecl: TPasElement;
  lSite: TPasImplElement;
  lTouched: boolean;
  i, j, n: integer;

  // True when statement aAt names aDecl at all.
  function Touches(aAt: integer; aDecl: TPasElement): boolean;
  begin
    Result := Mentions(lReadDecls[aAt], aDecl)
      or Mentions(lWriteDecls[aAt], aDecl);
  end;

  // True when statement aAt passes aDecl to a call.
  function HandsOver(aAt: integer; aDecl: TPasElement): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(lReadNodes[aAt]) do
      if (lReadDecls[aAt][k] = aDecl)
        and InCallArguments(lReadNodes[aAt][k]) then
        Exit(True);
  end;

  { True when statement aAt reads aDecl through an index that is neither a
    store target, a call argument nor an address-of operand. }
  function IndexedRead(aAt: integer; aDecl: TPasElement): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := 0 to High(lReadNodes[aAt]) do
      if (lReadDecls[aAt][k] = aDecl) and IsIndexedBase(lReadNodes[aAt][k])
        and not UnderAssignTarget(lReadNodes[aAt][k])
        and not UnderAddressOf(lReadNodes[aAt][k]) then
        Exit(True);
  end;

  // True when statement aAt writes aDecl, an indexed store included.
  function Writes(aAt: integer; aDecl: TPasElement): boolean;
  var
    k: integer;
  begin
    Result := Mentions(lWriteDecls[aAt], aDecl);
    for k := 0 to High(lReadNodes[aAt]) do
      if (lReadDecls[aAt][k] = aDecl)
        and UnderAssignTarget(lReadNodes[aAt][k]) then
        Exit(True);
  end;

  // The first unfilled indexed read of aSlot, nil when the routine has none.
  function UnfilledRead(const aSlot: TFpSonarFillSlot): TPasImplElement;
  var
    k: integer;
  begin
    Result := nil;
    for k := aSlot.Site + 1 to High(lStmts) do
    begin
      if HandsOver(k, aSlot.Decl) then
        Exit;
      if IndexedRead(k, aSlot.Decl) then
        Exit(lStmts[k]);
      if Writes(k, aSlot.Decl) then
        Exit;
    end;
  end;

begin
  if (aRoutine.Block = nil) or (aRoutine.Decl = nil)
    or (aRoutine.Decl.Body = nil) then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aRoutine.Block, lStmts);
  if not RoutineIsAnswerable(aResolver, aRoutine.Block, lStmts) then
    Exit;

  lDropped := TFPList.Create;
  try
    if not ScanNestedRoutines(aResolver, aRoutine.Decl.Body, lDropped) then
      Exit;
    ScanAbsoluteAliases(aResolver, aRoutine.Decl.Body, lDropped);
    SetLength(lReadNodes, Length(lStmts));
    SetLength(lReadDecls, Length(lStmts));
    SetLength(lWriteDecls, Length(lStmts));
    SetLength(lSlots, 0);
    for i := 0 to High(lStmts) do
    begin
      aResolver.TryStatementAccess(lStmts[i], lReadNodes[i], lReadDecls[i],
        lWriteDecls[i]);
      for j := 0 to High(lReadNodes[i]) do
        if UnderAddressOf(lReadNodes[i][j]) then
          AddEscaped(lDropped, lReadDecls[i][j]);
      lDecl := ResizedDecl(aResolver, lStmts[i]);
      if not IsFillTrackable(aResolver, lDecl, aRoutine.Decl.Body) then
        Continue;
      n := 0;
      while (n <= High(lSlots)) and (lSlots[n].Decl <> lDecl) do
        Inc(n);
      if n > High(lSlots) then
      begin
        SetLength(lSlots, n + 1);
        lSlots[n].Decl := lDecl;
        lSlots[n].Name := lDecl.Name;
        lSlots[n].Site := i;
        lSlots[n].Sites := 0;
      end;
      Inc(lSlots[n].Sites);
    end;

    SetLength(lCands, 0);
    for n := 0 to High(lSlots) do
    begin
      if (lSlots[n].Sites <> 1) or (lDropped.IndexOf(lSlots[n].Decl) >= 0) then
        Continue;
      lTouched := False;
      for i := 0 to lSlots[n].Site - 1 do
        lTouched := lTouched or Touches(i, lSlots[n].Decl);
      if lTouched then
        Continue;
      lSite := UnfilledRead(lSlots[n]);
      if lSite = nil then
        Continue;
      SetLength(lCands, Length(lCands) + 1);
      lCands[High(lCands)].Site := lSite;
      lCands[High(lCands)].Name := lSlots[n].Name;
    end;
  finally
    lDropped.Free;
  end;

  for j := 0 to High(lStmts) do
    for n := 0 to High(lCands) do
      if lCands[n].Site = lStmts[j] then
      begin
        SetLength(aFindings, Length(aFindings) + 1);
        aFindings[High(aFindings)] := lCands[n];
      end;
end;


function TFpSonarDataFlow.TrySetLengthFindings(
  out aFindings: TFpSonarSetLengthFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutineFill(FResolver, lRoutines[i], aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


{ The name a call statement names its callee by, '' for anything else and for
  a callee reached through a qualifier. }
function CalleeName(aStmt: TPasImplElement): string;

var
  lExpr: TPasExpr;

begin
  Result := '';
  lExpr := SimpleExpr(aStmt);
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
    lExpr := TParamsExpr(lExpr).Value;
  if (lExpr is TPrimitiveExpr) and (TPrimitiveExpr(lExpr).Kind = pekIdent) then
    Result := TPrimitiveExpr(lExpr).Value;
end;


// True when aName is one of the routines the I/O question is about.
function IsIORoutine(const aName: string): boolean;

var
  i: integer;

begin
  Result := False;
  for i := Low(cIORoutines) to High(cIORoutines) do
    if SameText(cIORoutines[i], aName) then
      Exit(True);
end;


// True when one of aChecks sits at or below aStmt.
function ChecksBelow(const aChecks: TPasElementArray;
  aStmt: TPasImplElement): boolean;

var
  lWalk: TPasElement;
  i, lDepth: integer;

begin
  Result := False;
  for i := 0 to High(aChecks) do
  begin
    lWalk := aChecks[i];
    lDepth := 0;
    while (lWalk <> nil) and (lDepth < cMaxParentDepth) do
    begin
      if lWalk = aStmt then
        Exit(True);
      lWalk := lWalk.Parent;
      Inc(lDepth);
    end;
  end;
end;


// True when aNode is the member half of a qualified reference.
function IsQualifiedMember(aNode: TPasElement): boolean;

begin
  Result := (aNode.Parent is TBinaryExpr)
    and (TBinaryExpr(aNode.Parent).OpCode = eopSubIdent)
    and (TBinaryExpr(aNode.Parent).Right = aNode);
end;


{ True when aStmt calls a routine other than IOResult that is either reached
  through a qualifier or absent from the I/O list. }
function CallsPossibleChecker(aResolver: TFpSonarResolver;
  aStmt: TPasImplElement): boolean;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i: integer;

begin
  Result := False;
  if not aResolver.TryStatementAccess(aStmt, lReadNodes, lReadDecls,
    lWriteDecls) then
    Exit;
  for i := 0 to High(lReadDecls) do
    if (lReadDecls[i] is TPasProcedure)
      and not SameText(lReadDecls[i].Name, cIOResultName)
      and (IsQualifiedMember(lReadNodes[i])
      or not IsIORoutine(lReadDecls[i].Name)) then
      Exit(True);
end;


// Appends the unchecked I/O calls of the routine body aBlock to aFindings.
procedure AnalyzeRoutineIOChecks(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; const aChecks: TPasElementArray;
  var aFindings: TFpSonarIOCheckFindingArray);

var
  lStmts: TPasImplElementArray;
  lNames: TFpSonarStringArray;
  lIsIO: array of boolean;
  lSatisfied: boolean;
  i, j, k: integer;

begin
  if aBlock = nil then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aBlock, lStmts);
  if not RoutineIsAnswerable(aResolver, aBlock, lStmts) then
    Exit;
  SetLength(lNames, Length(lStmts));
  SetLength(lIsIO, Length(lStmts));
  for i := 0 to High(lStmts) do
  begin
    lNames[i] := CalleeName(lStmts[i]);
    lIsIO[i] := IsIORoutine(lNames[i]);
  end;

  for i := 0 to High(lStmts) do
  begin
    if not lIsIO[i] then
      Continue;
    k := i + 1;
    while (k <= High(lStmts)) and not lIsIO[k] do
      Inc(k);
    if k > High(lStmts) then
      Continue;
    lSatisfied := False;
    for j := i + 1 to k - 1 do
      if ChecksBelow(aChecks, lStmts[j])
        or CallsPossibleChecker(aResolver, lStmts[j]) then
        lSatisfied := True;
    if lSatisfied then
      Continue;
    SetLength(aFindings, Length(aFindings) + 1);
    aFindings[High(aFindings)].Site := lStmts[i];
    aFindings[High(aFindings)].Name := lNames[i];
    aFindings[High(aFindings)].Next := lStmts[k];
  end;
end;


function TFpSonarDataFlow.TryIOCheckFindings(
  out aFindings: TFpSonarIOCheckFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  lChecks: TPasElementArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    if not FResolver.TryIdentifierNameSites(lNodes, lNames) then
      Exit;
    SetLength(lChecks, 0);
    for i := 0 to High(lNodes) do
      if SameText(lNames[i], cIOResultName) then
      begin
        SetLength(lChecks, Length(lChecks) + 1);
        lChecks[High(lChecks)] := lNodes[i];
      end;
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutineIOChecks(FResolver, lRoutines[i].Block, lChecks,
        aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;


type
  { Bits one tracked critical section carries on the paths reaching a node. }
  TFpSonarLockBit = (blUninitialized, blInitialized, blHeld, blFree);
  TFpSonarLockBits = set of TFpSonarLockBit;

  { Bit set per tracked section of one routine. }
  TFpSonarLockState = class(TObject)
  private
    FBits: array of TFpSonarLockBits;
    function GetCount: integer;
    function GetBits(aIndex: integer): TFpSonarLockBits;
    procedure SetBits(aIndex: integer; aValue: TFpSonarLockBits);
  public
    // Creates a state holding aCount empty sections.
    constructor Create(aCount: integer);
    // How many sections the state holds a value for.
    property Count: integer read GetCount;
    // Bits of the section at aIndex.
    property Bits[aIndex: integer]: TFpSonarLockBits read GetBits
      write SetBits; default;
  end;

  { One tracked critical section of a routine. }
  TFpSonarLockSlot = record
    Decl: TPasElement;
    Name: string;
    IsLocal: boolean;
  end;
  TFpSonarLockSlotArray = array of TFpSonarLockSlot;

  // What a recognised statement does to the critical section it names.
  TFpSonarLockAction = (laNone, laInit, laEnter, laRelease, laSync);

  { Forward per-section bit lattice over one routine's tracked sections; Merge
    is the bit union over a four-bit set. }
  TFpSonarConcurrencyLattice = class(TPasDataFlowLattice)
  private
    FResolver: TFpSonarResolver;
    FCFG: TPasCFG;
    FSlots: TFpSonarLockSlotArray;
    FThreadRoutine: boolean;
    FLockOpaque: boolean;
    FCandidates: TFpSonarConcurrencyFindingArray;
    FReporting: boolean;
    procedure AddCandidate(aSite: TPasElement; const aName, aCallee: string;
      aVerdict: TFpSonarConcurrencyVerdict);
    procedure ReportStatement(aStmt: TPasImplElement;
      aAction: TFpSonarLockAction; aDecl: TPasElement; const aCallee: string;
      aState: TFpSonarLockState);
    procedure StepStatement(aStmt: TPasImplElement; aState: TFpSonarLockState);
  public
    { Binds the lattice to the graph and the tracked sections of one routine.
      aLockOpaque withholds the absence claim of cvGlobalWrite. }
    constructor Create(aResolver: TFpSonarResolver; aCFG: TPasCFG;
      const aSlots: TFpSonarLockSlotArray; aThreadRoutine: boolean;
      aLockOpaque: boolean);
    // The engine propagates the lock state along the edges, so forward.
    function Direction: TPasDataFlowDirection; override;
    // A state in which no section has been reached.
    function CreateState: TObject; override;
    // An independent copy of aState.
    function CopyState(aState: TObject): TObject; override;
    // Releases a state obtained from CreateState or CopyState.
    procedure FreeState(aState: TObject); override;
    // Unions the bits of aSource into those of aTarget, section by section.
    procedure Merge(aTarget: TObject; aSource: TObject); override;
    // Applies aNode's section calls to aState, in source order.
    procedure Transfer(aNode: TPasCFGNode; aState: TObject); override;
    // True when both states hold the same bits for every section.
    function SameState(aLeft: TObject; aRight: TObject): Boolean; override;
    { Replays aNode over its in-state aState with reporting on, recording every
      verdict the state at each of its statements proposes. }
    procedure Replay(aNode: TPasCFGNode; aState: TObject);
    // The verdicts the replay proposed, in visit order.
    property Candidates: TFpSonarConcurrencyFindingArray read FCandidates;
  end;


{ TFpSonarLockState }

constructor TFpSonarLockState.Create(aCount: integer);

begin
  inherited Create;
  SetLength(FBits, aCount);
end;


function TFpSonarLockState.GetCount: integer;

begin
  Result := Length(FBits);
end;


function TFpSonarLockState.GetBits(aIndex: integer): TFpSonarLockBits;

begin
  Result := FBits[aIndex];
end;


procedure TFpSonarLockState.SetBits(aIndex: integer;
  aValue: TFpSonarLockBits);

begin
  FBits[aIndex] := aValue;
end;


// The argument list of a call statement, empty for anything else.
function CallArguments(aStmt: TPasImplElement): TPasExprArray;

var
  lExpr: TPasExpr;

begin
  SetLength(Result, 0);
  lExpr := SimpleExpr(aStmt);
  if (lExpr is TParamsExpr) and (TParamsExpr(lExpr).Kind = pekFuncParams) then
    Result := TParamsExpr(lExpr).Params;
end;


// True when aName is one of the routines that run a callback on the main thread.
function IsMainThreadRoutine(const aName: string): boolean;

var
  i: integer;

begin
  Result := False;
  for i := Low(cMainThreadRoutines) to High(cMainThreadRoutines) do
    if SameText(cMainThreadRoutines[i], aName) then
      Exit(True);
end;


// What aStmt does to the critical section it names, and the routine it calls.
function ClassifyLockSite(aResolver: TFpSonarResolver; aStmt: TPasImplElement;
  out aDecl: TPasElement; out aCallee: string): TFpSonarLockAction;

var
  lArgs: TPasExprArray;
  lName: string;

begin
  Result := laNone;
  aDecl := nil;
  aCallee := '';
  lName := CalleeName(aStmt);
  if lName = '' then
    Exit;
  if IsMainThreadRoutine(lName) then
  begin
    aCallee := lName;
    Exit(laSync);
  end;
  if SameText(lName, cInitSectionName) then
    Result := laInit
  else if SameText(lName, cEnterSectionName) then
    Result := laEnter
  else if SameText(lName, cLeaveSectionName)
    or SameText(lName, cDoneSectionName) then
    Result := laRelease
  else
    Exit;
  lArgs := CallArguments(aStmt);
  if Length(lArgs) > 0 then
    aDecl := aResolver.ReferencedDecl(lArgs[0]);
end;


// True when aDecl is a variable whose type is written TRTLCriticalSection.
function IsSectionDecl(aResolver: TFpSonarResolver;
  aDecl: TPasElement): boolean;

var
  lType: TFpSonarResolvedType;

begin
  Result := (aDecl is TPasVariable)
    and aResolver.TryResolvedType(TPasVariable(aDecl).VarType, lType)
    and (SameText(lType.TypeName, cSectionTypeName)
    or SameText(lType.NamedTypeName, cSectionTypeName));
end;


// The slot of aDecl in aSlots, -1 when it has none.
function LockSlotIndex(const aSlots: TFpSonarLockSlotArray;
  aDecl: TPasElement): integer;

var
  i: integer;

begin
  Result := -1;
  if aDecl = nil then
    Exit;
  for i := 0 to High(aSlots) do
    if aSlots[i].Decl = aDecl then
      Exit(i);
end;


// True when aState leaves at least one section held.
function AnySectionHeld(aState: TFpSonarLockState): boolean;

var
  i: integer;

begin
  Result := False;
  for i := 0 to aState.Count - 1 do
    if blHeld in aState[i] then
      Exit(True);
end;


{ TFpSonarConcurrencyLattice }

constructor TFpSonarConcurrencyLattice.Create(aResolver: TFpSonarResolver;
  aCFG: TPasCFG; const aSlots: TFpSonarLockSlotArray;
  aThreadRoutine: boolean; aLockOpaque: boolean);

begin
  inherited Create;
  FResolver := aResolver;
  FCFG := aCFG;
  FSlots := aSlots;
  FThreadRoutine := aThreadRoutine;
  FLockOpaque := aLockOpaque;
end;


function TFpSonarConcurrencyLattice.Direction: TPasDataFlowDirection;

begin
  Result := dfdForward;
end;


function TFpSonarConcurrencyLattice.CreateState: TObject;

begin
  Result := TFpSonarLockState.Create(Length(FSlots));
end;


function TFpSonarConcurrencyLattice.CopyState(aState: TObject): TObject;

var
  lCopy: TFpSonarLockState;
  i: integer;

begin
  lCopy := TFpSonarLockState.Create(Length(FSlots));
  for i := 0 to lCopy.Count - 1 do
    lCopy[i] := TFpSonarLockState(aState)[i];
  Result := lCopy;
end;


procedure TFpSonarConcurrencyLattice.FreeState(aState: TObject);

begin
  aState.Free;
end;


procedure TFpSonarConcurrencyLattice.Merge(aTarget: TObject;
  aSource: TObject);

var
  lTarget: TFpSonarLockState;
  lSource: TFpSonarLockState;
  i: integer;

begin
  lTarget := TFpSonarLockState(aTarget);
  lSource := TFpSonarLockState(aSource);
  for i := 0 to lTarget.Count - 1 do
    lTarget[i] := lTarget[i] + lSource[i];
end;


procedure TFpSonarConcurrencyLattice.AddCandidate(aSite: TPasElement;
  const aName, aCallee: string; aVerdict: TFpSonarConcurrencyVerdict);

var
  i: integer;

begin
  for i := 0 to High(FCandidates) do
    if (FCandidates[i].Site = aSite) and (FCandidates[i].Name = aName)
      and (FCandidates[i].Verdict = aVerdict) then
      Exit;
  SetLength(FCandidates, Length(FCandidates) + 1);
  FCandidates[High(FCandidates)].Site := aSite;
  FCandidates[High(FCandidates)].Name := aName;
  FCandidates[High(FCandidates)].Callee := aCallee;
  FCandidates[High(FCandidates)].Verdict := aVerdict;
end;


procedure TFpSonarConcurrencyLattice.ReportStatement(aStmt: TPasImplElement;
  aAction: TFpSonarLockAction; aDecl: TPasElement; const aCallee: string;
  aState: TFpSonarLockState);

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  i, n: integer;

begin
  n := LockSlotIndex(FSlots, aDecl);
  if (aAction in [laEnter, laRelease]) and (n >= 0)
    and (blUninitialized in aState[n]) then
    AddCandidate(aStmt, FSlots[n].Name, '', cvSectionNotInitialized);
  if aAction = laSync then
    for i := 0 to High(FSlots) do
      if blHeld in aState[i] then
      begin
        AddCandidate(aStmt, FSlots[i].Name, aCallee, cvSyncWithLock);
        Break;
      end;
  // A lock or dispatch call writes its own operand: the lock, not shared state.
  if (aAction <> laNone) or not FThreadRoutine or FLockOpaque
    or AnySectionHeld(aState) then
    Exit;
  if not FResolver.TryStatementAccess(aStmt, lReadNodes, lReadDecls,
    lWriteDecls) then
    Exit;
  for i := 0 to High(lWriteDecls) do
    if lWriteDecls[i].Parent is TPasSection then
      AddCandidate(aStmt, lWriteDecls[i].Name, '', cvGlobalWrite);
end;


procedure TFpSonarConcurrencyLattice.StepStatement(aStmt: TPasImplElement;
  aState: TFpSonarLockState);

var
  lAction: TFpSonarLockAction;
  lDecl: TPasElement;
  lCallee: string;
  n: integer;

begin
  lAction := ClassifyLockSite(FResolver, aStmt, lDecl, lCallee);
  if FReporting then
    ReportStatement(aStmt, lAction, lDecl, lCallee, aState);
  n := LockSlotIndex(FSlots, lDecl);
  if n < 0 then
    Exit;
  case lAction of
    laInit: aState[n] := aState[n] - [blUninitialized] + [blInitialized];
    laEnter: aState[n] := aState[n] - [blFree] + [blHeld];
    laRelease: aState[n] := aState[n] - [blHeld] + [blFree];
  end;
end;


procedure TFpSonarConcurrencyLattice.Transfer(aNode: TPasCFGNode;
  aState: TObject);

var
  lState: TFpSonarLockState;
  i, j: integer;

begin
  // The engine merges an unreachable node into its successors regardless.
  if not FCFG.Reachable(aNode) then
    Exit;
  lState := TFpSonarLockState(aState);
  // The entry node carries the boundary state the engine has no slot for.
  if aNode = FCFG.EntryNode then
    for i := 0 to High(FSlots) do
      if FSlots[i].IsLocal then
        lState[i] := lState[i] + [blUninitialized, blFree]
      else
        lState[i] := lState[i] + [blInitialized, blFree];
  for j := 0 to aNode.StatementCount - 1 do
    StepStatement(aNode.Statements[j], lState);
end;


function TFpSonarConcurrencyLattice.SameState(aLeft: TObject;
  aRight: TObject): Boolean;

var
  i: integer;

begin
  for i := 0 to TFpSonarLockState(aLeft).Count - 1 do
    if TFpSonarLockState(aLeft)[i] <> TFpSonarLockState(aRight)[i] then
      Exit(False);
  Result := True;
end;


procedure TFpSonarConcurrencyLattice.Replay(aNode: TPasCFGNode;
  aState: TObject);

begin
  FReporting := True;
  try
    Transfer(aNode, aState);
  finally
    FReporting := False;
  end;
end;


// Appends a slot for the section aDecl, or leaves the one it already has.
procedure AddLockSlot(var aSlots: TFpSonarLockSlotArray; aDecl: TPasElement;
  aBody: TPasElement);

var
  n: integer;

begin
  if LockSlotIndex(aSlots, aDecl) >= 0 then
    Exit;
  SetLength(aSlots, Length(aSlots) + 1);
  n := High(aSlots);
  aSlots[n].Decl := aDecl;
  aSlots[n].Name := aDecl.Name;
  aSlots[n].IsLocal := aDecl.Parent = aBody;
end;


{ Fills aSlots with one slot per critical section named by a section call of
  aStmts, minus the ones that escape. aOpaque reports a lock call left without
  a slot. False when a statement cannot be classified, which takes the whole
  routine out. }
function CollectLockSlots(aResolver: TFpSonarResolver;
  const aRoutine: TAstRoutine; const aStmts: TPasImplElementArray;
  out aSlots: TFpSonarLockSlotArray; out aOpaque: boolean): boolean;

var
  lReadNodes: TPasElementArray;
  lReadDecls: TPasElementArray;
  lWriteDecls: TPasElementArray;
  lDropped: TFPList;
  lAction: TFpSonarLockAction;
  lDecl: TPasElement;
  lCallee: string;
  i, j, n: integer;

begin
  SetLength(aSlots, 0);
  aOpaque := False;
  Result := False;
  lDropped := TFPList.Create;
  try
    if not ScanNestedRoutines(aResolver, aRoutine.Decl.Body, lDropped) then
      Exit;
    ScanAbsoluteAliases(aResolver, aRoutine.Decl.Body, lDropped);
    for i := 0 to High(aStmts) do
    begin
      if not aResolver.TryStatementAccess(aStmts[i], lReadNodes, lReadDecls,
        lWriteDecls) then
        Exit;
      lAction := ClassifyLockSite(aResolver, aStmts[i], lDecl, lCallee);
      for j := 0 to High(lReadNodes) do
        if ((lReadNodes[j].Parent is TUnaryExpr)
          and (TUnaryExpr(lReadNodes[j].Parent).OpCode = eopAddress))
          or ForeignQualifier(lReadNodes[j])
          or (not (lAction in [laInit, laEnter, laRelease])
          and InCallArguments(lReadNodes[j])) then
          AddEscaped(lDropped, lReadDecls[j]);
      // An absolute alias is not its own section; its target is the slot.
      if (lAction in [laInit, laEnter, laRelease])
        and IsSectionDecl(aResolver, lDecl)
        and (TPasVariable(lDecl).AbsoluteExpr = nil) then
        AddLockSlot(aSlots, lDecl, aRoutine.Decl.Body);
    end;
    n := 0;
    for i := 0 to High(aSlots) do
      if lDropped.IndexOf(aSlots[i].Decl) < 0 then
      begin
        aSlots[n] := aSlots[i];
        Inc(n);
      end;
    SetLength(aSlots, n);
    for i := 0 to High(aStmts) do
      if (ClassifyLockSite(aResolver, aStmts[i], lDecl, lCallee)
        in [laEnter, laRelease]) and (LockSlotIndex(aSlots, lDecl) < 0) then
        aOpaque := True;
    Result := True;
  finally
    lDropped.Free;
  end;
end;


// The part of aName after its last dot.
function LastNamePart(const aName: string): string;

begin
  Result := Copy(aName, LastDelimiter('.', aName) + 1, Length(aName));
end;


// The part of aName before its last dot, '' when it carries none.
function QualifierPart(const aName: string): string;

begin
  Result := Copy(aName, 1, LastDelimiter('.', aName) - 1);
end;


{ The class aDecl is a method of: its parent when it is declared in the class,
  otherwise the type of aModule its dotted name qualifies. }
function OwnerClass(aModule: TPasModule; aDecl: TPasProcedure): TPasClassType;

var
  lTypes: TPasTypeArray;
  lName: string;
  i: integer;

begin
  Result := nil;
  if aDecl.Parent is TPasClassType then
    Exit(TPasClassType(aDecl.Parent));
  lName := QualifierPart(aDecl.Name);
  if lName = '' then
    Exit;
  lTypes := EnumerateTypes(aModule);
  for i := 0 to High(lTypes) do
    if (lTypes[i] is TPasClassType) and SameText(lTypes[i].Name, lName) then
      Exit(TPasClassType(lTypes[i]));
end;


{ True when aDecl is the Execute body of a class whose ancestor chain, by
  written name, reaches TThread. }
function IsThreadRoutine(aModule: TPasModule; aDecl: TPasProcedure): boolean;

var
  lClass: TPasClassType;
  lAncestor: TPasType;
  i: integer;

begin
  Result := False;
  if not SameText(LastNamePart(aDecl.Name), cThreadRoutineName) then
    Exit;
  lClass := OwnerClass(aModule, aDecl);
  i := 0;
  while (lClass <> nil) and (i < cMaxParentDepth) do
  begin
    if SameText(lClass.Name, cThreadClassName) then
      Exit(True);
    lAncestor := lClass.AncestorType;
    if lAncestor = nil then
      Exit;
    if not (lAncestor is TPasClassType) then
      Exit(SameText(lAncestor.Name, cThreadClassName));
    lClass := TPasClassType(lAncestor);
    Inc(i);
  end;
end;


{ Rebuilds every node's in-state by pushing each out-state into its successors,
  then replays the node for the verdicts. }
procedure ReplayConcurrencyNodes(aCFG: TPasCFG; aEngine: TPasDataFlowEngine;
  aLattice: TFpSonarConcurrencyLattice);

var
  lIn: TFPList;
  lNode: TPasCFGNode;
  j, k: integer;

begin
  lIn := TFPList.Create;
  try
    for j := 0 to aCFG.NodeCount - 1 do
      lIn.Add(aLattice.CreateState);
    for j := 0 to aCFG.NodeCount - 1 do
    begin
      lNode := aCFG.Nodes[j];
      if aEngine.StateOf(lNode) = nil then
        Continue;
      for k := 0 to lNode.SuccessorCount - 1 do
        aLattice.Merge(TObject(lIn[lNode.Successors[k].Index]),
          aEngine.StateOf(lNode));
    end;
    for j := 0 to aCFG.NodeCount - 1 do
      if aCFG.Reachable(aCFG.Nodes[j]) then
        aLattice.Replay(aCFG.Nodes[j], TObject(lIn[j]));
  finally
    for j := 0 to lIn.Count - 1 do
      aLattice.FreeState(TObject(lIn[j]));
    lIn.Free;
  end;
end;


{ Runs the concurrency lattice over aBlock, recording every verdict the replay
  proposes. }
procedure RunConcurrencyLattice(aResolver: TFpSonarResolver;
  aBlock: TPasImplBlock; const aSlots: TFpSonarLockSlotArray;
  aThreadRoutine: boolean; aLockOpaque: boolean;
  out aCandidates: TFpSonarConcurrencyFindingArray);

var
  lCFG: TPasCFG;
  lLattice: TFpSonarConcurrencyLattice;
  lEngine: TPasDataFlowEngine;

begin
  SetLength(aCandidates, 0);
  lCFG := TPasCFG.Create(aBlock);
  try
    lLattice := TFpSonarConcurrencyLattice.Create(aResolver, lCFG, aSlots,
      aThreadRoutine, aLockOpaque);
    try
      lEngine := TPasDataFlowEngine.Create;
      try
        lEngine.Run(lCFG, lLattice);
        ReplayConcurrencyNodes(lCFG, lEngine, lLattice);
        aCandidates := lLattice.Candidates;
      finally
        lEngine.Free;
      end;
    finally
      lLattice.Free;
    end;
  finally
    lCFG.Free;
  end;
end;


// Appends the concurrency verdicts of aRoutine to aFindings, in statement order.
procedure AnalyzeRoutineConcurrency(aResolver: TFpSonarResolver;
  aModule: TPasModule; const aRoutine: TAstRoutine;
  var aFindings: TFpSonarConcurrencyFindingArray);

var
  lStmts: TPasImplElementArray;
  lSlots: TFpSonarLockSlotArray;
  lCands: TFpSonarConcurrencyFindingArray;
  lThread: boolean;
  lOpaque: boolean;
  i, j: integer;

begin
  if (aRoutine.Block = nil) or (aRoutine.Decl = nil)
    or (aRoutine.Decl.Body = nil) then
    Exit;
  SetLength(lStmts, 0);
  CollectStatements(aRoutine.Block, lStmts);
  if not RoutineIsAnswerable(aResolver, aRoutine.Block, lStmts) then
    Exit;
  if not CollectLockSlots(aResolver, aRoutine, lStmts, lSlots, lOpaque) then
    Exit;
  lThread := IsThreadRoutine(aModule, aRoutine.Decl);
  if (Length(lSlots) = 0) and not lThread then
    Exit;
  RunConcurrencyLattice(aResolver, aRoutine.Block, lSlots, lThread, lOpaque,
    lCands);
  for j := 0 to High(lStmts) do
    for i := 0 to High(lCands) do
      if lCands[i].Site = lStmts[j] then
      begin
        SetLength(aFindings, Length(aFindings) + 1);
        aFindings[High(aFindings)] := lCands[i];
      end;
end;


function TFpSonarDataFlow.TryConcurrencyFindings(
  out aFindings: TFpSonarConcurrencyFindingArray): boolean;

var
  lRoutines: TAstRoutineArray;
  i: integer;

begin
  aFindings := nil;
  Result := False;
  if (FResolver = nil) or (not FResolver.Succeeded)
    or (FResolver.Engine = nil) or (FResolver.ResolvedModule = nil) then
    Exit;

  try
    lRoutines := EnumerateRoutines(FResolver.ResolvedModule);
    for i := 0 to High(lRoutines) do
      AnalyzeRoutineConcurrency(FResolver, FResolver.ResolvedModule,
        lRoutines[i], aFindings);
    Result := True;
  except
    on E: Exception do
    begin
      aFindings := nil;
      FUsable := False;
      Result := False;
    end;
  end;
end;

end.

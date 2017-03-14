{
    This file is part of the Free Component Library

    Pascal parse tree classes
    Copyright (c) 2017  Mattias Gaertner, mattias@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
Abstract:
  After running TPasResolver, run this to
  - create a list of used declararion, either in a module or a whole program.
  - emit hints about unused declarations
  - and warnings about uninitialized variables.

Working:
- mark used elements of a module, starting from all accessible elements

ToDo:
- unary
- Note: (5025) Local variable "i" not used
- hint: local proc not used
- hint: private member not used
- check program/library: method for whole program checking
- Call Override: e.g. A.Proc, mark only overrides of descendants of A
- TPasArgument: compute the effective Access
- calls: use the effective Access of arguments
}
unit PasUseAnalyzer;

{$mode objfpc}{$H+}{$inline on}

interface

uses
  Classes, SysUtils, AVL_Tree, PasResolver, PasTree, PScanner;

const
  nPAUnitNotUsed = 5023;
  sPAUnitNotUsed = 'Unit "%s" not used in %s';
  nPAParameterNotUsed = 5024;
  sPAParameterNotUsed = 'Parameter "%s" not used';
  nPALocalVariableNotUsed = 5025;
  sPALocalVariableNotUsed = 'Local variable "%s" not used';
  nPAValueParameterIsAssignedButNeverUsed = 5026;
  sPAValueParameterIsAssignedButNeverUsed = 'Value parameter "$1" is assigned but never used';
  nPALocalVariableIsAssignedButNeverUsed = 5027;
  sPALocalVariableIsAssignedButNeverUsed = 'Local variable "$1" is assigned but never used';
  nPALocalXYNotUsed = 5028;
  sPALocalXYNotUsed = 'Local %s "%s" not used';
  nPAPrivateFieldIsNeverUsed = 5029;
  sPAPrivateFieldIsNeverUsed = 'Private field "$1" is never used';
  nPAPrivateFieldIsAssignedButNeverUsed = 5030;
  sPAPrivateFieldIsAssignedButNeverUsed = 'Private field "$1" is assigned but never used';
  nPAPrivateMethodIsNeverUsed = 5031;
  sPAPrivateMethodIsNeverUsed = 'Private method "$1" is never used';
  //nPAFunctionResultDoesNotSeemToBeSet = 5033;
  //sPAFunctionResultDoesNotSeemToBeSet  = 'Function result does not seem to be set';
  nPAPrivateTypeXNeverUsed = 5071;
  sPAPrivateTypeXNeverUsed = 'Private type "$1" never used';
  nPAPrivateConstXNeverUsed = 5072;
  sPAPrivateConstXNeverUsed = 'Private const "$1" never used';
  nPAPrivatePropertyXNeverUsed = 5073;
  sPAPrivatePropertyXNeverUsed = 'Private property "$1" never used';
  //nPAUnreachableCode = 6018;
  //sPAUnreachableCode = 'unreachable code';

type
  EPasAnalysis = class(EPasResolve);

  TPAMessage = class
  public
    Id: int64;
    MsgType: TMessageType;
    MsgNumber: integer;
    Fmt: String;
    Args: TMessageArgs;
    PosEl: TPasElement;
    Filename: string;
    Row, Col: integer;
  end;

  TPAMessageEvent = procedure(Sender: TObject; Msg: TPAMessage) of object;

  TPAIdentifierAccess = (
    paiaNone,
    paiaRead,
    paiaWrite,
    paiaReadWrite,
    paiaWriteRead
    );

  { TPAElement }

  TPAElement = class
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    Access: TPAIdentifierAccess;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;
  TPAElementClass = class of TPAElement;

  { TPAOverrideList }

  TPAOverrideList = class
  private
    FElement: TPasElement;
    FOverrides: TFPList; // list of TPasElement
    function GetOverrides(Index: integer): TPasElement; inline;
    procedure SetElement(AValue: TPasElement);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(OverrideEl: TPasElement);
    property Element: TPasElement read FElement write SetElement;
    function Count: integer;
    function IndexOf(OverrideEl: TPasElement): integer; inline;
    property Overrides[Index: integer]: TPasElement read GetOverrides; default;
  end;

  TPasAnalyzerOption = (
    paoAlwaysUsePublished, // when a class is used, use all published members as well
    paoOnlyExports // default: use all class members accessible from outside (protected, but not private)
    );
  TPasAnalyzerOptions = set of TPasAnalyzerOption;

  { TPasAnalyzer }

  TPasAnalyzer = class
  private
    FOnMessage: TPAMessageEvent;
    FOptions: TPasAnalyzerOptions;
    FOverrideLists: TAVLTree; // tree of TPAOverrideList sorted for Element
    FResolver: TPasResolver;
    FScopeModule: TPasModule;
    FUsedElements: TAVLTree; // tree of TPAElement sorted for Element
    function AddOverride(OverriddenEl, OverrideEl: TPasElement): boolean;
    function FindOverrideNode(El: TPasElement): TAVLTreeNode;
    function FindOverrideList(El: TPasElement): TPAOverrideList;
    procedure SetOptions(AValue: TPasAnalyzerOptions);
    procedure UpdateAccess(IsWrite: Boolean; IsRead: Boolean; Usage: TPAElement);
  protected
    procedure RaiseInconsistency(const Id: int64; Msg: string);
    procedure RaiseNotSupported(const Id: int64; El: TPasElement; const Msg: string = '');
    // mark used elements
    function Add(El: TPasElement; CheckDuplicate: boolean = true;
      aClass: TPAElementClass = nil): TPAElement;
    function FindNode(El: TPasElement): TAVLTreeNode; inline;
    function FindPAElement(El: TPasElement): TPAElement; inline;
    procedure CreateTree; virtual;
    function MarkElement(El: TPasElement; aClass: TPAElementClass = nil): boolean; // true if new
    procedure UseElement(El: TPasElement; Access: TResolvedRefAccess;
      UseFull: boolean); virtual;
    procedure UseSection(Section: TPasSection); virtual;
    procedure UseDeclarations(El: TPasDeclarations; OnlyExports: boolean); virtual;
    procedure UseImplBlock(Block: TPasImplBlock; Mark: boolean); virtual;
    procedure UseImplElement(El: TPasImplElement); virtual;
    procedure UseExpr(El: TPasExpr); virtual;
    procedure UseProcedure(Proc: TPasProcedure); virtual;
    procedure UseProcedureType(ProcType: TPasProcedureType); virtual;
    procedure UseType(El: TPasType); virtual;
    procedure UseArrayType(El: TPasArrayType; Mark: boolean); virtual;
    procedure UseRecordType(El: TPasRecordType; Mark: boolean); virtual;
    procedure UseClassType(El: TPasClassType; Mark: boolean); virtual;
    procedure UseVariable(El: TPasVariable; Access: TResolvedRefAccess;
      UseFull: boolean); virtual;
    procedure UseArgument(El: TPasArgument; Access: TResolvedRefAccess); virtual;
    procedure UseResultElement(El: TPasResultElement; Access: TResolvedRefAccess); virtual;
    // create hints for a unit, program or library
    procedure EmitElementHints(El: TPasElement); virtual;
    procedure EmitModuleHints(aModule: TPasModule); virtual;
    procedure EmitSectionHints(Section: TPasSection); virtual;
    procedure EmitDeclarationsHints(El: TPasDeclarations); virtual;
    procedure EmitTypeHints(El: TPasType); virtual;
    procedure EmitVariableHints(El: TPasVariable); virtual;
    procedure EmitProcedureHints(El: TPasProcedure); virtual;
    // utility
    function IsModuleInternal(El: TPasElement): boolean;
    function IsExport(El: TPasElement): boolean;
    procedure EmitMessage(const Id: int64; const MsgType: TMessageType;
      MsgNumber: integer; Fmt: String; const Args: array of const; PosEl: TPasElement);
    procedure EmitMessage(Msg: TPAMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AnalyzeModule(aModule: TPasModule);
    function FindElement(El: TPasElement): TPAElement;
    property OnMessage: TPAMessageEvent read FOnMessage write FOnMessage;
    property Options: TPasAnalyzerOptions read FOptions write SetOptions;
    property Resolver: TPasResolver read FResolver write FResolver;
    property ScopeModule: TPasModule read FScopeModule write FScopeModule;
  end;

function ComparePAElements(Identifier1, Identifier2: Pointer): integer;
function CompareElementWithPAElement(El, Id: Pointer): integer;
function ComparePAOverrideLists(List1, List2: Pointer): integer;
function CompareElementWithPAOverrideList(El, List: Pointer): integer;

implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function ComparePAElements(Identifier1, Identifier2: Pointer): integer;
var
  Item1: TPAElement absolute Identifier1;
  Item2: TPAElement absolute Identifier2;
begin
  Result:=ComparePointer(Item1.Element,Item2.Element);
end;

function CompareElementWithPAElement(El, Id: Pointer): integer;
var
  Identifier: TPAElement absolute Id;
begin
  Result:=ComparePointer(El,Identifier.Element);
end;

function ComparePAOverrideLists(List1, List2: Pointer): integer;
var
  Item1: TPAOverrideList absolute List1;
  Item2: TPAOverrideList absolute List2;
begin
  Result:=ComparePointer(Item1.Element,Item2.Element);
end;

function CompareElementWithPAOverrideList(El, List: Pointer): integer;
var
  OvList: TPAOverrideList absolute List;
begin
  Result:=ComparePointer(El,OvList.Element);
end;

{ TPAOverrideList }

// inline
function TPAOverrideList.GetOverrides(Index: integer): TPasElement;
begin
  Result:=TPasElement(FOverrides[Index]);
end;

// inline
function TPAOverrideList.IndexOf(OverrideEl: TPasElement): integer;
begin
  Result:=FOverrides.IndexOf(OverrideEl);
end;

procedure TPAOverrideList.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if FElement<>nil then
    FElement.Release;
  FElement:=AValue;
  if FElement<>nil then
    FElement.AddRef;
end;

constructor TPAOverrideList.Create;
begin
  FOverrides:=TFPList.Create;
end;

destructor TPAOverrideList.Destroy;
var
  i: Integer;
begin
  for i:=0 to FOverrides.Count-1 do
    TPasElement(FOverrides[i]).Release;
  FreeAndNil(FOverrides);
  inherited Destroy;
end;

procedure TPAOverrideList.Add(OverrideEl: TPasElement);
begin
  FOverrides.Add(OverrideEl);
  OverrideEl.AddRef;
end;

function TPAOverrideList.Count: integer;
begin
  Result:=FOverrides.Count;
end;

{ TPAElement }

procedure TPAElement.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if FElement<>nil then
    FElement.Release;
  FElement:=AValue;
  if FElement<>nil then
    FElement.AddRef;
end;

destructor TPAElement.Destroy;
begin
  Element:=nil;
  inherited Destroy;
end;

{ TPasAnalyzer }

// inline
function TPasAnalyzer.FindNode(El: TPasElement): TAVLTreeNode;
begin
  Result:=FUsedElements.FindKey(El,@CompareElementWithPAElement);
end;

// inline
function TPasAnalyzer.FindPAElement(El: TPasElement): TPAElement;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(El);
  if Node=nil then
    Result:=nil
  else
    Result:=TPAElement(Node.Data);
end;

procedure TPasAnalyzer.SetOptions(AValue: TPasAnalyzerOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

function TPasAnalyzer.FindOverrideNode(El: TPasElement): TAVLTreeNode;
begin
  Result:=FOverrideLists.FindKey(El,@CompareElementWithPAOverrideList);
end;

function TPasAnalyzer.FindOverrideList(El: TPasElement): TPAOverrideList;
var
  Node: TAVLTreeNode;
begin
  Node:=FindOverrideNode(El);
  if Node=nil then
    Result:=nil
  else
    Result:=TPAOverrideList(Node.Data);
end;

function TPasAnalyzer.AddOverride(OverriddenEl, OverrideEl: TPasElement): boolean;
// OverrideEl overrides OverriddenEl
// returns true if new override
var
  Node: TAVLTreeNode;
  Item: TPAOverrideList;
  OverriddenPAEl: TPAElement;
begin
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.AddOverride OverriddenEl=',GetObjName(OverriddenEl),' OverrideEl=',GetObjName(OverrideEl));
  {$ENDIF}
  Node:=FindOverrideNode(OverriddenEl);
  if Node=nil then
    begin
    Item:=TPAOverrideList.Create;
    Item.Element:=OverriddenEl;
    FOverrideLists.Add(Item);
    end
  else
    begin
    Item:=TPAOverrideList(Node.Data);
    if Item.IndexOf(OverrideEl)>=0 then
      exit(false);
    end;
  // new override
  Item.Add(OverrideEl);
  Result:=true;

  OverriddenPAEl:=FindPAElement(OverriddenEl);
  if OverriddenPAEl<>nil then
    UseElement(OverrideEl,rraNone,true);
end;

procedure TPasAnalyzer.UpdateAccess(IsWrite: Boolean; IsRead: Boolean;
  Usage: TPAElement);
begin
  if IsRead then
    case Usage.Access of
      paiaNone: Usage.Access:=paiaRead;
      paiaRead: ;
      paiaWrite: Usage.Access:=paiaWriteRead;
      paiaReadWrite: ;
      paiaWriteRead: ;
      else RaiseInconsistency(20170311183122, '');
    end;
  if IsWrite then
    case Usage.Access of
      paiaNone: Usage.Access:=paiaWrite;
      paiaRead: Usage.Access:=paiaReadWrite;
      paiaWrite: ;
      paiaReadWrite: ;
      paiaWriteRead: ;
      else RaiseInconsistency(20170311183127, '');
    end;
end;

procedure TPasAnalyzer.RaiseInconsistency(const Id: int64; Msg: string);
begin
  raise EPasAnalysis.Create('['+IntToStr(Id)+']: '+Msg);
end;

procedure TPasAnalyzer.RaiseNotSupported(const Id: int64; El: TPasElement;
  const Msg: string);
var
  s: String;
  E: EPasAnalysis;
begin
  s:='['+IntToStr(Id)+']: Element='+GetObjName(El);
  if Msg<>'' then S:=S+' '+Msg;
  E:=EPasAnalysis.Create(s);
  E.PasElement:=El;
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.RaiseNotSupported ',E.Message);
  {$ENDIF}
  raise E;
end;

function TPasAnalyzer.Add(El: TPasElement; CheckDuplicate: boolean;
  aClass: TPAElementClass): TPAElement;
begin
  if El=nil then
    RaiseInconsistency(20170308093407,'');
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.Add ',GetObjName(El),' New=',FindNode(El)=nil);
  {$ENDIF}
  if CheckDuplicate and (FindNode(El)<>nil) then
    RaiseInconsistency(20170304201318,'');
  if aClass=nil then
    aClass:=TPAElement;
  Result:=aClass.Create;
  Result.Element:=El;
  FUsedElements.Add(Result);
end;

procedure TPasAnalyzer.CreateTree;
begin
  FUsedElements:=TAVLTree.Create(@ComparePAElements);
end;

function TPasAnalyzer.MarkElement(El: TPasElement; aClass: TPAElementClass
  ): boolean;
var
  CurModule: TPasModule;
begin
  if El=nil then exit(false);
  if ScopeModule<>nil then
    begin
    CurModule:=El.GetModule;
    if CurModule=nil then
      begin
      if El.ClassType=TPasUnresolvedSymbolRef then
        exit(false);
      {$IFDEF VerbosePasAnalyzer}
      writeln('TPasAnalyzer.MarkElement GetModule failed for El=',GetObjName(El),' El.Parent=',GetObjName(El.Parent));
      {$ENDIF}
      RaiseInconsistency(20170308093540,GetObjName(El));
      end;
    if CurModule<>ScopeModule then
      begin
      // element from another unit -> mark unit as needed
      if FindNode(CurModule)=nil then
        Add(CurModule);
      exit(false);
      end;
    end;
  if FindNode(El)<>nil then exit(false);
  Add(El,false,aClass);
  Result:=true;
end;

procedure TPasAnalyzer.UseElement(El: TPasElement; Access: TResolvedRefAccess;
  UseFull: boolean);
begin
  if El=nil then exit;
  if El is TPasType then
    UseType(TPasType(El))
  else if El is TPasVariable then
    UseVariable(TPasVariable(El),Access,UseFull)
  else if El.ClassType=TPasArgument then
    UseArgument(TPasArgument(El),Access)
  else if El.ClassType=TPasResultElement then
    UseResultElement(TPasResultElement(El),Access)
  else if El is TPasProcedure then
    UseProcedure(TPasProcedure(El))
  else if El is TPasExpr then
    UseExpr(TPasExpr(El))
  else
    RaiseNotSupported(20170307090947,El);
end;

procedure TPasAnalyzer.UseSection(Section: TPasSection);
var
  i: Integer;
  UsesList: TFPList;
  UsedModule: TPasModule;
  InitSection: TInitializationSection;
begin
  if not MarkElement(Section) then exit;
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.UseSection ',GetObjName(Section),' New=',FindNode(Section)=nil);
  {$ENDIF}
  // initialization, program or library sections
  UsesList:=Section.UsesList;
  for i:=0 to UsesList.Count-1 do
    begin
    if TObject(UsesList[i]) is TPasModule then
      begin
      UsedModule:=TPasModule(UsesList[i]);
      InitSection:=UsedModule.InitializationSection;
      if (InitSection=nil) or (InitSection.Elements.Count=0) then continue;
      // has initialization section
      Add(UsedModule);
      if ScopeModule=nil then
        UseImplBlock(InitSection,true);
      end;
    end;
  UseDeclarations(Section,paoOnlyExports in Options);
end;

procedure TPasAnalyzer.UseDeclarations(El: TPasDeclarations;
  OnlyExports: boolean);
var
  i: Integer;
  Decl: TPasElement;
begin
  for i:=0 to El.Declarations.Count-1 do
    begin
    Decl:=TPasElement(El.Declarations[i]);
    if Decl is TPasProcedure then
      begin
      if OnlyExports and (TPasProcedure(Decl).PublicName=nil) then continue;
      UseProcedure(TPasProcedure(Decl))
      end
    else if Decl is TPasType then
      UseType(TPasType(Decl))
    else if Decl is TPasVariable then
      begin
      if OnlyExports and ([vmExport,vmPublic]*TPasVariable(Decl).VarModifiers=[]) then
        continue;
      UseVariable(TPasVariable(Decl),rraNone,true);
      end
    else
      RaiseNotSupported(20170306165213,Decl);
    end;
end;

procedure TPasAnalyzer.UseImplBlock(Block: TPasImplBlock; Mark: boolean);
var
  i: Integer;
  El: TPasElement;
begin
  if Block=nil then exit;
  if Mark and not MarkElement(Block) then exit;
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.UseImplBlock ',GetObjName(Block),' Elements=',Block.Elements.Count);
  {$ENDIF}
  for i:=0 to Block.Elements.Count-1 do
    begin
    El:=TPasElement(Block.Elements[i]);
    if El is TPasImplElement then
      UseImplElement(TPasImplElement(El))
    else
      RaiseNotSupported(20170306195110,El);
    end;
end;

procedure TPasAnalyzer.UseImplElement(El: TPasImplElement);
var
  C: TClass;
  ForLoop: TPasImplForLoop;
  CaseOf: TPasImplCaseOf;
  i, j: Integer;
  CaseSt: TPasImplCaseStatement;
  WithDo: TPasImplWithDo;
  SubEl: TPasElement;
begin
  // do not mark
  if El=nil then exit;
  C:=El.ClassType;
  if C=TPasImplBlock then
    // impl block
    UseImplBlock(TPasImplBlock(El),false)
  else if C=TPasImplSimple then
    // simple expression
    UseExpr(TPasImplSimple(El).expr)
  else if C=TPasImplAssign then
    // a:=b
    begin
    UseExpr(TPasImplAssign(El).left);
    UseExpr(TPasImplAssign(El).right);
    end
  else if C=TPasImplAsmStatement then
    // asm..end
  else if C=TPasImplBeginBlock then
    // begin..end
    UseImplBlock(TPasImplBeginBlock(El),false)
  else if C=TPasImplCaseOf then
    begin
    // case-of
    CaseOf:=TPasImplCaseOf(El);
    UseExpr(CaseOf.CaseExpr);
    for i:=0 to CaseOf.Elements.Count-1 do
      begin
      SubEl:=TPasElement(CaseOf.Elements[i]);
      if SubEl.ClassType=TPasImplCaseStatement then
        begin
        CaseSt:=TPasImplCaseStatement(SubEl);
        for j:=0 to CaseSt.Expressions.Count-1 do
          UseExpr(TObject(CaseSt.Expressions[j]) as TPasExpr);
        UseImplElement(CaseSt.Body);
        end
      else if SubEl.ClassType=TPasImplCaseElse then
        UseImplBlock(TPasImplCaseElse(SubEl),false)
      else
        RaiseNotSupported(20170307195329,SubEl);
      end;
    end
  else if C=TPasImplForLoop then
    begin
    // for-loop
    ForLoop:=TPasImplForLoop(El);
    UseExpr(ForLoop.VariableName);
    UseExpr(ForLoop.StartExpr);
    UseExpr(ForLoop.EndExpr);
    UseImplElement(ForLoop.Body);
    end
  else if C=TPasImplExceptOn then
    begin
    // except-on
    UseType(TPasImplExceptOn(El).TypeEl);
    UseImplElement(TPasImplExceptOn(El).Body);
    end
  else if C=TPasImplIfElse then
    begin
    // if-then-else
    UseExpr(TPasImplIfElse(El).ConditionExpr);
    UseImplElement(TPasImplIfElse(El).IfBranch);
    UseImplElement(TPasImplIfElse(El).ElseBranch);
    end
  else if C=TPasImplLabelMark then
    // label mark
  else if C=TPasImplRepeatUntil then
    begin
    // repeat-until
    UseImplBlock(TPasImplRepeatUntil(El),false);
    UseExpr(TPasImplRepeatUntil(El).ConditionExpr);
    end
  else if C=TPasImplWhileDo then
    begin
    // while-do
    UseExpr(TPasImplWhileDo(El).ConditionExpr);
    UseImplBlock(TPasImplWhileDo(El),false);
    end
  else if C=TPasImplWithDo then
    begin
    // with-do
    WithDo:=TPasImplWithDo(El);
    for i:=0 to WithDo.Expressions.Count-1 do
      UseExpr(TObject(WithDo.Expressions[i]) as TPasExpr);
    UseImplBlock(WithDo,false);
    end
  else if C=TPasImplRaise then
    begin
    // raise
    UseExpr(TPasImplRaise(El).ExceptObject);
    UseExpr(TPasImplRaise(El).ExceptAddr);
    end
  else if C=TPasImplTry then
    begin
    // try..finally/except..else..end
    UseImplBlock(TPasImplTry(El),false);
    UseImplBlock(TPasImplTry(El).FinallyExcept,false);
    UseImplBlock(TPasImplTry(El).ElseBranch,false);
    end
  else
    RaiseNotSupported(20170307162715,El);
end;

procedure TPasAnalyzer.UseExpr(El: TPasExpr);
var
  Ref: TResolvedReference;
  C: TClass;
  Params: TPasExprArray;
  i: Integer;
begin
  if El=nil then exit;
  // expression are not marked
  if El.CustomData is TResolvedReference then
    begin
    Ref:=TResolvedReference(El.CustomData);
    UseElement(Ref.Declaration,Ref.Access,false);
    end;
  UseExpr(El.format1);
  UseExpr(El.format2);
  C:=El.ClassType;
  if (C=TPrimitiveExpr)
      or (C=TSelfExpr)
      or (C=TBoolConstExpr)
      or (C=TInheritedExpr)
      or (C=TNilExpr) then
  else if C=TBinaryExpr then
    begin
    UseExpr(TBinaryExpr(El).left);
    UseExpr(TBinaryExpr(El).right);
    end
  else if C=TParamsExpr then
    begin
    UseExpr(TParamsExpr(El).Value);
    Params:=TParamsExpr(El).Params;
    for i:=0 to length(Params)-1 do
      UseExpr(Params[i]);
    end
  else
    RaiseNotSupported(20170307085444,El);
end;

procedure TPasAnalyzer.UseProcedure(Proc: TPasProcedure);

  procedure UseOverrides(CurProc: TPasProcedure);
  var
    OverrideList: TPAOverrideList;
    i: Integer;
    OverrideProc: TPasProcedure;
  begin
    OverrideList:=FindOverrideList(CurProc);
    if OverrideList=nil then exit;
    // Note: while traversing the OverrideList it may grow
    i:=0;
    while i<OverrideList.Count do
      begin
      OverrideProc:=TObject(OverrideList.Overrides[i]) as TPasProcedure;
      UseProcedure(OverrideProc);
      inc(i);
      end;
  end;

var
  ProcScope: TPasProcedureScope;
  ImplProc: TPasProcedure;
begin
  // use declaration, not implementation
  ProcScope:=Proc.CustomData as TPasProcedureScope;
  if ProcScope.DeclarationProc<>nil then
    exit; // skip implementation, Note:PasResolver always refers the declaration

  if not MarkElement(Proc) then exit;
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.UseProcedure ',GetObjName(Proc));
  {$ENDIF}
  UseProcedureType(Proc.ProcType);

  ImplProc:=Proc;
  if ProcScope.ImplProc<>nil then
    ImplProc:=ProcScope.ImplProc;
  if ImplProc.Body<>nil then
    UseImplBlock(ImplProc.Body.Body,false);

  if ProcScope.OverriddenProc<>nil then
    AddOverride(ProcScope.OverriddenProc,Proc);

  // mark overrides
  if [pmOverride,pmVirtual]*Proc.Modifiers<>[] then
    UseOverrides(Proc);
end;

procedure TPasAnalyzer.UseProcedureType(ProcType: TPasProcedureType);
var
  i: Integer;
  Arg: TPasArgument;
begin
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.UseProcedureType ',GetObjName(ProcType));
  {$ENDIF}
  // proc types are marked
  for i:=0 to ProcType.Args.Count-1 do
    begin
    Arg:=TPasArgument(ProcType.Args[i]);
    // Note: argument are marked when used in code
    // mark argument type
    UseType(Arg.ArgType);
    end;
  if ProcType is TPasFunctionType then
    UseType(TPasFunctionType(ProcType).ResultEl.ResultType);
end;

procedure TPasAnalyzer.UseType(El: TPasType);
var
  C: TClass;
begin
  {$IFDEF VerbosePasAnalyzer}
  if El=nil then exit;
  writeln('TPasAnalyzer.UseType ',GetObjName(El));
  {$ENDIF}
  if not MarkElement(El) then exit;
  C:=El.ClassType;
  if C=TPasUnresolvedSymbolRef then
    begin
    if El.CustomData is TResElDataBaseType then
    else
      RaiseNotSupported(20170307101353,El);
    end
  else if (C=TPasAliasType) or (C=TPasTypeAliasType) then
    UseType(TPasAliasType(El).DestType)
  else if C=TPasArrayType then
    UseArrayType(TPasArrayType(El),false)
  else if C=TPasRecordType then
    UseRecordType(TPasRecordType(El),false)
  else if C=TPasClassType then
    UseClassType(TPasClassType(El),false)
  else
    RaiseNotSupported(20170306170315,El);
end;

procedure TPasAnalyzer.UseArrayType(El: TPasArrayType; Mark: boolean);
var
  i: Integer;
begin
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.UseArrayType ',GetObjName(El),' ElType=',GetObjName(El.ElType),' Ranges=',length(El.Ranges));
  {$ENDIF}
  if Mark and not MarkElement(El) then exit;
  for i:=0 to length(El.Ranges)-1 do
    UseExpr(El.Ranges[i]);
  UseType(El.ElType);
end;

procedure TPasAnalyzer.UseRecordType(El: TPasRecordType; Mark: boolean);
var
  i: Integer;
begin
  if Mark and not MarkElement(El) then exit;
  if (paoOnlyExports in Options) or IsModuleInternal(El) then exit;
  for i:=0 to El.Members.Count-1 do
    UseVariable(TObject(El.Members) as TPasVariable,rraNone,true);
end;

procedure TPasAnalyzer.UseClassType(El: TPasClassType; Mark: boolean);
var
  i: Integer;
  Member: TPasElement;
  UsePublished, OnlyExports: Boolean;
  ProcScope: TPasProcedureScope;
begin
  if Mark and not MarkElement(El) then exit;
  UseType(El.AncestorType);
  UseType(El.HelperForType);
  UseExpr(El.GUIDExpr);
  for i:=0 to El.Interfaces.Count-1 do
    UseType(TPasType(El.Interfaces[i]));
  // members
  UsePublished:=paoAlwaysUsePublished in Options;
  OnlyExports:=(paoOnlyExports in Options) or IsModuleInternal(El);
  // Note: OnlyExports means to use only explicitely exported members
  //       and directly used members
  for i:=0 to El.Members.Count-1 do
    begin
    Member:=TPasElement(El.Members[i]);
    if (Member is TPasProcedure) then
      begin
      ProcScope:=Member.CustomData as TPasProcedureScope;
      if ProcScope.OverriddenProc<>nil then
        AddOverride(ProcScope.OverriddenProc,Member);
      end;
    if UsePublished and (Member.Visibility=visPublished) then
      // always include published
    else if OnlyExports then
      begin
      if not IsExport(Member) then continue;
      end
    else if IsModuleInternal(Member) then
      continue
    else
      ; // else: class is in unit interface, mark all non private members
    UseElement(Member,rraNone,true);
    end;
end;

procedure TPasAnalyzer.UseVariable(El: TPasVariable;
  Access: TResolvedRefAccess; UseFull: boolean);
var
  Usage: TPAElement;
  UseRead, UseWrite: boolean;

  procedure UpdateVarAccess(IsRead, IsWrite: boolean);
  begin
    if IsRead then
      case Usage.Access of
        paiaNone: begin Usage.Access:=paiaRead; UseRead:=true; end;
        paiaRead: ;
        paiaWrite: begin Usage.Access:=paiaWriteRead; UseRead:=true; end;
        paiaReadWrite: ;
        paiaWriteRead: ;
        else RaiseInconsistency(20170311182420,'');
      end;
    if IsWrite then
      case Usage.Access of
        paiaNone: begin Usage.Access:=paiaWrite; UseWrite:=true; end;
        paiaRead: begin Usage.Access:=paiaReadWrite; UseWrite:=true; end;
        paiaWrite: ;
        paiaReadWrite: ;
        paiaWriteRead: ;
        else RaiseInconsistency(20170311182536,'');
      end;
  end;

var
  Prop: TPasProperty;
  i: Integer;
  IsRead, IsWrite, CanRead, CanWrite: Boolean;
begin
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.UseVariable ',GetObjName(El),' ',Access,' Full=',UseFull);
  {$ENDIF}
  if El.ClassType=TPasProperty then
    Prop:=TPasProperty(El)
  else
    Prop:=nil;

  IsRead:=false;
  IsWrite:=false;
  if UseFull and (Prop<>nil) then
    begin
    CanRead:=Resolver.GetPasPropertyGetter(Prop)<>nil;
    CanWrite:=Resolver.GetPasPropertySetter(Prop)<>nil;
    if CanRead then
      begin
      if CanWrite then
        Access:=rraReadAndAssign
      else
        Access:=rraRead;
      end
    else
      if CanWrite then
        Access:=rraAssign
      else
        Access:=rraNone;
    end;
  case Access of
    rraNone: ;
    rraRead: IsRead:=true;
    rraAssign: IsWrite:=true;
    rraReadAndAssign,
    rraVarParam,
    rraOutParam: begin IsRead:=true; IsWrite:=true; end;
    rraParamToUnknownProc: RaiseInconsistency(20170307153439,'');
  else
    RaiseInconsistency(20170308120949,'');
  end;

  UseRead:=false;
  UseWrite:=false;
  if MarkElement(El) then
    begin
    // first access of this variable
    Usage:=FindElement(El);
    // first set flags
    if El.Expr<>nil then
      Usage.Access:=paiaWrite;
    UpdateVarAccess(IsRead,IsWrite);
    // then use recursively
    UseType(El.VarType);
    UseExpr(El.Expr);
    UseExpr(El.LibraryName);
    UseExpr(El.ExportName);
    if Prop<>nil then
      begin
      for i:=0 to Prop.Args.Count-1 do
        UseType(TPasArgument(Prop.Args[i]).ArgType);
      UseExpr(Prop.IndexExpr);
      // ToDo: Prop.ImplementsFunc
      // ToDo: Prop.DispIDExpr
      // ToDo: Prop.StoredAccessor;
      // ToDo: Prop.DefaultExpr;
      end;
    end
  else
    begin
    Usage:=FindElement(El);
    if Usage=nil then
      exit; // element outside of scope
    // var is accessed another time

    // first update flags
    UpdateVarAccess(IsRead,IsWrite);
    end;
  // then use recursively
  if Prop<>nil then
    begin
    {$IFDEF VerbosePasAnalyzer}
    writeln('TPasAnalyzer.UseVariable Property=',Prop.FullName,
      ' Ancestor=',GetObjName(Resolver.GetPasPropertyAncestor(Prop)),
      ' UseRead=',UseRead,',Acc=',GetObjName(Resolver.GetPasPropertyGetter(Prop)),
      ' UseWrite=',UseWrite,',Acc=',GetObjName(Resolver.GetPasPropertySetter(Prop)),
      '');
    {$ENDIF}
    if UseRead then
      UseElement(Resolver.GetPasPropertyGetter(Prop),rraRead,false);
    if UseWrite then
      UseElement(Resolver.GetPasPropertySetter(Prop),rraAssign,false);
    end;
end;

procedure TPasAnalyzer.UseArgument(El: TPasArgument; Access: TResolvedRefAccess
  );
var
  Usage: TPAElement;
  IsRead, IsWrite: Boolean;
begin
  IsRead:=false;
  IsWrite:=false;
  case Access of
    rraNone: ;
    rraRead: IsRead:=true;
    rraAssign: IsWrite:=true;
    rraReadAndAssign,
    rraVarParam,
    rraOutParam: begin IsRead:=true; IsWrite:=true; end;
    rraParamToUnknownProc: RaiseInconsistency(20170308121031,'');
  else
    RaiseInconsistency(20170308121037,'');
  end;
  if MarkElement(El) then
    begin
    // first time
    Usage:=FindElement(El);
    end
  else
    begin
    // used again
    Usage:=FindElement(El);
    if Usage=nil then
      RaiseNotSupported(20170308121928,El);
    end;
  UpdateAccess(IsWrite, IsRead, Usage);
end;

procedure TPasAnalyzer.UseResultElement(El: TPasResultElement;
  Access: TResolvedRefAccess);
var
  IsRead, IsWrite: Boolean;
  Usage: TPAElement;
begin
  IsRead:=false;
  IsWrite:=false;
  case Access of
    rraNone: ;
    rraRead: IsRead:=true;
    rraAssign: IsWrite:=true;
    rraReadAndAssign,
    rraVarParam,
    rraOutParam: begin IsRead:=true; IsWrite:=true; end;
    rraParamToUnknownProc: RaiseInconsistency(20170308122319,'');
  else
    RaiseInconsistency(20170308122324,'');
  end;
  if MarkElement(El) then
    begin
    // first time
    Usage:=FindElement(El);
    end
  else
    begin
    // used again
    Usage:=FindElement(El);
    if Usage=nil then
      RaiseNotSupported(20170308122333,El);
    end;
  UpdateAccess(IsWrite, IsRead, Usage);
end;

procedure TPasAnalyzer.EmitElementHints(El: TPasElement);
begin
  if El=nil then exit;
  if El is TPasVariable then
    EmitVariableHints(TPasVariable(El))
  else if El is TPasType then
    EmitTypeHints(TPasType(El))
  else if El is TPasProcedure then
    EmitProcedureHints(TPasProcedure(El))
  else
    RaiseInconsistency(20170312093126,'');
end;

procedure TPasAnalyzer.EmitModuleHints(aModule: TPasModule);
begin
  if aModule.ClassType=TPasProgram then
    EmitSectionHints(TPasProgram(aModule).ProgramSection)
  else if aModule.ClassType=TPasLibrary then
    EmitSectionHints(TPasLibrary(aModule).LibrarySection)
  else
    begin
    // unit
    EmitSectionHints(aModule.InterfaceSection);
    EmitSectionHints(aModule.ImplementationSection);
    end;
  //EmitBlockHints(aModule.InitializationSection);
  //EmitBlockHints(aModule.FinalizationSection);
end;

procedure TPasAnalyzer.EmitSectionHints(Section: TPasSection);
var
  UsesList: TFPList;
  i: Integer;
  UsedModule, aModule: TPasModule;
begin
  {$IFDEF VerbosePasAnalyzer}
  writeln('TPasAnalyzer.EmitSectionHints ',GetObjName(Section));
  {$ENDIF}
  // initialization, program or library sections
  aModule:=Section.GetModule;
  UsesList:=Section.UsesList;
  for i:=0 to UsesList.Count-1 do
    begin
    if TObject(UsesList[i]) is TPasModule then
      begin
      UsedModule:=TPasModule(UsesList[i]);
      if FindNode(UsedModule)=nil then
        EmitMessage(20170311191725,mtHint,nPAUnitNotUsed,sPAUnitNotUsed,
          [UsedModule.Name,aModule.Name],aModule);
      end;
    end;
  EmitDeclarationsHints(Section);
end;

procedure TPasAnalyzer.EmitDeclarationsHints(El: TPasDeclarations);
var
  i: Integer;
  Decl: TPasElement;
  Usage: TPAElement;
begin
  for i:=0 to El.Declarations.Count-1 do
    begin
    Decl:=TPasElement(El.Declarations[i]);
    if Decl is TPasVariable then
      EmitVariableHints(TPasVariable(Decl))
    else if Decl is TPasType then
      EmitTypeHints(TPasType(Decl))
    else if Decl is TPasProcedure then
      EmitProcedureHints(TPasProcedure(Decl))
    else
      begin
      Usage:=FindPAElement(Decl);
      if Usage=nil then
        begin
        // declaration was never used
        EmitMessage(20170311231734,mtHint,nPALocalXYNotUsed,
          sPALocalXYNotUsed,[Decl.ElementTypeName,Decl.Name],Decl);
        end;
      end;
    end;
end;

procedure TPasAnalyzer.EmitTypeHints(El: TPasType);
var
  C: TClass;
  Usage: TPAElement;
  i: Integer;
  Member: TPasElement;
begin
  Usage:=FindPAElement(El);
  if Usage=nil then
    begin
    // the whole type was never used
    if (El.Visibility in [visPrivate,visStrictPrivate]) then
      EmitMessage(20170312000020,mtHint,nPAPrivateTypeXNeverUsed,
        sPAPrivateTypeXNeverUsed,[El.FullName],El)
    else
      EmitMessage(20170312000025,mtHint,nPALocalXYNotUsed,
        sPALocalXYNotUsed,[El.ElementTypeName,El.Name],El);
    exit;
    end;
  // emit hints for sub elements
  C:=El.ClassType;
  if C=TPasRecordType then
    begin
    for i:=0 to TPasRecordType(El).Members.Count-1 do
      EmitVariableHints(TObject(TPasRecordType(El).Members) as TPasVariable);
    end
  else if C=TPasClassType then
    begin
    for i:=0 to TPasClassType(El).Members.Count-1 do
      begin
      Member:=TPasElement(TPasClassType(El).Members[i]);
      EmitElementHints(Member);
      end;
    end;
end;

procedure TPasAnalyzer.EmitVariableHints(El: TPasVariable);
var
  Usage: TPAElement;
begin
  Usage:=FindPAElement(El);
  if Usage=nil then
    begin
    // not used
    if El.Visibility in [visPrivate,visStrictPrivate] then
      begin
      if El.ClassType=TPasConst then
        EmitMessage(20170311234602,mtHint,nPAPrivateConstXNeverUsed,
          sPAPrivateConstXNeverUsed,[El.FullName],El)
      else if El.ClassType=TPasProperty then
        EmitMessage(20170311234634,mtHint,nPAPrivatePropertyXNeverUsed,
          sPAPrivatePropertyXNeverUsed,[El.FullName],El)
      else
        EmitMessage(20170311231412,mtHint,nPAPrivateFieldIsNeverUsed,
          sPAPrivateFieldIsNeverUsed,[El.FullName],El);
      end
    else
      EmitMessage(20170311234201,mtHint,nPALocalVariableNotUsed,
        sPALocalVariableNotUsed,[El.Name],El);
    end
  else if Usage.Access=paiaWrite then
    begin
    // write without read
    if El.Visibility in [visPrivate,visStrictPrivate] then
      EmitMessage(20170311234159,mtHint,nPAPrivateFieldIsAssignedButNeverUsed,
        sPAPrivateFieldIsAssignedButNeverUsed,[El.FullName],El)
    else
      EmitMessage(20170311233825,mtHint,nPALocalVariableIsAssignedButNeverUsed,
        sPALocalVariableIsAssignedButNeverUsed,[El.Name],El);
    end;
end;

procedure TPasAnalyzer.EmitProcedureHints(El: TPasProcedure);
var
  Args: TFPList;
  i: Integer;
  Arg: TPasArgument;
  Usage: TPAElement;
  ProcScope: TPasProcedureScope;
  WasNeverUsed: Boolean;
begin
  ProcScope:=El.CustomData as TPasProcedureScope;
  if ProcScope.DeclarationProc<>nil then
    WasNeverUsed:=FindNode(ProcScope.DeclarationProc)=nil
  else
    WasNeverUsed:=FindNode(El)=nil;
  if WasNeverUsed then
    begin
    // procedure never used
    if El.Visibility in [visPrivate,visStrictPrivate] then
      EmitMessage(20170312093348,mtHint,nPAPrivateMethodIsNeverUsed,
        sPAPrivateMethodIsNeverUsed,[El.FullName],El)
    else
      EmitMessage(20170312093418,mtHint,nPALocalXYNotUsed,
        sPALocalXYNotUsed,[El.ElementTypeName,El.Name],El);
    exit;
    end;

  // procedure was used

  if pmAssembler in El.Modifiers then exit;

  if ProcScope.DeclarationProc=nil then
    begin
    // check parameters
    Args:=El.ProcType.Args;
    for i:=0 to Args.Count-1 do
      begin
      Arg:=TPasArgument(Args[i]);
      Usage:=FindPAElement(Arg);
      if (Usage=nil) or (Usage.Access=paiaNone) then
        // parameter was never used
        EmitMessage(20170312094401,mtHint,nPAParameterNotUsed,
          sPAParameterNotUsed,[Arg.Name],Arg)
      else
        begin
        // parameter was used
        if Usage.Access=paiaWrite then
          EmitMessage(20170312095348,mtHint,nPAValueParameterIsAssignedButNeverUsed,
            sPAValueParameterIsAssignedButNeverUsed,[El.Name],El);
        end;
      end;
    end;
  if El.Body<>nil then
    begin
    // check declarations
    EmitDeclarationsHints(El.Body);
    // ToDo: emit hints for statements
    end;
end;

function TPasAnalyzer.IsModuleInternal(El: TPasElement): boolean;
begin
  if El=nil then
    exit(true);
  if El.ClassType=TInterfaceSection then
    exit(false);
  if IsExport(El) then exit(false);
  case El.Visibility of
  visPrivate,visStrictPrivate: exit(true);
  visPublished: if paoAlwaysUsePublished in Options then exit(false);
  end;
  Result:=IsModuleInternal(El.Parent);
end;

function TPasAnalyzer.IsExport(El: TPasElement): boolean;
begin
  if El is TPasVariable then
    Result:=[vmExport,vmPublic]*TPasVariable(El).VarModifiers<>[]
  else if El is TPasProcedure then
    Result:=[pmExport,pmPublic]*TPasProcedure(El).Modifiers<>[]
  else
    Result:=false;
end;

procedure TPasAnalyzer.EmitMessage(const Id: int64;
  const MsgType: TMessageType; MsgNumber: integer; Fmt: String;
  const Args: array of const; PosEl: TPasElement);
var
  Msg: TPAMessage;
begin
  Msg:=TPAMessage.Create;
  Msg.Id:=Id;
  Msg.MsgType:=MsgType;
  Msg.MsgNumber:=MsgNumber;
  Msg.Fmt:=Fmt;
  CreateMsgArgs(Msg.Args,Args);
  Msg.PosEl:=PosEl;
  Msg.Filename:=PosEl.SourceFilename;
  Resolver.UnmangleSourceLineNumber(PosEl.SourceLinenumber,Msg.Row,Msg.Col);
  EmitMessage(Msg);
end;

procedure TPasAnalyzer.EmitMessage(Msg: TPAMessage);
begin
  try
    OnMessage(Self,Msg);
  finally
    Msg.Free;
  end;
end;

constructor TPasAnalyzer.Create;
begin
  CreateTree;
  FOverrideLists:=TAVLTree.Create(@ComparePAOverrideLists);
end;

destructor TPasAnalyzer.Destroy;
begin
  Clear;
  FreeAndNil(FOverrideLists);
  FreeAndNil(FUsedElements);
  inherited Destroy;
end;

procedure TPasAnalyzer.Clear;
begin
  FOverrideLists.FreeAndClear;
  FUsedElements.FreeAndClear;
end;

procedure TPasAnalyzer.AnalyzeModule(aModule: TPasModule);
begin
  ScopeModule:=aModule;
  Add(aModule);
  if aModule is TPasProgram then
    UseSection(TPasProgram(aModule).ProgramSection)
  else if aModule is TPasLibrary then
    UseSection(TPasLibrary(aModule).LibrarySection)
  else
    begin
    // unit
    UseSection(aModule.InterfaceSection);
    end;
  UseImplBlock(aModule.InitializationSection,true);
  if Assigned(OnMessage) then
    EmitModuleHints(aModule);
end;

function TPasAnalyzer.FindElement(El: TPasElement): TPAElement;
var
  Node: TAVLTreeNode;
begin
  Node:=FindNode(El);
  if Node=nil then
    Result:=nil
  else
    Result:=TPAElement(Node.Data);
end;

end.


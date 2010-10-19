unit tcresref;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, TypInfo, testutils, testregistry;

type

  { TRefComponent }

  TRefComponent = Class(TComponent)
  private
    FRef1: TComponent;
    FRef2: TComponent;
  Published
    Property Ref1 : TComponent Read FRef1 Write FRef1;
    Property Ref2 : TComponent Read FRef2 Write FRef2;
  end;
  
  TRootA = Class(TRefComponent)
  end;
  
  TRootB = Class(TRefComponent)
  end;
  
  TA = Class(TRefComponent)
  end;
  TB = Class(TRefComponent)
  end;


  { TTestResolveReference }

  TTestResolveReference = class(TTestCase)
  Private
    RootA : TRootA;
    RootB : TRootB;
    PropA1,
    PropA2,
    PropB1,
    PropB2 : PPRopInfo;
    UnrA : TObject;
    UnrB : TObject;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestAddInst1;
    procedure TestAddInst2;
    procedure TestAddInst3;
    procedure TestAdd2;
    procedure TestAdd3;
    Procedure TestFixupReferenceNames1;
    procedure TestFixupReferenceNames2;
    procedure TestFixupReferenceNames3;
    Procedure TestFixupInstanceNames1;
    Procedure TestFixupInstanceNames2;
    procedure TestFixupInstanceNames3;
    procedure TestFixupInstanceNames4;
    procedure TestFixupInstanceNames5;
    procedure TestRedirectFixupReferences1;
    procedure TestRedirectFixupReferences2;
    procedure TestRedirectFixupReferences3;
    procedure TestRemoveFixupReferences1;
    procedure TestRemoveFixupReferences2;
    procedure TestFixupReferences1;
    procedure TestFixupReferences2;
    procedure TestFixupReferences3;
  end;

implementation


{$i sllist2.inc}
{$i resref2.inc}

{ ---------------------------------------------------------------------
  Auxiliary routines
  ---------------------------------------------------------------------}

// Simulate Adding RootA to unresolved instances
Function RootAToResolveList(TC : TTestResolveReference) : TUnresolvedInstance;

begin
  Result:=AddToResolveList(TC.RootA);
  TC.UnrA:=Result;
end;

// Simulate Adding RootB to unresolved instances
Function RootBToResolveList(TC : TTestResolveReference) : TUnresolvedInstance;

begin
  Result:=AddToResolveList(TC.RootB);
  TC.UnrB:=Result;
end;

// Simulate RootA.Ref1 -> RootB.A unresolved reference
Function SetupARef1A(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootAToResolveList(TC).AddReference(TC.RootA,TC.PropA1,'RootB','A');
end;

// Simulate RootA.Ref1 -> RootB.B unresolved reference
Function SetupARef1B(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootAToResolveList(TC).AddReference(TC.RootA,TC.PropA1,'RootB','B');
end;


// Simulate RootA.Ref2 -> RootB.A unresolved reference
Function SetupARef2A(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootAToResolveList(TC).AddReference(TC.RootA,TC.PropA2,'RootB','A');
end;
// Simulate RootA.Ref2 -> RootB.B unresolved reference
Function SetupARef2B(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootAToResolveList(TC).AddReference(TC.RootA,TC.PropA2,'RootB','B');
end;

// Simulate RootB.Ref2 -> RootA.B unresolved reference
Function SetupBRef2B(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootBToResolveList(TC).AddReference(TC.RootB,TC.PropB2,'RootA','B');
end;

Function SetupBRef1A(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootBToResolveList(TC).AddReference(TC.RootB,TC.PropB1,'RootA','A');
end;

// Simulate RootB.Ref1 -> RootA.B unresolved reference
Function SetupNRef1B(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootBToResolveList(TC).AddReference(TC.RootB,TC.PropB1,'RootA','B');
end;

// Simulate RootA.Ref2 -> RootA.A unresolved reference
Function SetupBRef2A(TC : TTestResolveReference) : TUnresolvedReference;

begin
  Result:=RootBToResolveList(TC).AddReference(TC.RootB,TC.PropB2,'RootA','A');
end;

{ ---------------------------------------------------------------------
  Search callback
  ---------------------------------------------------------------------}

Var
  TI : TTestResolveReference;

Function SearchRoots(Const AName : String) : TComponent;

begin
  Result:=Nil;
  If Assigned(TI) then
    begin
    If CompareText(AName,'RootA')=0 then
      Result:=TI.RootA
    else If CompareText(AName,'RootB')=0 then
      Result:=TI.RootB;
    end;
end;

{ ---------------------------------------------------------------------
  Setup/TearDown
  ---------------------------------------------------------------------}

procedure TTestResolveReference.SetUp;
begin
  TI:=Self;
  RegisterFindGlobalComponentProc(@SearchRoots);
  RootA:=TRootA.Create(Nil);
  RootA.Name:='RootA';
  With TA.Create(RootA) do
    Name:='A';
  With TB.Create(RootA) do
    Name:='B';
  RootB:=TRootB.Create(Nil);
  With TA.Create(RootB) do
    Name:='A';
  With TB.Create(RootB) do
    Name:='B';
  PRopA1:=GetPropInfo(TRootA,'Ref1');
  PRopA2:=GetPropInfo(TRootA,'Ref2');
  PRopB1:=GetPropInfo(TRootB,'Ref1');
  PRopB2:=GetPropInfo(TRootB,'Ref2');
end;

procedure TTestResolveReference.TearDown;
begin
  TI:=Nil;
  UnRegisterFindGlobalComponentProc(@SearchRoots);
  FreeAndNil(NeedResolving);
  FreeAndNil(RootA);
  FreeAndNil(RootB);
end;

{ ---------------------------------------------------------------------
  Actual tests
  ---------------------------------------------------------------------}

procedure TTestResolveReference.TestAddInst1;

Var
  A : TObject;

begin
  A:=AddToResolveList(RootA);
  If Not (A is TUnresolvedInstance) then
    Fail('AddToResolveList returns TUnresolvedInstance');
  AssertSame('UNresolvedinstance.Instance is RootA',RootA,TUnresolvedInstance(A).Instance);
  AssertSame('UNresolvedinstance.Next is nil',Nil,TUnresolvedInstance(A).Next);
end;

procedure TTestResolveReference.TestAddInst2;

Var
  A,B : TObject;

begin
  A:=AddToResolveList(RootA);
  B:=AddToResolveList(RootA);
  AssertSame('UNresolvedinstance.Instance is RootA',A,B);
end;

procedure TTestResolveReference.TestAddInst3;

Var
  A,B : TUnresolvedInstance;

begin
  A:=AddToResolveList(RootA);
  B:=AddToResolveList(RootB);
  AssertSame('UnresolvedInstances are chained',A,B.Next);
end;


procedure TTestResolveReference.TestAdd2;

Var
  R : TUnresolvedReference;

begin
  R:=SetupARef1A(Self);
  If (UnrA=Nil) then
    Fail('UnresolvedInstance A not set');
  AssertSame('TUnresolvedReference FRoot is rootA',RootA,R.FRoot);
  AssertSame('TUnresolvedReference FPropInfo is PropA1',PropA1,R.FPropInfo);
  AssertEquals('TUnresolvedReference FGlobal is rootB','RootB',R.FGlobal);
  AssertEquals('TUnresolvedReference FRelative is A','A',R.FRelative);
  AssertSame('Unresolved is root object',TUnresolvedinstance(UnrA).RootUnresolved,R);
end;

procedure TTestResolveReference.TestAdd3;

Var
  R1 : TUnresolvedReference;
  R2 : TUnresolvedReference;

begin
  R1:=SetupARef1A(Self);
  R2:=SetupARef2B(Self);
  AssertSame('TUnresolvedReference FRoot is rootA',RootA,R2.FRoot);
  AssertSame('TUnresolvedReference FPropInfo is PropA2',PropA2,R2.FPropInfo);
  AssertEquals('TUnresolvedReference FGlobal is rootB','RootB',R2.FGlobal);
  AssertEquals('TUnresolvedReference FRelative is A','B',R2.FRelative);
  AssertSame('Unresolved references are chained',R1,R2.Next);
end;

procedure TTestResolveReference.TestFixupReferenceNames1;

Var
  L : TStringList;
begin
  SetupARef1A(Self);
  L:=TstringList.Create;
  try
    GetFixupReferenceNames(RootA,L);
    AssertEquals('Number of referenced components in root component RootA is 1',1,L.Count);
    AssertEquals('Root component referred to is RootB','RootB',L[0]);
  finally
    L.Free;
  end;
end;

procedure TTestResolveReference.TestFixupReferenceNames2;

Var
  L : TStringList;
begin
  // Should result in 1 referenced name only.
  SetupARef1A(Self);
  SetupARef2B(Self);
  L:=TstringList.Create;
  try
    GetFixupReferenceNames(RootA,L);
    AssertEquals('Number of referenced components in root component RootA is 1',1,L.Count);
    AssertEquals('Root component referred to is always RootB','RootB',L[0]);
  finally
    L.Free;
  end;
end;

procedure TTestResolveReference.TestFixupReferenceNames3;

Var
  L : TStringList;
begin
  // Should result in 1 referenced name only.
  SetupARef1A(Self);
  SetupARef2B(Self);
  L:=TstringList.Create;
  try
    GetFixupReferenceNames(RootB,L);
    AssertEquals('Number of referenced components in root component RootB is 0',0,L.Count);
  finally
    L.Free;
  end;
end;

//procedure GetFixupInstanceNames(Root: TComponent; const ReferenceRootName: string; Names: TStrings);

procedure TTestResolveReference.TestFixupInstanceNames1;

Var
  L : TStringList;
begin
  SetupARef1A(Self);
  L:=TstringList.Create;
  try
    GetFixupinstanceNames(RootA,'RootB',L);
    AssertEquals('Number of references in RootA to component RootB is 1',1,L.Count);
    AssertEquals('Subcomponent of RootB referenced is A','A',L[0]);
  finally
    L.Free;
  end;
end;

procedure TTestResolveReference.TestFixupInstanceNames2;

Var
  L : TStringList;
begin
  SetupARef1A(Self);
  SetupARef2B(Self);
  L:=TstringList.Create;
  try
    GetFixupinstanceNames(RootA,'RootB',L);
    AssertEquals('Number of references in RootA to component RootB is 2',2,L.Count);
    If L.IndexOf('A')=-1 then
      Fail('A is not in list of references to RootB');
    If L.IndexOf('B')=-1 then
      Fail('B is not in list of references to RootB');
  finally
    L.Free;
  end;
end;

procedure TTestResolveReference.TestFixupInstanceNames3;

Var
  L : TStringList;
begin
  SetupARef1A(Self);
  SetupARef2B(Self);
  L:=TstringList.Create;
  try
    GetFixupinstanceNames(RootA,'RootA',L);
    AssertEquals('Number of references in RootA to component RootA is 0',0,L.Count);
  finally
    L.Free;
  end;
end;

procedure TTestResolveReference.TestFixupInstanceNames4;

Var
  L : TStringList;
begin
  SetupARef1A(Self);
  SetupARef2B(Self);
  L:=TstringList.Create;
  try
    GetFixupinstanceNames(RootB,'RootB',L);
    AssertEquals('Number of references in RootB to component RootB is 0',0,L.Count);
  finally
    L.Free;
  end;
end;

procedure TTestResolveReference.TestFixupInstanceNames5;

Var
  L : TStringList;
begin
  SetupARef1A(Self);
  SetupBRef2B(Self);
  L:=TstringList.Create;
  try
    GetFixupinstanceNames(RootB,'RootB',L);
    AssertEquals('Number of references in RootB to component RootB is 0',0,L.Count);
  finally
    L.Free;
  end;
end;

// procedure RedirectFixupReferences(Root: TComponent; const OldRootName, NewRootName: string);

procedure TTestResolveReference.TestRedirectFixupReferences1;

Var
  L : TStringList;
  R1 : TUnresolvedReference;
  R2 : TUnresolvedReference;
  
begin
  R1:=SetupARef1A(Self);
  R2:=SetupARef2B(Self);
  RedirectFixupReferences(RootA,'RootB','RootC');
  AssertEquals('Redirected R1.Root is RootC','RootC',R1.FGLobal);
  AssertEquals('Redirected R1.Root is RootC','RootC',R2.FGLobal);
end;

procedure TTestResolveReference.TestRedirectFixupReferences2;

Var
  L : TStringList;
  R1 : TUnresolvedReference;
  R2 : TUnresolvedReference;

begin
  R1:=SetupARef1A(Self);
  R2:=SetupBRef2B(Self);
  RedirectFixupReferences(RootA,'RootB','RootC');
  AssertEquals('Redirected R1.Root is RootC','RootC',R1.FGLobal);
  AssertEquals('R2.Root is not redirected, remains RootA','RootA',R2.FGLobal);
end;

procedure TTestResolveReference.TestRedirectFixupReferences3;

Var
  R1,R2 : TUnresolvedReference;

begin
  R1:=SetupARef1A(Self);
  R2:=SetupARef2B(Self);
  RedirectFixupReferences(RootA,'RootC','RootQ');
  AssertEquals('R1.Root is not redirected, remains RootB','RootB',R1.FGLobal);
  AssertEquals('R2.Root is not redirected, remains RootB','RootB',R2.FGLobal);
end;

// procedure RemoveFixupReferences(Root: TComponent; const RootName: string);
procedure TTestResolveReference.TestRemoveFixupReferences1;

begin
  SetupARef1A(Self);
  SetupARef2A(Self);
  RemoveFixupReferences(RootA,'RootB');
  AssertSame('No references left',Nil,NeedResolving.Root);
end;

procedure TTestResolveReference.TestRemoveFixupReferences2;

Var
  RA,RB : TUnresolvedInstance;
  R1,R2 : TUnresolvedReference;

begin
  RA:=RootAToResolveList(Self);
  RB:=RootBToResolveList(Self);
  R1:=SetupARef1A(Self);
  R2:=SetupBRef2A(Self);
  RemoveFixupReferences(RootA,'RootB');
  AssertSame('1 reference left',RB,NeedResolving.Root);
end;


procedure TTestResolveReference.TestFixupReferences1;

begin
  SetupARef1A(Self);
  GlobalFixupReferences;
  AssertSame('RootA.Ref1 resolved to RootB.A',RootB.FindComponent('A'),RootA.Ref1);
  AssertEquals('No more resolving needs to be done',0,NeedResolving.Count);
end;

procedure TTestResolveReference.TestFixupReferences2;

Var
  RI : TUnresolvedInstance;
  UR : TUnresolvedReference;

begin
  // Add Not existing
  RI:=RootBToResolveList(Self);
  UR:=RI.AddReference(RootB,PropB1,'RootC','A');
  // Add existing
  SetupARef1A(Self);
  GlobalFixupReferences;
  AssertSame('RootA.Ref1 resolved to RootB.A',RootB.FindComponent('A'),RootA.Ref1);
  AssertSame('Reference to RootC unresolved',RI,NeedResolving.Root);
end;

procedure TTestResolveReference.TestFixupReferences3;

Var
  RI : TUnresolvedInstance;
  UR : TUnresolvedReference;

begin
  // Add Not existing
  RI:=RootAToResolveList(Self);
  UR:=RI.AddReference(RootA,PropA2,'RootC','A');
  // Add existing
  SetupARef1A(Self);
  GlobalFixupReferences;
  AssertSame('RootA.Ref1 resolved to RootB.A',RootB.FindComponent('A'),RootA.Ref1);
  AssertSame('Reference to RootC unresolved',RI,NeedResolving.Root);
  AssertSame('Reference to RootC unresolved',RI.RootUnresolved,UR);
end;

initialization
  RegisterTest(TTestResolveReference);
  InitCriticalSection(ResolveSection);
  
finalization
  FreeAndNil(NeedResolving);
  DoneCriticalsection(ResolveSection);
end.


unit utc.opf.mementotracker;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.ChangeTracking, Opf.Memento, Opf.MementoTracker,
  Opf.Registry, Opf.Manager;

type

  { TMtCountedMemento - memento that counts its own destruction }

  TMtCountedMemento = class(TOPMemento)
  public
    // Count this memento as freed.
    destructor Destroy; override;
  end;

  { TMtPerson - subject that hands out its own memento }

  TMtPerson = class(TPersistent, IOPMementoSource)
  private
    FAge: Integer;
    FID: Integer;
    FName: String;
  public
    // Hand out a memento of the current state.
    function CreateMemento: TOPMemento;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TMtEmployee - descendant that inherits CreateMemento without overriding it }

  TMtEmployee = class(TMtPerson)
  private
    FSalary: Integer;
  published
    property Salary: Integer read FSalary write FSalary;
  end;

  { TMtPlain - subject without a memento of its own }

  TMtPlain = class(TPersistent)
  private
    FAge: Integer;
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TMtOptOut - subject that refuses to be tracked }

  TMtOptOut = class(TPersistent, IOPMementoSource)
  private
    FAge: Integer;
    FID: Integer;
    FName: String;
  public
    // Refuse tracking by handing out no memento.
    function CreateMemento: TOPMemento;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TMtPersonMemento - hand-written memento for TMtPerson }

  TMtPersonMemento = class(TMtCountedMemento)
  private
    FAge: Integer;
    FName: String;
  protected
    function GetSubjectClass: TClass; override;
    procedure Capture(aSubject: TObject); override;
    procedure DoRestore(aSubject: TObject); override;
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
  end;

  { TMtPlainMemento - memento the stand-in factory builds for TMtPlain }

  TMtPlainMemento = class(TMtCountedMemento)
  private
    FAge: Integer;
    FName: String;
  protected
    function GetSubjectClass: TClass; override;
    procedure Capture(aSubject: TObject); override;
    procedure DoRestore(aSubject: TObject); override;
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
  end;

  { TTestMementoTracker - tests for TOPMementoTracker }

  TTestMementoTracker = class(TTestCase)
  private
    FTracker: TOPMementoTracker;
    FFieldMap: TOPFieldMapList;
    FSavedFactory: TOPMementoFactory;
    function NewTracker: TOPMementoTracker;
    function MakePerson: TMtPerson;
    function MakePlain: TMtPlain;
    function MakeOptOut: TMtOptOut;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FreshTrackerHasDefaults;
    procedure UnchangedObjectIsTrackedWithoutNames;
    procedure OneChangedPropertyIsListedAlone;
    procedure TwoChangedPropertiesAreListed;
    procedure ChangedNamesBuildPartialFieldMap;
    procedure ClassWithoutMementoIsNotTracked;
    procedure ClassOptingOutIsNotTracked;
    procedure OptingOutDoesNotReachTheFactory;
    procedure DefaultFactoryMakesTheState;
    procedure DefaultFactoryOffLeavesObjectUntracked;
    procedure InterfaceBeatsTheFactory;
    procedure MarkedNameIsReported;
    procedure RecaptureClearsChangesAndMarks;
    procedure RestoredUserMementoLeavesNothingToWrite;
    procedure DiscardFreesTheMemento;
    procedure PurgeFreesTheMemento;
    procedure PurgeAllFreesTheMemento;
    procedure FreeingTrackedObjectFreesItsMemento;
    procedure FreeingTrackerFreesEveryMemento;
    procedure NoneModeMakesNoEntry;
    procedure ExcludedClassMakesNoEntry;
    procedure ExplicitModeWithoutRegistrationMakesNoEntry;
    procedure EvictionFreesTheOldestMemento;
    procedure InheritedCreateMementoRaises;
    procedure ConstructorAttachesToTheDefaultManager;
  end;

  { TTestMementoManagerTracker - the answer TOPManager.GetDirtyFields gives
    while a memento tracker is attached to it }

  TTestMementoManagerTracker = class(TTestCase)
  private
    FManager: TOPManager;
    FRegistry: TOPRegistry;
    FTracker: TOPMementoTracker;
    FPerson: TMtPerson;
    FPlain: TMtPlain;
    FPersonDef: TPersisterDef;
    FPlainDef: TPersisterDef;
    FSavedFactory: TOPMementoFactory;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TrackedClassGivesThePartialUpdate;
    procedure UntrackedClassGivesTheFullUpdate;
  end;

implementation

{ Counters the tests read to see what the tracker did behind their back. }

var
  MementosFreed: Integer;
  FactoryCalls: Integer;
  FactorySubject: TObject;
  FactoryFieldMap: TOPFieldMapList;

procedure CreateMtFieldMap(aMap: TOPFieldMapList);

var
  lField: TOPFieldMap;

begin
  lField:=aMap.Add as TOPFieldMap;
  lField.PropertyName:='ID';
  lField.FieldName:='id';
  lField.PropertyKind:=tkInteger;
  lField.Options:=[foInKey,foInSelect];

  lField:=aMap.Add as TOPFieldMap;
  lField.PropertyName:='Name';
  lField.FieldName:='name';
  lField.PropertyKind:=tkAString;
  lField.Options:=[foInSelect,foInInsert,foInUpdate];

  lField:=aMap.Add as TOPFieldMap;
  lField.PropertyName:='Age';
  lField.FieldName:='age';
  lField.PropertyKind:=tkInteger;
  lField.Options:=[foInSelect,foInInsert,foInUpdate];
end;


function NamesHave(const aNames: TUTF8StringArray; const aName: UTF8String): Boolean;

var
  lIndex: Integer;

begin
  Result:=False;
  for lIndex:=0 to Length(aNames)-1 do
    if SameText(aNames[lIndex],aName) then
      Exit(True);
end;


function MtTestFactory(aSubject: TObject; aFieldMap: TOPFieldMapList): TOPMemento;

begin
  Inc(FactoryCalls);
  FactorySubject:=aSubject;
  FactoryFieldMap:=aFieldMap;
  Result:=TMtPlainMemento.Create(aSubject);
end;


{ TMtCountedMemento }

destructor TMtCountedMemento.Destroy;

begin
  Inc(MementosFreed);
  inherited Destroy;
end;


{ TMtPerson }

function TMtPerson.CreateMemento: TOPMemento;

begin
  Result:=TMtPersonMemento.Create(Self);
end;


{ TMtOptOut }

function TMtOptOut.CreateMemento: TOPMemento;

begin
  Result:=Nil;
end;


{ TMtPersonMemento }

function TMtPersonMemento.GetSubjectClass: TClass;

begin
  Result:=TMtPerson;
end;


procedure TMtPersonMemento.Capture(aSubject: TObject);

var
  lPerson: TMtPerson;

begin
  lPerson:=TMtPerson(aSubject);
  FName:=lPerson.FName;
  FAge:=lPerson.FAge;
end;


procedure TMtPersonMemento.DoRestore(aSubject: TObject);

var
  lPerson: TMtPerson;

begin
  lPerson:=TMtPerson(aSubject);
  lPerson.FName:=FName;
  lPerson.FAge:=FAge;
end;


procedure TMtPersonMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

var
  lPerson: TMtPerson;

begin
  lPerson:=TMtPerson(aSubject);
  aNames.AddIf(FName<>lPerson.FName,'Name');
  aNames.AddIf(FAge<>lPerson.FAge,'Age');
end;


{ TMtPlainMemento }

function TMtPlainMemento.GetSubjectClass: TClass;

begin
  Result:=TMtPlain;
end;


procedure TMtPlainMemento.Capture(aSubject: TObject);

var
  lPlain: TMtPlain;

begin
  lPlain:=TMtPlain(aSubject);
  FName:=lPlain.FName;
  FAge:=lPlain.FAge;
end;


procedure TMtPlainMemento.DoRestore(aSubject: TObject);

var
  lPlain: TMtPlain;

begin
  lPlain:=TMtPlain(aSubject);
  lPlain.FName:=FName;
  lPlain.FAge:=FAge;
end;


procedure TMtPlainMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

var
  lPlain: TMtPlain;

begin
  lPlain:=TMtPlain(aSubject);
  aNames.AddIf(FName<>lPlain.FName,'Name');
  aNames.AddIf(FAge<>lPlain.FAge,'Age');
end;


{ TTestMementoTracker }

procedure TTestMementoTracker.SetUp;

begin
  FSavedFactory:=OPDefaultMementoFactory;
  OPDefaultMementoFactory:=Nil;
  MementosFreed:=0;
  FactoryCalls:=0;
  FactorySubject:=Nil;
  FactoryFieldMap:=Nil;
  FTracker:=NewTracker;
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateMtFieldMap(FFieldMap);
end;


procedure TTestMementoTracker.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FFieldMap);
  OPDefaultMementoFactory:=FSavedFactory;
end;


function TTestMementoTracker.NewTracker: TOPMementoTracker;

begin
  Result:=TOPMementoTracker.Create(Nil);
  // The constructor attaches to the global manager singleton, which this suite does not use.
  Result.Manager:=Nil;
end;


function TTestMementoTracker.MakePerson: TMtPerson;

begin
  Result:=TMtPerson.Create;
  Result.ID:=1;
  Result.Name:='Alice';
  Result.Age:=30;
end;


function TTestMementoTracker.MakePlain: TMtPlain;

begin
  Result:=TMtPlain.Create;
  Result.ID:=2;
  Result.Name:='Bob';
  Result.Age:=40;
end;


function TTestMementoTracker.MakeOptOut: TMtOptOut;

begin
  Result:=TMtOptOut.Create;
  Result.ID:=3;
  Result.Name:='Carol';
  Result.Age:=50;
end;


procedure TTestMementoTracker.FreshTrackerHasDefaults;

begin
  AssertTrue('Default factory used',FTracker.UseDefaultFactory);
  AssertTrue('Implicit mode',FTracker.Mode=tmImplicit);
  AssertFalse('TrackUnobserved off',FTracker.TrackUnobserved);
  AssertEquals('Unlimited table',0,FTracker.MaxEntries);
end;


procedure TTestMementoTracker.UnchangedObjectIsTrackedWithoutNames;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Nothing changed',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.OneChangedPropertyIsListedAlone;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Changed name','Age',lNames[0]);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.TwoChangedPropertiesAreListed;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    lPerson.Name:='Alicia';
    lPerson.Age:=31;
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Two names',2,Length(lNames));
    AssertTrue('Name reported',NamesHave(lNames,'Name'));
    AssertTrue('Age reported',NamesHave(lNames,'Age'));
  finally
    lPerson.Free;
  end;
end;


{ BuildChangedFieldMapList is what the save path uses, so the map it returns is
  the observable form of a partial update. }

procedure TTestMementoTracker.ChangedNamesBuildPartialFieldMap;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;
  lMap: TOPFieldMapList;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    lMap:=BuildChangedFieldMapList(FFieldMap,lNames);
    try
      AssertEquals('Key field and changed field only',2,lMap.Count);
      AssertEquals('Key field kept','id',lMap[0].FieldName);
      AssertEquals('Changed field kept','age',lMap[1].FieldName);
    finally
      lMap.Free;
    end;
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.ClassWithoutMementoIsNotTracked;

var
  lPlain: TMtPlain;
  lNames: TUTF8StringArray;

begin
  lPlain:=MakePlain;
  try
    FTracker.Capture(lPlain,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPlain,FFieldMap,lNames));
    AssertEquals('No names',0,Length(lNames));
  finally
    lPlain.Free;
  end;
end;


procedure TTestMementoTracker.ClassOptingOutIsNotTracked;

var
  lOptOut: TMtOptOut;
  lNames: TUTF8StringArray;

begin
  lOptOut:=MakeOptOut;
  try
    FTracker.Capture(lOptOut,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lOptOut,FFieldMap,lNames));
    AssertEquals('No names',0,Length(lNames));
  finally
    lOptOut.Free;
  end;
end;


procedure TTestMementoTracker.OptingOutDoesNotReachTheFactory;

var
  lOptOut: TMtOptOut;
  lNames: TUTF8StringArray;

begin
  OPDefaultMementoFactory:=@MtTestFactory;
  lOptOut:=MakeOptOut;
  try
    FTracker.Capture(lOptOut,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lOptOut,FFieldMap,lNames));
    AssertEquals('Factory not called',0,FactoryCalls);
  finally
    lOptOut.Free;
  end;
end;


procedure TTestMementoTracker.DefaultFactoryMakesTheState;

var
  lPlain: TMtPlain;
  lNames: TUTF8StringArray;

begin
  OPDefaultMementoFactory:=@MtTestFactory;
  lPlain:=MakePlain;
  try
    FTracker.Capture(lPlain,FFieldMap);
    AssertEquals('Factory called once',1,FactoryCalls);
    AssertSame('Factory saw the object',lPlain,FactorySubject);
    AssertSame('Factory saw the field map',FFieldMap,FactoryFieldMap);
    lPlain.Age:=41;
    AssertTrue('Object tracked',FTracker.GetChanges(lPlain,FFieldMap,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Changed name','Age',lNames[0]);
  finally
    lPlain.Free;
  end;
end;


procedure TTestMementoTracker.DefaultFactoryOffLeavesObjectUntracked;

var
  lPlain: TMtPlain;
  lNames: TUTF8StringArray;

begin
  OPDefaultMementoFactory:=@MtTestFactory;
  FTracker.UseDefaultFactory:=False;
  lPlain:=MakePlain;
  try
    FTracker.Capture(lPlain,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPlain,FFieldMap,lNames));
    AssertEquals('Factory not called',0,FactoryCalls);
  finally
    lPlain.Free;
  end;
end;


procedure TTestMementoTracker.InterfaceBeatsTheFactory;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  OPDefaultMementoFactory:=@MtTestFactory;
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Factory not called',0,FactoryCalls);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.MarkedNameIsReported;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    FTracker.MarkDirty(lPerson,'Name');
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Marked name','Name',lNames[0]);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.RecaptureClearsChangesAndMarks;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    FTracker.MarkDirty(lPerson,'Name');
    FTracker.Capture(lPerson,FFieldMap);
    AssertEquals('Replaced memento freed',1,MementosFreed);
    AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Nothing left to write',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


{ A memento the user takes himself never touches the one the tracker holds. }

procedure TTestMementoTracker.RestoredUserMementoLeavesNothingToWrite;

var
  lPerson: TMtPerson;
  lMemento: TOPMemento;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    lMemento:=lPerson.CreateMemento;
    try
      FTracker.Capture(lPerson,FFieldMap);
      lPerson.Name:='Alicia';
      AssertTrue('Object tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
      AssertEquals('One name before the restore',1,Length(lNames));
      lMemento.RestoreTo(lPerson);
      AssertTrue('Still tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
      AssertEquals('Nothing left to write',0,Length(lNames));
    finally
      lMemento.Free;
    end;
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.DiscardFreesTheMemento;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    FTracker.Discard(lPerson);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Memento freed',1,MementosFreed);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.PurgeFreesTheMemento;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    FTracker.Purge(lPerson);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Memento freed',1,MementosFreed);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.PurgeAllFreesTheMemento;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    FTracker.PurgeAll;
    AssertFalse('Object not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Memento freed',1,MementosFreed);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.FreeingTrackedObjectFreesItsMemento;

var
  lFirst, lSecond: TMtPerson;
  lNames: TUTF8StringArray;

begin
  lFirst:=MakePerson;
  try
    lSecond:=MakePerson;
    try
      FTracker.Capture(lFirst,FFieldMap);
      FTracker.Capture(lSecond,FFieldMap);
      FreeAndNil(lSecond);
      AssertEquals('Memento of the freed object freed',1,MementosFreed);
      AssertTrue('Other object still tracked',FTracker.GetChanges(lFirst,FFieldMap,lNames));
    finally
      lSecond.Free;
    end;
  finally
    lFirst.Free;
  end;
end;


procedure TTestMementoTracker.FreeingTrackerFreesEveryMemento;

var
  lTracker: TOPMementoTracker;
  lFirst, lSecond: TMtPerson;

begin
  lFirst:=Nil;
  lSecond:=Nil;
  lTracker:=NewTracker;
  try
    lFirst:=MakePerson;
    lSecond:=MakePerson;
    lTracker.Capture(lFirst,FFieldMap);
    lTracker.Capture(lSecond,FFieldMap);
    AssertEquals('Nothing freed yet',0,MementosFreed);
    FreeAndNil(lTracker);
    AssertEquals('Every held memento freed',2,MementosFreed);
  finally
    lTracker.Free;
    lFirst.Free;
    lSecond.Free;
  end;
end;


procedure TTestMementoTracker.NoneModeMakesNoEntry;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  FTracker.Mode:=tmNone;
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No memento made',0,MementosFreed);
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.ExcludedClassMakesNoEntry;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  FTracker.ExcludeClass(TMtPerson);
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.ExplicitModeWithoutRegistrationMakesNoEntry;

var
  lPerson: TMtPerson;
  lNames: TUTF8StringArray;

begin
  FTracker.Mode:=tmExplicit;
  lPerson:=MakePerson;
  try
    FTracker.Capture(lPerson,FFieldMap);
    AssertFalse('Object not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    FTracker.RegisterClass(TMtPerson);
    FTracker.Capture(lPerson,FFieldMap);
    AssertTrue('Registered class tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestMementoTracker.EvictionFreesTheOldestMemento;

var
  lFirst, lSecond: TMtPerson;
  lNames: TUTF8StringArray;

begin
  FTracker.MaxEntries:=1;
  lFirst:=MakePerson;
  try
    lSecond:=MakePerson;
    try
      FTracker.Capture(lFirst,FFieldMap);
      FTracker.Capture(lSecond,FFieldMap);
      AssertFalse('Oldest object untracked',FTracker.GetChanges(lFirst,FFieldMap,lNames));
      AssertTrue('Newest object tracked',FTracker.GetChanges(lSecond,FFieldMap,lNames));
      AssertEquals('Evicted memento freed',1,MementosFreed);
    finally
      lSecond.Free;
    end;
  finally
    lFirst.Free;
  end;
end;


{ TMtEmployee inherits CreateMemento, so the memento it gets back declares
  TMtPerson and the exact-class check refuses it. }

procedure TTestMementoTracker.InheritedCreateMementoRaises;

var
  lEmployee: TMtEmployee;
  lNames: TUTF8StringArray;
  lRaised: Boolean;

begin
  lEmployee:=TMtEmployee.Create;
  try
    lEmployee.ID:=4;
    lEmployee.Name:='Dave';
    lEmployee.Age:=60;
    lRaised:=False;
    try
      FTracker.Capture(lEmployee,FFieldMap);
    except
      on E: EOPFMemento do
        lRaised:=True;
    end;
    AssertTrue('EOPFMemento travelled out of Capture',lRaised);
    AssertEquals('Refused memento freed',1,MementosFreed);
    AssertFalse('Object not tracked',FTracker.GetChanges(lEmployee,FFieldMap,lNames));
  finally
    lEmployee.Free;
  end;
end;


procedure TTestMementoTracker.ConstructorAttachesToTheDefaultManager;

var
  lTracker: TOPMementoTracker;
  lAttached: IOPChangeTracker;

begin
  TOPManager.ClearInstance;
  try
    lTracker:=TOPMementoTracker.Create(Nil);
    try
      AssertTrue('Tracker exposes IOPChangeTracker',
        lTracker.GetInterface(SGUIDIOPChangeTracker,lAttached));
      AssertTrue('The constructor attached it to the default manager',
        OPManager.ChangeTracker=lAttached);
    finally
      lTracker.Free;
    end;
    AssertTrue('ChangeTracker nil after the tracker is freed',OPManager.ChangeTracker=Nil);
  finally
    TOPManager.ClearInstance;
  end;
end;


{ TTestMementoManagerTracker }

procedure TTestMementoManagerTracker.SetUp;

begin
  inherited SetUp;
  FSavedFactory:=OPDefaultMementoFactory;
  OPDefaultMementoFactory:=Nil;
  FRegistry:=TOPRegistry.Create(Nil);
  FPersonDef:=FRegistry.RegisterPersister(TMtPerson,Nil);
  FPersonDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateMtFieldMap(FPersonDef.FieldMap);
  FPlainDef:=FRegistry.RegisterPersister(TMtPlain,Nil);
  FPlainDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateMtFieldMap(FPlainDef.FieldMap);
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FTracker:=TOPMementoTracker.Create(Nil);
  // The constructor attaches to the global manager singleton, so detach before attaching here.
  FTracker.Manager:=Nil;
  FTracker.Manager:=FManager;
  FPerson:=TMtPerson.Create;
  FPerson.ID:=1;
  FPerson.Name:='Alice';
  FPerson.Age:=30;
  FPlain:=TMtPlain.Create;
  FPlain.ID:=2;
  FPlain.Name:='Bob';
  FPlain.Age:=40;
end;


procedure TTestMementoManagerTracker.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FRegistry);
  FreeAndNil(FPerson);
  FreeAndNil(FPlain);
  OPDefaultMementoFactory:=FSavedFactory;
  inherited TearDown;
end;


procedure TTestMementoManagerTracker.TrackedClassGivesThePartialUpdate;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,FPersonDef.FieldMap);
  FPerson.Age:=31;
  AssertTrue('Answered by the memento tracker',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('One name changed',1,Length(lNames));
  AssertEquals('Age changed','Age',lNames[0]);
end;


{ A class with no memento and no factory is not tracked, and the manager
  answering False is what makes the save path write every field. }

procedure TTestMementoManagerTracker.UntrackedClassGivesTheFullUpdate;

var
  lNames: TUTF8StringArray;

begin
  AssertTrue('No default factory assigned',OPDefaultMementoFactory=Nil);
  FTracker.Capture(FPlain,FPlainDef.FieldMap);
  FPlain.Age:=41;
  AssertFalse('Not tracked',FManager.GetDirtyFields(FPlain,lNames));
  AssertEquals('No names returned',0,Length(lNames));
end;


initialization
  RegisterTest(TTestMementoTracker);
  RegisterTest(TTestMementoManagerTracker);

end.

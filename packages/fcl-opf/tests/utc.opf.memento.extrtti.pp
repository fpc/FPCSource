unit utc.opf.memento.extrtti;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Variants,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, Variants, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Consts, Opf.Fields, Opf.ChangeTracking, Opf.Memento,
  Opf.MementoTracker, Opf.Nullable, Opf.Registry, Opf.Manager,
  Opf.Persister.ExtRTTI, Opf.Memento.ExtRTTI;

type
  TRmTag = (rmRed, rmGreen, rmBlue);
  TRmTags = set of TRmTag;

  { TRmChild - child object of a class-valued property }

  TRmChild = class(TPersistent)
  private
    FLabel: String;
  published
    property Caption: String read FLabel write FLabel;
  end;

  { TRmPerson - subject with one property of every kind the memento must handle }

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])
                  PROPERTIES([vcPublic,vcPublished])}
  TRmPerson = class(TPersistent)
  private
    FActive: Boolean;
    FAge: Integer;
    FChild: TRmChild;
    FExtra: Variant;
    FID: Integer;
    FName: String;
    FNickname: String;
    FNotes: TStrings;
    FNullAge: TNullableInteger;
    FNullName: TNullableString;
    FScore: Double;
    FSpouse: TRmChild;
    FTags: TRmTags;
  public
    // Create the subject with an empty note list, a spouse and no child.
    constructor Create;
    // Free the note list, the spouse and the child.
    destructor Destroy; override;
    // Scalar property described by extended RTTI only.
    property Nickname: String read FNickname write FNickname;
    // Read-only view of the nickname, a member a memento cannot write.
    property Alias: String read FNickname;
    // Class-valued property described by extended RTTI only.
    property Spouse: TRmChild read FSpouse;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
    property Score: Double read FScore write FScore;
    property Active: Boolean read FActive write FActive;
    property Tags: TRmTags read FTags write FTags;
    property Extra: Variant read FExtra write FExtra;
    property Notes: TStrings read FNotes;
    property Child: TRmChild read FChild write FChild;
  end;

  { TRmEmployee - descendant of the class a memento was taken from }

  TRmEmployee = class(TRmPerson)
  private
    FSalary: Integer;
  published
    property Salary: Integer read FSalary write FSalary;
  end;
  {$RTTI EXPLICIT FIELDS([vcPublished]) PROPERTIES([vcPublished])}

  { TRmNotified - subject that counts the restore notifications it receives }

  TRmNotified = class(TPersistent, IOPMementoRestored)
  private
    FName: String;
    FRestoreCount: Integer;
  public
    // Count one notification.
    procedure MementoRestored;
    // Number of notifications received so far.
    property RestoreCount: Integer read FRestoreCount;
  published
    property Name: String read FName write FName;
  end;

  { TTestRttiMemento - tests for TOPRttiMemento }

  TTestRttiMemento = class(TTestCase)
  private
    FMap: TOPFieldMapList;
    FPerson: TRmPerson;
    FMemento: TOPRttiMemento;
    // Run aBlock and return the message of the EOPFMemento it raised, or an empty string.
    function CaughtMessage(aBlock: TRunMethod): String;
    function NewMemento: TOPRttiMemento;
    function Changes: TUTF8StringArray;
    // Capture with a field map holding only aName of kind aKind.
    procedure CaptureOnly(const aName: UTF8String; aKind: TTypeKind);
    // Capture aName and 'Name' together.
    procedure CaptureWithName(const aName: UTF8String);
    procedure CallCreateFromNilSubject;
    procedure CallOneArgumentCreate;
    procedure CallIsChangedOnDescendant;
    procedure CallRestoreToNil;
    procedure CallRestoreToDescendant;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NilSubjectRaises;
    procedure OneArgumentCreateRaises;
    procedure SubjectClassIsTheCapturedClass;
    procedure DescendantSubjectRaises;
    procedure UntouchedSubjectHasNoChanges;
    procedure ChangedNameIsListedAlone;
    procedure ChangedFloatAndBooleanAreListed;
    procedure ChangedSetIsListed;
    procedure ChangedVariantIsListed;
    procedure UntouchedVariantIsNotListed;
    procedure ChangedKeyOnlyFieldIsNotListed;
    procedure EditedClassPropertyIsListed;
    procedure UntouchedClassPropertyIsNotListed;
    procedure AssignedChildIsListed;
    procedure EditedChildIsListed;
    procedure UntouchedChildIsNotListed;
    procedure ChildBecomingNilIsListed;
    procedure UntouchedPublicPropertyIsNotListed;
    procedure ChangedPublicPropertyIsListed;
    procedure UntouchedPublicClassPropertyIsNotListed;
    procedure EditedPublicClassPropertyIsListed;
    procedure UnreadablePropertiesAreListed;
    procedure ChangedNullableIsListed;
    procedure TwoNullsWithDifferentPayloadAreEqual;
    procedure NullBecomingAValueIsListed;
    procedure ValueBecomingNullIsListed;
    procedure RestoredNameIsTheCapturedValue;
    procedure RestoreLeavesNothingChanged;
    procedure RestoredFloatBooleanAndSetAreTheCapturedValues;
    procedure RestoredVariantIsTheCapturedValue;
    procedure RestoredKeyOnlyFieldIsTheCapturedValue;
    procedure RestoredNullableIsTheCapturedPayload;
    procedure RestoredClearedNullableHasItsValueBack;
    procedure RestoredNullableBecomesNullAgain;
    procedure RestoredNullableStringIsTheCapturedPayload;
    procedure RestoredClearedNullableStringHasItsValueBack;
    procedure RestoredNullableStringBecomesNullAgain;
    procedure RestoredPublicPropertyIsTheCapturedValue;
    procedure RestoredStringListHasTheCapturedText;
    procedure RestoredPublicClassPropertyKeepsItsInstance;
    procedure RestoredChildKeepsItsInstance;
    procedure RestoredReplacedChildCarriesTheCapturedCaption;
    procedure RestoreLeavesANilChildNil;
    procedure RestoreLeavesAChildAssignedAfterCaptureAlone;
    procedure RestoreSkipsAReadOnlyMember;
    procedure RestoreSkipsAnUnresolvableName;
    procedure RestoringTwiceGivesTheSameResult;
    procedure RestoreToNilRaises;
    procedure RestoreToDescendantRaises;
    procedure RestoreNotifiesTheSubject;
  end;

  { TTestRttiMementoTracker - the generic memento as the tracker uses it }

  TTestRttiMementoTracker = class(TTestCase)
  private
    FMap: TOPFieldMapList;
    FPerson: TRmPerson;
    FTracker: TOPMementoTracker;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FactoryIsAssigned;
    procedure ClassWithoutMementoIsTracked;
    procedure FactoryOffLeavesObjectUntracked;
    procedure RestoreIntoATrackedObjectLeavesNothingToWrite;
  end;

  { TTestRttiMementoManager - the answer TOPManager.GetDirtyFields gives for a
    class with no memento while a memento tracker is attached to it }

  TTestRttiMementoManager = class(TTestCase)
  private
    FManager: TOPManager;
    FRegistry: TOPRegistry;
    FTracker: TOPMementoTracker;
    FPerson: TRmPerson;
    FPersonDef: TPersisterDef;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ClassWithoutMementoGivesThePartialUpdate;
  end;

implementation

procedure CreateRmFieldMap(aMap: TOPFieldMapList);

var
  lPersister: TExtRTTIPersister;
  lField: TOPFieldMap;

begin
  lPersister:=TExtRTTIPersister.Create;
  try
    aMap.DefaultFieldOptions:=[foInSelect,foInInsert,foInUpdate];
    lPersister.PopulateFieldMap(TRmPerson,aMap);
  finally
    lPersister.Free;
  end;
  lField:=aMap.FindByPropertyName('ID');
  if lField=Nil then
    raise Exception.Create('The field map of TRmPerson has no ID entry');
  lField.Options:=[foInKey,foInSelect];
end;


{ A copy of aText in a heap block of its own. }

function HeapString(const aText: String): String;

begin
  SetLength(Result,Length(aText));
  Move(aText[1],Result[1],Length(aText));
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


{ TRmPerson }

constructor TRmPerson.Create;

begin
  inherited Create;
  FNotes:=TStringList.Create;
  FSpouse:=TRmChild.Create;
end;


destructor TRmPerson.Destroy;

begin
  FreeAndNil(FChild);
  FreeAndNil(FSpouse);
  FreeAndNil(FNotes);
  inherited Destroy;
end;


{ TRmNotified }

procedure TRmNotified.MementoRestored;

begin
  Inc(FRestoreCount);
end;


{ TTestRttiMemento }

procedure TTestRttiMemento.SetUp;

begin
  inherited SetUp;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateRmFieldMap(FMap);
  FPerson:=TRmPerson.Create;
  FPerson.ID:=1;
  FPerson.Name:='Alice';
  FPerson.Age:=30;
  FPerson.Score:=12.5;
  FPerson.Active:=True;
  FPerson.Tags:=[rmRed];
  FPerson.Notes.Add('first');
end;


procedure TTestRttiMemento.TearDown;

begin
  FreeAndNil(FMemento);
  FreeAndNil(FPerson);
  FreeAndNil(FMap);
  inherited TearDown;
end;


function TTestRttiMemento.CaughtMessage(aBlock: TRunMethod): String;

begin
  Result:='';
  try
    aBlock();
  except
    on E: EOPFMemento do
      Result:=E.Message;
  end;
end;


function TTestRttiMemento.NewMemento: TOPRttiMemento;

begin
  FreeAndNil(FMemento);
  FMemento:=TOPRttiMemento.CreateFromFieldMap(FPerson,FMap);
  Result:=FMemento;
end;


function TTestRttiMemento.Changes: TUTF8StringArray;

begin
  Result:=FMemento.GetChanges(FPerson);
end;


procedure TTestRttiMemento.CaptureOnly(const aName: UTF8String; aKind: TTypeKind);

var
  lField: TOPFieldMap;

begin
  FreeAndNil(FMemento);
  FreeAndNil(FMap);
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
  lField:=FMap.AddField(LowerCase(aName),aName,[foInSelect,foInUpdate]);
  lField.PropertyKind:=aKind;
  FMemento:=TOPRttiMemento.CreateFromFieldMap(FPerson,FMap);
end;


procedure TTestRttiMemento.CaptureWithName(const aName: UTF8String);

begin
  FreeAndNil(FMemento);
  FreeAndNil(FMap);
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
  FMap.AddField(LowerCase(aName),aName,[foInSelect,foInUpdate]);
  FMap.AddField('name','Name',[foInSelect,foInUpdate]);
  FMemento:=TOPRttiMemento.CreateFromFieldMap(FPerson,FMap);
end;


procedure TTestRttiMemento.CallCreateFromNilSubject;

begin
  TOPRttiMemento.CreateFromFieldMap(Nil,FMap).Free;
end;


procedure TTestRttiMemento.CallOneArgumentCreate;

begin
  TOPRttiMemento.Create(FPerson).Free;
end;


procedure TTestRttiMemento.CallIsChangedOnDescendant;

var
  lEmployee: TRmEmployee;

begin
  lEmployee:=TRmEmployee.Create;
  try
    FMemento.IsChanged(lEmployee);
  finally
    lEmployee.Free;
  end;
end;


procedure TTestRttiMemento.CallRestoreToNil;

begin
  FMemento.RestoreTo(Nil);
end;


procedure TTestRttiMemento.CallRestoreToDescendant;

var
  lEmployee: TRmEmployee;

begin
  lEmployee:=TRmEmployee.Create;
  try
    FMemento.RestoreTo(lEmployee);
  finally
    lEmployee.Free;
  end;
end;


procedure TTestRttiMemento.NilSubjectRaises;

var
  lMessage: String;

begin
  lMessage:=CaughtMessage(@CallCreateFromNilSubject);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names the Nil subject',Pos(SErrMementoNilSubject,lMessage)>0);
end;


procedure TTestRttiMemento.OneArgumentCreateRaises;

var
  lMessage: String;

begin
  lMessage:=CaughtMessage(@CallOneArgumentCreate);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names the missing class',
    Pos(Format(SErrMementoNoClass,['TOPRttiMemento']),lMessage)>0);
end;


procedure TTestRttiMemento.SubjectClassIsTheCapturedClass;

begin
  AssertTrue('Subject class is the class captured from',NewMemento.SubjectClass=TRmPerson);
end;


procedure TTestRttiMemento.DescendantSubjectRaises;

var
  lMessage: String;

begin
  NewMemento;
  lMessage:=CaughtMessage(@CallIsChangedOnDescendant);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names declared and subject class',
    Pos(Format(SErrMementoClassMismatch,['TRmPerson','TRmEmployee']),lMessage)>0);
end;


procedure TTestRttiMemento.UntouchedSubjectHasNoChanges;

begin
  NewMemento;
  AssertEquals('Nothing changed',0,Length(Changes));
end;


procedure TTestRttiMemento.ChangedNameIsListedAlone;

var
  lNames: TUTF8StringArray;

begin
  NewMemento;
  FPerson.Name:='Bob';
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Name reported',NamesHave(lNames,'Name'));
end;


procedure TTestRttiMemento.ChangedFloatAndBooleanAreListed;

var
  lNames: TUTF8StringArray;

begin
  NewMemento;
  FPerson.Score:=99.5;
  FPerson.Active:=False;
  lNames:=Changes;
  AssertEquals('Two names reported',2,Length(lNames));
  AssertTrue('Score reported',NamesHave(lNames,'Score'));
  AssertTrue('Active reported',NamesHave(lNames,'Active'));
end;


procedure TTestRttiMemento.ChangedSetIsListed;

var
  lNames: TUTF8StringArray;

begin
  NewMemento;
  FPerson.Tags:=[rmRed,rmBlue];
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Tags reported',NamesHave(lNames,'Tags'));
end;


procedure TTestRttiMemento.ChangedVariantIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.Extra:=5;
  NewMemento;
  FPerson.Extra:=7;
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Extra reported',NamesHave(lNames,'Extra'));
end;


procedure TTestRttiMemento.UntouchedVariantIsNotListed;

begin
  FPerson.Extra:='same';
  NewMemento;
  AssertEquals('An untouched variant is not a change',0,Length(Changes));
end;


procedure TTestRttiMemento.ChangedKeyOnlyFieldIsNotListed;

begin
  NewMemento;
  FPerson.ID:=7;
  AssertEquals('A field without foInUpdate is not compared',0,Length(Changes));
end;


procedure TTestRttiMemento.EditedClassPropertyIsListed;

var
  lNames: TUTF8StringArray;

begin
  NewMemento;
  FPerson.Notes.Add('second');
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Notes reported',NamesHave(lNames,'Notes'));
end;


procedure TTestRttiMemento.UntouchedClassPropertyIsNotListed;

begin
  NewMemento;
  FPerson.Notes.Text:=FPerson.Notes.Text;
  AssertEquals('Same content is not a change',0,Length(Changes));
end;


procedure TTestRttiMemento.AssignedChildIsListed;

var
  lNames: TUTF8StringArray;

begin
  NewMemento;
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Child reported',NamesHave(lNames,'Child'));
end;


procedure TTestRttiMemento.EditedChildIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  FPerson.Child.Caption:='two';
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Child reported',NamesHave(lNames,'Child'));
end;


procedure TTestRttiMemento.UntouchedChildIsNotListed;

begin
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  AssertEquals('An untouched child is not a change',0,Length(Changes));
end;


procedure TTestRttiMemento.ChildBecomingNilIsListed;

var
  lChild: TRmChild;
  lNames: TUTF8StringArray;

begin
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  lChild:=FPerson.Child;
  FPerson.Child:=Nil;
  try
    lNames:=Changes;
  finally
    lChild.Free;
  end;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Child reported',NamesHave(lNames,'Child'));
end;


procedure TTestRttiMemento.UntouchedPublicPropertyIsNotListed;

begin
  FPerson.Nickname:='Al';
  CaptureOnly('Nickname',tkAString);
  AssertEquals('An untouched public property is not a change',0,Length(Changes));
end;


procedure TTestRttiMemento.ChangedPublicPropertyIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.Nickname:='Al';
  CaptureOnly('Nickname',tkAString);
  FPerson.Nickname:='Bert';
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Nickname reported',NamesHave(lNames,'Nickname'));
end;


procedure TTestRttiMemento.UntouchedPublicClassPropertyIsNotListed;

begin
  FPerson.Spouse.Caption:='one';
  CaptureOnly('Spouse',tkClass);
  AssertEquals('An untouched public class property is not a change',0,Length(Changes));
end;


procedure TTestRttiMemento.EditedPublicClassPropertyIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.Spouse.Caption:='one';
  CaptureOnly('Spouse',tkClass);
  FPerson.Spouse.Caption:='two';
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Spouse reported',NamesHave(lNames,'Spouse'));
end;


{ A name the subject does not carry cannot be read, and an unreadable value
  counts as changed. }

procedure TTestRttiMemento.UnreadablePropertiesAreListed;

var
  lMap: TOPFieldMapList;
  lMemento: TOPRttiMemento;
  lField: TOPFieldMap;
  lNames: TUTF8StringArray;

begin
  lMap:=TOPFieldMapList.Create(TOPFieldMap);
  try
    lField:=lMap.AddField('missing','Missing',[foInSelect,foInUpdate]);
    lField.PropertyKind:=tkClass;
    lField:=lMap.AddField('absent','Absent',[foInSelect,foInUpdate]);
    lField.PropertyKind:=tkInteger;
    lMemento:=TOPRttiMemento.CreateFromFieldMap(FPerson,lMap);
    try
      lNames:=lMemento.GetChanges(FPerson);
      AssertEquals('Two names reported',2,Length(lNames));
      AssertTrue('Missing reported',NamesHave(lNames,'Missing'));
      AssertTrue('Absent reported',NamesHave(lNames,'Absent'));
    finally
      lMemento.Free;
    end;
  finally
    lMap.Free;
  end;
end;


procedure TTestRttiMemento.ChangedNullableIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.FNullAge.Value:=5;
  NewMemento;
  FPerson.FNullAge.Value:=7;
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('NullAge reported',NamesHave(lNames,'NullAge'));
end;


procedure TTestRttiMemento.TwoNullsWithDifferentPayloadAreEqual;

begin
  FPerson.FNullAge.Value:=5;
  FPerson.FNullAge.Clear;
  NewMemento;
  FPerson.FNullAge.Value:=7;
  FPerson.FNullAge.Clear;
  AssertEquals('Two nulls are the same value',0,Length(Changes));
end;


procedure TTestRttiMemento.NullBecomingAValueIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.FNullAge.Clear;
  NewMemento;
  FPerson.FNullAge.Value:=7;
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('NullAge reported',NamesHave(lNames,'NullAge'));
end;


procedure TTestRttiMemento.ValueBecomingNullIsListed;

var
  lNames: TUTF8StringArray;

begin
  FPerson.FNullAge.Value:=5;
  NewMemento;
  FPerson.FNullAge.Clear;
  lNames:=Changes;
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('NullAge reported',NamesHave(lNames,'NullAge'));
end;


procedure TTestRttiMemento.RestoredNameIsTheCapturedValue;

begin
  NewMemento;
  FPerson.Name:='Bob';
  FMemento.RestoreTo(FPerson);
  AssertEquals('Captured name written back','Alice',FPerson.Name);
end;


procedure TTestRttiMemento.RestoreLeavesNothingChanged;

begin
  FPerson.Extra:=42;
  FPerson.FNullAge.Value:=5;
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  FPerson.Name:='Bob';
  FPerson.Age:=31;
  FPerson.Score:=99.5;
  FPerson.Active:=False;
  FPerson.Tags:=[rmRed,rmBlue];
  FPerson.Extra:='text';
  FPerson.FNullAge.Value:=7;
  FPerson.Notes.Add('second');
  FPerson.Child.Caption:='two';
  FMemento.RestoreTo(FPerson);
  AssertEquals('Nothing left changed',0,Length(Changes));
end;


procedure TTestRttiMemento.RestoredFloatBooleanAndSetAreTheCapturedValues;

begin
  NewMemento;
  FPerson.Score:=99.5;
  FPerson.Active:=False;
  FPerson.Tags:=[rmRed,rmBlue];
  FMemento.RestoreTo(FPerson);
  AssertEquals('Score restored',12.5,FPerson.Score,0.0001);
  AssertTrue('Active restored',FPerson.Active);
  AssertTrue('Tags restored',FPerson.Tags=[rmRed]);
end;


procedure TTestRttiMemento.RestoredVariantIsTheCapturedValue;

begin
  FPerson.Extra:=42;
  NewMemento;
  FPerson.Extra:='text';
  FMemento.RestoreTo(FPerson);
  AssertEquals('Extra restored',42,Integer(FPerson.Extra));
end;


procedure TTestRttiMemento.RestoredKeyOnlyFieldIsTheCapturedValue;

begin
  NewMemento;
  FPerson.ID:=7;
  FMemento.RestoreTo(FPerson);
  AssertEquals('A key field is restored as well',1,FPerson.ID);
end;


procedure TTestRttiMemento.RestoredNullableIsTheCapturedPayload;

begin
  FPerson.FNullAge.Value:=5;
  NewMemento;
  FPerson.FNullAge.Value:=7;
  FMemento.RestoreTo(FPerson);
  AssertTrue('NullAge still has a value',FPerson.FNullAge.HasValue);
  AssertEquals('NullAge restored',5,FPerson.FNullAge.Value);
end;


procedure TTestRttiMemento.RestoredClearedNullableHasItsValueBack;

begin
  FPerson.FNullAge.Value:=5;
  NewMemento;
  FPerson.FNullAge.Clear;
  FMemento.RestoreTo(FPerson);
  AssertTrue('NullAge has a value again',FPerson.FNullAge.HasValue);
  AssertEquals('NullAge restored',5,FPerson.FNullAge.Value);
end;


procedure TTestRttiMemento.RestoredNullableBecomesNullAgain;

begin
  FPerson.FNullAge.Clear;
  NewMemento;
  FPerson.FNullAge.Value:=7;
  FMemento.RestoreTo(FPerson);
  AssertFalse('NullAge has no value again',FPerson.FNullAge.HasValue);
end;


procedure TTestRttiMemento.RestoredNullableStringIsTheCapturedPayload;

begin
  FPerson.FNullName.Value:=HeapString('Alice');
  NewMemento;
  FPerson.FNullName.Value:=HeapString('Bob');
  FMemento.RestoreTo(FPerson);
  AssertTrue('NullName still has a value',FPerson.FNullName.HasValue);
  AssertEquals('NullName restored','Alice',FPerson.FNullName.Value);
end;


procedure TTestRttiMemento.RestoredClearedNullableStringHasItsValueBack;

begin
  FPerson.FNullName.Value:=HeapString('Alice');
  NewMemento;
  FPerson.FNullName.Clear;
  FMemento.RestoreTo(FPerson);
  AssertTrue('NullName has a value again',FPerson.FNullName.HasValue);
  AssertEquals('NullName restored','Alice',FPerson.FNullName.Value);
end;


procedure TTestRttiMemento.RestoredNullableStringBecomesNullAgain;

begin
  FPerson.FNullName.Clear;
  NewMemento;
  FPerson.FNullName.Value:=HeapString('Bob');
  FMemento.RestoreTo(FPerson);
  AssertFalse('NullName has no value again',FPerson.FNullName.HasValue);
end;


procedure TTestRttiMemento.RestoredPublicPropertyIsTheCapturedValue;

begin
  FPerson.Nickname:='Al';
  CaptureOnly('Nickname',tkAString);
  FPerson.Nickname:='Bert';
  FMemento.RestoreTo(FPerson);
  AssertEquals('Nickname restored','Al',FPerson.Nickname);
end;


procedure TTestRttiMemento.RestoredStringListHasTheCapturedText;

var
  lText: String;

begin
  NewMemento;
  lText:=FPerson.Notes.Text;
  FPerson.Notes.Add('second');
  FMemento.RestoreTo(FPerson);
  AssertEquals('Notes restored',lText,FPerson.Notes.Text);
end;


procedure TTestRttiMemento.RestoredPublicClassPropertyKeepsItsInstance;

var
  lSpouse: TRmChild;

begin
  FPerson.Spouse.Caption:='one';
  CaptureOnly('Spouse',tkClass);
  lSpouse:=FPerson.Spouse;
  FPerson.Spouse.Caption:='two';
  FMemento.RestoreTo(FPerson);
  AssertEquals('Caption restored','one',FPerson.Spouse.Caption);
  AssertTrue('The spouse is the same instance',FPerson.Spouse=lSpouse);
end;


procedure TTestRttiMemento.RestoredChildKeepsItsInstance;

var
  lChild: TRmChild;

begin
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  lChild:=FPerson.Child;
  FPerson.Child.Caption:='two';
  FMemento.RestoreTo(FPerson);
  AssertEquals('Caption restored','one',FPerson.Child.Caption);
  AssertTrue('The child is the same instance',FPerson.Child=lChild);
end;


procedure TTestRttiMemento.RestoredReplacedChildCarriesTheCapturedCaption;

var
  lChild: TRmChild;

begin
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  FPerson.Child.Free;
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='two';
  lChild:=FPerson.Child;
  FMemento.RestoreTo(FPerson);
  AssertEquals('Caption restored','one',FPerson.Child.Caption);
  AssertTrue('The child assigned after the capture is written into',FPerson.Child=lChild);
end;


procedure TTestRttiMemento.RestoreLeavesANilChildNil;

var
  lChild: TRmChild;

begin
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='one';
  NewMemento;
  lChild:=FPerson.Child;
  FPerson.Child:=Nil;
  try
    FMemento.RestoreTo(FPerson);
  finally
    lChild.Free;
  end;
  AssertTrue('No child was created',FPerson.Child=Nil);
end;


procedure TTestRttiMemento.RestoreLeavesAChildAssignedAfterCaptureAlone;

begin
  NewMemento;
  FPerson.Child:=TRmChild.Create;
  FPerson.Child.Caption:='two';
  FMemento.RestoreTo(FPerson);
  AssertTrue('The child is still there',Assigned(FPerson.Child));
  AssertEquals('The child was left untouched','two',FPerson.Child.Caption);
end;


{ A read-only member cannot be written; the entries beside it still are. }

procedure TTestRttiMemento.RestoreSkipsAReadOnlyMember;

begin
  FPerson.Nickname:='Al';
  CaptureWithName('Alias');
  FPerson.Nickname:='Bert';
  FPerson.Name:='Bob';
  FMemento.RestoreTo(FPerson);
  AssertEquals('The read-only property was left alone','Bert',FPerson.Alias);
  AssertEquals('The writable property was restored','Alice',FPerson.Name);
end;


{ A name the subject does not carry cannot be written; the entries beside it
  still are. }

procedure TTestRttiMemento.RestoreSkipsAnUnresolvableName;

begin
  CaptureWithName('Absent');
  FPerson.Name:='Bob';
  FMemento.RestoreTo(FPerson);
  AssertEquals('The writable property was restored','Alice',FPerson.Name);
end;


procedure TTestRttiMemento.RestoringTwiceGivesTheSameResult;

begin
  NewMemento;
  FPerson.Name:='Bob';
  FMemento.RestoreTo(FPerson);
  FMemento.RestoreTo(FPerson);
  AssertEquals('Captured name written back','Alice',FPerson.Name);
  AssertEquals('Nothing left changed',0,Length(Changes));
end;


procedure TTestRttiMemento.RestoreToNilRaises;

var
  lMessage: String;

begin
  NewMemento;
  FPerson.Name:='Bob';
  lMessage:=CaughtMessage(@CallRestoreToNil);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names the Nil subject',Pos(SErrMementoNilSubject,lMessage)>0);
  AssertEquals('No state was written back','Bob',FPerson.Name);
end;


procedure TTestRttiMemento.RestoreToDescendantRaises;

var
  lMessage: String;

begin
  NewMemento;
  FPerson.Name:='Bob';
  lMessage:=CaughtMessage(@CallRestoreToDescendant);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names declared and subject class',
    Pos(Format(SErrMementoClassMismatch,['TRmPerson','TRmEmployee']),lMessage)>0);
  AssertEquals('No state was written back','Bob',FPerson.Name);
end;


procedure TTestRttiMemento.RestoreNotifiesTheSubject;

var
  lMap: TOPFieldMapList;
  lMemento: TOPRttiMemento;
  lSubject: TRmNotified;

begin
  lSubject:=TRmNotified.Create;
  try
    lSubject.Name:='Alice';
    lMap:=TOPFieldMapList.Create(TOPFieldMap);
    try
      lMap.AddField('name','Name',[foInSelect,foInUpdate]);
      lMemento:=TOPRttiMemento.CreateFromFieldMap(lSubject,lMap);
      try
        lSubject.Name:='Bob';
        lMemento.RestoreTo(lSubject);
      finally
        lMemento.Free;
      end;
    finally
      lMap.Free;
    end;
    AssertEquals('Captured name written back','Alice',lSubject.Name);
    AssertEquals('Notified once',1,lSubject.RestoreCount);
  finally
    lSubject.Free;
  end;
end;


{ TTestRttiMementoTracker }

procedure TTestRttiMementoTracker.SetUp;

begin
  inherited SetUp;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateRmFieldMap(FMap);
  FPerson:=TRmPerson.Create;
  FPerson.ID:=1;
  FPerson.Name:='Alice';
  FPerson.Age:=30;
  FTracker:=TOPMementoTracker.Create(Nil);
  // The constructor attaches to the global manager singleton, which this suite does not use.
  FTracker.Manager:=Nil;
end;


procedure TTestRttiMementoTracker.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FPerson);
  FreeAndNil(FMap);
  inherited TearDown;
end;


procedure TTestRttiMementoTracker.FactoryIsAssigned;

begin
  AssertTrue('Default memento factory assigned',Assigned(OPDefaultMementoFactory));
end;


procedure TTestRttiMementoTracker.ClassWithoutMementoIsTracked;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,FMap);
  FPerson.Age:=31;
  AssertTrue('Object is tracked',FTracker.GetChanges(FPerson,FMap,lNames));
  AssertEquals('One name reported',1,Length(lNames));
  AssertTrue('Age reported',NamesHave(lNames,'Age'));
end;


procedure TTestRttiMementoTracker.FactoryOffLeavesObjectUntracked;

var
  lNames: TUTF8StringArray;

begin
  FTracker.UseDefaultFactory:=False;
  FTracker.Capture(FPerson,FMap);
  FPerson.Age:=31;
  AssertFalse('Object is not tracked',FTracker.GetChanges(FPerson,FMap,lNames));
end;


{ A memento taken at the moment the tracker captured leaves the object equal to
  the tracker's reference state after a restore. }

procedure TTestRttiMementoTracker.RestoreIntoATrackedObjectLeavesNothingToWrite;

var
  lMemento: TOPRttiMemento;
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,FMap);
  lMemento:=TOPRttiMemento.CreateFromFieldMap(FPerson,FMap);
  try
    FPerson.Age:=31;
    lMemento.RestoreTo(FPerson);
  finally
    lMemento.Free;
  end;
  AssertTrue('Object is tracked',FTracker.GetChanges(FPerson,FMap,lNames));
  AssertEquals('Nothing left to write',0,Length(lNames));
end;


{ TTestRttiMementoManager }

procedure TTestRttiMementoManager.SetUp;

begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  FPersonDef:=FRegistry.RegisterPersister(TRmPerson,Nil);
  FPersonDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateRmFieldMap(FPersonDef.FieldMap);
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FTracker:=TOPMementoTracker.Create(Nil);
  // The constructor attaches to the global manager singleton, so detach before attaching here.
  FTracker.Manager:=Nil;
  FTracker.Manager:=FManager;
  FPerson:=TRmPerson.Create;
  FPerson.ID:=1;
  FPerson.Name:='Alice';
  FPerson.Age:=30;
end;


procedure TTestRttiMementoManager.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FRegistry);
  FreeAndNil(FPerson);
  inherited TearDown;
end;


procedure TTestRttiMementoManager.ClassWithoutMementoGivesThePartialUpdate;

var
  lNames: TUTF8StringArray;

begin
  AssertTrue('Default memento factory assigned',Assigned(OPDefaultMementoFactory));
  FTracker.Capture(FPerson,FPersonDef.FieldMap);
  FPerson.Age:=31;
  AssertTrue('Answered by the memento tracker',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('One name changed',1,Length(lNames));
  AssertEquals('Age changed','Age',lNames[0]);
end;


initialization
  RegisterTest(TTestRttiMemento);
  RegisterTest(TTestRttiMementoTracker);
  RegisterTest(TTestRttiMementoManager);

end.

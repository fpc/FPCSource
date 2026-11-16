unit utc.opf.memento;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Consts, Opf.Fields, Opf.ChangeTracking, Opf.Memento;

type

  { TMemPerson - memento subject that counts calls to its property setters }

  TMemPerson = class(TPersistent, IOPMementoSource)
  private
    FAge: Integer;
    FName: String;
    FSetterCalls: Integer;
    procedure SetAge(aValue: Integer);
    procedure SetName(const aValue: String);
  public
    // Hand out a memento of the current state.
    function CreateMemento: TOPMemento;
    // Number of times a property setter ran.
    property SetterCalls: Integer read FSetterCalls;
  published
    property Name: String read FName write SetName;
    property Age: Integer read FAge write SetAge;
  end;

  { TMemEmployee - descendant that inherits CreateMemento without overriding it }

  TMemEmployee = class(TMemPerson)
  private
    FSalary: Integer;
  published
    property Salary: Integer read FSalary write FSalary;
  end;

  { TMemCompany - unrelated class with the same properties }

  TMemCompany = class(TPersistent)
  private
    FAge: Integer;
    FName: String;
  published
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TMemNotified - subject that wants a notification after a restore }

  TMemNotified = class(TPersistent, IOPMementoSource, IOPMementoRestored)
  private
    FName: String;
    FNameAtNotify: String;
    FNotifyCount: Integer;
  public
    // Hand out a memento of the current state.
    function CreateMemento: TOPMemento;
    // Record that the memento was put back and what the name was at that moment.
    procedure MementoRestored;
    // Number of times MementoRestored ran.
    property NotifyCount: Integer read FNotifyCount;
    // Value of Name as seen by MementoRestored.
    property NameAtNotify: String read FNameAtNotify;
  published
    property Name: String read FName write FName;
  end;

  { TMemPersonMemento - hand-written memento for TMemPerson }

  TMemPersonMemento = class(TOPMemento)
  private
    FAge: Integer;
    FName: String;
  protected
    function GetSubjectClass: TClass; override;
    procedure Capture(aSubject: TObject); override;
    procedure DoRestore(aSubject: TObject); override;
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
  public
    // Captured name.
    property Name: String read FName;
    // Captured age.
    property Age: Integer read FAge;
  end;

  { TMemNotifiedMemento - hand-written memento for TMemNotified }

  TMemNotifiedMemento = class(TOPMemento)
  private
    FName: String;
  protected
    function GetSubjectClass: TClass; override;
    procedure Capture(aSubject: TObject); override;
    procedure DoRestore(aSubject: TObject); override;
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
  end;

  { TMemNoClassMemento - memento that declares no subject class }

  TMemNoClassMemento = class(TOPMemento)
  protected
    function GetSubjectClass: TClass; override;
    procedure Capture(aSubject: TObject); override;
    procedure DoRestore(aSubject: TObject); override;
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
  end;

  { TTestOPMemento - tests for TOPMemento }

  TTestOPMemento = class(TTestCase)
  private
    FPerson: TMemPerson;
    // Run aBlock and return the message of the EOPFMemento it raised, or an empty string.
    function CaughtMessage(aBlock: TRunMethod): String;
    procedure CallCreateNil;
    procedure CallCreateWrongClass;
    procedure CallCreateDescendant;
    procedure CallCreateNoClass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CaptureOnConstruction;
    procedure NilSubjectRaises;
    procedure WrongClassRaises;
    procedure InheritedCreateMementoRaises;
    procedure NoDeclaredClassRaises;
    procedure SubjectClassIsDeclaredClass;
    procedure SourceInterfaceHandsOutMemento;
    procedure UnchangedSubjectIsNotChanged;
    procedure UnchangedSubjectHasNoChanges;
    procedure ChangedPropertyIsChanged;
    procedure ChangedPropertyIsListedAlone;
    procedure TwoChangedPropertiesAreListed;
    procedure RestorePutsValuesBack;
    procedure RestoreDoesNotCallSetters;
    procedure RestoreNotifiesSubject;
    procedure FailedRestoreDoesNotNotify;
    procedure RestoreWithoutNotificationWorks;
    procedure RestoreToNilRaises;
    procedure RestoreToWrongClassRaises;
    procedure GetChangesWithNilRaises;
    procedure GetChangesWithWrongClassRaises;
    procedure IsChangedWithNilRaises;
    procedure IsChangedWithWrongClassRaises;
    procedure DefaultFactoryFollowsTheCompiler;
  end;

implementation

{ TMemPerson }

procedure TMemPerson.SetAge(aValue: Integer);

begin
  Inc(FSetterCalls);
  FAge:=aValue;
end;


procedure TMemPerson.SetName(const aValue: String);

begin
  Inc(FSetterCalls);
  FName:=aValue;
end;


function TMemPerson.CreateMemento: TOPMemento;

begin
  Result:=TMemPersonMemento.Create(Self);
end;


{ TMemNotified }

function TMemNotified.CreateMemento: TOPMemento;

begin
  Result:=TMemNotifiedMemento.Create(Self);
end;


procedure TMemNotified.MementoRestored;

begin
  Inc(FNotifyCount);
  FNameAtNotify:=FName;
end;


{ TMemPersonMemento }

function TMemPersonMemento.GetSubjectClass: TClass;

begin
  Result:=TMemPerson;
end;


procedure TMemPersonMemento.Capture(aSubject: TObject);

var
  lPerson: TMemPerson;

begin
  lPerson:=TMemPerson(aSubject);
  FName:=lPerson.FName;
  FAge:=lPerson.FAge;
end;


procedure TMemPersonMemento.DoRestore(aSubject: TObject);

var
  lPerson: TMemPerson;

begin
  lPerson:=TMemPerson(aSubject);
  lPerson.FName:=FName;
  lPerson.FAge:=FAge;
end;


procedure TMemPersonMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

var
  lPerson: TMemPerson;

begin
  lPerson:=TMemPerson(aSubject);
  aNames.AddIf(FName<>lPerson.FName,'Name');
  aNames.AddIf(FAge<>lPerson.FAge,'Age');
end;


{ TMemNotifiedMemento }

function TMemNotifiedMemento.GetSubjectClass: TClass;

begin
  Result:=TMemNotified;
end;


procedure TMemNotifiedMemento.Capture(aSubject: TObject);

begin
  FName:=TMemNotified(aSubject).FName;
end;


procedure TMemNotifiedMemento.DoRestore(aSubject: TObject);

begin
  TMemNotified(aSubject).FName:=FName;
end;


procedure TMemNotifiedMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

begin
  aNames.AddIf(FName<>TMemNotified(aSubject).FName,'Name');
end;


{ TMemNoClassMemento }

function TMemNoClassMemento.GetSubjectClass: TClass;

begin
  Result:=Nil;
end;


{$push}
{$warn 5024 off}
procedure TMemNoClassMemento.Capture(aSubject: TObject);

begin
end;


procedure TMemNoClassMemento.DoRestore(aSubject: TObject);

begin
end;


procedure TMemNoClassMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

begin
end;
{$pop}


{ TTestOPMemento }

procedure TTestOPMemento.SetUp;

begin
  Inherited SetUp;
  FPerson:=TMemPerson.Create;
  FPerson.Name:='Anna';
  FPerson.Age:=30;
end;


procedure TTestOPMemento.TearDown;

begin
  FreeAndNil(FPerson);
  Inherited TearDown;
end;


function TTestOPMemento.CaughtMessage(aBlock: TRunMethod): String;

begin
  Result:='';
  try
    aBlock();
  except
    on E: EOPFMemento do
      Result:=E.Message;
  end;
end;


procedure TTestOPMemento.CallCreateNil;

begin
  TMemPersonMemento.Create(Nil).Free;
end;


procedure TTestOPMemento.CallCreateWrongClass;

var
  lCompany: TMemCompany;

begin
  lCompany:=TMemCompany.Create;
  try
    TMemPersonMemento.Create(lCompany).Free;
  finally
    lCompany.Free;
  end;
end;


procedure TTestOPMemento.CallCreateDescendant;

var
  lEmployee: TMemEmployee;

begin
  lEmployee:=TMemEmployee.Create;
  try
    lEmployee.CreateMemento.Free;
  finally
    lEmployee.Free;
  end;
end;


procedure TTestOPMemento.CallCreateNoClass;

begin
  TMemNoClassMemento.Create(FPerson).Free;
end;


procedure TTestOPMemento.CaptureOnConstruction;

var
  lMemento: TMemPersonMemento;

begin
  lMemento:=TMemPersonMemento.Create(FPerson);
  try
    AssertEquals('Captured name','Anna',lMemento.Name);
    AssertEquals('Captured age',30,lMemento.Age);
    AssertFalse('Nothing changed yet',lMemento.IsChanged(FPerson));
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.NilSubjectRaises;

var
  lMessage: String;

begin
  lMessage:=CaughtMessage(@CallCreateNil);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names the operation',Pos('.Create:',lMessage)>0);
  AssertTrue('Message names the reason',Pos(SErrMementoNilSubject,lMessage)>0);
end;


procedure TTestOPMemento.WrongClassRaises;

var
  lMessage: String;

begin
  lMessage:=CaughtMessage(@CallCreateWrongClass);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names declared and subject class',
    Pos(Format(SErrMementoClassMismatch,['TMemPerson','TMemCompany']),lMessage)>0);
end;


procedure TTestOPMemento.InheritedCreateMementoRaises;

var
  lMessage: String;

begin
  lMessage:=CaughtMessage(@CallCreateDescendant);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names declared and subject class',
    Pos(Format(SErrMementoClassMismatch,['TMemPerson','TMemEmployee']),lMessage)>0);
end;


procedure TTestOPMemento.NoDeclaredClassRaises;

var
  lMessage: String;

begin
  lMessage:=CaughtMessage(@CallCreateNoClass);
  AssertTrue('EOPFMemento raised',lMessage<>'');
  AssertTrue('Message names the memento class',
    Pos(Format(SErrMementoNoClass,['TMemNoClassMemento']),lMessage)>0);
end;


procedure TTestOPMemento.SubjectClassIsDeclaredClass;

var
  lMemento: TOPMemento;

begin
  lMemento:=FPerson.CreateMemento;
  try
    AssertSame('SubjectClass reads GetSubjectClass',TMemPerson,lMemento.SubjectClass);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.SourceInterfaceHandsOutMemento;

var
  lSource: IOPMementoSource;
  lMemento: TOPMemento;

begin
  AssertTrue('Subject has the source interface',
    FPerson.GetInterface(SGUIDIOPMementoSource,lSource));
  lMemento:=lSource.CreateMemento;
  try
    AssertNotNull('Memento created',lMemento);
    AssertSame('Memento is for the subject class',TMemPerson,lMemento.SubjectClass);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.UnchangedSubjectIsNotChanged;

var
  lMemento: TOPMemento;

begin
  lMemento:=FPerson.CreateMemento;
  try
    AssertFalse('Untouched subject is not changed',lMemento.IsChanged(FPerson));
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.UnchangedSubjectHasNoChanges;

var
  lMemento: TOPMemento;

begin
  lMemento:=FPerson.CreateMemento;
  try
    AssertEquals('No names returned',0,Length(lMemento.GetChanges(FPerson)));
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.ChangedPropertyIsChanged;

var
  lMemento: TOPMemento;

begin
  lMemento:=FPerson.CreateMemento;
  try
    FPerson.Name:='Bea';
    AssertTrue('Changed subject is reported',lMemento.IsChanged(FPerson));
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.ChangedPropertyIsListedAlone;

var
  lMemento: TOPMemento;
  lNames: TUTF8StringArray;

begin
  lMemento:=FPerson.CreateMemento;
  try
    FPerson.Name:='Bea';
    lNames:=lMemento.GetChanges(FPerson);
    AssertEquals('One name returned',1,Length(lNames));
    AssertEquals('The changed property','Name',lNames[0]);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.TwoChangedPropertiesAreListed;

var
  lMemento: TOPMemento;
  lNames: TUTF8StringArray;

begin
  lMemento:=FPerson.CreateMemento;
  try
    FPerson.Name:='Bea';
    FPerson.Age:=44;
    lNames:=lMemento.GetChanges(FPerson);
    AssertEquals('Two names returned',2,Length(lNames));
    AssertEquals('First changed property','Name',lNames[0]);
    AssertEquals('Second changed property','Age',lNames[1]);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.RestorePutsValuesBack;

var
  lMemento: TOPMemento;

begin
  lMemento:=FPerson.CreateMemento;
  try
    FPerson.Name:='Bea';
    FPerson.Age:=44;
    lMemento.RestoreTo(FPerson);
    AssertEquals('Name restored','Anna',FPerson.Name);
    AssertEquals('Age restored',30,FPerson.Age);
    AssertFalse('Subject matches the memento again',lMemento.IsChanged(FPerson));
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.RestoreDoesNotCallSetters;

var
  lMemento: TOPMemento;
  lCount: Integer;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lCount:=FPerson.SetterCalls;
    FPerson.Name:='Bea';
    AssertEquals('Writing the property runs the setter',lCount+1,FPerson.SetterCalls);
    lCount:=FPerson.SetterCalls;
    lMemento.RestoreTo(FPerson);
    AssertEquals('Setter counter untouched',lCount,FPerson.SetterCalls);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.RestoreNotifiesSubject;

var
  lSubject: TMemNotified;
  lMemento: TOPMemento;

begin
  lSubject:=TMemNotified.Create;
  try
    lSubject.Name:='Anna';
    lMemento:=lSubject.CreateMemento;
    try
      lSubject.Name:='Bea';
      lMemento.RestoreTo(lSubject);
      AssertEquals('Notified once',1,lSubject.NotifyCount);
      AssertEquals('Notified after the restore','Anna',lSubject.NameAtNotify);
    finally
      lMemento.Free;
    end;
  finally
    lSubject.Free;
  end;
end;


procedure TTestOPMemento.FailedRestoreDoesNotNotify;

var
  lSubject: TMemNotified;
  lCompany: TMemCompany;
  lMemento: TOPMemento;
  lMessage: String;

begin
  lSubject:=TMemNotified.Create;
  try
    lSubject.Name:='Anna';
    lMemento:=lSubject.CreateMemento;
    try
      lCompany:=TMemCompany.Create;
      try
        lMessage:='';
        try
          lMemento.RestoreTo(lCompany);
        except
          on E: EOPFMemento do
            lMessage:=E.Message;
        end;
        AssertTrue('EOPFMemento raised',lMessage<>'');
        AssertEquals('Not notified after a refused restore',0,lSubject.NotifyCount);
      finally
        lCompany.Free;
      end;
    finally
      lMemento.Free;
    end;
  finally
    lSubject.Free;
  end;
end;


procedure TTestOPMemento.RestoreWithoutNotificationWorks;

var
  lMemento: TOPMemento;
  lRestored: IOPMementoRestored;

begin
  AssertFalse('Subject has no restore interface',
    FPerson.GetInterface(SGUIDIOPMementoRestored,lRestored));
  lMemento:=FPerson.CreateMemento;
  try
    FPerson.Name:='Bea';
    lMemento.RestoreTo(FPerson);
    AssertEquals('Name restored','Anna',FPerson.Name);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.RestoreToNilRaises;

var
  lMemento: TOPMemento;
  lMessage: String;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lMessage:='';
    try
      lMemento.RestoreTo(Nil);
    except
      on E: EOPFMemento do
        lMessage:=E.Message;
    end;
    AssertTrue('EOPFMemento raised',lMessage<>'');
    AssertTrue('Message names the operation',Pos('.RestoreTo:',lMessage)>0);
    AssertTrue('Message names the reason',Pos(SErrMementoNilSubject,lMessage)>0);
    AssertEquals('Subject untouched','Anna',FPerson.Name);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.RestoreToWrongClassRaises;

var
  lMemento: TOPMemento;
  lCompany: TMemCompany;
  lMessage: String;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lCompany:=TMemCompany.Create;
    try
      lCompany.Name:='Acme';
      lMessage:='';
      try
        lMemento.RestoreTo(lCompany);
      except
        on E: EOPFMemento do
          lMessage:=E.Message;
      end;
      AssertTrue('EOPFMemento raised',lMessage<>'');
      AssertTrue('Message names declared and subject class',
        Pos(Format(SErrMementoClassMismatch,['TMemPerson','TMemCompany']),lMessage)>0);
      AssertEquals('Subject untouched','Acme',lCompany.Name);
    finally
      lCompany.Free;
    end;
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.GetChangesWithNilRaises;

var
  lMemento: TOPMemento;
  lMessage: String;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lMessage:='';
    try
      lMemento.GetChanges(Nil);
    except
      on E: EOPFMemento do
        lMessage:=E.Message;
    end;
    AssertTrue('EOPFMemento raised',lMessage<>'');
    AssertTrue('Message names the operation',Pos('.GetChanges:',lMessage)>0);
    AssertTrue('Message names the reason',Pos(SErrMementoNilSubject,lMessage)>0);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.GetChangesWithWrongClassRaises;

var
  lMemento: TOPMemento;
  lCompany: TMemCompany;
  lMessage: String;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lCompany:=TMemCompany.Create;
    try
      lMessage:='';
      try
        lMemento.GetChanges(lCompany);
      except
        on E: EOPFMemento do
          lMessage:=E.Message;
      end;
      AssertTrue('EOPFMemento raised',lMessage<>'');
      AssertTrue('Message names declared and subject class',
        Pos(Format(SErrMementoClassMismatch,['TMemPerson','TMemCompany']),lMessage)>0);
    finally
      lCompany.Free;
    end;
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.IsChangedWithNilRaises;

var
  lMemento: TOPMemento;
  lMessage: String;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lMessage:='';
    try
      lMemento.IsChanged(Nil);
    except
      on E: EOPFMemento do
        lMessage:=E.Message;
    end;
    AssertTrue('EOPFMemento raised',lMessage<>'');
    AssertTrue('Message names the operation',Pos('.IsChanged:',lMessage)>0);
    AssertTrue('Message names the reason',Pos(SErrMementoNilSubject,lMessage)>0);
  finally
    lMemento.Free;
  end;
end;


procedure TTestOPMemento.IsChangedWithWrongClassRaises;

var
  lMemento: TOPMemento;
  lCompany: TMemCompany;
  lMessage: String;

begin
  lMemento:=FPerson.CreateMemento;
  try
    lCompany:=TMemCompany.Create;
    try
      lMessage:='';
      try
        lMemento.IsChanged(lCompany);
      except
        on E: EOPFMemento do
          lMessage:=E.Message;
      end;
      AssertTrue('EOPFMemento raised',lMessage<>'');
      AssertTrue('Message names declared and subject class',
        Pos(Format(SErrMementoClassMismatch,['TMemPerson','TMemCompany']),lMessage)>0);
    finally
      lCompany.Free;
    end;
  finally
    lMemento.Free;
  end;
end;


{ The factory is unassigned on 3.2.2 and assigned on a newer compiler. }

procedure TTestOPMemento.DefaultFactoryFollowsTheCompiler;

begin
{$IFDEF VER3_2}
  AssertTrue('No default memento factory assigned',OPDefaultMementoFactory=Nil);
{$ELSE}
  AssertTrue('Default memento factory assigned',OPDefaultMementoFactory<>Nil);
{$ENDIF}
end;


initialization
  RegisterTest(TTestOPMemento);

end.

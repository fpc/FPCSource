unit fpobserver;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, typinfo, contnrs;

Type

  TObservedHook = Class(TObject,IFPObserved)
  Protected
    FObservers : TFPList;
    FSender : TObject;
  Public
    // ASender will be the default sender.
    Constructor CreateSender(ASender : TObject);
    Destructor Destroy; override;
    Procedure FPOAttachObserver(AObserver : TObject);
    Procedure FPODetachObserver(AObserver : TObject);
    Procedure Changed;
    Procedure AddItem(AItem : TObject);
    Procedure DeleteItem(AItem : TObject);
    Procedure CustomNotify(Data : Pointer = Nil);
    Procedure FPONotifyObservers(ASender : TObject; AOperation : TFPObservedOperation; Data : Pointer);
    Property Sender : TObject Read FSender;
  end;

//  EObserver = Class(Exception);


  { TBaseMediator }

  TMediatingEvent = Procedure(Sender : TObject; var Handled : Boolean) of object;

  TBaseMediator = Class(TComponent,IFPObserver)
  private
    FActive: Boolean;
    FOnObjectToView: TMediatingEvent;
    FOnViewToObject: TMediatingEvent;
    FReadOnly: Boolean;
    FTransferring : Boolean;
    FSubjectPropertyName: String;
    FSubject: TObject;
    FValueList: TObjectList;
    FViewPropertyName: String;
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetValueList(const AValue: TObjectList);
    procedure SetViewPropertyName(const AValue: String); Virtual;
  Protected
    // Should return true (Default) if ViewPropertyName is published
    Class Function PublishedViewProperty : Boolean; virtual;
    // Should return true (Default) if SubjectPropertyName is published
    Class Function PublishedSubjectProperty : Boolean; virtual;
    // Set active. Descendents (such as list mediators) can override this.
    procedure SetActive(const AValue: Boolean); virtual;
    // set subject. Attaches observer and calls MaybeObjectToView
    procedure SetSubject(const AValue: TObject); virtual;
    // set subjectpropertyname. Checks if it exists, and calls MaybeObjectToView
    procedure SetSubjectPropertyName(const AValue: String); virtual;
    // Can be used in descendents to respond to onchange events
    Procedure ViewChangedHandler(Sender : TObject);  virtual;
    // Check if APropertyName is published property of AObject.
    // Only performed if both parameters are not empty.
    procedure CheckPropertyName(AObject: TObject; const APropertyName: String);
    // If all CheckObjectSubject and Active are true, call ObjectToView.
    Procedure MaybeObjectToView;
    // If all CheckObjectSubject and Active are true, call ViewToObject.
    Procedure MaybeViewToObject;
    // Check if Subject/View and property names are set up correctly.
    Function  CheckViewSubject : Boolean;
    // Override  next two for custom behaviour.
    // Copies Subject.SubjectPropertyName to View.ViewPropertyName.
    Procedure DoObjectToView; virtual;
    // Copies View.ViewPropertyName to Subject.SubjectPropertyName
    Procedure DoViewToObject; virtual;
    // Override these, and call inherited at the end.
    // Get View component. Typically a TCustomEdit instance.
    function  GetView : TObject; virtual;
    // Descendents should call this when the view changed.
    procedure ViewChanged; virtual;
    // Descendents should override this to handle changes in the value list
    procedure ValuelistChanged; virtual;
    // IFPObserver. Will call the necessary events.
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
    // Raise an error which shows more information about the control, subject and fieldname.
    Procedure RaiseMediatorError(Const Msg : String); overload;
    // Format version
    Procedure RaiseMediatorError(Const Fmt : String; Args : Array of const); overload;
    // View property that will be set by default
    Property ViewPropertyName : String Read FViewPropertyName Write SetViewPropertyName;
    // Is a copy operation View <-> Subject in progress ?
    Property Transferring : Boolean Read FTransferring;
  Public
    Destructor Destroy; override;
    // Copy subject to view. No check is done to see if all is well.
    Procedure ObjectToView;
    // Copy view to subject. No check is done to see if all is well.
    Procedure ViewToObject;
    // Minimum class that View must have to be handled by this mediator.
    class function ViewClass: TClass; virtual;
    // Composite mediator or not ?
    class function CompositeMediator : Boolean; virtual;
    // Subject. Must have IFPObserved interface
    Property Subject : TObject Read FSubject Write SetSubject;
    // View. Must have ViewPropertyName, if in use.
    Property View : TObject Read GetView;
    // Value list. To be used in mediators that use a dynamical value list
    // such as Listbox, combobox, groupbox.
    Property Valuelist : TObjectList Read FValueList Write SetValueList;
  Published
    // Property that will be copied to view.
    Property SubjectPropertyName : String Read FSubjectPropertyName Write SetSubjectPropertyName;
    // If not active, no copying is being done either way.
    Property Active : Boolean Read FActive Write SetActive;
    // If ReadOnly, only ObjectToView is used
    Property ReadOnly : Boolean Read FReadOnly Write SetReadOnly;
    // Can be used to copy data from control (view) to subject manually
    Property OnViewToObject : TMediatingEvent Read FOnViewToObject Write FOnViewToObject;
    // Can be used to copy data from control (view) to subject manually
    Property OnObjectToView : TMediatingEvent Read FOnObjectToView Write FOnObjectToView;
  end;
  TMediatorClass = Class of TBaseMediator;

  // Forward definitions
  TBaseListMediator = Class;

  { TComponentMediator }
  { General-purpose of Mediating views. Can be used on any form/component }

  TComponentMediator = Class(TBaseMediator)
    FViewComponent : TComponent;
  Protected
    function  GetView : TObject; override;
    procedure SetComponent(const AValue: TComponent);
  Public
    procedure Notification(AComponent: TComponent;  Operation: TOperation); override;
  Published
    // General component which can be set in Object Inspector
    Property ViewComponent : TComponent Read FViewComponent Write SetComponent;
    // Punlish property so it can be set in Object Inspector
    Property ViewPropertyName;
  end;

  { Event object used for OnBeforeSetupField event. Is used to allow formatting
    of fields before written to listview Caption or Items. }
  TOnBeforeSetupField = procedure(AObject: TObject; const AFieldName: string; var AValue: string) of object;

  { TListItemMediator }

  TListItemMediator = class(TObject, IFPObserver)
  private
    FSubject: TObject;
    FOnBeforeSetupField: TOnBeforeSetupField;
    FListMediator : TBaseListMediator;
    Function GetActive : Boolean;
  protected
    procedure SetSubject(const AValue: TObject); virtual;
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer); virtual;
    Procedure ObjectToView; virtual;
    Procedure ViewToObject; virtual;
  public
    destructor Destroy; override;
    procedure MaybeObjectToView;
    property OnBeforeSetupField: TOnBeforeSetupField read FOnBeforeSetupField write FOnBeforeSetupField;
    property Subject : TObject read FSubject write SetSubject;
    property Active : Boolean read GetActive;
  end;

  { TBaseListMediator - Base mediator that handles lists of objects.

    Needs a TList as subject. Items in list must have IFPObserved
    interface. It will create one (and use as subject) if passed a normal
    list or a collection.
  }

  TBaseListMediator = class(TBaseMediator)
  private
    FOnBeforeSetupField: TOnBeforeSetupField;
    FMediatorList: TFPObjectList;
    FListChanged : Boolean;
    procedure SetOnBeforeSetupField(const Value: TOnBeforeSetupField);
  protected
    // This needs to return false
    Class Function PublishedViewProperty : Boolean; override;
    // Descendents can override;
    Function AddObject(AObject: TObject; AIndex: Integer) : TListItemMediator; virtual;
    // Set all descendents to active
    procedure SetActive(const AValue: Boolean); override;
    // Must be overridden in descendents, and should return selected object
    function GetSelectedObject: TObject; virtual;
    // Must be overridden in descendents, and should set selected object
    procedure SetSelectedObject(const AValue: TObject); virtual;
    // Must be overridden in descendents to create an item mediator and add it to GUI control
    // Subject will be set after this call.
    Function CreateItemMediator(AData: TObject; ARow : integer) : TListItemMediator; virtual; abstract;
    // This frees the mediator. Descendents can override to additionally update the GUI control
    procedure DoDeleteItemMediator(AIndex : Integer; AMediator : TListItemMediator); virtual;
    // Creates a mediator for all items in the list. List Item Mediators are re-used (subject is set)
    procedure CreateSubMediators; virtual;
    // Does nothing
    procedure DoViewToObject; override;
    // Calls CreateSubMediators. Override for additional GUI setup.
    procedure DoObjectToView; override;
    // Additional checks on subject.
    procedure SetSubject(const AValue: TObject); override;

    Function FindObjectMediator(AObject : TObject; out AtIndex : Integer) : TListItemMediator;
    property MediatorList: TFPObjectList read FMediatorList;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    class function CompositeMediator: Boolean; override;
    // This should handle additional additem/deleteitem events
    Procedure ObservedChanged(ASender : TObject; Operation : TFPObservedOperation);
    // Selected item in the list.
    property SelectedObject: TObject read GetSelectedObject write SetSelectedObject;
  published
    // Event to setup fields in item mediators.
    property OnBeforeSetupField: TOnBeforeSetupField read FOnBeforeSetupField write SetOnBeforeSetupField;
  end;

  { TMediatorFieldInfo - Describe a column in a columnar list display }

  TMediatorFieldInfo = class(TCollectionItem)
  private
    FWidth: integer;
    FCaption: string;
    FPropName: string;
    FAlign: TAlignment;
    function GetCaption: string;
    procedure SetAlign(const AValue: TAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetPropName(const AValue: string);
    procedure SetWidth(const AValue: Integer);
  protected
    function GetAsString: string; virtual;
    procedure SetAsString(const AValue: string); virtual;
    Procedure Change;
  public
    procedure Assign(Source: TPersistent); override;
    // Setting this will parse everything.
    property AsString: string read GetAsString write SetAsString;
  published
    // Property Caption to be used for column head.
    property Caption: string read GetCaption write SetCaption;
    // Property Name to be displayed in column
    property PropertyName: string read FPropName write SetPropName;
    // Width of column
    property Width: Integer read FWidth write SetWidth;
    // Alignment of column
    property Alignment: TAlignment read FAlign write SetAlign default taLeftJustify;
  end;

  TColumnsListMediator = Class;

  { TMediatorFieldInfoList - Collection describing the columns in a columnar list display }

  TMediatorFieldInfoList = class(TCollection)
  private
    FMediator : TColumnsListMediator;
    function GetAsString: string;
    function GetI(Index: integer): TMediatorFieldInfo;
    procedure SetI(Index: integer; const AValue: TMediatorFieldInfo);
  protected
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
    Property Mediator : TColumnsListMediator read FMediator;
  public
    // Adding items to the collection.
    function AddFieldInfo: TMediatorFieldInfo; overload;
    function AddFieldInfo (Const APropName : String; AFieldWidth : Integer) : TMediatorFieldInfo; overload;
    function AddFieldInfo (Const APropName,ACaption : String; AFieldWidth : Integer) : TMediatorFieldInfo; overload;
    function AddFieldInfo (Const APropName,ACaption : String; AFieldWidth : Integer; AAlignment : TAlignment) : TMediatorFieldInfo; overload;
    property FieldInfo[Index: integer]: TMediatorFieldInfo read GetI write SetI; default;
    property AsString: string read GetAsString;
  end;

  { TColumnsListItemMediator - List item mediator that can handle multiple columns }

  TColumnsListItemMediator = class(TListItemMediator)
  Private
    Function GetFieldsInfo: TMediatorFieldInfoList;
  Published
    property FieldsInfo: TMediatorFieldInfoList read GetFieldsInfo;
  end;

  { TColumnsListMediator - List mediator that handles multiple columns }

  TColumnsListMediator = class(TBaseListMediator)
  Private
    FFieldsInfo: TMediatorFieldInfoList;
    procedure SetFieldsInfo(const AValue: TMediatorFieldInfoList);
    function GetDisplayNames: string;
    procedure SetDisplayNames(const AValue: string);
    procedure FieldInfoChanged(Item: TMediatorFieldInfo; Action: TCollectionNotification); virtual;
  Protected
    Class Function PublishedSubjectProperty : Boolean; override;
    procedure ParseDisplayNames(const AValue: string);
    // Called by DoViewToObject prior to creating item mediators
    procedure CreateColumns; virtual;
    // Calls CreateColumns and CreateSubMediators. Override for additional GUI setup.
    procedure DoObjectToView; override;
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    // Can be used to set the column properties in 1 statement.
    property DisplayNames: string read GetDisplayNames write SetDisplayNames;
  Published
    // How to display the columns in the list.
    property FieldsInfo: TMediatorFieldInfoList read FFieldsInfo write SetFieldsInfo;
  end;

  { TMediatorDef - Mediator Definition Storage for MediatorManager }

  TMediatorDef = class(TCollectionItem)
  private
    FMC: TMediatorClass;
    FMSC: TClass;
    FPN: string;
    FPT: TTypeKinds;
  public
    // Return True if this definition handles the Subject,Gui,APropinfo trio
    function Handles(ASubject: TObject; AGui: TComponent; APropInfo: PPropInfo): Boolean;
    // Return True if this definition matches 'closer' than M.
    // Note that both current and M must have Handles() returned true for this to be useful.
    function BetterMatch(M: TMediatorDef): Boolean;
    // Definition
    property MediatorClass: TMediatorClass read FMC write FMC;
    property MinSubjectClass: TClass read FMSC write FMSC;
    property PropertyTypes: TTypeKinds read FPT write FPT;
    property PropertyName: string read FPN write FPN;
  end;


  TMediatorDefs = class(TCollection)
  private
    function GetDef(Index: integer): TMediatorDef;
    procedure SetDef(Index: integer; const AValue: TMediatorDef);
  public
    function AddDef: TMediatorDef;
    property Defs[Index: integer]: TMediatorDef read GetDef write SetDef; default;
  end;


  TMediatorManager = class(TObject)
  private
    FDefs: TMediatorDefs;
  public
    constructor Create;
    destructor Destroy; override;
    // If APropName is empty or APropInfo is Nil, a composite mediator will be searched.
    function FindDefFor(ASubject: TObject; AGui: TComponent): TMediatorDef; overload;
    function FindDefFor(ASubject: TObject; AGui: TComponent; const APropName: string): TMediatorDef; overload;
    function FindDefFor(ASubject: TObject; AGui: TComponent; APropInfo: PPropInfo): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorClass; MinSubjectClass: TClass): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorClass; MinSubjectClass: TClass; PropertyName: string): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorClass; MinSubjectClass: TClass; PropertyTypes: TTypeKinds): TMediatorDef; overload;
    property Defs: TMediatorDefs read FDefs;
  end;

  EMediator = class(Exception);

function MediatorManager: TMediatorManager;
Procedure MediatorError(Sender : TObject; Const Msg : String); overload;
Procedure MediatorError(Sender : TObject; Const Fmt : String; Args : Array of const); overload;

implementation


Resourcestring
  SErrNotObserver = 'Instance of class %s is not an observer.';
  SErrInvalidPropertyName = '%s is not a valid published property of class %s';
  SErrObjectCannotBeObserved = 'Cannot observe an instance of class %d';
  sErrInvalidFieldName      = 'No fieldname specified for column %d';
  sErrInvalidAlignmentChar  = 'Invalid alignment character "%s" specified for column %d';
  sErrInvalidWidthSpecifier = 'Invalid with "%s" specified for column %d';
  sErrNotListObject         = '%s is not a TObjectList';
  sErrCompositeNeedsList    = '%s needs a TObjectList class but is registered with %s';
  SErrActive                = 'Operation not allowed while the mediator is active';
  SErrNoGuiFieldName        = 'no gui fieldname set';
  SErrNoSubjectFieldName    = 'no subject fieldname set';

{ ---------------------------------------------------------------------
  Mediator global routines
  ---------------------------------------------------------------------}

Procedure MediatorError(Sender : TObject; Const Msg : String); overload;

Var
  M : TBaseMediator;
  C : TComponent;
  V,S : TObject;
  CN,SN,Err : String;

begin
  if (Sender=Nil) then
    Err:=Msg
  else If Sender is TBaseMediator then
    begin
    M:=TBaseMediator(Sender);
    V:=M.View;
    S:=M.Subject;
    CN:='';
    If Assigned(V) then
      begin
      if (V is TComponent) then
        begin
        C:=TComponent(V);
        CN:=C.Name;
        end;
      If (CN='') then
        CN:=C.ClassName+' instance';
      end
    else
      CN:='Nil';
    If Assigned(S) then
      SN:=S.ClassName
    else
      SN:='Nil';
    Err:=Format('Mediator %s (%s,%s,%s) : %s',[M.ClassName,SN,CN,M.SubjectPropertyName,Msg]);
    end
  else if (Sender is TComponent) and (TComponent(Sender).Name<>'') then
    Err:=Format('%s : %s',[TComponent(Sender).Name,Msg])
  else
    Err:=Format('%s : %s',[Sender.ClassName,Msg]);
  Raise EMediator.Create(Err);
end;

Procedure MediatorError(Sender : TObject; const Fmt : String; Args : Array of const); overload;

begin
  MediatorError(Sender,Format(Fmt,Args));
end;

Var
  MM : TMediatorManager;

function MediatorManager: TMediatorManager;
begin
  if (MM = nil) then
    MM := TMediatorManager.Create;
  Result := MM;
end;

{ TObservedHook }

constructor TObservedHook.CreateSender(ASender: TObject);
begin
  FSender:=ASender;
  If FSender=Nil then
    FSender:=Self;
end;

destructor TObservedHook.Destroy;
begin
  If Assigned(FObservers) then
    begin
    FPONotifyObservers(FSender,ooFree,Nil);
    FreeAndNil(FObservers);
    end;
  inherited Destroy;
end;

procedure TObservedHook.FPOAttachObserver(AObserver: TObject);

Var
  I : IFPObserver;

begin
  If Not AObserver.GetInterface(SGUIDObserver,I) then
    Raise EObserver.CreateFmt(SErrNotObserver,[AObserver.ClassName]);
  If not Assigned(FObservers) then
    FObservers:=TFPList.Create;
  FObservers.Add(I);
end;

procedure TObservedHook.FPODetachObserver(AObserver: TObject);

Var
  I : IFPObserver;

begin
  If Not AObserver.GetInterface(SGUIDObserver,I) then
    Raise EObserver.CreateFmt(SErrNotObserver,[AObserver.ClassName]);
  If Assigned(FObservers) then
    begin
    FObservers.Remove(I);
    If (FObservers.Count=0) then
      FreeAndNil(FObservers);
    end;
end;

procedure TObservedHook.Changed;
begin
  FPONotifyObservers(Sender,ooChange,Nil)
end;

procedure TObservedHook.AddItem(AItem: TObject);
begin
  FPONotifyObservers(FSender,ooAddItem,AItem);
end;

procedure TObservedHook.DeleteItem(AItem: TObject);
begin
  FPONotifyObservers(FSender,ooDeleteItem,AItem);
end;

procedure TObservedHook.CustomNotify(Data : Pointer = Nil);
begin
  FPONotifyObservers(FSender,ooCustom,Data);
end;

procedure TObservedHook.FPONotifyObservers(ASender: TObject;  AOperation: TFPObservedOperation; Data : Pointer);

Var
  O : TObject;
  I : Integer;
  Obs : IFPObserver;

begin
  If Assigned(FObservers) then
    For I:=FObservers.Count-1 downto 0 do
      begin
      Obs:=IFPObserver(FObservers[i]);
      Obs.FPOObservedChanged(ASender,AOperation,Data);
      end;
end;

{ TBaseMediator }

function TBaseMediator.GetView: TObject;
begin
  Result:=Nil;
end;

procedure TBaseMediator.ViewChanged;
begin
  If PublishedViewProperty then
    CheckPropertyName(View,ViewPropertyName);
  MaybeObjectToView
end;

procedure TBaseMediator.ValuelistChanged;
begin
  // Do nothing
end;

procedure TBaseMediator.SetActive(const AValue: Boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  MaybeObjectToView;
end;

procedure TBaseMediator.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  MaybeObjectToView;
end;

procedure TBaseMediator.SetValueList(const AValue: TObjectList);

Var
  I : IFPObserved;

begin
  if FValueList=AValue then exit;
  If FValueList<>Nil then
      I.FPODetachObserver(Self);
  If Assigned(AValue) then
    begin
    FValueList:=AValue;
    If Assigned(AValue) then
      AValue.FPOAttachObserver(Self);
    end;
  FValueList:=AValue;
  ValueListChanged;
end;

procedure TBaseMediator.CheckPropertyName(AObject : TObject; const APropertyName : String);

begin
  If Assigned(AObject) and (APropertyName<>'') then
    If Not IsPublishedProp(AObject,APropertyName) then
      Raise EObserver.CreateFmt(SErrInvalidPropertyName,[APropertyName,AObject.ClassName]);
end;

procedure TBaseMediator.MaybeObjectToView;
begin
  If FActive and CheckViewSubject then
    ObjectToView
end;

procedure TBaseMediator.MaybeViewToObject;
begin
  If FActive and (Not ReadOnly) and CheckViewSubject then
    ViewToObject;
end;

function TBaseMediator.CheckViewSubject: Boolean;

Var
  O : TObject;

begin
  O:=GetView;
  Result:=Assigned(FSubject)
          and Assigned(O)
          and (ViewPropertyName<>'')
          and (SubjectPropertyName<>'');
end;

procedure TBaseMediator.SetSubjectPropertyName(const AValue: String);

begin
  if FSubjectPropertyName=AValue then exit;
  If PublishedSubjectProperty then
    CheckPropertyName(FSubject,AValue);
  FSubjectPropertyName:=AValue;
  MaybeObjectToView;
end;


procedure TBaseMediator.SetSubject(const AValue: TObject);

Var
  I : IFPObserved;

begin
  if FSubject=AValue then exit;
  If PublishedSubjectProperty then
    CheckPropertyName(AValue,FSubjectPropertyName);
  If FSubject<>Nil then
    If FSubject.GetInterface(SGUIDObserved,I) then
      I.FPODetachObserver(Self);
  If (AValue<>Nil) then
    begin
    If not AValue.GetInterface(SGUIDObserved,I) then
      Raise EObserver.CreateFmt(SErrObjectCannotBeObserved,[AValue.ClassName]);
    FSubject:=AValue;
    I.FPOAttachObserver(Self);
    end
  else
    FSubject:=AValue;
  MaybeObjectToView;
end;

procedure TBaseMediator.SetViewPropertyName(const AValue: String);
begin
  if FViewPropertyName=AValue then exit;
  If PublishedViewProperty then
    CheckPropertyName(GetView,AValue);
  FViewPropertyName:=AValue;
  MaybeObjectToView;
end;

class function TBaseMediator.PublishedViewProperty: Boolean;
begin
  Result:=True;
end;

class function TBaseMediator.PublishedSubjectProperty: Boolean;
begin
  Result:=True;
end;

procedure TBaseMediator.ViewChangedHandler(Sender: TObject);
begin
  MaybeViewToObject;
end;


procedure TBaseMediator.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data : Pointer);


begin
  If (ASender=FSubject) then
    begin
    If Operation=ooChange then
      MaybeObjectToView
    else if Operation=ooFree then
      FSubject:=Nil;
    end
  else if (ASender=FValueList) then
    begin
    If Operation=ooChange then
      ValueListChanged
    else if Operation=ooFree then
      FValueList:=Nil;
    end;
end;

procedure TBaseMediator.RaiseMediatorError(const Msg: String);
begin
  MediatorError(Self,Msg);
end;

procedure TBaseMediator.RaiseMediatorError(const Fmt: String;
  Args: array of const);
begin
  RaiseMediatorError(Format(FMT,Args));
end;

destructor TBaseMediator.Destroy;
begin
  Subject:=Nil;
  ValueList:=Nil;
  inherited Destroy;
end;

procedure TBaseMediator.DoObjectToView;

begin
  SetPropValue(GetView,ViewPropertyName,GetPropValue(FSubject,FSubjectPropertyName));
end;

procedure TBaseMediator.DoViewToObject;

begin
  SetPropValue(FSubject,FSubjectPropertyName,GetPropValue(GetView,ViewPropertyName));
end;

procedure TBaseMediator.ObjectToView;

Var
  B : Boolean;

begin
  If Not FTransferring then
    begin
    FTransferring:=True;
    try
      B:=False;
      If Assigned(FOnObjectToView) then
        FOnObjectToView(Self,B);
      If not B then
        DoObjectToView;
    finally
      FTransferring:=False;
    end;
    end;
end;

procedure TBaseMediator.ViewToObject;

Var
  B : Boolean;

begin
  If Not FTransferring then
    begin
    FTransferring:=True;
    try
      B:=False;
      If Assigned(FONViewToObject) then
        FONViewToObject(Self,B);
      If not B then
      DoViewToObject;
    finally
      FTransferring:=False;
    end;
    end;
end;

class function TBaseMediator.ViewClass: TClass;
begin
  Result:=TObject;
end;

class function TBaseMediator.CompositeMediator: Boolean;
begin
  Result:=False;
end;

{ TComponentMediator }

function TComponentMediator.GetView: TObject;
begin
  Result:=FViewComponent;
end;

procedure TComponentMediator.SetComponent(const AValue: TComponent);
begin
  If (Avalue=FViewComponent) then
    Exit;
  If Assigned(FViewComponent) then
    FViewComponent.RemoveFreeNotification(Self);
  FViewComponent:=AValue;
  If Assigned(FViewComponent) then
    FViewComponent.FreeNotification(Self);
  ViewChanged;
end;

procedure TComponentMediator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FViewComponent) then
    begin
    FViewComponent:=Nil;
    ViewChanged;
    end;
end;

{ TMediatorDef }

function TMediatorDef.Handles(ASubject: TObject; AGui: TComponent; APropInfo: PPropInfo): Boolean;
var
  N: string;
begin
  if (APropInfo = nil) then
    Result := FMC.CompositeMediator
  else
  begin
    N      := APropInfo^.Name;
    Result := True;
  end;
  if not Result then
    Exit; // ==>
  // At least the classes must match
  Result := AGui.InheritsFrom(FMC.ViewClass) and ASubject.InheritsFrom(FMSC);
  if Result and not FMC.CompositeMediator then
    if (PropertyName <> '') then
      Result := (CompareText(N, PropertyName) = 0)
    else // Property kind should match. Note that property MUST be set to something.
      Result := (APropInfo^.PropType^.Kind in PropertyTypes); // If PropertyName is set, it must match
end;

function TMediatorDef.BetterMatch(M: TMediatorDef): Boolean;
begin
  Result := (M = nil);
  if not Result then
  begin
    Result := (FMC.CompositeMediator = M.MediatorClass.CompositeMediator);
    if Result then
    begin
      Result := (PropertyName <> '') and (M.PropertyName = '');
      if not Result then
      begin
        // M's property matches closer
        Result := not ((M.PropertyName <> '') and (PropertyName = ''));
        if Result then
        begin
          // Properties are on equal level. Check GUI class.
          // Closer GUI class ?
          Result := FMC.ViewClass.InheritsFrom(M.MediatorClass.ViewClass);
          if not Result then
          begin
            // M's GUI class matches closer ?
            Result := not (M.MediatorClass.ViewClass.InheritsFrom(FMC.ViewClass));
            if Result then
            begin
              // GUI classes are on equal level (different branches in tree). Check subject class.
              // Closer Subject class ?
              Result := FMSC.InheritsFrom(M.FMSC);
              if not Result then
                // M's subject class matches closer ?
                Result := not M.FMSC.InheritsFrom(FMSC);
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TMediatorDefs }

function TMediatorDefs.GetDef(Index: integer): TMediatorDef;
begin
  Result := TMediatorDef(Items[Index]);
end;

procedure TMediatorDefs.SetDef(Index: integer; const AValue: TMediatorDef);
begin
  Items[Index] := AValue;
end;

function TMediatorDefs.AddDef: TMediatorDef;
begin
  Result := Add as TMediatorDef;
end;

{ TMediatorManager }

constructor TMediatorManager.Create;
begin
  FDefs := TMediatorDefs.Create(TMediatorDef);
end;

destructor TMediatorManager.Destroy;
begin
  FreeAndNil(FDefs);
  inherited Destroy;
end;

function TMediatorManager.FindDefFor(ASubject: TObject; AGui: TComponent): TMediatorDef;
begin
  Result := FindDefFor(ASubject, AGUI, PPropInfo(nil));
end;

function TMediatorManager.FindDefFor(ASubject: TObject; AGui: TComponent; const APropName: string): TMediatorDef;
var
  propinfo: PPropInfo;
begin
  propinfo := GetPropInfo(ASubject, APropName);
  Result := FindDefFor(ASubject, AGUI, propinfo);
end;

function TMediatorManager.FindDefFor(ASubject: TObject; AGui: TComponent; APropInfo: PPropInfo): TMediatorDef;
var
  D: TMediatorDef;
  I: integer;
begin
  Result := nil;
  for I := 0 to FDefs.Count - 1 do
  begin
    D := FDefs[I];
    if D.Handles(ASubject, AGUI, APropInfo) then
      if (D.BetterMatch(Result)) then
        Result := D;
  end;
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorClass; MinSubjectClass: TClass): TMediatorDef;

begin
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := tkProperties - [tkClass, tkInterface, tkDynArray, tkObject, tkInterfaceRaw];
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorClass; MinSubjectClass: TClass; PropertyName: string): TMediatorDef;

begin
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := PropertyName;
  Result.FPT  := [];
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorClass; MinSubjectClass: TClass; PropertyTypes: TTypeKinds): TMediatorDef;

begin
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := PropertyTypes;
end;

{ TListItemMediator }


function TListItemMediator.GetActive: Boolean;
begin
  Result:=False;
  If Assigned(FListMediator) then
    Result:=FListMediator.Active;
end;

procedure TListItemMediator.SetSubject(const AValue: TObject);

Var
  I : IFPObserved;

begin
  if Avalue=FSubject then
    Exit;
  If FSubject<>Nil then
    If FSubject.GetInterface(SGUIDObserved,I) then
      I.FPODetachObserver(Self);
  FSubject:=AValue;
  If (FSubject<>Nil) then
    begin
    If not FSubject.GetInterface(SGUIDObserved,I) then
      Raise EObserver.CreateFmt(SErrObjectCannotBeObserved,[FSubject.ClassName]);
    I.FPOAttachObserver(Self);
    end;
  MaybeObjectToView
end;

procedure TListItemMediator.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data : Pointer);
begin
  If Operation=ooFree then
    FSubject:=Nil
  else
    MaybeObjectToView;
end;

procedure TListItemMediator.ObjectToView;
begin
  // Do nothing
end;

procedure TListItemMediator.ViewToObject;
begin
  // Do nothing
end;

destructor TListItemMediator.Destroy;
begin
  Subject:=Nil;
  inherited Destroy;
end;

procedure TListItemMediator.MaybeObjectToView;
begin
  If Assigned(FSubject) and Active then
    ObjectToView;
end;

{ TMediatorFieldInfo }

procedure TMediatorFieldInfo.Change;

begin
  FPONotifyObservers(Self,ooChange,Nil);
end;

function TMediatorFieldInfo.GetCaption: string;
begin
  Result:=FCaption;
  If (Result='') then
    Result:=FPropName;
end;

procedure TMediatorFieldInfo.SetAlign(const AValue: TAlignment);
begin
  If AValue=fAlign then Exit;
  FAlign:=AValue;
  Change;
end;

procedure TMediatorFieldInfo.SetCaption(const AValue: string);
begin
  If AValue=Caption then Exit;
  FCaption:=AValue;
  Change;
end;

procedure TMediatorFieldInfo.SetPropName(const AValue: string);
begin
  If AValue=FPropName then Exit;
  FPropName:=AValue;
  Change;
end;

procedure TMediatorFieldInfo.SetWidth(const AValue: Integer);
begin
  If (FWidth=AValue) then Exit;
  FWidth:=AValue;
  Change;
end;

const
  AlignChars: array[TAlignMent] of char     = ('l', 'r', 'c');

function TMediatorFieldInfo.GetAsString: string;
begin
  Result := Format('%s|%s|%d|%s', [PropertyName, AlignChars[Alignment], Width, Caption]);
end;

procedure TMediatorFieldInfo.SetAsString(const AValue: string);

  Function GetToken(Var S : String) : String;

  Var
    P : Integer;

  begin
    P:=Pos('|',S);
    If P=0 then P:=Length(S)+1;
    Result:=Copy(S,1,P-1);
    Delete(S,1,P);
  end;

var
  V,S: string;
  A: TAlignment;
  I: integer;

begin
  V:=S;
  I := 0;
  PropertyName:=GetToken(V);
  if (PropertyName = '') then
    MediatorError(Self,SErrInvalidFieldName, [Index + 1]);
  Alignment:=taLeftJustify;
  Width:=50;
  S:=GetToken(V);
  if (S<>'') then
    begin
    if (length(S)<>1) then
      MediatorError(Self,SErrInvalidAlignmentChar, [S,Index+1]);
    for A := Low(Talignment) to High(TAlignment) do
      if (Upcase(AlignChars[A])=Upcase(S[1])) then
        Alignment := A;
    S:=GetToken(V);
    if (S<>'') then
      begin
      if not TryStrToInt(S,i) then
        MediatorError(Self,SErrInvalidWidthSpecifier,[S]);
      Width:=I;
      S:=getToken(V);
      if (S<>'') then
        Caption := S;
      end;
    end;
end;

procedure TMediatorFieldInfo.Assign(Source: TPersistent);

Var
  M : TMediatorFieldInfo;

begin
  if (Source is TMediatorFieldInfo) then
    begin
    M:=Source as TMediatorFieldInfo;
    FWidth:=M.FWidth;
    FCaption:=M.FCaption;
    FPropName:=M.FPropname;
    FAlign:=M.FAlign;
    end
  else
    inherited Assign(Source);
end;

{ TColumnsListItemMediator }

function TColumnsListItemMediator.GetFieldsInfo: TMediatorFieldInfoList;
begin
  If Assigned(FListmediator) and (FListMediator is TColumnsListMediator) then
    Result:=TColumnsListMediator(FListMediator).FFieldsInfo;
end;

{ TBaseListMediator }

procedure TBaseListMediator.SetOnBeforeSetupField(
  const Value: TOnBeforeSetupField);

var
  I: integer;
begin
  FOnBeforeSetupField := Value;
  for I := 0 to FMediatorList.Count - 1 do
    TListItemMediator(FMediatorList[i]).OnBeforeSetupField := Value;
end;

class function TBaseListMediator.PublishedViewProperty: Boolean;
begin
  Result:=False;
end;

procedure TBaseListMediator.SetActive(const AValue: Boolean);

Var
  i : Integer;

begin
  inherited SetActive(AValue);
  If AValue then
    For I:=0 to MediatorList.Count-1 do
      TListItemMediator(MediatorList[i]).MaybeObjectToView;
end;

function TBaseListMediator.GetSelectedObject: TObject;
begin
  Result := nil;
end;

procedure TBaseListMediator.SetSelectedObject(const AValue: TObject);
begin
  // Do nothing
end;

procedure TBaseListMediator.DoDeleteItemMediator(AIndex: Integer;
  AMediator: TListItemMediator);
begin
  MediatorList.Delete(AIndex);
end;

Function TBaseListMediator.AddObject(AObject : TObject; AIndex : Integer) : TListItemMediator;


begin
  Result:=CreateItemMediator(AObject,AIndex);
  If (Result<>Nil) then
    begin
    Result.FListMediator:=Self;
    Result.Subject:=AObject;
    MediatorList.Add(Result);
    end;
end;

procedure TBaseListMediator.CreateSubMediators;

var
  I : integer;
  Model : TObjectList;
  
begin
  Model:=Subject as TObjectList;
  for i := 0 to Model.Count - 1 do
    begin
    if i < MediatorList.Count then
      TListItemMediator(MediatorList[i]).Subject := Model[i]
    else
      AddObject(Model[i], i);
    end;
  for i := MediatorList.Count-1 downto Model.Count do
    DoDeleteItemMediator(I,TListItemMediator(MediatorList[i]));
  FListChanged:=False;
end;

procedure TBaseListMediator.DoViewToObject;
begin
  // Do nothing
end;

procedure TBaseListMediator.DoObjectToView;
begin
  CreateSubMediators;
end;

procedure TBaseListMediator.SetSubject(const AValue: TObject);

Var
  V : TOBject;

begin
  if (AValue <> nil) then
    begin
    V:=Nil;
    if (AValue is TObjectList) then
      V:=AValue
    else If (AValue is TList) then
      V:=AValue
    else If (AValue is TCollection) then
      V:=AValue;
    if (V=Nil) then
      RaiseMediatorError(SErrNotListObject, [AValue.ClassName]);
    end;
  FListChanged:=True;
  inherited SetSubject(AValue)
end;

function TBaseListMediator.FindObjectMediator(AObject: TObject; out
  AtIndex: Integer): TListItemMediator;
begin
  AtIndex:=FMediatorList.Count-1;
  While (AtIndex>=0) and (TListItemMediator(FMediatorList[AtIndex]).Subject<>AObject) do
    Dec(AtIndex);
  If (AtIndex=-1) then
    Result:=Nil
  else
    Result:=TListItemMediator(FMediatorList[AtIndex]);
end;

constructor TBaseListMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMediatorList := TFPObjectList.Create;
  Active        := False;
  ViewPropertyName:='Caption';
end;

destructor TBaseListMediator.Destroy;
begin
  FreeAndNil(FMediatorList);
  inherited Destroy;
end;

class function TBaseListMediator.CompositeMediator: Boolean;
begin
  Result:=True;
end;

procedure TBaseListMediator.ObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation);

var
  M     : TListItemMediator;
  Model : TObjectList;
  I     : Integer;

begin
  // Do not call inherited, it will rebuild the list !!
  Case Operation of
    ooAddItem    : AddObject(ASender,TObjectList(Subject).Count-1); // always at the end...
    ooDeleteItem : begin
                   M:=FindObjectMediator(ASender,I);
                   if M<>nil then
                     DoDeleteItemMediator(I,M);
                   end;
    ooChange    : begin
                   Model:=(Subject as TObjectList);
                   if FListChanged or (TObjectList(Model).Count<>MediatorList.Count) or (Model.Count=0) then // Safety measure
                     MaybeObjectToView;
                   end;
  end;

end;

{ TColumnsListMediator }

procedure TColumnsListMediator.SetFieldsInfo(
  const AValue: TMediatorFieldInfoList);
begin
  FFieldsInfo.Assign(AValue);
end;

function TColumnsListMediator.GetDisplayNames: string;
begin
  Result := FFieldsInfo.AsString;
end;

procedure TColumnsListMediator.SetDisplayNames(const AValue: string);
begin
  SubjectPropertyName:=AValue;
  ParseDisplayNames(AValue);
end;

procedure TColumnsListMediator.FieldInfoChanged(Item: TMediatorFieldInfo;
  Action: TCollectionNotification);
begin
  If Active  then
    RaiseMediatorError(SErrActive);
end;

class function TColumnsListMediator.PublishedSubjectProperty: Boolean;
begin
  Result:=False;
end;

procedure TColumnsListMediator.ParseDisplayNames(const AValue: string);

  Function GetToken(Var S : String) : String;

  Var
    P : Integer;

  begin
    P:=Pos(';',S);
    If P=0 then P:=Length(S)+1;
    Result:=Copy(S,1,P-1);
    Delete(S,1,P);
  end;

var
  I : integer;
  lField : string;
  MFI : TMediatorFieldInfo;
  A,S : String;

begin
  FFieldsInfo.Clear;
  A:=AValue;
  Repeat
    S:=GetToken(A);
    If (S<>'') then
      begin
      MFI:=FFieldsInfo.AddFieldInfo;
      MFI.AsString:=S;
      end;
  until (S='');
end;

procedure TColumnsListMediator.CreateColumns;
begin
  // Do nothing. Must be implemented by descendent objects.
end;

procedure TColumnsListMediator.DoObjectToView;
begin
  CreateColumns;
  inherited DoObjectToView;
end;

constructor TColumnsListMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldsInfo:=TMediatorFieldInfoList.create(TMediatorFieldInfo);
  SubjectPropertyName:='Caption';
end;

destructor TColumnsListMediator.Destroy;
begin
  FreeAndNil(FFieldsInfo);
  inherited Destroy;
end;

{ TMediatorFieldInfoList }

function TMediatorFieldInfoList.GetAsString: string;

var
  I: integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + ';';
    Result := Result + FieldInfo[i].AsString;
  end;
end;

function TMediatorFieldInfoList.GetI(Index: integer): TMediatorFieldInfo;
begin
  Result := TMediatorFieldInfo(Items[Index]);
end;

procedure TMediatorFieldInfoList.SetI(Index: integer;
  const AValue: TMediatorFieldInfo);

begin
  Items[Index] := AValue;
end;

procedure TMediatorFieldInfoList.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  If Assigned(FMediator) then
    FMediator.FieldInfoChanged(Item as TMediatorFieldInfo,Action)
end;

function TMediatorFieldInfoList.AddFieldInfo: TMediatorFieldInfo;
begin
  Result := Add as TMediatorFieldInfo;
end;

function TMediatorFieldInfoList.AddFieldInfo(const APropName: String;
  AFieldWidth: Integer): TMediatorFieldInfo;
begin
  Result:=AddFieldInfo();
  Result.PropertyName:=APropName;
  Result.Width:=AFieldWidth;
end;

function TMediatorFieldInfoList.AddFieldInfo(const APropName, ACaption: String;
  AFieldWidth: Integer): TMediatorFieldInfo;
begin
  Result:=AddFieldInfo(APropName,AFieldWidth);
  Result.Caption:=ACaption;
end;

function TMediatorFieldInfoList.AddFieldInfo(const APropName, ACaption: String;
  AFieldWidth: Integer; AAlignment: TAlignment): TMediatorFieldInfo;
begin
  Result:=AddFieldInfo(APropName,ACaption,AFieldWidth);
  Result.Alignment:=AAlignment;
end;

end.


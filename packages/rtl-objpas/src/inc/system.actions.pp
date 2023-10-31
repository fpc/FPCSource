{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Michael Van Canneyt
        member of the Free Pascal development team.

    Delphi compatibility unit with action(list) related types.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.Actions;

{$MODE OBJFPC}
{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.UITypes;
{$ELSE}
  SysUtils, Classes , system.uitypes;
{$ENDIF}

type
  EActionError = class(Exception);

  // Some aliases to avoid confusion
  TShortCut = {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Classes.TShortCut;
  TImageIndex = System.UITypes.TImageIndex;

  TStatusAction = (
    saNone,
    saTrivial,
    saDefault,
    saRequiredEmpty,
    saRequired,
    saValid,
    saInvalid,
    saWaiting,
    saWarning,
    saUnused,
    saCalculated,
    saError,
    saOther);

  TContainedActionList = class;
  TContainedActionListClass = class of TContainedActionList;

  TCustomShortCutList = class(TStringList)
  private
    function GetShortCut(Index: Integer): TShortCut; inline;
  public
    function IndexOfShortCut(const ShortCut: TShortCut): Integer; overload;
    function IndexOfShortCut(const ShortCut: string): Integer; overload;
    property ShortCuts[Index: Integer]: TShortCut read GetShortCut;
  end;

  { TContainedAction }

  TContainedAction = class(TBasicAction)
  private
    FActionList: TContainedActionList;
    FAutoCheck: Boolean;
    FCaption: string;
    FCategory: string;
    FChecked: Boolean;
    FDisableIfNoHandler: Boolean;
    FEnabled: Boolean;
    FGroupIndex: Integer;
    FHelpContext: THelpContext;
    FHelpKeyword: string;
    FHelpType: THelpType;
    FHint: string;
    FImageIndex: Integer;
    FOnHint: THintEvent;
    FSavedEnabledState: Boolean;
    FShortCut: TShortCut;
    FStatusAction: TStatusAction;
    FVisible: Boolean;
    FSecondaryShortCuts : TCustomShortCutList;
    function GetIndex: Integer;
    function GetSecondaryShortCuts: TCustomShortCutList;
    function IsSecondaryShortCutsStored: Boolean;
    procedure SetActionList(AValue: TContainedActionList);
    procedure SetCategory(AValue: string);
    procedure SetIndex(AValue: Integer);
    procedure SetSecondaryShortCuts(AValue: TCustomShortCutList);
  protected
    procedure ReadState(Reader: TReader); override;
    function SecondaryShortCutsCreated: boolean;
    function CreateShortCutList: TCustomShortCutList; virtual;
    property SavedEnabledState: Boolean read FSavedEnabledState write FSavedEnabledState;
    function HandleShortCut: Boolean; virtual;

    procedure SetAutoCheck(Value: Boolean); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetChecked(Value: Boolean); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetGroupIndex(const Value: Integer); virtual;
    procedure SetHelpContext(Value: THelpContext); virtual;
    procedure SetHelpKeyword(const Value: string); virtual;
    procedure SetHelpType(Value: THelpType); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetShortCut(Value: TShortCut); virtual;
    procedure SetImageIndex(Value: TImageIndex); virtual;
    procedure SetStatusAction(const Value: TStatusAction); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure SetParentComponent(AParent: TComponent); override;
    property ActionList: TContainedActionList read FActionList write SetActionList;
    function Suspended: Boolean; override;
    property Index: Integer read GetIndex write SetIndex stored False;
    property DisableIfNoHandler: Boolean read FDisableIfNoHandler write FDisableIfNoHandler default True;
    property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default False;
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read FChecked write SetChecked default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property HelpKeyword: string read FHelpKeyword write SetHelpKeyword;
    property HelpType: THelpType read FHelpType write SetHelpType default htKeyword;
    property Hint: string read FHint write SetHint;
    property Visible: Boolean read FVisible write SetVisible default True;
    property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
    property SecondaryShortCuts: TCustomShortCutList read GetSecondaryShortCuts  write SetSecondaryShortCuts stored IsSecondaryShortCutsStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    function DoHint(var HintStr: string): Boolean; dynamic;
    property OnHint: THintEvent read FOnHint write FOnHint;
    property StatusAction: TStatusAction read FStatusAction write SetStatusAction;
  published
    property Category: string read FCategory write SetCategory;
  end;

  TContainedActionLink = class(TBasicActionLink)
  protected
    procedure DefaultIsLinked(var Result: Boolean); virtual;
    function IsCaptionLinked: Boolean; virtual;
    function IsCheckedLinked: Boolean; virtual;
    function IsEnabledLinked: Boolean; virtual;
    function IsGroupIndexLinked: Boolean; virtual;
    function IsHelpContextLinked: Boolean; virtual;
    function IsHelpLinked: Boolean; virtual;
    function IsHintLinked: Boolean; virtual;
    function IsImageIndexLinked: Boolean; virtual;
    function IsShortCutLinked: Boolean; virtual;
    function IsVisibleLinked: Boolean; virtual;
    function IsStatusActionLinked: Boolean; virtual;
    procedure SetAutoCheck(Value: Boolean); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetGroupIndex(Value: Integer); virtual;
    procedure SetHelpContext(Value: THelpContext); virtual;
    procedure SetHelpKeyword(const Value: string); virtual;
    procedure SetHelpType(Value: THelpType); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetImageIndex(Value: Integer); virtual;
    procedure SetShortCut(Value: TShortCut); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetStatusAction(const Value: TStatusAction); virtual;
  end;

  TContainedActionLinkClass = class of TContainedActionLink;
  TContainedActionClass = class of TContainedAction;

  TActionListState = (asNormal,asSuspended,asSuspendedEnabled);

  TActionListEnumerator = class
  private
    FPosition: Integer;
    FList: TContainedActionList;
  Protected  
    function GetCurrent: TContainedAction; inline;
  public
    constructor Create(AList: TContainedActionList);
    function MoveNext: Boolean; inline;
    property Current: TContainedAction read GetCurrent;
  end;

  TEnumActionListEvent = procedure(const Action: TContainedAction; var Done: boolean) of object;
  TEnumActionListRef = reference to procedure(const Action: TContainedAction; var Done: boolean);

  { TContainedActionList }

  TContainedActionList = class(TComponent)
  private
    FList: TFPList;
    FOnChange: TNotifyEvent;
    FOnExecute: TActionEvent;
    FOnUpdate: TActionEvent;
    FState: TActionListState;
    FOnStateChange: TNotifyEvent;
    procedure CorrectActionStates(ReEnabled: Boolean);
    function GetAction(Index: Integer): TContainedAction;
    procedure SetAction(Index: Integer; aValue: TContainedAction);
    function GetActionCount: Integer;
  protected
    Procedure SetActionIndex(Action : TContainedAction; aValue: Integer);
    procedure AddAction(const aAction: TContainedAction);
    procedure RemoveAction(const aAction: TContainedAction);
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetState(const aValue: TActionListState); virtual;
    procedure GetActionsInCategory(const ACategory: string; aList: TFPList; IncludeSubCategory: Boolean);
    function SameCategory(const Source, Dest: string;
                          const IncludeSubCategory: Boolean = True): Boolean;
    function Suspended : Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnExecute: TActionEvent read FOnExecute write FOnExecute;
    property OnUpdate: TActionEvent read FOnUpdate write FOnUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function IndexOfAction(Action : TBasicAction) : Integer;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetEnumerator: TActionListEnumerator;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function EnumByCategory(Proc: TEnumActionListEvent; const Category: string; const IncludeSubCategory: Boolean = True): boolean;
    function EnumByCategory(Proc: TEnumActionListRef; const Category: string;  const IncludeSubCategory: Boolean = True): boolean; 
    property Actions[Index: Integer]: TContainedAction read GetAction write SetAction; default;
    property ActionCount: Integer read GetActionCount;
    property State: TActionListState read FState write SetState default asNormal;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

type
  TEnumActionProcInfo = Pointer;
  TEnumActionProc = procedure(const Category: string; ActionClass: TBasicActionClass; Info: TEnumActionProcInfo) of object;

procedure RegisterActions(const CategoryName: string; const AClasses: array of TBasicActionClass; Resource: TComponentClass);
procedure UnRegisterActions(const AClasses: array of TBasicActionClass);
procedure EnumRegisteredActions(Proc: TEnumActionProc; Info: TEnumActionProcInfo; FrameworkType: string = '');
function CreateAction(AOwner: TComponent; ActionClass: TBasicActionClass; FrameworkType: string = ''): TBasicAction;

Type
  TRegisterActionsProc = procedure(const aCategoryName: string; const aClasses: array of TBasicActionClass; aResource: TComponentClass);
  TUnRegisterActionsProc = procedure(const AClasses: array of TBasicActionClass);
  TEnumRegisteredActionsProc = procedure(Proc: TEnumActionProc; aInfo: Pointer; const aFrameworkType: string);
  TCreateActionProc = function(AOwner: TComponent; aActionClass: TBasicActionClass; const aFrameworkType: string): TBasicAction;
                             
var
  vDesignAction: boolean;
  RegisterActionsProc: TRegisterActionsProc = nil;
  UnRegisterActionsProc: TUnRegisterActionsProc = Nil;
  EnumRegisteredActionsProc: TEnumRegisteredActionsProc = Nil;
  CreateActionProc: TCreateActionProc = Nil;

function RegisterShortCut(aShortCut: TShortCut; Index: integer = -1): integer;
function UnregisterShortCut(aShortCut: TShortCut): boolean;
function RegisteredShortCutCount: integer;
function RegisteredShortCut(Idx: integer): TShortCut;

implementation

Resourcestring
  SErrNoRegisterActionsProc = 'No register actions handler';
  SErrNoUnRegisterActionsProc = 'No register actions handler';
  SErrNoEnumActionsProc = 'No enumerate actions handler';
  SErrNoCreateActionsProc = 'No action creation handler';
  SErrIndexOutOfBounds = 'Index %d out of bounds [%d,%d]';

{ ---------------------------------------------------------------------
  Action registry hooks
  ---------------------------------------------------------------------}

procedure RegisterActions(const CategoryName: string; const AClasses: array of TBasicActionClass;
  Resource: TComponentClass);
begin
  if not Assigned(RegisterActionsProc) then
    raise EActionError.Create(SErrNoRegisterActionsProc);
  RegisterActionsProc(CategoryName, AClasses, Resource);
end;

procedure UnRegisterActions(const AClasses: array of TBasicActionClass);
begin
  if not Assigned(UnRegisterActionsProc) then
    raise EActionError.Create(SErrNoUnRegisterActionsProc);
  UnRegisterActionsProc(AClasses)
end;

procedure EnumRegisteredActions(Proc: TEnumActionProc; Info: TEnumActionProcInfo; FrameworkType: string = '');
begin
  if not Assigned(EnumRegisteredActionsProc) then
    raise EActionError.Create(SErrNoEnumActionsProc);
  EnumRegisteredActionsProc(Proc, Info, FrameworkType)
end;

function CreateAction(AOwner: TComponent; ActionClass: TBasicActionClass; FrameworkType: string = ''): TBasicAction;

var
  Old: boolean;
  
begin
  if not Assigned(CreateActionProc) then
    raise EActionError.Create(SErrNoCreateActionsProc);  
  Old:=vDesignAction;
  try
    vDesignAction:=True;
    Result:=CreateActionProc(AOwner,ActionClass,FrameworkType)
  finally
    vDesignAction:=old;
  end;
end;

{ ---------------------------------------------------------------------
  TCustomShortCutList 
  ---------------------------------------------------------------------}

function TCustomShortCutList.GetShortCut(Index: Integer): TShortCut;
begin
  Result:=TShortCut(PtrInt(Objects[Index]));
end;

function TCustomShortCutList.IndexOfShortCut(const ShortCut: TShortCut): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if TShortCut(PtrInt(Objects[I])) = ShortCut then
    begin
      Result := I;
      break;
    end;
end;

function TCustomShortCutList.IndexOfShortCut(const ShortCut: string): Integer;

  function Normalize(S: string): string;
  begin
    Result:=UpperCase(StringReplace(S, ' ', '', [rfReplaceAll]));
  end;
  
var
  S: string;
  I: Integer;


begin
  Result:=-1;
  if Trim(ShortCut)='' then
    exit;
  S:=Normalize(Shortcut);
  for I:=Count-1 downto 0 do
    if Normalize(Strings[I])=S then
      Exit(I);
end;


{ ---------------------------------------------------------------------
  TActionListEnumerator
  ---------------------------------------------------------------------}

constructor TActionListEnumerator.Create(AList: TContainedActionList);
begin
  inherited Create;
  FPosition:=-1;
  FList:=aList;
end;

function TActionListEnumerator.GetCurrent: TContainedAction;
begin
  Result:=FList[FPosition];
end;

function TActionListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result:=(FPosition<FList.ActionCount);
end;

{ ---------------------------------------------------------------------
  TContainedActionList
  ---------------------------------------------------------------------}

constructor TContainedActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList:=TFPList.Create;
  FState:=asNormal;
end;

destructor TContainedActionList.Destroy;
begin
  while (FList.Count>0) do
    TObject(FList[Flist.Count-1]).Free;
  FreeAndNil(FList);
  inherited;
end;

function TContainedActionList.IndexOfAction(Action: TBasicAction): Integer;
begin
  Result:=FList.IndexOf(Action);
end;

procedure TContainedActionList.SetActionIndex(Action: TContainedAction;
  aValue: Integer);

var
  aMax,Curr : Integer;

begin
  aMax:=FList.Count;
  if aValue>aMax then
    aValue:=aMax-1;
  if aValue<0 then
   aValue:=0;
  Curr:=IndexOfAction(Action);
  if Curr<>aValue then
    FList.Move(Curr,aValue);
end;

procedure TContainedActionList.AddAction(const aAction: TContainedAction);
begin
  if aAction=nil then
    Exit;
  aAction.FreeNotification(Self);
  aAction.FActionList:=Self;
  FList.Add(aAction);
end;

procedure TContainedActionList.Change;

var
  I: Integer;

begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  for I:=FList.Count-1 downto 0 do
    TContainedAction(FList[I]).Change;
end;

function TContainedActionList.SameCategory(const Source, Dest: string;
                                           const IncludeSubCategory: Boolean = True): Boolean;

var
  Len : integer;
  Dst : String;

begin
  Dst:=Dest;
  Len:=Length(Source);
  if IncludeSubCategory and (Len<Length(Dst)) and (Dst[Len+1]='.') then
    Dst:=Copy(Dest,1,Len);
  Result:=SameText(Source,Dst);
end;

function TContainedActionList.Suspended: Boolean;
begin
  Result:=State<>asNormal;
end;

procedure TContainedActionList.GetActionsInCategory(const ACategory: string; aList: TFPList; IncludeSubCategory : Boolean);

var
  A: TContainedAction;
begin
  for A in self do
    if SameCategory(aCategory,A.Category,IncludeSubCategory) then
      aList.Add(A);
end;

function TContainedActionList.EnumByCategory(Proc: TEnumActionListEvent;
                                       const Category: string;
                                       const IncludeSubCategory: Boolean = True): boolean;

var
  P : Pointer;
  A: TContainedAction absolute P;
  Tmp: TFPList;

begin
  Result:=False;
  If Not Assigned(Proc) then
    exit;
  Tmp:=TFPList.Create;
  try
    GetActionsInCategory(Category,Tmp,IncludeSubCategory);
    for P in Tmp do
      begin
      Proc(A,Result);
      if Result then
        exit;
      end;
  finally
    FreeAndNil(Tmp);
  end;
end;

function TContainedActionList.EnumByCategory(Proc: TEnumActionListRef;
                                       const Category: string;
                                       const IncludeSubCategory: Boolean = True): boolean;

var
  P : Pointer;
  A: TContainedAction absolute P;
  Tmp: TFPList;

begin
  Result:=False;
  If Not Assigned(Proc) then
    exit;
  Tmp:=TFPList.Create;
  try
    GetActionsInCategory(Category,Tmp,IncludeSubCategory);
    for P in Tmp do
      begin
      Proc(A,Result);
      if Result then
        exit;
      end;
  finally
    FreeAndNil(Tmp);
  end;
end;


function TContainedActionList.ExecuteAction(Action: TBasicAction): Boolean;

begin
  Result:=False;
  if Assigned(FOnUpdate) then FOnUpdate(Action, Result);
end;


function TContainedActionList.UpdateAction(Action: TBasicAction): Boolean;

begin
  Result:=False;
  if Assigned(FOnUpdate) then
    FOnUpdate(Action, Result);
end;


function TContainedActionList.GetAction(Index: Integer): TContainedAction;

begin
  Result:=TContainedAction(FList[Index]);
end;


procedure TContainedActionList.SetAction(Index: Integer; aValue: TContainedAction);

begin
  FList[Index]:=aValue;
end;


function TContainedActionList.GetActionCount: Integer;

begin
  Result:=FList.Count;
end;


procedure TContainedActionList.GetChildren(Proc: TGetChildProc; Root: TComponent);

var
  A: TContainedAction;

begin
  for A in Self do
    if (Root=A.Owner) then
      Proc(A);
end;


function TContainedActionList.GetEnumerator: TActionListEnumerator;

begin
  Result:=TActionListEnumerator.Create(Self);
end;


procedure TContainedActionList.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
  if (Operation<>opRemove) then
    exit;
  if (AComponent is TContainedAction) then
    RemoveAction(TContainedAction(AComponent));
end;


procedure TContainedActionList.RemoveAction(const aAction: TContainedAction);

begin
  if Not Assigned(aAction) then
    exit;
  aAction.RemoveFreeNotification(Self); // just in case
  if FList.Remove(aAction)<0 then
    exit; // not our action...
  aAction.FActionList:=nil;
end;


procedure TContainedActionList.SetChildOrder(Component: TComponent; Order: Integer);

var
  A : TContainedAction absolute Component;

begin
  if Component is TContainedAction then
    if (IndexOfAction(A)>=0) then
      SetActionIndex(A,Order);
end;


procedure TContainedActionList.CorrectActionStates(ReEnabled: Boolean);

var
  I: Integer;
  A: TContainedAction;

begin
  for I:=ActionCount-1 downto 0 do
    begin
    A:=Actions[I];
    case State of
      asNormal:
        begin
        if ReEnabled then
          A.Enabled:=A.SavedEnabledState;
        A.Update;
        end;
      asSuspendedEnabled:
        begin
        A.SavedEnabledState:=A.Enabled;
        A.Enabled:=True;
        end;
      else
        //
      end;
    end;
end;

procedure TContainedActionList.SetState(const aValue: TActionListState);

var
  Old: TActionListState;

begin
  Old:=FState;
  if Old=aValue then exit;
  FState:=aValue;
  try
    if (aValue<>asSuspended) then
      CorrectActionStates(Old=asSuspendedEnabled);
  finally
    if Assigned(FOnStateChange) then
     FOnStateChange(Self);
  end;
end;

{ ---------------------------------------------------------------------
  TContainedAction
  ---------------------------------------------------------------------}


function TContainedAction.GetIndex: Integer;

begin
  if Assigned(ActionList) then
    Result:=ActionList.IndexOfAction(Self)
  else
    Result:=-1;
end;


function TContainedAction.GetSecondaryShortCuts: TCustomShortCutList;

begin
  if Not SecondaryShortCutsCreated then
    FSecondaryShortCuts:=CreateShortCutList;
  Result:=FSecondaryShortCuts;
end;


function TContainedAction.IsSecondaryShortCutsStored: Boolean;

begin
  Result:=SecondaryShortCutsCreated and (FSecondaryShortCuts.Count>0);
end;


procedure TContainedAction.SetActionList(AValue: TContainedActionList);

begin
  if FActionList=AValue then Exit;
  if Assigned(FActionList) then
    ActionList.RemoveAction(Self);
  if Assigned(aValue) then
    aValue.AddAction(Self); // will set FActionList
end;


procedure TContainedAction.SetCategory(AValue: string);

begin
  if FCategory=AValue then Exit;
  FCategory:=AValue;
  if Assigned(ActionList) then
    ActionList.Change;
end;


procedure TContainedAction.SetIndex(AValue: Integer);

begin
  If Assigned(ActionList) then
    ActionList.SetActionIndex(Self,aValue);
end;


procedure TContainedAction.SetSecondaryShortCuts(AValue: TCustomShortCutList);

begin
  if aValue=FSecondaryShortCuts then
    exit;
  if Assigned(aValue) and (aValue.Count>0) then
    SecondaryShortCuts.Assign(aValue) // will create
  else
    FreeAndNil(FSecondaryShortCuts);
end;


procedure TContainedAction.ReadState(Reader: TReader);

begin
  inherited ReadState(Reader);
  if Reader.Parent is TContainedActionList then
    ActionList:=TContainedActionList(Reader.Parent);
end;


function TContainedAction.SecondaryShortCutsCreated: boolean;
begin
  Result:=Assigned(FSecondaryShortCuts);
end;


function TContainedAction.CreateShortCutList: TCustomShortCutList;
begin
  Result:=TCustomShortCutList.Create;
end;


procedure TContainedAction.Assign(Source: TPersistent);

var
  Src : TContainedAction absolute Source;

begin
  if Source is TContainedAction then
    begin
    AutoCheck:=Src.AutoCheck;
    Caption:=Src.Caption;
    Checked:=Src.Checked;
    Enabled:=Src.Enabled;
    GroupIndex:=Src.GroupIndex;
    HelpContext:=Src.HelpContext;
    HelpKeyword:=Src.HelpKeyword;
    HelpType:=Src.HelpType;
    Hint:=Src.Hint;
    Visible:=Src.Visible;
    ShortCut:=Src.ShortCut;
    if Src.SecondaryShortCutsCreated then
      SecondaryShortCuts.Assign(Src.SecondaryShortCuts)
    else
      FreeAndNil(FSecondaryShortCuts);
    ImageIndex:=Src.ImageIndex;
    OnHint:=Src.OnHint;
    StatusAction:=Src.StatusAction;
    Category:=Src.Category;
    end;
  inherited Assign(Source);
end;


function TContainedAction.HandleShortCut: Boolean;
begin
  Result:=Execute;
end;


procedure TContainedAction.SetAutoCheck(Value: Boolean);
var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FAutoCheck then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetAutoCheck(Value);
    end;
  FAutoCheck:=Value;
  Change;
end;


procedure TContainedAction.SetCaption(const Value: string);
var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FCaption then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetCaption(Value);
    end;
  FCaption:=Value;
  Change;
end;


procedure TContainedAction.SetName(const Value: TComponentName);

var
  DoCaption : Boolean;

begin
  // Should we change caption as well ?
  DoCaption:=(Name=Caption) and (ClientCount=0);
  inherited SetName(Value);
  // No need to set caption.
  if Not DoCaption then
    exit;
  // Don't do anything when loading
  if (csLoading in Owner.ComponentState) then
    exit;
  Caption:=Name;
end;

procedure TContainedAction.SetChecked(Value: Boolean);

var
  I: Integer;
  Obj : TObject;
  A: TContainedAction;
  L : TContainedActionLink absolute obj;

begin
  if Value=FChecked then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetChecked(Value);
    end;
  FChecked:=Value;
  // Uncheck all others in group.
  if Not (Value and (GroupIndex>0) and Assigned(ActionList)) then
    exit;
  For A in ActionList do
    begin
    if (A<>Self) and (A.GroupIndex=GroupIndex) then
      A.Checked:=False;
    end;
  Change;
end;


procedure TContainedAction.SetEnabled(Value: Boolean);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FEnabled then
    exit;
  if Assigned(ActionList) then
    case ActionList.State of
      asSuspendedEnabled:
        Value:=True;
      asSuspended:
        begin
        FEnabled:=Value;
        exit;
        end;
      else
        //
      end;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetEnabled(Value);
    end;
  FEnabled:=Value;
  Change;
end;


procedure TContainedAction.SetGroupIndex(const Value: Integer);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;
  A : TContainedAction;

begin
  if Value=FGroupIndex then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetGroupIndex(Value);
    end;
  FGroupIndex:=Value;
  // Uncheck others.
  if FChecked and (Value>0) and Assigned(ActionList) then
    For A in ActionList do
      if (A.GroupIndex=Value) then
        A.Checked:=False;
  Change;
end;


procedure TContainedAction.SetHelpContext(Value: THelpContext);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FHelpContext then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetHelpContext(Value);
    end;
  FHelpContext:=Value;
  Change;
end;


procedure TContainedAction.SetHelpKeyword(const Value: string);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FHelpKeyword then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetHelpKeyword(Value);
    end;
  FHelpKeyword:=Value;
  Change;
end;


procedure TContainedAction.SetHelpType(Value: THelpType);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FHelpType then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetHelpType(Value);
    end;
  FHelpType:=Value;
  Change;
end;


procedure TContainedAction.SetHint(const Value: string);
var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FHint then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetHint(Value);
    end;
  FHint:=Value;
  Change;
end;


procedure TContainedAction.SetVisible(Value: Boolean);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FVisible then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetVisible(Value);
    end;
  FVisible:=Value;
  Change;
end;


procedure TContainedAction.SetShortCut(Value: TShortCut);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FImageIndex then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetShortCut(Value);
    end;
  FShortCut:=Value;
  Change;
end;


procedure TContainedAction.SetImageIndex(Value: TImageIndex);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FImageIndex then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetImageIndex(Value);
    end;
  FImageIndex:=Value;
  Change;
end;


procedure TContainedAction.SetStatusAction(const Value: TStatusAction);

var
  I: Integer;
  Obj : TObject;
  L : TContainedActionLink absolute obj;

begin
  if Value=FStatusAction then
    exit;
  for I:=0 to ClientCount-1 do
    begin
    Obj:=GetClient(I);
    if Obj is TContainedActionLink then
      L.SetStatusAction(Value);
    end;
  FStatusAction:=Value;
  Change;
end;


constructor TContainedAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled:=True;
  FVisible:=True;
  FImageIndex:=-1;
end;


destructor TContainedAction.Destroy;
begin
  ActionList:=Nil; // Remove ourselves from action list
  FreeAndNil(FSecondaryShortCuts);
  inherited Destroy;
end;


function TContainedAction.GetParentComponent: TComponent;
begin
  if Assigned(ActionList) then
    Result:=ActionList
  else
    Result:=inherited GetParentComponent;
end;


function TContainedAction.HasParent: Boolean;

begin
  Result:=Assigned(ActionList);
  If not Result then
    Result:=Inherited HasParent;
end;


procedure TContainedAction.SetParentComponent(AParent: TComponent);

begin
  Inherited;
  if not (csLoading in ComponentState) and (AParent is TContainedActionList) then
    ActionList:=TContainedActionList(AParent);
end;


function TContainedAction.Suspended: Boolean;

begin
  if Assigned(ActionList) then
    Result:=ActionList.Suspended
  else
    Result:=False;
end;


function TContainedAction.DoHint(var HintStr: string): Boolean;
begin
  Result:=True;
  if Assigned(FOnHint) then
    FOnHint(HintStr,Result);
end;


{ TContainedActionLink }

procedure TContainedActionLink.DefaultIsLinked(var Result: Boolean);

begin
  Result:=Action is TContainedAction;
end;


function TContainedActionLink.IsCaptionLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsCheckedLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsEnabledLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;

function TContainedActionLink.IsGroupIndexLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsHelpContextLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsHelpLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsHintLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsImageIndexLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsShortCutLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsVisibleLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


function TContainedActionLink.IsStatusActionLinked: Boolean;

begin
  Result:=False;
  DefaultIsLinked(Result);
end;


procedure TContainedActionLink.SetAutoCheck(Value: Boolean);

begin
  if Value then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetCaption(const Value: string);

begin
  if Value<>'' then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetChecked(Value: Boolean);

begin
  if Value then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetEnabled(Value: Boolean);

begin
  if Value then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetGroupIndex(Value: Integer);

begin
  if Value<>0 then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetHelpContext(Value: THelpContext);

begin
  if Ord(Value)<>0 then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetHelpKeyword(const Value: string);

begin
  if Value<>'' then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetHelpType(Value: THelpType);

begin
  if Ord(Value)<>0 then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetHint(const Value: string);

begin
  if Value<>'' then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetImageIndex(Value: Integer);

begin
  if Value<>0 then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetShortCut(Value: TShortCut);

begin
  if Value<>0 then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetVisible(Value: Boolean);

begin
  if Value then ; // Silence compiler
  // Needs to be implemented in descendants
end;


procedure TContainedActionLink.SetStatusAction(const Value: TStatusAction);
begin
  if Ord(Value)<>0 then ; // Silence compiler
  // Needs to be implemented in descendants
end;


Type
  TShortCutList = Class(TFPList)
  private
    function GetS(I : Integer): TShortCut;
    procedure SetS(I : Integer; AValue: TShortCut);
  Public
    Property ShortCuts[I : Integer] : TShortCut Read GetS Write SetS; default;
  end;

function ShToPtr(S : TShortCut) : Pointer; inline;

begin
  Result:=Pointer(PtrInt(S));
end;


function PtrToSh(P : Pointer) : TShortCut; inline;

begin
  Result:=TShortCut(PtrUint(P) and $FFFF);
end;


var
  _ShortCuts : TShortCutList;

function RegisterShortCut(aShortCut: TShortCut; Index: integer = -1): integer;

var
  Ptr : Pointer;

begin
  Result:=-1;
  if aShortCut<=0 then
    exit;
  if not Assigned(_ShortCuts) then
    exit;
  Ptr:=ShToPtr(aShortCut);
  if _ShortCuts.IndexOf(Ptr)>=0 then
    Exit;
  if (Index<0) or (Index>=_ShortCuts.Count) then
    Result:=_ShortCuts.Add(Ptr)
  else
    begin
    _ShortCuts.Insert(Index,Ptr);
    Result:=Index;
    end;
end;


function UnregisterShortCut(aShortCut: TShortCut): boolean;

var
  Idx: integer;

begin
  Result:=False;
  if (Integer(aShortCut)<0) then
    exit;
  if Not Assigned(_ShortCuts) then
    exit;
  Idx:=_ShortCuts.IndexOf(ShToPtr(aShortCut));
  if (Idx<0) then
    exit;
  _ShortCuts.Delete(Idx);
  Result:=True;
end;


function RegisteredShortCutCount: integer;
begin
  Result:=_ShortCuts.Count;
end;


function RegisteredShortCut(Idx: integer): TShortCut;
begin
  if (Idx>=0) and (Idx<_ShortCuts.Count) then
    Result:=PtrToSh(_ShortCuts.Items[Idx])
  else
    EListError.CreateFmt(SErrIndexOutOfBounds,[Idx, 0, RegisteredShortCutCount-1]);
end;


{ TShortCutList }

function TShortCutList.GetS(I : Integer): TShortCut;
begin
  Result:=PtrToSh(Items[i]);
end;


procedure TShortCutList.SetS(I : Integer; AValue: TShortCut);
begin
  Items[i]:=ShToPtr(aValue);
end;



initialization
  _ShortCuts:=TShortCutList.Create;
  vDesignAction:=False;

finalization
  FreeAndNil(_ShortCuts);

end.

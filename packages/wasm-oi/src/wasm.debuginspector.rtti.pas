{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 2023 by the Free Pascal development team

    This file provides a class to send RTTI info to the Javascript webassembly object inspector.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.debuginspector.rtti;

{$mode objfpc}
{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Rtti, System.Types,
{$ELSE FPC_DOTTEDUNITS}
  Classes, SysUtils, TypInfo, Rtti, Types,
{$ENDIF}
  wasm.debuginspector.shared,
  wasm.debuginspector.api;

Type

  TWasmDebugInspector = Class;
  TWasmDebugInspectorClass = class of TWasmDebugInspector;
  TMemberVisibilities = set of TMemberVisibility;
  TObjectCaptionEvent = procedure (aSender : TObject; aObject : TObject; var aCaption : String) of object;
  TObjectChildrenEvent = procedure (aSender : TObject; aObject : TObject; var aChildren : TObjectDynArray; var aHandled : Boolean) of Object;
  TPropertyValueKind = (pvkOK,pvkNoValue,pvkError);

  { TWasmDebugInspector }

  TWasmDebugInspector = Class(TComponent)
  private
  Type
    { TInspectorList }
    TInspectorList = class(TFPList)
      Procedure HandleObjectPropertiesEvent(aInspectorID: Longint; aObjectID : TObjectID; aFlags : Longint; var aResult : TWasmOIResult);
      Procedure HandleObjectTreeEvent (aInspectorID : Longint; aRootObjectID : TObjectID; aFlags : Longint; var aResult : TWasmOIResult);
      function FindInspector(aID : Longint) : TWasmDebugInspector;
      Constructor create;
      Destructor destroy; override;
    end;

    class var _list : TInspectorList;
    class var _Instance : TWasmDebugInspector;
    class function GetInstance : TWasmDebugInspector; static;
    class function PropertyFlagsToVisibilities(aFlags: Longint): TMemberVisibilities;
  Private
    FInspectorID: TInspectorID;
    FLastErrorClass : String;
    FLastErrorMessage : String;
    FContext : TRttiContext;
    FOnGetObjectCaption: TObjectCaptionEvent;
    FOnGetObjectChildren: TObjectChildrenEvent;
  protected
    // Errors
    Procedure SetLastError(E : Exception); virtual;
    Procedure GetLastError(out aErrorClass,aErrorMessage: String); virtual;
    // Convert Object ID to Object instance and vice versa.
    function FindObject(aObjectID : TObjectID) : TObject; virtual;
    function GetObjectID(aObject : TObject) : TObjectID; virtual;
    // From callbacks
    function SendObjectProperties(aObjectID : TObjectID; aFlags : Longint) : TWasmOIResult;
    function SendObjectTree(aRootObjectID : TObjectID; aFlags : Longint) : TWasmOIResult;
    // Properties
    function GetObjectPropertyValue(aObject: TObject; aIdx: Integer; aProp: TRttiProperty; Out aValue: RawByteString; Out lValueObjectID : TObjectID) : TPropertyValueKind; virtual;
    function SendObjectProperty(aObject: TObject; aIdx: Integer; aProp: TRttiProperty): Boolean; virtual;
    // Object Tree
    function GetObjectChildren(aObject: TObject): TObjectDynArray virtual;
    function GetObjectCaption(aObject: TObject): RawByteString; virtual;
    function DoSendObjectTree(aParent: TObject; aObject: TObject): Boolean; virtual;
  Public
    class var _InstanceClass : TWasmDebugInspectorClass;
    class property Instance : TWasmDebugInspector Read GetInstance;
    class destructor done;
    class constructor init;
  Public
    constructor Create(aOwner: TComponent); override;
    destructor destroy; override;
    function ClearObjectTree: Boolean;
    function ClearObjectInspector: Boolean;
    function SendObjectProperties(aObject: TObject; aVisibilities: TMemberVisibilities): Boolean; virtual;
    function SendObjectTree(aObject: TObject; const aCaption : string): Boolean; virtual;
    function SendObjectTree(aObject: TObject): Boolean; virtual;
    class function VisibilitiesToString(aVisibilities: TMemberVisibilities): string;
    property OnGetObjectCaption : TObjectCaptionEvent Read FOnGetObjectCaption Write FOnGetObjectCaption;
    Property InspectorID : TInspectorID Read FInspectorID;
    Property OnGetObjectChildren : TObjectChildrenEvent Read FOnGetObjectChildren Write FOnGetObjectChildren;
  end;

Function WasmDebugInspector : TWasmDebugInspector;

implementation

Function WasmDebugInspector : TWasmDebugInspector;

begin
  Result:=TWasmDebugInspector.Instance;
end;

{ TWasmDebugInspector }

class function TWasmDebugInspector.GetInstance: TWasmDebugInspector;
var
  C : TWasmDebugInspectorClass;
begin
  if _Instance=Nil then
    begin
    C:=_InstanceClass;
    if C=Nil then
      C:=TWasmDebugInspector;
    _Instance:=C.Create(Nil);
    end;
  Result:=_Instance
end;

procedure TWasmDebugInspector.SetLastError(E: Exception);
begin
  if E=Nil then
    begin
    FLastErrorClass:='';
    FLastErrorMessage:='';
    end
  else
    begin
    FLastErrorClass:=E.ClassName;
    FLastErrorMessage:=E.Message;
    end;
end;

procedure TWasmDebugInspector.GetLastError(out aErrorClass, aErrorMessage: String);
begin
  aErrorClass:=FLastErrorClass;
  aErrorMessage:=FLastErrorMessage;
end;

class function TWasmDebugInspector.PropertyFlagsToVisibilities(aFlags: Longint): TMemberVisibilities;

var
  lFlags : TMemberVisibilities;

begin
  lFLags:=[];
  if (aFlags and WASM_SENDPROPERTYFLAG_PRIVATE) <> 0 then
    include(lFlags,mvPrivate);
  if (aFlags and WASM_SENDPROPERTYFLAG_PROTECTED) <> 0 then
    include(lFlags,mvProtected);
  if (aFlags and WASM_SENDPROPERTYFLAG_PUBLIC) <> 0 then
    include(lFlags,mvPublic);
  if (aFlags and WASM_SENDPROPERTYFLAG_PUBLISHED) <> 0 then
    include(lFlags,mvPublished);
  Result:=lFlags;
end;

function TWasmDebugInspector.SendObjectProperties(aObjectID: TObjectID; aFlags: Longint): TWasmOIResult;

var
  Obj : TObject;
  Vis : TMemberVisibilities;

begin
  Vis:=PropertyFlagsToVisibilities(aFlags);
  Obj:=FindObject(aObjectID);
  if Obj=Nil then
    Result:=WASMOI_INVALIDOBJECT
  else
    begin
    SendObjectProperties(Obj,Vis);
    Result:=WASMOI_SUCCESS;
    end;
end;

function TWasmDebugInspector.SendObjectTree(aRootObjectID: TObjectID; aFlags: Longint): TWasmOIResult;

var
  Obj : TObject;

begin
  Obj:=FindObject(aRootObjectID);
  if Obj=Nil then
    Result:=WASMOI_INVALIDOBJECT
  else
    begin
    SendObjectTree(Obj);
    Result:=WASMOI_SUCCESS;
    end;
end;

function TWasmDebugInspector.FindObject(aObjectID: TObjectID): TObject;
begin
  if aObjectID=0 then
    Result:=Nil
  else
    Result:=TObject(PtrInt(aObjectID));
end;

class destructor TWasmDebugInspector.done;
begin
  FreeAndNil(_instance);
  FreeAndNil(_list);
end;

class constructor TWasmDebugInspector.init;
begin
  _List:=TInspectorList.Create;
  OnGetObjectProperties:=@_List.HandleObjectPropertiesEvent;
  OnGetObjectTree:=@_List.HandleObjectTreeEvent;
end;

constructor TWasmDebugInspector.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  // order is uncertain, so check
  if not __wasm_oi_allocate(@FInspectorID)=WASMOI_SUCCESS then
    begin
    FInspectorID:=0;
    __wasm_oi_log(wolError,'Failed to allocate inspector, using default');
    end;
  if Assigned(_List) then
    _List.Add(Self);
  FContext:=TRttiContext.Create;
end;

destructor TWasmDebugInspector.destroy;
begin
  if not __wasm_oi_deallocate(FInspectorID)=WASMOI_SUCCESS then
    begin
    FInspectorID:=0;
    __wasm_oi_log(wolError,'Failed to deallocate inspector, ignoring');
    end;
  // order is uncertain, so check
  if Assigned(_List) then
    _List.Remove(Self);
  FContext.Free;
  inherited destroy;
end;

function TWasmDebugInspector.GetObjectID(aObject: TObject): TObjectID;

begin
  Result:=TObjectID(aObject);
end;

function TWasmDebugInspector.GetObjectPropertyValue(aObject: TObject; aIdx: Integer; aProp: TRttiProperty; out
  aValue: RawByteString; out lValueObjectID: TObjectID): TPropertyValueKind;

const
  AllowedTypes = [tkInteger,tkChar,tkEnumeration,tkFloat,tkSet, tkSString, tkLString,
                  tkAString,tkWString, tkVariant, tkClass, tkWChar, tkBool, tkInt64,
                  tkQWord, tkUString, tkUChar];
var
  V : TValue;
  S : String;

begin
  try
    if not (aProp.PropertyType.TypeKind in AllowedTypes) then
      begin
      Result:=pvkNoValue;
      aValue:='<unable to display>';
      end
    else
      begin
      Result:=pvkOK;
      V:=aProp.GetValue(aObject);
      S:=V.ToString;
      {$IF SIZEOF(CHAR)=2)}
      aValue:=UTF8Encode(S);
      {$ELSE}
      aValue:=S;
      {$ENDIF}
      if aProp.PropertyType.TypeKind=tkClass then
        lValueObjectID:=GetObjectID(V.AsObject);
      end;
  except
    on E : Exception do
      begin
      aValue:=Format('<Error %s getting property: %s>',[E.ClassName,E.Message]);
      Result:=pvkError;
      end;
  end;
end;

function TWasmDebugInspector.SendObjectProperty(aObject : TObject; aIdx : Integer; aProp : TRttiProperty) : Boolean;

var
  lData : TPropertyData;
  lName : RawByteString;
  lValue : RawByteString;
  Res : TWasmOIResult;
  lFlags : Longint;
  lValueObjectID : TObjectID;

begin
  __wasm_oi_log(wolTrace,'--> TWasmDebugInspector.SendObjectProperty(%s,%d,"%s")',[aObject.ToString,aIdx,aProp.Name]);
  lData:=Default(TPropertyData);
  lData[WASM_PROPERTY_OBJECT_ID]:=GetObjectID(aObject);
  lData[WASM_PROPERTY_IDX]:=aIdx;
  lData[WASM_PROPERTY_VISIBILITY]:=Ord(aProp.Visibility);
  lData[WASM_PROPERTY_KIND]:=Ord(aProp.PropertyType.TypeKind);
{$IF SIZEOF(CHAR)=2)}
  lName:=UTF8Encode(aProp.Name);
{$ELSE}
  lName:=aProp.Name;
{$ENDIF}
  lData[WASM_PROPERTY_NAME]:=Longint(Pointer(lName));
  lData[WASM_PROPERTY_NAME_LEN]:=Length(lName);
  lValueObjectID:=0;
  case GetObjectPropertyValue(aObject,aIdx,aProp,lValue,lValueObjectID) of
    pvkError : lFlags:=lFlags or WASM_PROPERTYFLAGS_NOVALUE or WASM_PROPERTYFLAGS_ERROR;
    pvkNoValue: lFlags:=lFlags or WASM_PROPERTYFLAGS_NOVALUE or WASM_PROPERTYFLAGS_NOVALUE;
  else
    lFlags:=0;
  end;
  lData[WASM_PROPERTY_VALUE]:=Longint(Pointer(lValue));
  lData[WASM_PROPERTY_VALUE_LEN]:=Length(lValue);
  lData[WASM_PROPERTY_FLAGS]:=lFlags;
  lData[WASM_PROPERTY_PROPERTYOBJECTID]:=lValueObjectID;
  Res:=__wasm_oi_inspector_add_property(FInspectorID,@lData);
  Result:=Res=WASMOI_SUCCESS;
  if not Result then
    __wasm_oi_log(wolError,'Failed to send object %d (%s) property %s: %d',[lData[WASM_PROPERTY_OBJECT_ID],aObject.ToString,aProp.Name,Res]);
  __wasm_oi_log(wolTrace,'<-- TWasmDebugInspector.SendObjectProperty');
end;

class function TWasmDebugInspector.VisibilitiesToString(aVisibilities : TMemberVisibilities) : string;

const
  VisNames : Array[TMemberVisibility] of string = ('Private','Protected', 'Public', 'Published');

var
  Vis : TMemberVisibility;

begin
  Result:='';
  For Vis in TMemberVisibility do
    if Vis in AVisibilities then
      begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+VisNames[Vis];
      end;
  Result:='['+Result+']';
end;

function TWasmDebugInspector.SendObjectProperties(aObject: TObject; aVisibilities : TMemberVisibilities) : Boolean;

var
  lPropArray : TRttiPropertyArray;
  lProp : TRttiProperty;
  Info : TRttiType;
  Idx : Integer;
  S,Vis : String;
  ObjCaption : RawByteString;
  L : TStringList;

begin
  Result:=ClearObjectInspector;
  if not Result then exit;
  Vis:=VisibilitiesToString(aVisibilities);
  ObjCaption:=aObject.ToString;
  S:=Format('TWasmDebugInspector.SendObjectProperties(%s,%s)',[ObjCaption,Vis]);
  __wasm_oi_log(wolTrace,'--> '+S);
  if __wasm_oi_inspector_set_caption(FInspectorID,PByte(ObjCaption),Length(ObjCaption))<>WASMOI_SUCCESS then
    __wasm_oi_log(wolError,'Failed to set object inspector caption');
  Info:=FContext.GetType(AObject.ClassType);
  lPropArray:=Info.GetProperties;
  Idx:=0;
  __wasm_oi_log(wolDebug,'    '+S+Format(': %d properties',[Length(lPropArray)]));
  L:=TStringList.Create;
  try
    For lProp in lPropArray do
      L.AddObject(lProp.Name,lProp);
    L.Sort;
    for Idx:=0 to L.Count-1 do
      begin
      lProp:=TRttiProperty(L.Objects[Idx]);
      if (lProp.Visibility in aVisibilities) then
        if not SendObjectProperty(aObject,Idx,lProp) then
          Result:=False;
       end;
  finally
    L.Free;
  end;
  __wasm_oi_log(wolTrace,'<-- '+S);
end;

function TWasmDebugInspector.SendObjectTree(aObject: TObject; const aCaption: string): Boolean;

var
  lCaption : RawByteString;

begin
  lCaption:=UTF8Encode(aCaption);
  if __wasm_oi_tree_set_caption(FInspectorID,PByte(lCaption),Length(lCaption))<>WASMOI_SUCCESS then
    __wasm_oi_log(wolError,'Failed to set object inspector caption');
  SendObjectTree(aObject);
end;

function TWasmDebugInspector.ClearObjectTree: Boolean;

var
  Res : TWasmOIResult;

begin
  Res:=__wasm_oi_tree_clear(FInspectorID);
  Result:=Res=WASMOI_SUCCESS;
  if not Result then
    __wasm_oi_log(wolError,'Failed to clear object tree %d: %d',[FInspectorID, Res]);
end;

function TWasmDebugInspector.ClearObjectInspector: Boolean;
var
  Res : TWasmOIResult;

begin
  Res:=__wasm_oi_inspector_clear(FInspectorID);
  Result:=Res=WASMOI_SUCCESS;
  if not Result then
    __wasm_oi_log(wolError,'Failed to clear object inspector %d: %d',[FInspectorID, Res]);
end;

function TWasmDebugInspector.GetObjectCaption(aObject: TObject): RawByteString;

var
  lCaption : String;

begin
  if Assigned(FOnGetObjectCaption) then
    FOnGetObjectCaption(Self,aObject,lCaption)
  else
    lCaption:=aObject.ToString;
{$IF SIZEOF(CHAR)=2}
  Result:=UTF8Encode(lCaption);
{$ELSE}
  Result:=lCaption;
{$ENDIF}
end;

function TWasmDebugInspector.GetObjectChildren(aObject: TObject): TObjectDynArray;

var
  I : Integer;
  lComponent : TComponent absolute aObject;
  lCollection : TCollection absolute aObject;
  Handled : Boolean;

begin
  Result:=Nil;
  Handled:=False;
  if Assigned(FOnGetObjectChildren) then
    FOnGetObjectChildren(Self,aObject,Result,Handled);
  if not Handled then
    if aObject is TComponent then
      begin
      SetLength(Result,lComponent.ComponentCount);
      For I:=0 to lComponent.ComponentCount-1 do
        Result[I]:=lComponent.Components[I];
      end
    else if aObject is TCollection then
      begin
      SetLength(Result,lCollection.Count);
      For I:=0 to lCollection.Count-1 do
        Result[I]:=lCollection.Items[I];
      end;
end;

function TWasmDebugInspector.DoSendObjectTree(aParent : TObject; aObject: TObject): Boolean;

var
  Arr : TObjectDynArray;
  ObjectData : TObjectData;
  lCaption,
  lClassName : RawByteString;
  aChild : TObject;
  Res: TWasmOIResult;

begin
  lClassName:=aObject.ClassName;
  lCaption:=GetObjectCaption(aObject);
  ObjectData[WASM_OBJECT_PARENTID]:=GetObjectID(aParent);
  ObjectData[WASM_OBJECT_ID]:=GetObjectID(aObject);
  ObjectData[WASM_OBJECT_FLAGS]:=0;
  ObjectData[WASM_OBJECT_CLASSNAME]:=Longint(Pointer(lClassName));
  ObjectData[WASM_OBJECT_CLASSNAME_LEN]:=Length(lClassName);
  ObjectData[WASM_OBJECT_CAPTION]:=Longint(Pointer(lCaption));
  ObjectData[WASM_OBJECT_CAPTION_LEN]:=Length(lCaption);
  Res:=__wasm_oi_tree_add_object(FInspectorID,@ObjectData);
  Result:=Res=WASMOI_SUCCESS;
  if Not Result then
  else
    begin
    Arr:=GetObjectChildren(aObject);
    For aChild in Arr do
      Result:=DoSendObjectTree(aObject,aChild) and Result;
    end;
end;

function TWasmDebugInspector.SendObjectTree(aObject: TObject): Boolean;
begin
  Result:=ClearObjectTree;
  if Result then
    Result:=DoSendObjectTree(Nil,aObject);
end;

{ TWasmDebugInspector.TInspectorList }

procedure TWasmDebugInspector.TInspectorList.HandleObjectPropertiesEvent(aInspectorID: Longint; aObjectID: TObjectID;
  aFlags: Longint; var aResult: TWasmOIResult);

var
  Insp : TWasmDebugInspector;

begin
  Insp:=FindInspector(aInspectorID);
  if not assigned(Insp) then
    aResult:=WASMOI_NO_INSPECTOR
  else
    try
      aResult:=Insp.SendObjectProperties(aObjectID,aFlags)
    except
      On E : Exception do
        begin
        Insp.SetLastError(E);
        __wasm_oi_log(wolError,'Exception %s while sending properties: %s',[E.ClassName,E.Message]);
        aResult:=WASMOI_EXCEPTION;
        end;
    end;
end;

procedure TWasmDebugInspector.TInspectorList.HandleObjectTreeEvent(aInspectorID: Longint; aRootObjectID: TObjectID;
  aFlags: Longint; var aResult: TWasmOIResult);
var
  Insp : TWasmDebugInspector;

begin
  Insp:=FindInspector(aInspectorID);
  if not assigned(Insp) then
    aResult:=WASMOI_NO_INSPECTOR
  else
    try
      aResult:=Insp.SendObjectTree(aRootObjectID,aFlags);
    except
      On E : Exception do
        begin
        Insp.SetLastError(E);
        __wasm_oi_log(wolError,'Exception %s while sending properties: %s',[E.ClassName,E.Message]);
        aResult:=WASMOI_EXCEPTION;
        end;
    end;
end;

function TWasmDebugInspector.TInspectorList.FindInspector(aID: Longint): TWasmDebugInspector;

var
  I: Integer;

begin
  I:=Count-1;
  While (I>=0) and (TWasmDebugInspector(Items[i]).InspectorID<>aID) do
   Dec(I);
  if I=-1 then
    begin
    __wasm_oi_log(wolError,'Could not find object inspector ID %d',[aID]);
    Result:=Nil;
    end
  else
    begin
    Result:=TWasmDebugInspector(Items[I]);
    __wasm_oi_log(wolDebug,'found object inspector ID %d at pos %d (%b)',[aID,I,Assigned(Result)]);
    end;
end;

constructor TWasmDebugInspector.TInspectorList.create;
begin
  Inherited;
  OnGetObjectProperties:=@HandleObjectPropertiesEvent;
  OnGetObjectTree:=@HandleObjectTreeEvent;
end;

destructor TWasmDebugInspector.TInspectorList.destroy;
begin
  OnGetObjectProperties:=Nil;
  OnGetObjectTree:=Nil;
  inherited destroy;
end;


end.


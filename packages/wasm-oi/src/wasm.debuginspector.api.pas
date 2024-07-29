{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 2024 by the Free Pascal development team

    This file provides the import statements of 
    the Javascript webassembly object inspector API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.debuginspector.api;

{$mode objfpc}
{$h+}

interface

uses wasm.debuginspector.shared;

Type
  TWasmOILogLevel = (wolTrace, wolDebug, wolInfo, wolWarning, wolError, wolCritical);
  TWasmOILogLevels = set of TWasmOILogLevel;

  TGetObjectPropertiesEvent = Procedure(aInspectorID: Longint; aObjectID : TObjectID; aFlags : Longint; var aResult : TWasmOIResult) of object;
  TGetObjectTreeEvent = Procedure(aInspectorID : Longint; aRootObjectID : TObjectID; aFlags : Longint; var aResult : TWasmOIResult) of object;
  TWasmOILogHook = procedure (Level : TWasmOILogLevel; const Msg : string) of object;

function wasm_oi_get_object_properties(aInspectorID : Longint; aObjectID : TObjectID; aFlags : Longint) : TWasmOIResult;
function wasm_oi_get_object_tree(aInspectorID : Longint; aRootObjectID : TObjectID; aFlags : Longint) : TWasmOIResult;

var
  OnGetObjectProperties : TGetObjectPropertiesEvent;
  OnGetObjectTree : TGetObjectTreeEvent;
  OnWasmOILog : TWasmOILogHook;

function __wasm_oi_allocate(aInspectorID: PInspectorID) : TWasmOIResult external InspectorModuleName name call_allocate;
function __wasm_oi_deallocate(aInspectorID: TInspectorID) : TWasmOIResult external InspectorModuleName name call_deallocate;
function __wasm_oi_tree_clear(aInspectorID: TInspectorID) : TWasmOIResult external InspectorModuleName name call_tree_clear;
function __wasm_oi_tree_add_object(aInspectorID: TInspectorID; ObjectData : PObjectData) : TWasmOIResult external InspectorModuleName name call_tree_add_object;
function __wasm_oi_tree_set_caption(aInspectorID: TInspectorID; aCaption: PByte; aCaptionLen : Longint) : TWasmOIResult external InspectorModuleName name call_tree_set_caption;
function __wasm_oi_inspector_clear(aInspectorID: TInspectorID) : TWasmOIResult external InspectorModuleName name call_inspector_clear;
function __wasm_oi_inspector_add_property(aInspectorID: TInspectorID; PropertyData: PPropertyData) : TWasmOIResult external InspectorModuleName name call_inspector_add_property;
function __wasm_oi_inspector_set_caption(aInspectorID: TInspectorID; aCaption: PByte; aCaptionLen : Longint) : TWasmOIResult external InspectorModuleName name call_inspector_set_caption;

procedure __wasm_oi_log(aLevel : TWasmOILogLevel; Const Msg : string); overload;
procedure __wasm_oi_log(aLevel : TWasmOILogLevel; Const Fmt : string; const args : Array of const); overload;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils;
{$ELSE}
uses SysUtils;
{$ENDIF}

procedure __wasm_oi_log(aLevel : TWasmOILogLevel; Const Msg : string);

begin
  if Assigned(OnWasmOILog) then
    OnWasmOILog(aLevel,Msg);
end;

procedure __wasm_oi_log(aLevel : TWasmOILogLevel; Const Fmt : string; const args : Array of const);

begin
  if Assigned(OnWasmOILog) then
    OnWasmOILog(aLevel,SafeFormat(Fmt,Args));
end;

function wasm_oi_get_object_tree(aInspectorID : Longint; aRootObjectID : TObjectID; aFlags : Longint) : TWasmOIResult;

begin
  Result:=WASMOI_NOT_IMPLEMENTED;
  if Assigned(OnGetObjectTree) then
    OnGetObjectTree(aInspectorID, aRootObjectID,aFlags,Result);
end;

function wasm_oi_get_object_properties(aInspectorID : Longint; aObjectID : TObjectID; aFlags : Longint) : TWasmOIResult;

begin
  Result:=WASMOI_NOT_IMPLEMENTED;
  if Assigned(OnGetObjectProperties) then
    OnGetObjectProperties(aInspectorID, aObjectID,aFlags,Result);
end;

exports wasm_oi_get_object_properties;

end.


{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 2023 by the Free Pascal development team

    This file provides constants and base types for the Javascript webassembly object inspector.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.debuginspector.shared;

{$mode objfpc}

interface

{$IFDEF PAS2JS}
uses rtti;
{$ENDIF}

Const
  // API return values
  WASMOI_SUCCESS         = 0;
  WASMOI_NOT_IMPLEMENTED = -1;
  WASMOI_NO_INSPECTOR    = -2;
  WASMOI_EXCEPTION       = -3;
  WASMOI_INVALIDOBJECT   = -4;

  // Property data array
  WASM_PROPERTY_OBJECT_ID  = 0;
  WASM_PROPERTY_IDX        = 1;
  WASM_PROPERTY_KIND       = 2;
  WASM_PROPERTY_VISIBILITY = 3;
  WASM_PROPERTY_NAME       = 4;
  WASM_PROPERTY_NAME_LEN   = 5;
  WASM_PROPERTY_VALUE      = 6;
  WASM_PROPERTY_VALUE_LEN  = 7;
  WASM_PROPERTY_FLAGS      = 8;
  WASM_PROPERTY_PROPERTYOBJECTID = 9;

  WASM_PROPERTYDATA_MAXLEN = WASM_PROPERTY_PROPERTYOBJECTID;

  // Property Flags
  WASM_PROPERTYFLAGS_NOVALUE = 1;       // Value cannot be displayed
  WASM_PROPERTYFLAGS_ERROR   = 1 shl 1; // Error while calculating value


  // Send Property Flags
  WASM_SENDPROPERTYFLAG_PRIVATE    = 1;
  WASM_SENDPROPERTYFLAG_PROTECTED  = 1 shl 1;
  WASM_SENDPROPERTYFLAG_PUBLIC     = 1 shl 2;
  WASM_SENDPROPERTYFLAG_PUBLISHED  = 1 shl 3;
  WASM_SENDPROPERTYFLAG_ALLVISIBILITIES = WASM_SENDPROPERTYFLAG_PRIVATE
                                          or WASM_SENDPROPERTYFLAG_PROTECTED
                                          or WASM_SENDPROPERTYFLAG_PUBLIC
                                          or WASM_SENDPROPERTYFLAG_PUBLISHED;
  WASM_SENDPROPERTYFLAG_NOCAPTION = 1 shl 4;

  // Object data array
  WASM_OBJECT_PARENTID      = 0;
  WASM_OBJECT_ID            = 1;
  WASM_OBJECT_FLAGS         = 2;
  WASM_OBJECT_CLASSNAME     = 3;
  WASM_OBJECT_CLASSNAME_LEN = 4;
  WASM_OBJECT_CAPTION       = 5;
  WASM_OBJECT_CAPTION_LEN   = 6;

  WASM_OBJECTDATA_MAXLEN = WASM_OBJECT_CAPTION_LEN;

//  aParentID, aObjectID: TObjectID; aFlags : Longint; aCaption: TWasmPointer; aCaptionLen : Longint

type
  TWasmOIResult = longint;
  TInspectorID = longint;
  TObjectID = longint;

  TPropertyData = Array[0..WASM_PROPERTYDATA_MAXLEN] of longint;
  TObjectData = Array[0..WASM_OBJECTDATA_MAXLEN] of longint;

  {$IFNDEF PAS2JS}
  TWasmPointer = Pointer;
  PPropertyData = ^TPropertyData;
  PObjectData = ^TObjectData;
  PInspectorID = ^TInspectorID;
  {$ELSE PAS2JS}
  TWasmPointer = longint;
  PPropertyData = TWasmPointer;
  PObjectData = TWasmPointer;
  PInspectorID = TWasmPointer;
  {$ENDIF PAS2JS}

Const
  InspectorModuleName = 'wasm_oi';

  call_allocate = 'allocate';
  call_deallocate = 'deallocate';
  call_tree_clear = 'tree_clear';
  call_tree_set_caption = 'tree_set_caption';
  call_tree_add_object = 'tree_add_object';
  call_inspector_clear = 'inspector_clear';
  call_inspector_add_property = 'inspector_add_property';
  call_inspector_set_caption = 'inpector_set_caption';

Type
  // TTypeKind is different in Delphi/FPC and in Pas2JS
  TNativeTypeKind = (ntkUnknown,ntkInteger,ntkChar,ntkEnumeration,ntkFloat,
            ntkSet,ntkMethod,ntkSString,ntkLString,ntkAString,
            ntkWString,ntkVariant,ntkArray,ntkRecord,ntkInterface,
            ntkClass,ntkObject,ntkWChar,ntkBool,ntkInt64,ntkQWord,
            ntkDynArray,ntkInterfaceRaw,ntkProcVar,ntkUString,ntkUChar,
            ntkHelper,ntkFile,ntkClassRef,ntkPointer);



function GetPlatformTypeKind(aKind : TNativeTypeKind) : TTypeKind;

implementation

{$IFDEF PAS2JS}
function GetPlatformTypeKind(aKind : TNativeTypeKind) : TTypeKind;

begin
  case aKind of
    ntkUnknown : Result:=tkUnknown;  // 0
    ntkInt64,
    ntkQWord,
    ntkInteger : Result:=tkInteger;   // 1
    ntkUChar,
    ntkWChar,
    ntkChar : Result:=tkChar;         // 2 in Delphi/FPC tkWChar; tkUChar
    ntkSString,
    ntkAString,
    ntkWString,
    ntkUString: Result:=tkString;      // 3 in Delphi/FPC tkSString; tkWString or tkUString
    ntkEnumeration : Result:=tkEnumeration; // 4
    ntkSet : Result:=tkSet;            // 5
    ntkFloat : Result:=tkDouble;   // 6
    ntkBool : Result:=tkBool;     // 7
    ntkProcVar : Result:=tkProcVar;  // 8  function or procedure
    ntkMethod : Result:=tkMethod;   // 9  proc var of object
    ntkArray : Result:=tkArray;    // 10 static array
    ntkDynArray : Result:=tkDynArray; // 11
    ntkRecord : Result:=tkRecord;   // 12
    ntkClass : Result:=tkClass;    // 13
    ntkClassRef : Result:=tkClassRef; // 14
    ntkPointer : Result:=tkPointer;  // 15
    ntkVariant : Result:=tkJSValue;  // 16
    ntkInterface : Result:=tkInterface; // 18
  else
    Result:=tkUnknown;
  end;
end;
{$ELSE}
function GetPlatformTypeKind(aKind : TNativeTypeKind) : TTypeKind;
begin
  Result:=TTypeKind(aKind);
end;

{$ENDIF}

end.


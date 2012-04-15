{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004 by Dean Zobec

    Port to Free Pascal of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testutils;

interface

uses
  Classes, SysUtils;

type
  {$M+}
  TNoRefCountObject = class(TObject, IInterface)
  protected
    { IInterface }
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;
  {$M-}

procedure FreeObjects(List: TFPList);
procedure GetMethodList( AObject: TObject; AList: TStrings ); overload;
procedure GetMethodList( AClass: TClass; AList: TStrings ); overload;

implementation

function TNoRefCountObject.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then Result := 0
    else Result := HRESULT($80004002);
end;

function TNoRefCountObject._AddRef: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TNoRefCountObject._Release: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

// been to the dentist and suffered a lot
// Hack Alert! see objpas.inc
//  Get a list of published methods for a given class or object
procedure GetMethodList( AObject: TObject; AList: TStrings );
begin
  GetMethodList( AObject.ClassType, AList );
end;

procedure GetMethodList(AClass: TClass; AList: TStrings);
type
  PMethodNameRec = ^TMethodNameRec;
  TMethodNameRec = packed record
    name : pshortstring;
    addr : pointer;
  end;

  TMethodNameTable = packed record
    count : dword;
    entries : packed array[0..0] of TMethodNameRec;
  end;

  pMethodNameTable =  ^TMethodNameTable;

var
  methodTable : pMethodNameTable;
  i : dword;
  vmt: TClass;
  idx: integer;
  pmr: PMethodNameRec;
begin
  AList.Clear;
  vmt := aClass;
  while assigned(vmt) do
  begin
    methodTable := pMethodNameTable((Pointer(vmt) + vmtMethodTable)^);
    if assigned(MethodTable) then
    begin
      pmr := @methodTable^.entries[0];
      for i := 0 to MethodTable^.count - 1 do
      begin
        idx := aList.IndexOf(pmr^.name^);
        if (idx <> - 1) then
        //found overridden method so delete it
          aList.Delete(idx);
        aList.AddObject(pmr^.name^, TObject(pmr^.addr));
        Inc(pmr);
      end;
    end;
    vmt := pClass(pointer(vmt) + vmtParent)^;
  end;
end;

procedure FreeObjects(List: TFPList);
var
  i: integer;
begin
  for i:= 0 to List.Count - 1 do
    TObject(List.Items[i]).Free;
end;

end.

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
{$IFNDEF FPC_DOTTEDUNITS}
unit testutils;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

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
  TMethodNameRec =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif}
  record
    name : pshortstring;
    addr : codepointer;
  end;

  TMethodNameTable =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif}
  record
    count : dword;
    entries : packed array[0..0] of TMethodNameRec;
  end;

  pMethodNameTable =  ^TMethodNameTable;

var
  methodTable : pMethodNameTable;
  i : integer;
  vmt: PVmt;
  idx: integer;
  pmr: PMethodNameRec;
  lName : shortstring;
begin
  AList.Clear;
  vmt := PVmt(aClass);
  while assigned(vmt) do
  begin
    methodTable := pMethodNameTable(vmt^.vMethodTable);
    if assigned(MethodTable) then
    begin
      pmr := @methodTable^.entries[0];
      for i := 0 to MethodTable^.count - 1 do
      begin
        lName:=pmr^.name^;
        idx := aList.IndexOf(lName);
        if (idx <> - 1) then
        //found overridden method so delete it
          aList.Delete(idx);
        aList.AddObject(lName, TObject(pmr^.addr));
        Inc(pmr);
      end;
    end;
    vmt := vmt^.vParent;
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

{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Template support routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPTemplt;

interface

uses FPViews;

procedure InitTemplates;
function  GetTemplateCount: integer;
function  GetTemplateName(Index: integer): string;
function  StartTemplate(Index: integer; Editor: PSourceEditor): boolean;
procedure DoneTemplates;

implementation

uses
  Dos,Objects,
{$ifdef EDITORS}
  Editors,
{$else}
  WEditor,
{$endif}
  FPUtils;

type
    PTemplate = ^TTemplate;
    TTemplate = record
      Name : PString;
      Path : PString;
    end;

    PTemplateCollection = ^TTemplateCollection;
    TTemplateCollection = object(TSortedCollection)
      function  At(Index: Integer): PTemplate;
      procedure FreeItem(Item: Pointer); virtual;
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
    end;

const Templates : PTemplateCollection = nil;

function NewTemplate(Name, Path: string): PTemplate;
var P: PTemplate;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  P^.Name:=NewStr(Name); P^.Path:=NewStr(Path);
  NewTemplate:=P;
end;

procedure DisposeTemplate(P: PTemplate);
begin
  if P<>nil then
  begin
    if P^.Name<>nil then DisposeStr(P^.Name);
    if P^.Path<>nil then DisposeStr(P^.Path);
    Dispose(P);
  end;
end;

function TTemplateCollection.At(Index: Integer): PTemplate;
begin
  At:=inherited At(Index);
end;

procedure TTemplateCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeTemplate(Item);
end;

function TTemplateCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var R: Sw_integer;
    K1: PTemplate absolute Key1;
    K2: PTemplate absolute Key2;
begin
  if K1^.Name^<K2^.Name^ then R:=-1 else
  if K1^.Name^>K2^.Name^ then R:= 1 else
  R:=0;
  Compare:=R;
end;

procedure InitTemplates;
procedure ScanDir(Dir: PathStr);
var SR: SearchRec;
    S: string;
begin
  if copy(Dir,length(Dir),1)<>DirSep then Dir:=Dir+DirSep;
  FindFirst(Dir+'*.pt',AnyFile,SR);
  while (DosError=0) do
  begin
    S:=NameOf(SR.Name);
    S:=LowerCaseStr(S);
    S[1]:=Upcase(S[1]);
    Templates^.Insert(NewTemplate(S,FExpand(Dir+SR.Name)));
    FindNext(SR);
  end;
{$ifdef FPC}
  FindClose(SR);
{$endif def FPC}
end;
begin
  New(Templates, Init(10,10));
  ScanDir('.');
  ScanDir(DirOf(ParamStr(0)));
end;

function GetTemplateCount: integer;
var Count: integer;
begin
  if Templates=nil then Count:=0 else Count:=Templates^.Count;
  GetTemplateCount:=Count;
end;

function GetTemplateName(Index: integer): string;
begin
  GetTemplateName:=Templates^.At(Index)^.Name^;
end;

function StartTemplate(Index: integer; Editor: PSourceEditor): boolean;
var
    T: PTemplate;
    OK: boolean;
    E: PFileEditor;
    R: TRect;
begin
  T:=Templates^.At(Index);
  R.Assign(0,0,0,0);
  New(E, Init(R,nil,nil,nil,T^.Path^));
  OK:=E<>nil;
  if OK then OK:=E^.LoadFile;
  if OK then
    begin
      E^.SelectAll(true);
      Editor^.InsertFrom(E);
      Editor^.SetCurPtr(0,0);
      Editor^.SelectAll(false);
      Dispose(E, Done);
    end;
  StartTemplate:=OK;
end;

procedure DoneTemplates;
begin
  if Templates<>nil then
    Dispose(Templates, Done);
end;

END.
{
  $Log$
  Revision 1.4  1999-02-16 17:13:56  pierre
   + findclose added for FPC

  Revision 1.3  1999/01/21 11:54:24  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.2  1998/12/28 15:47:52  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.2  1998/12/22 10:39:51  peter
    + options are now written/read
    + find and replace routines

}

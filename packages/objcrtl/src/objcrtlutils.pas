{
  objcrtlutils.pas

  Copyright (C) 2009 Dmitry Boyarintsev
 
  This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 2.0
  headers included with XCode 3.1.2
  The original copyright note of is kept on each include file
}

unit objcrtlutils;

{$mode objfpc}{$H+}

interface

uses
  objcrtl;

function alloc(classname: PChar): id; inline;
function allocex(classname: PChar; extraBytes: Integer): id; inline;
function objcclass(classname: PChar): _class; inline;
function selector(cmdname: PChar): SEL; inline;
procedure release(objc: id); inline;
function AllocAndInit(classname: PChar): id; inline;
function AllocAndInitEx(classname: PChar; extraBytes: Integer): id; inline;
function super(obj: id): objc_super;

implementation

function super(obj: id): objc_super;
begin
  Result.reciever := obj;
  Result.class_ := class_getSuperclass(object_getClass(obj));
end;

function allocex(classname: PChar; extraBytes: Integer): id; inline;
begin
  Result := class_createInstance( objcclass(classname), extraBytes);
end;

function alloc(classname: PChar): id; inline;
begin
  Result := allocex(classname, 0);
  // Result := objc_msgSend( objc_getClass(classname), selector('alloc'), []);
end;

function objcclass(classname: PChar): _class; inline;
begin
  Result := _class(objc_getClass(classname));
end;

function selector(cmdname: PChar): SEL; inline;
begin
  Result := sel_registerName(cmdname);
end;

procedure release(objc: id); inline;
begin
  objc_msgSend(objc, selector('release'), []);
end;

function AllocAndInit(classname: PChar): id; inline;
begin
  Result:= objc_msgSend( alloc( classname ), selector('init'), []);
end;

function AllocAndInitEx(classname: PChar; extraBytes: Integer): id; inline;
begin
  Result := objc_msgSend( allocEx( classname, extraBytes ), selector('init'), []);
end;


end.


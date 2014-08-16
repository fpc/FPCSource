{ Copyright (c) Carl Eric Codere            }
{ This program tests the assigned() routine }
{ Tested against Delphi 6 Personal Edition  }
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
{$ifndef fpc}
  CodePointer = Pointer;
{$endif}

  tmyobject = object
    procedure myroutine(x: byte);
  end;

  tmyclass = class
    procedure myroutine(x: byte);
  end;


  tobjectmethod = procedure (x: byte) of object;
  tclassmethod = procedure (x: byte) of object;
  tproc = procedure (x: byte);


  type
    objpointer = packed record
      _method : codepointer;
      _vmt : pointer;
    end;

var
  myobject : tmyobject;
  myclass : tmyclass;

  procedure fail;
   begin
     WriteLn('Failure!');
     halt(1);
   end;

  procedure mydummyproc(x: byte);
   begin
   end;

  function getpointer : pointer;
   begin
     getpointer := nil;
   end;

  function getprocpointer : tproc;
   begin
     getprocpointer:=@mydummyproc;
   end;

{$ifdef fpc}
  function getobjmethodpointer : tobjectmethod;
   begin
     getobjmethodpointer:=@myobject.myroutine;
   end;

  function getclamethodpointer : tclassmethod;
   begin
     getclamethodpointer:=@myclass.myroutine;
   end;
{$endif}

  procedure tmyclass.myroutine(x: byte);
   begin
   end;

  procedure tmyobject.myroutine(x: byte);
   begin
   end;

  { possible chocies (fixes branch only)  :
      LOC_REGISTER
      LOC_REFERENCE
    second branch handles this in a generic way
  }
var
  objmethod : tobjectmethod;
  clamethod : tclassmethod;
  proc : tproc;
  p : pointer;
  x: array[1..8] of integer;
  _result : boolean;
  ptrrecord : objpointer;
Begin
  myclass := tmyclass.create;
  Write('Assigned(pointer) tests...');
  _result := true;
  p:=@x;
  if not assigned(p) then
    _result := false;
  p:=nil;
  if assigned(p) then
    _result := false;
{$ifdef fpc}
  if assigned(getpointer) then
    _result := false;
{$endif}

  if _result then
    WriteLn('Success!')
  else
    fail;
  {*******************************************************}
  Write('Assigned(proc) tests...');
  _result := true;
  proc:=@mydummyproc;
  if not assigned(proc) then
    _result := false;
  proc:=nil;
{$ifdef fpc}
  if assigned(proc) then
    _result := false;
  if not assigned(getprocpointer) then
    _result := false;
{$endif}
  if _result then
    WriteLn('Success!')
  else
    fail;
  {*******************************************************}
  Write('Assigned(object method) tests...');
  _result := true;
{$ifdef fpc}
  objmethod:=@myobject.myroutine;
  if not assigned(objmethod) then
    _result := false;
{$endif}
  objmethod:=nil;
  if assigned(objmethod) then
    _result := false;
  { lets put the individual fields to nil
    This is a hack which should never occur
  }
  objmethod:={$ifdef fpc}@{$endif}myobject.myroutine;
  move(objmethod, ptrrecord, sizeof(ptrrecord));
  ptrrecord._vmt := nil;
  move(ptrrecord, objmethod, sizeof(ptrrecord));
  if not assigned(objmethod) then
    _result := false;

  objmethod:={$ifdef fpc}@{$endif}myobject.myroutine;
  move(objmethod, ptrrecord, sizeof(ptrrecord));
  ptrrecord._method := nil;
  move(ptrrecord, objmethod, sizeof(ptrrecord));
  if assigned(objmethod) then
    _result := false;

{$ifdef fpc}
  if not assigned(getobjmethodpointer) then
    _result := false;
{$endif}

  if _result then
    WriteLn('Success!')
  else
    fail;
  {*******************************************************}
  Write('Assigned(class method) tests...');
  _result := true;
{$ifdef fpc}
  clamethod:=@myclass.myroutine;
  if not assigned(clamethod) then
    _result := false;
{$endif}
  clamethod:=nil;
  if assigned(clamethod) then
    _result := false;
  { lets put the individual fields to nil
    This is a hack which should never occur
  }
  clamethod:={$ifdef fpc}@{$endif}myclass.myroutine;
  move(clamethod, ptrrecord, sizeof(ptrrecord));
  ptrrecord._vmt := nil;
  move(ptrrecord, clamethod, sizeof(ptrrecord));
  if not assigned(clamethod) then
    _result := false;

  clamethod:={$ifdef fpc}@{$endif}myclass.myroutine;
  move(clamethod, ptrrecord, sizeof(ptrrecord));
  ptrrecord._method := nil;
  move(ptrrecord, clamethod, sizeof(ptrrecord));
  if assigned(clamethod) then
    _result := false;

{$ifdef fpc}
  if not assigned(getclamethodpointer) then
    _result := false;
{$endif}

  if _result then
    WriteLn('Success!')
  else
    fail;

end.

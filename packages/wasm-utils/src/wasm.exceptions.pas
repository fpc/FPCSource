{
    This file is part of the Free Component Library

    Export last exception info from Webassembly API.
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.exceptions;

{$mode objfpc}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

Type
  TLastExceptionInfo = record
    ClassName : AnsiString;
    ClassNameLen : Integer;
    Message : UTF8String;
    MessageLen : Integer;
    more : boolean;
  end;
  PLastExceptionInfo = ^TLastExceptionInfo;


function GetLastExceptionInfo : PLastExceptionInfo;
procedure FreeLastExceptionInfo(aInfo : PLastExceptionInfo);

Procedure LogWasmExceptions(aEnable  : boolean; aToConsole : Boolean);

implementation

uses wasm.logger.api;

function PopObjectStack : TObject; external name 'FPC_POPOBJECTSTACK';

function GetLastExceptionInfo : PLastExceptionInfo;

var
  lExc : PExceptObject;
  lMsg : UTF8String;
  Obj : TObject;
begin
  Result:=nil;
  lExc:=RaiseList;
  if lExc=Nil then
    exit;
  Result:=New(PLastExceptionInfo);
  Result^:=Default(TLastExceptionInfo);
  if assigned(lExc^.FObject) then
    begin
    Result^.ClassName:=LExc^.FObject.ClassName;
    if LExc^.FObject is Exception then
      begin
      {$IF SizeOf(Char)=2}
      Result^.Message:=UTF8Encode(Exception(LExc^.FObject).Message);
      {$ELSE}
      Result^.Message:=Exception(LExc^.FObject).Message;
      {$ENDIF}
      end
    else
      Result^.Message:=LExc^.FObject.ToString;
    Result^.ClassNameLen:=Length(Result^.ClassName);
    Result^.MessageLen:=Length(Result^.Message);
    end;
  Result^.More:=Assigned(lExc^.Next);
  ReleaseExceptionObject;
  Obj:=PopObjectStack;
  Obj.Free;

end;

procedure FreeLastExceptionInfo(aInfo : PLastExceptionInfo);
begin
  Dispose(aInfo);
end;

var
  _LogToConsole : Boolean;
  
Procedure DoLogWasmException(Obj : TObject; AnAddr : CodePointer; aFrame : Pointer);
var
  aClass : String;
  aMessage : String;
  lLog : String;
begin
  if assigned(Obj) then
    begin
    aClass:=Obj.ClassName;
    if Obj is Exception then
      aMessage:=Exception(Obj).Message
    else  
      aMessage:=Obj.ToString;
    end
  else
    begin
    aMessage:='';
    aClass:='<Nil>';  
    end;
  lLog:=SafeFormat('Raising exception %s with message "%s"',[aClass,aMessage]);
  if _LogToConsole then
    Writeln(lLog)
  else  
    __wasm_log(wllDebug,'SYSTEM',lLog);
end;

Procedure LogWasmExceptions(aEnable  : boolean; aToConsole : Boolean);
begin
  Writeln('LogWasmExceptions(',aEnable,',',aToConsole);
  _LogToConsole:=aToConsole;  
  if aEnable then
    WasmOnException:=@DoLogWasmException
  else  
    WasmOnException:=Nil;
end;

exports
  GetLastExceptionInfo,
  FreeLastExceptionInfo;

end.


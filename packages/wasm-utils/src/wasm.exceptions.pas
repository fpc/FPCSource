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

implementation

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
  if assigned(lExc.FObject) then
    begin
    Result^.ClassName:=LExc.FObject.ClassName;
    if LExc.FObject is Exception then
      begin
      {$IF SizeOf(Char)=2}
      Result^.Message:=UTF8Encode(Exception(LExc.FObject).Message);
      {$ELSE}
      Result^.Message:=Exception(LExc.FObject).Message;
      {$ENDIF}
      end
    else
      Result^.Message:=LExc.FObject.ToString;
    Result^.ClassNameLen:=Length(Result^.ClassName);
    Result^.MessageLen:=Length(Result^.Message);
    end;
  Result^.More:=Assigned(lExc.Next);
  ReleaseExceptionObject;
  Obj:=PopObjectStack;
  Obj.Free;

end;

procedure FreeLastExceptionInfo(aInfo : PLastExceptionInfo);
begin
  Dispose(aInfo);
end;

exports
  GetLastExceptionInfo,
  FreeLastExceptionInfo;

end.


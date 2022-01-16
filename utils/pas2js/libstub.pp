{
    libstub  -  pas2js stub generator, library version
    Copyright (C) 2017 - 2020 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
library stub;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, stubcreator;

Type
  PStubCreator = Pointer;  

Function GetStubCreator : PStubCreator; stdcall;

begin
  Result:=TStubCreator.Create(Nil);
end;

Procedure FreeStubCreator(P : PStubCreator); stdcall;

begin
  TStubCreator(P).Free;
end;

Function MaybeStr(P : PAnsiChar) : String;

begin
  If Assigned(P) then
    Result:=P
  else
    Result:='';
end;

Procedure SetStubCreatorInputFileName(P : PStubCreator; AFileName : PAnsiChar); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      InputFileName:=AFileName;
end;


Procedure SetStubCreatorConfigFileName(P : PStubCreator; AFileName : PAnsiChar); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      ConfigFileName:=MaybeStr(AFileName);
end;


Procedure SetStubCreatorOutputFileName(P : PStubCreator; AFileName : PAnsiChar); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      OutputFileName:=MaybeStr(AFileName);
end;

Procedure SetStubCreatorHeaderFileName(P : PStubCreator; AFileName : PAnsiChar); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      HeaderFileName:=MaybeStr(AFileName);
end;

Procedure AddStubCreatorDefine(P : PStubCreator; ADefine : PAnsiChar); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      TStubCreator(P).Defines.Add(MaybeStr(ADefine));
end;

Procedure AddStubCreatorForwardClass(P : PStubCreator; AForwardClass : PAnsiChar); stdcall;

Var
  S : String;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      begin
      S:=MaybeStr(AForwardClass);
      if (S<>'') then
        begin
        if TStubCreator(P).ForwardClasses<>'' then
          S:=','+S;
        TStubCreator(P).ForwardClasses:=TStubCreator(P).ForwardClasses+S;
        end;
      end;
end;

Procedure SetStubCreatorHeaderContent(P : PStubCreator; AContent : PAnsiChar); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      HeaderStream:=TStringStream.Create(MaybeStr(AContent));
end;

Procedure SetStubCreatorOuputCallBack(P : PStubCreator; AData : Pointer; ACallBack : TWriteCallBack); stdcall;

begin
  if Assigned(P) then
    With TStubCreator(P) do
      begin
      CallbackData:=AData;
      OnWriteCallBack:=ACallBack;
      end;
end;

Function ExecuteStubCreator(P : PStubCreator) : Boolean; stdcall;

begin
  Result:=TStubCreator(P).Execute;
end;

Procedure GetStubCreatorLastError(P : PStubCreator; AError : PAnsiChar;
  Var AErrorLength : Longint; AErrorClass : PAnsiChar; Var AErrorClassLength : Longint); stdcall;

Var
  L : Integer;
  E,C : String;

begin
  TStubCreator(P).GetLastError(E,C);
  L:=Length(E);
  if (L>AErrorLength) then
    L:=AErrorLength;
  if (L>0) then
    Move(E[1],AError^,L);
  L:=Length(C);
  if L>AErrorClassLength then
    L:=AErrorClassLength;
  if (L>0) then
    Move(C[1],AErrorClass^,L);
end;

Procedure SetStubCreatorUnitAliasCallBack(P : PStubCreator; ACallBack : TUnitAliasCallBack; CallBackData : Pointer); stdcall;
begin
  TStubCreator(P).OnUnitAlias:=ACallBack;
  TStubCreator(P).OnUnitAliasData:=CallBackData;
end;

Procedure AddStubCreatorExtraUnit(P : PStubCreator; AUnitName : PAnsiChar); stdcall;
begin
  TStubCreator(P).ExtraUnits:=AUnitName;
end;

exports
  // Stub creator
  GetStubCreator,
  FreeStubCreator,
  SetStubCreatorInputFileName,
  SetStubCreatorOutputFileName,
  SetStubCreatorHeaderFileName,
  SetStubCreatorConfigFileName,
  SetStubCreatorHeaderContent,
  SetStubCreatorOuputCallBack,
  GetStubCreatorLastError,
  AddStubCreatorDefine,
  AddStubCreatorForwardClass,
  AddStubCreatorExtraUnit,
  ExecuteStubCreator,
  SetStubCreatorUnitAliasCallBack;

end.


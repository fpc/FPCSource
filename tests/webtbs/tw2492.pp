{ Source provided for Free Pascal Bug Report 2492 }
{ Submitted by "Mattias Gaertner" on  2003-05-15 }
{ e-mail: mattias@freepascal.org }
program buggy;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes, SysUtils, TypInfo;

type
  TMyClassA = class;

  TMyClassB = class(TComponent)
  private
    FClassA: TMyClassA;
  published
    property ClassA: TMyClassA read FClassA write FClassA;
  end;

  TMyClassA = class(TComponent)
  end;

var
  MyClassA: TMyClassA;
begin
  MyClassA:=TMyClassA.Create(nil);
  writeln('PropertyCount=',
    GetTypeData(MyClassA.ClassInfo)^.PropCount);
  if GetTypeData(MyClassA.ClassInfo)^.PropCount<>2 then
    halt(1);
end.

program uGenAss;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  generic TGeneric<T> = class
  public
    function Find(AObject: T): T;
  end;

function TGeneric.Find(AObject: T): T;
var
  tt: T;
begin
  if tt <> nil then;
  if AObject <> nil then;

  if Assigned(tt) then;
  if Assigned(AObject) then;
end;

begin
end.

program tConstRef1;

{$mode objfpc}{$h+}

uses
  Classes, SysUtils;

type
  TConstRefProc = procedure(constref AParam: integer);

  TAClass = class(tobject)
  private
    function GetSomething(constref int:integer): integer;
  public
    property Something[constref int:integer] : integer read getSomething;
  end;

function TAClass.GetSomething(constref int: integer): integer;
begin
  if int<>$1234567 then
    halt(1);
  result := $54321;
end;

procedure TestConstRef(constref AParam: integer); [public, alias: '_TESTCONSTREF'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefAlias(AParam: PInteger); [external name '_TESTCONSTREF'];

const c = $1234567;
var a: integer;
    aclass: TAClass;
    p: TConstRefProc;

begin
  a := $1234567;
  TestConstRef(a);
  TestConstRef(c);
  TestConstRef($1234567);
  TestConstRefAlias(@a);

  aclass := TAClass.Create;
  if aclass.Something[a]<>$54321 then
    halt(1);
  aclass.Free;

  p := @TestConstRef;
  p(c);
end.


program texrtti17;

{$mode objfpc}

{ Test that class properties are returned in RTTI }

uses typinfo, uexrttiutil;

{$RTTI INHERIT
       METHODS(DefaultMethodRttiVisibility)
       FIELDS(DefaultFieldRttiVisibility)
       PROPERTIES(DefaultPropertyRttiVisibility)
}

Type
  T1 = Class(TObject)
    class function getsomething : integer; static;
    class property Something : Integer Read GetSomething;
  end;


class function T1.getsomething : integer;

begin
  Result:=0;
end;

var
  aCount : Integer;
  P: PPropListEx;

begin
  aCount:=GetPropListEx(T1,P);
  AssertEquals('class property not in RTTI properties',1,aCount);
end.


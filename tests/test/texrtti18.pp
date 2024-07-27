program texrtti18;

{ Test that array properties do not appear in the list of RTTI properties }

{$mode objfpc}

uses TypInfo, uexrttiutil;

{$RTTI INHERIT
       METHODS(DefaultMethodRttiVisibility)
       FIELDS(DefaultFieldRttiVisibility)
       PROPERTIES(DefaultPropertyRttiVisibility)
}

Type
  T1 = Class(TObject)
    function getsomething(aIndex : Integer) : TObject;
    property Something[a: Integer] : TObject Read GetSomething;
  end;


function T1.getsomething(aIndex : Integer) : TObject;

begin
  Result:=Nil;
end;

var
  aCount : Integer;
  P: PPropListEx;

begin
  aCount:=GetPropListEx(T1,P);
  AssertEquals('class property not in RTTI properties',0,aCount);
end.


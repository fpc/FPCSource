program example9;

{ This program demonstrates the GetEnumName, GetEnumValue functions }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  TI : PTypeInfo;

begin
  O:=TMyTestObject.Create;
  TI:=GetPropInfo(O,'MyEnumField')^.PropType;
  Writeln('GetEnumName           : ',GetEnumName(TI,Ord(O.MyEnumField)));
  Writeln('GetEnumValue(mefirst) : ',GetEnumName(TI,GetEnumValue(TI,'mefirst')));
  O.Free;
end.
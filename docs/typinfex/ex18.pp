program example18;

{ This program demonstrates the SetToString function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;
  I : longint;

begin
  O:=TMyTestObject.Create;
  PI:=GetPropInfo(O,'SetField');
  O.SetField:=[mefirst,meSecond,meThird];
  I:=GetOrdProp(O,PI);
  Writeln('Set property to string : ');
  Writeln('Value  : ',SetToString(PI,I,False));
  O.SetField:=[mefirst,meSecond];
  I:=GetOrdProp(O,PI);
  Writeln('Value  : ',SetToString(PI,I,True));
  I:=StringToSet(PI,'mefirst');
  SetOrdProp(O,PI,I);
  I:=GetOrdProp(O,PI);
  Writeln('Value  : ',SetToString(PI,I,False));
  I:=StringToSet(PI,'[mesecond,methird]');
  SetOrdProp(O,PI,I);
  I:=GetOrdProp(O,PI);
  Writeln('Value  : ',SetToString(PI,I,True));
  O.Free;
end.
program example10;

{ This program demonstrates the IsPublishedProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  Writeln('Property tests    : ');
  Write('IsPublishedProp(O,BooleanField)     : ');
  Writeln(IsPublishedProp(O,'BooleanField'));
  Write('IsPublishedProp(Class,BooleanField) : ');
  Writeln(IsPublishedProp(O.ClassType,'BooleanField'));
  Write('IsPublishedProp(O,SomeField)        : ');
  Writeln(IsPublishedProp(O,'SomeField'));
  Write('IsPublishedProp(Class,SomeField)    : ');
  Writeln(IsPublishedProp(O.ClassType,'SomeField'));
  O.Free;
end.
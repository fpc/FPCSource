program example11;

{ This program demonstrates the IsStoredProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  Writeln('Stored tests    : ');
  Write('IsStoredProp(O,StoredIntegerConstFalse)    : ');
  Writeln(IsStoredProp(O,'StoredIntegerConstFalse'));
  Write('IsStoredProp(O,StoredIntegerConstTrue)     : ');
  Writeln(IsStoredProp(O,'StoredIntegerConstTrue'));
  Write('IsStoredProp(O,StoredIntegerMethod)        : ');
  Writeln(IsStoredProp(O,'StoredIntegerMethod'));
  Write('IsStoredProp(O,StoredIntegerVirtualMethod) : ');
  Writeln(IsStoredProp(O,'StoredIntegerVirtualMethod'));
  O.Free;
end.
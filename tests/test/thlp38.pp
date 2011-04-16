{ the parent in the RTTI of a non derived helper is Nil, otherwise it is the
  typeinfo of the parent helper; also the type info of the extended type is
  available through ExtendedInfo }
program thlp38;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
 typinfo;

type
  TTest = class

  end;

  TTestHelper = class helper for TTest
  end;

  TTestHelperSub = class helper(TTestHelper) for TTest
  end;

var
  titest, titesthelper, titesthelpersub: PTypeInfo;
  td: PTypeData;
  ti: PTypeInfo;
begin
  titest := TypeInfo(TTest);
  titesthelper := TypeInfo(TTestHelper);
  titesthelpersub := TypeInfo(TTestHelperSub);

  if titesthelper^.Kind <> tkHelper then begin
    Writeln('Type is not a helper');
    Halt(1);
  end;
  if titesthelpersub^.Kind <> tkHelper then begin
    Writeln('Type is not a helper');
    Halt(2);
  end;

  td := GetTypeData(titesthelper);
  if td^.ExtendedInfo <> titest then begin
    Writeln('Extends wrong type');
    Halt(4);
  end;

  td := GetTypeData(titesthelpersub);
  if td^.ExtendedInfo <> titest then begin
    Writeln('Extends wrong type');
    Halt(6);
  end;
  if td^.HelperParent <> titesthelper then begin
    Writeln('Wrong parent of helper');
    Halt(7);
  end;

  Writeln('ok');
end.

{ %cpu=i386 }
{$ifdef fpc}{$mode delphi}{$endif}

uses SysUtils;

type TTest = record
              a, b: integer;
             end;

procedure Test(const T: TTest);
 begin
  if @T = nil then exit;
  // do something
 end;

begin
 Test(TTest(nil^));          {case 1}
 Test(TTest(Pointer(nil)^)); {case 2}
end.

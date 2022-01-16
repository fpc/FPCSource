{ %cpu=i386 }
program tw20075;

{$mode delphi}

uses
  Classes;

type

  TNodeArray = array of Pointer;

  { TTest }

  TTest = class
    function GetCount(TheArray: TNodeArray; Count: Integer): Integer;
    function GetCountNoExceptions(TheArray: TNodeArray; Count: Integer): Integer;
  end;

function TTest.GetCount(TheArray: TNodeArray; Count: Integer): Integer; assembler;
asm
  MOV     EAX, ECX
end;

{$IMPLICITEXCEPTIONS OFF}
function TTest.GetCountNoExceptions(TheArray: TNodeArray; Count: Integer): Integer; assembler;
asm
  MOV     EAX, ECX
end;
{$IMPLICITEXCEPTIONS ON}

var
  T: TTest;
  N: TNodeArray;
  I, R: Integer;
begin
  T := TTest.Create;
  I := 10;
  SetLength(N, I);
  R := T.GetCount(N, I);
  if R <> I then
    begin
      WriteLn('Normal: R <> I / R = ', R);
      halt(1);
    end
  else
    WriteLn('Normal: R = I = ', R);
  R := T.GetCountNoExceptions(N, I);
  if R <> I then
    begin
      WriteLn('WithoutException: R <> I / R = ', R);
      halt(1);
    end
  else
    WriteLn('WithoutException: R = I = ', R);
  T.Destroy;
  writeln('ok');
end.


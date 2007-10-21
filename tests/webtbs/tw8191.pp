program PCharRangeChecking;

{$APPTYPE CONSOLE}

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$R+}

function Test: Boolean;
var
  s: shortstring;
  p: PChar;
begin
  s := '1234567890';
  p := PChar(@s[1]);
  Inc(p,4);

  Result :=
   (p[-4] = '1') and
   (p[-3] = '2') and
   (p[-2] = '3') and
   (p[-1] = '4') and
   (p[ 0] = '5') and
   (p[ 1] = '6') and
   (p[ 2] = '7') and
   (p[ 3] = '8') and
   (p[ 4] = '9') and
   (p[ 5] = '0');
end;

begin
  if not Test then
    halt(1);
  WriteLn('ok');
end.

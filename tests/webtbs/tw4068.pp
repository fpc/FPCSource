{ %opt=-Sen }

{ Source provided for Free Pascal Bug Report 4068 }
{ Submitted by "David Butler" on  2005-06-12 }
{ e-mail: djbutler@gmail.com }

{$mode delphi}

function test: boolean;
var A, B : AnsiChar;
begin
  A := '1';
  B := '2';
  Result := ('0' in [A..B]);
end;

begin
  test;
end.

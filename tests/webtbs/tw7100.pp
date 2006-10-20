{ %opt=-Sd }

{$IFNDEF FPC}
{$apptype console}
{$ENDIF}
uses SysUtils;

var
  s1: string;

// To compile in Delphi
{$IFNDEF FPC}
type 
  pinteger = ^integer;
  pbyte = ^byte;
{$ENDIF}

function GetS1: string;
begin
  result := s1;
end;

function ThrowsException(a: integer): string;
begin
  result := '';
  if (a > 0) then
    Abort;
end;

function Test(cmd: integer): integer;
begin
  result := 0;
  if GetS1 <> '' then
  begin
    try
      // GetS1 returns reference to S1, and this reference is stored on a temp variable.
      // It's Ok, until an exception is raised in ThrowsException.
      // The problem is the compiler is planning to store the result of ThrowsException on the same temp variable.
      // As the ThrowsException raises an exception, this temp variable never gets new value, and in fact remains
      // equal to S1.
      // So when the temp variables are cleaned on exit from function Test, the S1's ref counter falls to
      // 0, and S1 is released.
      ThrowsException(cmd);
      result := 1;
    except
      result := -1;
      write('(exception occured) ');
    end;
  end;
end;

procedure DumpString(const s: string);
var
  i: sizeint;
  pi: psizeint;
  pb: pbyte;
begin
  pi := psizeint(s);
  pb := pbyte(pi);

  // Printing reference counter and string length
  dec(pi, 2);
  for i:=1 to 2 do
  begin
    { refcount has to be 1, length 2 -> happens to be the same as i }
    if (pi^ <> i) then
      halt(1);
    write(IntToHex(pi^, sizeof(sizeint)*2),' ');
    inc(pi);
  end;

  // Printing string bytes
  for i:=1 to length(s) do
  begin
    write(IntToHex(pb^, 2), ' ', '''', char(pb^), '''  ');
    inc(pb);
  end;
  writeln;
end;

begin
  s1 := '1'; s1 := s1 + '2'; // making a string with variable ref counter

  write('Dump of S1 at start: '); DumpString(s1);
  writeln;

  // Calling Test(0) - normal flow, string S1 remains in correct state
  writeln('Test(0) -> ', Test(0));
  write('Dump of S1 after Test(0): '); DumpString(s1);
  writeln;

  // Callig Test(1) - exception is raised by ThrowsException function, and this causes incorrect decrement of S1's ref-counter
  writeln('Test(1) -> ', Test(1));
  write('Dump of S1 after Test(1): '); DumpString(s1);
  writeln;
end.


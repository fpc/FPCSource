{ Old file: tbs0250.pp }
{ error with Ansistrings and loops.                    OK 0.99.11 (PFV) }

program testme;

uses erroru;

// Removing this switch removes the bug !!
{$H+}

var A : String;
    P : PChar;
    I : longint;

begin
  P := 'Some sample testchar';
  A := Ansistring(P);
  Writeln ('A : ',A);
  for I:=1 to length(A)-1 do
    begin
    A:='Some small test';
    A:=A+' ansistring';
    Writeln ('A : ',A);
    If A<>'' then
      Writeln ('All is fine')
    else
      begin
         writeln ('Oh-oh!');
         error;
      end;
    end;
end.

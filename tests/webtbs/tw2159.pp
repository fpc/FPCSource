{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2159 }
{ Submitted by "Yakov Sudeikin" on  2002-10-03 }
{ e-mail: yashka@exebook.com }
{$ifdef fpc}{$mode objfpc}{$endif}

var
 a,b,c: array of string;
begin
 setlength(a, 2);
 a[0] := 'asd';
 a[1] := 'qwe';
 b := copy(a);
 c := copy(a, 1, 1);
 if b[0]<>'asd' then
  begin
    writeln('Error 1');
    halt(1);
  end;
 if b[1]<>'qwe' then
  begin
    writeln('Error 2');
    halt(1);
  end;
 if c[0]<>'qwe' then
  begin
    writeln('Error 3');
    halt(1);
  end;
end.

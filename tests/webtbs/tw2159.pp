{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2159 }
{ Submitted by "Yakov Sudeikin" on  2002-10-03 }
{ e-mail: yashka@exebook.com }
{$mode objfpc}

var
 a,b,c: array of string;
begin
 setlength(a, 2);
 a[0] := 'asd';
 a[1] := 'qwe';
 b := copy(a);
 c := copy(a, 1, 1);
end.


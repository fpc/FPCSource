{ Source provided for Free Pascal Bug Report 3418 }
{ Submitted by "raum" on  2004-11-30 }
{ e-mail: raum@forward.to }

Program TEST;
uses Variants, sysutils;

Var
  p : array[0..1] of variant;
  s : string;
begin
 s := '101';
 writeln('doesnt crash');
 p[1] := StrToInt(s);
 p[0] := 'toto';

 writeln('crash'); // IDE internal error
 p[0] := 'toto';
 p[1] := StrToInt(s);
End.

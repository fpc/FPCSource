{ Source provided for Free Pascal Bug Report 1623 }
{ Submitted by "Henrik C. Jessen" on  2001-09-28 }
{ e-mail: henrik.jessen@nettest.com }
PROGRAM Test;

{$inline on}

   FUNCTION fnc(x: integer): integer; INLINE;
   BEGIN
      fnc:=x*2;
   END;

   FUNCTION lfnc(x: longint): longint; INLINE;
   BEGIN
      lfnc:=x*2;
   END;

VAR
   i: Integer;
   j : longint;
BEGIN
   i:=4;
   if fnc(i)<>8 then
     Begin
       Writeln('Error in inlined integer functions');
       RunError(1);
     End;

   j:=4;
   if lfnc(j)<>8 then
     Begin
       Writeln('Error in inlined longint functions');
       RunError(1);
     End;

   j:=lfnc(lfnc(4));
   if j<>16 then
     Begin
       Writeln('Error in inlined longint functions twice');
       RunError(1);
     End;
   i:=fnc(fnc(4));
   if i<>16 then
     Begin
       Writeln('Error in inlined integer functions twice');
       RunError(1);
     End;
END.

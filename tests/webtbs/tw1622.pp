{ Source provided for Free Pascal Bug Report 1622 }
{ Submitted by "Henrik C. Jessen" on  2001-09-28 }
{ e-mail: henrik.jessen@nettest.com }
PROGRAM Test;


{$ifdef go32v2}
CONST
   SomeSegment = $B800;
   SomeOffset  = $0000;
VAR
   AbsOne : Word ABSOLUTE $B800:$0000;
      { -- accepted by FPC }

   AbsTwo : Word ABSOLUTE SomeSegment:SomeOffset;
      { -- NOT accepted by FPC }

   AbsThree : Word ABSOLUTE $B000+$400*2:24*16+4-32;
   AbsFour : Word ABSOLUTE SomeSegment+$400*2:SomeOffset*4-32;
   { The two variables above are at the same }
   { address; that is correctly recognized   }
   { by Borland Pascal.                      }
   {-----------------------------------------}
{$endif go32v2}


{ Adding some test code for normal absolute stuff }

procedure testabs(var x : longint);
var
  y : longint absolute x;
begin
  y:=x+1;
end;

{ If x is diclared as const parametr
  the compilation should fail }

procedure testabs2({const }x : longint);
var
  y : longint absolute x;
begin
  y:=x+1;
end;

procedure teststring(s : string);
var
  y : byte absolute s;
begin
  { this will truncate the string to 4 chars }
  y:=4;
  Writeln(s);
end;

procedure teststringvar(var s : string);
var
  y : byte absolute s;
begin
  { this will truncate the string to 4 chars }
  y:=4;
  Writeln(s);
end;

procedure test_global_var;
var
  y : longint absolute system.exitcode;
begin
  y := 315;
end;

const
  x : longint = 5;
  y : longint = 7;
var
  s : string;

BEGIN
  testabs(x);
  if x<>6 then
    begin
      Writeln('Error in absolute handling');
      Halt(1);
    end;
  testabs2(y);
  if y<>7 then
    begin
      Writeln('Error in absolute handling');
      Halt(1);
    end;
  s:='Test dummy string';
  teststring(s);
  if s<>'Test dummy string' then
    begin
      Writeln('Error in absolute handling for strings');
      Halt(1);
    end;
  teststringvar(s);
  if s<>'Test' then
    begin
      Writeln('Error in absolute handling for strings');
      Halt(1);
    end;
  test_global_var;
  if exitcode<>315 then
    begin
      Writeln('Error in absolute handling');
      Halt(1);
    end;
  exitcode:=0;
END.

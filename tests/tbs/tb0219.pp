{ Old file: tbs0258.pp }
{ bugs in small const set extension to large sets       OK 0.99.12 (PM) }

{$ifdef fpc}
{$mode tp}
{$endif fpc}
program test_set;

uses erroru;

{$R-}

procedure test;

   var
      i : longint;
      j : integer;
      k : word;
      l : shortint;
      m : byte;
      x : array [1..32] of byte;

   begin
      for i:=1 to 32 do x[i]:=$ff;
      i:=1;
      if not(i in [1,3,5,8,11,14,15]) then
        begin
           writeln('Error in set');
           error;
        end;
      i:=135;
      if i in [1,3,5,8,11,14,15] then
        begin
           writeln('Error : 135 is in [1,3,5,8,11,14,15]');
           error;
        end;
      i:=257;
      if not(i in [1,3,5,8,11,14,15]) then
        begin
           writeln('Error : 257 isn''t in [1,3,5,8,11,14,15]');
           error;
        end;
      l:=-1;
      if not(l in [1,3,5,8,11,14,15,255]) then
        begin
           writeln('Error : -1 isn''t in [1,3,5,8,11,14,15,255]');
           error;
        end;
      i:=257;
      if not(l in [1,3,5,8,11,14,15,255]) then
        begin
           writeln('Error : longint(257) isn''t in [1,3,5,8,11,14,15,255]');
           error;
        end;
      for i:=1 to 32 do x[i]:=0;
      i:=135;
      if i in [1,3,5,8,11,14,15] then
        begin
           writeln('Second try Error : 135 is in [1,3,5,8,11,14,15]');
           error;
        end;
   end;

begin
   test;
end.

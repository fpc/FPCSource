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
      if i in [1,3,5,8,11,14,15] then
        writeln('1 is in [1,3,5,8,11,14,15]')
      else
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
      if i in [1,3,5,8,11,14,15] then
        begin
           writeln('Error : 257 is in [1,3,5,8,11,14,15]');
           error;
        end;
      l:=-1;
      if l in [1,3,5,8,11,14,15,255] then
        begin
           writeln('Error : -127 is in [1,3,5,8,11,14,15,255]');
           error;
        end;
      i:=257;
      if l in [1,3,5,8,11,14,15,255] then
        begin
           writeln('Error : longint(257) is in [1,3,5,8,11,14,15,255]');
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
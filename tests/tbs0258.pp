program test_set;

var error : boolean;


procedure test;

   var
      i : longint;
      x : array [1..32] of byte;

   begin
      error:=false;
      for i:=1 to 32 do x[i]:=$ff;
      i:=1;
      if i in [1,3,5,8,11,14,15] then
        writeln('1 is in [1,3,5,8,11,14,15]')
      else
        writeln('Error in set');
      i:=135;
      if i in [1,3,5,8,11,14,15] then
        begin
           writeln('Error : 135 is in [1,3,5,8,11,14,15]');
           error:=true;
        end;
      for i:=1 to 32 do x[i]:=0;
      i:=135;
      if i in [1,3,5,8,11,14,15] then
        begin
           writeln('Second try Error : 135 is in [1,3,5,8,11,14,15]')
           error:=true;
        end
      else
        begin
           if error then
             writeln('Result of 135 in [1,3,5,8,11,14,15] depends on x array !!');
        end;
   end;

begin
   test;
   if error then halt(1);
end.

{$mode objfpc}

var
  counter: integer;

{ exit statement in try..except must not bypass finally code of outer try..finally }
procedure test;
 begin
   try
     try
       inc(counter);
       exit;
     except
     end;
   finally
     inc(counter);
   end;
 end;
 
 begin
   counter:=0;
   test;
   if counter<>2 then
     Halt(1);
 end.
Program PackedAssignTest;
Uses uprec7;

Var BRecord:TRecord;
begin
 if SizeOf (BRecord) <> s then
   begin
     WriteLn ('failed 1');
     Halt
   end;
 BRecord := ARecord;
 with BRecord do
   if (a or not b or not c or not d or e or not f or g or h or not i)then
   begin
     WriteLn ('failed 2');
     Halt(1);
   end;
 WriteLn ('OK')
end.


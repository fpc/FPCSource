uses SysUtils;

{$mode objfpc}

{$R+}
procedure x(arr : array of byte);
 begin
   try
    if arr[12] <> $55 then
      WriteLn('Error! No Rangecheck error detected');
    Halt(1);
   except
     on e : exception do
       begin
         Writeln(e.message);
       end;
   end;
 end;

var
 arr : array[1..12] of byte;
begin
 arr[12] := $55;
 x(arr);
end.

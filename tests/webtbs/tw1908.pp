{$ifdef fpc}{$mode objfpc}{$endif}

uses SysUtils;

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

procedure varx(arr : array of byte);
  var
    i : byte;
 begin
   try
    i:=12;
    if arr[i] <> $55 then
      WriteLn('Error! No Rangecheck error detected');
    Halt(1);
   except
     on e : exception do
       begin
         Writeln(e.message);
       end;
   end;
 end;

procedure x2(arr : array of byte);
 begin
    if arr[12] <> $55 then
      begin
        WriteLn('Error! No Rangecheck error detected');
        Halt(1);
      end;
 end;

procedure varx2(arr : array of byte);
  var
    i : byte;
 begin
    i:=12;
    if arr[12] <> $55 then
      begin
        WriteLn('Error! No Rangecheck error detected');
        Halt(1);
      end;
 end;

var
 arr : array[1..12] of byte;
 arr2 : array[1..13] of byte;
 arr3 : array[-1..11] of byte;
begin
 arr[12] := $55;
 x(arr);
 varx(arr);
 arr2[13]:=$55;
 x2(arr2);
 varx2(arr2);
 arr3[11]:=$55;
 x2(arr3);
 varx2(arr3);
end.

{$mode objfpc}
type
   to1 = class
       function GetCaps1 : Longint;virtual;abstract;
       function GetCaps2 : Longint;virtual;stdcall;
       function GetCaps : Longint;virtual;stdcall;abstract;
   end;

function to1.GetCaps2 : Longint;stdcall;
begin
end;

begin
end.

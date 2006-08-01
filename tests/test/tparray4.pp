{$mode macpas}

program tparray4;

type
{$ifc defined __GPC__}
    Int32 = Integer attribute ( size = 32);
{$elsec}
    Int32 = longint;
{$endif}

type
  GPCKeyMap = packed array[0..127] of boolean;
  FPCKeyMap = array [0..3] of Int32;

var
  km: GPCKeymap;
begin
  fillchar(km,sizeof(km),0);
  km[56] := true;
  if (FPCKeyMap(km)[0] <> 0) or
     (FPCKeyMap(km)[1] <> 128) or
     (FPCKeyMap(km)[2] <> 0) or
     (FPCKeyMap(km)[3] <> 0) then
    begin
      writeln('error');
      halt(1);
    end;
end.


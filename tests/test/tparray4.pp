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
  FPCKeyMap = array [0..15] of byte;

var
  km: GPCKeymap;
begin
  if bitsizeof(km[1]) <> 1 then
    halt(1);
  if bitsizeof(FPCKeyMap(km)[0]) <> 8 then
    halt(2);
  fillchar(km,sizeof(km),0);
  km[56] := true;
  if (FPCKeyMap(km)[0] <> 0) or
     (FPCKeyMap(km)[1] <> 0) or
     (FPCKeyMap(km)[2] <> 0) or
     (FPCKeyMap(km)[3] <> 0) or
     (FPCKeyMap(km)[4] <> 0) or
     (FPCKeyMap(km)[5] <> 0) or
     (FPCKeyMap(km)[6] <> 0) or
{$ifdef ENDIAN_BIG}
     (FPCKeyMap(km)[7] <> 128) or
{$else ENDIAN_BIG}
     (FPCKeyMap(km)[7] <> 1) or
{$endif ENDIAN_BIG}
     (FPCKeyMap(km)[8] <> 0) or
     (FPCKeyMap(km)[9] <> 0) or
     (FPCKeyMap(km)[10] <> 0) or
     (FPCKeyMap(km)[11] <> 0) or
     (FPCKeyMap(km)[12] <> 0) or
     (FPCKeyMap(km)[13] <> 0) or
     (FPCKeyMap(km)[14] <> 0) or
     (FPCKeyMap(km)[15] <> 0) then
    begin
      writeln('error');
      halt(1);
    end;
  writeln('ok');
end.



{$mode delphi}

type
 TMyClass = class
   i, i2 :LongInt;
 end;

begin
 if ptruint(@TMyClass(pointer(5)).i2)<>(5+sizeof(pointer)+4) then
   halt(1);
end.


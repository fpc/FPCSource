 {$mode delphi}

type
 TMyClass = class
   i, i2 :LongInt;
 end;

begin
 if ptruint(@TMyClass(pointer(5)).i2)<>(5+sizeof(pointer){$IFDEF FPC_HAS_FEATURE_MONITOR}*2{$ENDIF FPC_HAS_FEATURE_MONITOR}+4) then
   halt(1);
end.

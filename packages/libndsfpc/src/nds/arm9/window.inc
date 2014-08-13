{$ifdef NDS_INTERFACE}

type 
  WINDOW = integer;
const
  WINDOW_0: WINDOW    = DISPLAY_WIN0_ON;
  WINDOW_1: WINDOW    = DISPLAY_WIN1_ON;  
  WINDOW_OBJ: WINDOW  = DISPLAY_SPR_WIN_ON;
  WINDOW_OUT: WINDOW  = (1 shl 16);
 
{$DEFINE WINDOW_MASK := (WINDOW_0 or WINDOW_1 or WINDOW_OBJ)} 

procedure windowEnable(w: WINDOW); inline;
procedure windowDisable(w: WINDOW); inline;
procedure windowEnableSub(w: WINDOW); inline;
procedure windowDisableSub(w: WINDOW); inline;

procedure windowSetBounds(w: WINDOW; left: cuint8; top: cuint8; right: cuint8; bottom: cuint8); cdecl; external;
procedure windowSetBoundsSub(w: WINDOW; left: cuint8; top: cuint8; right: cuint8; bottom: cuint8); cdecl; external;
procedure bgWindowEnable(id: cint; w: WINDOW); cdecl; external;
procedure bgWindowDisable(id: cint; w: WINDOW); cdecl; external;
procedure oamWindowEnable(oam: POamState; w: WINDOW); cdecl; external;
procedure oamWindowDisable(oam: POamState; w: WINDOW); cdecl; external;

{$endif NDS_INTERFACE}


{$ifdef NDS_IMPLEMENTATION}
procedure windowEnable(w: WINDOW); inline;
begin
  REG_DISPCNT^ := REG_DISPCNT^ or (w and WINDOW_MASK);
end;

procedure windowDisable(w: WINDOW); inline;
begin
  REG_DISPCNT^ := REG_DISPCNT^ and not (w and WINDOW_MASK);
end;

procedure windowEnableSub(w: WINDOW); inline;
begin
  REG_DISPCNT_SUB^ := REG_DISPCNT_SUB^ or (w and WINDOW_MASK);
end;

procedure windowDisableSub(w: WINDOW); inline;
begin
  REG_DISPCNT_SUB^ := REG_DISPCNT_SUB^ and not (w and WINDOW_MASK);
end;

{$endif NDS_IMPLEMENTATION}
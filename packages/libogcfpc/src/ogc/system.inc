{$ifdef OGC_INTERFACE}


const
  SYS_BASE_CACHED   = $80000000;
  SYS_BASE_UNCACHED = $C0000000;

  SYS_WD_NULL = $ffffffff;

  SYS_RESTART = 0;			
  SYS_HOTRESET = 1;
  SYS_SHUTDOWN = 2;			
                       
  SYS_RETURNTOMENU     = 3;
  SYS_POWEROFF         = 4;			
  SYS_POWEROFF_STANDBY = 5;
  SYS_POWEROFF_IDLE    = 6;				


  SYS_PROTECTCHAN0   = 0;
  SYS_PROTECTCHAN1   = 1;			
  SYS_PROTECTCHAN2   = 2;			
  SYS_PROTECTCHAN3   = 3;			
  SYS_PROTECTCHANMAX = 4;	

  SYS_PROTECTNONE  = $00000000;
  SYS_PROTECTREAD  = $00000001;
  SYS_PROTECTWRITE = $00000002;
  SYS_PROTECTRDWR  = (SYS_PROTECTREAD or SYS_PROTECTWRITE);

  SYS_FONTSIZE_ANSI = (288 + 131072);
  SYS_FONTSIZE_SJIS = (3840 + 1179648);

function MEM_VIRTUAL_TO_PHYSICAL(x: integer): cuint32; inline;
function MEM_PHYSICAL_TO_K0(x: cuint32): pointer; inline;
function MEM_PHYSICAL_TO_K1(x: cuint32): pointer; inline;
function MEM_K0_TO_PHYSICAL(x: cuint32): pointer; inline;
function MEM_K1_TO_PHYSICAL(x: cuint32): pointer; inline;
function MEM_K0_TO_K1(x: cuint32): pointer; inline;
function MEM_K1_TO_K0(x: cuint32): pointer; inline;


{ $define SYS_GetArenaLo    := SYS_GetArena1Lo}
{ $define SYS_SetArenaLo    := SYS_SetArena1Lo}
{ $define SYS_GetArenaHi    := SYS_GetArena1Hi}
{ $define SYS_SetArenaHi    := SYS_SetArena1Hi}
{$define SYS_GetArenaSize()  := SYS_GetArena1Size()}

function SYS_GetArenaLo(): pointer; inline;
procedure SYS_SetArenaLo(newLo: pointer); inline;
function SYS_GetArenaHi(): pointer; inline;
procedure SYS_SetArenaHi(newHi: pointer); inline;
//function SYS_GetArenaSize(): cuint32; inline;


type
  syswd_t = cuint32;
  psyswd_t = ^syswd_t;

  _syssram = packed record
    checksum: cuint16;
    checksum_inv: cuint16;
    ead0: cuint32;
    ead1: cuint32;
    counter_bias: cuint32;
    display_offsetH: cint8;
    ntd: cuint8;
    lang: cuint8;
    flags: cuint8;
  end;
  syssram = _syssram;

  _syssramex = packed record
    flash_id: array [0..1, 0..11] of cuint8;
    wirelessKbd_id: cuint32;
    wirelessPad_id: array [0..3] of cuint16;
    dvderr_code: cuint8;
    __padding0: cuint8;
    flashID_chksum: array [0..1] of cuint16;
    __padding1: array [0..3] of cuint8;
  end;
  syssramex = _syssramex;

  alarmcallback = procedure(alarm: syswd_t; cb_arg: pointer);
  


  _sys_fontheader = packed record
    font_type: cuint16;
    first_char: cuint16;
    last_char: cuint16;
    inval_char: cuint16;
    asc: cuint16;
    desc: cuint16;
    width: cuint16;
    leading: cuint16;
    cell_width: cuint16;
    cell_height: cuint16;
    sheet_size: cuint32;
    sheet_format: cuint16;
    sheet_column: cuint16;
    sheet_row: cuint16;
    sheet_width: cuint16;
    sheet_height: cuint16;
    width_table: cuint16;
    sheet_image: cuint32;
    sheet_fullsize: cuint32;
    c0: cuint8;
    c1: cuint8;
    c2: cuint8;
    c3: cuint8;
  end;
  sys_fontheader = _sys_fontheader;
  psys_fontheader = ^sys_fontheader;



  resetcallback = procedure;
  powercallback = procedure;
  resetfunction = function(_final: cint32): cint32;

  _sys_resetinfo = record
    node: lwp_node;
    func: resetfunction;
    prio: cuint32;
  end;
  sys_resetinfo = _sys_resetinfo;
  psys_resetinfo = ^sys_resetinfo;


//deprecated!
procedure SYS_Init(); cdecl; external;


function SYS_AllocateFramebuffer(rmode: pGXRModeObj): pointer; cdecl; external;


procedure SYS_ProtectRange(chan: cuint32; addr: pointer; bytes, cntrl: cuint32); cdecl; external;
procedure SYS_StartPMC(mcr0val, mcr1val: cuint32); cdecl; external;
procedure SYS_DumpPMC(); cdecl; external;
procedure SYS_StopPMC(); cdecl; external;


function SYS_CreateAlarm(thealarm: psyswd_t): cint32; cdecl; external;


function SYS_SetAlarm(thealarm: syswd_t; const tp: ptimespec; cb: alarmcallback; cbarg: pointer): cint32; cdecl; external;


function SYS_SetPeriodicAlarm(thealarm: syswd_t; const tp_start: ptimespec; const tp_period: ptimespec; cb: alarmcallback; cbarg: pointer): cint32; cdecl; external;


function SYS_RemoveAlarm(thealarm: syswd_t): cint32; cdecl; external;


function SYS_CancelAlarm(thealarm: syswd_t): cint32; cdecl; external;


procedure SYS_SetWirelessID(chan, id: cuint32); cdecl; external;
function SYS_GetWirelessID(chan: cuint32): cuint32; cdecl; external;
function SYS_GetFontEncoding(): cuint32; cdecl; external;
function SYS_InitFont(font_data: psys_fontheader): cuint32; cdecl; external;
procedure SYS_GetFontTexture(c: cint32; image: pointer; xpos, ypos, width: pcint32); cdecl; external;
procedure SYS_GetFontTexel(c: cint32; image: pointer; pos, stride: cint32; width: pcint32); cdecl; external;
procedure SYS_ResetSystem(reset: cint32; reset_code: cuint32; force_menu: cint32); cdecl; external;
procedure SYS_RegisterResetFunc(info: psys_resetinfo); cdecl; external;
procedure SYS_UnregisterResetFunc(info: psys_resetinfo); cdecl; external;
function SYS_GetArenaSize(): cuint32; cdecl; external;
procedure SYS_SwitchFiber(arg0, arg1, arg2, arg3, pc, newsp: cuint32); cdecl; external;

function SYS_GetArena1Lo(): pointer; cdecl; external;
procedure SYS_SetArena1Lo(newLo: pointer); cdecl; external;
function SYS_GetArena1Hi(): pointer; cdecl; external;
procedure SYS_SetArena1Hi(newHi: pointer); cdecl; external;
function SYS_GetArena1Size(): cuint32; cdecl; external;

function SYS_SetResetCallback(cb: resetcallback): resetcallback; cdecl; external;

function SYS_ResetButtonDown(): cuint32; cdecl; external;

{$ifdef HW_RVL}
function SYS_GetHollywoodRevision(): cuint32; cdecl; external;
function SYS_GetArena2Lo(): pointer; cdecl; external;
procedure SYS_SetArena2Lo(newLo: pointer); cdecl; external;
function SYS_GetArena2Hi(): pointer; cdecl; external;
procedure SYS_SetArena2Hi(newHi: pointer); cdecl; external;
function SYS_GetArena2Size(): cuint32; cdecl; external;
function SYS_SetPowerCallback(cb: powercallback): powercallback; cdecl; external;
{$endif HW_RVL}

procedure kprintf(const str: Pchar; args: array of const); cdecl; external;

{$endif OGC_INTERFACE}

{$ifdef OGC_IMPLEMENTATION}


function MEM_VIRTUAL_TO_PHYSICAL(x: integer): cuint32; inline;
begin
  MEM_VIRTUAL_TO_PHYSICAL := cuint32(x) and (not SYS_BASE_UNCACHED);
end;

function MEM_PHYSICAL_TO_K0(x: cuint32): pointer; inline;
begin
  MEM_PHYSICAL_TO_K0 := pointer(cuint32(x) + SYS_BASE_CACHED);
end;

function MEM_PHYSICAL_TO_K1(x: cuint32): pointer; inline;
begin
  MEM_PHYSICAL_TO_K1 := pointer(cuint32(x) + SYS_BASE_UNCACHED);
end;

function MEM_K0_TO_PHYSICAL(x: cuint32): pointer; inline;
begin
  MEM_K0_TO_PHYSICAL := pointer(cuint32(x) - SYS_BASE_CACHED);
end;

function MEM_K1_TO_PHYSICAL(x: cuint32): pointer; inline;
begin
  MEM_K1_TO_PHYSICAL := pointer(cuint32(x) - SYS_BASE_UNCACHED);
end;

function MEM_K0_TO_K1(x: cuint32): pointer; inline;
begin
  MEM_K0_TO_K1 := pointer(cuint32(x) + (SYS_BASE_UNCACHED - SYS_BASE_CACHED));
end;

function MEM_K1_TO_K0(x: cuint32): pointer; inline;
begin
  MEM_K1_TO_K0 := pointer(cuint32(x) - (SYS_BASE_UNCACHED - SYS_BASE_CACHED));
end;


{ $define SYS_GetArenaLo    := SYS_GetArena1Lo}
{ $define SYS_SetArenaLo    := SYS_SetArena1Lo}
{ $define SYS_GetArenaHi    := SYS_GetArena1Hi}
{ $define SYS_SetArenaHi    := SYS_SetArena1Hi}
{ $define SYS_GetArenaSize  := SYS_GetArena1Size}

function SYS_GetArenaLo(): pointer; inline;
begin
  SYS_GetArenaLo := SYS_GetArena1Lo();
end;

procedure SYS_SetArenaLo(newLo: pointer); inline;
begin
  SYS_SetArena1Lo(newLo);
end;

function SYS_GetArenaHi(): pointer; inline;
begin
  SYS_GetArenaHi := SYS_GetArena1Hi;
end;

procedure SYS_SetArenaHi(newHi: pointer); inline;
begin
  SYS_SetArena1Hi(newHi);
end;

//function SYS_GetArenaSize(): cuint32; inline;
//begin
//  SYS_GetArenaSize := SYS_GetArena1Size;
//end;


{$endif OGC_IMPLEMENTATION}

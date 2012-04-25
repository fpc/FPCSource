{$ifdef NDS_INTERFACE}
const
  REG_DISPSTAT	: pcuint16 = pointer($04000004);
  
type
  DISP_BITS = cint;
const
  DISP_IN_VBLANK    : DISP_BITS = (1 shl 0);
  DISP_IN_HBLANK    : DISP_BITS = (1 shl 1);
  DISP_YTRIGGERED   : DISP_BITS = (1 shl 2);
  DISP_VBLANK_IRQ   : DISP_BITS = (1 shl 3);
  DISP_HBLANK_IRQ   : DISP_BITS = (1 shl 4);
  DISP_YTRIGGER_IRQ : DISP_BITS = (1 shl 5);

  REG_VCOUNT		: pcuint16 = pointer($04000006);
  HALT_CR       : pcuint16 = pointer($04000300);
  REG_POWERCNT  : pcuint16 = pointer($04000304);
  REG_DSIMODE		: pcuint32 = pointer($04004000);  

procedure SetYtrigger(Yvalue: cint); inline;

const
  PM_ARM9_DIRECT = (1 shl 16);

type
  PM_Bits = cint;
const
	PM_SOUND_AMP         : PM_Bits = (1 shl 0);
	PM_SOUND_MUTE        : PM_Bits = (1 shl 1);
	PM_BACKLIGHT_BOTTOM  : PM_Bits = (1 shl 2);
	PM_BACKLIGHT_TOP     : PM_Bits = (1 shl 3);
	PM_SYSTEM_PWR        : PM_Bits = (1 shl 6);

	POWER_LCD      : PM_Bits = PM_ARM9_DIRECT or (1 shl 0);
	POWER_2D_A     : PM_Bits = PM_ARM9_DIRECT or (1 shl 1);
	POWER_MATRIX   : PM_Bits = PM_ARM9_DIRECT or (1 shl 2);
	POWER_3D_CORE  : PM_Bits = PM_ARM9_DIRECT or (1 shl 3);
	POWER_2D_B     : PM_Bits = PM_ARM9_DIRECT or (1 shl 9);
	POWER_SWAP_LCDS: PM_Bits = PM_ARM9_DIRECT or (1 shl 15);
//	POWER_ALL_2D   : PM_Bits = PM_ARM9_DIRECT or POWER_LCD or POWER_2D_A or POWER_2D_B;
	POWER_ALL_2D   : PM_Bits = (1 shl 16) or (1 shl 0) or (1 shl 1) or (1 shl 9);
//	POWER_ALL      : PM_Bits = PM_ARM9_DIRECT or POWER_ALL_2D or POWER_3D_CORE or POWER_MATRIX;
	POWER_ALL      : PM_Bits = (1 shl 16) or (1 shl 0) or (1 shl 1) or (1 shl 9) or (1 shl 3) or (1 shl 2);

//procedure SystemSleep(); cdecl; external;
procedure ledBlink(bm: cint); cdecl; external;

{$ifdef ARM9}
procedure SystemSleep(); cdecl; external;
procedure powerOn(bits: cint); cdecl; external;
procedure powerOff(bits: cint); cdecl; external;

procedure systemMsgHandler(bytes: cint; user_data: pointer); cdecl; external;
procedure systemValueHandler(value: cuint32; data: pointer); cdecl; external;

procedure lcdSwap(); inline; 
procedure lcdMainOnTop(); inline; 
procedure lcdMainOnBottom(); inline; 

procedure systemShutDown(); inline;

function getBatteryLevel(): cuint32; cdecl; external;

procedure setVectorBase(highVector: cint); cdecl; external;

type 
  sysVectors_t = packed record
    reset: cuint32;
    undefined: cuint32;
    swi: cuint32;
    prefetch_abort: cuint32;
    data_abort: cuint32;
    fiq: cuint32;
  end; 
  sysVectors = sysVectors_t;
  PsysVectors = ^sysVectors;

var
  SystemVectors: sysVectors; cvar; external;

//void setSDcallback(void(*callback)(int));
procedure setSDcallback(callback: pointer); cdecl; external; // ??

// Helper functions for heap size
//! returns current start of heap space
function getHeapStart(): pcuint8; cdecl; external;
//! returns current end of heap space
function getHeapEnd(): pcuint8; cdecl; external;
//! returns current heap limit
function getHeapLimit(): pcuint8; cdecl; external;

{$endif ARM9}


{$ifdef ARM7}
type
  ARM7_power = cint;
const
  POWER_SOUND         : ARM7_power = (1 shl 0);
  PM_CONTROL_REG      : ARM7_power = 0;
  PM_BATTERY_REG      : ARM7_power = 1;
  PM_AMPLIFIER_REG    : ARM7_power = 2;
  PM_READ_REGISTER    : ARM7_power = (1 shl 7);
  PM_AMP_OFFSET       : ARM7_power = 2;
  PM_GAIN_OFFSET      : ARM7_power = 3;
  PM_BACKLIGHT_LEVEL  : ARM7_power = 4;
  PM_GAIN_20          : ARM7_power = 0;
  PM_GAIN_40          : ARM7_power = 1;
  PM_GAIN_80          : ARM7_power = 2;
  PM_GAIN_160         : ARM7_power = 3;
  PM_AMP_ON           : ARM7_power = 1;
  PM_AMP_OFF          : ARM7_power = 0;

function PM_LED_CONTROL(m: cint): cint; inline;

procedure installSystemFIFO(); cdecl; external;
procedure systemSleep(); cdecl; external;
function sleepEnabled(): cint; cdecl; external;
function writePowerManagement(reg, command: cint): cint; cdecl; external;

function readPowerManagement(reg: cint): cint; inline; 

procedure powerOn(bits: cint); inline;
procedure powerOff(bits: cint); inline;
procedure readUserSettings(); cdecl; external;
procedure systemShutDown(); cdecl; external;
{$endif ARM7}


type
  BACKLIGHT_LEVELS = cint;
const
	BACKLIGHT_LOW  : BACKLIGHT_LEVELS = 0;
	BACKLIGHT_MED  : BACKLIGHT_LEVELS = 1;
	BACKLIGHT_HIGH : BACKLIGHT_LEVELS = 2;
	BACKLIGHT_MAX  : BACKLIGHT_LEVELS = 3;	

type
  tPERSONAL_DATA = bitpacked record
    RESERVED0: array [0..1] of cuint8;
    
    theme: cuint8;
    birthMonth: cuint8;
    birthDay: cuint8;
    
    RESERVED1: array [0..0] of cuint8;
    
    name: array [0..9] of cint16;
    nameLen: cuint16;
    
    message: array [0..25] of cint16;
    messageLen: cuint16;
    
    alarmHour: cuint8;
    alarmMinute: cuint8;
    
    RESERVED2: array [0..3] of cuint8;
    
    calX1: cuint16;
    calY1: cuint16;
    calX1px: cuint8;
    calY1px: cuint8;
    
    calX2: cuint16;
    calY2: cuint16;
    calX2px: cuint8;
    calY2px: cuint8;
    
  
    _user_data: packed record
      language: 0..3;
      gbaScreen: 0..1;
      defaultBrightness: 0..2;
      autoMode: 0..1;
      RESERVED5: 0..2;
	    settingsLost: 0..1;	
	    RESERVED6: 0..6;
    end;
    RESERVED3: cuint16;
    rtcOffset: cuint32;
    RESERVED4: cuint32;
  end; 
  PERSONAL_DATA = tPERSONAL_DATA;
  PPERSONAL_DATA = ^tPERSONAL_DATA;

const
  PersonalData  : PPERSONAL_DATA = pointer($2FFFC80);

type
  RTCtime = packed record
    year: cuint8;        // add 2000 to get 4 digit year
    month: cuint8;       // 1 to 12
    day: cuint8;         // 1 to (days in month)
    weekday: cuint8;     // day of week
    hours: cuint8;       // 0 to 11 for AM, 52 to 63 for PM
    minutes: cuint8;     // 0 to 59
    seconds: cuint8;     // 0 to 59
//    padding: cuint8;
  end;

const
  ARGV_MAGIC = $5f617267;

type 
  ppcchar = ^pcchar;
  __argv = record 
    argvMagic: cint;		  // argv magic number, set to 0x5f617267 ('_arg') if valid 
    commandLine: pcchar;	// base address of command line, set of null terminated strings
    length: cint;			    // total length of command line
    argc: integer;
    argv: ppcchar;    
    dummy: cint;          // internal use
    host: cuint32;        // internal use, host ip for dslink 
  end;
  Targv = __argv;
  Pargv = ^Targv;

const
  __system_argv: Pargv = pointer($02FFFE70);

const
  BOOTSIG = $62757473746F6F62; // ULL?


type
  __bootstub = packed record      // packed ?
    bootsig: 	cuint64;
    arm9reboot: VoidFn;
    arm7reboot: VoidFn;
    bootsize: cuint32;
  end;


{$ifdef ARM9}
//procedure memCached(address: pointer); cdecl; external;
function memCached(address: pointer): pointer; cdecl; external;
//procedure memUncached(address: pointer); cdecl; external;
function memUncached(address: pointer): pointer; cdecl; external;
procedure resetARM7(address: cuint32); cdecl; external;
{$endif ARM9}

{$ifdef ARM7}
procedure resetARM9(address: cuint32); cdecl; external;
{$endif ARM7}

{$endif NDS_INTERFACE}




{$ifdef NDS_IMPLEMENTATION}

procedure SetYtrigger(Yvalue: cint); inline;
begin
  REG_DISPSTAT^ := (REG_DISPSTAT^ and $007F ) or (Yvalue shl 8) or (( Yvalue and $100 ) shr 1);
end;

{$ifdef ARM9}
procedure lcdSwap(); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ xor POWER_SWAP_LCDS; 
end;

procedure lcdMainOnTop(); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ or cint(POWER_SWAP_LCDS); 
end;

procedure lcdMainOnBottom(); inline; 
begin
  REG_POWERCNT^ := REG_POWERCNT^ and (not cint(POWER_SWAP_LCDS)); 
end;

procedure systemShutDown(); inline;
begin
  powerOn(PM_SYSTEM_PWR);
end;

{$endif ARM9}


{$ifdef ARM7}
function PM_LED_CONTROL(m: cint): cint; inline;
begin
  result := (m) shl 4;
end;

function readPowerManagement(reg: cint): cint; inline; 
begin
	result := writePowerManagement(reg or PM_READ_REGISTER, 0);
end;

procedure powerOn(bits: cint); inline;
begin
	REG_POWERCNT^ := REG_POWERCNT^ or bits;
end;

procedure powerOff(bits: cint); inline;
begin
	REG_POWERCNT^ := REG_POWERCNT^ and not bits;
end;
{$endif ARM7}

{$endif NDS_IMPLEMENTATION}


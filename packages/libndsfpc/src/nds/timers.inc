{$ifdef NDS_INTERFACE}

const
  TIMER0_CR   : pcuint16 = pointer($04000102);
  TIMER1_CR   : pcuint16 = pointer($04000106);
  TIMER2_CR   : pcuint16 = pointer($0400010A);
  TIMER3_CR   : pcuint16 = pointer($0400010E);
function TIMER_CR(n: cint): pcuint16; inline;

const
  TIMER0_DATA    : pcuint16 = pointer($04000100);
  TIMER1_DATA    : pcuint16 = pointer($04000104);
  TIMER2_DATA    : pcuint16 = pointer($04000108);
  TIMER3_DATA    : pcuint16 = pointer($0400010C);
function TIMER_DATA(n: cuint): pcuint16; inline;

const
  BUS_CLOCK = 33513982;

const
  TIMER_ENABLE    = (1 shl 7);
  TIMER_IRQ_REQ   = (1 shl 6);
  TIMER_CASCADE   = (1 shl 2);


type
  ClockDivider = cint;
const
  ClockDivider_1   : ClockDivider = 0;
  ClockDivider_64  : ClockDivider = 1;
  ClockDivider_256 : ClockDivider = 2;
  ClockDivider_1024: ClockDivider = 3;

const
  TIMER_DIV_1     = (0);
  TIMER_DIV_64    = (1);
  TIMER_DIV_256   = (2);
  TIMER_DIV_1024  = (3);

function TIMER_FREQ(n: cint): cint; inline;
function TIMER_FREQ_64(n: cint): cint; inline;
function TIMER_FREQ_256(n: cint): cint; inline;
function TIMER_FREQ_1024(n: cint): cint; inline; 


procedure timerStart(channel: cint; divider: ClockDivider; ticks: cuint16; callback: fp); cdecl; external; 
function timerElapsed(channel: cint): cuint16; cdecl; external;

function timerTick(timer: cuint): cuint16; inline; 
function timerPause(timer: cuint): cuint16; cdecl; external;
procedure timerUnpause(timer: cuint); inline;

function timerStop(channel: cint): cuint16; cdecl; external;
procedure cpuStartTiming(timer: cuint32); cdecl; external;
function cpuGetTiming(): cuint32; cdecl; external;
function cpuEndTiming(): cuint32; cdecl; external;

function timerTicks2usec(ticks: cuint32): cuint32; inline;
function timerTicks2msec(ticks: cuint32): cuint32; inline;

function timerFreqToTicks_1(freq: cint): cuint16; inline;
function timerFreqToTicks_64(freq: cint): cuint16; inline;
function timerFreqToTicks_256(freq: cint): cuint16; inline;
function timerFreqToTicks_1024(freq: cint): cuint16; inline;

{$endif NDS_INTERFACE}



{$ifdef NDS_IMPLEMENTATION}

function TIMER_FREQ(n: cint): cint; inline;
begin
  TIMER_FREQ := cint(-BUS_CLOCK div (n));
end;
	
function TIMER_FREQ_64(n: cint): cint; inline;
begin
  TIMER_FREQ_64 := cint(-(BUS_CLOCK shr 6) div (n));
end;
	
function TIMER_FREQ_256(n: cint): cint; inline;
begin
  TIMER_FREQ_256 := cint(-(BUS_CLOCK shr 8) div (n));
end;

function TIMER_FREQ_1024(n: cint): cint; inline; 
begin
  TIMER_FREQ_1024 := cint(-(BUS_CLOCK shr 10) div (n));
end;

function TIMER_DATA(n: cuint): pcuint16; inline;
begin
  TIMER_DATA := pcuint16($04000100 + (n shl 2));
end;

function TIMER_CR(n: cint): pcuint16; inline;
begin
  TIMER_CR := pcuint16($04000102 + (n shl 2));
end;

function timerTick(timer: cuint): cuint16; inline; 
begin
	timerTick := TIMER_DATA(timer)^;
end;

procedure timerUnpause(timer: cuint); inline;
begin
	TIMER_CR(timer)^ := TIMER_CR(timer)^ or TIMER_ENABLE;
end;

function timerFreqToTicks_1(freq: cint): cuint16; inline;
begin
  result := -BUS_CLOCK div freq;
end;

function timerFreqToTicks_64(freq: cint): cuint16; inline;
begin
  result := (-BUS_CLOCK shr 6) div freq;
end;

function timerFreqToTicks_256(freq: cint): cuint16; inline;
begin
  result := (-BUS_CLOCK shr 8) div freq;
end;

function timerFreqToTicks_1024(freq: cint): cuint16; inline;
begin
  result := (-BUS_CLOCK shr 10) div freq;
end;

function timerTicks2usec(ticks: cuint32): cuint32; inline;
begin
  timerTicks2usec := (cuint64(ticks) * 1000000) div BUS_CLOCK;
end;

function timerTicks2msec(ticks: cuint32): cuint32; inline;
begin
  timerTicks2msec := (cuint64(ticks)*1000) div BUS_CLOCK;
end;

{$endif NDS_IMPLEMENTATION}

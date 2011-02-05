{$ifndef ARM7}
  {$error Touch screen is only available on the ARM7}
{$endif ARM7}

{$ifdef NDS_INTERFACE}
const
  SCREEN_WIDTH	= 256;
  SCREEN_HEIGHT	= 192;


  TSC_MEASURE_TEMP1    = $84;
  TSC_MEASURE_Y        = $90;
  TSC_MEASURE_BATTERY  = $A4;
  TSC_MEASURE_Z1       = $B4;
  TSC_MEASURE_Z2       = $C4;
  TSC_MEASURE_X        = $D0;
  TSC_MEASURE_AUX      = $E4;
  TSC_MEASURE_TEMP2    = $F4;


procedure touchReadXY(var touchPos: touchPosition); cdecl; external;

function touchRead(command: cuint32): cuint16; cdecl; external;
function touchReadTemperature(t1: pcint; t2: pcint): cuint32;  cdecl; external;

function touchPenDown(): cbool; cdecl; external;
{$endif NDS_INTERFACE}

{$ifndef ARM7}
  {$error The clock is only available on the ARM7}
{$endif ARM7}

{$ifdef NDS_INTERFACE}
const
// RTC registers
  WRITE_STATUS_REG1  = $60;
  READ_STATUS_REG1   = $61;
  STATUS_POC       = (1 shl 7);  // read-only, cleared by reading (1 if just powered on)
  STATUS_BLD       = (1 shl 6);  // read-only, cleared by reading (1 if power dropped below the safety threshold)
  STATUS_INT2      = (1 shl 5);  // read-only, INT2 has occured
  STATUS_INT1      = (1 shl 4);  // read-only, INT1 has occured
  STATUS_SC1       = (1 shl 3);  // R/W scratch bit
  STATUS_SC0       = (1 shl 2);  // R/W scratch bit
  STATUS_24HRS     = (1 shl 1);  // 24 hour mode when 1, 12 hour mode when 0
  STATUS_RESET     = (1 shl 0);  // write-only, reset when 1 written

  WRITE_STATUS_REG2  = $62;
  READ_STATUS_REG2   = $63;
  STATUS_TEST      = (1 shl 7);  // 
  STATUS_INT2AE    = (1 shl 6);  // 
  STATUS_SC3       = (1 shl 5);  // R/W scratch bit
  STATUS_SC2       = (1 shl 4);  // R/W scratch bit

  STATUS_32kE      = (1 shl 3);  // Interrupt mode bits
  STATUS_INT1AE    = (1 shl 2);  // 
  STATUS_INT1ME    = (1 shl 1);  // 
  STATUS_INT1FE    = (1 shl 0);  // 

  // full 7 bytes for time and date
  WRITE_TIME_AND_DATE = $64;
  READ_TIME_AND_DATE = $65;

	// last 3 bytes of current time
  WRITE_TIME = $66;
  READ_TIME = $67;


  WRITE_INT_REG1     = $68;
  READ_INT_REG1      = $69;

  READ_INT_REG2      = $6A;
  WRITE_INT_REG2     = $6B;

  READ_CLOCK_ADJUST_REG  = $6C;
  WRITE_CLOCK_ADJUST_REG = $6D;
// clock-adjustment register

  READ_FREE_REG      = $6E;
  WRITE_FREE_REG     = $6F;


procedure rtcReset(); cdecl; external;
procedure rtcTransaction(command: pcuint8; commandLength: cuint32; result: pcuint8; resultLength: cuint32); cdecl; external;
procedure rtcGetTime(time: pcuint8); cdecl; external;
procedure rtcSetTime(time: pcuint8); cdecl; external;

procedure rtcGetTimeAndDate(time: pcuint8); cdecl; external;
procedure rtcSetTimeAndDate(time: pcuint8); cdecl; external;

procedure rtcGetData(var data: cuint8; size: cuint32); cdecl; external;
procedure BCDToInteger(var data: cuint8; length: cuint32); cdecl; external;
procedure integerToBCD(var data: cuint8; length: cuint32); cdecl; external;

procedure initClockIRQ(); cdecl; external;
procedure resyncClock(); cdecl; external;
{$endif NDS_INTERFACE}

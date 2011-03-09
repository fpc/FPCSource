{$ifdef OGC_INTERFACE}

{$ifdef HW_RVL}
const
  TB_BUS_CLOCK: cuint = 243000000;
  TB_CORE_CLOCK: cuint = 729000000;
{$else}
{$ifdef HW_DOL}
const
  TB_BUS_CLOCK: cuint = 162000000;
  TB_CORE_CLOCK: cuint = 486000000;
{$else}
{$endif}
{$endif}

const
  TB_TIMER_CLOCK = 40500;//(TB_BUS_CLOCK div 4000);

  TB_MSPERSEC  = 1000;
  TB_USPERSEC  = 1000000;
  TB_NSPERSEC  = 1000000000;
  TB_NSPERMS   = 1000000;
  TB_NSPERUS   = 1000;
  TB_USPERTICK = 10000;

  TB_SECSPERMIN = 60;
  TB_MINSPERHR  = 60;
  TB_MONSPERYR  = 12;
  TB_DAYSPERYR  = 365;
  TB_HRSPERDAY  = 24;
  TB_SECSPERDAY = (TB_SECSPERMIN * TB_MINSPERHR * TB_HRSPERDAY);
  TB_SECSPERNYR = (365 * TB_SECSPERDAY);

function time(timer: ptime_t): time_t; cdecl; external;
function nanosleep(tb: ptimespec): cint; cdecl; external;

{$endif OGC_INTERFACE}

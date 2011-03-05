
{$IFDEF OGC_INTERFACE}
//{$if defined(HW_RVL)}
//const
//  TB_BUS_CLOCK = 243000000;
//  TB_CORE_CLOCK = 729000000;
//{$elif defined(HW_DOL)}
//const
//  TB_BUS_CLOCK = 162000000;
//  TB_CORE_CLOCK = 486000000;
//{$endif}

//const
//  TB_TIMER_CLOCK =  (TB_BUS_CLOCK div 4000)  ;  //4th of the bus frequency
//  TB_SECSPERMIN = 60;
//  TB_MINSPERHR = 60;
//  TB_MONSPERYR = 12;
//  TB_DAYSPERYR = 365;
//  TB_HRSPERDAY = 24;
//  TB_SECSPERDAY = ( TB_SECSPERMIN * TB_MINSPERHR * TB_HRSPERDAY );
//  TB_SECSPERNYR = ( 365 * TB_SECSPERDAY );
//  TB_MSPERSEC = 1000;
//  TB_USPERSEC = 1000000;
//  TB_NSPERSEC = 1000000000;
//  TB_NSPERMS = 1000000;
//  TB_NSPERUS = 1000;
//  TB_USPERTICK = 10000;

function ticks_to_cycles(ticks: cuint64): cuint64;	inline;
function ticks_to_secs(ticks: cuint64): cuint64;	inline;
function ticks_to_millisecs(ticks: cuint64): cuint64;	inline;
function ticks_to_microsecs(ticks: cuint64): cuint64;	inline;
function ticks_to_nanosecs(ticks: cuint64): cuint64;	inline;
function tick_microsecs(ticks: cuint64): cuint64;	inline;
function tick_nanosecs(ticks: cuint64): cuint64;	inline;
function secs_to_ticks(sec: cuint64): cuint64;	inline;
function millisecs_to_ticks(msec: cuint64): cuint64;	inline;
function microsecs_to_ticks(usec: cuint64): cuint64;	inline;
function nanosecs_to_ticks(nsec: cuint64): cuint64;	inline;
function diff_ticks(tick0, tick1: cuint64): cuint64;	inline;
function LWP_WD_ABS(x: cint64): cint64; inline;

const
  LWP_WD_INACTIVE = 0;
  LWP_WD_INSERTED = 1;
  LWP_WD_ACTIVE = 2;
  LWP_WD_REMOVE = 3;
  LWP_WD_FORWARD = 0;
  LWP_WD_BACKWARD = 1;
  LWP_WD_NOTIMEOUT = 0;


var
  _wd_sync_level : cuint32; external;
  _wd_sync_count : cuint32; external;
  _wd_ticks_since_boot : cuint32; external;
  _wd_ticks_queue : lwp_queue; external;


function gettick: cuint32; cdecl; external;
function gettime: cuint64; cdecl; external;
procedure settime(par0: cuint64); cdecl; external;
function diff_sec(start, end_: cuint64): cuint32; cdecl; external;
function diff_msec(start, end_: cuint64): cuint32; cdecl; external;
function diff_usec(start, end_: cuint64): cuint32; cdecl; external;
function diff_nsec(start, end_: cuint64): cuint32; cdecl; external;


type
  wd_service_routine = procedure(par0: pointer); cdecl;

  _wdcntrl = record
    node : lwp_node;
    start : cuint64;
    id : cuint32;
    state : cuint32;
    fire : cuint64;
    routine : wd_service_routine;
    usr_data : pointer;
  end;
  wd_cntrl = _wdcntrl;
  pwd_cntrl = ^_wdcntrl;


procedure __lwp_watchdog_init; cdecl; external;
procedure __lwp_watchdog_settimer(wd: Pwd_cntrl); cdecl; external;
procedure __lwp_wd_insert(header: Plwp_queue; wd: Pwd_cntrl); cdecl; external;
function __lwp_wd_remove(header: Plwp_queue; wd: Pwd_cntrl): cuint32; cdecl; external;
procedure __lwp_wd_tickle(queue: Plwp_queue); cdecl; external;
procedure __lwp_wd_adjust(queue: Plwp_queue; dir: cuint32; interval: cint64); cdecl; external;


//#ifdef LIBOGC_INTERNAL
//#include <libogc/lwp_watchdog.inl>
//#endif
{$ENDIF}

{$IFDEF OGC_IMPLEMENTATION}

function ticks_to_cycles(ticks: cuint64): cuint64;	inline;
begin
  result :=(((cuint64(ticks)*cuint64((TB_CORE_CLOCK*2) div TB_TIMER_CLOCK)) div 2));
end;

function ticks_to_secs(ticks: cuint64): cuint64;	inline;
begin
  result := ((cuint64(ticks) div cuint64(TB_TIMER_CLOCK*1000)));
end;

function ticks_to_millisecs(ticks: cuint64): cuint64;	inline;
begin
  result  := ((cuint64(ticks) div cuint64(TB_TIMER_CLOCK)));
end;

function ticks_to_microsecs(ticks: cuint64): cuint64;	inline;
begin
  result :=	(((cuint64(ticks)*8) div cuint64(TB_TIMER_CLOCK div 125)));
end;

function ticks_to_nanosecs(ticks: cuint64): cuint64;	inline;
begin
  result 	:= (((cuint64(ticks)*8000) div cuint64(TB_TIMER_CLOCK div 125)));
end;


function tick_microsecs(ticks: cuint64): cuint64;	inline;
begin
  result 	:=	(((cuint64(ticks)*8) mod cuint64(TB_TIMER_CLOCK div 125)));
end;

function tick_nanosecs(ticks: cuint64): cuint64;	inline;
begin
  result 	:= (((cuint64(ticks)*8000) mod cuint64(TB_TIMER_CLOCK div 125)));
end;



function secs_to_ticks(sec: cuint64): cuint64;	inline;
begin
  result 		:=	(cuint64(sec)*(TB_TIMER_CLOCK*1000));
end;

function millisecs_to_ticks(msec: cuint64): cuint64;	inline;
begin
  result  :=	(cuint64(msec)*(TB_TIMER_CLOCK));
end;

function microsecs_to_ticks(usec: cuint64): cuint64;	inline;
begin
  result 	:= ((cuint64(usec)*(TB_TIMER_CLOCK div 125)) div 8);
end;

function nanosecs_to_ticks(nsec: cuint64): cuint64;	inline;
begin
  result 	:=	((cuint64(nsec)*(TB_TIMER_CLOCK div 125)) div 8000);
end;


function diff_ticks(tick0, tick1: cuint64): cuint64;	inline;
begin
  if tick1 < tick0 then
    result := -1 - tick0 + tick1
  else
    result := tick1 - tick0;
end;



function LWP_WD_ABS(x: cint64): cint64; inline;
begin
  if x > 0 then result := (x)
  else
    result := -x;
end;


procedure __lwp_wd_initialize(wd: pwd_cntrl; routine: wd_service_routine; id: cuint32; usr_data: pointer);
begin
	wd^.state := LWP_WD_INACTIVE;
	wd^.id := id;
	wd^.routine := routine;
	wd^.usr_data := usr_data;
end;

function __lwp_wd_first(queue: plwp_queue): pwd_cntrl;
begin
	result := pwd_cntrl(@(queue^.first));
end;

function __lwp_wd_last(queue: plwp_queue): pwd_cntrl;
begin
	result := pwd_cntrl(@(queue^.last));
end;

function __lwp_wd_next(wd: pwd_cntrl): pwd_cntrl;
begin
	result := pwd_cntrl(@(wd^.node.next));
end;

function __lwp_wd_prev(wd: pwd_cntrl): pwd_cntrl;
begin
	result := pwd_cntrl(@(wd^.node.prev));
end;

procedure __lwp_wd_activate(wd: pwd_cntrl);
begin
	wd^.state := LWP_WD_ACTIVE;
end;

procedure __lwp_wd_deactivate(wd: pwd_cntrl);
begin
	wd^.state := LWP_WD_REMOVE;
end;

function __lwp_wd_isactive(wd: pwd_cntrl): cuint32;
begin
	result := 0;
  if (wd^.state = LWP_WD_ACTIVE) then result := 1;
end;

function __lwp_wd_calc_ticks(const time: ptimespec): cuint64;
var
	ticks: cuint64;
begin
	ticks := secs_to_ticks(time^.tv_sec);
	ticks := ticks + nanosecs_to_ticks(time^.tv_nsec);
	result := ticks;
end;

procedure __lwp_wd_tickle_ticks();
begin
	__lwp_wd_tickle(@_wd_ticks_queue);
end;

procedure __lwp_wd_insert_ticks(wd: pwd_cntrl; interval: cint64);
begin
	wd^.start := gettime();
	wd^.fire := (wd^.start + LWP_WD_ABS(interval));
	__lwp_wd_insert(@_wd_ticks_queue, wd);
end;

procedure __lwp_wd_adjust_ticks(dir: cuint32; interval: cint64);
begin
	__lwp_wd_adjust(@_wd_ticks_queue,dir,interval);
end;

procedure __lwp_wd_remove_ticks(wd: pwd_cntrl);
begin
	__lwp_wd_remove(@_wd_ticks_queue,wd);
end;

procedure __lwp_wd_reset(wd: pwd_cntrl);
begin
	__lwp_wd_remove(@_wd_ticks_queue,wd);
	__lwp_wd_insert(@_wd_ticks_queue,wd);
end;


{$ENDIF}


{$ifdef linux}
{$ifdef i386}
  {$define usestime}
{$endif}
{$endif}

{$ifdef usestime}

{$ifdef FPC_USE_LIBC}
function stime (t:ptime_t):cint; external name 'stime';
{$else}
function stime (t:ptime_t):cint; 
begin
 stime:=do_SysCall(Syscall_nr_stime,TSysParam(t));
end;
{$endif}

function settimeofday(tp:ptimeval;tzp:ptimezone):cint;

begin
  result:=stime(@tp^.tv_sec);
end;
{$else}

{$ifdef FPC_USE_LIBC}
function settimeofday(tp:ptimeval;tzp:ptimezone):cint; external clib name 'settimeofday';
{$else}
function settimeofday(tp:ptimeval;tzp:ptimezone):cint;

begin
  settimeofday:=do_SysCall(Syscal_nr_settimeofday,TSysParam(@tp),tzp);
end;
{$endif}

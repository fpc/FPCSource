{

  Support for timezone info in /usr/share/timezone
}

type

  ttzhead=packed record
    tzh_reserved : array[0..19] of byte;
    tzh_ttisgmtcnt,
    tzh_ttisstdcnt,
    tzh_leapcnt,
    tzh_timecnt,
    tzh_typecnt,
    tzh_charcnt  : longint;
  end;

  pttinfo=^tttinfo;
  tttinfo=packed record
    offset : longint;
    isdst  : boolean;
    idx    : byte;
    isstd  : byte;
    isgmt  : byte;
  end;

  pleap=^tleap;
  tleap=record
    transition : longint;
    change     : longint;
  end;

var
  num_transitions,
  num_leaps,
  num_types    : longint;

  transitions  : plongint;
  type_idxs    : pbyte;
  types        : pttinfo;
  zone_names   : pchar;
  leaps        : pleap;

function find_transition(timer:longint):pttinfo;
var
  i : longint;
begin
  if (num_transitions=0) or (timer<transitions[0]) then
   begin
     i:=0;
     while (i<num_types) and (types[i].isdst) do
      inc(i);
     if (i=num_types) then
      i:=0;
   end
  else
   begin
     for i:=1 to num_transitions do
      if (timer<transitions[i]) then
       break;
     i:=type_idxs[i-1];
   end;
  find_transition:=@types[i];
end;


procedure GetLocalTimezone(timer:longint;var leap_correct,leap_hit:longint);
var
  info : pttinfo;
  i    : longint;
begin
{ reset }
  TZDaylight:=false;
  TZSeconds:=0;
  TZName[false]:=nil;
  TZName[true]:=nil;
  leap_correct:=0;
  leap_hit:=0;
{ get info }
  info:=find_transition(timer);
  if not assigned(info) then
   exit;
  TZDaylight:=info^.isdst;
  TZSeconds:=info^.offset;
  i:=0;
  while (i<num_types) do
   begin
     tzname[types[i].isdst]:=@zone_names[types[i].idx];
     inc(i);
   end;
  tzname[info^.isdst]:=@zone_names[info^.idx];
  i:=num_leaps;
  repeat
    if i=0 then
     exit;
    dec(i);
  until (timer>leaps[i].transition);
  leap_correct:=leaps[i].change;
  if (timer=leaps[i].transition) and
     (((i=0) and (leaps[i].change>0)) or
      (leaps[i].change>leaps[i-1].change)) then
   begin
     leap_hit:=1;
     while (i>0) and
           (leaps[i].transition=leaps[i-1].transition+1) and
           (leaps[i].change=leaps[i-1].change+1) do
      begin
        inc(leap_hit);
        dec(i);
      end;
   end;
end;


procedure GetLocalTimezone(timer:longint);
var
  lc,lh : longint;
begin
  GetLocalTimezone(timer,lc,lh);
end;


procedure ReadTimezoneFile(fn:shortstring);

  procedure decode(var l:longint);
  var
    k : longint;
    p : pbyte;
  begin
    p:=pbyte(@l);
    if (p[0] and (1 shl 7))<>0 then
     k:=not 0
    else
     k:=0;
    k:=(k shl 8) or p[0];
    k:=(k shl 8) or p[1];
    k:=(k shl 8) or p[2];
    k:=(k shl 8) or p[3];
    l:=k;
  end;

const
  bufsize = 2048;
var
  buf    : array[0..bufsize-1] of byte;
  bufptr : pbyte;
  f      : longint;

  procedure readfilebuf;
  begin
    bufptr := @buf[0];
    fpread(f, buf, bufsize);
  end;

  function readbufbyte: byte;
  begin
    if bufptr > @buf[bufsize-1] then
      readfilebuf;
    readbufbyte := bufptr^;
    inc(bufptr);
  end;

  function readbuf(var dest; count: integer): integer;
  var
    numbytes: integer;
  begin
    readbuf := 0;
    repeat
      numbytes := (@buf[bufsize-1] + 1) - bufptr;
      if numbytes > count then
        numbytes := count;
      if numbytes > 0 then
      begin
        move(bufptr^, dest, numbytes);
        inc(bufptr, numbytes);
        dec(count, numbytes);
        inc(readbuf, numbytes);
      end;
      if count > 0 then
        readfilebuf
      else
        break;
    until false;
  end;

var
  tzdir  : shortstring;
  tzhead : ttzhead;
  i      : longint;
  chars  : longint;
begin
  if fn='' then
   fn:='localtime';
  if fn[1]<>'/' then
   begin
     tzdir:=fpgetenv('TZDIR');
     if tzdir='' then
      tzdir:='/usr/share/zoneinfo';
     if tzdir[length(tzdir)]<>'/' then
      tzdir:=tzdir+'/';
     fn:=tzdir+fn;
   end;
  f:=fpopen(fn,Open_RdOnly);
  if f<0 then
   exit;
  bufptr := @buf[bufsize-1]+1;
  i:=readbuf(tzhead,sizeof(tzhead));
  if i<>sizeof(tzhead) then
   exit;
  decode(tzhead.tzh_timecnt);
  decode(tzhead.tzh_typecnt);
  decode(tzhead.tzh_charcnt);
  decode(tzhead.tzh_leapcnt);
  decode(tzhead.tzh_ttisstdcnt);
  decode(tzhead.tzh_ttisgmtcnt);

  num_transitions:=tzhead.tzh_timecnt;
  num_types:=tzhead.tzh_typecnt;
  chars:=tzhead.tzh_charcnt;

  reallocmem(transitions,num_transitions*sizeof(longint));
  reallocmem(type_idxs,num_transitions);
  reallocmem(types,num_types*sizeof(tttinfo));
  reallocmem(zone_names,chars);
  reallocmem(leaps,num_leaps*sizeof(tleap));

  readbuf(transitions^,num_transitions*4);
  readbuf(type_idxs^,num_transitions);

  for i:=0 to num_transitions-1 do
   decode(transitions[i]);

  for i:=0 to num_types-1 do
   begin
     readbuf(types[i].offset,4);
     readbuf(types[i].isdst,1);
     readbuf(types[i].idx,1);
     decode(types[i].offset);
     types[i].isstd:=0;
     types[i].isgmt:=0;
   end;

  readbuf(zone_names^,chars);

  for i:=0 to num_leaps-1 do
   begin
     readbuf(leaps[i].transition,4);
     readbuf(leaps[i].change,4);
     decode(leaps[i].transition);
     decode(leaps[i].change);
   end;

  for i:=0 to tzhead.tzh_ttisstdcnt-1 do
   types[i].isstd:=byte(readbufbyte<>0);

  for i:=0 to tzhead.tzh_ttisgmtcnt-1 do
   types[i].isgmt:=byte(readbufbyte<>0);

  fpclose(f);
end;

Const
  // Debian system; contains location of timezone file.
  TimeZoneLocationFile = '/etc/timezone';
  // SuSE has link in /usr/lib/zoneinfo/localtime to /etc/localtime
  // RedHat uses /etc/localtime

  TimeZoneFile = '/etc/localtime';                      // POSIX
  AltTimeZoneFile = '/usr/lib/zoneinfo/localtime';      // Other
  iOSTimeZoneFile = '/var/db/timezone/localtime';      // iOS
{$ifdef BSD}
  BSDTimeZonefile = '/usr/share/zoneinfo';      // BSD usually is POSIX
                                                // compliant though
{$ENDIF}

{$ifndef FPC_HAS_GETTIMEZONEFILE}

function GetTimezoneFile:shortstring;
var
  f,len : longint;
  s : shortstring;
  info : stat;
begin
  GetTimezoneFile:='';
  f:=fpopen(TimeZoneLocationFile,Open_RdOnly);
  if f>0 then
    begin
    len:=fpread(f,s[1],high(s));
    s[0]:=chr(len);
    len:=pos(#10,s);
    if len<>0 then
     s[0]:=chr(len-1);
    fpclose(f);
    GetTimezoneFile:=s;
    end
  // Try SuSE
  else if fpstat(TimeZoneFile,{$ifdef oldlinuxstat}baseunix.stat(info){$else}info{$endif})>=0 then
    GetTimeZoneFile:=TimeZoneFile
  // Try RedHat
  else If fpstat(AltTimeZoneFile,{$ifdef oldlinuxstat}baseunix.stat(info){$else}info{$endif})>=0 then
      GetTimeZoneFile:=AltTimeZoneFile
{$ifdef BSD}
//  else
//       If fpstat(BSDTimeZoneFile,{$ifdef oldlinuxstat}baseunix.stat(info){$else}info{$endif})>=0 then
// GetTimeZoneFile:=BSDTimeZoneFile
{$ENDIF}
{$if (defined(darwin) and defined(arm)) or defined(iphonesim)}
  else If fpstat(iOSTimeZoneFile,info)>=0 then
      GetTimeZoneFile:=iOSTimeZoneFile
{$endif}
end;

{$endif ndef FPC_HAS_GETTIMEZONEFILE}

procedure InitLocalTime;
begin
  ReadTimezoneFile(GetTimezoneFile);
  GetLocalTimezone(fptime);
end;


procedure DoneLocalTime;
begin
  if assigned(transitions) then
   freemem(transitions);
  if assigned(type_idxs) then
   freemem(type_idxs);
  if assigned(types) then
   freemem(types);
  if assigned(zone_names) then
   freemem(zone_names);
  if assigned(leaps) then
   freemem(leaps);
  num_transitions:=0;
  num_leaps:=0;
  num_types:=0;
end;

Procedure ReReadLocalTime;

begin
  DoneLocalTime;
  InitLocalTime;
end;

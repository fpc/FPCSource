{

  Support for timezone info in /usr/share/timezone
}

type

  ttzhead=packed record
    tzh_identifier : array[0..3] of AnsiChar;
    tzh_version : AnsiChar;
    tzh_reserved : array[0..14] of byte;
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
    transition : int64;
    change     : longint;
  end;

var
  num_transitions,
  num_leaps,
  num_types    : longint;

  transitions  : PInt64 = nil;
  type_idxs    : pbyte = Nil;
  types        : pttinfo = Nil;
  zone_names   : PAnsiChar = Nil;
  leaps        : pleap = Nil;

{ TZif v2+ footer (RFC 8536 section 3.3): 
  a POSIX TZ string that describes local time after the last transition. 
  "Slim" zoneinfo files (zic -b slim, the default since tzdata 2020b) 
  have an empty 32-bit v1 section and stop listing transitions once a rule becomes regular, 
  relying on this footer for every later date:
  e.g. Europe/Prague has transitions only up to 1996. 
  Without it FPC sees such files as plain UTC. }
  
type
  TTZRuleKind = (rkJulian1, rkJulian0, rkMonthWeekDay);
  TTZRule = record
    kind  : TTZRuleKind;
    day,
    week,
    month,
    time  : longint;   { time: seconds after local midnight }
  end;

var
  footer_valid,
  footer_has_dst  : boolean;
  footer_std_off,
  footer_dst_off  : longint;   { seconds east of UTC }
  footer_start,
  footer_end      : TTZRule;
  footer_std_type,
  footer_dst_type : longint;   { indexes into types[] }
  num_chars       : longint = 0;

function tz_floordiv(a, b: int64): int64;
begin
  tz_floordiv:=a div b;
  if ((a mod b)<>0) and ((a<0)<>(b<0)) then
    dec(tz_floordiv);
end;

function tz_isleap(y: int64): boolean;
begin
  tz_isleap:=((y mod 4)=0) and (((y mod 100)<>0) or ((y mod 400)=0));
end;

{ days since 1970-01-01 of a proleptic Gregorian date (H. Hinnant) }
function tz_days_from_civil(y, m, d: int64): int64;
var
  era, yoe, doy, doe, mp: int64;
begin
  if m<=2 then
    dec(y);
  era:=tz_floordiv(y,400);
  yoe:=y-era*400;
  if m>2 then mp:=m-3 else mp:=m+9;
  doy:=(153*mp+2) div 5+d-1;
  doe:=yoe*365+yoe div 4-yoe div 100+doy;
  tz_days_from_civil:=era*146097+doe-719468;
end;

function tz_year_from_days(z: int64): int64;
var
  era, doe, yoe, y, doy, mp: int64;
begin
  z:=z+719468;
  era:=tz_floordiv(z,146097);
  doe:=z-era*146097;
  yoe:=(doe-doe div 1460+doe div 36524-doe div 146096) div 365;
  y:=yoe+era*400;
  doy:=doe-(365*yoe+yoe div 4-yoe div 100);
  mp:=(5*doy+2) div 153;
  if mp>=10 then   { January or February belong to the next civil year }
    inc(y);
  tz_year_from_days:=y;
end;

function tz_days_in_month(y, m: int64): longint;
const
  dim: array[1..12] of byte = (31,28,31,30,31,30,31,31,30,31,30,31);
begin
  tz_days_in_month:=dim[m];
  if (m=2) and tz_isleap(y) then
    inc(tz_days_in_month);
end;

{ day (since epoch) on which rule r falls in year y }
function tz_rule_day(const r: TTZRule; y: int64): int64;
var
  first, wd, d: int64;
begin
  case r.kind of
    rkJulian1:
      begin
        d:=r.day-1;                    { Jn: 1..365, Feb 29 never counted }
        if tz_isleap(y) and (r.day>=60) then
          inc(d);
        tz_rule_day:=tz_days_from_civil(y,1,1)+d;
      end;
    rkJulian0:
      tz_rule_day:=tz_days_from_civil(y,1,1)+r.day;   { n: 0..365 }
    else
      begin
        first:=tz_days_from_civil(y,r.month,1);
        wd:=(first+4) mod 7;           { 1970-01-01 was a Thursday }
        if wd<0 then
          inc(wd,7);
        d:=first+(r.day-wd+7) mod 7+(r.week-1)*7;
        while d>=first+tz_days_in_month(y,r.month) do
          dec(d,7);                    { week 5 = last such weekday }
        tz_rule_day:=d;
      end;
  end;
end;

{ Type index the footer gives for timer, and the UTC interval
  [trans_start,trans_end) over which that stays valid. }
function footer_lookup(timer:int64;timerIsUTC:Boolean;var trans_start,trans_end:int64):longint;
var
  ev_time : array[0..5] of int64;
  ev_dst  : array[0..5] of boolean;
  i, j, n : longint;
  y, t    : int64;
  b       : boolean;

  function dst_at(u: int64): boolean;
  var
    k : longint;
  begin
    dst_at:=false;
    for k:=0 to n-1 do
      if ev_time[k]<=u then
        dst_at:=ev_dst[k];
  end;

var
  u, u_std, u_dst : int64;
  isdst : boolean;
begin
  if not footer_has_dst then
    begin
      if num_transitions>0 then
        trans_start:=transitions[num_transitions-1]
      else
        trans_start:=low(trans_start);
      trans_end:=high(trans_end);
      exit(footer_std_type);
    end;

  { DST start/end events of the years around timer, in UTC and sorted;
    the start rule is given in standard time, the end rule in DST }
  y:=tz_year_from_days(tz_floordiv(timer,86400));
  n:=0;
  for i:=-1 to 1 do
    begin
      ev_time[n]:=tz_rule_day(footer_end,y+i)*86400+footer_end.time-footer_dst_off;
      ev_dst[n]:=false;
      inc(n);
      ev_time[n]:=tz_rule_day(footer_start,y+i)*86400+footer_start.time-footer_std_off;
      ev_dst[n]:=true;
      inc(n);
    end;
  for i:=1 to n-1 do
    begin
      j:=i;
      while (j>0) and (ev_time[j-1]>ev_time[j]) do
        begin
          t:=ev_time[j]; ev_time[j]:=ev_time[j-1]; ev_time[j-1]:=t;
          b:=ev_dst[j]; ev_dst[j]:=ev_dst[j-1]; ev_dst[j-1]:=b;
          dec(j);
        end;
    end;

  if timerIsUTC then
    begin
      u:=timer;
      isdst:=dst_at(u);
    end
  else
    begin
      u_std:=timer-footer_std_off;
      u_dst:=timer-footer_dst_off;
      if dst_at(u_std)=dst_at(u_dst) then
        isdst:=dst_at(u_std)
      else
        { in the gap a nonexistent local time is taken as standard time,
          in the overlap the ambiguous one as its first (DST) occurrence }
        isdst:=not dst_at(u_std);
      if isdst then
        u:=u_dst
      else
        u:=u_std;
    end;

  trans_start:=low(trans_start);
  trans_end:=high(trans_end);
  for i:=0 to n-1 do
    if ev_time[i]<=u then
      trans_start:=ev_time[i]
    else
      begin
        trans_end:=ev_time[i];
        break;
      end;
  if (num_transitions>0) and (trans_start<transitions[num_transitions-1]) then
    trans_start:=transitions[num_transitions-1];
  if isdst then
    footer_lookup:=footer_dst_type
  else
    footer_lookup:=footer_std_type;
end;

function find_transition(timer:int64;timerIsUTC:Boolean;var trans_start,trans_end:int64):pttinfo;
var
  i,L,R,CompareRes : longint;
  found : boolean;

  function DoCompare: longint;
  var
    timerUTC: int64;
  begin
    if not timerIsUTC then
      timerUTC:=timer-types[type_idxs[i-1]].offset
    else
      timerUTC:=timer;
    if timerUTC<transitions[i-1] then
      Exit(-1)
    else
    if timerUTC>=transitions[i] then
      Exit(1)
    else
      Exit(0);
  end;
var
  timerLoUTC, timerHiUTC: int64;
begin
  if (num_transitions>0) and not timerIsUTC then
   begin
     timerLoUTC:=timer-types[type_idxs[0]].offset;
     timerHiUTC:=timer-types[type_idxs[num_transitions-1]].offset;
   end
  else
   begin
     timerLoUTC:=timer;
     timerHiUTC:=timer;
   end;

  if footer_valid and (num_transitions=0) then
   begin
     i:=footer_lookup(timer,timerIsUTC,trans_start,trans_end);
   end
  else
  if (num_transitions=0) or (timerLoUTC<transitions[0]) then
   { timer is before the first transition }
   begin
     i:=0;
     while (i<num_types) and (types[i].isdst) do
      inc(i);
     if (i=num_types) then
      i:=0;
     { unknown transition boundaries }
     trans_start:=low(trans_start);
     trans_end:=high(trans_end);
   end
  else
  if footer_valid and (timerHiUTC>=transitions[num_transitions-1]) then
   { timer is after the last transition, the footer describes it }
   begin
     i:=footer_lookup(timer,timerIsUTC,trans_start,trans_end);
   end
  else
  if (num_transitions>0) and (timerHiUTC>=transitions[num_transitions-1]) then
   { timer is after the last transition }
   begin
     i:=type_idxs[num_transitions-1];
     trans_start:=transitions[num_transitions-1];
     trans_end:=high(trans_end);
   end
  else
   { timer in between }
   begin
      // Use binary search.
      L := 1;
      R := num_transitions-1;
      found := false;
      while not found and (L<=R) do
      begin
        I := L + (R - L) div 2;
        CompareRes := DoCompare;
        if (CompareRes>0) then
          L := I+1
        else begin
          R := I-1;
          if (CompareRes=0) then
             found:=true; // break cycle
        end;
      end;
     if not found then
       Exit(nil);
     trans_start:=transitions[i-1];
     trans_end:=transitions[i];
     i:=type_idxs[i-1];
   end;
  find_transition:=@types[i];
end;


procedure DoGetLocalTimezone(info:pttinfo;const trans_start,trans_end:int64;var ATZInfo:TTZInfo);
begin
  ATZInfo.validsince:=trans_start;
  ATZInfo.validuntil:=trans_end;
  ATZInfo.Daylight:=info^.isdst;
  ATZInfo.Seconds:=info^.offset;
end;

procedure DoGetLocalTimezoneEx(timer:int64;info:pttinfo;var ATZInfoEx:TTZInfoEx);
var
  i : longint;
  names: array[Boolean] of PAnsiChar;
begin
  names[true]:=nil;
  names[false]:=nil;
  ATZInfoEx.leap_hit:=0;
  ATZInfoEx.leap_correct:=0;

  i:=0;
  while (i<num_types) do
   begin
     names[types[i].isdst]:=@zone_names[types[i].idx];
     inc(i);
   end;
  names[info^.isdst]:=@zone_names[info^.idx];
  ATZInfoEx.name[true]:=names[true];
  ATZInfoEx.name[false]:=names[false];
  i:=num_leaps;
  repeat
    if i=0 then
     exit;
    dec(i);
  until (timer>leaps[i].transition);
  ATZInfoEx.leap_correct:=leaps[i].change;
  if (timer=leaps[i].transition) and
     (((i=0) and (leaps[i].change>0)) or
      (leaps[i].change>leaps[i-1].change)) then
   begin
     ATZInfoEx.leap_hit:=1;
     while (i>0) and
           (leaps[i].transition=leaps[i-1].transition+1) and
           (leaps[i].change=leaps[i-1].change+1) do
      begin
        inc(ATZInfoEx.leap_hit);
        dec(i);
      end;
   end;
end;

function GetLocalTimezone(timer:int64;timerIsUTC:Boolean;var ATZInfo:TTZInfo):Boolean;
var
  info: pttinfo;
  trans_start,trans_end,timerUTC: int64;
begin
  { check if time is in current global Tzinfo }
  ATZInfo:=CurrentTZinfo[InterlockedExchangeAdd(CurrentTZindex, 0)];
  if not timerIsUTC then
    timerUTC:=timer-ATZInfo.seconds
  else
    timerUTC:=timer;
  if (ATZInfo.validsince<=timerUTC) and (timerUTC<ATZInfo.validuntil) then
    Exit(True);

  LockTZInfo;
  info:=find_transition(timer,timerIsUTC,trans_start,trans_end);
  GetLocalTimezone:=assigned(info);
  if GetLocalTimezone then
    DoGetLocalTimezone(info,trans_start,trans_end,ATZInfo);
  UnlockTZInfo;
end;

function GetLocalTimezone(timer:int64;timerIsUTC:Boolean;var ATZInfo:TTZInfo;var ATZInfoEx:TTZInfoEx):Boolean;
var
  info: pttinfo;
  trans_start,trans_end,timerUTC: int64;
begin
  { check if time is in current global Tzinfo }
  ATZInfo:=CurrentTZinfo[InterlockedExchangeAdd(CurrentTZindex, 0)];
  if not timerIsUTC then
    timerUTC:=timer-ATZInfo.seconds
  else
    timerUTC:=timer;
  if (ATZInfo.validsince<=timerUTC) and (timerUTC<ATZInfo.validuntil) then
    begin
    ATZInfoEx:=TZInfoEx;
    Exit(True);
    end;

  { not current - search through all }
  LockTZInfo;
  info:=find_transition(timer,timerIsUTC,trans_start,trans_end);
  GetLocalTimezone:=assigned(info);
  if GetLocalTimezone then
    begin
    DoGetLocalTimezone(info,trans_start,trans_end,ATZInfo);
    DoGetLocalTimezoneEx(timer,info,ATZInfoEx);
    end;
  UnlockTZInfo;
end;

procedure RefreshTZInfo;
var
  NewTZInfo: TTZInfo;
  NewTZInfoEx: TTZInfoEx;
begin
  LockTZInfo;
  if GetLocalTimezone(fptime,true,NewTZInfo,NewTZInfoEx) then
    SetTZInfo(NewTZInfo,NewTZInfoEx);
  UnlockTZInfo;
end;

Const
  DefaultTimeZoneDir = '/usr/share/zoneinfo';

function TimeZoneDir : ShortString;

begin
  // Observe TZDIR environment variable.
  TimeZoneDir:=fpgetenv('TZDIR');
  if TimeZoneDir='' then
    TimeZoneDir:=DefaultTimeZoneDir;
  if TimeZoneDir[length(TimeZoneDir)]<>'/' then
    TimeZoneDir:=TimeZoneDir+'/';
end;

function ReadTimezoneFile(fn:string) : Boolean;

  function decode(const l:longint):longint;
  begin
    {$IFDEF ENDIAN_LITTLE}
    decode:=SwapEndian(l);
    {$ELSE}
    decode:=l;
    {$ENDIF}
  end;

  function decode(const l:int64):int64;
  begin
    {$IFDEF ENDIAN_LITTLE}
    decode:=SwapEndian(l);
    {$ELSE}
    decode:=l;
    {$ENDIF}
  end;

const
  bufsize = 2048;
var
  buf    : array[0..bufsize-1] of byte;
  bufptr : pbyte;
  bufbytes : tsSize;
  bufoverflow : boolean;
  f      : longint;
  tzhead : ttzhead;

  function readfilebuf : TsSize;
  begin
    bufptr := @buf[0];
    bufbytes:=fpread(f, buf, bufsize);
    readfilebuf:=bufbytes;
  end;

  Procedure checkbufptr(asize : integer);

  var
    a : tssize;

  begin
    a:=bufptr-@buf+asize;
    if (a>bufbytes) then
      bufoverflow:=true;
  end;

  function readbufbyte: byte;
  begin
    if bufptr > @buf[bufsize-1] then
      readfilebuf;
    checkbufptr(1);
    readbufbyte := bufptr^;
    inc(bufptr);
  end;

  function readbuf(dest:pointer; count: integer): integer;
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
        checkbufptr(numbytes);
        if assigned(dest) then
          move(bufptr^, dest^, numbytes);
        inc(bufptr, numbytes);
        dec(count, numbytes);
        inc(readbuf, numbytes);
        inc(dest, numbytes);
      end;
      if count > 0 then
        readfilebuf
      else
        break;
    until false;
  end;

  function readheader: boolean;
  var
    i      : longint;
  begin
    i:=readbuf(@tzhead,sizeof(tzhead));
    if i<>sizeof(tzhead) then
      exit(False);
    tzhead.tzh_timecnt:=decode(tzhead.tzh_timecnt);
    tzhead.tzh_typecnt:=decode(tzhead.tzh_typecnt);
    tzhead.tzh_charcnt:=decode(tzhead.tzh_charcnt);
    tzhead.tzh_leapcnt:=decode(tzhead.tzh_leapcnt);
    tzhead.tzh_ttisstdcnt:=decode(tzhead.tzh_ttisstdcnt);
    tzhead.tzh_ttisgmtcnt:=decode(tzhead.tzh_ttisgmtcnt);
    readheader:=(tzhead.tzh_identifier[0]='T') and (tzhead.tzh_identifier[1]='Z')
      and (tzhead.tzh_identifier[2]='i') and (tzhead.tzh_identifier[3]='f');
  end;

  procedure AllocFields;
  begin
    num_transitions:=tzhead.tzh_timecnt;
    num_types:=tzhead.tzh_typecnt;
    num_leaps:=tzhead.tzh_leapcnt;
    reallocmem(transitions,num_transitions*sizeof(int64));
    reallocmem(type_idxs,num_transitions);
    reallocmem(types,num_types*sizeof(tttinfo));
    num_chars:=tzhead.tzh_charcnt;
    reallocmem(zone_names,num_chars);
    reallocmem(leaps,num_leaps*sizeof(tleap));
  end;


  { true when the next readbufbyte would read past the end of the file }
  function bufateof: boolean;
  begin
    if bufptr > @buf[bufsize-1] then
      readfilebuf;
    bufateof:=(bufptr-@buf)>=bufbytes;
  end;

  { returns the index of a type with this offset/dst/name, adding one if
    needed; -1 if the name no longer fits the byte-sized name index }
  function addtype(off: longint; dst: boolean; const name: shortstring): longint;
  var
    i : longint;
  begin
    for i:=0 to num_types-1 do
      if (types[i].offset=off) and (types[i].isdst=dst) and
         (types[i].idx<num_chars) and
         (strpas(@zone_names[types[i].idx])=name) then
        exit(i);
    if num_chars+length(name)+1>256 then
      exit(-1);
    reallocmem(zone_names,num_chars+length(name)+1);
    move(name[1],zone_names[num_chars],length(name));
    zone_names[num_chars+length(name)]:=#0;
    reallocmem(types,(num_types+1)*sizeof(tttinfo));
    types[num_types].offset:=off;
    types[num_types].isdst:=dst;
    types[num_types].idx:=num_chars;
    types[num_types].isstd:=0;
    types[num_types].isgmt:=0;
    inc(num_chars,length(name)+1);
    addtype:=num_types;
    inc(num_types);
  end;

  function parsename(const s: shortstring; var p: longint; var name: shortstring): boolean;
  begin
    name:='';
    if (p<=length(s)) and (s[p]='<') then
      begin
        inc(p);
        while (p<=length(s)) and (s[p]<>'>') do
          begin
            name:=name+s[p];
            inc(p);
          end;
        if p>length(s) then
          exit(false);
        inc(p);
      end
    else
      while (p<=length(s)) and (s[p] in ['A'..'Z','a'..'z']) do
        begin
          name:=name+s[p];
          inc(p);
        end;
    parsename:=length(name)>=3;
  end;

  function parsenum(const s: shortstring; var p: longint; var n: longint): boolean;
  begin
    parsenum:=false;
    n:=0;
    while (p<=length(s)) and (s[p] in ['0'..'9']) and (n<100000) do
      begin
        n:=n*10+ord(s[p])-ord('0');
        inc(p);
        parsenum:=true;
      end;
  end;

  { [+|-]hh[:mm[:ss]] as written, in seconds }
  function parsetime(const s: shortstring; var p: longint; var secs: longint): boolean;
  var
    sign, h, m, sec : longint;
  begin
    parsetime:=false;
    sign:=1;
    if (p<=length(s)) and (s[p] in ['+','-']) then
      begin
        if s[p]='-' then
          sign:=-1;
        inc(p);
      end;
    if not parsenum(s,p,h) or (h>167) then
      exit;
    m:=0;
    sec:=0;
    if (p<=length(s)) and (s[p]=':') then
      begin
        inc(p);
        if not parsenum(s,p,m) or (m>59) then
          exit;
        if (p<=length(s)) and (s[p]=':') then
          begin
            inc(p);
            if not parsenum(s,p,sec) or (sec>59) then
              exit;
          end;
      end;
    secs:=sign*(h*3600+m*60+sec);
    parsetime:=true;
  end;

  function parserule(const s: shortstring; var p: longint; var r: TTZRule): boolean;
  begin
    parserule:=false;
    if p>length(s) then
      exit;
    if s[p]='J' then
      begin
        inc(p);
        r.kind:=rkJulian1;
        if not parsenum(s,p,r.day) or (r.day<1) or (r.day>365) then
          exit;
      end
    else if s[p]='M' then
      begin
        inc(p);
        r.kind:=rkMonthWeekDay;
        if not parsenum(s,p,r.month) then exit;
        if (p>length(s)) or (s[p]<>'.') then exit;
        inc(p);
        if not parsenum(s,p,r.week) then exit;
        if (p>length(s)) or (s[p]<>'.') then exit;
        inc(p);
        if not parsenum(s,p,r.day) then exit;
        if (r.month<1) or (r.month>12) or (r.week<1) or (r.week>5) or (r.day>6) then
          exit;
      end
    else
      begin
        r.kind:=rkJulian0;
        if not parsenum(s,p,r.day) or (r.day>365) then
          exit;
      end;
    r.time:=7200;
    if (p<=length(s)) and (s[p]='/') then
      begin
        inc(p);
        if not parsetime(s,p,r.time) then
          exit;
      end;
    parserule:=true;
  end;

  procedure parsefooter(const s: shortstring);
  var
    p, off, ti : longint;
    stdname,
    dstname    : shortstring;
  begin
    if s='' then
      exit;
    p:=1;
    if not parsename(s,p,stdname) then exit;
    if not parsetime(s,p,off) then exit;
    footer_std_off:=-off;           { POSIX offsets are west of UTC }
    if p<=length(s) then
      begin
        if not parsename(s,p,dstname) then exit;
        footer_dst_off:=footer_std_off+3600;
        if (p<=length(s)) and (s[p]<>',') then
          begin
            if not parsetime(s,p,off) then exit;
            footer_dst_off:=-off;
          end;
        if p<=length(s) then
          begin
            if s[p]<>',' then exit;
            inc(p);
            if not parserule(s,p,footer_start) then exit;
            if (p>length(s)) or (s[p]<>',') then exit;
            inc(p);
            if not parserule(s,p,footer_end) then exit;
            if p<=length(s) then exit;
          end
        else
          begin                     { no rules given: POSIX default }
            footer_start.kind:=rkMonthWeekDay;
            footer_start.month:=3; footer_start.week:=2;
            footer_start.day:=0;   footer_start.time:=7200;
            footer_end.kind:=rkMonthWeekDay;
            footer_end.month:=11;  footer_end.week:=1;
            footer_end.day:=0;     footer_end.time:=7200;
          end;
        footer_has_dst:=true;
      end;
    ti:=addtype(footer_std_off,false,stdname);
    if ti<0 then exit;
    footer_std_type:=ti;
    if footer_has_dst then
      begin
        ti:=addtype(footer_dst_off,true,dstname);
        if ti<0 then exit;
        footer_dst_type:=ti;
      end;
    footer_valid:=true;
  end;

  { the footer is optional for our purposes: a missing or malformed one
    leaves the file usable as before }
  procedure readfooter;
  var
    footer : shortstring;
    c      : byte;
    toolong: boolean;
  begin
    footer_valid:=false;
    footer_has_dst:=false;
    if bufateof or (readbufbyte<>10) then
      exit;
    footer:='';
    toolong:=false;
    repeat
      if bufateof then
        exit;                       { no closing newline }
      c:=readbufbyte;
      if c=10 then
        break;
      if length(footer)<255 then
        footer:=footer+AnsiChar(c)
      else
        toolong:=true;
    until false;
    if not toolong then
      parsefooter(footer);
  end;

  function readdata: boolean;
  var
    i      : longint;
    longval: longint;
    version: longint;
  begin
    if tzhead.tzh_version>='2' then
      begin
        version:=2;
        // skip version 0
        readbuf(nil,
           tzhead.tzh_timecnt*4  // transitions
          +tzhead.tzh_timecnt    // type_idxs
          +tzhead.tzh_typecnt*6  // types
          +tzhead.tzh_charcnt    // zone_names
          +tzhead.tzh_leapcnt*8  // leaps
          +tzhead.tzh_ttisstdcnt // isstd
          +tzhead.tzh_ttisgmtcnt // isgmt
          );
        readheader; // read version 2+ header
        if tzhead.tzh_version<'2' then
          Exit(False);
      end
    else
      version:=0;

    AllocFields;

    if version=2 then
      begin // read 64bit values
        readbuf(transitions,num_transitions*sizeof(int64));
        for i:=0 to num_transitions-1 do
          transitions[i]:=decode(transitions[i]);
      end
    else
      begin // read 32bit values
        for i:=0 to num_transitions-1 do
         begin
           readbuf(@longval,sizeof(longval));
           transitions[i]:=decode(longval);
         end;
      end;
    readbuf(type_idxs,num_transitions);

    for i:=0 to num_types-1 do
     begin
       readbuf(@types[i].offset,sizeof(LongInt));
       types[i].offset:=decode(types[i].offset);
       readbuf(@types[i].isdst,1);
       readbuf(@types[i].idx,1);
       types[i].isstd:=0;
       types[i].isgmt:=0;
     end;

    readbuf(zone_names,tzhead.tzh_charcnt);
    if version=2 then
      begin // read 64bit values
        for i:=0 to num_leaps-1 do
         begin
           readbuf(@leaps[i].transition,sizeof(int64));
           readbuf(@leaps[i].change,sizeof(longint));
           leaps[i].transition:=decode(leaps[i].transition);
           leaps[i].change:=decode(leaps[i].change);
         end;
      end
    else
      begin
        for i:=0 to num_leaps-1 do
         begin
           readbuf(@longval,sizeof(longval));
           leaps[i].transition:=decode(longval);
           readbuf(@longval,sizeof(longval));
           leaps[i].change:=decode(longval);
         end;
      end;

    for i:=0 to tzhead.tzh_ttisstdcnt-1 do
     types[i].isstd:=byte(readbufbyte<>0);

    for i:=0 to tzhead.tzh_ttisgmtcnt-1 do
     types[i].isgmt:=byte(readbufbyte<>0);

    if (version=2) and not bufoverflow then
      readfooter;

    readdata:=true;
  end;
  procedure ClearCurrentTZinfo;
  var
    i:integer;
  begin
    for i:=low(CurrentTZinfo) to high(CurrentTZinfo) do
      CurrentTZinfo[i] := Default(TTZInfo);
  end;
begin
  if fn='' then
   fn:='localtime';
  if fn[1]<>'/' then
    fn:='/usr/share/zoneinfo/'+fn;
  f:=fpopen(fn,Open_RdOnly);
  if f<0 then
   exit(False);
  bufoverflow:=false;
  bufptr := @buf[bufsize-1]+1;
  tzhead:=default(ttzhead);
  LockTZInfo;
  footer_valid:=false;
  footer_has_dst:=false;
  ReadTimezoneFile:=(readheader() and readdata()) and not BufOverflow;
  ClearCurrentTZinfo;
  UnlockTZInfo;
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
  BSDTimeZonefile = DefaultTimeZoneDir;      // BSD usually is POSIX
                                                // compliant though
{$ENDIF}

{$ifndef FPC_HAS_GETTIMEZONEFILE}

function GetTimezoneFile:string;
var
  f,len : longint;
  fn,s : shortstring;
  info : stat;
begin
  GetTimezoneFile:='';
  // Observe TZ variable.
  fn:=fpgetenv('TZ');
  if (fn<>'') then
    if (fn[1]=':') then
      begin
      Delete(fn,1,1);
      if (fn<>'') then
        begin
        if (fn[1]<>'/') then
          Exit(TimeZoneDir+fn);
        Exit(fn);
        end;
      end;
  if (fn='') then
    fn:=TimeZoneLocationFile;
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
  RefreshTZInfo;
end;


procedure DoneLocalTime;
begin
  if assigned(transitions) then
   freemem(transitions);
  transitions:=nil;
  if assigned(type_idxs) then
   freemem(type_idxs);
  type_idxs:=nil;
  if assigned(types) then
   freemem(types);
  types:=nil;
  if assigned(zone_names) then
   freemem(zone_names);
  zone_names:=Nil;
  if assigned(leaps) then
   freemem(leaps);
  leaps:=nil;
  num_transitions:=0;
  num_leaps:=0;
  num_types:=0;
  num_chars:=0;
  footer_valid:=false;
  footer_has_dst:=false;
end;

Procedure ReReadLocalTime;

begin
  LockTZInfo;
  DoneLocalTime;
  InitLocalTime;
  UnlockTZInfo;
end;

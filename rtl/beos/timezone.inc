{
    $Id: timezone.inc,v 1.2 2005/02/14 17:13:21 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by the Free Pascal development team.

    Timezone extraction routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  TZ_MAGIC = 'TZif';

type
  plongint=^longint;
  pbyte=^byte;

  ttzhead=packed record
    tzh_magic : array[0..3] of char;
    tzh_reserved : array[1..16] of byte;
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

function find_transition(timer:time_t):pttinfo;
var
  i : longint;
begin
  if (num_transitions=0) or (timer<time_t(transitions[0])) then
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


procedure GetLocalTimezone(timer:time_t;var leap_correct,leap_hit:longint);
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


procedure ReadTimezoneFile(fn:string);

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

var
  f      : File;
  tzdir  : string;
  tzhead : ttzhead;
  i      : longint;
  chars  : longint;
  buf    : pbyte;
  _result : longint;
  label  lose;
begin
  if fn = '' then
    exit;
{$IFOPT I+}
{$DEFINE IOCHECK_ON}
{$ENDIF}
{$I-}
  Assign(F, fn);
  Reset(F,1);
  If IOResult <> 0 then
   exit;
{$IFDEF IOCHECK_ON}
{$I+}
{$ENDIF}
{$UNDEF IOCHECK_ON}
  BlockRead(f,tzhead,sizeof(tzhead),i);
  if i<>sizeof(tzhead) then
     goto lose;
  if tzhead.tzh_magic<>TZ_MAGIC then
  begin
     goto lose;
  end;
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

  BlockRead(f,transitions^,num_transitions*4,_result);
  if _result <> num_transitions*4 then
  begin
     goto lose;
  end;
  BlockRead(f,type_idxs^,num_transitions,_result);
  if _result <> num_transitions then
  begin
    goto lose;
  end;
  {* Check for bogus indices in the data file, so we can hereafter
     safely use type_idxs[T] as indices into `types' and never crash.  *}
  for i := 0 to num_transitions-1 do
    if (type_idxs[i] >= num_types) then
    begin
      goto lose;
    end;


  for i:=0 to num_transitions-1 do
   decode(transitions[i]);

  for i:=0 to num_types-1 do
   begin
     blockread(f,types[i].offset,4,_result);
     if _result <> 4 then
     begin
      goto lose;
     end;
     blockread(f,types[i].isdst,1,_result);
     if _result <> 1 then
     begin
      goto lose;
     end;
     blockread(f,types[i].idx,1,_result);
     if _result <> 1 then
     begin
      goto lose;
     end;
     decode(types[i].offset);
     types[i].isstd:=0;
     types[i].isgmt:=0;
   end;

  blockread(f,zone_names^,chars,_result);
  if _result<>chars then
     begin
      goto lose;
     end;


  for i:=0 to num_leaps-1 do
   begin
     blockread(f,leaps[i].transition,4);
     if _result <> 4 then
     begin
      goto lose;
     end;
     blockread(f,leaps[i].change,4);
     begin
      goto lose;
     end;
     decode(leaps[i].transition);
     decode(leaps[i].change);
   end;

  getmem(buf,tzhead.tzh_ttisstdcnt);
  blockread(f,buf^,tzhead.tzh_ttisstdcnt,_result);
  if _result<>tzhead.tzh_ttisstdcnt then
     begin
      goto lose;
     end;
  for i:=0 to tzhead.tzh_ttisstdcnt-1 do
   types[i].isstd:=byte(buf[i]<>0);
  freemem(buf);

  getmem(buf,tzhead.tzh_ttisgmtcnt);
  blockread(f,buf^,tzhead.tzh_ttisgmtcnt);
  if _result<>tzhead.tzh_ttisgmtcnt then
     begin
      goto lose;
     end;
  for i:=0 to tzhead.tzh_ttisgmtcnt-1 do
   types[i].isgmt:=byte(buf[i]<>0);
  freemem(buf);
  close(f);
  exit;
lose:
  close(f);
end;


{ help function to extract TZ variable data }
function extractnumberend(tzstr: string; offset : integer): integer;
var
 j: integer;
begin
 j:=0;
 extractnumberend := 0;
 repeat
   if (offset+j) > length(tzstr) then
     begin
       exit;
     end;
  inc(j);
 until not (tzstr[offset+j] in ['0'..'9']);
 extractnumberend := offset+j;
end;

function getoffsetseconds(tzstr: string): longint;
{ extract GMT timezone information }
{ Returns the number of minutes to }
{ add or subtract to the GMT time  }
{ to get the local time.           }
{ Format of TZ variable (POSIX)    }
{  std offset dst                  }
{  std = characters of timezone    }
{  offset = hh[:mm] to add to GMT  }
{  dst = daylight savings time     }
{ CURRENTLY DOES NOT TAKE CARE     }
{ OF SUMMER TIME DIFFERENCIAL      }
var
 s: string;
 i, j: integer;
 code : integer;
 hours : longint;
 minutes : longint;
 negative : boolean;
begin
 hours:=0;
 minutes:=0;
 getoffsetseconds := 0;
 negative := FALSE;
 i:=-1;
 { get to offset field }
 repeat
   if i > length(tzstr) then
     begin
       exit;
     end;
   inc(i);
 until (tzstr[i] = '-') or (tzstr[i] in ['0'..'9']);
 if tzstr[i] = '-' then
  begin
   Inc(i);
   negative := TRUE;
  end;
 j:=extractnumberend(tzstr,i);
 s:=copy(tzstr,i,j-i);
 val(s,hours,code);
 if code <> 0 then
   begin
     exit;
   end;
 if tzstr[j] = ':' then
   begin
     i:=j;
     Inc(i);
     j:=extractnumberend(tzstr,i);
     s:=copy(tzstr,i,j-i);
     val(s,minutes,code);
     if code <> 0 then
      begin
        exit;
      end;
   end;
 if negative then
  begin
    minutes := -minutes;
    hours := -hours;
  end;
 getoffsetseconds := minutes*60 + hours*3600;
end;


procedure InitLocalTime;
var
 tloc: time_t;
 s : string;
begin
  TZSeconds:=0;
  { try to get the POSIX version  }
  { of the local time offset      }
  { if '', then it does not exist }
  { if ': ..', then non-POSIX     }
  s:=GetTimezoneString;
  if (s<>'') and (s[1]<>':') then
   begin
     TZSeconds := getoffsetseconds(s);
   end
  else
   begin
     s:=GetTimeZoneFile;
     { only read if there is something to read }
     if s<>'' then
     begin
       ReadTimezoneFile(s);
       tloc:=sys_time(tloc);
       GetLocalTimezone(tloc);
     end;
   end;
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



{
  $Log: timezone.inc,v $
  Revision 1.2  2005/02/14 17:13:21  peter
    * truncate log

}

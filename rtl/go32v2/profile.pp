{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller,
    member of the Free Pascal development team.

    Profiling support for Go32V2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

{$S- do not use stackcheck here .. PM }
{$ifdef FPC_PROFILE}
{$error }
{$message you can not compile profile unit with profiling}
{$endif FPC_PROFILE}

Unit profile;

interface

type
  header = record
    low,high,nbytes : longint;
  end;

{ entry of a GPROF type file }
  ppMTABE = ^pMTABE;
  pMTABE = ^MTABE;
  MTABE = record
    from,_to,count : longint;
  end;

{ internal form - sizeof(MTAB) is 4096 for efficiency }
  PMTAB = ^M_TAB;
  M_TAB  = record
    calls : array [0..340] of MTABE;
    prev  : PMTAB;
  end;

const
  mcount_skip : longint = 1;
  mtab        : PMTAB = nil;
var
  h           : header;
  histogram   : ^integer;
  histlen     : longint;
  oldexitproc : pointer;

{ called by functions.  Use the pointer it provides to cache the last used
  MTABE, so that repeated calls to/from the same pair works quickly -
  no lookup. }
procedure mcount;

implementation

{$asmmode ATT}

uses
  go32,dpmiexcp;

type
  plongint = ^longint;
const
  cache : pMTABE = nil;

var
  djgpp_old_timer : tseginfo;external name '___djgpp_old_timer';
  start : longint;external name 'start';
  _etext : longint;external name '_etext';
  starttext : longint;
  endtext : longint;

procedure djgpp_timer_hdlr;external name '___djgpp_timer_hdlr';

procedure sbrk_getmem(var p : pointer;size : longint);

  begin
     system.getmem(p,size);
  end;


{ problem how to avoid mcount calling itself !! }
procedure mcount;  [public, alias : 'MCOUNT'];
{
  ebp contains the frame of mcount (ebp) the frame of calling (to_)
  ((ebp)) the frame of from
}
var
   m : pmtab;
   i,to_,ebp,from,mtabi : longint;
begin
   { optimisation !! }
   asm
      pushal
      movl 4(%ebp),%eax
      movl %eax,to_
      movl (%ebp),%eax
      movl 4(%eax),%eax
      movl %eax,from
   end;
   if endtext=0 then
     asm
        popal
        leave
        ret
     end;
   mcount_skip := 1;
   if (to_ > endtext) or (from > endtext) then
     runerror(255);
   if ((cache<>nil) and (cache^.from=from) and (cache^._to=to_)) then
     begin
     { cache paid off - works quickly }
       inc(cache^.count);
       mcount_skip:=0;
       asm
         popal
         leave
         ret
       end;
     end;
{ no cache hit - search all mtab tables for a match, or an empty slot }
  mtabi := -1;
  m:=mtab;
  while m<>nil do
    begin
       for i:=0 to 340 do
         begin
           if m^.calls[i].from=0 then
             begin
              { empty slot - end of table }
                mtabi := i;
                break;
             end;
           if ((m^.calls[i].from = from) and (m^.calls[i]._to = to_)) then
             begin
              { found a match - bump count and return }
                inc(m^.calls[i].count);
                cache:=@(m^.calls[i]);
                mcount_skip:=0;
                asm
                   popal
                   leave
                   ret
                end;
             end;
        end;
      m:=m^.prev;
   end;
  if (mtabi<>-1) then
    begin
     { found an empty - fill it in }
       mtab^.calls[mtabi].from := from;
       mtab^.calls[mtabi]._to := to_;
       mtab^.calls[mtabi].count := 1;
       cache := @(mtab^.calls[mtabi]);
       mcount_skip := 0;
       asm
          popal
          leave
          ret
       end;
    end;
{ lob off another page of memory and initialize the new table }
  { problem here : getmem is not reentrant yet !!  PM }
  { lets hope that a direct call to sbrk correct this }
  sbrk_getmem(m,sizeof(M_TAB));
  fillchar(m^, sizeof(M_TAB),#0);
  m^.prev := mtab;
  mtab := m;
  m^.calls[0].from := from;
  m^.calls[0]._to := to_;
  m^.calls[0].count := 1;
  cache := @(m^.calls[0]);
  mcount_skip := 0;
  asm
     popal
     leave
     ret
  end;
end;


var
  new_timer,
  old_timer       : tseginfo;
  invalid_mcount_call,
  mcount_nb,
  doublecall,
  reload          : longint; {=0}

function mcount_tick(x : longint) : longint;cdecl;
var
  bin : longint;
begin
   if mcount_skip=0 then
     begin
        bin := djgpp_exception_state^.__eip;
        if (djgpp_exception_state^.__cs=get_cs) and (bin >= starttext) and (bin <= endtext) then
          begin
             bin := (bin - starttext) div 16;
             inc(histogram[bin]);
          end
        else
          inc(invalid_mcount_call);
        inc(mcount_nb);
     end
   else
     inc(doublecall);
   mcount_tick:=0;
end;


var
  ___djgpp_timer_countdown:longint;external name '___djgpp_timer_countdown';

function timer(x : longint) : longint;cdecl;
begin
   if reload>0 then
    ___djgpp_timer_countdown:=RELOAD;
   timer:=mcount_tick(x);
   { _raise(SIGPROF); }
end;


procedure mcount_write;
{
  this is called during program exit
}
var
  m : PMTAB;
  i : longint;
  f : file;
begin
  mcount_skip:=1;
  signal(SIGTIMR,@SIG_IGN);
  signal(SIGPROF,@SIG_IGN);
  set_pm_interrupt($8,old_timer);
  reload:=0;
  exitproc:=oldexitproc;
  writeln(stderr,'Writing profile output');
  writeln(stderr,'histogram length = ',histlen);
  writeln(stderr,'Nb of double calls = ',doublecall);
  if invalid_mcount_call>0 then
    writeln(stderr,'nb of invalid mcount : ',invalid_mcount_call,'/',mcount_nb)
  else
    writeln(stderr,'nb of mcount : ',mcount_nb);
  assign(f,'gmon.out');
  rewrite(f,1);
  blockwrite(f, h, sizeof(header));
  blockwrite(f, histogram^, histlen);
  m:=mtab;
  while m<>nil do
    begin
       for i:=0 to 340 do
         begin
            if (m^.calls[i].from = 0) then
              break;
            blockwrite(f, m^.calls[i],sizeof(MTABE));
{$ifdef DEBUG}
            if m^.calls[i].count>0 then
              writeln(stderr,'  0x',hexstr(m^.calls[i]._to,8),' called from ',hexstr(m^.calls[i].from,8),
                ' ',m^.calls[i].count,' times');
{$endif DEBUG}
         end;
       m:=m^.prev;
    end;
  close(f);
end;


procedure mcount_init;
{
  this is called to initialize profiling before the program starts
}

  procedure set_old_timer_handler;
    begin
      djgpp_old_timer:=Old_Timer;
    end;

begin
  starttext:=longint(@start);
  endtext:=longint(@_etext);
  h.low := starttext;
  h.high := endtext;
  histlen := ((h.high-h.low) div 16) * 2; { must be even }
  h.nbytes := sizeof(header) + histlen;
  getmem(histogram,histlen);
  fillchar(histogram^, histlen,#0);

  oldexitproc:=exitproc;
  exitproc:=@mcount_write;

{ here, do whatever it takes to initialize the timer interrupt }
  signal(SIGPROF,@mcount_tick);
  signal(SIGTIMR,@timer);

  get_pm_interrupt($8,old_timer);
  set_old_timer_handler;
{$ifdef DEBUG}
  writeln(stderr,'ori pm int8  '+hexstr(old_timer.segment,4)+':'+hexstr(longint(old_timer.offset),8));
  flush(stderr);
{$endif DEBUG}
  new_timer.segment:=get_cs;
  new_timer.offset:=@djgpp_timer_hdlr;
  reload:=3;
{$ifdef DEBUG}
  writeln(stderr,'new pm int8  '+hexstr(new_timer.segment,4)+':'+hexstr(longint(new_timer.offset),8));
  flush(stderr);
{$endif DEBUG}
  set_pm_interrupt($8,new_timer);
  reload:=1;
  ___djgpp_timer_countdown:=RELOAD;
  mcount_skip := 0;
end;


begin
  mcount_init;
end.

{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit profile;

{$I os.inc}

interface

uses go32,dpmiexcp;

type header = record
             low,high,nbytes : longint;
             end;

{/* entry of a GPROF type file
*/}
type MTABE = record
             from,_to,count : longint;
             end;

     pMTABE = ^MTABE;
     ppMTABE = ^pMTABE;
{/* internal form - sizeof(MTAB) is 4096 for efficiency
*/ }
type
  PMTAB = ^M_TAB;
  M_TAB  = record
          calls : array [0..340] of MTABE;
          prev : PMTAB;
          end;
var
   h : header;
   histogram : ^integer;
const
   mcount_skip : longint = 1;
var
   histlen : longint;
   oldexitproc : pointer;

const
   mtab : PMTAB = nil;

{/* called by functions.  Use the pointer it provides to cache
** the last used MTABE, so that repeated calls to/from the same
** pair works quickly - no lookup.
*/ }
procedure mcount;

implementation

type plongint = ^longint;

var starttext, endtext : longint;

const cache : pMTABE = nil;

  { ebp contains the frame of mcount)
    (ebp) the frame of calling (to_)
    ((ebp)) the frame of from }

    { problem how to avoid mcount calling itself !! }
  procedure mcount;  [public, alias : 'MCOUNT'];
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
       if (to_ > endtext) or (from > endtext) then runerror(255);
       if ((cache<>nil) and
         (cache^.from=from) and
         (cache^._to=to_)) then
         begin
       {/* cache paid off - works quickly */}
       inc(cache^.count);
       mcount_skip:=0;
       asm
          popal
          leave
          ret
       end;
    end;

  {/* no cache hit - search all mtab tables for a match, or an empty slot */}
  mtabi := -1;
  m:=mtab;
  while m<>nil do
    begin
       for i:=0 to 340 do
         begin
           if m^.calls[i].from=0 then
             begin
                {/* empty slot - end of table */ }
                mtabi := i;
                break;
             end;
           if ((m^.calls[i].from = from) and
               (m^.calls[i]._to = to_)) then
             begin
                {/* found a match - bump count and return */}
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
       {/* found an empty - fill it in */}
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
  {/* lob off another page of memory and initialize the new table */}
  getmem(m,sizeof(M_TAB));
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

  var new_timer,old_timer : tseginfo;

{ from itimer.c
/* Copyright (C) 1995 Charles Sandmann (sandmann@clio.rice.edu)
   setitimer implmentation - used for profiling and alarm
   BUGS: ONLY ONE AT A TIME, first pass code
   This software may be freely distributed, no warranty. */ }

{ static void timer_action(int signum) }
{
  if(reload)
    __djgpp_timer_countdown = reload;
  else
    stop_timer();
  raise(sigtype);
}
var reload : longint;
const invalid_mcount_call : longint = 0;
      mcount_nb : longint = 0;
      doublecall : longint = 0;

function mcount_tick(x : longint) : longint;forward;

function timer(x : longint) : longint;
begin
   if reload>0 then
     asm
       movl _RELOAD,%eax
       movl %eax,___djgpp_timer_countdown
     end;

   mcount_tick(x);
   { _raise(SIGPROF); }
end;

{/* this is called during program exit (installed by atexit). */}
procedure mcount_write;
 var m : PMTAB;
     i : longint;
     f : file;
{
  MTAB *m;
  int i, f;
  struct itimerval new_values;

  mcount_skip = 1;

  /* disable timer */
  new_values.it_value.tv_usec = new_values.it_interval.tv_usec = 0;
  new_values.it_value.tv_sec = new_values.it_interval.tv_sec = 0;
  setitimer(ITIMER_PROF, &new_values, NULL); }
  begin
  mcount_skip:=1;
  signal(SIGTIMR,@SIG_IGN);
  signal(SIGPROF,@SIG_IGN);
  set_pm_interrupt($8,old_timer);
  reload:=0;
  exitproc:=oldexitproc;
  writeln('Writing profile output');
  writeln('histogram length = ',histlen);
  writeln('Nb of double calls = ',doublecall);
  if invalid_mcount_call>0 then
    writeln('nb of invalid mcount : ',invalid_mcount_call,'/',mcount_nb)
  else
    writeln('nb of mcount : ',mcount_nb);
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
              writeln('  0x',hexstr(m^.calls[i]._to,8),' called from ',hexstr(m^.calls[i].from,8),
                ' ',m^.calls[i].count,' times');
{$endif DEBUG}
         end;
       m:=m^.prev;
    end;
  close(f);
end;

(* extern unsigned start __asm__ ("start");
#define START (unsigned)&start
extern int etext;

/* ARGSUSED */
static void *)


function mcount_tick(x : longint) : longint;
  var bin : longint;
begin
   if mcount_skip=0 then
     begin
        {bin = __djgpp_exception_state->__eip;}
        bin := djgpp_exception_state^.__eip;
        if (djgpp_exception_state^.__cs=get_cs) and
           (bin >= starttext) and (bin <= endtext) then
          begin
             {bin := (bin - starttext) div 4;}	{/* 4 EIP's per bin */}
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

{/* this is called to initialize profiling before the program starts */}
procedure _mcount_init;

{struct itimerval new_values;}

  function djgpp_timer_hdlr : pointer;
    begin
       asm
          movl $___djgpp_timer_hdlr,%eax
          movl %eax,__RESULT
       end;
    end;

  procedure set_old_timer_handler;
    begin
       asm
          movl $_OLD_TIMER,%eax
          movl $___djgpp_old_timer,%ebx
          movl (%eax),%ecx
          movl %ecx,(%ebx)
          movw 4(%eax),%ax
          movw %ax,4(%ebx)
       end;

    end;
begin

       asm
          movl $_etext,_ENDTEXT
          movl $start,_STARTTEXT
       end;
  h.low := starttext;
  h.high := endtext;
  histlen := ((h.high-h.low) div 16) * 2; { must be even }
  h.nbytes := sizeof(header) + histlen;
  getmem(histogram,histlen);
  fillchar(histogram^, histlen,#0);

  oldexitproc:=exitproc;
  exitproc:=@mcount_write;

  {/* here, do whatever it takes to initialize the timer interrupt */}
  signal(SIGPROF,@mcount_tick);
  signal(SIGTIMR,@timer);

  get_pm_interrupt($8,old_timer);
  set_old_timer_handler;
{$ifdef DEBUG}
       writeln(stderr,'ori pm int8  '+hexstr(old_timer.segment,4)+':'
           +hexstr(longint(old_timer.offset),8));
       flush(stderr);
{$endif DEBUG}
  new_timer.segment:=get_cs;
  new_timer.offset:=djgpp_timer_hdlr;
  reload:=3;
{$ifdef DEBUG}
       writeln(stderr,'new pm int8  '+hexstr(new_timer.segment,4)+':'
           +hexstr(longint(new_timer.offset),8));
       flush(stderr);
{$endif DEBUG}
  set_pm_interrupt($8,new_timer);
  reload:=1;
     asm
       movl _RELOAD,%eax
       movl %eax,___djgpp_timer_countdown
     end;
  mcount_skip := 0;
  end;

begin
_mcount_init;
end.
{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.4  1998/01/26 11:57:39  michael
  + Added log at the end

  Revision 1.3  1998/01/16 16:54:22  pierre
    + logs added at end
    + dxeload and emu387 added in makefile

}

{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.4  1998/01/26 11:57:39  michael
  + Added log at the end


  
  Working file: rtl/dos/go32v2/profile.pp
  description:
  ----------------------------
  revision 1.3
  date: 1998/01/16 16:54:22;  author: pierre;  state: Exp;  lines: +5 -2
    + logs added at end
    + dxeload and emu387 added in makefile
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:26:09;  author: michael;  state: Exp;  lines: +14 -3
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:52;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:52;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}

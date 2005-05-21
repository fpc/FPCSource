{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    TSigContext

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$packrecords C}

const
  __SUNOS_MAXWIN = 31;

type
  twbuf = record
    locals : array[0..7] of longint;
    ins    : array[0..7] of longint;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    sigc_onstack,      { state to restore }
    sigc_mask,         { sigmask to restore }
    sigc_sp,           { stack pointer }
    sigc_pc,           { program counter }
    sigc_npc,          { next program counter }
    sigc_psr,          { for condition codes etc }
    sigc_g1,           { User uses these two registers }
    sigc_o0, { within the trampoline code. }
    { Now comes information regarding the users window set
      * at the time of the signal. }
    sigc_oswins : longint;       { outstanding windows }
    { stack ptrs for each regwin buf }
    sigc_spbuf  : array[0..__SUNOS_MAXWIN-1] of pchar;
    { Windows to restore after signal }
    sigc_wbuf   : array[0..__SUNOS_MAXWIN] of twbuf;
  end;

{
  $Log: sighndh.inc,v $
  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.1  2005/01/30 18:01:15  peter
    * signal cleanup for linux
    * sigactionhandler instead of tsigaction for bsds
    * sigcontext moved to cpu dir

}


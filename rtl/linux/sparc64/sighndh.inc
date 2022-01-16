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
  SIG_G0 = 0;
  SIG_G1 = 1;
  SIG_G2 = 2;
  SIG_G3 = 3;
  SIG_G4 = 4;
  SIG_G5 = 5;
  SIG_G6 = 6;
  SIG_G7 = 7;
  SIG_O0 = 8;
  SIG_O1 = 9;
  SIG_O2 = 10;
  SIG_O3 = 11;
  SIG_O4 = 12;
  SIG_O5 = 13;
  SIG_O6 = 14;
  SIG_O7 = 15;
  { l and i registers do not seem to be 
    written in sigcontext struct 
  SIG_L0 = 16;
  SIG_L1 = 17;
  SIG_L2 = 18;
  SIG_L3 = 19;
  SIG_L4 = 20;
  SIG_L5 = 21;
  SIG_L6 = 22;
  SIG_L7 = 23;
  SIG_I0 = 24;
  SIG_I1 = 25;
  SIG_I2 = 26;
  SIG_I3 = 27;
  SIG_I4 = 28;
  SIG_I5 = 29;
  SIG_I6 = 30;
  SIG_I7 = 31; }

  SIG_SP = SIG_O6;
  { SIG_FP = SIG_I6; }


type
  twbuf = record
    locals : array[0..7] of longint;
    ins    : array[0..7] of longint;
  end;

  PSigContext = ^TSigContext;
  TSigContext = record
    // sigc_onstack,      { state to restore }
    // sigc_mask,         { sigmask to restore }
    // sigc_sp,           { stack pointer }
    sigc_psr,          { for condition codes etc }
    sigc_pc,           { program counter }
    sigc_npc,          { next program counter }
    sigc_y : clong;
    sigc_gregs : array [0..31] of clong;
  end;



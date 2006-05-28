{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2002 by the Free Pascal development team

    BIOS functions unit for Gameboy Advance
    Copyright (c) 2006 by Francesco Lombardi

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *****************************************************************************}

{*****************************************************************************
                              GBA Bios Functions
*****************************************************************************}

(*
  GBA Bios Functions
  ------------------
  Following infos come from GBATEK, Gameboy Advance Technical Info, that you can
	find here: http://nocash.emubase.de/gbatek.htm
  
  GBA Bios includes some useful optimized functions which can be accessed by
  SWI. Parameters can be passed to function by r0,r1,r2 and r3 registers; results
  are stored in r0,r1 and r3. Unused 'out registers' generally return garbage; 
  other registers are unchanged.
  
  In ARM mode SWI register are called by:
  
    SWI n * 0x010000
  
  In THUMB mode:
  
    SWI n



  SWI     Hex       Function
  ---     ---       --------
  0       00h       SoftReset
  1       01h       RegisterRamReset
  2       02h       Halt
  3       03h       Stop
  4       04h       IntrWait
  5       05h       VBlankIntrWait
  6       06h       Div
  7       07h       DivArm
  8       08h       Sqrt
  9       09h       ArcTan
  10      0Ah       ArcTan2
  11      0Bh       CpuSet
  12      0Ch       CpuFastSet
  13      0Dh       -Undoc- ("GetBiosChecksum")
  14      0Eh       BgAffineSet
  15      0Fh       ObjAffineSet
  16      10h       BitUnPack
  17      11h       LZ77UnCompWram
  18      12h       LZ77UnCompVram
  19      13h       HuffUnComp
  20      14h       RLUnCompWram
  21      15h       RLUnCompVram
  22      16h       Diff8bitUnFilterWram
  23      17h       Diff8bitUnFilterVram
  24      18h       Diff16bitUnFilter
  25      19h       SoundBias
  26      1Ah       SoundDriverInit
  27      1Bh       SoundDriverMode
  28      1Ch       SoundDriverMain
  29      1Dh       SoundDriverVSync
  30      1Eh       SoundChannelClear
  31      1Fh       MidiKey2Freq
  32-36   20h-24h   -Undoc- (Sound Related ???)
  37      25h       MultiBoot
  38      26h       -Undoc- ("HardReset")
  39      27h       -Undoc- ("CustomHalt")
  40      28h       SoundDriverVSyncOff
  41      29h       SoundDriverVSyncOn
  42      2Ah       -Undoc- ("GetJumpList" for Sound ???)
  43-255  2Bh-FFh   -Not used-
  
  Values passed to SWI aren't range-checked, so calling 43-255 will lock-up GBA.
*)


(* 
  Following defines are intended for future use, when fpc hopefully will handle 
  both arm and thumb code. I have provided ARM and THUMB funcs, that can be 
	activated by defines:
	{$define __THUMB__}
	{$define __ARM__}
	At this time I'll force ARM definition in "system.pp"
*)


(* Generic system call !!Does Not Work!!
{$ifdef __THUMB__}
  procedure SystemCall(Number: integer); assembler; inline;
  asm
    SWI r0
  end;
{$else}
  procedure SystemCall(n: integer); assembler; inline;
  asm
    MOV    R0, R0, LSL #0x10
    SWI    R0
  end;
{$endif}
*)

{$ifdef __THUMB__}
    (*=========================
      SWI6 Div
        Signed Division, r0/r1.
        r0 signed 32bit Number
        r1 signed 32bit Denom
      Return:
        r0 Number DIV Denom
      =========================*) 
    function fpc_div_longint(n,z: longint):longint; [public, alias: 'FPC_DIV_LONGINT']; compilerproc; assembler; inline;
    asm
      swi 6
    end;
    (*=====================*)


    (*=========================
      SWI6 DivMod
        Signed Division, r0/r1.
        r0 signed 32bit Number
        r1 signed 32bit Denom
      Return:
        r1 Number MOD Denom
      =========================*)    
    function fpc_mod_longint(n,z: longint):longint; [public, alias: 'FPC_MOD_LONGINT']; compilerproc; assembler; inline;
    asm
      swi 6
      mov r0, r1
    end; 
    (*=========================*)

{$endif}

{$ifdef __ARM__}

    (*=========================
      SWI7 DivArm
        Signed Division, r1/r0.
        r1 signed 32bit Number
        r0 signed 32bit Denom
      Return:
        r0 Number DIV Denom
      =========================*) 
    function fpc_div_longint(n,z: longint):longint; [public, alias: 'FPC_DIV_LONGINT']; compilerproc; assembler; inline;
    asm
      swi #0x070000
    end;
    (*=========================*)

    (*=========================
      SWI7 DivModArm
        Signed Division, r1/r0.
        r1 signed 32bit Number
        r0 signed 32bit Denom
      Return:
        r1 Number MOD Denom
      =========================*) 
    function fpc_mod_longint(n,z: longint):longint; [public, alias: 'FPC_MOD_LONGINT']; compilerproc; assembler; inline;
    asm
      swi #0x070000
      mov r0, r1
    end; 
    (*=========================*)

{$endif}


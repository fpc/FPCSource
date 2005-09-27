{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe and other members of the
    Free Pascal development team

    Implementation of mathematical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


const
  longint_to_real_helper: int64 = $4330000080000000;
  cardinal_to_real_helper: int64 = $4330000000000000;
  int_to_real_factor: double = double(high(cardinal))+1.0;


{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

    {$define FPC_SYSTEM_HAS_PI}
    function fpc_pi_real : valreal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;

    {$define FPC_SYSTEM_HAS_ABS}
    function fpc_abs_real(d : valreal) : valreal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;

    {$define FPC_SYSTEM_HAS_SQR}
    function fpc_sqr_real(d : valreal) : valreal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;

      const
        factor: double = double(int64(1) shl 32);
        factor2: double = double(int64(1) shl 31);
(*
{$ifndef FPC_SYSTEM_HAS_TRUNC}
    {$define FPC_SYSTEM_HAS_TRUNC}
    function fpc_trunc_real(d : valreal) : int64;assembler;compilerproc;
      { input: d in fr1      }
      { output: result in r3 }
      assembler;
      var
        temp: packed record
            case byte of
              0: (l1,l2: longint);
              1: (d: double);
          end;
      asm
        // store d in temp
        stfd    f1,temp
        // extract sign bit (record in cr0)
        lwz     r3,temp
        rlwinm. r3,r3,1,31,31
        // make d positive
        fabs    f1,f1
        // load 2^32 in f2
        {$ifndef macos}
        lis    r4,factor@ha
        lfd    f2,factor@l(r4)
        {$else}
        lwz    r4,factor(r2)
        lfd    f2,0(r4)
        {$endif}
        // check if value is < 0
        // f3 := d / 2^32;
        fdiv     f3,f1,f2
        // round
        fctiwz   f4,f3
        // store
        stfd     f4,temp
        // and load into r4
        lwz      r3,temp+4
        // convert back to float
        lis      r0,0x4330
        stw      r0,temp
        xoris    r0,r3,0x8000
        stw      r0,temp+4
        {$ifndef macos}
        lis    r4,longint_to_real_helper@ha
        lfd    f0,longint_to_real_helper@l(r4)
        {$else}
        lwz    r4,longint_to_real_helper(r2)
        lfd    f0,0(r4)
        {$endif}
        lfd    f3,temp
        fsub   f3,f3,f0


        // f4 := d "mod" 2^32 ( = d - ((d / 2^32) * 2^32))
        fnmsub   f4,f3,f2,f1

        // now, convert to unsigned 32 bit

        // load 2^31 in f2
        {$ifndef macos}
        lis    r4,factor2@ha
        lfd    f2,factor2@l(r4)
        {$else}
        lwz    r4,factor2(r2)
        lfd    f2,0(r4)
        {$endif}

        // subtract 2^31
        fsub   f3,f4,f2
        // was the value > 2^31?
        fcmpu  cr1,f4,f2
        // use diff if >= 2^31
        fsel   f4,f3,f3,f4

        // next part same as conversion to signed integer word
        fctiwz f4,f4
        stfd   f4,temp
        lwz    r4,temp+4
        // add 2^31 if value was >=2^31
        blt    cr1, .LTruncNoAdd
        xoris  r4,r4,0x8000
.LTruncNoAdd:
        // negate value if it was negative to start with
        beq    cr0,.LTruncPositive
        subfic r4,r4,0
        subfze r3,r3
.LTruncPositive:
      end;
{$endif not FPC_SYSTEM_HAS_TRUNC}
*)

(*
{$ifndef FPC_SYSTEM_HAS_ROUND}
    {$define FPC_SYSTEM_HAS_ROUND}
    function round(d : extended) : int64;

    function fpc_round(d : extended) : int64;assembler;[public, alias:'FPC_ROUND'];compilerproc;
      { exactly the same as trunc, except that one fctiwz has become fctiw }
      { input: d in fr1      }
      { output: result in r3 }
      assembler;
      var
        temp: packed record
            case byte of
              0: (l1,l2: longint);
              1: (d: double);
          end;
      asm
        // store d in temp
        stfd    f1, temp
        // extract sign bit (record in cr0)
        lwz     r4,temp
        rlwinm. r4,r4,1,31,31
        // make d positive
        fabs    f1,f1
        // load 2^32 in f2
        {$ifndef macos}
        lis    r4,factor@ha
        lfd    f2,factor@l(r4)
        {$else}
        lwz    r4,factor(r2)
        lfd    f2,0(r4)
        {$endif}
        // check if value is < 0
        // f3 := d / 2^32;
        fdiv     f3,f1,f2
        // round
        fctiwz   f4,f3
        // store
        stfd     f4,temp
        // and load into r4
        lwz      r3,temp+4
        // convert back to float
        lis      r0,0x4330
        stw      r0,temp
        xoris    r0,r3,0x8000
        stw      r0,temp+4
        {$ifndef macos}
        lis    r4,longint_to_real_helper@ha
        lfd    f0,longint_to_real_helper@l(r4)
        {$else}
        lwz    r4,longint_to_real_helper(r2)
        lfd    f0,0(r4)
        {$endif}
        lfd    f3,temp
        fsub   f3,f3,f0


        // f4 := d "mod" 2^32 ( = d - ((d / 2^32) * 2^32))
        fnmsub   f4,f3,f2,f1

        // now, convert to unsigned 32 bit

        // load 2^31 in f2
        {$ifndef macos}
        lis    r4,factor2@ha
        lfd    f2,factor2@l(r4)
        {$else}
        lwz    r4,factor2(r2)
        lfd    f2,0(r4)
        {$endif}

        // subtract 2^31
        fsub   f3,f4,f2
        // was the value > 2^31?
        fcmpu  cr1,f4,f2
        // use diff if >= 2^31
        fsel   f4,f3,f3,f4

        // next part same as conversion to signed integer word
        fctiw  f4,f4
        stfd   f4,temp
        lwz    r4,temp+4
        // add 2^31 if value was >=2^31
        blt    cr1, .LRoundNoAdd
        xoris  r4,r4,0x8000
.LRoundNoAdd:
        // negate value if it was negative to start with
        beq    cr0,.LRoundPositive
        subfic r4,r4,0
        subfze r3,r3
.LRoundPositive:
      end;
{$endif not FPC_SYSTEM_HAS_ROUND}
*)


{****************************************************************************
                         Int to real helpers
 ****************************************************************************}

{$define FPC_SYSTEM_HAS_INT64_TO_DOUBLE}
function fpc_int64_to_double(i: int64): double; compilerproc;
assembler;
{ input: i in r3 }
{ output: double(i) in f0            }
{from "PowerPC Microprocessor Family: Programming Environments Manual for 64 and 32-Bit Microprocessors", v2.0, pg. 698 }
var temp : int64;
asm
        std r3,temp // store dword
        lfd f0,temp // load float
        fcfid f0,f0     // convert to fpu int
end;


{$define FPC_SYSTEM_HAS_QWORD_TO_DOUBLE}
function fpc_qword_to_double(q: qword): double; compilerproc;
assembler;
const
  longint_to_real_helper: qword = $80000000;
{from "PowerPC Microprocessor Family: Programming Environments Manual for
 64 and 32-Bit Microprocessors", v2.0, pg. 698, *exact version*              }
{ input: q in r3 }
{ output: double(q) in f0            }
var
  temp1, temp2: qword;
asm
    // load 2^32 into f4
        lis   r4, longint_to_real_helper@highesta
        ori   r4, r4, longint_to_real_helper@highera
        sldi  r4, r4, 32
        oris  r4, r4, longint_to_real_helper@ha
        lfd   f4, longint_to_real_helper@l(r4)

        rldicl r4,r3,32,32  // isolate high half
        rldicl r0,r3,0,32   // isolate low half
        std r4,temp1        // store dword both
        std r0,temp2
        lfd f2,temp1        // load float both
        lfd f0,temp2        // load float both
        fcfid f2,f2         // convert each half to
        fcfid f0,f0         // fpu int (no rnd)
        fmadd f0,f4,f2,f0   // (2**32)*high+low (only add can rnd)
end;


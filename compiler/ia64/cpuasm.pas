{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Contains the assembler object for the ia64

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit cpuasm;

{$i defines.inc}

interface

uses
  cobjects,
  aasm,globals,verbose,
  cpubase;


type
  pairegalloc = ^tairegalloc;
  tairegalloc = object(tai)
     allocation : boolean;
     reg        : tregister;
     constructor alloc(r : tregister);
     constructor dealloc(r : tregister);
  end;

  { Types of operand }
  toptype=(top_none,top_reg,top_ref,top_const,top_symbol);

  toper=record
    case typ : toptype of
       top_none   : ();
       top_reg    : (reg:tregister);
       top_qp     : (qp : tqp);
       top_ref    : (ref:preference);
       top_const  : (val:int64);
       top_symbol : (sym:pasmsymbol;symofs:longint);
  end;

  paicpu = ^taicpu;
  taicpu = object(tai)
     is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
     opcode    : tasmop;
     ops       : longint;
     ops       : array[0..4] of longint;
     oper
     qp        : tqp;
     { ALU instructions }
     { A1,A9: integer ALU }
     constructor op_reg_reg_reg(op : tasmop;const r1,r2,r3 : tregister);
     { A2,A10: shift left and add }
     constructor op_reg_reg_const_reg(qp : tqp;op : tasmop;
       const r1,r2 : tregister;i : byte;const r3 : tregister);
     { A3,A4,A5: integer ALU - imm.,register }
     constructor op_reg_const_reg(qp : tqp;op : tasmop;
       const r1 : tregister;i : longint;const r3 : tregister);
     { A6,A7: integer compare - register,register }
     constructor op_preg_preg_reg_reg(qp : tqp;op : tasmop;
       cond : tasmcond;p1,p2 : tqp;const r2,r3 : tregister)
     { A8: integer compare - imm.,register }
     constructor op_preg_preg_const_reg(qp : tqp;op : tasmop;
       cond : tasmcond;p1,p2 : tqp;i : longint;const r3 : tregister);
{!!!!!!!
     { multimedia shift and multiply }
     constructor op_reg_reg_reg_const(qp : tqp;
     { multimedia mux }
     constructor op_reg_reg_mbtype(qp : tqp;
     { multimedia shift fixed }
     constructor op_reg_reg_const(qp : tqp;
     { div. }
     constructor op_reg_reg(qp : tqp;
     { mm extract }
     constructor op_reg_reg_const_const(qp : tqp;
     { zero and deposit imm }
     constructor op_reg_const_const_const(qp : tqp;
     { deposit imm }
     constructor op_reg_const_reg_const_const(qp : tqp;
     { deposit }
     constructor op_reg_reg_reg_const_const(qp : tqp;
     { test bit }
     { !!!! here we need also to take care of the postfix }
     constructor op_preg_preg_reg_const(qp : tqp;
     { test NaT }
     { !!!! here we need also to take care of the postfix }
     constructor op_preg_preg_reg(qp : tqp;
     { break/nop }
     constructor op_const(qp : tqp;
     { speculation check }
     constructor op_reg_const(qp : tqp;

     { -------- here are some missed ----------- }
}

     { M1: integer load }
     { M4: integer store }
     { M6: floating-point load }
     { M9: floating-point store }
     constructor op_reg_ref(qp : tqp;op : tasmop;postfix : tldsttype;
       hint : thint;const r1 : tregister;ref : preference);

     { M2: integer load incremented by register }
     { M7: floating-point load incremented by register }
     constructor op_reg_ref_reg(qp : tqp;op : tasmop;postfix : tldsttype;
       hint : thint;const r1 : tregister;const ref : treference;
       const r2 : tregister);

     { M3: integer load increment by imm. }
     { M5: integer store increment by imm. }
     { M8: floating-point load increment by imm. }
     { M10: floating-point store increment by imm. }
     constructor op_reg_ref_const(qp : tqp;op : tasmop;postfix : tldsttype;
       hint : thint;const r1 : tregister;ref : preference;i : longint);

     { M11: floating-point load pair}
     constructor op_reg_ref(qp : tqp;op : tasmop;postfix : tldsttype;
       hint : thint;const r1,r2 : tregister;ref : preference);

     { M12: floating-point load pair increment by imm. }
     constructor op_reg_ref(qp : tqp;op : tasmop;postfix : tldsttype;
       hint : thint;const r1,r2 : tregister;ref : preference;i : longint);
  end;

  { the following objects are special for the ia64 }
  { they decribe a stop and the bundles            }
  paistop = ^taistop;
  taistop = object(tai)
    constructor init;
  end;

  { a second underscro indicates a stop }
  tbundletemplate = (but_none,but_mii,but_mii_,
    but_mi_i,but_mi_i_,but_mlx,but_mlx_,
    but_mmi,but_mmi_,but_m_mi,but_m_mi_,
    but_mfi,but_mfi_,but_mmf,but_mmf_,
    but_mif,but_mib_,but_mbb,but_mbb_,
    but_bbb,but_bbb_,but_mmb,but_mmb_,
    but_mfb,but_mfb_);

  paibundle = ^taibundle;
  taibundle = object(tai)
     template : tbundletemplate;
     instructions : array[0..1] of paicpu;
  end;

implementation


{*****************************************************************************
                                 TaiStop
*****************************************************************************}

    constructor taistop.init;

      begin
         inherited init;
         typ:=ait_stop;
      end;


{*****************************************************************************
                                 TaiRegAlloc
*****************************************************************************}

    constructor tairegalloc.alloc(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=true;
        reg:=r;
      end;


    constructor tairegalloc.dealloc(r : tregister);
      begin
        inherited init;
        typ:=ait_regalloc;
        allocation:=false;
        reg:=r;
      end;


end.
{
  $Log$
  Revision 1.1  2000-12-31 16:54:19  florian
    + initial revision

}

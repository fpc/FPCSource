{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the x86 specific class for the register
    allocator

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

unit rgx86;

{$i fpcdefs.inc}

  interface

    uses
      cclasses,globtype,
      cpubase,cpuinfo,cgbase,cgutils,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      rgobj;

    type
       trgx86 = class(trgobj)
         function  get_spill_subreg(r : tregister) : tsubregister;override;
         function  do_spill_replace(list:TAsmList;instr:taicpu;orgreg:tsuperregister;const spilltemp:treference):boolean;override;
       end;

       tpushedsavedloc = record
         case byte of
           0: (pushed: boolean);
           1: (ofs: longint);
       end;

       tpushedsavedfpu = array[tsuperregister] of tpushedsavedloc;

       trgx86fpu = class
          { The "usableregsxxx" contain all registers of type "xxx" that }
          { aren't currently allocated to a regvar. The "unusedregsxxx"  }
          { contain all registers of type "xxx" that aren't currently    }
          { allocated                                                    }
          unusedregsfpu,usableregsfpu : Tsuperregisterset;
          { these counters contain the number of elements in the }
          { unusedregsxxx/usableregsxxx sets                     }
          countunusedregsfpu : byte;

          { Contains the registers which are really used by the proc itself.
            It doesn't take care of registers used by called procedures
          }
          used_in_proc : tcpuregisterset;

          {reg_pushes_other : regvarother_longintarray;
          is_reg_var_other : regvarother_booleanarray;
          regvar_loaded_other : regvarother_booleanarray;}

          { tries to hold the amount of times which the current tree is processed  }
          t_times: longint;

          fpuvaroffset : byte;

          constructor create;

          function getregisterfpu(list: TAsmList) : tregister;
          procedure ungetregisterfpu(list: TAsmList; r : tregister);

          { pushes and restores registers }
          procedure saveusedfpuregisters(list:TAsmList;
                                         var saved:Tpushedsavedfpu;
                                         const s:Tcpuregisterset);
          procedure restoreusedfpuregisters(list:TAsmList;
                                            const saved:Tpushedsavedfpu);

          { corrects the fpu stack register by ofs }
          function correct_fpuregister(r : tregister;ofs : byte) : tregister;
       end;


implementation

    uses
       systems,
       verbose;

    const
       { This value is used in tsaved. If the array value is equal
         to this, then this means that this register is not used.}
       reg_not_saved = $7fffffff;


{******************************************************************************
                                    Trgcpu
******************************************************************************}

    function trgx86.get_spill_subreg(r : tregister) : tsubregister;
      begin
        result:=getsubreg(r);
      end;


    function trgx86.do_spill_replace(list:TAsmList;instr:taicpu;orgreg:tsuperregister;const spilltemp:treference):boolean;
      var
        replaceoper : longint;
      begin
        result:=false;
        with instr do
          begin
            replaceoper:=-1;
            case ops of
              1 :
                begin
                  if (oper[0]^.typ=top_reg) then
                    begin
                      if get_alias(getsupreg(oper[0]^.reg))<>orgreg then
                        internalerror(200410101);
                      replaceoper:=0;
                    end;
                end;
              2,3 :
                begin
                  { We can handle opcodes with 2 and 3 operands the same way. The opcodes
                    with 3 registers are shrd/shld, where the 3rd operand is const or CL,
                    that doesn't need spilling }
                  if (oper[0]^.typ=top_reg) and
                     (oper[1]^.typ=top_reg) and
                     (getsupreg(oper[0]^.reg)<>getsupreg(oper[1]^.reg)) then
                    begin
                      { One of the arguments shall be able to be replaced }
                      if (getregtype(oper[0]^.reg)=regtype) and
                         (get_alias(getsupreg(oper[0]^.reg))=orgreg) then
                        replaceoper:=0
                      else
                        if (getregtype(oper[1]^.reg)=regtype) and
                           (get_alias(getsupreg(oper[1]^.reg))=orgreg) then
                          replaceoper:=1
                      else
                        internalerror(200410106);
                      case replaceoper of
                        0 :
                          begin
                            { Some instructions don't allow memory references
                              for source }
                            case instr.opcode of
                              A_BT,
                              A_BTS,
                              A_BTC,
                              A_BTR :
                                replaceoper:=-1;
                            end;
                          end;
                        1 :
                          begin
                            { Some instructions don't allow memory references
                              for destination }
                            case instr.opcode of
                              A_MOVZX,
                              A_MOVSX,
                              A_MULSS,
                              A_MULSD,
                              A_SUBSS,
                              A_SUBSD,
                              A_ADDSD,
                              A_ADDSS,
                              A_DIVSD,
                              A_DIVSS,
                              A_SHLD,
                              A_SHRD,
                              A_CVTDQ2PD,
                              A_CVTDQ2PS,
                              A_CVTPD2DQ,
                              A_CVTPD2PI,
                              A_CVTPD2PS,
                              A_CVTPI2PD,
                              A_CVTPS2DQ,
                              A_CVTPS2PD,
                              A_CVTSD2SI,
                              A_CVTSD2SS,
                              A_CVTSI2SD,
                              A_CVTSS2SD,
                              A_CVTTPD2PI,
                              A_CVTTPD2DQ,
                              A_CVTTPS2DQ,
                              A_CVTTSD2SI,
                              A_CVTPI2PS,
                              A_CVTPS2PI,
                              A_CVTSI2SS,
                              A_CVTSS2SI,
                              A_CVTTPS2PI,
                              A_CVTTSS2SI,
                              A_IMUL,
                              A_XORPD,
                              A_XORPS,
                              A_ORPD,
                              A_ORPS,
                              A_ANDPD,
                              A_ANDPS:
                                replaceoper:=-1;
                            end;
                          end;
                      end;
                    end;
                end;
            end;

            { Replace register with spill reference }
            if replaceoper<>-1 then
              begin
                oper[replaceoper]^.typ:=top_ref;
                new(oper[replaceoper]^.ref);
                oper[replaceoper]^.ref^:=spilltemp;
                { memory locations aren't guaranteed to be aligned }
                case opcode of
                  A_MOVAPS:
                    opcode:=A_MOVSS;
                  A_MOVAPD:
                    opcode:=A_MOVSD;
                end;
                result:=true;
              end;
          end;
      end;


{******************************************************************************
                                  Trgx86fpu
******************************************************************************}

    constructor Trgx86fpu.create;
      begin
        used_in_proc:=[];
        t_times := 0;
        unusedregsfpu:=usableregsfpu;
      end;


    function trgx86fpu.getregisterfpu(list: TAsmList) : tregister;
      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result:=NR_ST;
      end;


    procedure trgx86fpu.ungetregisterfpu(list : TAsmList; r : tregister);
      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;



    function trgx86fpu.correct_fpuregister(r : tregister;ofs : byte) : tregister;
      begin
        correct_fpuregister:=r;
        setsupreg(correct_fpuregister,ofs);
      end;


    procedure trgx86fpu.saveusedfpuregisters(list: TAsmList;
                                             var saved : tpushedsavedfpu;
                                             const s: tcpuregisterset);
      var
         r : tregister;
         hr : treference;
      begin
        used_in_proc:=used_in_proc+s;

{$warning TODO firstsavefpureg}
(*
        { don't try to save the fpu registers if not desired (e.g. for }
        { the 80x86)                                                   }
        if firstsavefpureg <> R_NO then
          for r.enum:=firstsavefpureg to lastsavefpureg do
            begin
              saved[r.enum].ofs:=reg_not_saved;
              { if the register is used by the calling subroutine and if }
              { it's not a regvar (those are handled separately)         }
              if not is_reg_var_other[r.enum] and
                 (r.enum in s) and
                 { and is present in use }
                 not(r.enum in unusedregsfpu) then
                begin
                  { then save it }
                  tg.GetTemp(list,extended_size,tt_persistent,hr);
                  saved[r.enum].ofs:=hr.offset;
                  cg.a_loadfpu_reg_ref(list,OS_FLOAT,r,hr);
                  cg.a_reg_dealloc(list,r);
                  include(unusedregsfpu,r.enum);
                  inc(countunusedregsfpu);
                end;
            end;
*)
      end;


    procedure trgx86fpu.restoreusedfpuregisters(list : TAsmList;
                                                const saved : tpushedsavedfpu);

      var
         r,r2 : tregister;
         hr : treference;

      begin
{$warning TODO firstsavefpureg}
(*
        if firstsavefpureg <> R_NO then
          for r.enum:=lastsavefpureg downto firstsavefpureg do
            begin
              if saved[r.enum].ofs <> reg_not_saved then
                begin
                  r2.enum:=R_INTREGISTER;
                  r2.number:=NR_FRAME_POINTER_REG;
                  reference_reset_base(hr,r2,saved[r.enum].ofs);
                  cg.a_reg_alloc(list,r);
                  cg.a_loadfpu_ref_reg(list,OS_FLOAT,hr,r);
                  if not (r.enum in unusedregsfpu) then
                    { internalerror(10)
                      in n386cal we always save/restore the reg *state*
                      using save/restoreunusedstate -> the current state
                      may not be real (JM) }
                  else
                    begin
                      dec(countunusedregsfpu);
                      exclude(unusedregsfpu,r.enum);
                    end;
                  tg.UnGetTemp(list,hr);
                end;
            end;
*)
      end;

(*
    procedure Trgx86fpu.saveotherregvars(list: TAsmList; const s: totherregisterset);
      var
        r: Tregister;
      begin
        if not(cs_opt_regvar in current_settings.optimizerswitches) then
          exit;
        if firstsavefpureg <> NR_NO then
          for r.enum := firstsavefpureg to lastsavefpureg do
            if is_reg_var_other[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
      end;
*)

end.

{
    $Id$
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
      cpubase,
      cpuinfo,
      aasmbase,aasmtai,aasmcpu,
      cclasses,globtype,cgbase,rgobj;

    type
       trgx86 = class(trgobj)
{$ifdef OLDRGX86}
         function instr_spill_register(list:Taasmoutput;
                                       instr:taicpu;
                                       const r:Tsuperregisterset;
                                       const spilltemplist:Tspill_temp_list): boolean;override;
{$endif OLDRGX86}
        function  get_spill_subreg(r : tregister) : tsubregister;override;
{
        procedure do_spill_read(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
                                const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
        procedure do_spill_written(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
                                   const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
        procedure do_spill_readwritten(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
                                       const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);override;
}
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

          function getregisterfpu(list: taasmoutput) : tregister;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister);

          { pushes and restores registers }
          procedure saveusedfpuregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedfpu;
                                         const s:Tcpuregisterset);
          procedure restoreusedfpuregisters(list:Taasmoutput;
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

{$ifdef OLDRGX86}
    function trgx86.instr_spill_register(list:Taasmoutput;
                                         instr:taicpu;
                                         const r:Tsuperregisterset;
                                         const spilltemplist:Tspill_temp_list): boolean;
    {
      Spill the registers in r in this instruction. Returns true if any help
      registers are used. This procedure has become one big hack party, because
      of the huge amount of situations you can have. The irregularity of the i386
      instruction set doesn't help either. (DM)
    }
    var i:byte;
        supreg:Tsuperregister;
        subreg:Tsubregister;
        helpreg:Tregister;
        helpins:Taicpu;
        op:Tasmop;
        hopsize:Topsize;
        pos:Tai;

    begin
      {Situation examples are in intel notation, so operand order:
       mov    eax       ,    ebx
              ^^^            ^^^
              oper[1]        oper[0]
      (DM)}
      result:=false;
      with taicpu(instr) do
       begin
         case ops of
           0:
             ;
           1:
             begin
               if (oper[0]^.typ=top_reg) and
                  (getregtype(oper[0]^.reg)=regtype) then
                 begin
                   supreg:=getsupreg(oper[0]^.reg);
                   if supregset_in(r,supreg) then
                     begin
                       {Situation example:
                        push r20d              ; r20d must be spilled into [ebp-12]

                       Change into:
                        push [ebp-12]          ; Replace register by reference }
   {                    hopsize:=reg2opsize(oper[0].reg);}
                       oper[0]^.typ:=top_ref;
                       new(oper[0]^.ref);
                       oper[0]^.ref^:=spilltemplist[supreg];
   {                    oper[0]^.ref^.size:=hopsize;}
                     end;
                 end;
               if oper[0]^.typ=top_ref then
                 begin
                   supreg:=getsupreg(oper[0]^.ref^.base);
                   if supregset_in(r,supreg) then
                     begin
                       {Situation example:
                        push [r21d+4*r22d]        ; r21d must be spilled into [ebp-12]

                        Change into:

                        mov r23d,[ebp-12]         ; Use a help register
                        push [r23d+4*r22d]        ; Replace register by helpregister }
                       subreg:=getsubreg(oper[0]^.ref^.base);
                       if oper[0]^.ref^.index=NR_NO then
                         pos:=Tai(previous)
                       else
                         pos:=get_insert_pos(Tai(previous),getsupreg(oper[0]^.ref^.index),RS_INVALID,RS_INVALID);
                       getregisterinline(list,pos,subreg,helpreg);
                       result:=true;
                       helpins:=Taicpu.op_ref_reg(A_MOV,reg2opsize(oper[0]^.ref^.base),spilltemplist[supreg],helpreg);
                       if pos=nil then
                         list.insertafter(helpins,list.first)
                       else
                         list.insertafter(helpins,pos.next);
                       ungetregisterinline(list,helpins,helpreg);
                       forward_allocation(Tai(helpins.next),instr);
                       oper[0]^.ref^.base:=helpreg;
                     end;
                   supreg:=getsupreg(oper[0]^.ref^.index);
                   if supregset_in(r,supreg) then
                     begin
                       {Situation example:
                        push [r21d+4*r22d]        ; r22d must be spilled into [ebp-12]

                        Change into:

                        mov r23d,[ebp-12]         ; Use a help register
                        push [r21d+4*r23d]        ; Replace register by helpregister }
                       subreg:=getsubreg(oper[0]^.ref^.index);
                       if oper[0]^.ref^.base=NR_NO then
                         pos:=Tai(instr.previous)
                       else
                         pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[0]^.ref^.base),RS_INVALID,RS_INVALID);
                       getregisterinline(list,pos,subreg,helpreg);
                       result:=true;
                       helpins:=Taicpu.op_ref_reg(A_MOV,reg2opsize(oper[0]^.ref^.index),spilltemplist[supreg],helpreg);
                       if pos=nil then
                         list.insertafter(helpins,list.first)
                       else
                         list.insertafter(helpins,pos.next);
                       ungetregisterinline(list,helpins,helpreg);
                       forward_allocation(Tai(helpins.next),instr);
                       oper[0]^.ref^.index:=helpreg;
                     end;
                   end;
             end;
           2,
           3 :
             begin
               { Opcodes with 3 registers are shrd/shld, where the 3rd operand is const or CL,
                 that doesn't need spilling }

               { First spill the registers from the references. This is
                 required because the reference can be moved from this instruction
                 to a MOV instruction when spilling of the register operand is done }
               for i:=0 to 1 do
                 if oper[i]^.typ=top_ref then
                   begin
                     supreg:=getsupreg(oper[i]^.ref^.base);
                     if supregset_in(r,supreg) then
                       begin
                         {Situation example:
                          add r20d,[r21d+4*r22d]    ; r21d must be spilled into [ebp-12]

                          Change into:

                          mov r23d,[ebp-12]         ; Use a help register
                          add r20d,[r23d+4*r22d]    ; Replace register by helpregister }
                         subreg:=getsubreg(oper[i]^.ref^.base);
                         if i=1 then
                           pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[i]^.ref^.index),getsupreg(oper[0]^.reg),RS_INVALID)
                         else
                           pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[i]^.ref^.index),RS_INVALID,RS_INVALID);
                         getregisterinline(list,pos,subreg,helpreg);
                         result:=true;
                         helpins:=Taicpu.op_ref_reg(A_MOV,reg2opsize(oper[i]^.ref^.base),spilltemplist[supreg],helpreg);
                         if pos=nil then
                           list.insertafter(helpins,list.first)
                         else
                           list.insertafter(helpins,pos.next);
                         oper[i]^.ref^.base:=helpreg;
                         ungetregisterinline(list,helpins,helpreg);
                         forward_allocation(Tai(helpins.next),instr);
                     end;
                     supreg:=getsupreg(oper[i]^.ref^.index);
                     if supregset_in(r,supreg) then
                       begin
                         {Situation example:
                          add r20d,[r21d+4*r22d]    ; r22d must be spilled into [ebp-12]

                          Change into:

                          mov r23d,[ebp-12]         ; Use a help register
                          add r20d,[r21d+4*r23d]    ; Replace register by helpregister }
                         subreg:=getsubreg(oper[i]^.ref^.index);
                         if i=1 then
                           pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[i]^.ref^.base),
                                               getsupreg(oper[0]^.reg),RS_INVALID)
                         else
                           pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[i]^.ref^.base),RS_INVALID,RS_INVALID);
                         getregisterinline(list,pos,subreg,helpreg);
                         result:=true;
                         helpins:=Taicpu.op_ref_reg(A_MOV,reg2opsize(oper[i]^.ref^.index),spilltemplist[supreg],helpreg);
                         if pos=nil then
                           list.insertafter(helpins,list.first)
                         else
                           list.insertafter(helpins,pos.next);
                         oper[i]^.ref^.index:=helpreg;
                         ungetregisterinline(list,helpins,helpreg);
                         forward_allocation(Tai(helpins.next),instr);
                       end;
                   end;
               if (oper[0]^.typ=top_reg) and
                  (getregtype(oper[0]^.reg)=regtype) then
                 begin
                   supreg:=getsupreg(oper[0]^.reg);
                   subreg:=getsubreg(oper[0]^.reg);
                   if supregset_in(r,supreg) then
                     if oper[1]^.typ=top_ref then
                       begin
                         {Situation example:
                          add [r20d],r21d      ; r21d must be spilled into [ebp-12]

                          Change into:

                          mov r22d,[ebp-12]    ; Use a help register
                          add [r20d],r22d      ; Replace register by helpregister }
                         pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[0]^.reg),
                                             getsupreg(oper[1]^.ref^.base),getsupreg(oper[1]^.ref^.index));
                         getregisterinline(list,pos,subreg,helpreg);
                         result:=true;
                         helpins:=Taicpu.op_ref_reg(A_MOV,reg2opsize(oper[0]^.reg),spilltemplist[supreg],helpreg);
                         if pos=nil then
                           list.insertafter(helpins,list.first)
                         else
                           list.insertafter(helpins,pos.next);
                         oper[0]^.reg:=helpreg;
                         ungetregisterinline(list,helpins,helpreg);
                         forward_allocation(Tai(helpins.next),instr);
                       end
                     else
                       begin
                         {Situation example:
                          add r20d,r21d        ; r21d must be spilled into [ebp-12]

                          Change into:

                          add r20d,[ebp-12]    ; Replace register by reference }
                         oper[0]^.typ:=top_ref;
                         new(oper[0]^.ref);
                         oper[0]^.ref^:=spilltemplist[supreg];
                       end;
                 end;
               if (oper[1]^.typ=top_reg) and
                  (getregtype(oper[1]^.reg)=regtype) then
                 begin
                   supreg:=getsupreg(oper[1]^.reg);
                   subreg:=getsubreg(oper[1]^.reg);
                   if supregset_in(r,supreg) then
                     begin
                       if oper[0]^.typ=top_ref then
                         begin
                           {Situation example:
                            add r20d,[r21d]      ; r20d must be spilled into [ebp-12]

                            Change into:

                            mov r22d,[r21d]      ; Use a help register
                            add [ebp-12],r22d    ; Replace register by helpregister }
                           pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[0]^.ref^.base),
                                               getsupreg(oper[0]^.ref^.index),RS_INVALID);
                           getregisterinline(list,pos,subreg,helpreg);
                           result:=true;
                           op:=A_MOV;
                           hopsize:=opsize;  {Save old value...}
                           if (opcode=A_MOVZX) or (opcode=A_MOVSX) or (opcode=A_LEA) then
                             begin
                               {Because 'movzx memory,register' does not exist...}
                               op:=opcode;
                               opcode:=A_MOV;
                               opsize:=reg2opsize(oper[1]^.reg);
                             end;
                           helpins:=Taicpu.op_ref_reg(op,hopsize,oper[0]^.ref^,helpreg);
                           if pos=nil then
                             list.insertafter(helpins,list.first)
                           else
                             list.insertafter(helpins,pos.next);
                           dispose(oper[0]^.ref);
                           oper[0]^.typ:=top_reg;
                           oper[0]^.reg:=helpreg;
                           oper[1]^.typ:=top_ref;
                           new(oper[1]^.ref);
                           oper[1]^.ref^:=spilltemplist[supreg];
                           ungetregisterinline(list,helpins,helpreg);
                           forward_allocation(Tai(helpins.next),instr);
                         end
                       else
                         begin
                           {Situation example:
                            add r20d,r21d        ; r20d must be spilled into [ebp-12]

                            Change into:

                            add [ebp-12],r21d    ; Replace register by reference }
                           if (opcode=A_MOVZX) or (opcode=A_MOVSX) then
                             begin
                               {Because 'movzx memory,register' does not exist...}
                               result:=true;
                               op:=opcode;
                               hopsize:=opsize;
                               opcode:=A_MOV;
                               opsize:=reg2opsize(oper[1]^.reg);
                               pos:=get_insert_pos(Tai(instr.previous),getsupreg(oper[0]^.reg),RS_INVALID,RS_INVALID);
                               getregisterinline(list,pos,subreg,helpreg);
                               helpins:=Taicpu.op_reg_reg(op,hopsize,oper[0]^.reg,helpreg);
                               if pos=nil then
                                 list.insertafter(helpins,list.first)
                               else
                                 list.insertafter(helpins,pos.next);
                               oper[0]^.reg:=helpreg;
                               ungetregisterinline(list,helpins,helpreg);
                               forward_allocation(Tai(helpins.next),instr);
                             end;
                           oper[1]^.typ:=top_ref;
                           new(oper[1]^.ref);
                           oper[1]^.ref^:=spilltemplist[supreg];
                         end;
                     end;
                 end;

               { The i386 instruction set never gets boring...
                 some opcodes do not support a memory location as destination }
               if (oper[1]^.typ=top_ref) and
                  (
                   (oper[0]^.typ=top_const) or
                   ((oper[0]^.typ=top_reg) and
                    (getregtype(oper[0]^.reg)=regtype))
                  ) then
                 begin
                   case opcode of
                     A_SHLD,A_SHRD,
                     A_IMUL :
                       begin
                         {Yikes! We just changed the destination register into
                          a memory location above here.

                          Situation examples:

                          imul [ebp-12],r21d    ; We need a help register
                          imul [ebp-12],<const> ; We need a help register

                          Change into:

                          mov r22d,[ebp-12]    ; Use a help instruction (only for IMUL)
                          imul r22d,r21d       ; Replace reference by helpregister
                          mov [ebp-12],r22d    ; Use another help instruction}
                         getregisterinline(list,Tai(previous),subreg,helpreg);
                         result:=true;
                         {First help instruction.}
                         helpins:=Taicpu.op_ref_reg(A_MOV,opsize,oper[1]^.ref^,helpreg);
                         if previous=nil then
                           list.insert(helpins)
                         else
                           list.insertafter(helpins,previous);
                         {Second help instruction.}
                         helpins:=Taicpu.op_reg_ref(A_MOV,opsize,helpreg,oper[1]^.ref^);
                         dispose(oper[1]^.ref);
                         oper[1]^.typ:=top_reg;
                         oper[1]^.reg:=helpreg;
                         list.insertafter(helpins,instr);
                         ungetregisterinline(list,instr,helpreg);
                       end;
                   end;
                 end;

               { The i386 instruction set never gets boring...
                 some opcodes do not support a memory location as source }
               if (oper[0]^.typ=top_ref) and
                  (oper[1]^.typ=top_reg) and
                  (getregtype(oper[1]^.reg)=regtype) then
                 begin
                   case opcode of
                     A_BT,A_BTS,
                     A_BTC,A_BTR :
                       begin
                         {Yikes! We just changed the source register into
                          a memory location above here.

                          Situation example:

                          bt  r21d,[ebp-12]   ; We need a help register

                          Change into:

                          mov r22d,[ebp-12]    ; Use a help instruction (only for IMUL)
                          bt  r21d,r22d        ; Replace reference by helpregister}
                         getregisterinline(list,Tai(previous),subreg,helpreg);
                         result:=true;
                         {First help instruction.}
                         helpins:=Taicpu.op_ref_reg(A_MOV,opsize,oper[0]^.ref^,helpreg);
                         if previous=nil then
                           list.insert(helpins)
                         else
                           list.insertafter(helpins,previous);
                         dispose(oper[0]^.ref);
                         oper[0]^.typ:=top_reg;
                         oper[0]^.reg:=helpreg;
                         ungetregisterinline(list,helpins,helpreg);
                       end;
                   end;
                 end;
             end;
           else
             internalerror(200409202);
         end;
       end;
    end;
{$endif OLDRGX86}


    function trgx86.get_spill_subreg(r : tregister) : tsubregister;
      begin
        result:=getsubreg(r);
      end;


      (*
    procedure trgx86.do_spill_read(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
                                   const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins: tai;
        tmpref,ref : treference;
        helplist : taasmoutput;
        tmpreg : tregister;
      begin
{        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
          end
        else }
          inherited do_spill_read(list,instr,pos,regidx,spilltemplist,regs);
      end;


    procedure trgx86.do_spill_written(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
                                      const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins: tai;
        ref,tmpref : treference;
        helplist : taasmoutput;
        tmpreg : tregister;
      begin
{        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
          end
        else }
          inherited do_spill_written(list,instr,pos,regidx,spilltemplist,regs);
      end;


    procedure trgx86.do_spill_readwritten(list : taasmoutput;instr : taicpu;pos: tai; regidx: word;
                                          const spilltemplist:Tspill_temp_list;const regs : tspillregsinfo);
      var
        helpins1, helpins2: tai;
        tmpref,ref : treference;
        helplist : taasmoutput;
         tmpreg : tregister;
      begin
{        ref:=spilltemplist[regs[regidx].orgreg];
        if abs(ref.offset)>4095 then
          begin
          end
        else  }
          inherited do_spill_readwritten(list,instr,pos,regidx,spilltemplist,regs);
      end;
*)

{******************************************************************************
                                  Trgx86fpu
******************************************************************************}

    constructor Trgx86fpu.create;

      var i:Tsuperregister;

      begin
        used_in_proc:=[];
        t_times := 0;
        unusedregsfpu:=usableregsfpu;
      end;


    function trgx86fpu.getregisterfpu(list: taasmoutput) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result:=NR_ST;
      end;


    procedure trgx86fpu.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;



    function trgx86fpu.correct_fpuregister(r : tregister;ofs : byte) : tregister;

      begin
        correct_fpuregister:=r;
        setsupreg(correct_fpuregister,ofs);
      end;


    procedure trgx86fpu.saveusedfpuregisters(list: taasmoutput;
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


    procedure trgx86fpu.restoreusedfpuregisters(list : taasmoutput;
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
    procedure Trgx86fpu.saveotherregvars(list: taasmoutput; const s: totherregisterset);
      var
        r: Tregister;
      begin
        if not(cs_regvars in aktglobalswitches) then
          exit;
        if firstsavefpureg <> NR_NO then
          for r.enum := firstsavefpureg to lastsavefpureg do
            if is_reg_var_other[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
      end;
*)

end.
{
  $Log$
  Revision 1.7  2004-10-04 20:46:22  peter
    * spilling code rewritten for x86. It now used the generic
      spilling routines. Special x86 optimization still needs
      to be added.
    * Spilling fixed when both operands needed to be spilled
    * Cleanup of spilling routine, do_spill_readwritten removed

  Revision 1.6  2004/09/27 14:49:45  peter
    * handle 3 operand opcodes the same as 2 operand opcodes, the
      third operand can only be a const or register CL, so it doesn't
      affect spilling
    * support shrd/shld that don't allow memory operands

  Revision 1.5  2004/09/26 07:15:07  florian
    * ie checking in spilling code improved

  Revision 1.4  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.3  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.2.2.1  2004/04/10 12:36:42  peter
    * fixed alignment issues

  Revision 1.2  2004/01/12 16:37:59  peter
    * moved spilling code from taicpu to rg

}

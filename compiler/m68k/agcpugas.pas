{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an asmoutput class for m68k GAS syntax

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
{ This unit implements an asmoutput class for i386 AT&T syntax
}
unit agcpugas;

{$i fpcdefs.inc}

interface

    uses
      cclasses,cpubase,
      globals,
      aasmbase,aasmtai,aasmcpu,assemble,aggas;

    type
      TM68kAssembler=class(TGNUassembler)
      public
        procedure WriteInstruction(hp: tai);override;
      end;

    const
      gas_op2str:op2strtable=
       { 68000 only instructions }
       ('abcd','add', 'adda','addi','addq','addx','and','andi',
       'asl','asr','bcc','bcs','beq','bge','bgt','bhi',
       'ble','bls','blt','bmi','bne','bpl','bvc','bvs',
       'bchg','bclr','bra','bset','bsr','btst','chk',
       'clr','cmp','cmpa','cmpi','cmpm','dbcc','dbcs','dbeq','dbge',
       'dbgt','dbhi','dble','dbls','dblt','dbmi','dbne','dbra',
       'dbpl','dbt','dbvc','dbvs','dbf','divs','divu',
       'eor','eori','exg','illegal','ext','jmp','jsr',
       'lea','link','lsl','lsr','move','movea','movei','moveq',
       'movem','movep','muls','mulu','nbcd','neg','negx',
       'nop','not','or','ori','pea','rol','ror','roxl',
       'roxr','rtr','rts','sbcd','scc','scs','seq','sge',
       'sgt','shi','sle','sls','slt','smi','sne',
       'spl','st','svc','svs','sf','sub','suba','subi','subq',
       'subx','swap','tas','trap','trapv','tst','unlk',
       'rte','reset','stop',
       { MC68010 instructions }
       'bkpt','movec','moves','rtd',
       { MC68020 instructions }
       'bfchg','bfclr','bfexts','bfextu','bfffo',
       'bfins','bfset','bftst','callm','cas','cas2',
       'chk2','cmp2','divsl','divul','extb','pack','rtm',
       'trapcc','tracs','trapeq','trapf','trapge','trapgt',
       'traphi','traple','trapls','traplt','trapmi','trapne',
       'trappl','trapt','trapvc','trapvs','unpk',
       { FPU Processor instructions - directly supported only. }
       { IEEE aware and misc. condition codes not supported   }
       'fabs','fadd',
       'fbeq','fbne','fbngt','fbgt','fbge','fbnge',
       'fblt','fbnlt','fble','fbgl','fbngl','fbgle','fbngle',
       'fdbeq','fdbne','fdbgt','fdbngt','fdbge','fdnbge',
       'fdblt','fdbnlt','fdble','fdbgl','fdbngl','fdbgle','fbdngle',
       'fseq','fsne','fsgt','fsngt','fsge','fsnge',
       'fslt','fsnlt','fsle','fsgl','fsngl','fsgle','fsngle',
       'fcmp','fdiv','fmove','fmovem',
       'fmul','fneg','fnop','fsqrt','fsub','fsgldiv',
       'fsflmul','ftst',
       'fint','fintrz',
       'ftrapeq','ftrapne','ftrapgt','ftrapngt','ftrapge','ftrapnge',
       'ftraplt','ftrapnlt','ftraple','ftrapgl','ftrapngl','ftrapgle',
       'ftrapngle',
       { Useful for assembly langage output }
       { Protected instructions }
       'cprestore','cpsave',
       { FPU Unit protected instructions                    }
       { and 68030/68851 common MMU instructions            }
       { (this may include 68040 MMU instructions)          }
       'frestore','fsave','pflush','pflusha','pload','pmove','ptest',
       { Useful for assembly langage output }
       '','','','');

     gas_opsize2str : array[topsize] of string[2] =
     ('','.b','.w','.l','.s','.d','.x',''
     );

     gas_reg2str : reg2strtable =
      ('', '%d0','%d1','%d2','%d3','%d4','%d5','%d6','%d7',
       '%a0','%a1','%a2','%a3','%a4','%a5','%a6','%sp',
       '-(%sp)','(%sp)+',
       '%ccr','%fp0','%fp1','%fp2','%fp3','%fp4','%fp5',
       '%fp6','%fp7','%fpcr','%sr','%ssp','%dfc',
       '%sfc','%vbr','%fpsr');


  implementation

    uses
      cutils,systems,
      verbose;



    function getreferencestring(var ref : treference) : string;
      var
         s,basestr,indexstr : string;

      begin
         s:='';
         with ref do
           begin
             inc(offset,offsetfixup);
             offsetfixup:=0;
             basestr:=gas_reg2str[base];
             indexstr:=gas_reg2str[index];
             if assigned(symbol) then
               s:=s+symbol.name;

             if offset<0 then s:=s+tostr(offset)
              else if (offset>0) then
                begin
                  if (symbol=nil) then s:=tostr(offset)
                       else s:=s+'+'+tostr(offset);
                    end
                  else if (index=R_NO) and (base=R_NO) and not assigned(symbol) then
                    s:=s+'0';

               if (index<>R_NO) and (base=R_NO) and (direction=dir_none) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                    s:=s+'(,'+indexstr+'.l)'
                  else
                    s:=s+'(,'+indexstr+'.l*'+tostr(scalefactor)+')'
                end
                else if (index=R_NO) and (base<>R_NO) and (direction=dir_inc) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                      s:=s+'('+basestr+')+'
                  else
                   InternalError(10002);
                end
                else if (index=R_NO) and (base<>R_NO) and (direction=dir_dec) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                      s:=s+'-('+basestr+')'
                  else
                   InternalError(10003);
                end
                  else if (index=R_NO) and (base<>R_NO) and (direction=dir_none) then
                begin
                  s:=s+'('+basestr+')'
                end
                  else if (index<>R_NO) and (base<>R_NO) and (direction=dir_none) then
                begin
                  if (scalefactor = 1) or (scalefactor = 0) then
                    s:=s+'('+basestr+','+indexstr+'.l)'
                  else
                    s:=s+'('+basestr+','+indexstr+'.l*'+tostr(scalefactor)+')';
                end;
          end;
         getreferencestring:=s;
      end;


    function getopstr(const o:toper) : string;
    var
      hs : string;
      i : tregister;
    begin
      case o.typ of
            top_reg : getopstr:=gas_reg2str[o.reg];
            top_ref : getopstr:=getreferencestring(o.ref^);
        top_reglist : begin
                      hs:='';
                      for i:=R_NO to R_FPSR do
                      begin
                        if i in o.registerlist then
                         hs:=hs+gas_reg2str[i]+'/';
                      end;
                      delete(hs,length(hs),1);
                      getopstr := hs;
                    end;
             top_const : getopstr:='#'+tostr(o.val);
            top_symbol :
                    { compare with i386, where a symbol is considered }
                    { a constant.                                     }
                    begin
                     if assigned(o.sym) then
                       hs:='#'+o.sym.name
                     else
                       hs:='#';
                       if o.symofs>0 then
                        hs:=hs+'+'+tostr(o.symofs)
                       else
                        if o.symofs<0 then
                         hs:=hs+tostr(o.symofs)
                       else
                        if not(assigned(o.sym)) then
                          hs:=hs+'0';
                       getopstr:=hs;
                    end;
            else internalerror(10001);
         end;
      end;

    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
            top_reg : getopstr_jmp:=gas_reg2str[o.reg];
            top_ref : getopstr_jmp:=getreferencestring(o.ref^);
            top_const : getopstr_jmp:=tostr(o.val);
            top_symbol : begin
                           if assigned(o.sym) then
                             hs:=o.sym.name
                           else
                             hs:='';
                             if o.symofs>0 then
                              hs:=hs+'+'+tostr(o.symofs)
                             else
                              if o.symofs<0 then
                               hs:=hs+tostr(o.symofs)
                             else
                              if not(assigned(o.sym)) then
                                hs:=hs+'0';
                           getopstr_jmp:=hs;
                         end;
            else internalerror(10001);
         end;
      end;

{****************************************************************************
                            TM68kASMOUTPUT
 ****************************************************************************}

(*
   ait_instruction : begin
                       { old versions of GAS don't like PEA.L and LEA.L }
                       if (paicpu(hp)^.opcode in [
                            A_LEA,A_PEA,A_ABCD,A_BCHG,A_BCLR,A_BSET,A_BTST,
                            A_EXG,A_NBCD,A_SBCD,A_SWAP,A_TAS,A_SCC,A_SCS,
                            A_SEQ,A_SGE,A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,
                            A_SNE,A_SPL,A_ST,A_SVC,A_SVS,A_SF]) then
                        s:=#9+mot_op2str[paicpu(hp)^.opcode]
                       else
                        s:=#9+mot_op2str[paicpu(hp)^.opcode]+mit_opsize2str[paicpu(hp)^.opsize];
                       if paicpu(hp)^.ops>0 then
                        begin
                        { call and jmp need an extra handling                          }
                        { this code is only callded if jmp isn't a labeled instruction }
                          if paicpu(hp)^.opcode in [A_BSR,A_BRA,A_LEA,A_PEA,A_JSR,A_JMP] then
                           s:=s+#9#9+getopstr_jmp(paicpu(hp)^.oper[0])
                          else
                            s:=s+#9+getopstr(paicpu(hp)^.oper[0]);
                           if paicpu(hp)^.ops>1 then
                            begin
                              s:=s+','+getopstr(paicpu(hp)^.oper[1]);
                            { three operands }
                              if paicpu(hp)^.ops>2 then
                               begin
                                   if (paicpu(hp)^.opcode = A_DIVSL) or
                                      (paicpu(hp)^.opcode = A_DIVUL) or
                                      (paicpu(hp)^.opcode = A_MULU) or
                                      (paicpu(hp)^.opcode = A_MULS) or
                                      (paicpu(hp)^.opcode = A_DIVS) or
                                      (paicpu(hp)^.opcode = A_DIVU) then
                                    s:=s+':'+getopstr(paicpu(hp)^.oper[2])
                                   else
                                    s:=s+','+getopstr(paicpu(hp)^.oper[2]);
                               end;
                            end;
                        end;
                       AsmWriteLn(s);
                     end;


ait_labeled_instruction : begin
                     { labeled operand }
                       if pai_labeled(hp)^.register = R_NO then
                         begin
                           if pai_labeled(hp)^.lab <> nil then
                             AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+pai_labeled(hp)^.lab^.name)
                           else
                             AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+pai_labeled(hp)^.sym^.name);
                         end
                       else
                     { labeled operand with register }
                        begin
                           if pai_labeled(hp)^.lab <> nil then
                             begin
                                  AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+
                                    gas_reg2str[pai_labeled(hp)^.register]+','+pai_labeled(hp)^.lab^.name);
                               end
                           else
                           { a symbol is the value }
                             begin
                                  AsmWriteLn(#9+mot_op2str[pai_labeled(hp)^.opcode]+#9+
                                    gas_reg2str[pai_labeled(hp)^.register]+','+pai_labeled(hp)^.sym^.name);
                             end;
                        end;
                     end;
*)

    { returns the opcode string }
    function getopcodestring(hp : tai) : string;
      var
        op : tasmop;
        s : string;
      begin
        op:=taicpu(hp).opcode;
        { old versions of GAS don't like PEA.L and LEA.L }
        if (op in [
         A_LEA,A_PEA,A_ABCD,A_BCHG,A_BCLR,A_BSET,A_BTST,
         A_EXG,A_NBCD,A_SBCD,A_SWAP,A_TAS,A_SCC,A_SCS,
         A_SEQ,A_SGE,A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,
         A_SNE,A_SPL,A_ST,A_SVC,A_SVS,A_SF]) then
         s:=gas_op2str[op]
        else
        if op = A_SXX then
         s:=gas_op2str[op]+cond2str[taicpu(hp).condition]
        else
        if op in [a_dbxx,a_bxx,a_fbxx] then
         s:=gas_op2str[op]+cond2str[taicpu(hp).condition]+gas_opsize2str[taicpu(hp).opsize]
        else
         s:=gas_op2str[op]+gas_opsize2str[taicpu(hp).opsize];
        getopcodestring:=s;
      end;

    procedure TM68kAssembler. WriteInstruction(hp: tai);
    var
      op       : tasmop;
      s        : string;
      sep      : char;
      calljmp  : boolean;
      i        : integer;
     begin
       if hp.typ <> ait_instruction then exit;
       op:=taicpu(hp).opcode;
       calljmp:=is_calljmp(op);
       { call maybe not translated to call }
       s:=#9+getopcodestring(hp);
       { process operands }
       if taicpu(hp).ops<>0 then
         begin
           { call and jmp need an extra handling                          }
           { this code is only called if jmp isn't a labeled instruction  }
           { quick hack to overcome a problem with manglednames=255 chars }
           if calljmp then
              begin
                AsmWrite(s+#9);
                s:=getopstr_jmp(taicpu(hp).oper[0]);
              end
            else
              begin
                for i:=0 to taicpu(hp).ops-1 do
                  begin
                    if i=0 then
                      sep:=#9
                    else
                    if ((op = A_DIVSL) or
                       (op = A_DIVUL) or
                       (op = A_MULU) or
                       (op = A_MULS) or
                       (op = A_DIVS) or
                       (op = A_DIVU)) and (i=1) then
                      sep:=':'
                    else
                      sep:=',';
                    s:=s+sep+getopstr(taicpu(hp).oper[i])
                  end;
              end;
         end;
         AsmWriteLn(s);
     end;


{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_m68k_as_info : tasminfo =
          (
            id     : as_gas;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_any;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr','COMMON')
          );

initialization
  RegisterAssembler(as_m68k_as_info,TM68kAssembler);
end.
{
  $Log$
  Revision 1.3  2002-09-07 15:25:11  peter
    * old logs removed and tabs fixed

  Revision 1.2  2002/08/13 18:58:54  carl
    + m68k problems with cvs fixed?()!

}

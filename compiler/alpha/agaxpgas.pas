{
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an asm for the DEC Alpha

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
unit agaxpgas;

  {$i fpcdefs.inc}

  interface

    uses
       globals,systems,aasmbase,aasmtai,aasmdata,
       aggas,cpubase;

    type
      TAXPGNUAssembler=class(TGNUAssembler)
        procedure WriteInstruction(hp : tai);override;
      end;

    const
       gas_reg2str : array[tregister] of string[4] = (
         '',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '','',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '',''
       );

  implementation

    const
       op2str : array[tasmop] of string[14] = (
          'addf','addg','addl','addq',
          'adds','addt','amask','and','beq','bge',
          'bgt','bic','bis','blbc','blbs','ble',
          'blt','bne','br','bsr','call_pal','cmoveq',
          'cmovge','cmovgt','cmovlbc','cmovlbs','cmovle','cmovlt',
          'cmovne','cmpbge','cmpeq','cmpgeq','cmpgle','cmpglt',
          'cmple','cmplt','cmpteq','cmptle','cmptlt','cmptun',
          'cmpule','cmpult','cpys','cpyse','cpysn','ctlz',
          'ctpop','cttz','cvtdg','cvtgd','cvtgf','cvtgq',
          'cvtlq','cvtqf','cvtqg','cvtql','cvtqs','cvtqt',
          'cvtst','cvttq','cvtts','divf','divg','divs',
          'divt','ecb','eqv','excb','extbl','extlh',
          'extll','extqh','extql','extwh','extwl','fbeq',
          'fbge','fbgt','fble','fblt','fbne','fcmoveq',
          'fcmovge','fcmovgt','fcmovle','fcmovlt','fcmovne','fetch',
          'fetch_m','ftois','ftoit','implver','insbl','inslh',
          'insll','insqh','insql','inswh','inswl','itoff',
          'itofs','itoft','jmp','jsr','jsr_coroutine','lda',
          'ldah','ldbu','ldwu','ldf','ldg','ldl',
          'ldl_l','ldq','ldq_l','ldq_u','lds','ldt',
          'maxsb8','maxsw4','maxub8','maxuw4','mb','mf_fpcr',
          'minsb8','minsw4','minub8','minuw4','mskbl','msklh',
          'mskll','mskqh','mskql','mskwh','mskwl','mt_fpcr',
          'mulf','mulg','mull','mulq',
          'muls','mult','ornot','perr','pklb','pkwb',
          'rc','ret','rpcc','rs','s4addl','s4addq',
          's4subl','s4subq','s8addl','s8addq','s8subl','s8subq',
          'sextb','sextw','sll','sqrtf','sqrtg','sqrts',
          'sqrtt','sra','srl','stb','stf','stg',
          'sts','stl','stl_c','stq','stq_c','stq_u',
          'stt','stw','subf','subg','subl',
          'subq','subs','subt','trapb','umulh','unpkbl',
          'unpkbw','wh64','wmb','xor','zap','zapnot',
          'ldgp');

      procedure TAXPGNUAssembler.WriteInstruction (hp : tai);
        begin
(*
               op:=paicpu(hp)^.opcode;
               calljmp:=is_calljmp(op);
             { call maybe not translated to calll }
               s:=#9+att_op2str[op]+cond2str[paicpu(hp)^.condition];
               if (not calljmp) and
                  (not att_nosuffix[op]) and
                  not(
                   (paicpu(hp)^.oper[0].typ=top_reg) and
                   (paicpu(hp)^.oper[0].reg in [R_ST..R_ST7])
                  ) then
                s:=s+att_opsize2str[paicpu(hp)^.opsize];
             { process operands }
               if paicpu(hp)^.ops<>0 then
                begin
                { call and jmp need an extra handling                          }
                { this code is only called if jmp isn't a labeled instruction }
                  if calljmp then
                   s:=s+#9+getopstr_jmp(paicpu(hp)^.oper[0])
                  else
                   begin
                     for i:=0to paicpu(hp)^.ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(paicpu(hp)^.oper[i])
                      end;
                   end;
                end;
               AsmWriteLn(s);
*)
             end;

end.

{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an asmoutput class for i386 AT&T syntax

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
{ This unit implements an asmoutput class for x86-64 AT&T syntax
}
unit agx64att;

{$i fpcdefs.inc}

interface

    uses
      cclasses,cpubase,
      globals,
      aasmbase,aasmtai,aasmcpu,assemble,aggas;

    type
      Tx86_64ATTAssembler=class(TGNUassembler)
      public
        procedure WriteInstruction(hp: tai);override;
      end;

    TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint);

    const
      att_op2str:op2strtable={$i x64att.inc}
      att_needsuffix:array[tasmop] of TAttSuffix={$i x64atts.inc}

      att_reg2str : reg2strtable = ('',
         '%rax','%rcx','%rdx','%rbx','%rsp','%rbp','%rsi','%rdi',
         '%r8','%r9','%r10','%r11','%r12','%r13','%r14','%r15','%rip',
         '%eax','%ecx','%edx','%ebx','%esp','%ebp','%esi','%edi',
         '%r8d','%r9d','%r10d','%r11d','%r12d','%r13d','%r14d','%r15d',
         '%ax','%cx','%dx','%bx','%sp','%bp','%si','%di',
         '%r8w','%r9w','%r10w','%r11w','%r12w','%r13w','%r14w','%r15w',
         '%al','%cl','%dl','%bl','%spl','%bpl','%sil','%dil',
         '%r8b','%r9b','%r10b','%r11b','%r12b','%r13b','%r14b','%r15b',
         '%ah','%ch','%bh','%dh',
         '%cs','%ds','%es','%ss','%fs','%gs',
         '%st','%st(0)','%st(1)','%st(2)','%st(3)','%st(4)','%st(5)','%st(6)','%st(7)',
         '%dr0','%dr1','%dr2','%dr3','%dr6','%dr7',
         '%cr0','%cr2','%cr3','%cr4',
         '%tr3','%tr4','%tr5','%tr6','%tr7',
         '%mm0','%mm1','%mm2','%mm3','%mm4','%mm5','%mm6','%mm7',
         '%xmm0','%xmm1','%xmm2','%xmm3','%xmm4','%xmm5','%xmm6','%xmm7',
         '%xmm8','%xmm9','%xmm10','%xmm11','%xmm12','%xmm13','%xmm14','%xmm15'
       );

     att_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','bw','bl','wl','bq','wq','lq',
       's','l','q',
       's','l','t','d','q','v',
       '','',''
     );

  implementation

    uses
      cutils,systems,
      verbose;



    function getreferencestring(var ref : treference) : string;
    var
      s : string;
    begin
      with ref do
       begin
         inc(offset,offsetfixup);
         offsetfixup:=0;
       { have we a segment prefix ? }
       { These are probably not correctly handled under GAS }
       { should be replaced by coding the segment override  }
       { directly! - DJGPP FAQ                              }
         if segment<>R_NO then
          s:=att_reg2str[segment]+':'
         else
          s:='';
         if assigned(symbol) then
          s:=s+symbol.name;
         if offset<0 then
          s:=s+tostr(offset)
         else
          if (offset>0) then
           begin
             if assigned(symbol) then
              s:=s+'+'+tostr(offset)
             else
              s:=s+tostr(offset);
           end
         else if (index=R_NO) and (base=R_NO) and not assigned(symbol) then
           s:=s+'0';
         if (index<>R_NO) and (base=R_NO) then
          begin
            s:=s+'(,'+att_reg2str[index];
            if scalefactor<>0 then
             s:=s+','+tostr(scalefactor)+')'
            else
             s:=s+')';
          end
         else
          if (index=R_NO) and (base<>R_NO) then
           s:=s+'('+att_reg2str[base]+')'
          else
           if (index<>R_NO) and (base<>R_NO) then
            begin
              s:=s+'('+att_reg2str[base]+','+att_reg2str[index];
              if scalefactor<>0 then
               s:=s+','+tostr(scalefactor)+')'
              else
               s := s+')';
            end;
       end;
      getreferencestring:=s;
    end;

    function getopstr(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr:=att_reg2str[o.reg];
        top_ref :
          getopstr:=getreferencestring(o.ref^);
        top_const :
          getopstr:='$'+tostr(longint(o.val));
        top_symbol :
          begin
            if assigned(o.sym) then
              hs:='$'+o.sym.name
            else
              hs:='$';
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
        else
          internalerror(10001);
      end;
    end;

    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:='*'+att_reg2str[o.reg];
        top_ref :
          getopstr_jmp:='*'+getreferencestring(o.ref^);
        top_const :
          getopstr_jmp:=tostr(longint(o.val));
        top_symbol :
          begin
            hs:=o.sym.name;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
            getopstr_jmp:=hs;
          end;
        else
          internalerror(10001);
      end;
    end;


{****************************************************************************
                            TI386ATTASMOUTPUT
 ****************************************************************************}



    procedure Tx86_64AttAssembler. WriteInstruction(hp: tai);
    var
      op       : tasmop;
      s        : string;
      sep      : char;
      calljmp  : boolean;
      i        : integer;
     begin
       if hp.typ <> ait_instruction then exit;
       taicpu(hp).SetOperandOrder(op_att);
       op:=taicpu(hp).opcode;
       calljmp:=is_calljmp(op);
       { call maybe not translated to call }
       s:=#9+att_op2str[op]+cond2str[taicpu(hp).condition];
       { suffix needed ?  fnstsw,fldcw don't support suffixes
         with binutils 2.9.5 under linux }
       if (not calljmp) and
           (att_needsuffix[op]<>AttSufNONE) and
           (op<>A_FNSTSW) and (op<>A_FSTSW) and
           (op<>A_FNSTCW) and (op<>A_FSTCW) and
           (op<>A_FLDCW) and not(
           (taicpu(hp).oper[0].typ=top_reg) and
           (taicpu(hp).oper[0].reg in [R_ST..R_ST7])
          ) then
              s:=s+att_opsize2str[taicpu(hp).opsize];
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
       as_x86_64_as_info : tasminfo =
          (
            id     : as_x86_64_as;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_any;
            outputbinary: false;
            allowdirect : true;
            externals : false;
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
  RegisterAssembler(as_x86_64_as_info,Tx86_64ATTAssembler);
end.
{
  $Log$
  Revision 1.2  2002-07-25 22:55:33  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code
}

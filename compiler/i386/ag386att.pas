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
{ This unit implements an asmoutput class for i386 AT&T syntax
}
unit ag386att;

{$i fpcdefs.inc}

interface

    uses
      cclasses,cpubase,
      globals,
      aasmbase,aasmtai,aasmcpu,assemble,aggas;

    type
      T386ATTAssembler=class(TGNUassembler)
      private
        procedure WriteReference(var ref : treference);
        procedure WriteOper(const o:toper);
        procedure WriteOper_jmp(const o:toper);
      public
        procedure WriteInstruction(hp: tai);override;
      end;

    TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint);

    const
      gas_op2str:op2strtable={$i i386att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i i386atts.inc}

      gas_reg2str : reg2strtable = ('',
        '%eax','%ecx','%edx','%ebx','%esp','%ebp','%esi','%edi',
        '%ax','%cx','%dx','%bx','%sp','%bp','%si','%di',
        '%al','%cl','%dl','%bl','%ah','%ch','%bh','%dh',
        '%cs','%ds','%es','%ss','%fs','%gs',
        '%st','%st(0)','%st(1)','%st(2)','%st(3)','%st(4)','%st(5)','%st(6)','%st(7)',
        '%dr0','%dr1','%dr2','%dr3','%dr6','%dr7',
        '%cr0','%cr2','%cr3','%cr4',
        '%tr3','%tr4','%tr5','%tr6','%tr7',
        '%mm0','%mm1','%mm2','%mm3','%mm4','%mm5','%mm6','%mm7',
        '%xmm0','%xmm1','%xmm2','%xmm3','%xmm4','%xmm5','%xmm6','%xmm7'
       );
       
     regname_count=45;
     regname_count_bsstart=32;
     
     gas_regname2regnum:array[0..regname_count-1] of regname2regnumrec=(
        (name:'%ah';     number:NR_AH),
        (name:'%al';     number:NR_AL),
        (name:'%ax';     number:NR_AX),
        (name:'%bh';     number:NR_BH),
        (name:'%bl';     number:NR_BL),
        (name:'%bp';     number:NR_BP),
        (name:'%bx';     number:NR_BX),
        (name:'%ch';     number:NR_CH),
        (name:'%cl';     number:NR_CL),
        (name:'%cs';     number:NR_CS),
        (name:'%cr0';    number:NR_CR0),
        (name:'%cr2';    number:NR_CR2),
        (name:'%cr3';    number:NR_CR3),
        (name:'%cr4';    number:NR_CR4),
        (name:'%cx';     number:NR_CX),
        (name:'%dh';     number:NR_DH),
        (name:'%dl';     number:NR_DL),
        (name:'%di';     number:NR_DI),
        (name:'%dr0';    number:NR_DR0),
        (name:'%dr1';    number:NR_DR1),
        (name:'%dr2';    number:NR_DR2),
        (name:'%dr3';    number:NR_DR3),
        (name:'%dr6';    number:NR_DR6),
        (name:'%dr7';    number:NR_DR7),
        (name:'%ds';     number:NR_DS),
        (name:'%dx';     number:NR_DX),
        (name:'%eax';    number:NR_EAX),
        (name:'%ebp';    number:NR_EBP),
        (name:'%ebx';    number:NR_EBX),
        (name:'%ecx';    number:NR_ECX),
        (name:'%edi';    number:NR_EDI),
        (name:'%edx';    number:NR_EDX),
        (name:'%es';     number:NR_ES),
        (name:'%esi';    number:NR_ESI),
        (name:'%esp';    number:NR_ESP),
        (name:'%fs';     number:NR_FS),
        (name:'%gs';     number:NR_GS),
        (name:'%si';     number:NR_SI),
        (name:'%sp';     number:NR_SP),
        (name:'%ss';     number:NR_SS),
        (name:'%tr3';    number:NR_DR0),
        (name:'%tr4';    number:NR_DR1),
        (name:'%tr5';    number:NR_DR2),
        (name:'%tr6';    number:NR_DR6),
        (name:'%tr7';    number:NR_DR7)
     );

     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','bw','bl','wl',
       's','l','q',
       's','l','t','d','q','v','',
       '','',''
     );

     function gas_regnum_search(const s:string):Tnewregister;
       

  implementation

    uses
      cutils,systems,
      verbose;


{****************************************************************************
                            TI386ATTASMOUTPUT
 ****************************************************************************}

    procedure T386AttAssembler.WriteReference(var ref : treference);
      begin
        with ref do
         begin
           inc(offset,offsetfixup);
           offsetfixup:=0;
         { have we a segment prefix ? }
         { These are probably not correctly handled under GAS }
         { should be replaced by coding the segment override  }
         { directly! - DJGPP FAQ                              }
           if segment.enum>lastreg then
             internalerror(200301081);
           if base.enum>lastreg then
             internalerror(200301081);
           if index.enum>lastreg then
             internalerror(200301081);
           if segment.enum<>R_NO then
            AsmWrite(gas_reg2str[segment.enum]+':');
           if assigned(symbol) then
             AsmWrite(symbol.name);
           if offset<0 then
            AsmWrite(tostr(offset))
           else
            if (offset>0) then
             begin
               if assigned(symbol) then
                AsmWrite('+'+tostr(offset))
               else
                AsmWrite(tostr(offset));
             end
           else if (index.enum=R_NO) and (base.enum=R_NO) and not assigned(symbol) then
             AsmWrite('0');
           if (index.enum<>R_NO) and (base.enum=R_NO) then
            begin
              AsmWrite('(,'+gas_reg2str[index.enum]);
              if scalefactor<>0 then
               AsmWrite(','+tostr(scalefactor)+')')
              else
               AsmWrite(')');
            end
           else
            if (index.enum=R_NO) and (base.enum<>R_NO) then
             AsmWrite('('+gas_reg2str[base.enum]+')')
            else
             if (index.enum<>R_NO) and (base.enum<>R_NO) then
              begin
                AsmWrite('('+gas_reg2str[base.enum]+','+gas_reg2str[index.enum]);
                if scalefactor<>0 then
                 AsmWrite(','+tostr(scalefactor));
                AsmWrite(')');
              end;
         end;
      end;


    procedure T386AttAssembler.WriteOper(const o:toper);
      begin
        case o.typ of
          top_reg :
            begin
              if o.reg.enum>lastreg then
                internalerror(200301081);
              AsmWrite(gas_reg2str[o.reg.enum]);
            end;
          top_ref :
            WriteReference(o.ref^);
          top_const :
            AsmWrite('$'+tostr(longint(o.val)));
          top_symbol :
            begin
              AsmWrite('$');
              if assigned(o.sym) then
               AsmWrite(o.sym.name);
              if o.symofs>0 then
               AsmWrite('+'+tostr(o.symofs))
              else
               if o.symofs<0 then
                AsmWrite(tostr(o.symofs))
              else
               if not(assigned(o.sym)) then
                 AsmWrite('0');
            end;
          else
            internalerror(10001);
        end;
      end;


    procedure T386AttAssembler.WriteOper_jmp(const o:toper);
      begin
        case o.typ of
          top_reg :
            begin
              if o.reg.enum>lastreg then
                internalerror(200301081);
              AsmWrite('*'+gas_reg2str[o.reg.enum]);
            end;
          top_ref :
            begin
              AsmWrite('*');
              WriteReference(o.ref^);
            end;
          top_const :
            AsmWrite(tostr(longint(o.val)));
          top_symbol :
            begin
              AsmWrite(o.sym.name);
              if o.symofs>0 then
               AsmWrite('+'+tostr(o.symofs))
              else
               if o.symofs<0 then
                AsmWrite(tostr(o.symofs));
            end;
          else
            internalerror(10001);
        end;
      end;


    procedure T386AttAssembler.WriteInstruction(hp: tai);
      var
       op       : tasmop;
       calljmp  : boolean;
       i        : integer;
      begin
        if hp.typ <> ait_instruction then
          exit;
        taicpu(hp).SetOperandOrder(op_att);
        op:=taicpu(hp).opcode;
        calljmp:=is_calljmp(op);
        { call maybe not translated to call }
        AsmWrite(#9+gas_op2str[op]+cond2str[taicpu(hp).condition]);
        { suffix needed ?  fnstsw,fldcw don't support suffixes
          with binutils 2.9.5 under linux }
        if (Taicpu(hp).oper[0].typ=top_reg) and
            (Taicpu(hp).oper[0].reg.enum>lastreg) then
          internalerror(200301081);
          
        if (not calljmp) and
            (gas_needsuffix[op]<>AttSufNONE) and
            (op<>A_FNSTSW) and (op<>A_FSTSW) and
            (op<>A_FNSTCW) and (op<>A_FSTCW) and
            (op<>A_FLDCW) and not(
            (taicpu(hp).oper[0].typ=top_reg) and
            (taicpu(hp).oper[0].reg.enum in [R_ST..R_ST7])
           ) then
          AsmWrite(gas_opsize2str[taicpu(hp).opsize]);
        { process operands }
        if taicpu(hp).ops<>0 then
          begin
            if calljmp then
             begin
               AsmWrite(#9);
               WriteOper_jmp(taicpu(hp).oper[0]);
             end
            else
             begin
               for i:=0 to taicpu(hp).ops-1 do
                 begin
                   if i=0 then
                     AsmWrite(#9)
                   else
                     AsmWrite(',');
                   WriteOper(taicpu(hp).oper[i]);
                 end;
             end;
          end;
        AsmLn;
      end;


     function gas_regnum_search(const s:string):Tnewregister;
     
     {Searches the register number that belongs to the register in s.
      s must be in uppercase!.}
     
     var i,p:byte;
     
     begin
        {Binary search.}
        p:=0;
        i:=regname_count_bsstart;
        while i<>0 do
          begin
            if (p+i<regname_count) and (upper(gas_regname2regnum[p+i].name)<=s) then
              p:=p+i;
            i:=i shr 1;
          end;
        if upper(gas_regname2regnum[p].name)=s then
          gas_regnum_search:=gas_regname2regnum[p].number
        else
          gas_regnum_search:=NR_NO;
     end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_i386_as_info : tasminfo =
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

       as_i386_as_aout_info : tasminfo =
          (
            id           : as_i386_as_aout;
            idtxt  : 'AS_AOUT';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_i386_os2;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr','COMMON')
          );

       as_i386_asw_info : tasminfo =
          (
            id           : as_i386_asw;
            idtxt  : 'ASW';
            asmbin : 'asw';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_i386_win32;
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.section .bss',
              '.section .idata$2','.section .idata$4','.section .idata$5',
                '.section .idata$6','.section .idata$7','.section .edata',
              '.stab','.stabstr','COMMON')
          );

initialization
  RegisterAssembler(as_i386_as_info,T386ATTAssembler);
  RegisterAssembler(as_i386_as_aout_info,T386ATTAssembler);
  RegisterAssembler(as_i386_asw_info,T386ATTAssembler);
end.
{
  $Log$
  Revision 1.30  2003-02-19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.29  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.28  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.27  2002/12/24 18:10:34  peter
    * Long symbol names support

  Revision 1.26  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.25  2002/07/26 21:15:42  florian
    * rewrote the system handling

  Revision 1.24  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.23  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.22  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.21  2002/05/16 19:46:49  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.19  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.18  2002/04/15 19:12:10  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.17  2002/04/14 16:58:04  carl
  + move into aggas most of the stuff non-processor specific

  Revision 1.16  2002/04/10 08:07:55  jonas
    * fix for the ie9999 under Linux (patch from Peter)

  Revision 1.15  2002/04/04 19:06:06  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.14  2002/04/04 18:26:55  carl
  + added wdosx patch from Pavel

  Revision 1.13  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}

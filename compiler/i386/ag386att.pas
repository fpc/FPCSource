{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit ag386att;

{$i defines.inc}

interface

    uses
      cclasses,
      globals,
      aasm,assemble,aggas;

    type
      T386ATTAssembler=class(TGNUassembler)
      public
        procedure WriteInstruction(hp: tai);  override;
      end;

  implementation

    uses
{$ifdef Delphi}
      dmisc,
{$else Delphi}
      dos,
{$endif Delphi}
      cutils,globtype,systems,
      fmodule,finput,verbose,cpubase,cpuasm,tainst
{$ifdef GDB}
  {$ifdef delphi}
      ,sysutils
  {$else}
      ,strings
  {$endif}
      ,gdb
{$endif GDB}
      ;



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
          s:=gas_reg2str[segment]+':'
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
            s:=s+'(,'+gas_reg2str[index];
            if scalefactor<>0 then
             s:=s+','+tostr(scalefactor)+')'
            else
             s:=s+')';
          end
         else
          if (index=R_NO) and (base<>R_NO) then
           s:=s+'('+gas_reg2str[base]+')'
          else
           if (index<>R_NO) and (base<>R_NO) then
            begin
              s:=s+'('+gas_reg2str[base]+','+gas_reg2str[index];
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
          getopstr:=gas_reg2str[o.reg];
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
          getopstr_jmp:='*'+gas_reg2str[o.reg];
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



    procedure T386AttAssembler. WriteInstruction(hp: tai);
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
       as_i386_as_info : tasminfo =
          (
            id           : as_i386_as;
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
              '.stab','.stabstr')
          );

       as_i386_as_aout_info : tasminfo =
          (
            id           : as_i386_as_aout;
            idtxt  : 'AS_AOUT';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_os2;
            outputbinary: false;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : 'L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.bss',
              '','','','','','',
              '.stab','.stabstr')
          );

       as_i386_asw_info : tasminfo =
          (
            id           : as_i386_asw;
            idtxt  : 'ASW';
            asmbin : 'asw';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_win32;
            outputbinary: false;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.section .bss',
              '.section .idata$2','.section .idata$4','.section .idata$5',
                '.section .idata$6','.section .idata$7','.section .edata',
              '.stab','.stabstr')
          );

       as_i386_aswwdosx_info : tasminfo =
          (
            id           : as_i386_aswdosx;
            idtxt  : 'ASWDOSX';
            asmbin : 'asw';
            asmcmd : '-o $OBJ $ASM';
            supported_target : target_i386_wdosx;
            outputbinary: false;
            allowdirect : true;
            externals : false;
            needar : true;
            labelprefix_only_inside_procedure : false;
            labelprefix : '.L';
            comment : '# ';
            secnames : ('',
              '.text','.data','.section .bss',
              '.section .idata$2','.section .idata$4','.section .idata$5',
                '.section .idata$6','.section .idata$7','.section .edata',
              '.stab','.stabstr')
          );


initialization
  RegisterAssembler(as_i386_as_info,T386ATTAssembler);
  RegisterAssembler(as_i386_as_aout_info,T386ATTAssembler);
  RegisterAssembler(as_i386_asw_info,T386ATTAssembler);
  RegisterAssembler(as_i386_aswwdosx_info,T386ATTAssembler);
end.
{
  $Log$
  Revision 1.17  2002-04-14 16:58:04  carl
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

  Revision 1.12  2001/12/29 15:29:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.11  2001/09/17 21:29:13  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.10  2001/08/30 20:57:10  peter
    * asbsd merged

  Revision 1.9  2001/05/06 17:13:23  jonas
    * completed incomplete typed constant records

  Revision 1.8  2001/04/21 15:33:03  peter
    * stupid bug, finalization to initialization renaming

  Revision 1.7  2001/04/21 12:09:00  peter
    * fixed bug 1472 (merged)

  Revision 1.6  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.5  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.4  2001/03/05 21:39:11  peter
    * changed to class with common TAssembler also for internal assembler

  Revision 1.3  2001/01/13 20:24:24  peter
    * fixed operand order that got mixed up for external writers after
      my previous assembler block valid instruction check

  Revision 1.2  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.1  2000/11/30 22:18:48  florian
    * moved to i386

  Revision 1.6  2000/09/24 15:06:10  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/20 17:38:21  peter
    * smartlinking fixed for linux (merged)

  Revision 1.3  2000/07/13 12:08:24  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:28  michael
  + removed logs

}

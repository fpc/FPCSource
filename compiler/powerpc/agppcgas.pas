{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an asm for the PowerPC

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
{ This unit implements the GNU Assembler writer for the PowerPC
}

unit agppcgas;

{$i fpcdefs.inc}

  interface

    uses
       aasmtai,
       aggas,
       cpubase;

    type
      PPPCGNUAssembler=^TPPCGNUAssembler;
      TPPCGNUAssembler=class(TGNUassembler)
        procedure WriteExtraHeader;override;
        procedure WriteInstruction(hp : tai);override;
      end;


  implementation

    uses
       cutils,globals,verbose,
       cgbase,systems,
       assemble,
       itppcgas,
       aasmcpu;

    procedure TPPCGNUAssembler.WriteExtraHeader;
      var
         i : longint;
      begin
         for i:=0 to 31 do
           AsmWriteln(#9'.set'#9'r'+tostr(i)+','+tostr(i));
         for i:=0 to 31 do
           AsmWriteln(#9'.set'#9'f'+tostr(i)+','+tostr(i));
      end;

    const
       as_ppc_gas_info : tasminfo =
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
              '.text','.data','.text',
              '','','','','','',
              '.stab','.stabstr','COMMON')
          );

     symaddr2str: array[trefsymaddr] of string[3] = ('','@ha','@l');


    function getreferencestring(var ref : treference) : string;
    var
      s : string;
    begin
       with ref do
        begin
          inc(offset,offsetfixup);
          if ((offset < -32768) or (offset > 32767)) and
             (symaddr = refs_full) then
            internalerror(19991);
          if (symaddr = refs_full) then
            s := ''
          else if not assigned(symbol) then
            s := '('
          else
            s:='('+symbol.name;
          if offset<0 then
           s:=s+tostr(offset)
          else
           if (offset>0) then
            begin
              if assigned(symbol) then
               s:=s+'+'+tostr(offset)
              else
               s:=s+tostr(offset);
            end;

           if (symaddr <> refs_full) then
             s := s+')'+symaddr2str[symaddr];

           if (index=NR_NO) and (base<>NR_NO) then
             begin
                if offset=0 then
                  begin
                     if assigned(symbol) then
                       s:=s+'+0'
                     else
                       s:=s+'0';
                  end;
                s:=s+'('+gas_regname(base)+')';
             end
           else if (index<>NR_NO) and (base<>NR_NO) then
             begin
               if (offset=0) then
                 s:=s+gas_regname(base)+','+gas_regname(index)
               else
                 internalerror(19992);
             end;
        end;
      getreferencestring:=s;
    end;


    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:=gas_regname(o.reg);
        { no top_ref jumping for powerpc }
        top_const :
          getopstr_jmp:=tostr(o.val);
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
        top_none:
          getopstr_jmp:='';
        else
          internalerror(2002070603);
      end;
    end;

    function getopstr(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg:
          getopstr:=gas_regname(o.reg);
        top_const:
          getopstr:=tostr(longint(o.val));
        top_ref:
          getopstr:=getreferencestring(o.ref^);
        top_symbol:
          begin
            hs:=o.sym.name;
            if o.symofs>0 then
             hs:=hs+'+'+tostr(o.symofs)
            else
             if o.symofs<0 then
              hs:=hs+tostr(o.symofs);
            getopstr:=hs;
          end;
        else
          internalerror(2002070604);
      end;
    end;

    function branchmode(o: tasmop): string[4];
      var tempstr: string[4];
      begin
        tempstr := '';
        case o of
          A_BCCTR,A_BCCTRL: tempstr := 'ctr';
          A_BCLR,A_BCLRL: tempstr := 'lr';
        end;
        case o of
          A_BL,A_BLA,A_BCL,A_BCLA,A_BCCTRL,A_BCLRL: tempstr := tempstr+'l';
        end;
        case o of
          A_BA,A_BLA,A_BCA,A_BCLA: tempstr:=tempstr+'a';
        end;
        branchmode := tempstr;
      end;

    function cond2str(op: tasmop; c: tasmcond): string;
    { note: no checking is performed whether the given combination of }
    { conditions is valid                                             }
    var tempstr: string;
    begin
      tempstr:=#9;
      case c.simple of
        false: cond2str := tempstr+gas_op2str[op]+#9+tostr(c.bo)+','+
                           tostr(c.bi);
        true:
          if (op >= A_B) and (op <= A_BCLRL) then
            case c.cond of
              { unconditional branch }
              C_NONE:
                cond2str := tempstr+gas_op2str[op];
              { bdnzt etc }
              else
                begin
                  tempstr := tempstr+'b'+asmcondflag2str[c.cond]+
                              branchmode(op)+#9;
                  case c.cond of
                    C_LT..C_NU:
                      cond2str := tempstr+gas_regname(newreg(R_SPECIALREGISTER,c.cr,R_SUBWHOLE));
                    C_T..C_DZF:
                      cond2str := tempstr+tostr(c.crbit);
                  end;
                end;
            end
          { we have a trap instruction }
          else
            begin
              internalerror(2002070601);
              { not yet implemented !!!!!!!!!!!!!!!!!!!!! }
              { case tempstr := 'tw';}
            end;
      end;
    end;

    Procedure TPPCGNUAssembler.WriteInstruction(hp : tai);
    var op: TAsmOp;
        s: string;
        i: byte;
        sep: string[3];
    begin
      op:=taicpu(hp).opcode;
      if is_calljmp(op) then
        begin
          { direct BO/BI in op[0] and op[1] not supported, put them in condition! }
          case op of
             A_B,A_BA,A_BL,A_BLA:
               s:=#9+gas_op2str[op]+#9;
             A_BCTR,A_BCTRL,A_BLR,A_BLRL:
               s:=#9+gas_op2str[op]
             else
               s:=cond2str(op,taicpu(hp).condition)+',';
          end;

          if (taicpu(hp).oper[0].typ <> top_none) then
            s:=s+getopstr_jmp(taicpu(hp).oper[0]);
        end
      else
        { process operands }
        begin
          s:=#9+gas_op2str[op];
          if taicpu(hp).ops<>0 then
            begin
            {
              if not is_calljmp(op) then
                sep:=','
              else
            }
                sep:=#9;
              for i:=0 to taicpu(hp).ops-1 do
                begin
                   // debug code
                   // writeln(s);
                   // writeln(taicpu(hp).fileinfo.line);
                   s:=s+sep+getopstr(taicpu(hp).oper[i]);
                   sep:=',';
                end;
            end;
        end;
      AsmWriteLn(s);
    end;

begin
  RegisterAssembler(as_ppc_gas_info,TPPCGNUAssembler);
end.
{
  $Log$
  Revision 1.31  2003-10-01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.30  2003/09/03 19:35:24  peter
    * powerpc compiles again

  Revision 1.29  2003/08/20 14:28:52  daniel
    * Fixed PowerPC compilation

  Revision 1.28  2003/08/19 11:53:03  daniel
    * Fixed PowerPC compilation

  Revision 1.27  2003/08/18 11:58:14  daniel
    * Improved -sr on PowerPC ATT asm writer

  Revision 1.26  2003/08/17 21:11:00  daniel
    * Now -sr works...

  Revision 1.25  2003/08/17 20:47:47  daniel
    * Notranslation changed into -sr functionality

  Revision 1.24  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.23  2003/04/24 22:29:58  florian
    * fixed a lot of PowerPC related stuff

  Revision 1.22  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.21  2003/03/12 22:43:38  jonas
    * more powerpc and generic fixes related to the new register allocator

  Revision 1.20  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.19  2002/11/07 15:50:23  jonas
    * fixed bctr(l) problems

  Revision 1.18  2002/09/08 13:03:26  jonas
    * several large offset-related fixes

  Revision 1.17  2002/09/01 21:04:48  florian
    * several powerpc related stuff fixed

  Revision 1.16  2002/08/31 19:27:48  jonas
    + support top_none for branches

  Revision 1.15  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output

  Revision 1.14  2002/08/18 22:16:14  florian
    + the ppc gas assembler writer adds now registers aliases
      to the assembler file

  Revision 1.13  2002/08/18 21:36:42  florian
    + handling of local variables in direct reader implemented

  Revision 1.12  2002/08/18 10:34:30  florian
    * more ppc assembling fixes

  Revision 1.11  2002/08/17 18:23:53  florian
    * some assembler writer bugs fixed

  Revision 1.10  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.9  2002/07/27 19:57:18  jonas
    * some typo corrections in the instruction tables
    * renamed the m* registers to v*

  Revision 1.8  2002/07/26 21:15:45  florian
    * rewrote the system handling

  Revision 1.7  2002/07/26 11:19:57  jonas
    * fixed range errors

  Revision 1.6  2002/07/21 16:56:20  jonas
    * fixed bugs with writing out unconditinal jumps

  Revision 1.5  2002/07/12 10:10:01  jonas
    * changed motorola syntax of references with symbols to GNU syntax

  Revision 1.4  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.3  2002/07/11 07:34:55  jonas
    * fixed mullw entry in instruction list

  Revision 1.2  2002/07/09 19:45:01  jonas
    * unarynminus and shlshr node fixed for 32bit and smaller ordinals
    * small fixes in the assembler writer
    * changed scratch registers, because they were used by the linker (r11
      and r12) and by the abi under linux (r31)

  Revision 1.1  2002/07/07 09:44:31  florian
    * powerpc target fixed, very simple units can be compiled
}

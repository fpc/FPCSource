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

  interface

    uses
       aasmtai,
       aggas;

    type
      PPPCGNUAssembler=^TPPCGNUAssembler;
      TPPCGNUAssembler=class(TGNUassembler)
        procedure WriteInstruction(hp : tai);override;
      end;

  implementation

    uses
       cutils,globals,verbose,
       systems,
       assemble,
       aasmcpu,cpubase;

    const
       as_ppc_gas_info : tasminfo =
          (
            id     : as_powerpc_as;
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
              '.text','.data','.text',
              '','','','','','',
              '.stab','.stabstr','COMMON')
          );

       op2str : array[tasmop] of string[14] = ('<none>',
         'add','add.','addo','addo.','addc','addc.','addco','addco.',
         'adde','adde.','addeo','addeo.','addi','addic','addic.','addis',
         'addme','addme.','addmeo','addmeo.','addze','addze.','addzeo',
         'addzeo.','and','and.','andc','andc.','andi.','andis.','b',
         'ba','bl','bla','bc','bca','bcl','bcla','bcctr','bcctrl','bclr',
         'bclrl','cmp','cmpi','cmpl','cmpli','cntlzw','cntlzw.','crand',
         'crandc','creqv','crnand','crnor','cror','crorc','crxor','dcba',
         'dcbf','dcbi','dcbst','dcbt','divw','divw.','divwo','divwo.',
         'divwu','divwu.','divwuo','divwuo.','eciwx','ecowx','eieio','eqv',
         'eqv.','extsb','extsb.','extsh','extsh.','fabs','fabs.','fadd',
         'fadd.','fadds','fadds.','fcompo','fcmpu','fctiw','fctw.','fctwz',
         'fctwz.','fdiv','fdiv.','fdivs','fdivs.','fmadd','fmadd.','fmadds',
         'fmadds.','fmr','fmsub','fmsub.','fmsubs','fmsubs.','fmul','fmul.',
         'fmuls','fmuls.','fnabs','fnabs.','fneg','fneg.','fnmadd',
         'fnmadd.','fnmadds','fnmadds.','fnmsub','fnmsub.','fnmsubs',
         'fnmsubs.','fres','fres.','frsp','frsp.','frsqrte','frsqrte.',
         'fsel','fsel.','fsqrt','fsqrt.','fsqrts','fsqrts.','fsub','fsub.',
         'fsubs','fsubs.','icbi','isync','lbz','lbzu','lbzux','lbzx',
         'lfd','lfdu','lfdux','lfdx','lfs','lfsu','lfsux','lfsx','lha',
         'lhau','lhaux','lhax','hbrx','lhz','lhzu','lhzux','lhzx','lmw',
         'lswi','lswx','lwarx','lwbrx','lwz','lwzu','lwzux','lwzx','mcrf',
         'mcrfs','mcrxr','lcrxe','mfcr','mffs','maffs.','mfmsr','mfspr','mfsr',
         'mfsrin','mftb','mtfcrf','mtfd0','mtfsb1','mtfsf','mtfsf.',
         'mtfsfi','mtfsfi.','mtmsr','mtspr','mtsr','mtsrin','mulhw',
         'mulhw.','mulhwu','mulhwu.','mulli','mullh','mullw.','mullwo',
         'mullwo.','nand','nand.','neg','neg.','nego','nego.','nor','nor.',
         'or','or.','orc','orc.','ori','oris', 'rfi', 'rlwimi', 'rlwimi.',
         'rlwinm', 'tlwinm.','rlwnm','sc','slw', 'slw.', 'sraw', 'sraw.',
         'srawi', 'srawi.','srw', 'srw.', 'stb', 'stbu', 'stbux','stbx','stfd',
         'stfdu', 'stfdux', 'stfdx', 'stfiwx', 'stfs', 'stfsu', 'stfsux', 'stfsx',
         'sth', 'sthbrx', 'sthu', 'sthux', 'sthx', 'stmw', 'stswi', 'stswx', 'stw',
         'stwbrx', 'stwx.', 'stwu', 'stwux', 'stwx', 'subf', 'subf.', 'subfo',
         'subfo.', 'subfc', 'subc.', 'subfco', 'subfco.', 'subfe', 'subfe.',
         'subfeo', 'subfeo.', 'subfic', 'subfme', 'subfme.', 'subfmeo', 'subfmeo.',
         'subfze', 'subfze.', 'subfzeo', 'subfzeo.', 'sync', 'tlbia', 'tlbie',
         'tlbsync', 'tw', 'twi', 'xor', 'xor.', 'xori', 'xoris',
         { some simplified mnemonics }
         'subi', 'subis', 'subic', 'subic.', 'sub', 'sub.', 'subo', 'subo.',
         'subc', 'subc.', 'subco', '.subco.', 'cmpwi', 'cmpw', 'cmplwi', 'cmplw',
         'extlwi', 'extlwi.', 'extrwi', 'extrwi.', 'inslwi', 'inslwi.', 'insrwi',
         'insrwi.', 'rotlwi', 'rotlwi.', 'rotlw', 'rotlw.', 'slwi', 'slwi.',
         'srwi', 'srwi.', 'clrlwi', 'clrlwi.', 'clrrwi', 'clrrwi.', 'clrslwi',
         'clrslwi.', 'blr', 'bctr', 'blrl', 'bctrl', 'crset', 'crclr', 'crmove',
         'crnot', 'mt', 'mf','nop', 'li', 'lis', 'la', 'mr','mr.','not', 'mtcr');

      reg2str : reg2strtable = ('',
        '0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16',
        '17','18','19','20','21','22','23','24','25','26','27','28','29','30','31',
        'F0','F1','F2','F3','F4','F5','F6','F7', 'F8','F9','F10','F11','F12',
        'F13','F14','F15','F16','F17', 'F18','F19','F20','F21','F22', 'F23','F24',
        'F25','F26','F27','F28','F29','F30','F31',
        'M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12',
        'M13','M14','M15','M16','M17','M18','M19','M20','M21','M22', 'M23','M24',
        'M25','M26','M27','M28','M29','M30','M31',
        'CR','CR0','CR1','CR2','CR3','CR4','CR5','CR6','CR7',
        'XER','LR','CTR','FPSCR'
      );

    function getreferencestring(var ref : treference) : string;
    var
      s : string;
    begin
       with ref do
        begin
          inc(offset,offsetfixup);
          if (offset < -32768) or (offset > 32767) then
{$ifndef testing}
            internalerror(19991);
{$else testing}
            begin
              writeln('internalerror 19991');
              halt(1);
            end;
{$endif testing}
          s:='';
          if assigned(symbol) then
           s:=s+symbol.name + symaddr2str[symaddr];
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
           if (index=R_NO) and (base<>R_NO) then
             s:=s+'('+reg2str[base]+')'
           else if (index<>R_NO) and (base<>R_NO) and (offset = 0) then
             s:=s+reg2str[base]+','+reg2str[index]
           else if ((index<>R_NO) or (base<>R_NO)) then
{$ifndef testing}
            internalerror(19992);
{$else testing}
            begin
              writeln('internalerror 19992');
              halt(1);
            end;
{$endif testing}
        end;
      getreferencestring:=s;
    end;

    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:=reg2str[o.reg];
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
        else
{$ifndef testing}
          internalerror(2002070603);
{$else testing}
          begin
            writeln('internalerror 10001');
            halt(1);
          end;
{$endif testing}
      end;
    end;

    function getopstr(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg:
          getopstr:=reg2str[o.reg];
        { no top_ref jumping for powerpc }
        top_const:
          getopstr:=tostr(o.val);
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
{$ifndef testing}
          internalerror(2002070604);
{$else testing}
          begin
            writeln('internalerror 10001');
            halt(1);
          end;
{$endif testing}
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
        false: cond2str := tempstr+op2str[op]+#9+tostr(c.bo)+','+
                           tostr(c.bi);
        true:
          if (op >= A_B) and (op <= A_BCLRL) then
            case c.cond of
              { unconditional branch }
              C_NONE:
                cond2str := tempstr+op2str[op];
              { bdnzt etc }
              else
                begin
                  tempstr := tempstr+'b'+asmcondflag2str[c.cond]+
                              branchmode(op)+#9;
                  case c.cond of
                    C_LT..C_NU:
                      cond2str := tempstr+reg2str[c.cr];
                    C_T..C_DZF:
                      cond2str := tempstr+tostr(c.crbit);
                  end;
                end;
            end
          { we have a trap instruction }
          else
            begin
              internalerror(2002070601);
              { not yet implementer !!!!!!!!!!!!!!!!!!!!! }
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
      { direct BO/BI in op[0] and op[1] not supported, put them in condition! }
        s:=cond2str(op,taicpu(hp).condition)+
           getopstr_jmp(taicpu(hp).oper[0])
      else
        { process operands }
        begin
          s:=#9+op2str[op];
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
  Revision 1.1  2002-07-07 09:44:31  florian
    * powerpc target fixed, very simple units can be compiled

  Revision 1.6  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.5  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.3  2002/04/20 21:41:51  carl
  * renamed some constants

  Revision 1.2  2002/04/06 18:13:01  jonas
    * several powerpc-related additions and fixes
}

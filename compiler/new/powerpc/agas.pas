{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit agas;

  interface

    uses
       dos,globals,systems,errors,cobjects,aasm,alpha,strings,files
{$ifdef GDB}
       ,gdb
{$endif GDB}
       ;

    type
      paicpuattasmlist=^taicpuattasmlist;
      taicpuattasmlist=object(tasmlist)
        function getreferencestring(var ref : treference) : string; Virtual;
        function getopstr_jmp(const o:toper) : string; Virtual;

        procedure WriteInstruction (P : Pai); virtual;
        procedure cond2str(op: tasnop; c: tasmcond): string;

        { to construct the output for conditional branches }
        function branchmode(o: tasmop): string[4];

      end;

  implementation

    const
       att_op2str : array[tasmop] of string[14] = ('<none>',
    'add','add.','addo','addo.','addc','addc.','addco','addco.,
    'adde','adde.','addeo','addeo.','addi','addic','addic.','addis,
    'addme','addme.','addmeo','addmeo.','addze','addze.','addzeo,
    'addzeo.','and','and.','andc','andc.','andi.','andis.','b,
    'ba','bl','bla','bc','bca','bcl','bcla','bcctr','bcctrl','bclr,
    'bclrl','cmp','cmpi','cmpl','cmpli','cntlzw','cntlzw.','crand,
    'crandc','creqv','crnand','crnor','cror','crorc','crxor','dcba,
    'dcbf','dcbi','dcbst','dcbt','divw','divw.','divwo','divwo.,
    'divwu','divwu.','divwuo','divwuo.','eciwx','ecowx','eieio','eqv,
    'eqv.','extsb','extsb.','extsh','extsh.','fabs','fabs.','fadd,
    'fadd.','fadds','fadds.','fcompo','fcmpu','fctiw','fctw.','fctwz,
    'fctwz.','fdiv','fdiv.','fdivs','fdivs.','fmadd','fmadd.','fmadds,
    'fmadds.','fmr','fmsub','fmsub.','fmsubs','fmsubs.','fmul','fmul.,
    'fmuls','fmuls.','fnabs','fnabs.','fneg','fneg.','fnmadd,
    'fnmadd.','fnmadds','fnmadds.','fnmsub','fnmsub.','fnmsubs,
    'fnmsubs.','fres','fres.','frsp','frsp.','frsqrte','frsqrte.,
    'fsel','fsel.','fsqrt','fsqrt.','fsqrts','fsqrts.','fsub','fsub.,
    'fsubs','fsubs.','icbi','isync','lbz','lbzu','lbzux','lbzx,
    'lfd','lfdu','lfdux','lfdx','lfs','lfsu','lfsux','lfsx','lha,
    'lhau','lhaux','lhax','hbrx','lhz','lhzu','lhzux','lhzx','lmw,
    'lswi','lswx','lwarx','lwbrx','lwz','lwzu','lwzux','lwzx','mcrf,
    'mcrfs','lcrxe','mfcr','mffs','maffs.','mfmsr','mfspr','mfsr,
    'mfsrin','mftb','mtfcrf','mtfd0','mtfsb1','mtfsf','mtfsf.,
    'mtfsfi','mtfsfi.','mtmsr','mtspr','mtsr','mtsrin','mulhw,
    'mulhw.','mulhwu','mulhwu.','mulli','mullh','mullw.','mullwo,
    'mullwo.','nand','nand.','neg','neg.','nego','nego.','nor','nor.,
    'or','or.','orc','orc.','ori','oris', 'rfi', 'rlwimi', 'rlwimi.',
    'rlwinm', 'tlwinm.','rlwnm','sc','slw', 'slw.', 'sraw', 'sraw.,
    'srawi', 'srawi.','srw', 'srw.', 'stb', 'stbu', 'stbux','stbx','stfd',
    'stfdu', 'stfdux', 'stfdx', 'stfiwx', 'stfs', 'stfsu', 'stfsux', 'stfsx',
    'sth', 'sthbrx', 'sthu', 'sthux', 'sthx', 'stmw', 'stswi', 'stswx', 'stw',
    'stwbrx', 'stwx.', 'stwu', 'stwux', 'stwx', 'subf', 'subf.', 'subfo',
    'subfo.', 'subfc', 'subc.', 'subfco', 'subfco.', 'subfe', 'subfe.',
    'subfeo', 'subfeo.', 'subfic', 'subfme', 'subfme.', 'subfmeo', 'subfmeo.,
    'subfze', 'subfze.', 'subfzeo', 'subfzeo.', 'sync', 'tlbia', 'tlbie,
    'tlbsync', 'tw', 'twi', 'xor', 'xor.', 'xori', 'xoris',
    { some simplified mnemonics }
    'subi', 'subis', 'subic', 'subic.', 'sub', 'sub.', 'subo', 'subo.',
    'subc', 'subc.', 'subco', '.subco.', 'cmpwi', 'cmpw', 'cmplwi', 'cmplw',
    'extlwi', 'extlwi.', 'extrwi', 'extrwi.', 'inslwi', 'inslwi.', 'insrwi',
    'insrwi.', 'rotlwi', 'rotlwi.', 'rotlw', 'rotlw.', 'slwi', 'slwi.',
    'srwi', 'srwi.', 'clrlwi', 'clrlwi.', 'clrrwi', 'clrrwi.', 'clrslwi',
    'clrslwi.', 'blr', 'bctr', 'blrl', 'bctrl', 'crset', 'crclr', 'crmove',
    'crnot', 'mt', 'mf','nop', 'li', 'la', 'mr','not', 'mtcr');

    function taicpuattasmlist.getreferencestring(var ref : treference) : string;
    var
      s : string;
    begin
      if ref.is_immediate then
       begin
         internalerror(1000101);
         exit;
       end
      else
       begin
         with ref do
          begin
            inc(offset,offsetfixup);
            if (offset < -32768) or (offset > 32767) then
              internalerror(19991);
            s:='';
            if assigned(symbol) then
             s:=s+symbol^.name + symaddr2str[symaddr];
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
               s:=s+'('+att_reg2str[base]+')'
             else if (index<>R_NO) and (base<>R_NO) and (offset = 0) then
               s:=s+att_reg2str[base]+','+att_reg2str[index]
             else if (index<>R_NO) or (base<>R_NO)) then
               internalerror(19992);
          end;
       end;
      getreferencestring:=s;
    end;

    function taicpuattasmlist.getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:=att_reg2str[o.reg];
        { no top_ref jumping for powerpc }
        top_const :
          getopstr_jmp:=tostr(o.val);
        top_symbol :
          begin
            hs:=o.sym^.name;
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


    Procedure taicpuattasmlist.WriteInstruction (P : Pai);
    var op: TAsmOp;
        s: string;
        i: byte;
    begin
      op:=paicpu(hp)^.opcode;
      if not is_calljmp(o) then
        s:=#9+att_op2str[op];
      else
    { direct BO/BI in op[0] and op[1] not supported, put them in condition! }
        s:=s+cond2str(op,paicpu(hp)^.condition)+
           getopstr_jmp(paicpu(hp)^.oper[0])
      else
    { process operands }
        begin
          if paicpu(hp)^.ops<>0 then
            begin
              if not is_calljmp(op) then
                sep := ','
              else sep := '#9';
              for i:=0 to paicpu(hp)^.ops-1 do
              begin
                s:=s+sep+getopstr(paicpu(hp)^.oper[i])
                sep:=',';
              end;
            end;
        end;
      AsmWriteLn(s);
    end;

    procedure taicpuattasmlist.cond2str(op: tasmop; c: tasmcond): string;
    { note: no checking is performed whether the given combination of }
    { conditions is valid                                             }
    var tempstr: sintrg;
    begin
      tempstr := '#9';
      case c.simple of
        false: cond2str := tempstr+att_op2str[op]+'#9'+tostr(c.bo)+','+
                           tostr(c.bi);
        true:
          if (op >= A_B) and (op <= A_BCLRL) then
            case c.cond of
              { unconditional branch }
              CF_NONE: condstr := tempstr+op2str(op);
              { bdnzt etc }
              else
                begin
                  tempstr := tempstr+'b'+asmcondflag2str[c.cond]+
                              branchmode(op)+'#9';
                  case op of
                    CF_LT..CF_NU:
                      cond2str := tempstr+att_reg2str[c.cr];
                    CF_T..CF_DZF:
                      cond2str := tempstr+tostr(c.crbit);
                  end;
                end;
            end
          { we have a trap instruction }
          { not yet implementer !!!!!!!!!!!!!!!!!!!!! }
{          else
            begin
              case tempstr := 'tw';}
      end;
    end;

    function taicpuattasmlist.branchmode(o: tasmop): string[4];
      var tempstr: string[4];
      begin
        tempstr := '';
        case o of
          A_BCCTR,A_BCCTRL: tempstr := 'ctr'
          A_BCLR,A_BCLRL: tempstr := 'lr'
        case o of
          A_BL,A_BLA,A_BCL,A_BCLA,A_BCCTRL,A_BCLRL: tempstr := tempstr+'l';
        end;
        case o of
          A_BA,A_BLA,A_BCA,A_BCLA: tempstr:=tempstr+'a';
        end;
        branchmode := tempstr;
      end;

end.
{
  $Log$
  Revision 1.4  2000-01-07 01:14:57  peter
    * updated copyright to 2000

  Revision 1.3  1999/09/03 13:15:47  jonas
    + implemented most necessary methods

  Revision 1.2  1999/08/25 12:00:22  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.1  1999/08/03 23:37:52  jonas
    + initial implementation for PowerPC based on the Alpha stuff

}

{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit implements an asmoutput class for PowerPC with MPW syntax

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
{
  This unit implements an asmoutput class for PowerPC with MPW syntax
}
unit agppcmpw;

{$i fpcdefs.inc}

interface

    uses
       aasmbase,aasmtai,aasmcpu,assemble,
       cpubase;

    const
      mpw_reg2str : treg2strtable = ('',
        'r0','r1','r2','r3','r4','r5','r6','r7','r8','r9','r10','r11','r12','r13','r14','r15','r16',
        'r17','r18','r19','r20','r21','r22','r23','r24','r25','r26','r27','r28','r29','r30','r31',
        'f0','f1','f2','f3','f4','f5','f6','f7', 'f8','f9','f10','f11','f12',
        'f13','f14','f15','f16','f17', 'f18','f19','f20','f21','f22', 'f23','f24',
        'f25','f26','f27','f28','f29','f30','f31',
        'v0','v1','v2','v3','v4','v5','v6','v7','v8','v9','v10','v11','v12',
        'v13','v14','v15','v16','v17','v18','v19','v20','v21','v22', 'v23','v24',
        'v25','v26','v27','v28','v29','v30','v31',
        'cR','cr0','cr1','cr2','cr3','cr4','cr5','cr6','cr7',
        'xer','lr','ctr','fpscr'
      );
    type
      TPPCMPWAssembler = class(TExternalAssembler)
        procedure WriteTree(p:TAAsmoutput);override;
        procedure WriteAsmList;override;
        Function  DoAssemble:boolean;override;
        procedure WriteExternals;
        procedure WriteAsmFileHeader;
      end;


  implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      cutils,globtype,globals,systems,cclasses,
      verbose,finput,fmodule,script,cpuinfo
      ;

    const
      line_length = 70;



    function ReplaceForbiddenChars(var s: string):Boolean;
         {Returns wheater a replacement has occured.}

        var
          i:Integer;

        {The dollar sign is not allowed in MPW PPCAsm}

    begin
      ReplaceForbiddenChars:=false;
      for i:=1 to Length(s) do
        if s[i]='$' then
          begin
            s[i]:='s';
            ReplaceForbiddenChars:=true;
          end;
    end;

    const

{*** From here is copyed from agppcgas.pp, except where marked with CHANGED.
     Perhaps put in a third common file. ***}

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
         'fadd.','fadds','fadds.','fcmpo','fcmpu','fctiw','fctw.','fctwz',
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
         'mulhw.','mulhwu','mulhwu.','mulli','mullw','mullw.','mullwo',
         'mullwo.','nand','nand.','neg','neg.','nego','nego.','nor','nor.',
         'or','or.','orc','orc.','ori','oris', 'rfi', 'rlwimi', 'rlwimi.',
         'rlwinm', 'rlwinm.','rlwnm','sc','slw', 'slw.', 'sraw', 'sraw.',
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
         'subc', 'subc.', 'subco', 'subco.', 'cmpwi', 'cmpw', 'cmplwi', 'cmplw',
         'extlwi', 'extlwi.', 'extrwi', 'extrwi.', 'inslwi', 'inslwi.', 'insrwi',
         'insrwi.', 'rotlwi', 'rotlwi.', 'rotlw', 'rotlw.', 'slwi', 'slwi.',
         'srwi', 'srwi.', 'clrlwi', 'clrlwi.', 'clrrwi', 'clrrwi.', 'clrslwi',
         'clrslwi.', 'blr', 'bctr', 'blrl', 'bctrl', 'crset', 'crclr', 'crmove',
         'crnot', 'mt', 'mf','nop', 'li', 'lis', 'la', 'mr','mr.','not', 'mtcr', 'mtlr', 'mflr',
         'mtctr', 'mfctr');

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
            begin
              {s:='('+symbol.name; CHANGED from this to: }
              s:= symbol.name;
              ReplaceForbiddenChars(s);
              s:='('+s;
            end;

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

           if (index=R_NO) and (base<>R_NO) then
             begin
                if offset=0 then
                  begin
                     if assigned(symbol) then
                       s:=s+'+0'
                     else
                       s:=s+'0';
                  end;
                s:=s+'('+mpw_reg2str[base]+')'
             end
           else if (index<>R_NO) and (base<>R_NO) and (offset=0) then
             s:=s+mpw_reg2str[base]+','+mpw_reg2str[index]
           else if ((index<>R_NO) or (base<>R_NO)) then
            internalerror(19992);
        end;
      getreferencestring:=s;
    end;

    function getopstr_jmp(const o:toper) : string;
    var
      hs : string;
    begin
      case o.typ of
        top_reg :
          getopstr_jmp:=mpw_reg2str[o.reg];
        { no top_ref jumping for powerpc }
        top_const :
          getopstr_jmp:=tostr(o.val);
        top_symbol :
          begin
            hs:=o.sym.name;
            ReplaceForbiddenChars(hs);
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
          getopstr:=mpw_reg2str[o.reg];
        { no top_ref jumping for powerpc }
        top_const:
          getopstr:=tostr(longint(o.val));
        top_ref:
          getopstr:=getreferencestring(o.ref^);
        top_symbol:
          begin
            hs:=o.sym.name;
            ReplaceForbiddenChars(hs);
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
                      cond2str := tempstr+mpw_reg2str[c.cr];
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

    Function GetInstruction(hp : tai):string; {CHANGED from method to proc}
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
             A_BA,A_BLA:
               s:=#9+op2str[op]+#9;
             A_B,A_BL:
               s:=#9+op2str[op]+#9'.';
             else
               s:=cond2str(op,taicpu(hp).condition)+',';
          end;
          s:=s+getopstr_jmp(taicpu(hp).oper[0]);
          if (op=A_B) or (op=A_BL) then
            s:=s+'[PR]';
        end
      else
        { process operands }
        begin
          case op of
             A_MFSPR:
               case taicpu(hp).oper[1].reg of
                  R_CR:
                    begin
                       op:=A_MFCR;
                       taicpu(hp).ops:=1;
                    end;
                  R_LR:
                    begin
                       op:=A_MFLR;
                       taicpu(hp).ops:=1;
                    end;
                  else
                    internalerror(2002100701);
               end;
             A_MTSPR:
               case taicpu(hp).oper[1].reg of
                  R_CR:
                    begin
                       op:=A_MTCR;
                       taicpu(hp).ops:=1;
                    end;
                  R_LR:
                    begin
                       op:=A_MTLR;
                       taicpu(hp).ops:=1;
                    end;
                  else
                    internalerror(2002100701);
               end;
          end;
          s:=#9+op2str[op];
          if taicpu(hp).ops<>0 then
            begin
               sep:=#9;
               for i:=0 to taicpu(hp).ops-1 do
                 begin
                   s:=s+sep+getopstr(taicpu(hp).oper[i]);
                   sep:=',';
                 end;
            end;
        end;
      GetInstruction:=s;
    end;

    {*** Until here is copyed from agppcgas.pp. ***}


    function single2str(d : single) : string;
      var
         hs : string;
         p : byte;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         single2str:=lower(hs);
      end;

    function double2str(d : double) : string;
      var
         hs : string;
         p : byte;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         double2str:=lower(hs);
      end;

   function fixline(s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j,k : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       for k:=j to i do
        if s[k] in [#0..#31,#127..#255] then
         s[k]:='.';
       fixline:=Copy(s,j,i-j+1);
     end;


{****************************************************************************
                               PowerPC MPW Assembler
 ****************************************************************************}

    var
      LasTSec : TSection;
      lastfileinfo : tfileposinfo;
      infile,
      lastinfile   : tinputfile;

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'dc.l'#9,#9'dc.w'#9,#9'dc.b'#9);

    Function PadTabs(const p:string;addch:char):string;
    var
      s : string;
      i : longint;
    begin
      i:=length(p);
      if addch<>#0 then
       begin
         inc(i);
         s:=p+addch;
       end
      else
       s:=p;
      if i<8 then
       PadTabs:=s+#9#9
      else
       PadTabs:=s+#9;
    end;


    procedure TPPCMPWAssembler.WriteTree(p:TAAsmoutput);
    const
      nolinetai =[ait_label,
                  ait_regalloc,ait_tempalloc,
{$ifdef GDB}
                  ait_stabn,ait_stabs,ait_stab_function_name,
{$endif GDB}
                  ait_cut,ait_marker,ait_align,ait_section];

        {TODO: Perhaps replace internalerror(10000) with something else}

    var
      s,
      prefix,
      suffix   : string;
      hp       : tai;
      counter,
      lines,
      InlineLevel : longint;
      i,j,l    : longint;
      consttyp : taitype;
      found,
      do_line,DoNotSplitLine,
      quoted   : boolean;
      sep      : char;
      replaced : boolean;

    begin
      if not assigned(p) then
       exit;
      { lineinfo is only needed for codesegment (PFV) }
      do_line:=((cs_asm_source in aktglobalswitches) or
                (cs_lineinfo in aktmoduleswitches))
                 and (p=codesegment);
      InlineLevel:=0;
      DoNotSplitLine:=false;
      hp:=tai(p.first);
      while assigned(hp) do
       begin
         if do_line and not(hp.typ in nolinetai) and
            not DoNotSplitLine then
           begin
           { load infile }
             if lastfileinfo.fileindex<>hp.fileinfo.fileindex then
              begin
                infile:=current_module.sourcefiles.get_file(hp.fileinfo.fileindex);
                if assigned(infile) then
                 begin
                   { open only if needed !! }
                   if (cs_asm_source in aktglobalswitches) then
                    infile.open;
                 end;
                { avoid unnecessary reopens of the same file !! }
                lastfileinfo.fileindex:=hp.fileinfo.fileindex;
                { be sure to change line !! }
                lastfileinfo.line:=-1;
              end;
           { write source }
             if (cs_asm_source in aktglobalswitches) and
                assigned(infile) then
              begin
                if (infile<>lastinfile) then
                  begin
                    AsmWriteLn(target_asm.comment+'['+infile.name^+']');
                    if assigned(lastinfile) then
                      lastinfile.close;
                  end;
                if (hp.fileinfo.line<>lastfileinfo.line) and
                   ((hp.fileinfo.line<infile.maxlinebuf) or (InlineLevel>0)) then
                  begin
                    if (hp.fileinfo.line<>0) and
                       ((infile.linebuf^[hp.fileinfo.line]>=0) or (InlineLevel>0)) then
                      AsmWriteLn(target_asm.comment+'['+tostr(hp.fileinfo.line)+'] '+
                        fixline(infile.GetLineStr(hp.fileinfo.line)));
                    { set it to a negative value !
                    to make that is has been read already !! PM }
                    if (infile.linebuf^[hp.fileinfo.line]>=0) then
                      infile.linebuf^[hp.fileinfo.line]:=-infile.linebuf^[hp.fileinfo.line]-1;
                  end;
              end;
             lastfileinfo:=hp.fileinfo;
             lastinfile:=infile;
           end;
         DoNotSplitLine:=false;
         case hp.typ of
            ait_comment:
              begin
                 AsmWrite(target_asm.comment);
                 AsmWritePChar(tai_comment(hp).str);
                 AsmLn;
              end;
            ait_regalloc,
            ait_tempalloc:
              ;
            ait_section:
              begin
                 {if LasTSec<>sec_none then
                  AsmWriteLn('_'+target_asm.secnames[LasTSec]+#9#9'ENDS');}
                 if tai_section(hp).sec<>sec_none then
                  begin
                    AsmLn;
                    AsmWriteLn(#9+target_asm.secnames[tai_section(hp).sec]){+#9#9+
                               'SEGMENT'#9'PARA PUBLIC USE32 '''+
                               target_asm.secnames[tai_section(hp).sec]+'''');}
                  end;
                 LasTSec:=tai_section(hp).sec;
               end;
            ait_align:
              begin
                 case tai_align(hp).aligntype of
                   1:AsmWriteLn(#9'align 0');
                   2:AsmWriteLn(#9'align 1');
                   4:AsmWriteLn(#9'align 2');
                   otherwise internalerror(10000);
                 end;
              end;
            ait_datablock:
              begin
                 s:= tai_datablock(hp).sym.name;
                 replaced:= ReplaceForbiddenChars(s);
                 if tai_datablock(hp).is_global then
                   if replaced then
                     AsmWriteLn(#9'export'#9+s+' => '''+tai_datablock(hp).sym.name+'''')
                   else
                     AsmWriteLn(#9'export'#9+s);
                 AsmWriteLn(PadTabs(s,#0)+'DS.B '+tostr(tai_datablock(hp).size));
                 {TODO: ? PadTabs(s,#0) }
              end;
            ait_const_32bit,
            ait_const_8bit,
            ait_const_16bit :
              begin
                 AsmWrite(ait_const2str[hp.typ]+tostr(tai_const(hp).value));
                 consttyp:=hp.typ;
                 l:=0;
                 repeat
                   found:=(not (tai(hp.next)=nil)) and (tai(hp.next).typ=consttyp);
                   if found then
                    begin
                      hp:=tai(hp.next);
                      s:=','+tostr(tai_const(hp).value);
                      AsmWrite(s);
                      inc(l,length(s));
                    end;
                 until (not found) or (l>line_length);
                 AsmLn;
               end;
            ait_const_symbol:
              begin
                 AsmWriteLn(#9#9'dd'#9'offset '+tai_const_symbol(hp).sym.name);
                 if tai_const_symbol(hp).offset>0 then
                   AsmWrite('+'+tostr(tai_const_symbol(hp).offset))
                 else if tai_const_symbol(hp).offset<0 then
                   AsmWrite(tostr(tai_const_symbol(hp).offset));
                 AsmLn;
               end;
            ait_real_32bit:
              AsmWriteLn(#9'dc.l'#9'"'+single2str(tai_real_32bit(hp).value)+'"');
            ait_real_64bit:
              AsmWriteLn(#9'dc.d'#9'"'+double2str(tai_real_64bit(hp).value)+'"');
            ait_string:
              begin
                 {NOTE When a single quote char is encountered, it is
                 replaced with a numeric ascii value. It could also
                 have been replaced with the escape seq of double quotes.}
                 counter := 0;
                 lines := tai_string(hp).len div line_length;
                 { separate lines in different parts }
                 if tai_string(hp).len > 0 then
                  Begin
                    for j := 0 to lines-1 do
                     begin
                       AsmWrite(#9'dc.b'#9);
                       quoted:=false;
                       for i:=counter to counter+line_length do
                          begin
                            { it is an ascii character. }
                            if (ord(tai_string(hp).str[i])>31) and
                               (ord(tai_string(hp).str[i])<128) and
                               (tai_string(hp).str[i]<>'''') then
                                begin
                                  if not(quoted) then
                                      begin
                                        if i>counter then
                                          AsmWrite(',');
                                        AsmWrite('''');
                                      end;
                                  AsmWrite(tai_string(hp).str[i]);
                                  quoted:=true;
                                end { if > 31 and < 128 and ord('"') }
                            else
                                begin
                                    if quoted then
                                        AsmWrite('''');
                                    if i>counter then
                                        AsmWrite(',');
                                    quoted:=false;
                                    AsmWrite(tostr(ord(tai_string(hp).str[i])));
                                end;
                         end; { end for i:=0 to... }
                       if quoted then AsmWrite('''');
                         AsmWrite(target_info.newline);
                       counter := counter+line_length;
                    end; { end for j:=0 ... }
                  { do last line of lines }
                  AsmWrite(#9'dc.b'#9);
                  quoted:=false;
                  for i:=counter to tai_string(hp).len-1 do
                    begin
                      { it is an ascii character. }
                      if (ord(tai_string(hp).str[i])>31) and
                         (ord(tai_string(hp).str[i])<128) and
                         (tai_string(hp).str[i]<>'''') then
                          begin
                            if not(quoted) then
                                begin
                                  if i>counter then
                                    AsmWrite(',');
                                  AsmWrite('''');
                                end;
                            AsmWrite(tai_string(hp).str[i]);
                            quoted:=true;
                          end { if > 31 and < 128 and " }
                      else
                          begin
                            if quoted then
                              AsmWrite('''');
                            if i>counter then
                                AsmWrite(',');
                            quoted:=false;
                            AsmWrite(tostr(ord(tai_string(hp).str[i])));
                          end;
                    end; { end for i:=0 to... }
                  if quoted then
                    AsmWrite('''');
                  end;
                 AsmLn;
              end;
            ait_label:
              begin
                 if tai_label(hp).l.is_used then
                  begin
                    s:= tai_label(hp).l.name;
                    ReplaceForbiddenChars(s);
                    AsmWrite(s);
                    {if assigned(hp.next) and not(tai(hp.next).typ in
                       [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                        ait_const_symbol,ait_const_rva,
                        ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                     AsmWriteLn(':')
                    else
                     DoNotSplitLine:=true;}
                    AsmWriteLn(':');
                  end;
               end;
             ait_direct:
               begin
                  AsmWritePChar(tai_direct(hp).str);
                  AsmLn;
               end;
             ait_symbol:
               begin
                  s:= tai_label(hp).l.name;
                  replaced:= ReplaceForbiddenChars(s);
                  if tai_label(hp).l.typ=AT_FUNCTION then
                    begin
                       if replaced then
                         begin
                            AsmWriteLn(#9'export'#9'.'+s+'[PR] => ''.'+tai_symbol(hp).sym.name+'[PR]''');
                            AsmWriteLn(#9'export'#9+s+'[DS] => '''+tai_symbol(hp).sym.name+'[DS]''');
                         end
                       else
                         begin
                            AsmWriteLn(#9'export'#9'.'+s+'[PR]');
                            AsmWriteLn(#9'export'#9+s+'[DS]');
                         end;
                       {Entry in transition vector: }
                       AsmWriteLn(#9'csect'#9+s+'[DS]');
                       AsmWriteLn(#9'dc.l'#9'.'+s);
                       AsmWriteln(#9'dc.l'#9'TOC[tc0]');
                       {Entry in TOC: }
                       AsmWriteLn(#9'toc');
                       AsmWriteLn(#9'tc'#9+s+'[TC],'+s+'[DS]');
                       {Start the section of the body of the proc: }
                       AsmWriteLn(#9'csect'#9'.'+s+'[PR]');
                       AsmWrite('.');
                       AsmWrite(s);
                       AsmWriteLn(':');
                    end
                  else
                    begin
                       if tai_symbol(hp).is_global then
                         if replaced then
                           AsmWriteLn(#9'export'#9+s+' => '''+tai_symbol(hp).sym.name+'''')
                         else
                           AsmWriteLn(#9'export'#9+s);
                       AsmWrite(s);
                       AsmWriteLn(':');
                    end;
                end;
              ait_symbol_end:
                ;
              ait_instruction:
                AsmWriteLn(GetInstruction(hp));
{$ifdef GDB}
             ait_stabn,
             ait_stabs,
        ait_force_line,
ait_stab_function_name : ;
{$endif GDB}
           ait_cut : begin
                     { only reset buffer if nothing has changed }
                       if AsmSize=AsmStartSize then
                        AsmClear
                       else
                        begin
                          {
                          if LasTSec<>sec_none then
                           AsmWriteLn('_'+target_asm.secnames[LasTSec]+#9#9'ends');
                          AsmLn;
                          }
                          AsmWriteLn(#9'end');
                          AsmClose;
                          DoAssemble;
                          AsmCreate(tai_cut(hp).place);
                        end;
                     { avoid empty files }
                       while assigned(hp.next) and (tai(hp.next).typ in [ait_cut,ait_section,ait_comment]) do
                        begin
                          if tai(hp.next).typ=ait_section then
                           begin
                             lasTSec:=tai_section(hp.next).sec;
                           end;
                          hp:=tai(hp.next);
                        end;
                       WriteAsmFileHeader;

                       if lasTSec<>sec_none then
                         AsmWriteLn(#9+target_asm.secnames[lasTSec]);
                       {   AsmWriteLn('_'+target_asm.secnames[lasTSec]+#9#9+
                                     'SEGMENT'#9'PARA PUBLIC USE32 '''+
                                     target_asm.secnames[lasTSec]+'''');
                       }
                       AsmStartSize:=AsmSize;
                     end;
           ait_marker :
             begin
               if tai_marker(hp).kind=InlineStart then
                 inc(InlineLevel)
               else if tai_marker(hp).kind=InlineEnd then
                 dec(InlineLevel);
             end;
         else
          internalerror(10000);
         end;
         hp:=tai(hp.next);
       end;
    end;

    var
      currentasmlist : TExternalAssembler;

    procedure writeexternal(p:tnamedindexitem;arg:pointer);

      var
        s:string;

      begin
        if tasmsymbol(p).defbind=AB_EXTERNAL then
          begin
            {currentasmlist.AsmWriteln(#9'import'#9+p.name);}
            s:= p.name;
            case tasmsymbol(p).typ of
              AT_FUNCTION:
                begin
                   if ReplaceForbiddenChars(s) then
                     begin
                        currentasmlist.AsmWriteLn(#9'import'#9'.'+s+' <= ''.'+p.name+'[PR]''');
                        currentasmlist.AsmWriteLn(#9'import'#9+s+' <= '''+p.name+'[DS]''');
                     end
                   else
                     begin
                        currentasmlist.AsmWriteLn(#9'import'#9'.'+s+'[PR]');
                        currentasmlist.AsmWriteLn(#9'import'#9+s+'[DS]');
                     end;
                   currentasmlist.AsmWriteLn(#9'toc');
                   currentasmlist.AsmWriteLn(#9'tc'#9+s+'[TC],'+s+'[DS]');
                end
              else
                begin
                   if ReplaceForbiddenChars(s) then
                     currentasmlist.AsmWriteLn(#9'import'#9+s+' <= '''+p.name+'''')
                   else
                     currentasmlist.AsmWriteLn(#9'import'#9+s);
                end;
            end;
          end;
      end;

    procedure TPPCMPWAssembler.WriteExternals;
      begin
        currentasmlist:=self;
        objectlibrary.symbolsearch.foreach_static({$ifdef fpcprocvar}@{$endif}writeexternal,nil);
      end;


    function TPPCMPWAssembler.DoAssemble : boolean;
    var f : file;
    begin
      DoAssemble:=Inherited DoAssemble;
      (*
      { masm does not seem to recognize specific extensions and uses .obj allways PM }
      if (aktoutputformat = as_i386_masm) then
        begin
          if not(cs_asm_extern in aktglobalswitches) then
            begin
              if Not FileExists(objfile) and
                 FileExists(ForceExtension(objfile,'.obj')) then
                begin
                  Assign(F,ForceExtension(objfile,'.obj'));
                  Rename(F,objfile);
                end;
            end
          else
            AsmRes.AddAsmCommand('mv',ForceExtension(objfile,'.obj')+' '+objfile,objfile);
        end;
      *)
    end;

    procedure TPPCMPWAssembler.WriteAsmFileHeader;

    begin
      (*
      AsmWriteLn(#9'.386p');
      { masm 6.11 does not seem to like LOCALS PM }
      if (aktoutputformat = as_i386_tasm) then
        begin
          AsmWriteLn(#9'LOCALS '+target_asm.labelprefix);
        end;
      AsmWriteLn('DGROUP'#9'GROUP'#9'_BSS,_DATA');
      AsmWriteLn(#9'ASSUME'#9'CS:_CODE,ES:DGROUP,DS:DGROUP,SS:DGROUP');
      AsmLn;
      *)

      AsmWriteLn(#9'STRING ASIS'); {Interpret strings just to be the content between the quotes.}
      AsmLn;
    end;

    procedure TPPCMPWAssembler.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Start writing MPW-styled assembler output for '+current_module.mainsource^);
{$endif}
      LasTSec:=sec_none;

      WriteAsmFileHeader;
      WriteExternals;

    { PowerPC MPW ASM doesn't support stabs, as we now.
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(resourcestringlist);
      WriteTree(bsssegment);

      AsmWriteLn(#9'end');
      AsmLn;

{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Done writing MPW-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
   end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
       as_powerpc_mpw_info : tasminfo =
          (
            id           : as_powerpc_mpw;
            idtxt  : 'MPW';
            asmbin : 'PPCAsm';
            asmcmd : '';
            supported_target : system_any; { what should I write here ?? }
            outputbinary: false;
            allowdirect : true;
            needar : true;
            labelprefix_only_inside_procedure : true;
            labelprefix : '@@';
            comment : '; ';
            secnames : ('',
              'csect','csect','csect',  {TODO: Perhaps use other section types.}
              '','','','','','',
              '','','')
          );

initialization
  RegisterAssembler(as_powerpc_mpw_info,TPPCMPWAssembler);
end.
{
  $Log$
  Revision 1.10  2002-10-10 19:39:37  florian
    * changes from Olle to get simple programs compiled and assembled

  Revision 1.9  2002/10/07 21:19:53  florian
    * more mpw fixes

  Revision 1.8  2002/10/06 22:46:20  florian
    * fixed function exporting

  Revision 1.7  2002/10/02 22:14:15  florian
    * improve function imports

  Revision 1.6  2002/09/27 21:09:49  florian
    + readed because previous version was broken

  Revision 1.2  2002/08/31 12:43:31  florian
    * ppc compilation fixed

  Revision 1.1  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output
}

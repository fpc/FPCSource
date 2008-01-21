{
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
       aasmtai,aasmdata,
       globals,aasmbase,aasmcpu,assemble,
       cpubase;

    type
      TPPCMPWAssembler = class(TExternalAssembler)
        procedure WriteTree(p:TAsmList);override;
        procedure WriteAsmList;override;
        Function  DoAssemble:boolean;override;
        procedure WriteExternals;
        procedure WriteAsmFileHeader;
      private
        procedure WriteInstruction(hp : tai);
        procedure WriteProcedureHeader(var hp:tai);
        procedure WriteDataHeader(var s:string; isExported, isConst:boolean);
        cur_CSECT_name: String;
        cur_CSECT_class: String;
      end;


  implementation

    uses
      cutils,globtype,systems,cclasses,
      verbose,finput,fmodule,script,cpuinfo,
      cgbase,cgutils,
      itcpugas
      ;

    const
      line_length = 70;

      {Whether internal procedure references should be xxx[PR]: }
      use_PR = false;

      const_storage_class = '';
      var_storage_class = '';

      secnames : array[TAsmSectiontype] of string[10] = (
        '',      {none}
        'csect', {code}
        'csect', {data}
        'csect', {read only data}
        'csect', {read only data - no relocations}
        'csect', {bss} 'csect', '',
        'csect','csect','csect','csect',
         '','','','','','','','','','','','','',''
      );

    type
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;

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


{*** From here is copyed from agppcgas.pp, except where marked with CHANGED.
     Perhaps put in a third common file. ***}


    function getreferencestring(var ref : treference) : string;
    var
      s : string;
    begin
       with ref do
        begin
          if (refaddr <> addr_no) then
            InternalError(2002110301)
          else if ((offset < -32768) or (offset > 32767)) then
            InternalError(19991);


          if assigned(symbol) then
            begin
              s:= symbol.name;
              ReplaceForbiddenChars(s);
              {if symbol.typ = AT_FUNCTION then
                  ;}

              s:= s+'[TC]' {ref to TOC entry }
            end
          else
            s:= '';


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

          if (index=NR_NO) and (base<>NR_NO) then
            begin
              if offset=0 then
                if not assigned(symbol) then
                  s:=s+'0';
              s:=s+'('+gas_regname(base)+')';
            end
          else if (index<>NR_NO) and (base<>NR_NO) and (offset=0) then
            begin
              if (offset=0) then
                s:=s+gas_regname(base)+','+gas_regname(index)
              else
                internalerror(19992);
            end
          else if (base=NR_NO) and (offset=0) then
            begin
              {Temporary fix for inline asm, where a local var is referenced.}
              //if assigned(symbol) then
              //  s:= s+'(rtoc)';
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
        top_ref :
          begin
            if o.ref^.refaddr=addr_full then
              begin
                hs:=o.ref^.symbol.name;
                ReplaceForbiddenChars(hs);
                case o.ref^.symbol.typ of
                  AT_FUNCTION:
                    begin
                      if hs[1] <> '@' then {if not local label}
                        if use_PR then
                          hs:= '.'+hs+'[PR]'
                        else
                          hs:= '.'+hs
                    end
                  else
                    ;
                end;
                if o.ref^.offset>0 then
                 hs:=hs+'+'+tostr(o.ref^.offset)
                else
                 if o.ref^.offset<0 then
                  hs:=hs+tostr(o.ref^.offset);
                getopstr_jmp:=hs;
              end
            else
              internalerror(200402263);
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
          if o.ref^.refaddr=addr_no then
            getopstr:=getreferencestring(o.ref^)
          else
            begin
              hs:=o.ref^.symbol.name;
              ReplaceForbiddenChars(hs);
              if o.ref^.offset>0 then
               hs:=hs+'+'+tostr(o.ref^.offset)
              else
               if o.ref^.offset<0 then
                hs:=hs+tostr(o.ref^.offset);
              getopstr:=hs;
            end;
        else
          internalerror(2002070604);
      end;
    end;

    type
      topstr = string[4];

    function branchmode(o: tasmop): topstr;
      var tempstr: topstr;
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
    var
      tempstr: string;
    begin
      tempstr:=#9;
      case c.simple of
        false:
          begin
            cond2str := tempstr+gas_op2str[op];
            case c.dirhint of
              DH_None:;
              DH_Minus:
                cond2str:=cond2str+'-';
              DH_Plus:
                cond2str:=cond2str+'+';
              else
                internalerror(2003112901);
            end;
            cond2str:=cond2str+#9+tostr(c.bo)+','+tostr(c.bi)+',';
          end;
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
                              branchmode(op);
                  case c.dirhint of
                    DH_None:
                      tempstr:=tempstr+#9;
                    DH_Minus:
                      tempstr:=tempstr+('-'+#9);
                    DH_Plus:
                      tempstr:=tempstr+('+'+#9);
                    else
                      internalerror(2003112901);
                  end;
                  case c.cond of
                    C_LT..C_NU:
                      cond2str := tempstr+gas_regname(newreg(R_SPECIALREGISTER,c.cr,R_SUBWHOLE));
                    C_T,C_F,C_DNZT,C_DNZF,C_DZT,C_DZF:
                      cond2str := tempstr+tostr(c.crbit);
                    else
                      cond2str := tempstr;
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

    procedure TPPCMPWAssembler.WriteInstruction(hp : tai);
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
             A_B,A_BA:
               s:=#9+gas_op2str[op]+#9;
             A_BCTR,A_BCTRL,A_BLR,A_BLRL:
               s:=#9+gas_op2str[op];
             A_BL,A_BLA:
               s:=#9+gas_op2str[op]+#9;
             else
               begin
                 s:=cond2str(op,taicpu(hp).condition);
                 if (s[length(s)] <> #9) and
                    (taicpu(hp).ops>0) then
                   s := s + ',';
               end;
          end;
          if (taicpu(hp).ops>0) and (taicpu(hp).oper[0]^.typ<>top_none) then
            begin
              { first write the current contents of s, because the symbol }
              { may be 255 characters                                     }
              asmwrite(s);
              s:=getopstr_jmp(taicpu(hp).oper[0]^);
            end;
        end
      else
        { process operands }
        begin
          s:=#9+gas_op2str[op];
          if taicpu(hp).ops<>0 then
            begin
               sep:=#9;
               for i:=0 to taicpu(hp).ops-1 do
                 begin
                   s:=s+sep+getopstr(taicpu(hp).oper[i]^);
                   sep:=',';
                 end;
            end;
        end;
      AsmWriteLn(s);
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

  { convert floating point values }
  { to correct endian             }
  procedure swap64bitarray(var t: t64bitarray);
    var
     b: byte;
    begin
      b:= t[7];
      t[7] := t[0];
      t[0] := b;

      b := t[6];
      t[6] := t[1];
      t[1] := b;

      b:= t[5];
      t[5] := t[2];
      t[2] := b;

      b:= t[4];
      t[4] := t[3];
      t[3] := b;
   end;

   procedure swap32bitarray(var t: t32bitarray);
    var
     b: byte;
    begin
      b:= t[1];
      t[1]:= t[2];
      t[2]:= b;

      b:= t[0];
      t[0]:= t[3];
      t[3]:= b;
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

{****************************************************************************
                               PowerPC MPW Assembler
 ****************************************************************************}
    procedure TPPCMPWAssembler.WriteProcedureHeader(var hp:tai);
      {Returns the current hp where the caller should continue from}
      {For multiple entry procedures, only the last is exported as xxx[PR]
       (if use_PR is set) }

      procedure WriteExportHeader(hp:tai);

        var
          s: string;
          replaced: boolean;

      begin
        s:= tai_symbol(hp).sym.name;
        replaced:= ReplaceForbiddenChars(s);

        if not use_PR then
          begin
            AsmWrite(#9'export'#9'.');
            AsmWrite(s);
            if replaced then
              begin
                AsmWrite(' => ''.');
                AsmWrite(tai_symbol(hp).sym.name);
                AsmWrite('''');
              end;
            AsmLn;
          end;

        AsmWrite(#9'export'#9);
        AsmWrite(s);
        AsmWrite('[DS]');
        if replaced then
          begin
            AsmWrite(' => ''');
            AsmWrite(tai_symbol(hp).sym.name);
            AsmWrite('[DS]''');
          end;
        AsmLn;

        {Entry in transition vector: }
        AsmWrite(#9'csect'#9); AsmWrite(s); AsmWriteLn('[DS]');

        AsmWrite(#9'dc.l'#9'.'); AsmWriteLn(s);

        AsmWriteln(#9'dc.l'#9'TOC[tc0]');

        {Entry in TOC: }
        AsmWriteLn(#9'toc');

        AsmWrite(#9'tc'#9);
        AsmWrite(s); AsmWrite('[TC],');
        AsmWrite(s); AsmWriteln('[DS]');
      end;

    function GetAdjacentTaiSymbol(var hp:tai):Boolean;

    begin
      GetAdjacentTaiSymbol:= false;
      while assigned(hp.next) do
        case tai(hp.next).typ of
          ait_symbol:
            begin
              hp:=tai(hp.next);
              GetAdjacentTaiSymbol:= true;
              Break;
            end;
          ait_function_name:
            hp:=tai(hp.next);
          else
            begin
              //AsmWriteln('  ;#*#*# ' + tostr(Ord(tai(hp.next).typ)));
              Break;
            end;
        end;
    end;

    var
      first,last: tai;
      s: string;
      replaced: boolean;


    begin
      s:= tai_symbol(hp).sym.name;
      {Write all headers}
      first:= hp;
      repeat
        WriteExportHeader(hp);
        last:= hp;
      until not GetAdjacentTaiSymbol(hp);

      {Start the section of the body of the proc: }
      s:= tai_symbol(last).sym.name;
      replaced:= ReplaceForbiddenChars(s);

      if use_PR then
        begin
          AsmWrite(#9'export'#9'.'); AsmWrite(s); AsmWrite('[PR]');
          if replaced then
            begin
              AsmWrite(' => ''.');
              AsmWrite(tai_symbol(last).sym.name);
              AsmWrite('[PR]''');
            end;
          AsmLn;
        end;

      {Starts the section: }
      AsmWrite(#9'csect'#9'.');
      AsmWrite(s);
      AsmWriteLn('[PR]');

      {Info for the debugger: }
      AsmWrite(#9'function'#9'.');
      AsmWrite(s);
      AsmWriteLn('[PR]');

      {Write all labels: }
      hp:= first;
      repeat
        s:= tai_symbol(hp).sym.name;
        ReplaceForbiddenChars(s);
        AsmWrite('.'); AsmWrite(s); AsmWriteLn(':');
      until not GetAdjacentTaiSymbol(hp);
    end;

    procedure TPPCMPWAssembler.WriteDataHeader(var s:string; isExported, isConst:boolean);
    // Returns in s the changed string
    var
      sym: string;
      replaced: boolean;

    begin
      sym:= s;
      replaced:= ReplaceForbiddenChars(s);

      if isExported then
        begin
          AsmWrite(#9'export'#9);
          AsmWrite(s);
          if isConst then
            AsmWrite(const_storage_class)
          else
            AsmWrite(var_storage_class);
          if replaced then
              begin
                AsmWrite(' => ''');
                AsmWrite(sym);
                AsmWrite('''');
              end;
          AsmLn;
        end;

      if not macos_direct_globals then
        begin
          {The actual section is here interrupted, by inserting a "tc" entry}
          AsmWriteLn(#9'toc');

          AsmWrite(#9'tc'#9);
          AsmWrite(s);
          AsmWrite('[TC], ');
          AsmWrite(s);
          if isConst then
            AsmWrite(const_storage_class)
          else
            AsmWrite(var_storage_class);
          AsmLn;

          {The interrupted section is here continued.}
          AsmWrite(#9'csect'#9);
          AsmWriteln(cur_CSECT_name+cur_CSECT_class);
          AsmWrite(PadTabs(s+':',#0));
        end
      else
        begin
          AsmWrite(#9'csect'#9);
          AsmWrite(s);
          AsmWrite('[TC]');
        end;

      AsmLn;
    end;

    const
      ait_const2str:array[aitconst_32bit..aitconst_8bit] of string[8]=
        (#9'dc.l'#9,#9'dc.w'#9,#9'dc.b'#9);


    procedure TPPCMPWAssembler.WriteTree(p:TAsmList);
    var
      s        : string;
      hp       : tai;
      hp1      : tailineinfo;
      counter,
      lines,
      InlineLevel : longint;
      i,j,l    : longint;
      consttype : taiconst_type;
      do_line,DoNotSplitLine,
      quoted   : boolean;
      sin      : single;
      d        : double;

    begin
      if not assigned(p) then
       exit;
      InlineLevel:=0;
      { lineinfo is only needed for al_procedures (PFV) }
      do_line:=((cs_asm_source in current_settings.globalswitches) or
                (cs_lineinfo in current_settings.moduleswitches))
                 and (p=current_asmdata.asmlists[al_procedures]);
      DoNotSplitLine:=false;
      hp:=tai(p.first);
      while assigned(hp) do
       begin
         if not(hp.typ in SkipLineInfo) and
            not DoNotSplitLine then
           begin
             hp1 := hp as tailineinfo;

             if do_line then
              begin
           { load infile }
             if lastfileinfo.fileindex<>hp1.fileinfo.fileindex then
              begin
                infile:=current_module.sourcefiles.get_file(hp1.fileinfo.fileindex);
                if assigned(infile) then
                 begin
                   { open only if needed !! }
                   if (cs_asm_source in current_settings.globalswitches) then
                    infile.open;
                 end;
                { avoid unnecessary reopens of the same file !! }
                lastfileinfo.fileindex:=hp1.fileinfo.fileindex;
                { be sure to change line !! }
                lastfileinfo.line:=-1;
              end;
           { write source }
             if (cs_asm_source in current_settings.globalswitches) and
                assigned(infile) then
              begin
                if (infile<>lastinfile) then
                  begin
                    AsmWriteLn(target_asm.comment+'['+infile.name^+']');
                    if assigned(lastinfile) then
                      lastinfile.close;
                  end;
                if (hp1.fileinfo.line<>lastfileinfo.line) and
                   ((hp1.fileinfo.line<infile.maxlinebuf) or (InlineLevel>0)) then
                  begin
                    if (hp1.fileinfo.line<>0) and
                       ((infile.linebuf^[hp1.fileinfo.line]>=0) or (InlineLevel>0)) then
                      AsmWriteLn(target_asm.comment+'['+tostr(hp1.fileinfo.line)+'] '+
                        fixline(infile.GetLineStr(hp1.fileinfo.line)));
                    { set it to a negative value !
                    to make that is has been read already !! PM }
                    if (infile.linebuf^[hp1.fileinfo.line]>=0) then
                      infile.linebuf^[hp1.fileinfo.line]:=-infile.linebuf^[hp1.fileinfo.line]-1;
                  end;
              end;
             lastfileinfo:=hp1.fileinfo;
             lastinfile:=infile;
           end;
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
                 {if LastSecType<>sec_none then
                  AsmWriteLn('_'+target_asm.secnames[LastSecType]+#9#9'ENDS');}

                 if tai_section(hp).sectype<>sec_none then
                  begin
                    if tai_section(hp).sectype in [sec_data,sec_rodata,sec_bss] then
                      cur_CSECT_class:= '[RW]'
                    else if tai_section(hp).sectype in [sec_code] then
                      cur_CSECT_class:= ''
                    else
                      cur_CSECT_class:= '[RO]';

                    s:= tai_section(hp).name^;
                    if s = '' then
                      InternalError(2004101001);    {Nameless sections should not occur on MPW}
                    ReplaceForbiddenChars(s);
                    cur_CSECT_name:= s;

                    AsmLn;
                    AsmWriteLn(#9+secnames[tai_section(hp).sectype]+' '+cur_CSECT_name+cur_CSECT_class);
                  end;
                 LastSecType:=tai_section(hp).sectype;
               end;
            ait_align:
              begin
                 case tai_align(hp).aligntype of
                   1:AsmWriteLn(#9'align 0');
                   2:AsmWriteLn(#9'align 1');
                   4:AsmWriteLn(#9'align 2');
                   otherwise internalerror(2002110302);
                 end;
              end;
            ait_datablock: {Storage for global variables.}
              begin
                 s:= tai_datablock(hp).sym.name;

                 WriteDataHeader(s, tai_datablock(hp).is_global, false);
                 if not macos_direct_globals then
                   begin
                     AsmWriteLn(#9'ds.b '+tostr(tai_datablock(hp).size));
                   end
                 else
                   begin
                     AsmWriteLn(PadTabs(s+':',#0)+'ds.b '+tostr(tai_datablock(hp).size));
                     {TODO: ? PadTabs(s,#0) }
                   end;
              end;

            ait_const:
              begin
                consttype:=tai_const(hp).consttype;
                case consttype of
                   aitconst_128bit:
                      begin
                        internalerror(200404291);
                      end;
                   aitconst_64bit:
                      begin
                        if assigned(tai_const(hp).sym) then
                          internalerror(200404292);
                        AsmWrite(ait_const2str[aitconst_32bit]);
                        if target_info.endian = endian_little then
                          begin
                            AsmWrite(tostr(longint(lo(tai_const(hp).value))));
                            AsmWrite(',');
                            AsmWrite(tostr(longint(hi(tai_const(hp).value))));
                          end
                        else
                          begin
                            AsmWrite(tostr(longint(hi(tai_const(hp).value))));
                            AsmWrite(',');
                            AsmWrite(tostr(longint(lo(tai_const(hp).value))));
                          end;
                        AsmLn;
                      end;

                   aitconst_uleb128bit,
                   aitconst_sleb128bit,
                   aitconst_32bit,
                   aitconst_16bit,
                   aitconst_8bit,
                   aitconst_rva_symbol,
                   aitconst_indirect_symbol :
                     begin
                       AsmWrite(ait_const2str[consttype]);
                       l:=0;
                       repeat
                         if assigned(tai_const(hp).sym) then
                           begin
                             if assigned(tai_const(hp).endsym) then
                               begin
                                 if (tai_const(hp).endsym.typ = AT_FUNCTION) and use_PR then
                                   AsmWrite('.');

                                 s:=tai_const(hp).endsym.name;
                                 ReplaceForbiddenChars(s);
                                 AsmWrite(s);
                                 inc(l,length(s));

                                 if tai_const(hp).endsym.typ = AT_FUNCTION then
                                   begin
                                     if use_PR then
                                       AsmWrite('[PR]')
                                     else
                                       AsmWrite('[DS]');
                                   end;

                                 AsmWrite('-');
                                 inc(l,5); {Approx 5 extra, no need to be exactly}
                               end;

                             if (tai_const(hp).sym.typ = AT_FUNCTION) and use_PR then
                               AsmWrite('.');

                             s:= tai_const(hp).sym.name;
                             ReplaceForbiddenChars(s);
                             AsmWrite(s);
                             inc(l,length(s));

                             if tai_const(hp).sym.typ = AT_FUNCTION then
                               begin
                                 if use_PR then
                                   AsmWrite('[PR]')
                                 else
                                   AsmWrite('[DS]');
                               end;
                             inc(l,5); {Approx 5 extra, no need to be exactly}

                             if tai_const(hp).value > 0 then
                               s:= '+'+tostr(tai_const(hp).value)
                             else if tai_const(hp).value < 0 then
                               s:= '-'+tostr(tai_const(hp).value)
                             else
                               s:= '';
                             if s<>'' then
                               begin
                                 AsmWrite(s);
                                 inc(l,length(s));
                               end;
                           end
                         else
                           begin
                             s:= tostr(tai_const(hp).value);
                             AsmWrite(s);
                             inc(l,length(s));
                           end;

                         if (l>line_length) or
                            (hp.next=nil) or
                            (tai(hp.next).typ<>ait_const) or
                            (tai_const(hp.next).consttype<>consttype) then
                           break;
                         hp:=tai(hp.next);
                         AsmWrite(',');
                       until false;
                       AsmLn;
                     end;
                end;
              end;

            ait_real_64bit :
              begin
                AsmWriteLn(target_asm.comment+'value: '+double2str(tai_real_64bit(hp).value));
                d:=tai_real_64bit(hp).value;
                { swap the values to correct endian if required }
                if source_info.endian <> target_info.endian then
                  swap64bitarray(t64bitarray(d));
                AsmWrite(#9'dc.b'#9);
                  begin
                    for i:=0 to 7 do
                      begin
                        if i<>0 then
                          AsmWrite(',');
                        AsmWrite(tostr(t64bitarray(d)[i]));
                      end;
                  end;
                AsmLn;
              end;

            ait_real_32bit :
              begin
                AsmWriteLn(target_asm.comment+'value: '+single2str(tai_real_32bit(hp).value));
                sin:=tai_real_32bit(hp).value;
                { swap the values to correct endian if required }
                if source_info.endian <> target_info.endian then
                  swap32bitarray(t32bitarray(sin));
                AsmWrite(#9'dc.b'#9);
                for i:=0 to 3 do
                  begin
                    if i<>0 then
                      AsmWrite(',');
                    AsmWrite(tostr(t32bitarray(sin)[i]));
                  end;
                AsmLn;
              end;

            ait_string:
              begin
                {NOTE When a single quote char is encountered, it is
                replaced with a numeric ascii value. It could also
                have been replaced with the escape seq of double quotes.
                Backslash seems to be used as an escape char, although
                this is not mentioned in the PPCAsm documentation.}
                counter := 0;
                lines := tai_string(hp).len div line_length;
                { separate lines in different parts }
                if tai_string(hp).len > 0 then
                  begin
                    for j := 0 to lines-1 do
                      begin
                        AsmWrite(#9'dc.b'#9);
                        quoted:=false;
                        for i:=counter to counter+line_length-1 do
                          begin
                            { it is an ascii character. }
                            if (ord(tai_string(hp).str[i])>31) and
                               (ord(tai_string(hp).str[i])<128) and
                               (tai_string(hp).str[i]<>'''') and
                               (tai_string(hp).str[i]<>'\') then
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
                        AsmLn;
                        counter := counter+line_length;
                      end; { end for j:=0 ... }

                  { do last line of lines }
                  if counter < tai_string(hp).len then
                    AsmWrite(#9'dc.b'#9);
                  quoted:=false;
                  for i:=counter to tai_string(hp).len-1 do
                    begin
                      { it is an ascii character. }
                      if (ord(tai_string(hp).str[i])>31) and
                         (ord(tai_string(hp).str[i])<128) and
                         (tai_string(hp).str[i]<>'''') and
                         (tai_string(hp).str[i]<>'\') then
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
                 if tai_label(hp).labsym.is_used then
                  begin
                    s:= tai_label(hp).labsym.name;
                    if s[1] = '@' then
                      begin
                        ReplaceForbiddenChars(s);
                        //Local labels:
                        AsmWriteLn(s+':')
                      end
                    else
                      begin
                        //Procedure entry points:
                        if not macos_direct_globals then
                          begin
                            WriteDataHeader(s, tai_label(hp).is_global, true);
                          end
                        else
                          begin
                            ReplaceForbiddenChars(s);
                            AsmWrite(#9'csect'#9); AsmWrite(s);
                            AsmWriteLn('[TC]');

                            AsmWriteLn(PadTabs(s+':',#0));
                          end;
                      end;
                  end;
               end;
             ait_symbol:
               begin
                  if tai_symbol(hp).sym.typ=AT_FUNCTION then
                    WriteProcedureHeader(hp)
                  else if tai_symbol(hp).sym.typ=AT_DATA then
                    begin
                       s:= tai_symbol(hp).sym.name;
                       WriteDataHeader(s, tai_symbol(hp).is_global, true);
                       if macos_direct_globals then
                         begin
                           AsmWrite(s);
                           AsmWriteLn(':');
                         end;
                    end
                  else
                    InternalError(2003071301);
                end;
              ait_symbol_end:
                ;
              ait_instruction:
                WriteInstruction(hp);
              ait_stab,
              ait_force_line,
              ait_function_name : ;
              ait_cutobject :
                begin
                  InternalError(2004101101);  {Smart linking is done transparently by the MPW linker.}
                end;
              ait_marker :
                 begin
                   if tai_marker(hp).kind=mark_InlineStart then
                     inc(InlineLevel)
                   else if tai_marker(hp).kind=mark_InlineEnd then
                     dec(InlineLevel);
                 end;
         else
          internalerror(2002110303);
         end;
         hp:=tai(hp.next);
       end;
    end;

    var
      currentasmlist : TExternalAssembler;

    procedure writeexternal(p:tasmsymbol);

      var
        s:string;
        replaced: boolean;

      begin
        if tasmsymbol(p).bind=AB_EXTERNAL then
          begin
            //Writeln('ZZZ ',p.name,' ',p.classname,' ',Ord(tasmsymbol(p).typ));
            s:= p.name;
            replaced:= ReplaceForbiddenChars(s);

            with currentasmlist do
              case tasmsymbol(p).typ of
                AT_FUNCTION:
                  begin
                    AsmWrite(#9'import'#9'.');
                    AsmWrite(s);
                    if use_PR then
                     AsmWrite('[PR]');

                    if replaced then
                     begin
                       AsmWrite(' <= ''.');
                       AsmWrite(p.name);
                       if use_PR then
                         AsmWrite('[PR]''')
                       else
                         AsmWrite('''');
                     end;
                    AsmLn;

                    AsmWrite(#9'import'#9);
                    AsmWrite(s);
                    AsmWrite('[DS]');
                    if replaced then
                     begin
                       AsmWrite(' <= ''');
                       AsmWrite(p.name);
                       AsmWrite('[DS]''');
                     end;
                    AsmLn;

                    AsmWriteLn(#9'toc');

                    AsmWrite(#9'tc'#9);
                    AsmWrite(s);
                    AsmWrite('[TC],');
                    AsmWrite(s);
                    AsmWriteLn('[DS]');
                  end;
                AT_DATA:
                  begin
                    AsmWrite(#9'import'#9);
                    AsmWrite(s);
                    AsmWrite(var_storage_class);
                    if replaced then
                      begin
                        AsmWrite(' <= ''');
                        AsmWrite(p.name);
                        AsmWrite('''');
                      end;
                    AsmLn;

                    AsmWriteLn(#9'toc');
                    AsmWrite(#9'tc'#9);
                    AsmWrite(s);
                    AsmWrite('[TC],');
                    AsmWrite(s);
                    AsmWriteLn(var_storage_class);
                  end
                else
                  InternalError(2003090901);
              end;
          end;
      end;

    procedure TPPCMPWAssembler.WriteExternals;
      var
        i : longint;
      begin
        currentasmlist:=self;
//        current_asmdata.asmsymboldict.foreach_static(@writeexternal,nil);
        for i:=0 to current_asmdata.AsmSymbolDict.Count-1 do
          begin
            writeexternal(tasmsymbol(current_asmdata.AsmSymbolDict[i]));
          end;
     end;


    function TPPCMPWAssembler.DoAssemble : boolean;
    begin
      DoAssemble:=Inherited DoAssemble;
    end;

    procedure TPPCMPWAssembler.WriteAsmFileHeader;

    begin
      AsmWriteLn(#9'string asis');  {Interpret strings just to be the content between the quotes.}
      AsmWriteLn(#9'aligning off'); {We do our own aligning.}
      AsmLn;
    end;

    procedure TPPCMPWAssembler.WriteAsmList;
    var
      hal : tasmlisttype;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       comment(v_info,'Start writing MPW-styled assembler output for '+current_module.mainsource^);
{$endif}

      WriteAsmFileHeader;
      WriteExternals;

      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          AsmWriteLn(target_asm.comment+'Begin asmlist '+AsmListTypeStr[hal]);
          writetree(current_asmdata.asmlists[hal]);
          AsmWriteLn(target_asm.comment+'End asmlist '+AsmListTypeStr[hal]);
        end;

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
            asmcmd : '-case on $ASM -o $OBJ';
            supported_target : system_any; { what should I write here ?? }
            flags : [af_allowdirect,af_needar,af_smartlink_sections,af_labelprefix_only_inside_procedure];
            labelprefix : '@';
            comment : '; ';
          );

initialization
  RegisterAssembler(as_powerpc_mpw_info,TPPCMPWAssembler);
end.

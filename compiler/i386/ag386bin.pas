{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements an binary assembler output class

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
unit ag386bin;

{$i fpcdefs.inc}

{$define MULTIPASS}

interface

    uses
      cclasses,
      globals,
      cpubase,aasm,
      fmodule,finput,
      ogbase,assemble;

    type
      togtype=(og_none,og_dbg,og_coff,og_pecoff,og_elf);

      TInternalAssembler=class(TAssembler)
      public
        constructor create(t:togtype;smart:boolean);
        destructor  destroy;override;
        procedure WriteBin;
      private
        { the aasmoutput lists that need to be processed }
        lists        : byte;
        list         : array[1..maxoutputlists] of TAAsmoutput;
        { current processing }
        currlistidx  : byte;
        currlist     : TAAsmoutput;
        currpass     : byte;
{$ifdef GDB}
        n_line       : byte;     { different types of source lines }
        linecount,
        includecount : longint;
        funcname     : tasmsymbol;
        stabslastfileinfo : tfileposinfo;
        procedure convertstabs(p:pchar);
        procedure emitlineinfostabs(nidx,line : longint);
        procedure emitstabs(s:string);
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure StartFileLineInfo;
        procedure EndFileLineInfo;
{$endif}
        function  MaybeNextList(var hp:Tai):boolean;
        function  TreePass0(hp:Tai):Tai;
        function  TreePass1(hp:Tai):Tai;
        function  TreePass2(hp:Tai):Tai;
        procedure writetree;
        procedure writetreesmart;
      end;

implementation

    uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       cutils,globtype,systems,verbose,
       cpuasm,
{$ifdef GDB}
       gdb,
{$endif}
       { binary writers }
       ogcoff,ogelf
       ;



{$ifdef GDB}

    procedure TInternalAssembler.convertstabs(p:pchar);
      var
        ofs,
        nidx,nother,ii,i,line,j : longint;
        code : integer;
        hp : pchar;
        reloc : boolean;
        sec : tsection;
        ps : tasmsymbol;
        s : string;
      begin
        ofs:=0;
        reloc:=true;
        ps:=nil;
        sec:=sec_none;
        if p[0]='"' then
         begin
           i:=1;
           { we can have \" inside the string !! PM }
           while not ((p[i]='"') and (p[i-1]<>'\')) do
            inc(i);
           p[i]:=#0;
           ii:=i;
           hp:=@p[1];
           s:=StrPas(@P[i+2]);
         end
        else
         begin
           hp:=nil;
           s:=StrPas(P);
           i:=-2; {needed below (PM) }
         end;
      { When in pass 1 then only alloc and leave }
        if currpass=1 then
         begin
           objectalloc.staballoc(hp);
           if assigned(hp) then
            p[i]:='"';
           exit;
         end;
      { Parse the rest of the stabs }
        if s='' then
         internalerror(33000);
        j:=pos(',',s);
        if j=0 then
         internalerror(33001);
        Val(Copy(s,1,j-1),nidx,code);
        if code<>0 then
         internalerror(33002);
        i:=i+2+j;
        Delete(s,1,j);
        j:=pos(',',s);
        if (j=0) then
         internalerror(33003);
        Val(Copy(s,1,j-1),nother,code);
        if code<>0 then
         internalerror(33004);
        i:=i+j;
        Delete(s,1,j);
        j:=pos(',',s);
        if j=0 then
         begin
           j:=256;
           ofs:=-1;
         end;
        Val(Copy(s,1,j-1),line,code);
        if code<>0 then
          internalerror(33005);
        if ofs=0 then
          begin
            Delete(s,1,j);
            i:=i+j;
            Val(s,ofs,code);
            if code=0 then
              reloc:=false
            else
              begin
                ofs:=0;
                s:=strpas(@p[i]);
                { handle asmsymbol or
                    asmsymbol - asmsymbol }
                j:=pos(' ',s);
                if j=0 then
                  j:=pos('-',s);
                { single asmsymbol }
                if j=0 then
                  j:=256;
                { the symbol can be external
                  so we must use newasmsymbol and
                  not getasmsymbol !! PM }
                ps:=newasmsymbol(copy(s,1,j-1));
                if not assigned(ps) then
                  internalerror(33006)
                else
                  begin
                    sec:=ps.section;
                    ofs:=ps.address;
                    reloc:=true;
                    UsedAsmSymbolListInsert(ps);
                  end;
                if j<256 then
                  begin
                    i:=i+j;
                    s:=strpas(@p[i]);
                    if (s<>'') and (s[1]=' ') then
                      begin
                         j:=0;
                         while (s[j+1]=' ') do
                           inc(j);
                         i:=i+j;
                         s:=strpas(@p[i]);
                      end;
                    ps:=getasmsymbol(s);
                    if not assigned(ps) then
                      internalerror(33007)
                    else
                      begin
                        if ps.section<>sec then
                          internalerror(33008);
                        ofs:=ofs-ps.address;
                        reloc:=false;
                        UsedAsmSymbolListInsert(ps);
                      end;
                  end;
              end;
          end;
        { external bss need speical handling (PM) }
        if assigned(ps) and (ps.section=sec_none) then
          begin
            if currpass=2 then
              begin
                objectdata.writesymbol(ps);
                objectoutput.exportsymbol(ps);
              end;
            objectdata.WriteSymStabs(sec,ofs,hp,ps,nidx,nother,line,reloc)
          end
        else
          objectdata.WriteStabs(sec,ofs,hp,nidx,nother,line,reloc);
        if assigned(hp) then
         p[ii]:='"';
      end;


    procedure TInternalAssembler.emitlineinfostabs(nidx,line : longint);
      var
         sec : tsection;
      begin
        if currpass=1 then
          begin
            objectalloc.staballoc(nil);
            exit;
          end;

        if (nidx=n_textline) and assigned(funcname) and
           (target_os.use_function_relative_addresses) then
          objectdata.WriteStabs(sec_code,objectdata.sectionsize(sec_code)-funcname.address,
              nil,nidx,0,line,false)
        else
          begin
            if nidx=n_textline then
              sec:=sec_code
            else if nidx=n_dataline then
              sec:=sec_data
            else
              sec:=sec_bss;
            objectdata.WriteStabs(sec,objectdata.sectionsize(sec),
              nil,nidx,0,line,true);
          end;
      end;


    procedure TInternalAssembler.emitstabs(s:string);
      begin
        s:=s+#0;
        ConvertStabs(@s[1]);
      end;


    procedure TInternalAssembler.WriteFileLineInfo(var fileinfo : tfileposinfo);
      var
        curr_n : byte;
        hp : tasmsymbol;
        infile : tinputfile;
      begin
        if not ((cs_debuginfo in aktmoduleswitches) or
           (cs_gdb_lineinfo in aktglobalswitches)) then
         exit;
      { file changed ? (must be before line info) }
        if (fileinfo.fileindex<>0) and
           (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
         begin
           infile:=current_module.sourcefiles.get_file(fileinfo.fileindex);
           if includecount=0 then
            curr_n:=n_sourcefile
           else
            curr_n:=n_includefile;
           { get symbol for this includefile }
           hp:=newasmsymboltype('Ltext'+ToStr(IncludeCount),AB_LOCAL,AT_FUNCTION);
           if currpass=1 then
             begin
                hp.setaddress(objectalloc.currsec,objectalloc.sectionsize,0);
                UsedAsmSymbolListInsert(hp);
             end
           else
             objectdata.writesymbol(hp);
           { emit stabs }
           if (infile.path^<>'') then
             EmitStabs('"'+lower(BsToSlash(FixPath(infile.path^,false)))+'",'+tostr(curr_n)+
               ',0,0,Ltext'+ToStr(IncludeCount));
           EmitStabs('"'+lower(FixFileName(infile.name^))+'",'+tostr(curr_n)+
             ',0,0,Ltext'+ToStr(IncludeCount));
           inc(includecount);
         end;
      { line changed ? }
        if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
          emitlineinfostabs(n_line,fileinfo.line);
        stabslastfileinfo:=fileinfo;
      end;


    procedure TInternalAssembler.StartFileLineInfo;
      var
        fileinfo : tfileposinfo;
      begin
        FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
        n_line:=n_textline;
        funcname:=nil;
        linecount:=1;
        includecount:=0;
        fileinfo.fileindex:=1;
        fileinfo.line:=1;
        WriteFileLineInfo(fileinfo);
      end;


    procedure TInternalAssembler.EndFileLineInfo;
      var
        hp : tasmsymbol;
        store_sec : tsection;
      begin
          if not ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
           exit;
        store_sec:=objectalloc.currsec;
        objectalloc.setsection(sec_code);
        hp:=newasmsymboltype('Letext',AB_LOCAL,AT_FUNCTION);
        if currpass=1 then
          begin
            hp.setaddress(objectalloc.currsec,objectalloc.sectionsize,0);
            UsedAsmSymbolListInsert(hp);
          end
        else
          objectdata.writesymbol(hp);
        EmitStabs('"",'+tostr(n_sourcefile)+',0,0,Letext');
        objectalloc.setsection(store_sec);
      end;
{$endif GDB}


    function TInternalAssembler.MaybeNextList(var hp:Tai):boolean;
      begin
        { maybe end of list }
        while not assigned(hp) do
         begin
           if currlistidx<lists then
            begin
              inc(currlistidx);
              currlist:=list[currlistidx];
              hp:=Tai(currList.first);
            end
           else
            begin
              MaybeNextList:=false;
              exit;
            end;
         end;
        MaybeNextList:=true;
      end;


    function TInternalAssembler.TreePass0(hp:Tai):Tai;
      var
        l : longint;
      begin
        while assigned(hp) do
         begin
           case hp.typ of
             ait_align :
               begin
                 { always use the maximum fillsize in this pass to avoid possible
                   short jumps to become out of range }
                 Tai_align(hp).fillsize:=Tai_align(hp).aligntype;
                 objectalloc.sectionalloc(Tai_align(hp).fillsize);
               end;
             ait_datablock :
               begin
                 if not SmartAsm then
                  begin
                    if not Tai_datablock(hp).is_global then
                     begin
                        l:=Tai_datablock(hp).size;
                        if l>2 then
                          objectalloc.sectionalign(4)
                        else if l>1 then
                          objectalloc.sectionalign(2);
                        objectalloc.sectionalloc(Tai_datablock(hp).size);
                     end;
                  end
                 else
                  begin
                    l:=Tai_datablock(hp).size;
                    if l>2 then
                      objectalloc.sectionalign(4)
                    else if l>1 then
                      objectalloc.sectionalign(2);
                    objectalloc.sectionalloc(Tai_datablock(hp).size);
                  end;
               end;
             ait_const_32bit :
               objectalloc.sectionalloc(4);
             ait_const_16bit :
               objectalloc.sectionalloc(2);
             ait_const_8bit :
               objectalloc.sectionalloc(1);
             ait_real_80bit :
               objectalloc.sectionalloc(10);
             ait_real_64bit :
               objectalloc.sectionalloc(8);
             ait_real_32bit :
               objectalloc.sectionalloc(4);
             ait_comp_64bit :
               objectalloc.sectionalloc(8);
             ait_const_rva,
             ait_const_symbol :
               objectalloc.sectionalloc(4);
             ait_section:
               objectalloc.setsection(Tai_section(hp).sec);
             ait_symbol :
               Tai_symbol(hp).sym.setaddress(objectalloc.currsec,objectalloc.sectionsize,0);
             ait_label :
               Tai_label(hp).l.setaddress(objectalloc.currsec,objectalloc.sectionsize,0);
             ait_string :
               objectalloc.sectionalloc(Tai_string(hp).len);
             ait_instruction :
               begin
                 { reset instructions which could change in pass 2 }
                 Taicpu(hp).resetpass2;
                 objectalloc.sectionalloc(Taicpu(hp).Pass1(objectalloc.sectionsize));
               end;
             ait_cut :
               if SmartAsm then
                break;
           end;
           hp:=Tai(hp.next);
         end;
        TreePass0:=hp;
      end;


    function TInternalAssembler.TreePass1(hp:Tai):Tai;
      var
        i,l : longint;
      begin
        while assigned(hp) do
         begin
{$ifdef GDB}
           { write stabs }
          if ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
            begin
              if (objectalloc.currsec<>sec_none) and
                 not(hp.typ in  [
                     ait_label,
                     ait_regalloc,ait_tempalloc,
                     ait_stabn,ait_stabs,ait_section,
                     ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
               WriteFileLineInfo(hp.fileinfo);
            end;
{$endif GDB}
           case hp.typ of
             ait_align :
               begin
                 { here we must determine the fillsize which is used in pass2 }
                 Tai_align(hp).fillsize:=align(objectalloc.sectionsize,Tai_align(hp).aligntype)-
                   objectalloc.sectionsize;
                 objectalloc.sectionalloc(Tai_align(hp).fillsize);
               end;
             ait_datablock :
               begin
                 if objectalloc.currsec<>sec_bss then
                  Message(asmw_e_alloc_data_only_in_bss);
                 if not SmartAsm then
                  begin
                    if Tai_datablock(hp).is_global then
                     begin
                       Tai_datablock(hp).sym.setaddress(sec_none,Tai_datablock(hp).size,Tai_datablock(hp).size);
                       { force to be common/external, must be after setaddress as that would
                         set it to AS_GLOBAL }
                       Tai_datablock(hp).sym.bind:=AB_COMMON;
                     end
                    else
                     begin
                       l:=Tai_datablock(hp).size;
                       if l>2 then
                         objectalloc.sectionalign(4)
                       else if l>1 then
                         objectalloc.sectionalign(2);
                       Tai_datablock(hp).sym.setaddress(objectalloc.currsec,objectalloc.sectionsize,
                         Tai_datablock(hp).size);
                       objectalloc.sectionalloc(Tai_datablock(hp).size);
                     end;
                   end
                  else
                   begin
                     l:=Tai_datablock(hp).size;
                     if l>2 then
                       objectalloc.sectionalign(4)
                     else if l>1 then
                       objectalloc.sectionalign(2);
                     Tai_datablock(hp).sym.setaddress(objectalloc.currsec,objectalloc.sectionsize,Tai_datablock(hp).size);
                     objectalloc.sectionalloc(Tai_datablock(hp).size);
                   end;
                 UsedAsmSymbolListInsert(Tai_datablock(hp).sym);
               end;
             ait_const_32bit :
               objectalloc.sectionalloc(4);
             ait_const_16bit :
               objectalloc.sectionalloc(2);
             ait_const_8bit :
               objectalloc.sectionalloc(1);
             ait_real_80bit :
               objectalloc.sectionalloc(10);
             ait_real_64bit :
               objectalloc.sectionalloc(8);
             ait_real_32bit :
               objectalloc.sectionalloc(4);
             ait_comp_64bit :
               objectalloc.sectionalloc(8);
             ait_const_rva,
             ait_const_symbol :
               begin
                 objectalloc.sectionalloc(4);
                 UsedAsmSymbolListInsert(Tai_const_symbol(hp).sym);
               end;
             ait_section:
               begin
                 objectalloc.setsection(Tai_section(hp).sec);
{$ifdef GDB}
                 case Tai_section(hp).sec of
                  sec_code : n_line:=n_textline;
                  sec_data : n_line:=n_dataline;
                   sec_bss : n_line:=n_bssline;
                 else
                  n_line:=n_dataline;
                 end;
                 stabslastfileinfo.line:=-1;
{$endif GDB}
               end;
{$ifdef GDB}
             ait_stabn :
               convertstabs(Tai_stabn(hp).str);
             ait_stabs :
               convertstabs(Tai_stabs(hp).str);
             ait_stab_function_name :
               begin
                 if assigned(Tai_stab_function_name(hp).str) then
                  begin
                    funcname:=getasmsymbol(strpas(Tai_stab_function_name(hp).str));
                    UsedAsmSymbolListInsert(funcname);
                  end
                 else
                  funcname:=nil;
               end;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_symbol :
               begin
                 Tai_symbol(hp).sym.setaddress(objectalloc.currsec,objectalloc.sectionsize,0);
                 UsedAsmSymbolListInsert(Tai_symbol(hp).sym);
               end;
             ait_symbol_end :
               begin
                 if target_info.target=target_i386_linux then
                  begin
                    Tai_symbol(hp).sym.size:=objectalloc.sectionsize-Tai_symbol(hp).sym.address;
                    UsedAsmSymbolListInsert(Tai_symbol(hp).sym);
                  end;
                end;
             ait_label :
               begin
                 Tai_label(hp).l.setaddress(objectalloc.currsec,objectalloc.sectionsize,0);
                 UsedAsmSymbolListInsert(Tai_label(hp).l);
               end;
             ait_string :
               objectalloc.sectionalloc(Tai_string(hp).len);
             ait_instruction :
               begin
                 objectalloc.sectionalloc(Taicpu(hp).Pass1(objectalloc.sectionsize));
                 { fixup the references }
                 for i:=1 to Taicpu(hp).ops do
                  begin
                    with Taicpu(hp).oper[i-1] do
                     begin
                       case typ of
                         top_ref :
                           begin
                             if assigned(ref^.symbol) then
                              UsedAsmSymbolListInsert(ref^.symbol);
                           end;
                         top_symbol :
                           begin
                             if sym=nil then
                              sym:=sym;
                             UsedAsmSymbolListInsert(sym);
                           end;
                       end;
                     end;
                  end;
               end;
             ait_direct :
               Message(asmw_f_direct_not_supported);
             ait_cut :
               if SmartAsm then
                break;
           end;
           hp:=Tai(hp.next);
         end;
        TreePass1:=hp;
      end;


    function TInternalAssembler.TreePass2(hp:Tai):Tai;
      var
        l  : longint;
{$ifdef I386}
        co : comp;
{$endif I386}
      begin
        { main loop }
        while assigned(hp) do
         begin
{$ifdef GDB}
           { write stabs }
          if ((cs_debuginfo in aktmoduleswitches) or
             (cs_gdb_lineinfo in aktglobalswitches)) then
            begin
              if (objectdata.currsec<>sec_none) and
                 not(hp.typ in [
                     ait_label,
                     ait_regalloc,ait_tempalloc,
                     ait_stabn,ait_stabs,ait_section,
                     ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
               WriteFileLineInfo(hp.fileinfo);
            end;
{$endif GDB}
           case hp.typ of
             ait_align :
               objectdata.writebytes(Tai_align(hp).getfillbuf^,Tai_align(hp).fillsize);
             ait_section :
               begin
                 objectdata.defaultsection(Tai_section(hp).sec);
{$ifdef GDB}
                 case Tai_section(hp).sec of
                  sec_code : n_line:=n_textline;
                  sec_data : n_line:=n_dataline;
                   sec_bss : n_line:=n_bssline;
                 else
                  n_line:=n_dataline;
                 end;
                 stabslastfileinfo.line:=-1;
{$endif GDB}
               end;
             ait_symbol :
               begin
                 objectdata.writesymbol(Tai_symbol(hp).sym);
                 objectoutput.exportsymbol(Tai_symbol(hp).sym);
               end;
             ait_datablock :
               begin
                 objectdata.writesymbol(Tai_datablock(hp).sym);
                 objectoutput.exportsymbol(Tai_datablock(hp).sym);
                 if SmartAsm or (not Tai_datablock(hp).is_global) then
                   begin
                     l:=Tai_datablock(hp).size;
                     if l>2 then
                       objectdata.allocalign(4)
                     else if l>1 then
                       objectdata.allocalign(2);
                     objectdata.alloc(Tai_datablock(hp).size);
                   end;
               end;
             ait_const_32bit :
               objectdata.writebytes(Tai_const(hp).value,4);
             ait_const_16bit :
               objectdata.writebytes(Tai_const(hp).value,2);
             ait_const_8bit :
               objectdata.writebytes(Tai_const(hp).value,1);
             ait_real_80bit :
               objectdata.writebytes(Tai_real_80bit(hp).value,10);
             ait_real_64bit :
               objectdata.writebytes(Tai_real_64bit(hp).value,8);
             ait_real_32bit :
               objectdata.writebytes(Tai_real_32bit(hp).value,4);
             ait_comp_64bit :
               begin
{$ifdef FPC}
                 co:=comp(Tai_comp_64bit(hp).value);
{$else}
                 co:=Tai_comp_64bit(hp).value;
{$endif}
                 objectdata.writebytes(co,8);
               end;
             ait_string :
               objectdata.writebytes(Tai_string(hp).str^,Tai_string(hp).len);
             ait_const_rva :
               objectdata.writereloc(Tai_const_symbol(hp).offset,4,
                 Tai_const_symbol(hp).sym,relative_rva);
             ait_const_symbol :
               objectdata.writereloc(Tai_const_symbol(hp).offset,4,
                 Tai_const_symbol(hp).sym,relative_false);
             ait_label :
               begin
                 objectdata.writesymbol(Tai_label(hp).l);
                 { exporting shouldn't be necessary as labels are local,
                   but it's better to be on the safe side (PFV) }
                 objectoutput.exportsymbol(Tai_label(hp).l);
               end;
             ait_instruction :
               Taicpu(hp).Pass2;
{$ifdef GDB}
             ait_stabn :
               convertstabs(Tai_stabn(hp).str);
             ait_stabs :
               convertstabs(Tai_stabs(hp).str);
             ait_stab_function_name :
               if assigned(Tai_stab_function_name(hp).str) then
                 funcname:=getasmsymbol(strpas(Tai_stab_function_name(hp).str))
               else
                 funcname:=nil;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_cut :
               if SmartAsm then
                break;
           end;
           hp:=Tai(hp.next);
         end;
        TreePass2:=hp;
      end;


    procedure TInternalAssembler.writetree;
      var
        hp : Tai;
      label
        doexit;
      begin
        objectalloc.resetsections;
        objectalloc.setsection(sec_code);

        objectoutput.initwriting(ObjFile);
        objectdata:=objectoutput.data;
        objectdata.defaultsection(sec_code);
      { reset the asmsymbol list }
        CreateUsedAsmsymbolList;

{$ifdef MULTIPASS}
      { Pass 0 }
        currpass:=0;
        objectalloc.setsection(sec_code);
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
           hp:=TreePass0(hp);
           MaybeNextList(hp);
         end;
        { leave if errors have occured }
        if errorcount>0 then
         goto doexit;
{$endif}

      { Pass 1 }
        currpass:=1;
        objectalloc.resetsections;
        objectalloc.setsection(sec_code);
{$ifdef GDB}
        StartFileLineInfo;
{$endif GDB}
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
           hp:=TreePass1(hp);
           MaybeNextList(hp);
         end;
{$ifdef GDB}
        EndFileLineInfo;
{$endif GDB}
        { check for undefined labels and reset }
        UsedAsmSymbolListCheckUndefined;

        { set section sizes }
        objectdata.setsectionsizes(objectalloc.secsize);
        { leave if errors have occured }
        if errorcount>0 then
         goto doexit;

      { Pass 2 }
        currpass:=2;
{$ifdef GDB}
        StartFileLineInfo;
{$endif GDB}
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
           hp:=TreePass2(hp);
           MaybeNextList(hp);
         end;
{$ifdef GDB}
        EndFileLineInfo;
{$endif GDB}

        { leave if errors have occured }
        if errorcount>0 then
         goto doexit;

        { write last objectfile }
        objectoutput.donewriting;
        objectdata:=nil;

      doexit:
        { reset the used symbols back, must be after the .o has been
          written }
        UsedAsmsymbolListReset;
        DestroyUsedAsmsymbolList;
      end;


    procedure TInternalAssembler.writetreesmart;
      var
        hp : Tai;
        startsec : tsection;
        place: tcutplace;
      begin
        objectalloc.resetsections;
        objectalloc.setsection(sec_code);

        NextSmartName(cut_normal);
        objectoutput.initwriting(ObjFile);
        objectdata:=objectoutput.data;
        objectdata.defaultsection(sec_code);
        startsec:=sec_code;

        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=Tai(currList.first);
        while assigned(hp) do
         begin
         { reset the asmsymbol list }
           CreateUsedAsmSymbolList;

{$ifdef MULTIPASS}
         { Pass 0 }
           currpass:=0;
           objectalloc.resetsections;
           objectalloc.setsection(startsec);
           TreePass0(hp);
           { leave if errors have occured }
           if errorcount>0 then
            exit;
{$endif MULTIPASS}

         { Pass 1 }
           currpass:=1;
           objectalloc.resetsections;
           objectalloc.setsection(startsec);
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           TreePass1(hp);
{$ifdef GDB}
           EndFileLineInfo;
{$endif GDB}
           { check for undefined labels }
           UsedAsmSymbolListCheckUndefined;

           { set section sizes }
           objectdata.setsectionsizes(objectalloc.secsize);
           { leave if errors have occured }
           if errorcount>0 then
            exit;

         { Pass 2 }
           currpass:=2;
           objectdata.defaultsection(startsec);
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           hp:=TreePass2(hp);
{$ifdef GDB}
           EndFileLineInfo;
{$endif GDB}
           { leave if errors have occured }
           if errorcount>0 then
            exit;

           { if not end then write the current objectfile }
           objectoutput.donewriting;
           objectdata:=nil;

           { reset the used symbols back, must be after the .o has been
             written }
           UsedAsmsymbolListReset;
           DestroyUsedAsmsymbolList;

           { end of lists? }
           if not MaybeNextList(hp) then
            break;
           { save section for next loop }
           { this leads to a problem if startsec is sec_none !! PM }
           startsec:=objectalloc.currsec;

           { we will start a new objectfile so reset everything }
           { The place can still change in the next while loop, so don't init }
           { the writer yet (JM)                                              }
           if (hp.typ=ait_cut) then
            place := Tai_cut(hp).place
           else
            place := cut_normal;

           { avoid empty files }
           while assigned(hp.next) and
                 (Tai(hp.next).typ in [ait_marker,ait_comment,ait_section,ait_cut]) do
            begin
              if Tai(hp.next).typ=ait_section then
               startsec:=Tai_section(hp.next).sec
              else if (Tai(hp.next).typ=ait_cut) then
               place := Tai_cut(hp).place;
              hp:=Tai(hp.next);
            end;

           NextSmartName(place);
           objectoutput.initwriting(ObjFile);
           objectdata:=objectoutput.data;

           hp:=Tai(hp.next);

           { there is a problem if startsec is sec_none !! PM }
           if startsec=sec_none then
             startsec:=sec_code;

           if not MaybeNextList(hp) then
             break;
         end;
      end;


    procedure TInternalAssembler.writebin;

        procedure addlist(p:TAAsmoutput);
        begin
          inc(lists);
          list[lists]:=p;
        end;

      begin

        if cs_debuginfo in aktmoduleswitches then
          addlist(debuglist);
        addlist(codesegment);
        addlist(datasegment);
        addlist(consts);
        addlist(rttilist);
        if assigned(resourcestringlist) then
          addlist(resourcestringlist);
        addlist(bsssegment);
        if assigned(importssection) then
          addlist(importssection);
        if assigned(exportssection) and not UseDeffileForExport then
          addlist(exportssection);
        if assigned(resourcesection) then
          addlist(resourcesection);

        if SmartAsm then
          writetreesmart
        else
          writetree;
      end;


    constructor TInternalAssembler.create(t:togtype;smart:boolean);
      begin
        inherited create(smart);
        case t of
          og_none :
            Message(asmw_f_no_binary_writer_selected);
          og_coff :
            objectoutput:=tcoffobjectoutput.createdjgpp(smart);
          og_pecoff :
            objectoutput:=tcoffobjectoutput.createwin32(smart);
          og_elf :
            objectoutput:=telf32objectoutput.create(smart);
          else
            internalerror(43243432);
        end;
        objectalloc:=tobjectalloc.create;
        SmartAsm:=smart;
        currpass:=0;
      end;


   destructor TInternalAssembler.destroy;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
      begin
{$ifdef MEMDEBUG}
         d.init('agbin');
{$endif}
        objectoutput.free;
        objectalloc.free;
{$ifdef MEMDEBUG}
         d.free;
{$endif}
      end;

end.
{
  $Log$
  Revision 1.10  2002-05-16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.8  2002/04/15 19:12:10  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.7  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.6  2001/03/11 22:58:51  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.5  2001/03/05 21:39:11  peter
    * changed to class with common TAssembler also for internal assembler

  Revision 1.4  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.3  2000/12/23 19:59:35  peter
    * object to class for ow/og objects
    * split objectdata from objectoutput

  Revision 1.2  2000/12/12 19:50:21  peter
    * clear usedasmsymbol at exit of writetree

  Revision 1.1  2000/11/30 22:18:48  florian
    * moved to i386

  Revision 1.9  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.8  2000/09/24 15:06:10  peter
    * use defines.inc

  Revision 1.7  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/12 15:34:22  peter
    + usedasmsymbollist to check and reset only the used symbols (merged)

  Revision 1.5  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

  Revision 1.4  2000/08/04 22:00:50  peter
    * merges from fixes

  Revision 1.3  2000/07/13 12:08:24  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:29  michael
  + removed logs

}

{
    $Id$
    Copyright (c) 1996-98 by the FPC development team

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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ag386bin;

{$define MULTIPASS}
{$define EXTERNALBSS}

  interface

    uses
       cpubase,cobjects,aasm,files,assemble;

    type
      togtype=(og_none,og_dbg,og_coff,og_pecoff);

      pi386binasmlist=^ti386binasmlist;
      ti386binasmlist=object
        SmartAsm : boolean;
        constructor init(t:togtype;smart:boolean);
        destructor  done;
        procedure WriteBin;
      private
        { the aasmoutput lists that need to be processed }
        lists        : byte;
        list         : array[1..maxoutputlists] of paasmoutput;
        { current processing }
        currlistidx  : byte;
        currlist     : paasmoutput;
        currpass     : byte;
{$ifdef GDB}
        n_line       : byte;     { different types of source lines }
        linecount,
        includecount : longint;
        funcname     : pasmsymbol;
        stabslastfileinfo : tfileposinfo;
        procedure convertstabs(p:pchar);
{$ifdef unused}
        procedure emitsymbolstabs(s : string;nidx,nother,line : longint;firstasm,secondasm : pasmsymbol);
{$endif}
        procedure emitlineinfostabs(nidx,line : longint);
        procedure emitstabs(s:string);
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
        procedure StartFileLineInfo;
{$endif}
        function  MaybeNextList(var hp:pai):boolean;
        function  TreePass0(hp:pai):pai;
        function  TreePass1(hp:pai):pai;
        function  TreePass2(hp:pai):pai;
        procedure writetree;
        procedure writetreesmart;
      end;

  implementation

    uses
       strings,
       globtype,globals,systems,verbose,
       cpuasm,
{$ifdef GDB}
       gdb,
{$endif}
       og386,og386dbg,og386cff;

{$ifdef GDB}

    procedure ti386binasmlist.convertstabs(p:pchar);
      var
        ofs,
        nidx,nother,ii,i,line,j : longint;
        code : integer;
        hp : pchar;
        reloc : boolean;
        sec : tsection;
        ps : pasmsymbol;
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
           objectalloc^.staballoc(hp);
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
                    sec:=ps^.section;
                    ofs:=ps^.address;
                    reloc:=true;
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
                        if ps^.section<>sec then
                          internalerror(33008);
                        ofs:=ofs-ps^.address;
                        reloc:=false;
                      end;
                  end;
              end;
          end;
        { external bss need speical handling (PM) }
        if assigned(ps) and (ps^.section=sec_none) then
          objectoutput^.WriteSymStabs(sec,ofs,hp,ps,nidx,nother,line,reloc)
        else
          objectoutput^.WriteStabs(sec,ofs,hp,nidx,nother,line,reloc);
        if assigned(hp) then
         p[ii]:='"';
      end;


{$ifdef unused}
    procedure ti386binasmlist.emitsymbolstabs(s : string;nidx,nother,line : longint;
                firstasm,secondasm : pasmsymbol);
      var
         hp : pchar;
      begin
        if s='' then
          hp:=nil
        else
          begin
            s:=s+#0;
            hp:=@s[1];
          end;
        if not assigned(secondasm) then
          begin
            if not assigned(firstasm) then
              internalerror(33009);
            objectoutput^.WriteStabs(firstasm^.section,firstasm^.address,hp,nidx,nother,line,true);
          end
        else
          begin
            if firstasm^.section<>secondasm^.section then
              internalerror(33010);
            objectoutput^.WriteStabs(firstasm^.section,firstasm^.address-secondasm^.address,
              hp,nidx,nother,line,false);
          end;
      end;
{$endif}


    procedure ti386binasmlist.emitlineinfostabs(nidx,line : longint);
      var
         sec : tsection;
      begin
        if currpass=1 then
          begin
            objectalloc^.staballoc(nil);
            exit;
          end;

        if (nidx=n_textline) and assigned(funcname) and
           (target_os.use_function_relative_addresses) then
          objectoutput^.WriteStabs(sec_code,pgenericcoffoutput(objectoutput)^.sects[sec_code]^.len-funcname^.address,
              nil,nidx,0,line,false)
        else
          begin
            if nidx=n_textline then
              sec:=sec_code
            else if nidx=n_dataline then
              sec:=sec_data
            else
              sec:=sec_bss;
            objectoutput^.WriteStabs(sec,pgenericcoffoutput(objectoutput)^.sects[sec]^.len,
              nil,nidx,0,line,true);
          end;
      end;

    procedure ti386binasmlist.emitstabs(s:string);
      begin
        s:=s+#0;
        ConvertStabs(@s[1]);
      end;


    procedure ti386binasmlist.WriteFileLineInfo(var fileinfo : tfileposinfo);
      var
        curr_n : byte;
        hp : pasmsymbol;
        infile : pinputfile;
      begin
        if not (cs_debuginfo in aktmoduleswitches) then
         exit;
      { file changed ? (must be before line info) }
        if (fileinfo.fileindex<>0) and
           (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
         begin
           infile:=current_module^.sourcefiles^.get_file(fileinfo.fileindex);
           if includecount=0 then
            curr_n:=n_sourcefile
           else
            curr_n:=n_includefile;
           { get symbol for this includefile }
           hp:=newasmsymbol('Ltext'+ToStr(IncludeCount));
           if currpass=1 then
             begin
                hp^.typ:=AS_LOCAL;
                hp^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,0);
             end
           else
             objectoutput^.writesymbol(hp);
           { emit stabs }
           if (infile^.path^<>'') then
             EmitStabs('"'+lower(BsToSlash(FixPath(infile^.path^,false)))+'",'+tostr(curr_n)+
               ',0,0,Ltext'+ToStr(IncludeCount));
           EmitStabs('"'+lower(FixFileName(infile^.name^))+'",'+tostr(curr_n)+
             ',0,0,Ltext'+ToStr(IncludeCount));
           inc(includecount);
         end;
      { line changed ? }
        if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
          emitlineinfostabs(n_line,fileinfo.line);
        stabslastfileinfo:=fileinfo;
      end;


    procedure ti386binasmlist.StartFileLineInfo;
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
{$endif GDB}


    function ti386binasmlist.MaybeNextList(var hp:pai):boolean;
      begin
        { maybe end of list }
        while not assigned(hp) do
         begin
           if currlistidx<lists then
            begin
              inc(currlistidx);
              currlist:=list[currlistidx];
              hp:=pai(currlist^.first);
            end
           else
            begin
              MaybeNextList:=false;
              exit;
            end;
         end;
        MaybeNextList:=true;
      end;


    function ti386binasmlist.TreePass0(hp:pai):pai;
      var
        lastsec : tsection;
        l : longint;
      begin
        while assigned(hp) do
         begin
           case hp^.typ of
             ait_align :
               begin
                 { always use the maximum fillsize in this pass to avoid possible
                   short jumps to become out of range }
                 pai_align(hp)^.fillsize:=pai_align(hp)^.aligntype;
                 objectalloc^.sectionalloc(pai_align(hp)^.fillsize);
               end;
             ait_datablock :
               begin
{$ifdef EXTERNALBSS}
                 if not SmartAsm then
                  begin
                    if not pai_datablock(hp)^.is_global then
                     begin
                        l:=pai_datablock(hp)^.size;
                        if l>2 then
                          objectalloc^.sectionalign(4)
                        else if l>1 then
                          objectalloc^.sectionalign(2);
                        objectalloc^.sectionalloc(pai_datablock(hp)^.size);
                     end;
                  end
                 else
                  begin
{$endif}
                    l:=pai_datablock(hp)^.size;
                    if l>2 then
                      objectalloc^.sectionalign(4)
                    else if l>1 then
                      objectalloc^.sectionalign(2);
                    objectalloc^.sectionalloc(pai_datablock(hp)^.size);
                  end;
               end;
             ait_const_32bit :
               objectalloc^.sectionalloc(4);
             ait_const_16bit :
               objectalloc^.sectionalloc(2);
             ait_const_8bit :
               objectalloc^.sectionalloc(1);
             ait_real_80bit :
               objectalloc^.sectionalloc(10);
             ait_real_64bit :
               objectalloc^.sectionalloc(8);
             ait_real_32bit :
               objectalloc^.sectionalloc(4);
             ait_comp_64bit :
               objectalloc^.sectionalloc(8);
             ait_const_rva,
             ait_const_symbol :
               objectalloc^.sectionalloc(4);
             ait_section:
               begin
                 objectalloc^.setsection(pai_section(hp)^.sec);
                 lastsec:=pai_section(hp)^.sec;
               end;
             ait_symbol :
               pai_symbol(hp)^.sym^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,0);
             ait_label :
               pai_label(hp)^.l^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,0);
             ait_string :
               objectalloc^.sectionalloc(pai_string(hp)^.len);
             ait_instruction :
               objectalloc^.sectionalloc(paicpu(hp)^.Pass1(objectalloc^.sectionsize));
             ait_cut :
               if SmartAsm then
                begin
                  objectalloc^.resetsections;
                  objectalloc^.setsection(lastsec);
                end;
           end;
           hp:=pai(hp^.next);
         end;
        TreePass0:=hp;
      end;


    function ti386binasmlist.TreePass1(hp:pai):pai;
      var
        l : longint;
      begin
        while assigned(hp) do
         begin
{$ifdef GDB}
           { write stabs }
           if (cs_debuginfo in aktmoduleswitches) then
            begin
              if (objectalloc^.currsec<>sec_none) and
                 not(hp^.typ in  [
                     ait_label,
                     ait_regalloc,ait_tempalloc,
                     ait_stabn,ait_stabs,ait_section,
                     ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
               WriteFileLineInfo(hp^.fileinfo);
            end;
{$endif GDB}
           case hp^.typ of
             ait_align :
               begin
                 { here we must determine the fillsize which is used in pass2 }
                 pai_align(hp)^.fillsize:=align(objectalloc^.sectionsize,pai_align(hp)^.aligntype)-
                   objectalloc^.sectionsize;
                 objectalloc^.sectionalloc(pai_align(hp)^.fillsize);
               end;
             ait_datablock :
               begin
                 if objectalloc^.currsec<>sec_bss then
                  Message(asmw_e_alloc_data_only_in_bss);
{$ifdef EXTERNALBSS}
                 if not SmartAsm then
                  begin
                    if pai_datablock(hp)^.is_global then
                     begin
                       pai_datablock(hp)^.sym^.typ:=AS_EXTERNAL;
                       pai_datablock(hp)^.sym^.setaddress(sec_none,pai_datablock(hp)^.size,pai_datablock(hp)^.size);
                     end
                    else
                     begin
                       l:=pai_datablock(hp)^.size;
                       if l>2 then
                         objectalloc^.sectionalign(4)
                       else if l>1 then
                         objectalloc^.sectionalign(2);
                       pai_datablock(hp)^.sym^.typ:=AS_LOCAL;
                       pai_datablock(hp)^.sym^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,
                         pai_datablock(hp)^.size);
                       objectalloc^.sectionalloc(pai_datablock(hp)^.size);
                     end;
                   end
                  else
{$endif}
                   begin
                     if pai_datablock(hp)^.is_global then
                      pai_datablock(hp)^.sym^.typ:=AS_GLOBAL
                     else
                      pai_datablock(hp)^.sym^.typ:=AS_LOCAL;
                     l:=pai_datablock(hp)^.size;
                     if l>2 then
                       objectalloc^.sectionalign(4)
                     else if l>1 then
                       objectalloc^.sectionalign(2);
                     pai_datablock(hp)^.sym^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,pai_datablock(hp)^.size);
                     objectalloc^.sectionalloc(pai_datablock(hp)^.size);
                   end;
               end;
             ait_const_32bit :
               objectalloc^.sectionalloc(4);
             ait_const_16bit :
               objectalloc^.sectionalloc(2);
             ait_const_8bit :
               objectalloc^.sectionalloc(1);
             ait_real_80bit :
               objectalloc^.sectionalloc(10);
             ait_real_64bit :
               objectalloc^.sectionalloc(8);
             ait_real_32bit :
               objectalloc^.sectionalloc(4);
             ait_comp_64bit :
               objectalloc^.sectionalloc(8);
             ait_const_rva,
             ait_const_symbol :
               objectalloc^.sectionalloc(4);
             ait_section:
               begin
                 objectalloc^.setsection(pai_section(hp)^.sec);
{$ifdef GDB}
                 case pai_section(hp)^.sec of
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
               convertstabs(pai_stabn(hp)^.str);
             ait_stabs :
               convertstabs(pai_stabs(hp)^.str);
             ait_stab_function_name :
               if assigned(pai_stab_function_name(hp)^.str) then
                 funcname:=getasmsymbol(strpas(pai_stab_function_name(hp)^.str))
               else
                 funcname:=nil;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_symbol :
               begin
                 if pai_symbol(hp)^.is_global then
                  pai_symbol(hp)^.sym^.typ:=AS_GLOBAL
                 else
                  pai_symbol(hp)^.sym^.typ:=AS_LOCAL;
                 pai_symbol(hp)^.sym^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,0);
               end;
             ait_label :
               begin
                 if pai_label(hp)^.is_global then
                  pai_label(hp)^.l^.typ:=AS_GLOBAL
                 else
                  pai_label(hp)^.l^.typ:=AS_LOCAL;
                 pai_label(hp)^.l^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,0);
               end;
             ait_string :
               objectalloc^.sectionalloc(pai_string(hp)^.len);
             ait_instruction :
               objectalloc^.sectionalloc(paicpu(hp)^.Pass1(objectalloc^.sectionsize));
             ait_direct :
               Message(asmw_f_direct_not_supported);
             ait_cut :
               if SmartAsm then
                break;
           end;
           hp:=pai(hp^.next);
         end;
        TreePass1:=hp;
      end;


    function ti386binasmlist.TreePass2(hp:pai):pai;
      const
        alignarray:array[0..5] of string[8]=(
          #$8D#$B4#$26#$00#$00#$00#$00,
          #$8D#$B6#$00#$00#$00#$00,
          #$8D#$74#$26#$00,
          #$8D#$76#$00,
          #$89#$F6,
          #$90
        );
      var
        l,j : longint;
        alignoparray:array[0..63] of byte;
{$ifdef I386}
        co : comp;
{$endif I386}
      begin
        { main loop }
        while assigned(hp) do
         begin
{$ifdef GDB}
           { write stabs }
           if cs_debuginfo in aktmoduleswitches then
            begin
              if (objectoutput^.currsec<>sec_none) and
                 not(hp^.typ in  [
                     ait_label,
                     ait_regalloc,ait_tempalloc,
                     ait_stabn,ait_stabs,ait_section,
                     ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
               WriteFileLineInfo(hp^.fileinfo);
            end;
{$endif GDB}
           case hp^.typ of
             ait_align :
               begin
                 if not pai_align(hp)^.use_op then
                  begin
                    l:=pai_align(hp)^.fillsize;
                    while (l>0) do
                     begin
                       for j:=0to 5 do
                        if (l>=length(alignarray[j])) then
                         break;
                       objectoutput^.writebytes(alignarray[j][1],length(alignarray[j]));
                       dec(l,length(alignarray[j]));
                     end;
                  end
                 else
                  begin
                    fillchar(alignoparray,pai_align(hp)^.fillsize,pai_align(hp)^.fillop);
                    objectoutput^.writebytes(alignoparray,pai_align(hp)^.fillsize);
                  end;
               end;
             ait_section :
               begin
                 objectoutput^.defaultsection(pai_section(hp)^.sec);
{$ifdef GDB}
                 case pai_section(hp)^.sec of
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
               objectoutput^.writesymbol(pai_symbol(hp)^.sym);
             ait_datablock :
               begin
                 objectoutput^.writesymbol(pai_datablock(hp)^.sym);
                 if SmartAsm
{$ifdef EXTERNALBSS}
                    or (not pai_datablock(hp)^.is_global)
{$endif}
                    then
                   begin
                     l:=pai_datablock(hp)^.size;
                     if l>2 then
                       objectoutput^.writealign(4)
                     else if l>1 then
                       objectoutput^.writealign(2);
                     objectoutput^.writealloc(pai_datablock(hp)^.size);
                   end;
               end;
             ait_const_32bit :
               objectoutput^.writebytes(pai_const(hp)^.value,4);
             ait_const_16bit :
               objectoutput^.writebytes(pai_const(hp)^.value,2);
             ait_const_8bit :
               objectoutput^.writebytes(pai_const(hp)^.value,1);
             ait_real_80bit :
               objectoutput^.writebytes(pai_real_80bit(hp)^.value,10);
             ait_real_64bit :
               objectoutput^.writebytes(pai_real_64bit(hp)^.value,8);
             ait_real_32bit :
               objectoutput^.writebytes(pai_real_32bit(hp)^.value,4);
             ait_comp_64bit :
               begin
{$ifdef FPC}
                 co:=comp(pai_comp_64bit(hp)^.value);
{$else}
                 co:=pai_comp_64bit(hp)^.value;
{$endif}
                 objectoutput^.writebytes(co,8);
               end;
             ait_string :
               objectoutput^.writebytes(pai_string(hp)^.str^,pai_string(hp)^.len);
             ait_const_rva :
               objectoutput^.writereloc(pai_const_symbol(hp)^.offset,4,
                 pai_const_symbol(hp)^.sym,relative_rva);
             ait_const_symbol :
               objectoutput^.writereloc(pai_const_symbol(hp)^.offset,4,
                 pai_const_symbol(hp)^.sym,relative_false);
             ait_label :
               objectoutput^.writesymbol(pai_label(hp)^.l);
             ait_instruction :
               paicpu(hp)^.Pass2;
{$ifdef GDB}
             ait_stabn :
               convertstabs(pai_stabn(hp)^.str);
             ait_stabs :
               convertstabs(pai_stabs(hp)^.str);
             ait_stab_function_name :
               if assigned(pai_stab_function_name(hp)^.str) then
                 funcname:=getasmsymbol(strpas(pai_stab_function_name(hp)^.str))
               else
                 funcname:=nil;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_cut :
               if SmartAsm then
                break;
           end;
           hp:=pai(hp^.next);
         end;
        TreePass2:=hp;
      end;


    procedure ti386binasmlist.writetree;
      var
        hp : pai;
      begin
        objectalloc^.setsection(sec_code);
        objectoutput^.defaultsection(sec_code);

      { Pass 1 }
        currpass:=1;
{$ifdef GDB}
        StartFileLineInfo;
{$endif GDB}
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=pai(currlist^.first);
        while assigned(hp) do
         begin
           hp:=TreePass1(hp);
           MaybeNextList(hp);
         end;
        { check for undefined labels }
        CheckAsmSymbolListUndefined;
        { set section sizes }
        objectoutput^.setsectionsizes(objectalloc^.secsize);
        { leave if errors have occured }
        if errorcount>0 then
         exit;

      { Pass 2 }
        currpass:=2;
{$ifdef GDB}
        StartFileLineInfo;
{$endif GDB}
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=pai(currlist^.first);
        while assigned(hp) do
         begin
           hp:=TreePass2(hp);
           MaybeNextList(hp);
         end;
      end;


    procedure ti386binasmlist.writetreesmart;
      var
        hp : pai;
      begin
        objectalloc^.setsection(sec_code);
        objectoutput^.defaultsection(sec_code);
        { start with list 1 }
        currlistidx:=1;
        currlist:=list[currlistidx];
        hp:=pai(currlist^.first);
        while assigned(hp) do
         begin
         { Pass 1 }
           currpass:=1;
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           TreePass1(hp);
           { check for undefined labels }
           CheckAsmSymbolListUndefined;
           { set section sizes }
           objectoutput^.setsectionsizes(objectalloc^.secsize);
           { leave if errors have occured }
           if errorcount>0 then
            exit;

         { Pass 2 }
           currpass:=2;
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           hp:=TreePass2(hp);

           if not MaybeNextList(hp) then
            break;

           { leave if errors have occured }
           if errorcount>0 then
            exit;
           { write the current objectfile }
           objectoutput^.donewriting;

           { we will start a new objectfile so reset everything }
           if (hp^.typ=ait_cut) then
            objectoutput^.initwriting(pai_cut(hp)^.place)
           else
            objectoutput^.initwriting(cut_normal);
           objectalloc^.resetsections;
           ResetAsmsymbolList;

           { avoid empty files }
           while assigned(hp^.next) and
                 (pai(hp^.next)^.typ in [ait_marker,ait_comment,ait_section,ait_cut]) do
            begin
              if pai(hp^.next)^.typ=ait_section then
                begin
                  objectalloc^.setsection(pai_section(hp^.next)^.sec);
                  objectoutput^.defaultsection(pai_section(hp^.next)^.sec);
                end;
              hp:=pai(hp^.next);
            end;

           hp:=pai(hp^.next);

           if not MaybeNextList(hp) then
            break;
         end;
      end;


    procedure ti386binasmlist.writebin;

        procedure addlist(p:paasmoutput);
        begin
          inc(lists);
          list[lists]:=p;
        end;

      begin
{$ifdef MULTIPASS}
        { Process the codesegment twice so the short jmp instructions can
          be optimized }
        currpass:=0;
        TreePass0(pai(codesegment^.first));
{$endif}

        objectalloc^.resetsections;
        objectalloc^.setsection(sec_code);

        objectoutput^.initwriting(cut_normal);
        objectoutput^.defaultsection(sec_code);

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

        { leave if errors have occured }
        if errorcount>0 then
         exit;

        { write last objectfile }
        objectoutput^.donewriting;
      end;


    constructor ti386binasmlist.init(t:togtype;smart:boolean);
      begin
        case t of
          og_none :
            Message(asmw_f_no_binary_writer_selected);
          og_dbg :
            objectoutput:=new(pdbgoutput,init(smart));
          og_coff :
            objectoutput:=new(pdjgppcoffoutput,init(smart));
          og_pecoff :
            objectoutput:=new(pwin32coffoutput,init(smart));
        end;
        objectalloc:=new(pobjectalloc,init);
        SmartAsm:=smart;
        currpass:=0;
      end;


   destructor ti386binasmlist.done;
      begin
        dispose(objectoutput,done);
        dispose(objectalloc,done);
      end;

end.
{
  $Log$
  Revision 1.31  1999-12-22 01:01:46  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.30  1999/12/08 10:39:59  pierre
    + allow use of unit var in exports of DLL for win32
      by using direct export writing by default instead of use of DEFFILE
      that does not allow assembler labels that do not
      start with an underscore.
      Use -WD to force use of Deffile for Win32 DLL

  Revision 1.29  1999/12/01 22:05:13  pierre
   * problem with unused external symbol in stabs solved

  Revision 1.28  1999/11/30 10:40:42  peter
    + ttype, tsymlist

  Revision 1.27  1999/11/06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.26  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.25  1999/09/26 21:13:40  peter
    * short jmp with alignment problems fixed

  Revision 1.24  1999/08/25 11:59:33  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.23  1999/08/10 12:26:21  pierre
   * avoid double .edata section if using DLLTOOL

  Revision 1.22  1999/08/04 00:22:35  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.21  1999/08/01 18:28:09  florian
    * modifications for the new code generator

  Revision 1.20  1999/07/31 12:33:11  peter
    * fixed smartlinking

  Revision 1.19  1999/07/22 09:37:30  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.18  1999/07/03 00:26:02  peter
    * ag386bin doesn't destroy the aasmoutput lists anymore

  Revision 1.17  1999/06/10 23:52:34  pierre
   * merged from fixes branch

  Revision 1.16.2.1  1999/06/10 23:33:35  pierre
   * pchar memory loss and .bss size problem solved

  Revision 1.16  1999/06/03 16:39:10  pierre
   * EXTERNALBSS fixed for stabs and default again

  Revision 1.15  1999/06/02 22:43:59  pierre
   * previous wrong log corrected

  Revision 1.14  1999/06/02 22:25:25  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.13  1999/06/01 10:24:09  pierre
   * ts010021.pp problem solved for ag386bin !

  Revision 1.12  1999/05/27 19:43:59  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.11  1999/05/21 13:54:41  peter
    * NEWLAB for label as symbol

  Revision 1.10  1999/05/19 11:54:17  pierre
   + experimental code for externalbss and stabs problem

  Revision 1.9  1999/05/12 00:19:37  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.8  1999/05/09 11:38:04  peter
    * don't write .o and link if errors occure during assembling

}

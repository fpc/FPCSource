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
{define EXTERNALBSS}

  interface

    uses
       i386base,
       cobjects,aasm,files,assemble;

    type
      togtype=(og_none,og_dbg,og_coff,og_pecoff);

      pi386binasmlist=^ti386binasmlist;
      ti386binasmlist=object
        constructor init(t:togtype);
        destructor  done;
        procedure WriteBin;
      private
        currpass : byte;
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
        function  TreePass0(hp:pai):pai;
        function  TreePass1(hp:pai):pai;
        function  TreePass2(hp:pai):pai;
        procedure writetree(p:paasmoutput);
      end;

  implementation

    uses
       strings,verbose,
       globtype,globals,
       i386asm,systems,
{$ifdef GDB}
       gdb,
{$endif}
       og386,og386dbg,og386cff;

{$ifdef GDB}

    procedure ti386binasmlist.convertstabs(p:pchar);
      var
        ofs,
        nidx,nother,i,line,j : longint;
        code : integer;
        hp : pchar;
        reloc : boolean;
        sec : tsection;
        ps : pasmsymbol;
        s : string;
      begin
        ofs:=0;
        reloc:=true;
        sec:=sec_none;
        if p[0]='"' then
         begin
           i:=1;
           { we can have \" inside the string !! PM }
           while not ((p[i]='"') and (p[i-1]<>'\')) do
            inc(i);
           p[i]:=#0;
           hp:=@p[1];
           s:=StrPas(@P[i+2]);
         end
        else
         begin
           hp:=nil;
           s:=StrPas(P);
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
        Delete(s,1,j);
        j:=pos(',',s);
        if (j=0) then
         internalerror(33003);
        Val(Copy(s,1,j-1),nother,code);
        if code<>0 then
         internalerror(33004);
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
          Delete(s,1,j);
        if ofs=0 then
          begin
            Val(s,ofs,code);
            if code=0 then
              reloc:=false
            else
              begin
                ofs:=0;
                { handle asmsymbol or
                    asmsymbol - asmsymbol }
                j:=pos(' ',s);
                if j=0 then
                  j:=pos('-',s);
                { single asmsymbol }
                if j=0 then
                  j:=256;
                ps:=getasmsymbol(copy(s,1,j-1));
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
                    delete(s,1,j);
                    while (s<>'') and (s[1]=' ') do
                      delete(s,1,1);
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
        objectoutput^.WriteStabs(sec,ofs,hp,nidx,nother,line,reloc);
        if assigned(hp) then
         p[i]:='"';
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
                 if (objectalloc^.sectionsize mod pai_align(hp)^.aligntype)<>0 then
                   begin
                     pai_align(hp)^.fillsize:=pai_align(hp)^.aligntype-
                       (objectalloc^.sectionsize mod pai_align(hp)^.aligntype);
                     objectalloc^.sectionalloc(pai_align(hp)^.fillsize);
                   end
                 else
                   pai_align(hp)^.fillsize:=0;
               end;
             ait_datablock :
               begin
{$ifdef EXTERNALBSS}
                 if not pai_datablock(hp)^.is_global then
                  begin
                     l:=pai_datablock(hp)^.size;
                     if l>2 then
                       objectalloc^.sectionalign(4)
                     else if l>1 then
                       objectalloc^.sectionalign(2);
                     objectalloc^.sectionalloc(pai_datablock(hp)^.size);
                  end;
{$else}
                 l:=pai_datablock(hp)^.size;
                 if l>2 then
                   objectalloc^.sectionalign(4)
                 else if l>1 then
                   objectalloc^.sectionalign(2);
                 objectalloc^.sectionalloc(pai_datablock(hp)^.size);
{$endif}
               end;
             ait_const_32bit :
               objectalloc^.sectionalloc(4);
             ait_const_16bit :
               objectalloc^.sectionalloc(2);
             ait_const_8bit :
               objectalloc^.sectionalloc(1);
             ait_real_64bit :
               objectalloc^.sectionalloc(8);
             ait_real_32bit :
               objectalloc^.sectionalloc(4);
             ait_real_80bit :
               objectalloc^.sectionalloc(10);
             ait_comp :
{$ifdef I386}
               objectalloc^.sectionalloc(8);
{$else not I386}
               Message(asmw_f_comp_not_supported);
{$endif I386}
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
               begin
                 pai_label(hp)^.setaddress(objectalloc^.sectionsize);
                 if pai_label(hp)^.l^.is_symbol then
                   begin
                     pai_label(hp)^.sym:=newasmsymbol(lab2str(pai_label(hp)^.l));
                     if (pai_label(hp)^.l^.is_data) and (cs_smartlink in aktmoduleswitches) then
                       pai_label(hp)^.sym^.typ:=AS_GLOBAL
                     else
                       pai_label(hp)^.sym^.typ:=AS_LOCAL;
                     pai_label(hp)^.sym^.setaddress(objectalloc^.currsec,pai_label(hp)^.l^.address,0);
                   end;
               end;
             ait_string :
               objectalloc^.sectionalloc(pai_string(hp)^.len);
             ait_labeled_instruction,
             ait_instruction :
               objectalloc^.sectionalloc(pai386(hp)^.Pass1(objectalloc^.sectionsize));
             ait_cut :
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
                 not(hp^.typ in  [ait_external,ait_regalloc, ait_tempalloc,
                     ait_stabn,ait_stabs,ait_section,
                     ait_label,ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
               WriteFileLineInfo(hp^.fileinfo);
            end;
{$endif GDB}
           case hp^.typ of
             ait_align :
               begin
                 if (objectalloc^.sectionsize mod pai_align(hp)^.aligntype)<>0 then
                   begin
                     pai_align(hp)^.fillsize:=pai_align(hp)^.aligntype-
                       (objectalloc^.sectionsize mod pai_align(hp)^.aligntype);
                     objectalloc^.sectionalloc(pai_align(hp)^.fillsize);
                   end
                 else
                   pai_align(hp)^.fillsize:=0;
               end;
             ait_datablock :
               begin
                 if objectalloc^.currsec<>sec_bss then
                  Message(asmw_e_alloc_data_only_in_bss);
{$ifdef EXTERNALBSS}
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
                    pai_datablock(hp)^.sym^.setaddress(objectalloc^.currsec,objectalloc^.sectionsize,pai_datablock(hp)^.size);
                    objectalloc^.sectionalloc(pai_datablock(hp)^.size);
                  end;
{$else}
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
{$endif}
               end;
             ait_const_32bit :
               objectalloc^.sectionalloc(4);
             ait_const_16bit :
               objectalloc^.sectionalloc(2);
             ait_const_8bit :
               objectalloc^.sectionalloc(1);
             ait_real_64bit :
               objectalloc^.sectionalloc(8);
             ait_real_32bit :
               objectalloc^.sectionalloc(4);
             ait_real_80bit :
               objectalloc^.sectionalloc(10);
             ait_comp :
{$ifdef I386}
               objectalloc^.sectionalloc(8);
{$else not I386}
               Message(asmw_f_comp_not_supported);
{$endif I386}
             ait_const_rva,
             ait_const_symbol :
               objectalloc^.sectionalloc(4);
             ait_external :
               pai_external(hp)^.sym^.typ:=AS_EXTERNAL;
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
                 funcname:=getasmsymbol(pai_stab_function_name(hp)^.str)
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
                 pai_label(hp)^.setaddress(objectalloc^.sectionsize);
                 if pai_label(hp)^.l^.is_symbol then
                   begin
                     pai_label(hp)^.sym:=newasmsymbol(lab2str(pai_label(hp)^.l));
                     if (pai_label(hp)^.l^.is_data) and (cs_smartlink in aktmoduleswitches) then
                       pai_label(hp)^.sym^.typ:=AS_GLOBAL
                     else
                       pai_label(hp)^.sym^.typ:=AS_LOCAL;
                     pai_label(hp)^.sym^.setaddress(objectalloc^.currsec,pai_label(hp)^.l^.address,0);
                   end;
               end;
             ait_string :
               objectalloc^.sectionalloc(pai_string(hp)^.len);
             ait_labeled_instruction,
             ait_instruction :
               objectalloc^.sectionalloc(pai386(hp)^.Pass1(objectalloc^.sectionsize));
             ait_direct :
               Message(asmw_f_direct_not_supported);
             ait_cut :
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
                 not(hp^.typ in  [ait_external,ait_regalloc, ait_tempalloc,
                     ait_stabn,ait_stabs,ait_section,
                     ait_label,ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
               WriteFileLineInfo(hp^.fileinfo);
            end;
{$endif GDB}
           case hp^.typ of
             ait_align :
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
             ait_external :
               objectoutput^.writesymbol(pai_external(hp)^.sym);
             ait_symbol :
               objectoutput^.writesymbol(pai_symbol(hp)^.sym);
             ait_datablock :
               begin
                 l:=pai_datablock(hp)^.size;
                 if l>2 then
                   objectoutput^.writealign(4)
                 else if l>1 then
                   objectoutput^.writealign(2);
                 objectoutput^.writesymbol(pai_datablock(hp)^.sym);
{$ifdef EXTERNALBSS}
                 if not pai_datablock(hp)^.is_global then
{$endif}
                   objectoutput^.writealloc(pai_datablock(hp)^.size);
               end;
             ait_const_32bit :
               objectoutput^.writebytes(pai_const(hp)^.value,4);
             ait_const_16bit :
               objectoutput^.writebytes(pai_const(hp)^.value,2);
             ait_const_8bit :
               objectoutput^.writebytes(pai_const(hp)^.value,1);
             ait_real_64bit :
               objectoutput^.writebytes(pai_double(hp)^.value,8);
             ait_real_32bit :
               objectoutput^.writebytes(pai_single(hp)^.value,4);
             ait_real_80bit :
               objectoutput^.writebytes(pai_extended(hp)^.value,10);
{$ifdef I386}
             ait_comp :
               begin
{$ifdef FPC}
                 co:=comp(pai_comp(hp)^.value);
{$else}
                 co:=pai_comp(hp)^.value;
{$endif}
                 objectoutput^.writebytes(co,8);
               end;
{$endif I386}
             ait_string :
               objectoutput^.writebytes(pai_string(hp)^.str^,pai_string(hp)^.len);
             ait_const_rva :
               objectoutput^.writereloc(pai_const_symbol(hp)^.offset,4,
                 pai_const_symbol(hp)^.sym,relative_rva);
             ait_const_symbol :
               objectoutput^.writereloc(pai_const_symbol(hp)^.offset,4,
                 pai_const_symbol(hp)^.sym,relative_false);
             ait_label :
               begin
                 if assigned(pai_label(hp)^.sym) then
                  objectoutput^.writesymbol(pai_label(hp)^.sym);
               end;
             ait_labeled_instruction,
             ait_instruction :
               pai386(hp)^.Pass2;
{$ifdef GDB}
             ait_stabn :
               convertstabs(pai_stabn(hp)^.str);
             ait_stabs :
               convertstabs(pai_stabs(hp)^.str);
             ait_stab_function_name :
               if assigned(pai_stab_function_name(hp)^.str) then
                 funcname:=getasmsymbol(pai_stab_function_name(hp)^.str)
               else
                 funcname:=nil;
             ait_force_line :
               stabslastfileinfo.line:=0;
{$endif}
             ait_cut :
               break;
           end;
           hp:=pai(hp^.next);
         end;
        TreePass2:=hp;
      end;


    procedure ti386binasmlist.writetree(p:paasmoutput);
      var
        hp,hp1 : pai;
      begin
        if not assigned(p) then
         exit;
        objectalloc^.setsection(sec_code);
        objectoutput^.defaultsection(sec_code);
        hp:=pai(p^.first);
        while assigned(hp) do
         begin
         { Pass 1 }
           currpass:=1;
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           hp1:=TreePass1(hp);

         { set section sizes }
           objectoutput^.setsectionsizes(objectalloc^.secsize);
         { Pass 2 }
           currpass:=2;
{$ifdef GDB}
           StartFileLineInfo;
{$endif GDB}
           hp1:=TreePass2(hp);

         { if assigned then we have a ait_cut }
           hp:=hp1;
           if assigned(hp) then
            begin
              if hp^.typ<>ait_cut then
               internalerror(3334443);
              { write the current objectfile }
              objectoutput^.donewriting;
              { start the writing again }
              objectoutput^.initwriting;
              { we will start a new objectfile so reset everything }
              ResetAsmsymbolList;
              objectalloc^.resetsections;
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
            end;
         end;
      end;


    procedure ti386binasmlist.writebin;
      var
        mylist : paasmoutput;

        procedure addlist(p:paasmoutput);
        begin
          mylist^.concat(new(pai_section,init(sec_code)));
          mylist^.concatlist(p);
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

        objectoutput^.initwriting;
        objectoutput^.defaultsection(sec_code);

        new(mylist,init);

        if not(cs_compilesystem in aktmoduleswitches) then
          addlist(externals);
        if cs_debuginfo in aktmoduleswitches then
          addlist(debuglist);
        addlist(codesegment);
        addlist(datasegment);
        addlist(consts);
        addlist(rttilist);
        addlist(bsssegment);
        if assigned(importssection) then
          addlist(importssection);
        if assigned(exportssection) then
          addlist(exportssection);
        if assigned(resourcesection) then
          addlist(resourcesection);

        WriteTree(mylist);

        dispose(mylist,done);

        objectoutput^.donewriting;
      end;


    constructor ti386binasmlist.init(t:togtype);
      begin
        case t of
          og_none :
            Message(asmw_f_no_binary_writer_selected);
          og_dbg :
            objectoutput:=new(pdbgoutput,init);
          og_coff :
            objectoutput:=new(pdjgppcoffoutput,init);
          og_pecoff :
            objectoutput:=new(pwin32coffoutput,init);
        end;
        objectalloc:=new(pobjectalloc,init);
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
  Revision 1.8  1999-05-09 11:38:04  peter
    * don't write .o and link if errors occure during assembling

  Revision 1.6  1999/05/07 00:36:58  pierre
    * added alignment code for .bss
    * stabs correct but externalbss disabled
      would need a special treatment in writestabs

  Revision 1.5  1999/05/06 09:05:07  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.4  1999/05/05 22:21:47  peter
    * updated messages

  Revision 1.3  1999/05/05 17:34:29  peter
    * output is more like as 2.9.1
    * stabs really working for go32v2

  Revision 1.2  1999/05/04 21:44:30  florian
    * changes to compile it with Delphi 4.0

  Revision 1.1  1999/05/01 13:23:57  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.14  1999/04/16 11:49:48  peter
    + tempalloc
    + -at to show temp alloc info in .s file

  Revision 1.13  1999/03/12 00:20:03  pierre
   + win32 output working !

  Revision 1.12  1999/03/11 17:52:34  peter
    * fixed wrong ot_signed generation in insns tab

  Revision 1.11  1999/03/10 13:41:07  pierre
   + partial implementation for win32 !
     winhello works but pp still does not !

  Revision 1.10  1999/03/08 14:51:05  peter
    + smartlinking for ag386bin

  Revision 1.9  1999/03/06 17:24:18  peter
    * rewritten intel parser a lot, especially reference reading
    * size checking added for asm parsers

  Revision 1.8  1999/03/05 13:09:50  peter
    * first things for tai_cut support for ag386bin

  Revision 1.7  1999/03/03 11:41:53  pierre
    + stabs info corrected to give results near to GAS output
    * local labels (with .L are not stored in object anymore)
      so we get the same number of symbols as from GAS !

  Revision 1.6  1999/03/03 01:36:44  pierre
    + stabs output working (though not really tested)
      for a simple file the only difference to GAS output is due
      to the VMA of the different sections

  Revision 1.5  1999/03/02 02:56:18  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.4  1999/03/01 15:46:20  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes

  Revision 1.3  1999/02/25 21:03:01  peter
    * ag386bin updates
    + coff writer

  Revision 1.2  1999/02/22 02:16:00  peter
    * updates for ag386bin

  Revision 1.1  1999/02/16 17:59:37  peter
    + initial files

}

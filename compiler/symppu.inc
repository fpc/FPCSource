{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Implementation of the reading of PPU Files for the symtable

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

    const
{$ifdef FPC}
       ppubufsize=32768;
{$ELSE}
    {$IFDEF USEOVERLAY}
       ppubufsize=512;
    {$ELSE}
       ppubufsize=4096;
    {$ENDIF}
{$ENDIF}

{$define ORDERSOURCES}

{*****************************************************************************
                                 PPU Writing
*****************************************************************************}

    procedure writebyte(b:byte);
      begin
        current_ppu^.putbyte(b);
      end;


    procedure writeword(w:word);
      begin
        current_ppu^.putword(w);
      end;


    procedure writelong(l:longint);
      begin
        current_ppu^.putlongint(l);
      end;


    procedure writereal(d:bestreal);
      begin
        current_ppu^.putreal(d);
      end;


    procedure writestring(const s:string);
      begin
        current_ppu^.putstring(s);
      end;


    procedure writenormalset(var s); {You cannot pass an array[0..31] of byte!}
      begin
        current_ppu^.putdata(s,sizeof(tnormalset));
      end;


    procedure writesmallset(var s);
      begin
        current_ppu^.putdata(s,4);
      end;


    { posinfo is not relevant for changes in PPU }
    procedure writeposinfo(const p:tfileposinfo);
      var
        oldcrc : boolean;
      begin
        oldcrc:=current_ppu^.do_crc;
        current_ppu^.do_crc:=false;
        current_ppu^.putword(p.fileindex);
        current_ppu^.putlongint(p.line);
        current_ppu^.putword(p.column);
        current_ppu^.do_crc:=oldcrc;
      end;


    procedure writederef(p : psymtableentry);
      begin
        if p=nil then
         current_ppu^.putbyte(ord(derefnil))
        else
         begin
           { Static symtable ? }
           if p^.owner^.symtabletype=staticsymtable then
            begin
              current_ppu^.putbyte(ord(derefaktstaticindex));
              current_ppu^.putword(p^.indexnr);
            end
           { Local record/object symtable ? }
           else if (p^.owner=aktrecordsymtable) then
            begin
              current_ppu^.putbyte(ord(derefaktrecordindex));
              current_ppu^.putword(p^.indexnr);
            end
           { Local local/para symtable ? }
           else if (p^.owner=aktlocalsymtable) then
            begin
              current_ppu^.putbyte(ord(derefaktlocal));
              current_ppu^.putword(p^.indexnr);
            end
           else
            begin
              current_ppu^.putbyte(ord(derefindex));
              current_ppu^.putword(p^.indexnr);
           { Current unit symtable ? }
              repeat
                if not assigned(p) then
                 internalerror(556655);
                case p^.owner^.symtabletype of
                 { when writing the pseudo PPU file
                   to get CRC values the globalsymtable is not yet
                   a unitsymtable PM }
                  globalsymtable,
                  unitsymtable :
                    begin
                      { check if the unit is available in the uses
                        clause, else it's an error }
                      if p^.owner^.unitid=$ffff then
                       internalerror(55665566);
                      current_ppu^.putbyte(ord(derefunit));
                      current_ppu^.putword(p^.owner^.unitid);
                      break;
                    end;
                  staticsymtable :
                    begin
                      current_ppu^.putbyte(ord(derefaktstaticindex));
                      current_ppu^.putword(p^.indexnr);
                      break;
                    end;
                  localsymtable :
                    begin
                      p:=p^.owner^.defowner;
                      current_ppu^.putbyte(ord(dereflocal));
                      current_ppu^.putword(p^.indexnr);
                    end;
                  parasymtable :
                    begin
                      p:=p^.owner^.defowner;
                      current_ppu^.putbyte(ord(derefpara));
                      current_ppu^.putword(p^.indexnr);
                    end;
                  objectsymtable,
                  recordsymtable :
                    begin
                      p:=p^.owner^.defowner;
                      current_ppu^.putbyte(ord(derefrecord));
                      current_ppu^.putword(p^.indexnr);
                    end;
                  else
                    internalerror(556656);
                end;
              until false;
            end;
         end;
      end;

    procedure writedefref(p : pdef);
      begin
        writederef(p);
      end;

    procedure writesymref(p : psym);
      begin
        writederef(p);
      end;

    procedure writesourcefiles;
      var
        hp    : pinputfile;
{$ifdef ORDERSOURCES}
        i,j : longint;
{$endif ORDERSOURCES}
      begin
      { second write the used source files }
        current_ppu^.do_crc:=false;
        hp:=current_module^.sourcefiles^.files;
{$ifdef ORDERSOURCES}
      { write source files directly in good order }
        j:=0;
        while assigned(hp) do
          begin
            inc(j);
            hp:=hp^.ref_next;
          end;
        while j>0 do
          begin
            hp:=current_module^.sourcefiles^.files;
            for i:=1 to j-1 do
              hp:=hp^.ref_next;
            current_ppu^.putstring(hp^.name^);
            dec(j);
         end;
{$else not ORDERSOURCES}
        while assigned(hp) do
         begin
         { only name and extension }
           current_ppu^.putstring(hp^.name^);
           hp:=hp^.ref_next;
         end;
{$endif ORDERSOURCES}
        current_ppu^.writeentry(ibsourcefiles);
        current_ppu^.do_crc:=true;
      end;

    procedure writeusedmacros;
      var
        hp    : pmacrosym;
        i     : longint;
      begin
      { second write the used source files }
        current_ppu^.do_crc:=false;
        for i:=1 to macros^.symindex^.count do
         begin
           hp:=pmacrosym(macros^.symindex^.search(i));
         { only used or init defined macros are stored }
           if hp^.is_used or hp^.defined_at_startup then
             begin
               current_ppu^.putstring(hp^.name);
               current_ppu^.putbyte(byte(hp^.defined_at_startup));
               current_ppu^.putbyte(byte(hp^.is_used));
             end;
         end;
        current_ppu^.writeentry(ibusedmacros);
        current_ppu^.do_crc:=true;
      end;


    procedure writeusedunit;
      var
        hp      : pused_unit;
      begin
        numberunits;
        hp:=pused_unit(current_module^.used_units.first);
        while assigned(hp) do
         begin
           { implementation units should not change
             the CRC PM }
           current_ppu^.do_crc:=hp^.in_interface;
           current_ppu^.putstring(hp^.name^);
           { the checksum should not affect the crc of this unit ! (PFV) }
           current_ppu^.do_crc:=false;
           current_ppu^.putlongint(hp^.checksum);
           current_ppu^.putlongint(hp^.interface_checksum);
           current_ppu^.putbyte(byte(hp^.in_interface));
           current_ppu^.do_crc:=true;
           hp:=pused_unit(hp^.next);
         end;
        current_ppu^.do_interface_crc:=true;
        current_ppu^.writeentry(ibloadunit);
      end;


    procedure writelinkcontainer(var p:tlinkcontainer;id:byte;strippath:boolean);
      var
        hcontainer : tlinkcontainer;
        s : string;
        mask : longint;
      begin
        hcontainer.init;
        while not p.empty do
         begin
           s:=p.get(mask);
           if strippath then
            current_ppu^.putstring(SplitFileName(s))
           else
            current_ppu^.putstring(s);
           current_ppu^.putlongint(mask);
           hcontainer.insert(s,mask);
         end;
        current_ppu^.writeentry(id);
        p:=hcontainer;
      end;


    procedure writeunitas(const s : string;unittable : punitsymtable;only_crc : boolean);
      begin
         Message1(unit_u_ppu_write,s);

       { create unit flags }
         with Current_Module^ do
          begin
{$ifdef GDB}
            if cs_gdb_dbx in aktglobalswitches then
             flags:=flags or uf_has_dbx;
{$endif GDB}
            if target_os.endian=endian_big then
             flags:=flags or uf_big_endian;
            if cs_browser in aktmoduleswitches then
             flags:=flags or uf_has_browser;
            if cs_local_browser in aktmoduleswitches then
             flags:=flags or uf_local_browser;
          end;

{$ifdef Test_Double_checksum_write}
        If only_crc then
          Assign(CRCFile,s+'.INT')
        else
          Assign(CRCFile,s+'.IMP');
        Rewrite(CRCFile);
{$endif def Test_Double_checksum_write}
       { open ppufile }
         current_ppu:=new(pppufile,init(s));
         current_ppu^.crc_only:=only_crc;
         if not current_ppu^.create then
           Message(unit_f_ppu_cannot_write);

{$ifdef Test_Double_checksum}
         if only_crc then
           begin
              new(current_ppu^.crc_test);
              new(current_ppu^.crc_test2);
           end
         else
           begin
             current_ppu^.crc_test:=Current_Module^.crc_array;
             current_ppu^.crc_index:=Current_Module^.crc_size;
             current_ppu^.crc_test2:=Current_Module^.crc_array2;
             current_ppu^.crc_index2:=Current_Module^.crc_size2;
           end;
{$endif def Test_Double_checksum}

         current_ppu^.change_endian:=source_os.endian<>target_os.endian;
       { write symbols and definitions }
         unittable^.writeasunit;

       { flush to be sure }
         current_ppu^.flush;
       { create and write header }
         current_ppu^.header.size:=current_ppu^.size;
         current_ppu^.header.checksum:=current_ppu^.crc;
         current_ppu^.header.interface_checksum:=current_ppu^.interface_crc;
         current_ppu^.header.compiler:=wordversion;
         current_ppu^.header.cpu:=word(target_cpu);
         current_ppu^.header.target:=word(target_info.target);
         current_ppu^.header.flags:=current_module^.flags;
         If not only_crc then
           current_ppu^.writeheader;
       { save crc in current_module also }
         current_module^.crc:=current_ppu^.crc;
         current_module^.interface_crc:=current_ppu^.interface_crc;
         if only_crc then
          begin
{$ifdef Test_Double_checksum}
            Current_Module^.crc_array:=current_ppu^.crc_test;
            current_ppu^.crc_test:=nil;
            Current_Module^.crc_size:=current_ppu^.crc_index2;
            Current_Module^.crc_array2:=current_ppu^.crc_test2;
            current_ppu^.crc_test2:=nil;
            Current_Module^.crc_size2:=current_ppu^.crc_index2;
{$endif def Test_Double_checksum}
            closecurrentppu;
          end;
{$ifdef Test_Double_checksum_write}
        close(CRCFile);
{$endif Test_Double_checksum_write}
      end;


    procedure closecurrentppu;
      begin
{$ifdef Test_Double_checksum}
         if assigned(current_ppu^.crc_test) then
           dispose(current_ppu^.crc_test);
         if assigned(current_ppu^.crc_test2) then
           dispose(current_ppu^.crc_test2);
{$endif Test_Double_checksum}
       { close }
         current_ppu^.close;
         dispose(current_ppu,done);
         current_ppu:=nil;
      end;


{*****************************************************************************
                                 PPU Reading
*****************************************************************************}

    function readbyte:byte;
      begin
        readbyte:=current_ppu^.getbyte;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readword:word;
      begin
        readword:=current_ppu^.getword;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readlong:longint;
      begin
        readlong:=current_ppu^.getlongint;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readreal : bestreal;
      begin
        readreal:=current_ppu^.getreal;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readstring : string;
      begin
        readstring:=current_ppu^.getstring;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readnormalset(var s);   {You cannot pass an array [0..31] of byte.}
      begin
        current_ppu^.getdata(s,sizeof(tnormalset));
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readsmallset(var s);
      begin
        current_ppu^.getdata(s,4);
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readposinfo(var p:tfileposinfo);
      begin
        p.fileindex:=current_ppu^.getword;
        p.line:=current_ppu^.getlongint;
        p.column:=current_ppu^.getword;
      end;


    function readderef : pderef;
      var
        hp,p : pderef;
        b : tdereftype;
      begin
        p:=nil;
        repeat
          hp:=p;
          b:=tdereftype(current_ppu^.getbyte);
          case b of
            derefnil :
              break;
            derefunit,
            derefaktrecordindex,
            derefaktlocal,
            derefaktstaticindex :
              begin
                new(p,init(b,current_ppu^.getword));
                p^.next:=hp;
                break;
              end;
            derefindex,
            dereflocal,
            derefpara,
            derefrecord :
              begin
                new(p,init(b,current_ppu^.getword));
                p^.next:=hp;
              end;
          end;
        until false;
        readderef:=p;
      end;

    function readdefref : pdef;
      begin
        readdefref:=pdef(readderef);
      end;

    function readsymref : psym;
      begin
        readsymref:=psym(readderef);
      end;

    procedure readusedmacros;
      var
        hs : string;
        mac : pmacrosym;
        was_defined_at_startup,
        was_used : boolean;
      begin
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
           was_defined_at_startup:=boolean(current_ppu^.getbyte);
           was_used:=boolean(current_ppu^.getbyte);
           mac:=pmacrosym(macros^.search(hs));
           if assigned(mac) then
             begin
{$ifndef EXTDEBUG}
           { if we don't have the sources why tell }
              if current_module^.sources_avail then
{$endif ndef EXTDEBUG}
               if not was_defined_at_startup and was_used and
                 mac^.defined_at_startup then
                 Comment(V_Hint,'Conditional '+hs+' was not set at startup '+
                 'in last compilation of '+current_module^.mainsource^);
             end
           else { not assigned }
             if was_defined_at_startup and was_used then
               Comment(V_Hint,'Conditional '+hs+' was set at startup '+
               'in last compilation of '+current_module^.mainsource^);
         end;
      end;

    procedure readsourcefiles;
      var
        temp,hs       : string;
{$ifdef ORDERSOURCES}
        main_dir      : string;
{$endif ORDERSOURCES}
        incfile_found,
        main_found,
        is_main       : boolean;
        ppufiletime,
        source_time   : longint;
        hp            : pinputfile;
      begin
        ppufiletime:=getnamedfiletime(current_module^.ppufilename^);
        current_module^.sources_avail:=true;
{$ifdef ORDERSOURCES}
        is_main:=true;
        main_dir:='';
{$endif ORDERSOURCES}
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
{$ifndef ORDERSOURCES}
           is_main:=current_ppu^.endofentry;
{$endif ORDERSOURCES}
           temp:='';
           if (current_module^.flags and uf_in_library)<>0 then
            begin
              current_module^.sources_avail:=false;
              temp:=' library';
            end
           else if pos('Macro ',hs)=1 then
            begin
              { we don't want to find this file }
              { but there is a problem with file indexing !! }
              temp:='';
            end
           else
            begin
              { check the date of the source files }
              Source_Time:=GetNamedFileTime(current_module^.path^+hs);
              incfile_found:=false;
              if Source_Time<>-1 then
                hs:=current_module^.path^+hs
{$ifdef ORDERSOURCES}
              else if not(is_main) then
                begin
                  Source_Time:=GetNamedFileTime(main_dir+hs);
                  if Source_Time<>-1 then
                    hs:=main_dir+hs;
                end
{$endif def ORDERSOURCES}
                   ;
              if (Source_Time=-1) then
                begin
                  if is_main then
                    temp:=unitsearchpath.FindFile(hs,main_found)
                  else
                    temp:=includesearchpath.FindFile(hs,incfile_found);
                  if incfile_found or main_found then
                   begin
                     hs:=temp+hs;
                     Source_Time:=GetNamedFileTime(hs);
                   end
                end;
              if Source_Time=-1 then
               begin
                 current_module^.sources_avail:=false;
                 temp:=' not found';
               end
              else
               begin
                 { time newer? But only allow if the file is not searched
                   in the include path (PFV), else you've problems with
                   units which use the same includefile names }
                 if incfile_found then
                  temp:=' found'
                 else
                  begin
                    temp:=' time '+filetimestring(source_time);
                    if (source_time>ppufiletime) then
                     begin
                       current_module^.do_compile:=true;
                       current_module^.recompile_reason:=rr_sourcenewer;
                       temp:=temp+' *'
                     end;
                  end;
               end;
              new(hp,init(hs));
              { the indexing is wrong here PM }
              current_module^.sourcefiles^.register_file(hp);
            end;
{$ifdef ORDERSOURCES}
           if is_main then
             begin
               stringdispose(current_module^.mainsource);
               current_module^.mainsource:=stringdup(hs);
               if main_found then
                 main_dir:=temp;
             end;
{$endif ORDERSOURCES}
           Message1(unit_u_ppu_source,hs+temp);
{$ifdef ORDERSOURCES}
           is_main:=false;
{$endif ORDERSOURCES}
         end;
{$ifndef ORDERSOURCES}
      { main source is always the last }
        stringdispose(current_module^.mainsource);
        current_module^.mainsource:=stringdup(hs);

        { the indexing is corrected here PM }
        current_module^.sourcefiles^.inverse_register_indexes;
{$endif ORDERSOURCES}
      { check if we want to rebuild every unit, only if the sources are
        available }
        if do_build and current_module^.sources_avail then
          begin
             current_module^.do_compile:=true;
             current_module^.recompile_reason:=rr_build;
          end;
      end;


    procedure readloadunit;
      var
        hs : string;
        intfchecksum,
        checksum : longint;
        in_interface : boolean;
      begin
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
           checksum:=current_ppu^.getlongint;
           intfchecksum:=current_ppu^.getlongint;
           in_interface:=(current_ppu^.getbyte<>0);
           current_module^.used_units.concat(new(pused_unit,init_to_load(hs,checksum,intfchecksum,in_interface)));
         end;
      end;


    procedure readlinkcontainer(var p:tlinkcontainer);
      var
        s : string;
        m : longint;
      begin
        while not current_ppu^.endofentry do
         begin
           s:=current_ppu^.getstring;
           m:=current_ppu^.getlongint;
           p.insert(s,m);
         end;
      end;


    procedure load_interface;
      var
        b : byte;
        newmodulename : pstring;
      begin
       { read interface part }
         repeat
           b:=current_ppu^.readentry;
           case b of
             ibmodulename :
               begin
                 newmodulename:=stringdup(current_ppu^.getstring);
                 if newmodulename^<>current_module^.modulename^ then
                   Message2(unit_f_unit_name_error,current_module^.modulename^,newmodulename^);
                 stringdispose(current_module^.modulename);
                 current_module^.modulename:=newmodulename;
               end;
             ibsourcefiles :
               readsourcefiles;
             ibusedmacros :
               readusedmacros;
             ibloadunit :
               readloadunit;
             iblinkunitofiles :
               readlinkcontainer(current_module^.LinkUnitOFiles);
             iblinkunitstaticlibs :
               readlinkcontainer(current_module^.LinkUnitStaticLibs);
             iblinkunitsharedlibs :
               readlinkcontainer(current_module^.LinkUnitSharedLibs);
             iblinkotherofiles :
               readlinkcontainer(current_module^.LinkotherOFiles);
             iblinkotherstaticlibs :
               readlinkcontainer(current_module^.LinkotherStaticLibs);
             iblinkothersharedlibs :
               readlinkcontainer(current_module^.LinkotherSharedLibs);
             ibendinterface :
               break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;

{
  $Log$
  Revision 1.2  2000-07-13 11:32:49  michael
  + removed logs
 
}

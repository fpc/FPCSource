{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

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


{$ifndef OLDPPU}

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


    procedure writedouble(d:double);
      begin
        current_ppu^.putdata(d,sizeof(double));
      end;


    procedure writestring(const s:string);
      begin
        current_ppu^.putstring(s);
      end;


    procedure writeset(var s); {You cannot pass an array[0..31] of byte!}
      begin
        current_ppu^.putdata(s,32);
      end;


    procedure writecontainer(var p:tstringcontainer;id:byte;hold:boolean);
      var
        hcontainer : tstringcontainer;
        s          : string;
      begin
        if hold then
         hcontainer.init;
        while not p.empty do
         begin
           s:=p.get;
           current_ppu^.putstring(s);
           if hold then
            hcontainer.insert(s);
         end;
        current_ppu^.writeentry(id);
        if hold then
         p:=hcontainer;
      end;


    procedure writeposinfo(const p:tfileposinfo);
      begin
        current_ppu^.putword(p.fileindex);
        current_ppu^.putlongint(p.line);
        current_ppu^.putword(p.column);
      end;


    procedure writedefref(p : pdef);
      begin
        if p=nil then
         current_ppu^.putlongint($ffffffff)
        else
         begin
           if (p^.owner^.symtabletype in [recordsymtable,objectsymtable]) then
            current_ppu^.putword($ffff)
           else
            current_ppu^.putword(p^.owner^.unitid);
           current_ppu^.putword(p^.indexnb);
         end;
      end;


    procedure writesymref(p : psym);
      begin
        if p=nil then
         current_ppu^.putlongint($ffffffff)
        else
         begin
           if (p^.owner^.symtabletype in [recordsymtable,objectsymtable]) then
            current_ppu^.putword($ffff)
           else
            current_ppu^.putword(p^.owner^.unitid);
           current_ppu^.putword(p^.indexnb);
         end;
      end;


    procedure writesourcefiles;
      var
        hp    : pinputfile;
        index : longint;
      begin
      { second write the used source files }
        hp:=current_module^.sourcefiles.files;
        index:=current_module^.sourcefiles.last_ref_index;
        while assigned(hp) do
         begin
         { only name and extension }
           current_ppu^.putstring(hp^.name^);
         { index in that order }
           hp^.ref_index:=index;
           dec(index);
           hp:=hp^.ref_next;
         end;
        current_ppu^.writeentry(ibsourcefiles);
      end;


    procedure writeusedunit;
      var
        hp      : pused_unit;
      begin
      numberunits;
        hp:=pused_unit(current_module^.used_units.first);
        while assigned(hp) do
         begin
           current_ppu^.putstring(hp^.name^);
           current_ppu^.putlongint(hp^.checksum);
           current_ppu^.putbyte(byte(hp^.in_interface));
           hp:=pused_unit(hp^.next);
         end;
        current_ppu^.writeentry(ibloadunit_int);
      end;


    procedure writeunitas(const s : string;unittable : punitsymtable);
      begin
         Message1(unit_u_ppu_write,s);

       { create unit flags }
         with Current_Module^ do
          begin
            if cs_smartlink in aktmoduleswitches then
             begin
               flags:=flags or uf_smartlink;
               if SplitName(ppufilename^)<>SplitName(libfilename^) then
                 flags:=flags or uf_in_library;
             end;
            if use_dbx then
             flags:=flags or uf_has_dbx;
            if target_os.endian=en_big_endian then
             flags:=flags or uf_big_endian;
{$ifdef UseBrowser}
            if cs_browser in aktswitches then
             flags:=flags or uf_has_browser;
{$endif UseBrowser}
          end;

       { open ppufile }
         current_ppu:=new(pppufile,init(s));
         current_ppu^.change_endian:=source_os.endian<>target_os.endian;
         if not current_ppu^.create then
          Message(unit_f_ppu_cannot_write);

       { write symbols and definitions }
         unittable^.writeasunit;

       { flush to be sure }
         current_ppu^.flush;
       { create and write header }
         current_ppu^.header.size:=current_ppu^.size;
         current_ppu^.header.checksum:=current_ppu^.crc;
         current_ppu^.header.compiler:=wordversion;
         current_ppu^.header.cpu:=word(target_cpu);
         current_ppu^.header.target:=word(target_info.target);
         current_ppu^.header.flags:=current_module^.flags;
         current_ppu^.writeheader;
       { save crc in current_module also }
         current_module^.crc:=current_ppu^.crc;
       { close }
         current_ppu^.close;
         dispose(current_ppu,done);
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


    function readdouble : double;
      var
         d : double;
      begin
        current_ppu^.getdata(d,sizeof(double));
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
        readdouble:=d;
      end;


    function readstring : string;
      begin
        readstring:=current_ppu^.getstring;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readset(var s);   {You cannot pass an array [0..31] of byte.}
      begin
        current_ppu^.getdata(s,32);
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readcontainer(var p:tstringcontainer);
      begin
        while not current_ppu^.endofentry do
         p.insert(current_ppu^.getstring);
      end;


    procedure readposinfo(var p:tfileposinfo);
      begin
        p.fileindex:=current_ppu^.getword;
        p.line:=current_ppu^.getlongint;
        p.column:=current_ppu^.getword;
      end;


    function readdefref : pdef;
      var
        hd : pdef;
      begin
        longint(hd):=current_ppu^.getword;
        longint(hd):=longint(hd) or (longint(current_ppu^.getword) shl 16);
        readdefref:=hd;
      end;


{$ifdef UseBrowser}
    function readsymref : psym;
      var
        hd : psym;
      begin
        longint(hd):=current_ppu^.getword;
        longint(hd):=longint(hd) or (longint(current_ppu^.getword) shl 16);
        readsymref:=hd;
      end;
{$endif}


    procedure readsourcefiles;
      var
        temp,hs       : string;
        incfile_found : boolean;
        ppufiletime,
        source_time   : longint;
{$ifdef UseBrowser}
        hp : pextfile;
        _d,_n,_e : string;
{$endif UseBrowser}
      begin
        ppufiletime:=getnamedfiletime(current_module^.ppufilename^);
        current_module^.sources_avail:=true;
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
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
              if Source_Time=-1 then
                begin
                { search for include files in the includepathlist }
                  temp:=search(hs,includesearchpath,incfile_found);
                  if incfile_found then
                   begin
                     hs:=temp+hs;
                     Source_Time:=GetNamedFileTime(hs);
                   end;
                end
              else
                hs:=current_module^.path^+hs;
              if Source_Time=-1 then
               begin
                 current_module^.sources_avail:=false;
                 temp:=' not found';
               end
              else
               begin
                 temp:=' time '+filetimestring(source_time);
                 if (source_time>ppufiletime) then
                  begin
                    current_module^.do_compile:=true;
                    temp:=temp+' *'
                  end;
               end;
            end;
           Message1(unit_t_ppu_source,hs+temp);
{$ifdef UseBrowser}
           fsplit(hs,_d,_n,_e);
           new(hp,init(_d,_n,_e));
           { the indexing should match what is done in writeasunit }
           current_module^.sourcefiles.register_file(hp);
{$endif UseBrowser}
         end;
      { main source is always the last }
        stringdispose(current_module^.mainsource);
        current_module^.mainsource:=stringdup(hs);
      { check if we want to rebuild every unit, only if the sources are
        available }
        if do_build and current_module^.sources_avail then
         current_module^.do_compile:=true;
      end;


    procedure readloadunit;
      var
        hs : string;
        checksum : longint;
        in_interface : boolean;
      begin
        while not current_ppu^.endofentry do
         begin
           hs:=current_ppu^.getstring;
           checksum:=current_ppu^.getlongint;
           in_interface:=(current_ppu^.getbyte<>0);
           current_module^.used_units.concat(new(pused_unit,init_to_load(hs,checksum,in_interface)));
         end;
      end;


    procedure load_interface;
      var
        b : byte;
      begin
       { read interface part }
         repeat
           b:=current_ppu^.readentry;
           case b of
            { ibinitunit : usedunits^.insert(readstring); }
            ibmodulename : begin
                             stringdispose(current_module^.modulename);
                             current_module^.modulename:=stringdup(current_ppu^.getstring);
                           end;
           ibsourcefiles : readsourcefiles;
          ibloadunit_int : readloadunit;
        iblinksharedlibs : readcontainer(current_module^.LinkSharedLibs);
        iblinkstaticlibs : readcontainer(current_module^.LinkStaticLibs);
            iblinkofiles : readcontainer(current_module^.LinkOFiles);
          ibendinterface : break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;


{$else OLDPPU}


{*****************************************************************************

                               Old PPU

*****************************************************************************}

    function readbyte : byte;

      var
         count : longint;
         b : byte;

      begin
         current_module^.ppufile^.read_data(b,sizeof(byte),count);
         readbyte:=b;
         if count<>1 then
           Message(unit_f_ppu_read_error);
      end;

    function readword : word;

      var
         count : longint;
         w : word;

      begin
         current_module^.ppufile^.read_data(w,sizeof(word),count);
         readword:=w;
         if count<>sizeof(word) then
           Message(unit_f_ppu_read_error);
      end;

    function readlong : longint;

      var
         count,l : longint;

      begin
         current_module^.ppufile^.read_data(l,sizeof(longint),count);
         readlong:=l;
         if count<>sizeof(longint) then
           Message(unit_f_ppu_read_error);
      end;

    function readdouble : double;

      var
         count : longint;
         d : double;

      begin
         current_module^.ppufile^.read_data(d,sizeof(double),count);
         readdouble:=d;
         if count<>sizeof(double) then
           Message(unit_f_ppu_read_error);
      end;

    function readstring : string;

      var
         s : string;
         count : longint;

      begin
         s[0]:=char(readbyte);
         current_module^.ppufile^.read_data(s[1],ord(s[0]),count);
         if count<>ord(s[0]) then
           Message(unit_f_ppu_read_error);
         readstring:=s;
      end;

{***SETCONST}
    procedure readset(var s);   {You cannot pass an array [0..31] of byte.}

    var count:longint;

      begin
         current_module^.ppufile^.read_data(s,32,count);
         if count<>32 then
           Message(unit_f_ppu_read_error);
      end;
{***}

    procedure readposinfo(var p:tfileposinfo);
      begin
        p.fileindex:=readword;
        p.line:=readlong;
        p.column:=readword;
      end;


    function readdefref : pdef;
      var
         hd : pdef;
      begin
         longint(hd):=readword;
         longint(hd):=longint(hd) or (longint(readword) shl 16);
         readdefref:=hd;
      end;

{$ifdef UseBrowser}
    function readsymref : psym;
      var
         hd : psym;
      begin
         longint(hd):=readword;
         longint(hd):=longint(hd) or (longint(readword) shl 16);
         readsymref:=hd;
      end;
{$endif UseBrowser}

    procedure writebyte(b:byte);
      begin
        ppufile.write_data(b,1);
      end;

    procedure writeword(w:word);
      begin
        ppufile.write_data(w,2);
      end;

    procedure writelong(l:longint);
      begin
        ppufile.write_data(l,4);
      end;

    procedure writedouble(d:double);
      begin
        ppufile.write_data(d,sizeof(double));
      end;

    procedure writestring(s : string);
      begin
        ppufile.write_data(s,length(s)+1);
      end;

    procedure writeset(var s); {You cannot pass an array[0..31] of byte!}
      begin
        ppufile.write_data(s,32);
      end;

    procedure writecontainer(var p:tstringcontainer;id:byte;hold:boolean);
      var
        hcontainer : tstringcontainer;
        s          : string;
      begin
        if hold then
         hcontainer.init;
        while not p.empty do
         begin
           writebyte(id);
           s:=p.get;
           writestring(s);
           if hold then
            hcontainer.insert(s);
         end;
        if hold then
         p:=hcontainer;
      end;


    procedure writeposinfo(const p:tfileposinfo);
      begin
        writeword(p.fileindex);
        writelong(p.line);
        writeword(p.column);
      end;

    procedure writedefref(p : pdef);
      begin
        if p=nil then
         writelong($ffffffff)
        else
         begin
           if (p^.owner^.symtabletype in [recordsymtable,objectsymtable]) then
            writeword($ffff)
           else
            writeword(p^.owner^.unitid);
           writeword(p^.indexnb);
         end;
      end;

    procedure writesymref(p : psym);
      begin
        if p=nil then
         writelong($ffffffff)
        else
         begin
           if (p^.owner^.symtabletype in [recordsymtable,objectsymtable]) then
            writeword($ffff)
           else
            writeword(p^.owner^.unitid);
           writeword(p^.indexnb);
         end;
      end;

    procedure writeunitas(const s : string;unittable : punitsymtable);
{$ifdef UseBrowser}
      var
         pus : punitsymtable;
{$endif UseBrowser}
      begin
         Message1(unit_u_ppu_write,s);

       { create unit flags }
         with Current_Module^ do
          begin
            if cs_smartlink in aktswitches then
             begin
               flags:=flags or uf_smartlink;
               if SplitName(ppufilename^)<>SplitName(libfilename^) then
                 flags:=flags or uf_in_library;
             end;
            if use_dbx then
             flags:=flags or uf_has_dbx;
            if target_os.endian=en_big_endian then
             flags:=flags or uf_big_endian;
{$ifdef UseBrowser}
            if use_browser then
             flags:=flags or uf_uses_browser;
{$endif UseBrowser}
          end;

       { open en init ppufile }
         ppufile.init(s,ppubufsize);
         ppufile.change_endian:=source_os.endian<>target_os.endian;
         ppufile.rewrite;
         if ioresult<>0 then
          Message(unit_f_ppu_cannot_write);
       { create and write header }
         unitheader[8]:=char(byte(target_info.target));
         unitheader[9]:=char(current_module^.flags);
         ppufile.write_data(unitheader,sizeof(unitheader));
         ppufile.clear_crc;
         ppufile.do_crc:=true;
         unittable^.writeasunit;
         ppufile.flush;
         ppufile.do_crc:=false;
{$ifdef UseBrowser}
         { write all new references to old unit elements }
         pus:=punitsymtable(unittable^.next);
         if use_browser then
         while assigned(pus) do
           begin
              if pus^.symtabletype = unitsymtable then
                pus^.write_external_references;
              pus:=punitsymtable(pus^.next);
           end;
{$endif UseBrowser}
         { writes the checksum }
         ppufile.seek(10);
         current_module^.crc:=ppufile.getcrc;
         ppufile.write_data(current_module^.crc,4);
         ppufile.flush;
         ppufile.done;
      end;



{$endif OLDPPU}

{
  $Log$
  Revision 1.8  1998-08-10 14:50:29  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.7  1998/07/14 14:47:07  peter
    * released NEWINPUT

  Revision 1.6  1998/07/07 11:20:14  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.5  1998/06/24 14:48:39  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.4  1998/06/16 08:56:32  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.3  1998/06/13 00:10:17  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.2  1998/05/28 14:40:28  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.1  1998/05/27 19:45:09  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

}


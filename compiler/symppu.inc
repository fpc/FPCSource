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


    procedure writecontainer(var p:tstringcontainer;id:byte;hold,strippath:boolean);
      var
        hcontainer : tstringcontainer;
        s          : string;
      begin
        if hold then
         hcontainer.init;
        while not p.empty do
         begin
           s:=p.get;
           if strippath then
            current_ppu^.putstring(SplitFileName(s))
           else
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
      begin
      { second write the used source files }
        current_ppu^.do_crc:=false;
        hp:=current_module^.sourcefiles^.files;
        while assigned(hp) do
         begin
         { only name and extension }
           current_ppu^.putstring(hp^.name^);
           hp:=hp^.ref_next;
         end;
        current_ppu^.writeentry(ibsourcefiles);
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
           current_ppu^.putstring(hp^.name^);
           { the checksum should not affect the crc of this unit ! (PFV) }
           current_ppu^.do_crc:=false;
           current_ppu^.putlongint(hp^.checksum);
           current_ppu^.do_crc:=true;
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
            if (((cs_create_staticlib in aktmoduleswitches) or
                 (cs_smartlink in aktmoduleswitches)) and
                (SplitName(ppufilename^)<>SplitName(staticlibfilename^))) or

               ((cs_create_sharedlib in aktmoduleswitches) and
                (SplitName(ppufilename^)<>SplitName(sharedlibfilename^))) then
             flags:=flags or uf_in_library;
            if cs_smartlink in aktmoduleswitches then
             flags:=flags or uf_smartlink;
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

       { open ppufile }
         current_ppu:=new(pppufile,init(s));
         if not current_ppu^.create then
          Message(unit_f_ppu_cannot_write);

         current_ppu^.change_endian:=source_os.endian<>target_os.endian;
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


    function readsymref : psym;
      var
        hd : psym;
      begin
        longint(hd):=current_ppu^.getword;
        longint(hd):=longint(hd) or (longint(current_ppu^.getword) shl 16);
        readsymref:=hd;
      end;


    procedure readsourcefiles;
      var
        temp,hs       : string;
        incfile_found : boolean;
        ppufiletime,
        source_time   : longint;
        hp            : pinputfile;
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
              incfile_found:=false;
              if (Source_Time=-1) then
                begin
                  temp:=search(hs,includesearchpath,incfile_found);
                  if incfile_found then
                   begin
                     hs:=temp+hs;
                     Source_Time:=GetNamedFileTime(hs);
                   end
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
                       temp:=temp+' *'
                     end;
                  end;
               end;
              new(hp,init(hs));
              { the indexing is wrong here PM }
              current_module^.sourcefiles^.register_file(hp);
            end;
           Message1(unit_u_ppu_source,hs+temp);
         end;
      { main source is always the last }
        stringdispose(current_module^.mainsource);
        current_module^.mainsource:=stringdup(hs);

        { the indexing is corrected here PM }
        current_module^.sourcefiles^.inverse_register_indexes;
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
         iblinkunitfiles : readcontainer(current_module^.LinkUnitFiles);
            iblinkofiles : readcontainer(current_module^.LinkOFiles);
          ibendinterface : break;
           else
             Message1(unit_f_ppu_invalid_entry,tostr(b));
           end;
         until false;
      end;

{
  $Log$
  Revision 1.31  1999-02-16 00:48:25  peter
    * save in the ppu if linked with obj file instead of using the
      library flag, so the .inc files are also checked

  Revision 1.30  1999/02/05 08:54:30  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.29  1999/01/20 10:16:46  peter
    * don't update crc when writing objs,libs and sources

  Revision 1.28  1999/01/12 14:25:35  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.27  1998/12/08 10:18:14  peter
    + -gh for heaptrc unit

  Revision 1.26  1998/11/26 14:36:02  peter
    * set also library flag if smartlinking and outputname is different

  Revision 1.25  1998/10/26 09:35:47  peter
    * don't count includefiles which are found in the includepath for a
      recompile.

  Revision 1.24  1998/10/20 08:06:59  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.23  1998/10/16 13:37:24  florian
    + switch -FD added to specify the path for utilities

  Revision 1.22  1998/10/14 13:38:24  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.21  1998/10/14 10:45:10  pierre
    * ppu problems for m68k fixed (at least in cross compiling)
    * one last memory leak for sysamiga fixed
    * the amiga RTL compiles now completely !!

  Revision 1.20  1998/10/13 13:10:30  peter
    * new style for m68k/i386 infos and enums

  Revision 1.19  1998/10/08 23:29:07  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.18  1998/09/28 16:57:27  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.17  1998/09/22 17:13:53  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.16  1998/09/22 15:40:56  peter
    * some extra ifdef GDB

  Revision 1.15  1998/09/21 08:45:23  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.14  1998/09/01 07:54:24  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.13  1998/08/17 10:10:11  peter
    - removed OLDPPU

  Revision 1.12  1998/08/17 09:17:53  peter
    * static/shared linking updates

  Revision 1.11  1998/08/16 20:32:49  peter
    * crcs of used units are not important for the current crc, reduces the
      amount of recompiles

  Revision 1.10  1998/08/13 10:57:30  peter
    * constant sets are now written correctly to the ppufile

  Revision 1.9  1998/08/11 15:31:41  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.8  1998/08/10 14:50:29  peter
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


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

{$ifdef NEWPPU}

    procedure writebyte(b:byte);
      begin
        ppufile^.putbyte(b);
      end;

    procedure writeword(w:word);
      begin
        ppufile^.putword(w);
      end;

    procedure writelong(l:longint);
      begin
        ppufile^.putlongint(l);
      end;

    procedure writedouble(d:double);
      begin
        ppufile^.putdata(d,sizeof(double));
      end;

    procedure writestring(const s:string);
      begin
        ppufile^.putstring(s);
      end;

    procedure writeset(var s); {You cannot pass an array[0..31] of byte!}
      begin
        ppufile^.putdata(s,32);
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
           ppufile^.putstring(s);
           if hold then
            hcontainer.insert(s);
         end;
        ppufile^.writeentry(id);
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
         ppufile^.putlongint($ffffffff)
        else
         begin
           if (p^.owner^.symtabletype in [recordsymtable,objectsymtable]) then
            ppufile^.putword($ffff)
           else
            ppufile^.putword(p^.owner^.unitid);
           ppufile^.putword(p^.number);
         end;
      end;


{$ifdef UseBrowser}
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
{$endif UseBrowser}

    procedure writeunitas(const s : string;unit_symtable : punitsymtable);
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
             flags:=flags or uf_uses_dbx;
            if target_os.endian=en_big_endian then
             flags:=flags or uf_big_endian;
{$ifdef UseBrowser}
            if use_browser then
             flags:=flags or uf_uses_browser;
{$endif UseBrowser}
          end;

         ppufile:=new(pppufile,init(s));
         ppufile^.change_endian:=source_os.endian<>target_os.endian;
         if not ppufile^.create then
          Message(unit_f_ppu_cannot_write);
         unit_symtable^.writeasunit;
{$ifdef UseBrowser}
         { write all new references to old unit elements }
         pus:=punitsymtable(unit_symtable^.next);
         if use_browser then
         while assigned(pus) do
           begin
              if pus^.symtabletype = unitsymtable then
                pus^.write_external_references;
              pus:=punitsymtable(pus^.next);
           end;
{$endif UseBrowser}
         ppufile^.flush;
       { create and write header }
         ppufile^.header.size:=ppufile^.size;
         ppufile^.header.checksum:=ppufile^.crc;
         ppufile^.header.compiler:=wordversion;
         ppufile^.header.target:=word(target_info.target);
         ppufile^.header.flags:=current_module^.flags;
         ppufile^.writeheader;
       { save crc in current_module also }
         current_module^.crc:=ppufile^.crc;
       { close }
         ppufile^.close;
         dispose(ppufile,done);
      end;


{$else NEWPPU}

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
           writeword(p^.number);
         end;
      end;


{$ifdef UseBrowser}
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
{$endif UseBrowser}


    procedure writeunitas(const s : string;unit_symtable : punitsymtable);
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
             flags:=flags or uf_uses_dbx;
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
         unit_symtable^.writeasunit;
         ppufile.flush;
         ppufile.do_crc:=false;
{$ifdef UseBrowser}
         { write all new references to old unit elements }
         pus:=punitsymtable(unit_symtable^.next);
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

{$endif NEWPPU}

{*****************************************************************************
                                 PPU Reading
*****************************************************************************}

{$ifdef NEWPPU}
    function readbyte:byte;
      begin
        readbyte:=ppufile^.getbyte;
        if ppufile^.error then
         Message(unit_f_ppu_read_error);
      end;

    function readword:word;
      begin
        readword:=ppufile^.getword;
        if ppufile^.error then
         Message(unit_f_ppu_read_error);
      end;

    function readlong:longint;
      begin
        readlong:=ppufile^.getlongint;
        if ppufile^.error then
         Message(unit_f_ppu_read_error);
      end;

    function readdouble : double;
      var
         d : double;
      begin
        ppufile^.getdata(d,sizeof(double));
        if ppufile^.error then
         Message(unit_f_ppu_read_error);
        readdouble:=d;
      end;

    function readstring : string;
      begin
        readstring:=ppufile^.getstring;
        if ppufile^.error then
         Message(unit_f_ppu_read_error);
      end;

    procedure readset(var s);   {You cannot pass an array [0..31] of byte.}
      begin
        ppufile^.getdata(s,32);
        if ppufile^.error then
         Message(unit_f_ppu_read_error);
      end;

    procedure readcontainer(var p:tstringcontainer);
      begin
        while not current_module^.ppufile^.endofentry do
         p.insert(current_module^.ppufile^.getstring);
      end;

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
{$endif}


{$else NEWPPU}


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

{$endif NEWPPU}

{
  $Log$
  Revision 1.2  1998-05-28 14:40:28  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.1  1998/05/27 19:45:09  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifdef NEWPPU

}
  
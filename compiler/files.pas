{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

    This unit implements an extended file management and the first loading
    and searching of the modules (ppufiles)

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
unit files;

  interface

    uses
       cobjects,globals
{$ifndef OLDPPU}
       ,ppu
{$endif}
       ;

    const
{$ifdef FPC}
       maxunits = 1024;
       extbufsize = 65535;
{$else}
       maxunits = 128;
       {$ifndef msdos}
       extbufsize = 2000;
       {$else}
       extbufsize=512;
       {$endif dpmi}
{$endif}

    type
       { this isn't a text file, this is t-ext-file }
       { which means a extended file this files can }
       { be handled by a file manager               }
       pextfile = ^textfile;
       textfile = object(tbufferedfile)
          path,name,ext : pstring;
          _next      : pextfile; { else conflicts with tinputstack }
          ref_index  : word;     { 65000 input files for a unit should be enough !! }
          { p must be the complete path (with ending \ (or / for unix ...) }
          constructor init(const p,n,e : string);
          destructor done;virtual;
       end;

       pinputfile = ^tinputfile;
       tinputfile = object(textfile)
          filenotatend : boolean;
          line_no      : longint;    { position to give out }
          true_line    : longint;    { real line counter }
          column       : longint;
          next         : pinputfile; { next input file in the stack of input files }
          ref_count    : longint;    { to handle the browser refs }
          constructor init(const p,n,e : string);
          procedure write_file_line(var t : text); { writes the file name and line number to t }
          function  get_file_line : string;
       end;

       pfilemanager = ^tfilemanager;
       tfilemanager = object
          files : pextfile;
          last_ref_index : word;
          constructor init;
          destructor done;
          procedure close_all;
          procedure register_file(f : pextfile);
          function  get_file(w : word) : pextfile;
       end;

    type
       tunitmap = array[0..maxunits-1] of pointer;
       punitmap = ^tunitmap;

       pmodule = ^tmodule;
       tmodule = object(tlinkedlist_item)
{$ifndef OLDPPU}
          ppufile       : pppufile; { the PPU file }
{$else}
          ppufile       : pextfile; { the PPU file }
{$endif}
          crc,
          flags         : longint;  { the PPU flags }

          compiled,                 { unit is already compiled }
          do_assemble,              { only assemble the object, don't recompile }
          do_compile,               { need to compile the sources }
          sources_avail,            { if all sources are reachable }
          is_unit,
          in_implementation,        { processing the implementation part? }
          in_main       : boolean;  { global, after uses else false }

          map           : punitmap; { mapping of all used units }
          unitcount     : word;     { local unit counter }
          unit_index    : word;     { global counter for browser }
          symtable      : pointer;  { pointer to the psymtable of this unit }

          uses_imports  : boolean;  { Set if the module imports from DLL's.}
          imports       : plinkedlist;

          sourcefiles   : tfilemanager;
          linksharedlibs,
          linkstaticlibs,
          linkofiles    : tstringcontainer;
          used_units    : tlinkedlist;
          current_inputfile : pinputfile;
          { used in firstpass for faster settings }
          current_index : word;

          path,                     { path where the module is find/created }
          modulename,               { name of the module in uppercase }
          objfilename,              { fullname of the objectfile }
          asmfilename,              { fullname of the assemblerfile }
          ppufilename,              { fullname of the ppufile }
          libfilename,              { fullname of the libraryfile }
          mainsource    : pstring;  { name of the main sourcefile }

          constructor init(const s:string;_is_unit:boolean);
{$ifndef OLDPPU}
          destructor done;virtual;
{$else}
          destructor special_done;virtual; { this is to be called only when compiling again }
{$endif OLDPPU}
          procedure setfilename(const _path,name:string);
{$ifndef OLDPPU}
          function  openppu:boolean;
{$else}
          function  load_ppu(const unit_path,n,ext:string):boolean;
{$endif}
          function  search_unit(const n : string):boolean;
       end;

       pused_unit = ^tused_unit;
       tused_unit = object(tlinkedlist_item)
          unitid          : word;
{$ifndef OLDPPU}
          name            : pstring;
          checksum        : longint;
          loaded          : boolean;
{$endif OLDPPU}
          in_uses,
          in_interface,
          is_stab_written : boolean;
          u               : pmodule;
{$ifndef OLDPPU}
          constructor init(_u : pmodule;intface:boolean);
          constructor init_to_load(const n:string;c:longint;intface:boolean);
{$else OLDPPU}
          constructor init(_u : pmodule;f : byte);
{$endif OLDPPU}
          destructor done;virtual;
       end;

{$ifndef NEWPPU}
    type
       tunitheader = array[0..19] of char;
    const
                                   {                compiler version }
                                   {             format      |       }
                                   { signature    |          |       }
                                   {  |           |          |       }
                                   { /-------\   /-------\  /----\   }
       unitheader : tunitheader  = ('P','P','U','0','1','4',#0,#99,
                                     #0,#0,#0,#0,#0,#0,#255,#255,
                                   { |   | \---------/ \-------/    }
                                   { |   |    |             |        }
                                   { |   |    check sum     |        }
                                   { |   \--flags        unused      }
                                   { target system                   }
                                    #0,#0,#0,#0);
                                   {\---------/                      }
                                   {  |                              }
                                   {  start of machine language      }

       ibloadunit      = 1;
       iborddef        = 2;
       ibpointerdef    = 3;
       ibtypesym       = 4;
       ibarraydef      = 5;
       ibprocdef       = 6;
       ibprocsym       = 7;
       iblinkofile     = 8;
       ibstringdef     = 9;
       ibvarsym        = 10;
       ibconstsym      = 11;
       ibinitunit      = 12;
       ibenumsym       = 13;
       ibtypedconstsym = 14;
       ibrecorddef     = 15;
       ibfiledef       = 16;
       ibformaldef     = 17;
       ibobjectdef     = 18;
       ibenumdef       = 19;
       ibsetdef        = 20;
       ibprocvardef    = 21;
       ibsourcefile    = 22;
       ibdbxcount      = 23;
       ibfloatdef      = 24;
       ibref           = 25;
       ibextsymref     = 26;
       ibextdefref     = 27;
       ibabsolutesym   = 28;
       ibclassrefdef   = 29;
       ibpropertysym   = 30;
       ibsharedlibs    = 31;
       iblongstringdef = 32;
       ibansistringdef = 33;
       ibunitname      = 34;
       ibwidestringdef = 35;
       ibstaticlibs    = 36;
       ibvarsym_C      = 37;
       ibend           = 255;

       { unit flags }
       uf_init           = $1;
       uf_has_dbx        = $2;
       uf_has_browser    = $4;
       uf_in_library     = $8;
       uf_shared_library = $10;
       uf_big_endian     = $20;
       uf_smartlink      = $40;
       uf_finalize       = $80;
{$endif}

    var
       main_module    : pmodule;     { Main module of the program }
       current_module : pmodule;     { Current module which is compiled }
{$ifndef OLDPPU}
       current_ppu    : pppufile;    { Current ppufile which is read }
{$endif}
       global_unit_count : word;
       usedunits      : tlinkedlist; { Used units for this program }
       loaded_units   : tlinkedlist; { All loaded units }


  implementation

  uses
    dos,verbose,systems;


{****************************************************************************
                                  TFILE
 ****************************************************************************}

    constructor textfile.init(const p,n,e : string);
      begin
         inherited init(p+n+e,extbufsize);
         path:=stringdup(p);
         name:=stringdup(n);
         ext:=stringdup(e);
      end;

    destructor textfile.done;
      begin
         inherited done;
      end;


{****************************************************************************
                                  TINPUTFILE
 ****************************************************************************}

    constructor tinputfile.init(const p,n,e : string);
      begin
         inherited init(p,n,e);
         filenotatend:=true;
         line_no:=1;
         true_line:=1;
         column:=1;
         next:=nil;
      end;

    procedure tinputfile.write_file_line(var t : text);
      begin
         write(t,get_file_line);
      end;


    function tinputfile.get_file_line : string;
      begin
        if Use_Rhide then
          get_file_line:=lower(bstoslash(path^)+name^+ext^)+':'+tostr(line_no)+':'
        else
          get_file_line:=name^+ext^+'('+tostr(line_no)+')'
      end;


{****************************************************************************
                                TFILEMANAGER
 ****************************************************************************}

    constructor tfilemanager.init;
      begin
         files:=nil;
         last_ref_index:=0;
      end;


    destructor tfilemanager.done;
      var
         hp : pextfile;
      begin
         hp:=files;
         while assigned(hp) do
           begin
              files:=files^._next;
              dispose(hp,done);
              hp:=files;
           end;
         last_ref_index:=0;
      end;


    procedure tfilemanager.close_all;
      var
         hp : pextfile;
      begin
         hp:=files;
         while assigned(hp) do
           begin
              hp^.close;
              hp:=hp^._next;
           end;
      end;


    procedure tfilemanager.register_file(f : pextfile);
      begin
         inc(last_ref_index);
         f^._next:=files;
         f^.ref_index:=last_ref_index;
         files:=f;
      end;


   function tfilemanager.get_file(w : word) : pextfile;
     var
        ff : pextfile;
     begin
        ff:=files;
        while assigned(ff) and (ff^.ref_index<>w) do
          ff:=ff^._next;
        get_file:=ff;
     end;


{****************************************************************************
                                  TMODULE
 ****************************************************************************}

    procedure tmodule.setfilename(const _path,name:string);
      var
        s : string;
      begin
         stringdispose(objfilename);
         stringdispose(asmfilename);
         stringdispose(ppufilename);
         stringdispose(libfilename);
         stringdispose(path);
         path:=stringdup(FixPath(_path));
         s:=FixFileName(FixPath(_path)+name);
         objfilename:=stringdup(s+target_info.objext);
         asmfilename:=stringdup(s+target_info.asmext);
         ppufilename:=stringdup(s+target_info.unitext);
         libfilename:=stringdup(s+target_os.staticlibext);
      end;

{$ifndef OLDPPU}

    function tmodule.openppu:boolean;
      var
         objfiletime,
         ppufiletime,
         asmfiletime : longint;
      begin
        openppu:=false;
      { Get ppufile time (also check if the file exists) }
        ppufiletime:=getnamedfiletime(ppufilename^);
        if ppufiletime=-1 then
         exit;
      { Open the ppufile }
        Message1(unit_u_ppu_loading,ppufilename^);
        ppufile:=new(pppufile,init(ppufilename^));
        if not ppufile^.open then
         begin
           dispose(ppufile,done);
           Message(unit_d_ppu_file_too_short);
           exit;
         end;
      { check for a valid PPU file }
        if not ppufile^.CheckPPUId then
         begin
           dispose(ppufile,done);
           Message(unit_d_ppu_invalid_header);
           exit;
         end;
      { check for allowed PPU versions }
        if not (ppufile^.GetPPUVersion in [15]) then
         begin
           dispose(ppufile,done);
           Message1(unit_d_ppu_invalid_version,tostr(ppufile^.GetPPUVersion));
           exit;
         end;
      { check the target processor }
        if ttargetcpu(ppufile^.header.cpu)<>target_cpu then
         begin
           dispose(ppufile,done);
           Comment(V_Debug,'unit is compiled for an other processor');
           exit;
         end;
      { check target }
        if ttarget(ppufile^.header.target)<>target_info.target then
         begin
           dispose(ppufile,done);
           Comment(V_Debug,'unit is compiled for an other target');
           exit;
         end;
  {!!!!!!!!!!!!!!!!!!! }
      { Load values to be access easier }
        flags:=ppufile^.header.flags;
        crc:=ppufile^.header.checksum;
      { Show Debug info }
        Message1(unit_d_ppu_time,filetimestring(ppufiletime));
        Message1(unit_d_ppu_flags,tostr(flags));
        Message1(unit_d_ppu_crc,tostr(ppufile^.header.checksum));
      { check the object and assembler file to see if we need only to
        assemble, only if it's not in a library }
        do_compile:=false;
        if (flags and uf_in_library)=0 then
         begin
           if (flags and uf_smartlink)<>0 then
            begin
              objfiletime:=getnamedfiletime(libfilename^);
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
                do_compile:=true;
            end
           else
            begin
            { the objectfile should be newer than the ppu file }
              objfiletime:=getnamedfiletime(objfilename^);
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
               begin
               { check if assembler file is older than ppu file }
                 asmfileTime:=GetNamedFileTime(asmfilename^);
                 if (asmfiletime<0) or (ppufiletime>asmfiletime) then
                  begin
                    Message(unit_d_obj_and_asm_are_older_than_ppu);
                    do_compile:=true;
                  end
                 else
                  begin
                    Message(unit_d_obj_is_older_than_asm);
                    do_assemble:=true;
                  end;
               end;
            end;
         end;
        openppu:=true;
      end;


    function tmodule.search_unit(const n : string):boolean;
      var
         ext       : string[8];
         singlepathstring,
         unitPath,
         filename  : string;
         found     : boolean;
         start,i   : longint;

         Function UnitExists(const ext:string):boolean;
         begin
           Message1(unit_t_unitsearch,Singlepathstring+filename+ext);
           UnitExists:=FileExists(Singlepathstring+FileName+ext);
         end;

       begin
         start:=1;
         filename:=FixFileName(n);
         unitpath:=UnitSearchPath;
         Found:=false;
         repeat
         { Create current path to check }
           i:=pos(';',unitpath);
           if i=0 then
            i:=length(unitpath)+1;
           singlepathstring:=FixPath(copy(unitpath,start,i-start));
           delete(unitpath,start,i-start+1);
         { Check for PPL file }
           if not (cs_link_static in aktswitches) then
            begin
              Found:=UnitExists(target_info.unitlibext);
              if Found then
               Begin
                 SetFileName(SinglePathString,FileName);
                 Found:=OpenPPU;
               End;
             end;
         { Check for PPU file }
           if not (cs_link_dynamic in aktswitches) and not Found then
            begin
              Found:=UnitExists(target_info.unitext);
              if Found then
               Begin
                 SetFileName(SinglePathString,FileName);
                 Found:=OpenPPU;
               End;
            end;
         { Check for Sources }
           if not Found then
            begin
              ppufile:=nil;
              do_compile:=true;
            {Check for .pp file}
              Found:=UnitExists(target_os.sourceext);
              if Found then
               Ext:=target_os.sourceext
              else
               begin
               {Check for .pas}
                 Found:=UnitExists(target_os.pasext);
                 if Found then
                  Ext:=target_os.pasext;
               end;
              stringdispose(mainsource);
              if Found then
               begin
                 sources_avail:=true;
               {Load Filenames when found}
                 mainsource:=StringDup(SinglePathString+FileName+Ext);
                 SetFileName(SinglePathString,FileName);
               end
              else
               sources_avail:=false;
            end;
         until Found or (unitpath='');
         search_unit:=Found;
      end;

{$else OLDPPU}

{*****************************************************************************

                               Old PPU

*****************************************************************************}

    function tmodule.load_ppu(const unit_path,n,ext : string):boolean;
      var
         header  : tunitheader;
         count   : longint;
         temp,hs : string;
         b       : byte;
         incfile_found : boolean;
         code    : word;
         ppuversion,
         objfiletime,
         ppufiletime,
         asmfiletime,
         source_time : longint;
{$ifdef UseBrowser}
         hp : pextfile;
         _d : dirstr;
         _n : namestr;
         _e : extstr;
{$endif UseBrowser}
      begin
        load_ppu:=false;
      { Get ppufile time (also check if the file exists) }
        ppufiletime:=getnamedfiletime(ppufilename^);
        if ppufiletime=-1 then
         exit;

        Message1(unit_u_ppu_loading,ppufilename^);
        ppufile:=new(pextfile,init(unit_path,n,ext));
        ppufile^.reset;
        ppufile^.flush;
        { load the header }
        ppufile^.read_data(header,sizeof(header),count);
        if count<>sizeof(header) then
         begin
           ppufile^.done;
           Message(unit_d_ppu_file_too_short);
           exit;
         end;
        { check for a valid PPU file }
        if (header[0]<>'P') or (header[1]<>'P') or (header[2]<>'U') then
         begin
           ppufile^.done;
           Message(unit_d_ppu_invalid_header);
           exit;
         end;
        { load ppu version }
        val(header[3]+header[4]+header[5],ppuversion,code);
        if not(ppuversion in [13..14]) then
         begin
           ppufile^.done;
           Message1(unit_d_ppu_invalid_version,tostr(ppuversion));
           exit;
         end;
        flags:=byte(header[9]);
        crc:=plongint(@header[10])^;
        {Get ppufile time}
        ppufiletime:=getnamedfiletime(ppufilename^);
        {Show Debug info}
        Message1(unit_d_ppu_time,filetimestring(ppufiletime));
        Message1(unit_d_ppu_flags,tostr(flags));
        Message1(unit_d_ppu_crc,tostr(crc));

      { read name if its there }
        ppufile^.read_data(b,1,count);
        if b=ibunitname then
         begin
           ppufile^.read_data(hs[0],1,count);
           ppufile^.read_data(hs[1],ord(hs[0]),count);
           stringdispose(modulename);
           modulename:=stringdup(hs);
           ppufile^.read_data(b,1,count);
         end;

      { search source files there is at least one source file }
        do_compile:=false;
        sources_avail:=true;
        while b<>ibend do
         begin
           ppufile^.read_data(hs[0],1,count);
           ppufile^.read_data(hs[1],ord(hs[0]),count);
           ppufile^.read_data(b,1,count);
           temp:='';
           if (flags and uf_in_library)<>0 then
            begin
              sources_avail:=false;
              temp:=' library';
            end
           else
            begin
              { check the date of the source files }
              Source_Time:=GetNamedFileTime(unit_path+hs);
              if Source_Time=-1 then
                begin
                   { search for include files in the includepathlist }
                   if b<>ibend then
                     begin
                        temp:=search(hs,includesearchpath,incfile_found);
                        if incfile_found then
                          begin
                             hs:=temp+hs;
                             Source_Time:=GetNamedFileTime(hs);
                          end;
                     end;
                end
              else
                hs:=unit_path+hs;
              if Source_Time=-1 then
               begin
                 sources_avail:=false;
                 temp:=' not found';
               end
              else
               begin
                 temp:=' time '+filetimestring(source_time);
                 if (source_time>ppufiletime) then
                  begin
                    do_compile:=true;
                    temp:=temp+' *'
                  end;
               end;
            end;
           Message1(unit_t_ppu_source,hs+temp);
  {$ifdef UseBrowser}
           fsplit(hs,_d,_n,_e);
           new(hp,init(_d,_n,_e));
           { the indexing should match what is done in writeasunit }
           sourcefiles.register_file(hp);
  {$endif UseBrowser}
         end;
      { main source is always the last }
        stringdispose(mainsource);
        mainsource:=stringdup(hs);

      { check the object and assembler file if not a library }
        if (flags and uf_smartlink)<>0 then
         begin
           objfiletime:=getnamedfiletime(libfilename^);
           if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
             do_compile:=true;
         end
        else
         begin
           if (flags and uf_in_library)=0 then
            begin
            { the objectfile should be newer than the ppu file }
              objfiletime:=getnamedfiletime(objfilename^);
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
               begin
               { check if assembler file is older than ppu file }
                 asmfileTime:=GetNamedFileTime(asmfilename^);
                 if (asmfiletime<0) or (ppufiletime>asmfiletime) then
                  begin
                    Message(unit_d_obj_and_asm_are_older_than_ppu);
                    do_compile:=true;
                  end
                 else
                  begin
                    Message(unit_d_obj_is_older_than_asm);
                    do_assemble:=true;
                  end;
               end;
            end;
         end;
        load_ppu:=true;
      end;

    function tmodule.search_unit(const n : string):boolean;
      var
         ext       : string[8];
         singlepathstring,
         UnitPath,
         filename  : string;
         found     : boolean;
         start,i   : longint;

         Function UnitExists(const ext:string):boolean;
         begin
           Message1(unit_t_unitsearch,Singlepathstring+filename+ext);
           UnitExists:=FileExists(Singlepathstring+FileName+ext);
         end;

       begin
         start:=1;
         filename:=FixFileName(n);
         unitpath:=UnitSearchPath;
         Found:=false;
         repeat
         {Create current path to check}
           i:=pos(';',unitpath);
           if i=0 then
            i:=length(unitpath)+1;
           singlepathstring:=FixPath(copy(unitpath,start,i-start));
           delete(unitpath,start,i-start+1);
         { Check for PPL file }
           if not (cs_link_static in aktswitches) then
            begin
              Found:=UnitExists(target_info.unitlibext);
              if Found then
               Begin
                 SetFileName(SinglePathString,FileName);
                 Found:=Load_PPU(singlepathstring,filename,target_info.unitlibext);
               End;
             end;
         { Check for PPU file }
           if not (cs_link_dynamic in aktswitches) and not Found then
            begin
              Found:=UnitExists(target_info.unitext);
              if Found then
               Begin
                 SetFileName(SinglePathString,FileName);
                 Found:=Load_PPU(singlepathstring,filename,target_info.unitext);
               End;
            end;
         { Check for Sources }
           if not Found then
            begin
              ppufile:=nil;
              do_compile:=true;
            {Check for .pp file}
              Found:=UnitExists(target_os.sourceext);
              if Found then
               Ext:=target_os.sourceext
              else
               begin
               {Check for .pas}
                 Found:=UnitExists(target_os.pasext);
                 if Found then
                  Ext:=target_os.pasext;
               end;
              stringdispose(mainsource);
              if Found then
               begin
                 sources_avail:=true;
               {Load Filenames when found}
                 mainsource:=StringDup(SinglePathString+FileName+Ext);
                 SetFileName(SinglePathString,FileName);
               end
              else
               sources_avail:=false;
            end;
         until Found or (unitpath='');
         search_unit:=Found;
      end;

{$endif OLDPPU}


    constructor tmodule.init(const s:string;_is_unit:boolean);
      var
        p : dirstr;
        n : namestr;
        e : extstr;
      begin
         FSplit(s,p,n,e);
      { Programs have the name program to don't conflict with dup id's }
         if _is_unit then
           modulename:=stringdup(Upper(n))
         else
           modulename:=stringdup('PROGRAM');
         mainsource:=stringdup(s);
         objfilename:=nil;
         asmfilename:=nil;
         libfilename:=nil;
         ppufilename:=nil;
         path:=nil;
         setfilename(p,n);
         used_units.init;
         sourcefiles.init;
         linkofiles.init;
         linkstaticlibs.init;
         linksharedlibs.init;
         ppufile:=nil;
         current_inputfile:=nil;
         map:=nil;
         symtable:=nil;
         flags:=0;
         crc:=0;
         unitcount:=1;
         inc(global_unit_count);
         unit_index:=global_unit_count;
         do_assemble:=false;
         do_compile:=false;
         sources_avail:=true;
         compiled:=false;
         in_implementation:=false;
         in_main:=false;
         is_unit:=_is_unit;
         uses_imports:=false;
         imports:=new(plinkedlist,init);
       { set smartlink flag }
         if (cs_smartlink in aktswitches) then
          flags:=flags or uf_smartlink;
       { search the PPU file if it is an unit }
         if is_unit then
          begin
            if (not search_unit(modulename^)) and (length(modulename^)>8) then
             search_unit(copy(modulename^,1,8));
          end;
      end;


{$ifndef OLDPPU}
    destructor tmodule.done;
      begin
        if assigned(map) then
         dispose(map);
        if assigned(ppufile) then
         dispose(ppufile,done);
        if assigned(imports) then
         dispose(imports,done);
        used_units.done;
        sourcefiles.done;
        linkofiles.done;
        linkstaticlibs.done;
        linksharedlibs.done;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(libfilename);
        stringdispose(path);
        stringdispose(modulename);
        stringdispose(mainsource);
        inherited done;
      end;

{$else}

    destructor tmodule.special_done;
      begin
         if assigned(map) then
           dispose(map);
         { cannot remove that because it is linked
         in the global chain of used_objects
         used_units.done; }
         sourcefiles.done;
         linkofiles.done;
         linkstaticlibs.done;
         linksharedlibs.done;
         if assigned(ppufile) then
          dispose(ppufile,done);
         if assigned(imports) then
          dispose(imports,done);
         inherited done;
      end;

{$endif OLDPPU}

{****************************************************************************
                              TUSED_UNIT
 ****************************************************************************}

{$ifndef OLDPPU}

    constructor tused_unit.init(_u : pmodule;intface:boolean);
      begin
        u:=_u;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=true;
        name:=stringdup(_u^.modulename^);
        checksum:=_u^.crc;
        unitid:=0;
      end;


    constructor tused_unit.init_to_load(const n:string;c:longint;intface:boolean);
      begin
        u:=nil;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=false;
        name:=stringdup(n);
        checksum:=c;
        unitid:=0;
      end;


    destructor tused_unit.done;
      begin
        stringdispose(name);
        inherited done;
      end;

{$else OLDPPU}

    constructor tused_unit.init(_u : pmodule;f : byte);
      begin
        u:=_u;
        in_interface:=false;
        in_uses:=false;
        is_stab_written:=false;
        unitid:=f;
      end;

    destructor tused_unit.done;
      begin
        inherited done;
      end;

{$endif OLDPPU}


end.
{
  $Log$
  Revision 1.28  1998-06-25 08:48:12  florian
    * first version of rtti support

  Revision 1.27  1998/06/24 14:48:34  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.26  1998/06/17 14:36:19  peter
    * forgot an $ifndef OLDPPU :(

  Revision 1.25  1998/06/17 14:10:11  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.24  1998/06/16 08:56:20  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.23  1998/06/15 14:44:36  daniel


  * BP updates.

  Revision 1.22  1998/06/14 18:25:41  peter
    * small fix with crc in newppu

  Revision 1.21  1998/06/13 00:10:05  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.20  1998/06/12 14:50:48  peter
    * removed the tree dependency to types.pas
    * long_fil.pas support (not fully tested yet)

  Revision 1.19  1998/06/12 10:32:26  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.18  1998/06/11 13:58:07  peter
    * small fix to let newppu compile

  Revision 1.17  1998/06/09 16:01:40  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.16  1998/06/04 10:42:19  pierre
    * small bug fix in load_ppu or openppu

  Revision 1.15  1998/05/28 14:37:53  peter
    * default programname is PROGRAM (like TP7) to avoid dup id's

  Revision 1.14  1998/05/27 19:45:02  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifndef OLDPPU

  Revision 1.13  1998/05/23 01:21:05  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.12  1998/05/20 09:42:33  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.11  1998/05/12 10:46:59  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.10  1998/05/11 13:07:53  peter
    + $ifndef OLDPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.9  1998/05/06 15:04:20  pierre
    + when trying to find source files of a ppufile
      check the includepathlist for included files
      the main file must still be in the same directory

  Revision 1.8  1998/05/04 17:54:25  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.7  1998/05/01 16:38:44  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.6  1998/05/01 07:43:53  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.5  1998/04/30 15:59:40  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.4  1998/04/29 10:33:52  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/27 23:10:28  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.2  1998/04/21 10:16:47  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version
}

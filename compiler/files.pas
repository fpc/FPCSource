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

{$ifdef TP}
{$V+}
{$endif}

  interface

    uses
       globtype,
       cobjects,globals,ppu;

    const
{$ifdef FPC}
       maxunits = 1024;
       InputFileBufSize=32*1024;
       linebufincrease=512;
{$else}
       maxunits = 128;
       InputFileBufSize=1024;
       linebufincrease=64;
{$endif}

    type
{$ifdef FPC}
       tlongintarr = array[0..1000000] of longint;
{$else}
       tlongintarr = array[0..16000] of longint;
{$endif}
       plongintarr = ^tlongintarr;

       pinputfile = ^tinputfile;
       tinputfile = object
         path,name : pstring;       { path and filename }
         next      : pinputfile;    { next file for reading }

         f            : file;       { current file handle }
         is_macro,
         endoffile,                 { still bytes left to read }
         closed       : boolean;    { is the file closed }

         buf          : pchar;      { buffer }
         bufstart,                  { buffer start position in the file }
         bufsize,                   { amount of bytes in the buffer }
         maxbufsize   : longint;    { size in memory for the buffer }

         saveinputpointer : pchar;  { save fields for scanner variables }
         savelastlinepos,
         saveline_no      : longint;

         linebuf    : plongintarr;  { line buffer to retrieve lines }
         maxlinebuf : longint;

         ref_count  : longint;      { to handle the browser refs }
         ref_index  : longint;
         ref_next   : pinputfile;

         constructor init(const fn:string);
         destructor done;
         procedure setpos(l:longint);
         procedure seekbuf(fpos:longint);
         procedure readbuf;
         function  open:boolean;
         procedure close;
         procedure tempclose;
         function  tempopen:boolean;
         procedure setmacro(p:pchar;len:longint);
         procedure setline(line,linepos:longint);
         function  getlinestr(l:longint):string;
       end;

       pfilemanager = ^tfilemanager;
       tfilemanager = object
          files : pinputfile;
          last_ref_index : longint;
          cacheindex : longint;
          cacheinputfile : pinputfile;
          constructor init;
          destructor done;
          procedure register_file(f : pinputfile);
          procedure inverse_register_indexes;
          function  get_file(l:longint) : pinputfile;
          function  get_file_name(l :longint):string;
          function  get_file_path(l :longint):string;
       end;


    type
{$ifndef NEWMAP}
       tunitmap = array[0..maxunits-1] of pointer;
       punitmap = ^tunitmap;

       pmodule = ^tmodule;

{$else NEWMAP}
       pmodule = ^tmodule;

       tunitmap = array[0..maxunits-1] of pmodule;
       punitmap = ^tunitmap;
{$endif NEWMAP}

       tmodule = object(tlinkedlist_item)
          ppufile       : pppufile; { the PPU file }
          crc,
{$ifdef Double_checksum}
          interface_crc : longint;
          do_reload_ppu : boolean;
{$endif def Double_checksum}
          flags         : longint;  { the PPU flags }

          compiled,                 { unit is already compiled }
          do_assemble,              { only assemble the object, don't recompile }
          do_compile,               { need to compile the sources }
          sources_avail,            { if all sources are reachable }
          is_unit,
          in_second_compile,        { is this unit being compiled for the 2nd time? }
          in_implementation,        { processing the implementation part? }
          in_global     : boolean;  { allow global settings }
          islibrary     : boolean;  { if it is a library (win32 dll) }
          map           : punitmap; { mapping of all used units }
          unitcount     : word;     { local unit counter }
          unit_index    : word;     { global counter for browser }
          globalsymtable,           { pointer to the local/static symtable of this unit }
          localsymtable : pointer;  { pointer to the psymtable of this unit }
          scanner       : pointer;  { scanner object used }
          loaded_from   : pmodule;
          uses_imports  : boolean;  { Set if the module imports from DLL's.}
          imports       : plinkedlist;
          _exports      : plinkedlist;

          sourcefiles   : pfilemanager;
          resourcefiles,
          linksharedlibs,
          linkstaticlibs,
          linkunitfiles,
          linkofiles    : tstringcontainer;
          used_units    : tlinkedlist;
          dependent_units : tlinkedlist;
          localunitsearchpath,           { local searchpaths }
          localobjectsearchpath,
          localincludesearchpath,
          locallibrarysearchpath : pstring;

          path,                     { path where the module is find/created }
          outpath,
          modulename,               { name of the module in uppercase }
          objfilename,              { fullname of the objectfile }
          asmfilename,              { fullname of the assemblerfile }
          ppufilename,              { fullname of the ppufile }
          staticlibfilename,        { fullname of the static libraryfile }
          sharedlibfilename,        { fullname of the shared libraryfile }
          exefilename,              { fullname of the exefile }
          asmprefix,                { prefix for the smartlink asmfiles }
          mainsource    : pstring;  { name of the main sourcefile }
{$ifdef Test_Double_checksum}
          crc_array : pointer;
          crc_size : longint;
{$endif def Test_Double_checksum}

          constructor init(const s:string;_is_unit:boolean);
          destructor done;virtual;
          procedure reset;
          procedure setfilename(const fn:string;allowoutput:boolean);
          function  openppu:boolean;
          function  search_unit(const n : string;onlysource:boolean):boolean;
       end;

       pused_unit = ^tused_unit;
       tused_unit = object(tlinkedlist_item)
          unitid          : word;
          name            : pstring;
          checksum        : longint;
{$ifdef Double_checksum}
          interface_checksum : longint;
{$endif def Double_checksum}
          loaded          : boolean;
          in_uses,
          in_interface,
          is_stab_written : boolean;
          u               : pmodule;
          constructor init(_u : pmodule;intface:boolean);
          constructor init_to_load(const n:string;c,intfc:longint;intface:boolean);
          destructor done;virtual;
       end;

       pdependent_unit = ^tdependent_unit;
       tdependent_unit = object(tlinkedlist_item)
          u : pmodule;
          constructor init(_u : pmodule);
       end;

    var
       main_module    : pmodule;     { Main module of the program }
       current_module : pmodule;     { Current module which is compiled }
       current_ppu    : pppufile;    { Current ppufile which is read }
       global_unit_count : word;
       usedunits      : tlinkedlist; { Used units for this program }
       loaded_units   : tlinkedlist; { All loaded units }

  function get_source_file(moduleindex,fileindex : word) : pinputfile;


implementation

uses
{$ifdef Double_checksum}
  comphook,
{$endif Double_checksum}
  dos,verbose,systems,
  symtable,scanner;

{****************************************************************************
                                  TINPUTFILE
 ****************************************************************************}

    constructor tinputfile.init(const fn:string);
      var
        p:dirstr;
        n:namestr;
        e:extstr;
      begin
        FSplit(fn,p,n,e);
        name:=stringdup(n+e);
        path:=stringdup(p);
        next:=nil;
      { file info }
        is_macro:=false;
        endoffile:=false;
        closed:=true;
        buf:=nil;
        bufstart:=0;
        bufsize:=0;
        maxbufsize:=InputFileBufSize;
      { save fields }
        saveinputpointer:=nil;
        saveline_no:=0;
        savelastlinepos:=0;
      { indexing refs }
        ref_next:=nil;
        ref_count:=0;
        ref_index:=0;
      { line buffer }
        linebuf:=nil;
        maxlinebuf:=0;
      end;


    destructor tinputfile.done;
      begin
        if not closed then
         close;
        stringdispose(path);
        stringdispose(name);
      { free memory }
        if assigned(linebuf) then
         freemem(linebuf,maxlinebuf shl 2);
      end;


    procedure tinputfile.setpos(l:longint);
      begin
        bufstart:=l;
      end;


    procedure tinputfile.seekbuf(fpos:longint);
      begin
        if closed then
         exit;
        seek(f,fpos);
        bufstart:=fpos;
        bufsize:=0;
      end;


    procedure tinputfile.readbuf;
    {$ifdef TP}
      var
        w : word;
    {$endif}
      begin
        if is_macro then
         endoffile:=true;
        if closed then
         exit;
        inc(bufstart,bufsize);
      {$ifdef TP}
        blockread(f,buf^,maxbufsize-1,w);
        bufsize:=w;
      {$else}
        blockread(f,buf^,maxbufsize-1,bufsize);
      {$endif}
        buf[bufsize]:=#0;
        endoffile:=eof(f);
      end;


    function tinputfile.open:boolean;
      var
        ofm : byte;
      begin
        open:=false;
        if not closed then
         Close;
        ofm:=filemode;
        filemode:=0;
        Assign(f,path^+name^);
        {$I-}
         reset(f,1);
        {$I+}
        filemode:=ofm;
        if ioresult<>0 then
         exit;
      { file }
        endoffile:=false;
        closed:=false;
        Getmem(buf,MaxBufsize);
        bufstart:=0;
        bufsize:=0;
        open:=true;
      end;


    procedure tinputfile.close;
      var
        i : word;
      begin
        if is_macro then
         begin
           if assigned(buf) then
             Freemem(buf,maxbufsize);
           buf:=nil;
           {is_macro:=false;
           still needed for dispose in scanner PM }
           closed:=true;
           exit;
         end;
        if not closed then
         begin
           {$I-}
            system.close(f);
           {$I+}
           i:=ioresult;
           closed:=true;
         end;
        if assigned(buf) then
          begin
             Freemem(buf,maxbufsize);
             buf:=nil;
          end;
        bufstart:=0;
      end;


    procedure tinputfile.tempclose;
      var
        i : word;
      begin
        if is_macro then
         exit;
        if not closed then
         begin
           {$I-}
            system.close(f);
           {$I+}
           i:=ioresult;
           Freemem(buf,maxbufsize);
           buf:=nil;
           closed:=true;
         end;
      end;


    function tinputfile.tempopen:boolean;
      var
        ofm : byte;
      begin
        tempopen:=false;
        if is_macro then
         begin
           tempopen:=true;
           exit;
         end;
        if not closed then
         exit;
        ofm:=filemode;
        filemode:=0;
        Assign(f,path^+name^);
        {$I-}
         reset(f,1);
        {$I+}
        filemode:=ofm;
        if ioresult<>0 then
         exit;
        closed:=false;
      { get new mem }
        Getmem(buf,maxbufsize);
      { restore state }
        seek(f,BufStart);
        bufsize:=0;
        readbuf;
        tempopen:=true;
      end;


    procedure tinputfile.setmacro(p:pchar;len:longint);
      begin
      { create new buffer }
        getmem(buf,len+1);
        move(p^,buf^,len);
        buf[len]:=#0;
      { reset }
        bufstart:=0;
        bufsize:=len;
        maxbufsize:=len+1;
        is_macro:=true;
        endoffile:=true;
        closed:=true;
      end;


    procedure tinputfile.setline(line,linepos:longint);
      var
        oldlinebuf  : plongintarr;
      begin
        if line<1 then
         exit;
        while (line>=maxlinebuf) do
         begin
           oldlinebuf:=linebuf;
         { create new linebuf and move old info }
           getmem(linebuf,(maxlinebuf+linebufincrease) shl 2);
           if assigned(oldlinebuf) then
            begin
              move(oldlinebuf^,linebuf^,maxlinebuf shl 2);
              freemem(oldlinebuf,maxlinebuf shl 2);
            end;
           fillchar(linebuf^[maxlinebuf],linebufincrease shl 2,0);
           inc(maxlinebuf,linebufincrease);
         end;
        linebuf^[line]:=linepos;
      end;


    function tinputfile.getlinestr(l:longint):string;
      var
        c    : char;
        i,
        fpos : longint;
        p    : pchar;
      begin
        getlinestr:='';
        if l<maxlinebuf then
         begin
           fpos:=linebuf^[l];
           { fpos is set negativ if the line was already written }
           { but we still know the correct value                 }
           if fpos<0 then
             fpos:=-fpos+1;
           if closed then
            open;
         { in current buf ? }
           if (fpos<bufstart) or (fpos>bufstart+bufsize) then
            begin
              seekbuf(fpos);
              readbuf;
            end;
         { the begin is in the buf now simply read until #13,#10 }
           i:=0;
           p:=@buf[fpos-bufstart];
           repeat
             c:=p^;
             if c=#0 then
              begin
                if endoffile then
                 break;
                readbuf;
                p:=buf;
                c:=p^;
              end;
             if c in [#10,#13] then
              break;
             inc(i);
             getlinestr[i]:=c;
             inc(longint(p));
           until (i=255);
           {$ifndef TP}
             {$ifopt H+}
               setlength(getlinestr,i);
             {$else}
               getlinestr[0]:=chr(i);
             {$endif}
           {$else}
             getlinestr[0]:=chr(i);
           {$endif}
         end;
      end;


{****************************************************************************
                                TFILEMANAGER
 ****************************************************************************}

    constructor tfilemanager.init;
      begin
         files:=nil;
         last_ref_index:=0;
         cacheindex:=0;
         cacheinputfile:=nil;
      end;


    destructor tfilemanager.done;
      var
         hp : pinputfile;
      begin
         hp:=files;
         while assigned(hp) do
          begin
            files:=files^.ref_next;
            dispose(hp,done);
            hp:=files;
          end;
         last_ref_index:=0;
      end;


    procedure tfilemanager.register_file(f : pinputfile);
      begin
         inc(last_ref_index);
         f^.ref_next:=files;
         f^.ref_index:=last_ref_index;
         files:=f;
         { update cache }
         cacheindex:=last_ref_index;
         cacheinputfile:=f;
{$ifdef FPC}
  {$ifdef heaptrc}
         writeln(stderr,f^.name^,' index ',current_module^.unit_index*100000+f^.ref_index);
  {$endif heaptrc}
{$endif FPC}
      end;


   { this procedure is necessary after loading the
     sources files from a PPU file  PM }
   procedure tfilemanager.inverse_register_indexes;
     var
        f : pinputfile;
     begin
        f:=files;
        while assigned(f) do
          begin
             f^.ref_index:=last_ref_index-f^.ref_index+1;
             f:=f^.ref_next;
          end;
        { reset cache }
        cacheindex:=0;
        cacheinputfile:=nil;
     end;



   function tfilemanager.get_file(l :longint) : pinputfile;
     var
        ff : pinputfile;
     begin
       { check cache }
       if (l=cacheindex) and assigned(cacheinputfile) then
        begin
          get_file:=cacheinputfile;
          exit;
        end;
       ff:=files;
       while assigned(ff) and (ff^.ref_index<>l) do
         ff:=ff^.ref_next;
       get_file:=ff;
     end;


   function tfilemanager.get_file_name(l :longint):string;
     var
       hp : pinputfile;
     begin
       hp:=get_file(l);
       if assigned(hp) then
        get_file_name:=hp^.name^
       else
        get_file_name:='';
     end;


   function tfilemanager.get_file_path(l :longint):string;
     var
       hp : pinputfile;
     begin
       hp:=get_file(l);
       if assigned(hp) then
        get_file_path:=hp^.path^
       else
        get_file_path:='';
     end;


    function get_source_file(moduleindex,fileindex : word) : pinputfile;
      var
         hp : pmodule;
         f : pinputfile;
      begin
         hp:=pmodule(loaded_units.first);
         while assigned(hp) and (hp^.unit_index<>moduleindex) do
           hp:=pmodule(hp^.next);
         get_source_file:=nil;
         if not assigned(hp) then
           exit;
         f:=pinputfile(hp^.sourcefiles^.files);
         while assigned(f) do
           begin
              if f^.ref_index=fileindex then
                begin
                   get_source_file:=f;
                   exit;
                end;
              f:=pinputfile(f^.ref_next);
           end;
      end;


{****************************************************************************
                                  TMODULE
 ****************************************************************************}

    procedure tmodule.setfilename(const fn:string;allowoutput:boolean);
      var
        p : dirstr;
        n : NameStr;
        e : ExtStr;
      begin
         stringdispose(objfilename);
         stringdispose(asmfilename);
         stringdispose(ppufilename);
         stringdispose(staticlibfilename);
         stringdispose(sharedlibfilename);
         stringdispose(exefilename);
         stringdispose(outpath);
         stringdispose(path);
         { Create names }
         fsplit(fn,p,n,e);
         n:=FixFileName(n);
         { set path }
         path:=stringdup(FixPath(p,false));
         { obj,asm,ppu names }
         p:=path^;
         if AllowOutput then
          begin
            if (OutputUnitDir<>'') then
             p:=OutputUnitDir
            else
             if (OutputExeDir<>'') then
              p:=OutputExeDir;
          end;
         outpath:=stringdup(p);
         objfilename:=stringdup(p+n+target_info.objext);
         asmfilename:=stringdup(p+n+target_info.asmext);
         ppufilename:=stringdup(p+n+target_info.unitext);
         { lib and exe could be loaded with a file specified with -o }
         if AllowOutput and (OutputFile<>'') then
          n:=OutputFile;
         staticlibfilename:=stringdup(p+target_os.libprefix+n+target_os.staticlibext);
         if target_info.target=target_i386_WIN32 then
           sharedlibfilename:=stringdup(p+n+target_os.sharedlibext)
         else
           sharedlibfilename:=stringdup(p+target_os.libprefix+n+target_os.sharedlibext);
         { output dir of exe can be specified separatly }
         if AllowOutput and (OutputExeDir<>'') then
          p:=OutputExeDir
         else
          p:=path^;
         exefilename:=stringdup(p+n+target_info.exeext);
      end;


    function tmodule.openppu:boolean;
      var
         objfiletime,
         ppufiletime,
         asmfiletime : longint;
      begin
        openppu:=false;
        Message1(unit_t_ppu_loading,ppufilename^);
      { Get ppufile time (also check if the file exists) }
        ppufiletime:=getnamedfiletime(ppufilename^);
        if ppufiletime=-1 then
         exit;
      { Open the ppufile }
        Message1(unit_u_ppu_name,ppufilename^);
        ppufile:=new(pppufile,init(ppufilename^));
        ppufile^.change_endian:=source_os.endian<>target_os.endian;
        if not ppufile^.open then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_file_too_short);
           exit;
         end;
      { check for a valid PPU file }
        if not ppufile^.CheckPPUId then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_invalid_header);
           exit;
         end;
      { check for allowed PPU versions }
        if not (ppufile^.GetPPUVersion = CurrentPPUVersion) then
         begin
           dispose(ppufile,done);
           Message1(unit_u_ppu_invalid_version,tostr(ppufile^.GetPPUVersion));
           exit;
         end;
      { check the target processor }
        if ttargetcpu(ppufile^.header.cpu)<>target_cpu then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_invalid_processor);
           exit;
         end;
      { check target }
        if ttarget(ppufile^.header.target)<>target_info.target then
         begin
           dispose(ppufile,done);
           Message(unit_u_ppu_invalid_target);
           exit;
         end;
      { Load values to be access easier }
        flags:=ppufile^.header.flags;
        crc:=ppufile^.header.checksum;
{$ifdef Double_checksum}
        interface_crc:=ppufile^.header.interface_checksum;
{$endif def Double_checksum}
      { Show Debug info }
        Message1(unit_u_ppu_time,filetimestring(ppufiletime));
        Message1(unit_u_ppu_flags,tostr(flags));
        Message1(unit_u_ppu_crc,tostr(ppufile^.header.checksum));
{$ifdef Double_checksum}
        Message1(unit_u_ppu_crc,tostr(ppufile^.header.interface_checksum)+' (intfc)');
{$endif}
      { check the object and assembler file to see if we need only to
        assemble, only if it's not in a library }
        do_compile:=false;
        if (flags and uf_in_library)=0 then
         begin
           if (flags and (uf_static_linked or uf_smartlink))<>0 then
            begin
              objfiletime:=getnamedfiletime(staticlibfilename^);
              Message2(unit_u_check_time,staticlibfilename^,filetimestring(objfiletime));
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
                begin
                  Message(unit_u_recompile_staticlib_is_older);
                  do_compile:=true;
                  exit;
                end;
            end
           else
            if (flags and uf_shared_linked)<>0 then
             begin
               objfiletime:=getnamedfiletime(sharedlibfilename^);
               Message2(unit_u_check_time,sharedlibfilename^,filetimestring(objfiletime));
               if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
                begin
                  Message(unit_u_recompile_sharedlib_is_older);
                  do_compile:=true;
                  exit;
                end;
             end
           else
            if (flags and uf_obj_linked)<>0 then
             begin
             { the objectfile should be newer than the ppu file }
               objfiletime:=getnamedfiletime(objfilename^);
               Message2(unit_u_check_time,objfilename^,filetimestring(objfiletime));
               if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
                begin
                { check if assembler file is older than ppu file }
                  asmfileTime:=GetNamedFileTime(asmfilename^);
                  Message2(unit_u_check_time,asmfilename^,filetimestring(asmfiletime));
                  if (asmfiletime<0) or (ppufiletime>asmfiletime) then
                   begin
                     Message(unit_u_recompile_obj_and_asm_older);
                     do_compile:=true;
                     exit;
                   end
                  else
                   begin
                     Message(unit_u_recompile_obj_older_than_asm);
                     if not(cs_asm_extern in aktglobalswitches) then
                      begin
                        do_compile:=true;
                        exit;
                      end;
                   end;
                end;
             end;
         end;
        openppu:=true;
      end;


    function tmodule.search_unit(const n : string;onlysource:boolean):boolean;
      var
         singlepathstring,
         filename : string;

         Function UnitExists(const ext:string):boolean;
         begin
           Message1(unit_t_unitsearch,Singlepathstring+filename+ext);
           UnitExists:=FileExists(Singlepathstring+FileName+ext);
         end;

         Function SearchPath(unitpath:string):boolean;
         var
           found   : boolean;
           start,i : longint;
           ext     : string[8];
         begin
           start:=1;
           Found:=false;
           repeat
           { Create current path to check }
             i:=pos(';',unitpath);
             if i=0 then
              i:=length(unitpath)+1;
             singlepathstring:=FixPath(copy(unitpath,start,i-start),false);
             delete(unitpath,start,i-start+1);
             if not onlysource then
              begin
              { Check for PPL file }
                if not Found then
                 begin
                   Found:=UnitExists(target_info.unitlibext);
                   if Found then
                    Begin
                      SetFileName(SinglePathString+FileName,false);
                      Found:=OpenPPU;
                    End;
                  end;
              { Check for PPU file }
                if not Found then
                 begin
                   Found:=UnitExists(target_info.unitext);
                   if Found then
                    Begin
                      SetFileName(SinglePathString+FileName,false);
                      Found:=OpenPPU;
                    End;
                 end;
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
                   SetFileName(SinglePathString+FileName,false);
                 end
                else
                 sources_avail:=false;
              end;
           until Found or (unitpath='');
           SearchPath:=Found;
         end;

       var
         fnd : boolean;
       begin
         filename:=FixFileName(n);
         { try to find unit
            1. cwd
            2. local unit path
            3. global unit path }
         fnd:=SearchPath('.');
         if (not fnd) and assigned(current_module^.LocalUnitSearchPath) then
          fnd:=SearchPath(current_module^.LocalUnitSearchPath^);
         if not fnd then
          fnd:=SearchPath(UnitSearchPath);
         search_unit:=fnd;
      end;

    procedure tmodule.reset;

{$ifdef Double_checksum}
      var
         pm : pdependent_unit;
{$endif}

      begin
        if assigned(scanner) then
          pscannerfile(scanner)^.invalid:=true;
        if assigned(globalsymtable) then
          begin
            dispose(punitsymtable(globalsymtable),done);
            globalsymtable:=nil;
          end;
        if assigned(localsymtable) then
          begin
            dispose(punitsymtable(localsymtable),done);
            localsymtable:=nil;
          end;
        if assigned(map) then
         begin
           dispose(map);
           map:=nil;
         end;
        if assigned(ppufile) then
         begin
           dispose(ppufile,done);
           ppufile:=nil;
         end;
        sourcefiles^.done;
        sourcefiles^.init;
        imports^.done;
        imports^.init;
        _exports^.done;
        _exports^.init;
        used_units.done;
        used_units.init;
        { all units that depend on this one must be recompiled ! }
{$ifdef Double_checksum}
        pm:=pdependent_unit(dependent_units.first);
        while assigned(pm) do
          begin
            pm^.u^.do_reload_ppu:=true;
            def_comment(v_warning,'Reloading '+pm^.u^.mainsource^+' needed because '+
              mainsource^+' is reloaded');
            pm:=pdependent_unit(pm^.next);
          end;
{$endif Double_checksum}
        dependent_units.done;
        dependent_units.init;
        resourcefiles.done;
        resourcefiles.init_no_double;
        linkunitfiles.done;
        linkunitfiles.init_no_double;
        linkofiles.done;
        linkofiles.init_no_double;
        linkstaticlibs.done;
        linkstaticlibs.init_no_double;
        linksharedlibs.done;
        linksharedlibs.init_no_double;
        uses_imports:=false;
        do_assemble:=false;
        do_compile:=false;
        { sources_avail:=true;
        should not be changed PM }
        compiled:=false;
        in_implementation:=false;
        in_global:=true;
        loaded_from:=nil;
        flags:=0;
        crc:=0;
{$ifdef Double_checksum}
        interface_crc:=0;
{$endif def Double_checksum}
        unitcount:=1;
      end;


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
         ppufilename:=nil;
         objfilename:=nil;
         asmfilename:=nil;
         staticlibfilename:=nil;
         sharedlibfilename:=nil;
         exefilename:=nil;
         outpath:=nil;
         { Dos has the famous 8.3 limit :( }
{$ifdef tp}
         asmprefix:=stringdup(FixFileName('as'));
{$else}
  {$ifdef go32v2}
         asmprefix:=stringdup(FixFileName('as'));
  {$else}
    {$ifdef OS2}
         {Allthough OS/2 supports long filenames I play it safe and
          use 8.3 filenames, because this allows the compiler to run
          on a FAT partition. (DM)}
         asmprefix:=stringdup(FixFileName('as'));
    {$else}
         asmprefix:=stringdup(FixFileName(n));
    {$endif}
  {$endif}
{$endif tp}
         path:=nil;
         setfilename(p+n,true);
         localunitsearchpath:=nil;
         localobjectsearchpath:=nil;
         localincludesearchpath:=nil;
         locallibrarysearchpath:=nil;
         used_units.init;
         dependent_units.init;
         new(sourcefiles,init);
         resourcefiles.init_no_double;
         linkunitfiles.init_no_double;
         linkofiles.init_no_double;
         linkstaticlibs.init_no_double;
         linksharedlibs.init_no_double;
         ppufile:=nil;
         scanner:=nil;
         map:=nil;
         globalsymtable:=nil;
         localsymtable:=nil;
         loaded_from:=nil;
         flags:=0;
         crc:=0;
{$ifdef Double_checksum}
        interface_crc:=0;
        do_reload_ppu:=false;
{$endif def Double_checksum}
         unitcount:=1;
         inc(global_unit_count);
         unit_index:=global_unit_count;
         do_assemble:=false;
         do_compile:=false;
         sources_avail:=true;
         compiled:=false;
         in_second_compile:=false;
         in_implementation:=false;
         in_global:=true;
         is_unit:=_is_unit;
         islibrary:=false;
         uses_imports:=false;
         imports:=new(plinkedlist,init);
         _exports:=new(plinkedlist,init);
       { search the PPU file if it is an unit }
         if is_unit then
          begin
            if (not search_unit(modulename^,false)) and (length(modulename^)>8) then
             search_unit(copy(modulename^,1,8),false);
          end;
      end;


    destructor tmodule.done;
      begin
        if assigned(map) then
         dispose(map);
        if assigned(ppufile) then
         dispose(ppufile,done);
        ppufile:=nil;
        if assigned(imports) then
         dispose(imports,done);
        imports:=nil;
        if assigned(_exports) then
         dispose(_exports,done);
        _exports:=nil;
        if assigned(scanner) then
          pscannerfile(scanner)^.invalid:=true;
        if assigned(sourcefiles) then
         dispose(sourcefiles,done);
        sourcefiles:=nil;
        used_units.done;
        dependent_units.done;
        resourcefiles.done;
        linkunitfiles.done;
        linkofiles.done;
        linkstaticlibs.done;
        linksharedlibs.done;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(staticlibfilename);
        stringdispose(sharedlibfilename);
        stringdispose(exefilename);
        stringdispose(outpath);
        stringdispose(path);
        stringdispose(modulename);
        stringdispose(mainsource);
        stringdispose(asmprefix);
        stringdispose(localunitsearchpath);
        stringdispose(localobjectsearchpath);
        stringdispose(localincludesearchpath);
        stringdispose(locallibrarysearchpath);
        if assigned(globalsymtable) then
          dispose(punitsymtable(globalsymtable),done);
        globalsymtable:=nil;
        if assigned(localsymtable) then
          dispose(punitsymtable(localsymtable),done);
        localsymtable:=nil;
        inherited done;
      end;


{****************************************************************************
                              TUSED_UNIT
 ****************************************************************************}

    constructor tused_unit.init(_u : pmodule;intface:boolean);
      begin
        u:=_u;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=true;
        name:=stringdup(_u^.modulename^);
        checksum:=_u^.crc;
{$ifdef Double_checksum}
        interface_checksum:=_u^.interface_crc;
{$endif def Double_checksum}
        unitid:=0;
      end;


    constructor tused_unit.init_to_load(const n:string;c,intfc:longint;intface:boolean);
      begin
        u:=nil;
        in_interface:=intface;
        in_uses:=false;
        is_stab_written:=false;
        loaded:=false;
        name:=stringdup(n);
        checksum:=c;
{$ifdef Double_checksum}
        interface_checksum:=intfc;
{$endif def Double_checksum}
        unitid:=0;
      end;


    destructor tused_unit.done;
      begin
        stringdispose(name);
        inherited done;
      end;

{****************************************************************************
                            TDENPENDENT_UNIT
 ****************************************************************************}

    constructor tdependent_unit.init(_u : pmodule);
      begin
         u:=_u;
      end;

end.
{
  $Log$
  Revision 1.91  1999-04-21 09:43:36  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.90  1999/04/14 09:14:48  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.89  1999/04/07 15:39:29  pierre
    + double_checksum code added

  Revision 1.88  1999/03/25 16:55:29  peter
    + unitpath,librarypath,includepath,objectpath directives

  Revision 1.87  1999/02/16 00:48:23  peter
    * save in the ppu if linked with obj file instead of using the
      library flag, so the .inc files are also checked

  Revision 1.86  1999/02/05 08:54:24  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.85  1999/01/14 21:47:11  peter
    * status.currentmodule is now also updated
    + status.currentsourcepath

  Revision 1.84  1999/01/14 11:38:39  daniel
  * Exe name derived from target_info instead of target_os

  Revision 1.83  1999/01/13 15:02:00  daniel
  * Tinputfile.readbuf eof bugfix

  Revision 1.82  1999/01/12 14:25:26  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.81  1998/12/28 23:26:14  peter
    + resource file handling ($R directive) for Win32

  Revision 1.80  1998/12/16 00:27:19  peter
    * removed some obsolete version checks

  Revision 1.79  1998/12/11 00:03:14  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.78  1998/12/04 10:18:07  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.77  1998/12/02 16:23:37  jonas
    * changed "if longintvar in set" to case or "if () or () .." statements
    * tree.pas: changed inlinenumber (and associated constructor/vars) to a byte

  Revision 1.76  1998/12/01 12:51:19  peter
    * fixed placing of ppas.sh and link.res when using -FE

  Revision 1.75  1998/11/16 15:41:40  peter
    * tp7 didn't like my ifopt H+ :(

  Revision 1.74  1998/11/16 12:18:01  peter
    * H+ fixes

  Revision 1.73  1998/11/16 11:28:58  pierre
    * stackcheck removed for i386_win32
    * exportlist does not crash at least !!
      (was need for tests dir !)z

  Revision 1.72  1998/11/15 16:32:35  florian
    * some stuff of Pavel implement (win32 dll creation)
    * bug with ansistring function results fixed

  Revision 1.71  1998/11/06 09:45:40  pierre
    * bug on errors (file used after dispose !) fixed

  Revision 1.70  1998/11/03 11:33:14  peter
    + search_unit arg to only search for sources

  Revision 1.69  1998/10/29 11:35:44  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.68  1998/10/27 10:22:34  florian
    + First things for win32 export sections

  Revision 1.67  1998/10/26 22:23:29  peter
    + fixpath() has an extra option to allow a ./ as path

  Revision 1.66  1998/10/19 18:07:11  peter
    + external dll_name name func support for linux

  Revision 1.65  1998/10/15 12:22:25  pierre
    * close include files immediately after end reading
      instead of waiting until unit compilation ended !

  Revision 1.64  1998/10/14 13:38:19  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.63  1998/10/14 11:02:49  daniel
  * Stupid typo fixed.

  Revision 1.62  1998/10/14 10:59:37  daniel
  * Staticlibfilename now doesn't include path.

  Revision 1.61  1998/10/14 10:57:25  daniel
  * Dirstr, namestr, extstr.
  * $V+ to prevent Peter from forgetting this.
  * OS/2 compiler uses 8.3 filenames to support running the compiler on an old
    DOS FAT partition.

  Revision 1.60  1998/10/14 10:45:07  pierre
    * ppu problems for m68k fixed (at least in cross compiling)
    * one last memory leak for sysamiga fixed
    * the amiga RTL compiles now completely !!

  Revision 1.59  1998/10/13 14:01:07  peter
    * fixed -al

  Revision 1.58  1998/10/12 11:59:00  peter
    + show name and date of .o and .s files which the compiler checks

  Revision 1.57  1998/10/09 16:36:03  pierre
    * some memory leaks specific to usebrowser define fixed
    * removed tmodule.implsymtable (was like tmodule.localsymtable)

  Revision 1.56  1998/10/09 08:56:26  pierre
    * several memory leaks fixed

  Revision 1.55  1998/10/08 23:28:54  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.54  1998/10/08 17:17:19  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.53  1998/10/08 13:48:43  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.52  1998/10/06 22:09:48  peter
    * fixed for compiling with 0.99.8 due circular units

  Revision 1.51  1998/10/06 17:16:47  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.50  1998/09/30 16:43:34  peter
    * fixed unit interdependency with circular uses

  Revision 1.49  1998/09/28 16:57:20  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.48  1998/09/24 23:46:34  peter
    + outputdir support

  Revision 1.47  1998/09/22 17:13:43  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.46  1998/09/21 08:45:10  pierre
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

  Revision 1.45  1998/09/18 09:58:51  peter
    * -s doesn't require the .o to be available, this allows compiling of
      everything on other platforms (profiling the windows.pp loading ;)

  Revision 1.44  1998/09/10 13:51:32  peter
    * tp compiler also uses 'as' as asmprefix

  Revision 1.43  1998/09/03 17:08:45  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.42  1998/09/03 11:24:00  peter
    * moved more inputfile things from tscannerfile to tinputfile
    * changed ifdef Sourceline to cs_asm_source

  Revision 1.41  1998/08/26 15:35:30  peter
    * fixed scannerfiles for macros
    + $I %<environment>%

  Revision 1.40  1998/08/26 10:08:48  peter
    * fixed problem with libprefix at the wrong place
    * fixed lib generation with smartlinking and no -CS used

  Revision 1.39  1998/08/25 16:44:16  pierre
    * openppu was true even if the object file is missing
      this lead to trying to open a filename without extension
      and prevented the 'make cycle' to work for win32

  Revision 1.38  1998/08/19 10:06:12  peter
    * fixed filenames and removedir which supports slash at the end

  Revision 1.37  1998/08/18 20:52:19  peter
    * renamed in_main to in_global which is more logical

  Revision 1.36  1998/08/17 10:10:07  peter
    - removed OLDPPU

  Revision 1.35  1998/08/17 09:17:44  peter
    * static/shared linking updates

  Revision 1.34  1998/08/14 21:56:31  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.33  1998/08/11 14:09:08  peter
    * fixed some messages and smaller msgtxt.inc

  Revision 1.32  1998/08/10 14:49:58  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.31  1998/07/14 14:46:48  peter
    * released NEWINPUT

  Revision 1.30  1998/07/07 11:19:55  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.29  1998/06/25 10:51:00  pierre
    * removed a remaining ifndef NEWPPU
      replaced by ifdef OLDPPU
    * added uf_finalize to ppu unit

  Revision 1.28  1998/06/25 08:48:12  florian
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

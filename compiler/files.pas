{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$ifdef TP}
  {$define SHORTASMPREFIX}
{$endif}
{$ifdef go32v1}
  {$define SHORTASMPREFIX}
{$endif}
{$ifdef go32v2}
  {$define SHORTASMPREFIX}
{$endif}
{$ifdef OS2}
  { Allthough OS/2 supports long filenames I play it safe and
    use 8.3 filenames, because this allows the compiler to run
    on a FAT partition. (DM) }
  {$define SHORTASMPREFIX}
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
       trecompile_reason = (rr_unknown,rr_noppu,rr_sourcenewer,
         rr_build,rr_libolder,rr_objolder,rr_asmolder,rr_crcchanged);
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

       plinkcontaineritem=^tlinkcontaineritem;
       tlinkcontaineritem=object(tcontaineritem)
          data     : pstring;
          needlink : longint;
          constructor init(const s:string;m:longint);
          destructor  done;virtual;
       end;

       plinkcontainer=^tlinkcontainer;
       tlinkcontainer=object(tcontainer)
          constructor Init;
          procedure insert(const s : string;m:longint);
          function get(var m:longint) : string;
          function getusemask(mask:longint) : string;
          function find(const s:string):boolean;
       end;

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
          interface_crc,
          flags         : longint;  { the PPU flags }

          compiled,                 { unit is already compiled }
          do_reload,                { force reloading of the unit }
          do_assemble,              { only assemble the object, don't recompile }
          do_compile,               { need to compile the sources }
          sources_avail,            { if all sources are reachable }
          is_unit,
          in_compile,               { is it being compiled ?? }
          in_second_compile,        { is this unit being compiled for the 2nd time? }
          in_second_load,           { is this unit PPU loaded a 2nd time? }
          in_implementation,        { processing the implementation part? }
          in_global     : boolean;  { allow global settings }
          recompile_reason : trecompile_reason;  { the reason why the unit should be recompiled }

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
          resourcefiles : tstringcontainer;

          linkunitofiles,
          linkunitstaticlibs,
          linkunitsharedlibs,
          linkotherofiles,           { objects,libs loaded from the source }
          linkothersharedlibs,       { using $L or $LINKLIB or import lib (for linux) }
          linkotherstaticlibs  : tlinkcontainer;

          used_units           : tlinkedlist;
          dependent_units      : tlinkedlist;

          localunitsearchpath,           { local searchpaths }
          localobjectsearchpath,
          localincludesearchpath,
          locallibrarysearchpath : TSearchPathList;

          path,                     { path where the module is find/created }
          outputpath,               { path where the .s / .o / exe are created }
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
          crc_array2 : pointer;
          crc_size2 : longint;
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
          checksum,
          interface_checksum : longint;
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
       main_module       : pmodule;     { Main module of the program }
       current_module    : pmodule;     { Current module which is compiled or loaded }
       compiled_module   : pmodule;     { Current module which is compiled }
       current_ppu       : pppufile;    { Current ppufile which is read }
       global_unit_count : word;
       usedunits         : tlinkedlist; { Used units for this program }
       loaded_units      : tlinkedlist; { All loaded units }
       SmartLinkOFiles   : TStringContainer; { List of .o files which are generated,
                                               used to delete them after linking }

  function get_source_file(moduleindex,fileindex : word) : pinputfile;


implementation

uses
{$ifdef Delphi}
   dmisc,
{$else Delphi}
   dos,
{$endif Delphi}
  verbose,systems,
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
      {$ifdef VER70}
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
           if ioresult<>0 then;
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
      begin
        if is_macro then
         exit;
        if not closed then
         begin
           {$I-}
            system.close(f);
           {$I+}
           if ioresult<>0 then;
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
           { seek buffer postion to bufstart }
           if bufstart>0 then
            begin
              move(buf[bufstart],buf[0],bufsize-bufstart+1);
              bufstart:=0;
            end;
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
         { don't register macro's }
         if f^.is_macro then
          exit;
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
                             TLinkContainerItem
 ****************************************************************************}

constructor TLinkContainerItem.Init(const s:string;m:longint);
begin
  inherited Init;
  data:=stringdup(s);
  needlink:=m;
end;


destructor TLinkContainerItem.Done;
begin
  stringdispose(data);
end;


{****************************************************************************
                           TLinkContainer
 ****************************************************************************}

    constructor TLinkContainer.Init;
      begin
        inherited init;
      end;


    procedure TLinkContainer.insert(const s : string;m:longint);
      var
        newnode : plinkcontaineritem;
      begin
         {if find(s) then
          exit; }
         new(newnode,init(s,m));
         inherited insert(newnode);
      end;


    function TLinkContainer.get(var m:longint) : string;
      var
        p : plinkcontaineritem;
      begin
        p:=plinkcontaineritem(inherited get);
        if p=nil then
         begin
           get:='';
           m:=0;
           exit;
         end;
        get:=p^.data^;
        m:=p^.needlink;
        dispose(p,done);
      end;


    function TLinkContainer.getusemask(mask:longint) : string;
      var
         p : plinkcontaineritem;
         found : boolean;
      begin
        found:=false;
        repeat
          p:=plinkcontaineritem(inherited get);
          if p=nil then
           begin
             getusemask:='';
             exit;
           end;
          getusemask:=p^.data^;
          found:=(p^.needlink and mask)<>0;
          dispose(p,done);
        until found;
      end;


    function TLinkContainer.find(const s:string):boolean;
      var
        newnode : plinkcontaineritem;
      begin
        find:=false;
        newnode:=plinkcontaineritem(root);
        while assigned(newnode) do
         begin
           if newnode^.data^=s then
            begin
              find:=true;
              exit;
            end;
           newnode:=plinkcontaineritem(newnode^.next);
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
         stringdispose(outputpath);
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
         outputpath:=stringdup(p);
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
        interface_crc:=ppufile^.header.interface_checksum;
      { Show Debug info }
        Message1(unit_u_ppu_time,filetimestring(ppufiletime));
        Message1(unit_u_ppu_flags,tostr(flags));
        Message1(unit_u_ppu_crc,tostr(ppufile^.header.checksum));
        Message1(unit_u_ppu_crc,tostr(ppufile^.header.interface_checksum)+' (intfc)');
      { check the object and assembler file to see if we need only to
        assemble, only if it's not in a library }
        do_compile:=false;
        if (flags and uf_in_library)=0 then
         begin
           if (flags and uf_smart_linked)<>0 then
            begin
              objfiletime:=getnamedfiletime(staticlibfilename^);
              Message2(unit_u_check_time,staticlibfilename^,filetimestring(objfiletime));
              if (ppufiletime<0) or (objfiletime<0) or (ppufiletime>objfiletime) then
                begin
                  recompile_reason:=rr_libolder;
                  Message(unit_u_recompile_staticlib_is_older);
                  do_compile:=true;
                  exit;
                end;
            end;
           if (flags and uf_static_linked)<>0 then
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
                    recompile_reason:=rr_objolder;
                    do_compile:=true;
                    exit;
                  end
                 else
                  begin
                    Message(unit_u_recompile_obj_older_than_asm);
                    if not(cs_asm_extern in aktglobalswitches) then
                     begin
                       do_compile:=true;
                       recompile_reason:=rr_asmolder;
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

         Function SearchPath(const s:string):boolean;
         var
           found   : boolean;
           ext     : string[8];
         begin
           Found:=false;
           singlepathstring:=FixPath(s,false);
           if not onlysource then
            begin
{$ifdef CHECKPPL}
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
{$endif CHECKPPL}
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
              recompile_reason:=rr_noppu;
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
           SearchPath:=Found;
         end;

         Function SearchPathList(list:TSearchPathList):boolean;
         var
           hp : PStringQueueItem;
           found : boolean;
         begin
           found:=false;
           hp:=list.First;
           while assigned(hp) do
            begin
              found:=SearchPath(hp^.data^);
              if found then
               break;
              hp:=hp^.next;
            end;
           SearchPathList:=found;
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
         if (not fnd) then
          fnd:=SearchPathList(current_module^.LocalUnitSearchPath);
         if (not fnd) then
          fnd:=SearchPathList(UnitSearchPath);

         { try to find a file with the first 8 chars of the modulename, like
           dos }
         if (not fnd) and (length(filename)>8) then
          begin
            filename:=copy(filename,1,8);
            fnd:=SearchPath('.');
            if (not fnd) then
             fnd:=SearchPathList(current_module^.LocalUnitSearchPath);
            if not fnd then
             fnd:=SearchPathList(UnitSearchPath);
          end;
         search_unit:=fnd;
      end;



    procedure tmodule.reset;
      var
         pm : pdependent_unit;
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
        pm:=pdependent_unit(dependent_units.first);
        while assigned(pm) do
          begin
            if pm^.u^.in_second_compile then
             Comment(v_debug,'No reload already in second compile: '+pm^.u^.modulename^)
            else
             begin
               pm^.u^.do_reload:=true;
               Comment(v_debug,'Reloading '+pm^.u^.modulename^+' needed because '+modulename^+' is reloaded');
             end;
            pm:=pdependent_unit(pm^.next);
          end;
        dependent_units.done;
        dependent_units.init;
        resourcefiles.done;
        resourcefiles.init;
        linkunitofiles.done;
        linkunitofiles.init;
        linkunitstaticlibs.done;
        linkunitstaticlibs.init;
        linkunitsharedlibs.done;
        linkunitsharedlibs.init;
        linkotherofiles.done;
        linkotherofiles.init;
        linkotherstaticlibs.done;
        linkotherstaticlibs.init;
        linkothersharedlibs.done;
        linkothersharedlibs.init;
        uses_imports:=false;
        do_assemble:=false;
        do_compile:=false;
        { sources_avail:=true;
        should not be changed PM }
        compiled:=false;
        in_implementation:=false;
        in_global:=true;
        {loaded_from:=nil;
        should not be changed PFV }
        flags:=0;
        crc:=0;
        interface_crc:=0;
        unitcount:=1;
        recompile_reason:=rr_unknown;
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
{$ifdef UNITALIASES}
          modulename:=stringdup(GetUnitAlias(Upper(n)))
{$else}
          modulename:=stringdup(Upper(n))
{$endif}
        else
          modulename:=stringdup('PROGRAM');
        mainsource:=stringdup(s);
        ppufilename:=nil;
        objfilename:=nil;
        asmfilename:=nil;
        staticlibfilename:=nil;
        sharedlibfilename:=nil;
        exefilename:=nil;
        { Dos has the famous 8.3 limit :( }
{$ifdef SHORTASMPREFIX}
        asmprefix:=stringdup(FixFileName('as'));
{$else}
        asmprefix:=stringdup(FixFileName(n));
{$endif}
        outputpath:=nil;
        path:=nil;
        setfilename(p+n,true);
        localunitsearchpath.init;
        localobjectsearchpath.init;
        localincludesearchpath.init;
        locallibrarysearchpath.init;
        used_units.init;
        dependent_units.init;
        new(sourcefiles,init);
        resourcefiles.init;
        linkunitofiles.init;
        linkunitstaticlibs.init;
        linkunitsharedlibs.init;
        linkotherofiles.init;
        linkotherstaticlibs.init;
        linkothersharedlibs.init;
        ppufile:=nil;
        scanner:=nil;
        map:=nil;
        globalsymtable:=nil;
        localsymtable:=nil;
        loaded_from:=nil;
        flags:=0;
        crc:=0;
        interface_crc:=0;
        do_reload:=false;
        unitcount:=1;
        inc(global_unit_count);
        unit_index:=global_unit_count;
        do_assemble:=false;
        do_compile:=false;
        sources_avail:=true;
        compiled:=false;
        recompile_reason:=rr_unknown;
        in_second_load:=false;
        in_compile:=false;
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
         search_unit(modulename^,false);
      end;


    destructor tmodule.done;
{$ifdef MEMDEBUG}
      var
        d : tmemdebug;
{$endif}
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
        linkunitofiles.done;
        linkunitstaticlibs.done;
        linkunitsharedlibs.done;
        linkotherofiles.done;
        linkotherstaticlibs.done;
        linkothersharedlibs.done;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(staticlibfilename);
        stringdispose(sharedlibfilename);
        stringdispose(exefilename);
        stringdispose(outputpath);
        stringdispose(path);
        stringdispose(modulename);
        stringdispose(mainsource);
        stringdispose(asmprefix);
        localunitsearchpath.done;
        localobjectsearchpath.done;
        localincludesearchpath.done;
        locallibrarysearchpath.done;
{$ifdef MEMDEBUG}
        d.init('symtable');
{$endif}
        if assigned(globalsymtable) then
          dispose(punitsymtable(globalsymtable),done);
        globalsymtable:=nil;
        if assigned(localsymtable) then
          dispose(punitsymtable(localsymtable),done);
        localsymtable:=nil;
{$ifdef MEMDEBUG}
        d.done;
{$endif}
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
        interface_checksum:=_u^.interface_crc;
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
        interface_checksum:=intfc;
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
  Revision 1.116  2000-02-24 18:41:38  peter
    * removed warnings/notes

  Revision 1.115  2000/02/10 16:00:23  peter
    * dont' check for ppl files as they aren't used atm.

  Revision 1.114  2000/02/09 13:22:52  peter
    * log truncated

  Revision 1.113  2000/01/11 09:52:06  peter
    * fixed placing of .sl directories
    * use -b again for base-file selection
    * fixed group writing for linux with smartlinking

  Revision 1.112  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.111  1999/12/08 01:01:11  peter
    * fixed circular unit reference checking. loaded_from was reset after
      reseting a unit, so no loaded_from info was available anymore.

  Revision 1.110  1999/11/16 23:39:04  peter
    * use outputexedir for link.res location

  Revision 1.109  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.108  1999/11/06 14:34:20  peter
    * truncated log to 20 revs

  Revision 1.107  1999/11/04 23:13:25  peter
    * moved unit alias support into ifdef

  Revision 1.106  1999/11/04 10:54:02  peter
    + -Ua<oldname>=<newname> unit alias support

  Revision 1.105  1999/10/28 13:14:00  pierre
   * allow doubles in TLinkContainer needed for double libraries

  Revision 1.104  1999/09/27 23:40:12  peter
    * fixed macro within macro endless-loop

  Revision 1.103  1999/09/16 08:00:50  pierre
   + compiled_module to avoid wrong file info when load PPU files

  Revision 1.102  1999/08/31 15:51:10  pierre
   * in_second_compile cleaned up, in_compile and in_second_load added

  Revision 1.101  1999/08/27 10:43:20  pierre
   + interface CRC check with ifdef Test_double_checksum added

  Revision 1.100  1999/08/24 13:14:01  peter
    * MEMDEBUG to see the sizes of asmlist,asmsymbols,symtables

}

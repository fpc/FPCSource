{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an extended file management

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
unit finput;

{$i fpcdefs.inc}

interface

    uses
      cutils,cclasses;

    const
       InputFileBufSize=32*1024+1;
       linebufincrease=512;

    type
       tlongintarr = array[0..1000000] of longint;
       plongintarr = ^tlongintarr;

       tinputfile = class
         path,name : pshortstring;       { path and filename }
         next      : tinputfile;    { next file for reading }

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

         ref_index  : longint;
         ref_next   : tinputfile;

         constructor create(const fn:string);
         destructor  destroy;override;
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
         function  getfiletime:longint;
       protected
         filetime  : longint;
         function fileopen(const filename: string): boolean; virtual; abstract;
         function fileseek(pos: longint): boolean; virtual; abstract;
         function fileread(var databuf; maxsize: longint): longint; virtual; abstract;
         function fileeof: boolean; virtual; abstract;
         function fileclose: boolean; virtual; abstract;
         procedure filegettime; virtual; abstract;
       end;

       tdosinputfile = class(tinputfile)
       protected
         function fileopen(const filename: string): boolean; override;
         function fileseek(pos: longint): boolean; override;
         function fileread(var databuf; maxsize: longint): longint; override;
         function fileeof: boolean; override;
         function fileclose: boolean; override;
         procedure filegettime; override;
       private
         f            : file;       { current file handle }
       end;

       tinputfilemanager = class
          files : tinputfile;
          last_ref_index : longint;
          cacheindex : longint;
          cacheinputfile : tinputfile;
          constructor create;
          destructor destroy;override;
          procedure register_file(f : tinputfile);
          function  get_file(l:longint) : tinputfile;
          function  get_file_name(l :longint):string;
          function  get_file_path(l :longint):string;
       end;

{****************************************************************************
                                TModuleBase
 ****************************************************************************}

     type
        tmodulestate = (ms_unknown,
          ms_registered,
          ms_load,ms_compile,
          ms_second_load,ms_second_compile,
          ms_compiled
        );
     const
        ModuleStateStr : array[TModuleState] of string[20] = (
          'Unknown',
          'Registered',
          'Load','Compile',
          'Second_Load','Second_Compile',
          'Compiled'
        );

     type
        tmodulebase = class(TLinkedListItem)
          { index }
          unit_index       : longint;  { global counter for browser }
          { status }
          state            : tmodulestate;
          { sources }
          sourcefiles      : tinputfilemanager;
          { paths and filenames }
          paramallowoutput : boolean;  { original allowoutput parameter }
          paramfn,                  { original filename }
          path,                     { path where the module is find/created }
          outputpath,               { path where the .s / .o / exe are created }
          modulename,               { name of the module in uppercase }
          realmodulename,           { name of the module in the orignal case }
          objfilename,              { fullname of the objectfile }
          asmfilename,              { fullname of the assemblerfile }
          ppufilename,              { fullname of the ppufile }
          importlibfilename,        { fullname of the import libraryfile }
          staticlibfilename,        { fullname of the static libraryfile }
          sharedlibfilename,        { fullname of the shared libraryfile }
          mapfilename,              { fullname of the mapfile }
          exefilename,              { fullname of the exefile }
          dbgfilename,              { fullname of the debug info file }
          mainsource   : pshortstring;   { name of the main sourcefile }
          constructor create(const s:string);
          destructor destroy;override;
          procedure setfilename(const fn:string;allowoutput:boolean);
       end;


     Function GetNamedFileTime (Const F : String) : Longint;


implementation

uses
  SysUtils,
  GlobType,Comphook,
{$ifdef heaptrc}
  fmodule,
  ppheap,
{$endif heaptrc}
  cfileutl,
  Globals,Systems
  ;


{****************************************************************************
                                  Utils
 ****************************************************************************}

   Function GetNamedFileTime (Const F : String) : Longint;
     begin
       GetNamedFileTime:=do_getnamedfiletime(F);
     end;


{****************************************************************************
                                  TINPUTFILE
 ****************************************************************************}

    constructor tinputfile.create(const fn:string);
      begin
        name:=stringdup(ExtractFileName(fn));
        path:=stringdup(ExtractFilePath(fn));
        next:=nil;
        filetime:=-1;
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
        ref_index:=0;
      { line buffer }
        linebuf:=nil;
        maxlinebuf:=0;
      end;


    destructor tinputfile.destroy;
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
        fileseek(fpos);
        bufstart:=fpos;
        bufsize:=0;
      end;


    procedure tinputfile.readbuf;
      begin
        if is_macro then
         endoffile:=true;
        if closed then
         exit;
        inc(bufstart,bufsize);
        bufsize:=fileread(buf^,maxbufsize-1);
        buf[bufsize]:=#0;
        endoffile:=fileeof;
      end;


    function tinputfile.open:boolean;
      begin
        open:=false;
        if not closed then
         Close;
        if not fileopen(path^+name^) then
         exit;
      { file }
        endoffile:=false;
        closed:=false;
        Getmem(buf,MaxBufsize);
        buf[0]:=#0;
        bufstart:=0;
        bufsize:=0;
        open:=true;
      end;


    procedure tinputfile.close;
      begin
        if is_macro then
         begin
           if assigned(buf) then
            begin
              Freemem(buf,maxbufsize);
              buf:=nil;
            end;
           closed:=true;
           exit;
         end;
        if not closed then
         begin
           fileclose;
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
           fileclose;
           if assigned(buf) then
            begin
              Freemem(buf,maxbufsize);
              buf:=nil;
            end;
           closed:=true;
         end;
      end;


    function tinputfile.tempopen:boolean;
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
        if not fileopen(path^+name^) then
         exit;
        closed:=false;
      { get new mem }
        Getmem(buf,maxbufsize);
      { restore state }
        fileseek(BufStart);
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
             inc(p);
           until (i=255);
           getlinestr[0]:=chr(i);
         end;
      end;


    function tinputfile.getfiletime:longint;
      begin
        if filetime=-1 then
         filegettime;
        getfiletime:=filetime;
      end;


{****************************************************************************
                                TDOSINPUTFILE
 ****************************************************************************}

    function tdosinputfile.fileopen(const filename: string): boolean;
      var
        ofm : byte;
      begin
        { Check if file exists, this will also check if it is
          a real file and not a directory }
        if not fileexists(filename,false) then
          begin
            result:=false;
            exit;
          end;
        { Open file }
        ofm:=filemode;
        filemode:=0;
        Assign(f,filename);
        {$I-}
         reset(f,1);
        {$I+}
        filemode:=ofm;
        fileopen:=(ioresult=0);
      end;


    function tdosinputfile.fileseek(pos: longint): boolean;
      begin
        {$I-}
         seek(f,Pos);
        {$I+}
        fileseek:=(ioresult=0);
      end;


    function tdosinputfile.fileread(var databuf; maxsize: longint): longint;
      var
        w : longint;
      begin
        blockread(f,databuf,maxsize,w);
        fileread:=w;
      end;


    function tdosinputfile.fileeof: boolean;
      begin
        fileeof:=eof(f);
      end;


    function tdosinputfile.fileclose: boolean;
      begin
        {$I-}
         system.close(f);
        {$I+}
        fileclose:=(ioresult=0);
      end;


    procedure tdosinputfile.filegettime;
      begin
        filetime:=getnamedfiletime(path^+name^);
      end;


{****************************************************************************
                                Tinputfilemanager
 ****************************************************************************}

    constructor tinputfilemanager.create;
      begin
         files:=nil;
         last_ref_index:=0;
         cacheindex:=0;
         cacheinputfile:=nil;
      end;


    destructor tinputfilemanager.destroy;
      var
         hp : tinputfile;
      begin
         hp:=files;
         while assigned(hp) do
          begin
            files:=files.ref_next;
            hp.free;
            hp:=files;
          end;
         last_ref_index:=0;
      end;


    procedure tinputfilemanager.register_file(f : tinputfile);
      begin
         { don't register macro's }
         if f.is_macro then
          exit;
         inc(last_ref_index);
         f.ref_next:=files;
         f.ref_index:=last_ref_index;
         files:=f;
         { update cache }
         cacheindex:=last_ref_index;
         cacheinputfile:=f;
{$ifdef heaptrc}
         ppheap_register_file(f.name^,current_module.unit_index*100000+f.ref_index);
{$endif heaptrc}
      end;


   function tinputfilemanager.get_file(l :longint) : tinputfile;
     var
        ff : tinputfile;
     begin
       { check cache }
       if (l=cacheindex) and assigned(cacheinputfile) then
        begin
          get_file:=cacheinputfile;
          exit;
        end;
       ff:=files;
       while assigned(ff) and (ff.ref_index<>l) do
         ff:=ff.ref_next;
       if assigned(ff) then
         begin
           cacheindex:=ff.ref_index;
           cacheinputfile:=ff;
         end;
       get_file:=ff;
     end;


   function tinputfilemanager.get_file_name(l :longint):string;
     var
       hp : tinputfile;
     begin
       hp:=get_file(l);
       if assigned(hp) then
        get_file_name:=hp.name^
       else
        get_file_name:='';
     end;


   function tinputfilemanager.get_file_path(l :longint):string;
     var
       hp : tinputfile;
     begin
       hp:=get_file(l);
       if assigned(hp) then
        get_file_path:=hp.path^
       else
        get_file_path:='';
     end;


{****************************************************************************
                                TModuleBase
 ****************************************************************************}

    procedure tmodulebase.setfilename(const fn:string;allowoutput:boolean);
      var
        p,n,
        prefix,
        suffix : string;
      begin
         stringdispose(objfilename);
         stringdispose(asmfilename);
         stringdispose(ppufilename);
         stringdispose(importlibfilename);
         stringdispose(staticlibfilename);
         stringdispose(sharedlibfilename);
         stringdispose(mapfilename);
         stringdispose(exefilename);
         stringdispose(dbgfilename);
         stringdispose(outputpath);
         stringdispose(path);
         stringdispose(paramfn);
         { Create names }
         paramfn := stringdup(fn);
         paramallowoutput := allowoutput;
         p := FixPath(ExtractFilePath(fn),false);
         n := FixFileName(ChangeFileExt(ExtractFileName(fn),''));
         { set path }
         path:=stringdup(p);
         { obj,asm,ppu names }
         if AllowOutput then
           begin
             if (OutputUnitDir<>'') then
               p:=OutputUnitDir
             else
               if (OutputExeDir<>'') then
                 p:=OutputExeDir;
           end;
         outputpath:=stringdup(p);
         asmfilename:=stringdup(p+n+target_info.asmext);
         objfilename:=stringdup(p+n+target_info.objext);
         ppufilename:=stringdup(p+n+target_info.unitext);
         importlibfilename:=stringdup(p+target_info.importlibprefix+n+target_info.importlibext);
         staticlibfilename:=stringdup(p+target_info.staticlibprefix+n+target_info.staticlibext);

         { output dir of exe can be specified separatly }
         if AllowOutput and (OutputExeDir<>'') then
           p:=OutputExeDir
         else
           p:=path^;

         { lib and exe could be loaded with a file specified with -o }
         if AllowOutput and
            (compile_level=1) and
            (OutputFileName<>'')then
           begin
             exefilename:=stringdup(p+OutputFileName);
             sharedlibfilename:=stringdup(p+OutputFileName);
           end
         else
           begin
             exefilename:=stringdup(p+n+target_info.exeext);
             if Assigned(OutputPrefix) then
               prefix := OutputPrefix^
             else
               prefix := target_info.sharedlibprefix;
             if Assigned(OutputSuffix) then
               suffix := OutputSuffix^
             else
               suffix := '';
             sharedlibfilename:=stringdup(p+prefix+n+suffix+target_info.sharedlibext);
           end;
         mapfilename:=stringdup(p+n+'.map');
         dbgfilename:=stringdup(p+n+'.dbg');
      end;


    constructor tmodulebase.create(const s:string);
      begin
        modulename:=stringdup(Upper(s));
        realmodulename:=stringdup(s);
        mainsource:=nil;
        ppufilename:=nil;
        objfilename:=nil;
        asmfilename:=nil;
        importlibfilename:=nil;
        staticlibfilename:=nil;
        sharedlibfilename:=nil;
        exefilename:=nil;
        dbgfilename:=nil;
        mapfilename:=nil;
        outputpath:=nil;
        paramfn:=nil;
        path:=nil;
        { status }
        state:=ms_registered;
        { unit index }
        inc(global_unit_count);
        unit_index:=global_unit_count;
        { sources }
        sourcefiles:=TInputFileManager.Create;
      end;


    destructor tmodulebase.destroy;
      begin
        if assigned(sourcefiles) then
         sourcefiles.free;
        sourcefiles:=nil;
        stringdispose(objfilename);
        stringdispose(asmfilename);
        stringdispose(ppufilename);
        stringdispose(importlibfilename);
        stringdispose(staticlibfilename);
        stringdispose(sharedlibfilename);
        stringdispose(exefilename);
        stringdispose(dbgfilename);
        stringdispose(mapfilename);
        stringdispose(outputpath);
        stringdispose(path);
        stringdispose(modulename);
        stringdispose(realmodulename);
        stringdispose(mainsource);
        stringdispose(paramfn);
        inherited destroy;
      end;

end.

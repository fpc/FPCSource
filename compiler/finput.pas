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
      cutils,globtype,cclasses,cstreams;

    const
       InputFileBufSize=32*1024+1;
       linebufincrease=512;

    type
       tlongintarr = array[0..1000000] of longint;
       plongintarr = ^tlongintarr;

       tinputfile = class
         path,name : TPathStr;       { path and filename }
         inc_path  : TPathStr;       { path if file was included with $I directive }
         next      : tinputfile;    { next file for reading }

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

         is_macro,
         endoffile,                 { still bytes left to read }
         closed       : boolean;    { is the file closed }

         { this file represents an internally generated macro. Enables
           certain escape sequences }
         internally_generated_macro: boolean;

         constructor create(const fn:TPathStr);
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
         function fileopen(const filename: TPathStr): boolean; virtual; abstract;
         function fileseek(pos: longint): boolean; virtual; abstract;
         function fileread(var databuf; maxsize: longint): longint; virtual; abstract;
         function fileeof: boolean; virtual; abstract;
         function fileclose: boolean; virtual; abstract;
         procedure filegettime; virtual; abstract;
       end;
       ptinputfile = ^tinputfile;

       tdosinputfile = class(tinputfile)
       protected
         function fileopen(const filename: TPathStr): boolean; override;
         function fileseek(pos: longint): boolean; override;
         function fileread(var databuf; maxsize: longint): longint; override;
         function fileeof: boolean; override;
         function fileclose: boolean; override;
         procedure filegettime; override;
       private
         f            : TCCustomFileStream;       { current file handle }
       end;

       tinputfilemanager = class
          files : ptinputfile;
          nfiles,afiles : sizeint;
          constructor create;
          destructor destroy;override;
          procedure register_file(f : tinputfile);
          function  get_file(l:longint) : tinputfile;
          function  get_file_name(l :longint):TPathStr;
          function  get_file_path(l :longint):TPathStr;
       end;

{****************************************************************************
                                TModuleBase
 ****************************************************************************}

     type
        tmodulestate = (ms_unknown,
          ms_registered,
          ms_load,
          ms_compile,
          ms_compiling_waitintf,
          ms_compiling_waitimpl,
          ms_compiling_waitfinish,
          ms_compiling_wait,
          ms_compiled,
          ms_processed,
          ms_moduleerror
        );
        tmodulestates = set of tmodulestate;

     const
        ModuleStateStr : array[TModuleState] of string[32] = (
          'Unknown',
          'Registered',
          'Load',
          'Compile',
          'Compiling_Waiting_interface',
          'Compiling_Waiting_implementation',
          'Compiling_Waiting_finish',
          'Compiling_Waiting',
          'Compiled',
          'Processed',
          'Error'
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
          modulename,               { name of the module in uppercase }
          realmodulename: pshortstring; { name of the module in the orignal case }
          paramfn,                  { original filename }
          mainsource,               { name of the main sourcefile }
          objfilename,              { fullname of the objectfile }
          asmfilename,              { fullname of the assemblerfile }
          ppufilename,              { fullname of the ppufile }
{$ifdef DEBUG_NODE_XML}
          ppxfilename,              { fullname of the intermediate node XML file }
{$endif DEBUG_NODE_XML}
          importlibfilename,        { fullname of the import libraryfile }
          staticlibfilename,        { fullname of the static libraryfile }
          sharedlibfilename,        { fullname of the shared libraryfile }
          exportfilename,           { fullname of the export file }
          mapfilename,              { fullname of the mapfile }
          exefilename,              { fullname of the exefile }
          dbgfilename,              { fullname of the debug info file }
          path,                     { path where the module is find/created }
          outputpath   : TPathStr;  { path where the .s / .o / exe are created }
{$ifdef DEBUG_NODE_XML}
          ppxfilefail: Boolean;     { If the ppxfile could not be accessed, flag it }
{$endif DEBUG_NODE_XML}
          is_initial : boolean;     { is this the initial module, i.e. the one specified on the command-line ?}
          constructor create(const s:string);
          destructor destroy;override;
          procedure setfilename(const fn:TPathStr;allowoutput:boolean);
       end;


     Function GetNamedFileTime (Const F : TPathStr) : Longint;


implementation

uses
  SysUtils,
  Comphook,
{$ifndef GENERIC_CPU}
{$ifdef heaptrc}
  fmodule,
  ppheap,
{$endif heaptrc}
{$endif not GENERIC_CPU}
  cfileutl,
  Globals,Systems
  ;


{****************************************************************************
                                  Utils
 ****************************************************************************}

   Function GetNamedFileTime (Const F : TPathStr) : Longint;
     begin
       GetNamedFileTime:=do_getnamedfiletime(F);
     end;


{****************************************************************************
                                  TINPUTFILE
 ****************************************************************************}

    constructor tinputfile.create(const fn:TPathStr);
      begin
        name:=ExtractFileName(fn);
        path:=ExtractFilePath(fn);
        inc_path:='';
        next:=nil;
        filetime:=-1;
        buf:=nil;
        bufstart:=0;
        bufsize:=0;
        maxbufsize:=InputFileBufSize;
      { save fields }
        saveinputpointer:=nil;
        saveline_no:=0;
        savelastlinepos:=0;
      { indexing refs }
        ref_index:=0;
      { line buffer }
        linebuf:=nil;
        maxlinebuf:=0;
      { file info }
        is_macro:=false;
        endoffile:=false;
        closed:=true;
        internally_generated_macro:=false;
      end;


    destructor tinputfile.destroy;
      begin
        if not closed then
         close;
      { free memory }
        if assigned(linebuf) then
         freemem(linebuf,maxlinebuf*sizeof(linebuf^[0]));
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
        if not fileopen(path+name) then
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
           name:='';
           path:='';
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
        if not fileopen(path+name) then
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
      begin
        if line<1 then
         exit;
        while (line>=maxlinebuf) do
          begin
            { create new linebuf and move old info }
            linebuf:=reallocmem(linebuf,(maxlinebuf+linebufincrease)*sizeof(linebuf^[0]));
            fillchar(linebuf^[maxlinebuf],linebufincrease*sizeof(linebuf^[0]),0);
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

    function tdosinputfile.fileopen(const filename: TPathStr): boolean;
      begin
        { Check if file exists, this will also check if it is
          a real file and not a directory }
        if not fileexists(filename,false) then
          begin
            result:=false;
            exit;
          end;
        { Open file }
        fileopen:=false;
        try
          f:=CFileStreamClass.Create(filename,fmOpenRead);
          fileopen:=CStreamError=0;
        except
        end;
      end;


    function tdosinputfile.fileseek(pos: longint): boolean;
      begin
        fileseek:=false;
        try
          f.position:=Pos;
          fileseek:=true;
        except
        end;
      end;


    function tdosinputfile.fileread(var databuf; maxsize: longint): longint;
      begin
        fileread:=f.Read(databuf,maxsize);
      end;


    function tdosinputfile.fileeof: boolean;
      begin
        fileeof:=f.eof();
      end;


    function tdosinputfile.fileclose: boolean;
      begin
        fileclose:=false;
        try
          f.Free;
          fileclose:=true;
        except
        end;
      end;


    procedure tdosinputfile.filegettime;
      begin
        filetime:=getnamedfiletime(path+name);
      end;


{****************************************************************************
                                Tinputfilemanager
 ****************************************************************************}

    constructor tinputfilemanager.create;
      begin
      end;


    destructor tinputfilemanager.destroy;
      var
         ifile : SizeInt;
      begin
         for ifile:=0 to nfiles-1 do
          files[ifile].free;
         FreeMem(files);
      end;


    procedure tinputfilemanager.register_file(f : tinputfile);
      begin
         { don't register macro's }
         if f.is_macro then
          exit;

         if nfiles=afiles then
          begin
            afiles:=afiles+4+SizeUint(afiles) div 4+SizeUint(afiles) div 8;
            ReallocMem(files,afiles*sizeof(files[0]));
          end;
         f.ref_index:=1+nfiles;
         files[nfiles]:=f;
         inc(nfiles);

{$ifndef GENERIC_CPU}
{$ifdef heaptrc}
         ppheap_register_file(f.path+f.name,current_module.unit_index*100000+f.ref_index);
{$endif heaptrc}
{$endif not GENERIC_CPU}
      end;


   function tinputfilemanager.get_file(l :longint) : tinputfile;
     begin
       if not ((l>=1) and (l<=nfiles)) then
         exit(nil);
       result:=files[l-1];
     end;


   function tinputfilemanager.get_file_name(l :longint):TPathStr;
     var
       hp : tinputfile;
     begin
       hp:=get_file(l);
       if assigned(hp) then
        get_file_name:=hp.name
       else
        get_file_name:='';
     end;


   function tinputfilemanager.get_file_path(l :longint):TPathStr;
     var
       hp : tinputfile;
     begin
       hp:=get_file(l);
       if assigned(hp) then
        get_file_path:=hp.path
       else
        get_file_path:='';
     end;


{****************************************************************************
                                TModuleBase
 ****************************************************************************}

    procedure tmodulebase.setfilename(const fn:TPathStr;allowoutput:boolean);
      var
        p, n,
        prefix,
        suffix : TPathStr;
      begin
         { Create names }
         paramfn := fn;
         paramallowoutput := allowoutput;
         p := FixPath(ExtractFilePath(fn),false);
         n := FixFileName(ChangeFileExt(ExtractFileName(fn),''));
         { set path }
         path:=p;
         { obj,asm,ppu names }
         if AllowOutput then
           begin
             if (OutputUnitDir<>'') then
               p:=OutputUnitDir
             else
               if (OutputExeDir<>'') then
                 p:=OutputExeDir;
           end;
         outputpath:=p;
         asmfilename:=p+n+target_info.asmext;
         objfilename:=p+n+target_info.objext;
         ppufilename:=p+n+target_info.unitext;
{$ifdef DEBUG_NODE_XML}
         ppxfilename:=p+n+'-node-dump.xml';
{$endif DEBUG_NODE_XML}
         importlibfilename:=p+target_info.importlibprefix+n+target_info.importlibext;
         staticlibfilename:=p+target_info.staticlibprefix+n+target_info.staticlibext;
         exportfilename:=p+'exp'+n+target_info.objext;

         { output dir of exe can be specified separatly }
         if AllowOutput and (OutputExeDir<>'') then
           p:=OutputExeDir
         else
           p:=path;

         { lib and exe could be loaded with a file specified with -o }
         if AllowOutput and is_initial and
            (OutputFileName<>'')then
           begin
             exefilename:=p+OutputFileName;
             sharedlibfilename:=p+OutputFileName;
             n:=ChangeFileExt(OutputFileName,''); { for mapfilename and dbgfilename }
           end
         else
           begin
             exefilename:=p+n+target_info.exeext;
             if Assigned(OutputPrefix) then
               prefix := OutputPrefix^
             else
               prefix := target_info.sharedlibprefix;
             if Assigned(OutputSuffix) then
               suffix := OutputSuffix^
             else
               suffix := '';
             sharedlibfilename:=p+prefix+n+suffix+target_info.sharedlibext;
           end;
         mapfilename:=p+n+'.map';
         dbgfilename:=p+n+'.dbg';
      end;


    constructor tmodulebase.create(const s:string);
      begin
        modulename:=stringdup(Upper(s));
        realmodulename:=stringdup(s);
        mainsource:='';
        ppufilename:='';
{$ifdef DEBUG_NODE_XML}
        ppxfilename:='';
{$endif DEBUG_NODE_XML}
        objfilename:='';
        asmfilename:='';
        importlibfilename:='';
        staticlibfilename:='';
        sharedlibfilename:='';
        exefilename:='';
        dbgfilename:='';
        mapfilename:='';
        outputpath:='';
        paramfn:='';
        path:='';
{$ifdef DEBUG_NODE_XML}
        { Setting ppxfilefail to true will stop it from being written to if it
          was never initialised, which happens if a module doesn't need
          recompiling. }
        ppxfilefail := True;
{$endif DEBUG_NODE_XML}
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
        stringdispose(modulename);
        stringdispose(realmodulename);
        inherited destroy;
      end;

end.

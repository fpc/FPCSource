{
    $Id$
    Copyright (c) 1993-98 by the FPC development team

    Support routines for the browser

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
unit browser;

interface
uses
  cobjects,files;

const
{$ifdef TP}
  logbufsize   = 1024;
{$else}
  logbufsize   = 16384;
{$endif}
type
  pref = ^tref;
  tref = object
    nextref     : pref;
    posinfo     : tfileposinfo;
    moduleindex : word;
    constructor init(ref:pref;pos:pfileposinfo);
    destructor  done; virtual;
    function  get_file_line : string;
  end;

  pbrowser=^tbrowser;
  tbrowser=object
    fname    : string;
    logopen  : boolean;
    f        : file;
    buf      : pchar;
    bufidx   : longint;
    identidx : longint;
    constructor init;
    destructor done;
    procedure setfilename(const fn:string);
    procedure createlog;
    procedure flushlog;
    procedure addlog(const s:string);
    procedure addlogrefs(p:pref);
    procedure closelog;
    procedure ident;
    procedure unident;
  end;

var
  browse : tbrowser;

  function get_source_file(moduleindex,fileindex : word) : pinputfile;

implementation

  uses
    globals,systems,verbose;

{****************************************************************************
                               TRef
****************************************************************************}


    constructor tref.init(ref :pref;pos : pfileposinfo);
      begin
        nextref:=nil;
        if assigned(pos) then
          posinfo:=pos^;
        if assigned(current_module) then
          moduleindex:=current_module^.unit_index;
        if assigned(ref) then
          ref^.nextref:=@self;
      end;


    destructor tref.done;
      var
         inputfile : pinputfile;
         ref : pref;
      begin
         inputfile:=get_source_file(moduleindex,posinfo.fileindex);
         if inputfile<>nil then
           dec(inputfile^.ref_count);
         ref:=@self;
         if assigned(ref^.nextref) then
          dispose(ref^.nextref,done);
         nextref:=nil;
      end;


    function tref.get_file_line : string;
      var
         inputfile : pinputfile;
      begin
        get_file_line:='';
        inputfile:=get_source_file(moduleindex,posinfo.fileindex);
        if assigned(inputfile) then
          if Use_Rhide then
            get_file_line:=lower(inputfile^.name^+inputfile^.ext^)
              +':'+tostr(posinfo.line)+':'+tostr(posinfo.column)+':'
          else
            get_file_line:=inputfile^.name^+inputfile^.ext^
              +'('+tostr(posinfo.line)+','+tostr(posinfo.column)+')'
        else
          if Use_Rhide then
            get_file_line:='file_unknown:'
              +tostr(posinfo.line)+':'+tostr(posinfo.column)+':'
          else
            get_file_line:='file_unknown('
              +tostr(posinfo.line)+','+tostr(posinfo.column)+')'
      end;

{****************************************************************************
                              TBrowser
****************************************************************************}

    constructor tbrowser.init;
      begin
        fname:=FixFileName('browser.log');
        logopen:=false;
      end;


    destructor tbrowser.done;
      begin
        if logopen then
         closelog;
      end;


    procedure tbrowser.setfilename(const fn:string);
      begin
        fname:=FixFileName(fn);
      end;


    procedure tbrowser.createlog;
      begin
        if logopen then
         closelog;
        assign(f,fname);
        {$I-}
         rewrite(f,1);
        {$I+}
        if ioresult<>0 then
         exit;
        logopen:=true;
        getmem(buf,logbufsize);
        bufidx:=0;
        identidx:=0;
      end;


    procedure tbrowser.flushlog;
      begin
        if logopen then
         blockwrite(f,buf^,bufidx);
        bufidx:=0;
      end;


    procedure tbrowser.closelog;
      begin
        if logopen then
         begin
           flushlog;
           close(f);
           freemem(buf,logbufsize);
           logopen:=false;
         end;
      end;


    procedure tbrowser.addlog(const s:string);
      begin
        if not logopen then
         exit;
      { add ident }
        if identidx>0 then
         begin
           if bufidx+identidx>logbufsize then
            flushlog;
           fillchar(buf[bufidx],identidx,' ');
           inc(bufidx,identidx);
         end;
      { add text }
        if bufidx+length(s)>logbufsize-2 then
         flushlog;
        move(s[1],buf[bufidx],length(s));
        inc(bufidx,length(s));
      { add crlf }
        buf[bufidx]:=target_os.newline[1];
        inc(bufidx);
        if length(target_os.newline)=2 then
         begin
           buf[bufidx]:=target_os.newline[2];
           inc(bufidx);
         end;
      end;


    procedure tbrowser.addlogrefs(p:pref);
      var
        ref : pref;
      begin
        ref:=p;
        Ident;
        while assigned(ref) do
         begin
           Browse.AddLog(ref^.get_file_line);
           ref:=ref^.nextref;
         end;
        Unident;
      end;


    procedure tbrowser.ident;
      begin
        inc(identidx,2);
      end;


    procedure tbrowser.unident;
      begin
        dec(identidx,2);
      end;

{****************************************************************************
                             Helpers
****************************************************************************}


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
         f:=pinputfile(hp^.sourcefiles.files);
         while assigned(f) do
           begin
              if f^.ref_index=fileindex then
                begin
                   get_source_file:=f;
                   exit;
                end;
              f:=pinputfile(f^._next);
           end;
      end;

begin
  browse.init
end.
{
  $Log$
  Revision 1.5  1998-06-13 00:10:04  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.4  1998/06/11 10:11:57  peter
    * -gb works again

  Revision 1.3  1998/05/20 09:42:32  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.2  1998/04/30 15:59:39  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes
}


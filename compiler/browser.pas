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
    is_written  : boolean;
    constructor init(ref:pref;pos:pfileposinfo);
    destructor  done; virtual;
    function  get_file_line : string;
  end;

  pbrowser=^tbrowser;
  tbrowser=object
    fname    : string;
    logopen  : boolean;
    stderrlog : boolean;
    f        : file;
    elements_to_list : pstringqueue;
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
    procedure browse_symbol(const sr : string);
    procedure list_elements;
    procedure list_debug_infos;
  end;

var
  browse : tbrowser;

  function get_source_file(moduleindex,fileindex : word) : pinputfile;

implementation

  uses
    comphook,globals,symtable,systems,verbose;

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
        is_written:=false;
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
          if status.use_gccoutput then
            get_file_line:=lower(inputfile^.name^)
              +':'+tostr(posinfo.line)+':'+tostr(posinfo.column)+':'
          else
            get_file_line:=inputfile^.name^
              +'('+tostr(posinfo.line)+','+tostr(posinfo.column)+')'
        else
          if status.use_gccoutput then
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
        elements_to_list:=new(pstringqueue,init);
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
         if not stderrlog then
           blockwrite(f,buf^,bufidx)
         else
           begin
             buf[bufidx]:=#0;
{$ifndef TP}
             write(stderr,buf);
{$else TP}
             write(buf);
{$endif TP}
           end;
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
      
    procedure tbrowser.list_elements;

      begin

         stderrlog:=true;
         getmem(buf,logbufsize);
         logopen:=true;
         while not elements_to_list^.empty do
           browse_symbol(elements_to_list^.get);
         flushlog;
         logopen:=false;
         freemem(buf,logbufsize);
         stderrlog:=false;
      end;

    procedure tbrowser.list_debug_infos;
{$ifndef debug}
      begin
      end;
{$else debug}
      var
         hp : pmodule;
         ff : pinputfile;
      begin
         hp:=pmodule(loaded_units.first);
         while assigned(hp) do
           begin
              addlog('Unit '+hp^.modulename^+' has index '+tostr(hp^.unit_index));
              ff:=hp^.sourcefiles.files;
              while assigned(ff) do
                begin
                   addlog('File '+ff^.name^+' index '+tostr(ff^.ref_index));
                   ff:=ff^.ref_next;
                end;
              hp:=pmodule(hp^.next);
           end;
      end;
{$endif debug}
      
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


    procedure tbrowser.browse_symbol(const sr : string);
      var
         sym,symb : psym;
         symt : psymtable;
         hp : pmodule;
         s,ss : string;
         p : byte;

         procedure next_substring;
           begin
              p:=pos('.',s);
              if p>0 then
                begin
                   ss:=copy(s,1,p-1);
                   s:=copy(s,p+1,255);
                end
              else
                begin
                  ss:=s;
                  s:='';
                end;
              addlog('substring : '+ss);
          end;
      begin
         s:=sr;
         symt:=symtablestack;
         next_substring;
         if assigned(symt) then
           begin
              sym:=symt^.search(ss);
              if sym=nil then
                sym:=symt^.search(upper(ss));
           end
         else
           sym:=nil;
         if assigned(sym) and (sym^.typ=unitsym) and (s<>'') then
           begin
              addlog('Unitsym found !');
              symt:=punitsym(sym)^.unitsymtable;
              if assigned(symt) then
                begin
                   next_substring;
                   sym:=symt^.search(ss);
                end
              else
                sym:=nil;
           end;
         if not assigned(sym) then
           begin
              symt:=nil;
              { try all loaded_units }
              hp:=pmodule(loaded_units.first);
              while assigned(hp) do
                begin
                   if hp^.modulename^=upper(ss) then
                     begin
                        symt:=hp^.symtable;
                        break;
                     end;
                   hp:=pmodule(hp^.next);
                end;
              if not assigned(symt) then
                begin
                   addlog('!!!Symbol '+ss+' not found !!!');
                   exit;
                end
              else
                begin
                   next_substring;
                   sym:=symt^.search(ss);
                   if sym=nil then
                     sym:=symt^.search(upper(ss));
                end;
           end;

         while assigned(sym) and (s<>'') do
           begin
              next_substring;
              case sym^.typ of
                typesym :
                  begin
                     if ptypesym(sym)^.definition^.deftype in [recorddef,objectdef] then
                       begin
                          if ptypesym(sym)^.definition^.deftype=recorddef then
                            symt:=precdef(ptypesym(sym)^.definition)^.symtable
                          else
                            symt:=pobjectdef(ptypesym(sym)^.definition)^.publicsyms;
                          sym:=symt^.search(ss);
                          if sym=nil then
                            sym:=symt^.search(upper(ss));
                       end;
                  end;
                varsym :
                  begin
                     if pvarsym(sym)^.definition^.deftype in [recorddef,objectdef] then
                       begin
                          if pvarsym(sym)^.definition^.deftype=recorddef then
                            symt:=precdef(pvarsym(sym)^.definition)^.symtable
                          else
                            symt:=pobjectdef(pvarsym(sym)^.definition)^.publicsyms;
                          sym:=symt^.search(ss);
                          if sym=nil then
                            sym:=symt^.search(upper(ss));
                       end;
                  end;
                procsym :
                  begin
                     symt:=pprocsym(sym)^.definition^.parast;
                     symb:=symt^.search(ss);
                     if symb=nil then
                       symb:=symt^.search(upper(ss));
                     if not assigned(symb) then
                       begin
                          symt:=pprocsym(sym)^.definition^.parast;
                          sym:=symt^.search(ss);
                          if symb=nil then
                            symb:=symt^.search(upper(ss));
                       end
                     else
                       sym:=symb;
                  end;
                {else
                  sym^.add_to_browserlog;}
                end;
           end;
           if assigned(sym) then
             sym^.add_to_browserlog
           else
             addlog('!!!Symbol '+ss+' not found !!!');
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
              f:=pinputfile(f^.ref_next);
           end;
      end;

begin
  browse.init
end.
{
  $Log$
  Revision 1.8  1998-09-22 17:13:42  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.7  1998/09/21 08:45:05  pierre
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

  Revision 1.6  1998/09/01 07:54:16  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.5  1998/06/13 00:10:04  peter
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


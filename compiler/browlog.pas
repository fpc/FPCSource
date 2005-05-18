{
    Copyright (c) 1998-2002 by Florian Klaempfl and Pierre Muller

    Support routines for creating the browser log

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
unit browlog;

{$i fpcdefs.inc}

interface
uses
  cclasses,
  globtype,
  fmodule,finput,
  symbase,symconst,symtype,symsym,symdef,symtable;

const
  logbufsize   = 16384;

type
  pbrowserlog=^tbrowserlog;
  tbrowserlog=object
    fname    : string;
    logopen  : boolean;
    stderrlog : boolean;
    f        : file;
    elements_to_list : tstringlist;
    buf      : pchar;
    bufidx   : longint;
    identidx : longint;
    constructor init;
    destructor done;
    procedure setfilename(const fn:string);
    procedure createlog;
    procedure flushlog;
    procedure addlog(const s:string);
    procedure addlogrefs(p:tref);
    procedure closelog;
    procedure ident;
    procedure unident;
    procedure browse_symbol(const sr : string);
    procedure list_elements;
    procedure list_debug_infos;
  end;

var
  browserlog : tbrowserlog;

  procedure WriteBrowserLog;

  procedure InitBrowserLog;
  procedure DoneBrowserLog;


implementation

  uses
    cutils,comphook,
    globals,systems,
    ppu;

    function get_file_line(ref:tref): string;
      var
         inputfile : tinputfile;
      begin
        get_file_line:='';
        with ref do
         begin
           inputfile:=get_source_file(moduleindex,posinfo.fileindex);
           if assigned(inputfile) then
             if status.use_gccoutput then
             { for use with rhide
               add warning so that it does not interpret
               this as an error !! }
               get_file_line:=lower(inputfile.name^)
                 +':'+tostr(posinfo.line)+': warning: '+tostr(posinfo.column)+':'
             else
               get_file_line:=inputfile.name^
                 +'('+tostr(posinfo.line)+','+tostr(posinfo.column)+')'
           else
             if status.use_gccoutput then
               get_file_line:='file_unknown:'
                 +tostr(posinfo.line)+': warning: '+tostr(posinfo.column)+':'
             else
               get_file_line:='file_unknown('
                 +tostr(posinfo.line)+','+tostr(posinfo.column)+')'
         end;
      end;

{****************************************************************************
                              TBrowser
****************************************************************************}

    constructor tbrowserlog.init;
      begin
        fname:=FixFileName('browser.log');
        logopen:=false;
        elements_to_list:=TStringList.Create;
      end;


    destructor tbrowserlog.done;
      begin
        if logopen then
         closelog;
        elements_to_list.free;
      end;


    procedure tbrowserlog.setfilename(const fn:string);
      begin
        fname:=FixFileName(fn);
      end;


    procedure tbrowserlog.createlog;
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


    procedure tbrowserlog.flushlog;
      begin
        if logopen then
         if not stderrlog then
           blockwrite(f,buf^,bufidx)
         else
           begin
             buf[bufidx]:=#0;
{$ifdef FPC}
             write(stderr,buf);
{$else FPC}
             write(buf);
{$endif FPC}
           end;
        bufidx:=0;
      end;


    procedure tbrowserlog.closelog;
      begin
        if logopen then
         begin
           flushlog;
           close(f);
           freemem(buf,logbufsize);
           logopen:=false;
         end;
      end;

    procedure tbrowserlog.list_elements;

      begin

         stderrlog:=true;
         getmem(buf,logbufsize);
         logopen:=true;
         while not elements_to_list.empty do
           browse_symbol(elements_to_list.getfirst);
         flushlog;
         logopen:=false;
         freemem(buf,logbufsize);
         stderrlog:=false;
      end;

    procedure tbrowserlog.list_debug_infos;
{$ifndef debug}
      begin
      end;
{$else debug}
      var
         hp : tmodule;
         ff : tinputfile;
      begin
         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
           begin
              addlog('Unit '+hp.modulename^+' has index '+tostr(hp.unit_index));
              ff:=hp.sourcefiles.files;
              while assigned(ff) do
                begin
                   addlog('File '+ff.name^+' index '+tostr(ff.ref_index));
                   ff:=ff.ref_next;
                end;
              hp:=tmodule(hp.next);
           end;
      end;
{$endif debug}

    procedure tbrowserlog.addlog(const s:string);
      begin
        if not logopen then
         exit;
      { add ident }
        if (identidx>0) and not stderrlog then
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
        buf[bufidx]:=target_info.newline[1];
        inc(bufidx);
        if length(target_info.newline)=2 then
         begin
           buf[bufidx]:=target_info.newline[2];
           inc(bufidx);
         end;
      end;


    procedure tbrowserlog.addlogrefs(p:tref);
      var
        ref : tref;
      begin
        ref:=p;
        Ident;
        while assigned(ref) do
         begin
           Browserlog.AddLog(get_file_line(ref));
           ref:=ref.nextref;
         end;
        Unident;
      end;


    procedure tbrowserlog.browse_symbol(const sr : string);
      var
         sym  : tsym;
         symb : tstoredsym;
         symt : tsymtable;
         hp : tmodule;
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
         { don't create a new reference when
          looking for the symbol !! }
         make_ref:=false;
         s:=sr;
         symt:=symtablestack;
         next_substring;
         if assigned(symt) then
           begin
              sym:=tstoredsym(symt.search(ss));
              if sym=nil then
                sym:=tstoredsym(symt.search(upper(ss)));
           end
         else
           sym:=nil;
         if assigned(sym) and (sym.typ=unitsym) and (s<>'') then
           begin
              addlog('Unitsym found !');
              symt:=tunitsym(sym).unitsymtable;
              if assigned(symt) then
                begin
                   next_substring;
                   sym:=tstoredsym(symt.search(ss));
                end
              else
                sym:=nil;
           end;
         if not assigned(sym) then
           begin
              symt:=nil;
              { try all loaded_units }
              hp:=tmodule(loaded_units.first);
              while assigned(hp) do
                begin
                   if hp.modulename^=upper(ss) then
                     begin
                        symt:=hp.globalsymtable;
                        break;
                     end;
                   hp:=tmodule(hp.next);
                end;
              if not assigned(symt) then
                begin
                   addlog('!!!Symbol '+ss+' not found !!!');
                   make_ref:=true;
                   exit;
                end
              else
                begin
                   next_substring;
                   sym:=tstoredsym(symt.search(ss));
                   if sym=nil then
                     sym:=tstoredsym(symt.search(upper(ss)));
                end;
           end;

         while assigned(sym) and (s<>'') do
           begin
              next_substring;
              case sym.typ of
                typesym :
                  begin
                     if ttypesym(sym).restype.def.deftype in [recorddef,objectdef] then
                       begin
                          if ttypesym(sym).restype.def.deftype=recorddef then
                            symt:=trecorddef(ttypesym(sym).restype.def).symtable
                          else
                            symt:=tobjectdef(ttypesym(sym).restype.def).symtable;
                          sym:=tstoredsym(symt.search(ss));
                          if sym=nil then
                            sym:=tstoredsym(symt.search(upper(ss)));
                       end;
                  end;
                globalvarsym,
                localvarsym,
                paravarsym,
                fieldvarsym :
                  begin
                     if tabstractvarsym(sym).vartype.def.deftype in [recorddef,objectdef] then
                       begin
                          symt:=tabstractvarsym(sym).vartype.def.getsymtable(gs_record);
                          sym:=tstoredsym(symt.search(ss));
                          if sym=nil then
                            sym:=tstoredsym(symt.search(upper(ss)));
                       end;
                  end;
                procsym :
                  begin
                     symt:=tprocsym(sym).first_procdef.parast;
                     symb:=tstoredsym(symt.search(ss));
                     if symb=nil then
                       symb:=tstoredsym(symt.search(upper(ss)));
                     if not assigned(symb) then
                       begin
                          symt:=tprocsym(sym).first_procdef.localst;
                          sym:=tstoredsym(symt.search(ss));
                          if symb=nil then
                            symb:=tstoredsym(symt.search(upper(ss)));
                       end
                     else
                       sym:=symb;
                  end;
                end;
           end;
           if assigned(sym) then
            begin
              if assigned(sym.defref) then
               begin
                 browserlog.AddLog('***'+sym.name+'***');
                 browserlog.AddLogRefs(sym.defref);
               end;
            end
           else
             addlog('!!!Symbol '+ss+' not found !!!');
           make_ref:=true;
      end;

    procedure tbrowserlog.ident;
      begin
        inc(identidx,2);
      end;


    procedure tbrowserlog.unident;
      begin
        dec(identidx,2);
      end;

    procedure writesymtable(p:Tsymtable);forward;

    procedure writelocalsymtables(p:Tprocdef;arg:pointer);

    begin
        if assigned(p.defref) then
            begin
                browserlog.AddLog('***'+p.mangledname);
                browserlog.AddLogRefs(p.defref);
                if (current_module.flags and uf_local_browser)<>0 then
                    begin
                        if assigned(p.parast) then
                            writesymtable(p.parast);
                        if assigned(p.localst) then
                            writesymtable(p.localst);
                    end;
             end;
    end;


    procedure writesymtable(p:tsymtable);
      var
        hp : tsym;
        prdef : pprocdeflist;
      begin
        if cs_browser in aktmoduleswitches then
         begin
           if assigned(p.name) then
             Browserlog.AddLog('---Symtable '+p.name^)
           else
             begin
                if (p.symtabletype=recordsymtable) and
                   assigned(tdef(p.defowner).typesym) then
                  Browserlog.AddLog('---Symtable '+tdef(p.defowner).typesym.name)
                else
                  Browserlog.AddLog('---Symtable with no name');
             end;
           Browserlog.Ident;
           hp:=tstoredsym(p.symindex.first);
           while assigned(hp) do
            begin
              if assigned(hp.defref) then
               begin
                 browserlog.AddLog('***'+hp.name+'***');
                 browserlog.AddLogRefs(hp.defref);
               end;
              case hp.typ of
                typesym :
                  begin
                    if (ttypesym(hp).restype.def.deftype=recorddef) then
                      writesymtable(trecorddef(ttypesym(hp).restype.def).symtable);
                    if (ttypesym(hp).restype.def.deftype=objectdef) then
                      writesymtable(tobjectdef(ttypesym(hp).restype.def).symtable);
                  end;
                procsym :
                    Tprocsym(hp).foreach_procdef_static(@writelocalsymtables,nil);
              end;
              hp:=tstoredsym(hp.indexnext);
            end;
           browserlog.Unident;
         end;
      end;


{****************************************************************************
                             Helpers
****************************************************************************}

   procedure WriteBrowserLog;
     var
       p : tstoredsymtable;
       hp : tmodule;
     begin
       browserlog.CreateLog;
       browserlog.list_debug_infos;
       hp:=tmodule(loaded_units.first);
       while assigned(hp) do
         begin
            p:=tstoredsymtable(hp.globalsymtable);
            if assigned(p) then
              writesymtable(p);
            if cs_local_browser in aktmoduleswitches then
              begin
                 p:=tstoredsymtable(hp.localsymtable);
                 if assigned(p) then
                   writesymtable(p);
              end;
            hp:=tmodule(hp.next);
         end;
       browserlog.CloseLog;
     end;


  procedure InitBrowserLog;
    begin
       browserlog.init;
    end;

  procedure DoneBrowserLog;
    begin
       browserlog.done;
    end;

end.

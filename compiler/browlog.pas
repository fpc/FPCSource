{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Pierre Muller

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

{$i defines.inc}

interface
uses
  cobjects,cclasses,
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
    procedure addlogrefs(p:pref);
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
    globals,systems,verbose,
    ppu;

    function get_file_line(ref:pref): string;
      var
         inputfile : tinputfile;
      begin
        get_file_line:='';
        with ref^ do
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
        buf[bufidx]:=target_os.newline[1];
        inc(bufidx);
        if length(target_os.newline)=2 then
         begin
           buf[bufidx]:=target_os.newline[2];
           inc(bufidx);
         end;
      end;


    procedure tbrowserlog.addlogrefs(p:pref);
      var
        ref : pref;
      begin
        ref:=p;
        Ident;
        while assigned(ref) do
         begin
           Browserlog.AddLog(get_file_line(ref));
           ref:=ref^.nextref;
         end;
        Unident;
      end;


    procedure tbrowserlog.browse_symbol(const sr : string);
      var
         sym,symb : pstoredsym;
         symt : psymtable;
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
              sym:=pstoredsym(symt^.search(ss));
              if sym=nil then
                sym:=pstoredsym(symt^.search(upper(ss)));
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
                   sym:=pstoredsym(symt^.search(ss));
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
                   sym:=pstoredsym(symt^.search(ss));
                   if sym=nil then
                     sym:=pstoredsym(symt^.search(upper(ss)));
                end;
           end;

         while assigned(sym) and (s<>'') do
           begin
              next_substring;
              case sym^.typ of
                typesym :
                  begin
                     if ptypesym(sym)^.restype.def^.deftype in [recorddef,objectdef] then
                       begin
                          if ptypesym(sym)^.restype.def^.deftype=recorddef then
                            symt:=precorddef(ptypesym(sym)^.restype.def)^.symtable
                          else
                            symt:=pobjectdef(ptypesym(sym)^.restype.def)^.symtable;
                          sym:=pstoredsym(symt^.search(ss));
                          if sym=nil then
                            sym:=pstoredsym(symt^.search(upper(ss)));
                       end;
                  end;
                varsym :
                  begin
                     if pvarsym(sym)^.vartype.def^.deftype in [recorddef,objectdef] then
                       begin
                          if pvarsym(sym)^.vartype.def^.deftype=recorddef then
                            symt:=precorddef(pvarsym(sym)^.vartype.def)^.symtable
                          else
                            symt:=pobjectdef(pvarsym(sym)^.vartype.def)^.symtable;
                          sym:=pstoredsym(symt^.search(ss));
                          if sym=nil then
                            sym:=pstoredsym(symt^.search(upper(ss)));
                       end;
                  end;
                procsym :
                  begin
                     symt:=pprocsym(sym)^.definition^.parast;
                     symb:=pstoredsym(symt^.search(ss));
                     if symb=nil then
                       symb:=pstoredsym(symt^.search(upper(ss)));
                     if not assigned(symb) then
                       begin
                          symt:=pprocsym(sym)^.definition^.parast;
                          sym:=pstoredsym(symt^.search(ss));
                          if symb=nil then
                            symb:=pstoredsym(symt^.search(upper(ss)));
                       end
                     else
                       sym:=symb;
                  end;
                end;
           end;
           if assigned(sym) then
            begin
              if assigned(sym^.defref) then
               begin
                 browserlog.AddLog('***'+sym^.name+'***');
                 browserlog.AddLogRefs(sym^.defref);
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


    procedure writesymtable(p:psymtable);
      var
        hp : pstoredsym;
        prdef : pprocdef;
      begin
        if cs_browser in aktmoduleswitches then
         begin
           if assigned(p^.name) then
             Browserlog.AddLog('---Symtable '+p^.name^)
           else
             begin
                if (p^.symtabletype=recordsymtable) and
                   assigned(pdef(p^.defowner)^.typesym) then
                  Browserlog.AddLog('---Symtable '+pdef(p^.defowner)^.typesym^.name)
                else
                  Browserlog.AddLog('---Symtable with no name');
             end;
           Browserlog.Ident;
           hp:=pstoredsym(p^.symindex^.first);
           while assigned(hp) do
            begin
              if assigned(hp^.defref) then
               begin
                 browserlog.AddLog('***'+hp^.name+'***');
                 browserlog.AddLogRefs(hp^.defref);
               end;
              case hp^.typ of
                typesym :
                  begin
                    if (ptypesym(hp)^.restype.def^.deftype=recorddef) then
                      writesymtable(precorddef(ptypesym(hp)^.restype.def)^.symtable);
                    if (ptypesym(hp)^.restype.def^.deftype=objectdef) then
                      writesymtable(pobjectdef(ptypesym(hp)^.restype.def)^.symtable);
                  end;
                procsym :
                  begin
                    prdef:=pprocsym(hp)^.definition;
                    while assigned(prdef) do
                     begin
                       if assigned(prdef^.defref) then
                        begin
                          browserlog.AddLog('***'+prdef^.mangledname);
                          browserlog.AddLogRefs(prdef^.defref);
                          if (current_module.flags and uf_local_browser)<>0 then
                            begin
                               if assigned(prdef^.parast) then
                                 writesymtable(prdef^.parast);
                               if assigned(prdef^.localst) then
                                 writesymtable(prdef^.localst);
                            end;
                        end;
                       if assigned(pprocdef(prdef)^.defref) then
                        begin
                          browserlog.AddLog('***'+pprocdef(prdef)^.name+'***');
                          browserlog.AddLogRefs(pprocdef(prdef)^.defref);
                        end;
                       prdef:=pprocdef(prdef)^.nextoverloaded;
                     end;
                  end;
              end;
              hp:=pstoredsym(hp^.indexnext);
            end;
           browserlog.Unident;
         end;
      end;


{****************************************************************************
                             Helpers
****************************************************************************}

   procedure WriteBrowserLog;
     var
       p : pstoredsymtable;
       hp : tmodule;
     begin
       browserlog.CreateLog;
       browserlog.list_debug_infos;
       hp:=tmodule(loaded_units.first);
       while assigned(hp) do
         begin
            p:=pstoredsymtable(hp.globalsymtable);
            if assigned(p) then
              writesymtable(p);
            if cs_local_browser in aktmoduleswitches then
              begin
                 p:=pstoredsymtable(hp.localsymtable);
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
{
  $Log$
  Revision 1.6  2000-12-25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/10/31 22:02:46  peter
    * symtable splitted, no real code changes

  Revision 1.4  2000/09/24 15:06:11  peter
    * use defines.inc

  Revision 1.3  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:32  michael
  + removed logs

}

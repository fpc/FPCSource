{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Netware target

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

    First Implementation 10 Sept 2000 Armin Diehl

    Currently generating NetWare-NLM's only work under Linux. This is
    because nlmconf from binutils does not work with i.e. win32 coff
    object files. It works fine with ELF-Objects.

    The following compiler-swiches are supported for NetWare:
    $DESCRIPTION    : NLM-Description, will be displayed at load-time
    $M              : For Stack-Size, Heap-Size will be ignored
    $VERSION x.x.x  : Sets Major, Minor and Revision

    Sorry, Displaying copyright does not work with nlmconv from gnu bunutils.

    Exports will be handled like in win32:
    procedure bla;
    begin
    end;

    exports bla name 'bla';

    Without Name 'bla' this will be exported in upper-case.

    The path to the import-Files (from netware-sdk, see developer.novell.com)
    must be specified by the library-path. All external modules are defined
    as autoload.

    i.e. Procedure ConsolePrintf (p:pchar); cdecl; external 'clib.nlm';
    sets IMPORT @clib.imp and MODULE clib.

    If you dont have nlmconv, compile gnu-binutils with
       ./configure --enable-targets=i386-linux,i386-netware
       make all

    Debugging is currently only possible at assembler level with nwdbg, written
    by Jan Beulich. Nwdbg supports symbols but it's not a source-level
    debugger. You can get nwdbg from developer.novell.com. To enter the
    debugger from your program, define "EnterDebugger" as external cdecl and
    call it. Int3 will not work with Netware 5.

    A sample program:

    Program Hello;
    (*$DESCRIPTION HelloWorldNlm*)
    (*$VERSION 1.2.2*)
    (*$M 8192,8192*)
    begin
      writeLn ('hello world');
    end.

    compile with:
    ppc386 -Tnetware hello

    ToDo:
      - No duplicate imports and autoloads
      - Screen and Thread-Names

****************************************************************************
}
unit t_nwm;

{$i defines.inc}

interface

  uses
    import,export,link;

  type
    pimportlibnetware=^timportlibnetware;
    timportlibnetware=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
    end;

    pexportlibnetware=^texportlibnetware;
    texportlibnetware=object(texportlib)
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
    end;

    plinkernetware=^tlinkernetware;
    tlinkernetware=object(tlinker)
    private
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Init;
      procedure SetDefaultInfo;virtual;
      function  MakeExecutable:boolean;virtual;
    end;


implementation

  uses
    cutils,verbose,cobjects,systems,globtype,globals,
    symconst,script,
    fmodule,aasm,cpuasm,cpubase,symsym;

{*****************************************************************************
                               TIMPORTLIBNETWARE
*****************************************************************************}

procedure timportlibnetware.preparelib(const s : string);
begin
end;


procedure timportlibnetware.importprocedure(const func,module : string;index : longint;const name : string);
begin
  { insert sharedlibrary }
  current_module^.linkothersharedlibs.insert(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
    aktprocsym^.definition^.setmangledname(name)
  else
    message(parser_e_empty_import_name);
end;


procedure timportlibnetware.importvariable(const varname,module:string;const name:string);
begin
  { insert sharedlibrary }
  current_module^.linkothersharedlibs.insert(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  aktvarsym^.setmangledname(name);
  exclude(aktvarsym^.varoptions,vo_is_dll_var);
end;


procedure timportlibnetware.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBNETWARE
*****************************************************************************}

procedure texportlibnetware.preparelib(const s:string);
begin
end;


procedure texportlibnetware.exportprocedure(hp : pexported_item);
var
  hp2 : pexported_item;
begin
  { first test the index value }
  if (hp^.options and eo_index)<>0 then
   begin
     Comment(V_Error,'can''t export with index under netware');
     exit;
   end;
  { use pascal name is none specified }
  if (hp^.options and eo_name)=0 then
    begin
       hp^.name:=stringdup(hp^.sym^.name);
       hp^.options:=hp^.options or eo_name;
    end;
  { now place in correct order }
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) and
     (hp^.name^>hp2^.name^) do
    hp2:=pexported_item(hp2^.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2^.name^=hp^.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp^.name^);
      exit;
    end;
  if hp2=pexported_item(current_module^._exports^.first) then
    current_module^._exports^.insert(hp)
  else if assigned(hp2) then
    begin
       hp^.next:=hp2;
       hp^.previous:=hp2^.previous;
       if assigned(hp2^.previous) then
         hp2^.previous^.next:=hp;
       hp2^.previous:=hp;
    end
  else
    current_module^._exports^.concat(hp);
end;


procedure texportlibnetware.exportvar(hp : pexported_item);
begin
  hp^.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibnetware.generatelib;
var
  hp2 : pexported_item;
begin
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) do
   begin
     if not hp2^.is_var then
      begin
{$ifdef i386}
        { place jump in codesegment }
        codesegment^.concat(new(pai_align,init_op(4,$90)));
        codesegment^.concat(new(pai_symbol,initname_global(hp2^.name^,0)));
        codesegment^.concat(new(paicpu,op_sym(A_JMP,S_NO,newasmsymbol(hp2^.sym^.mangledname))));
        codesegment^.concat(new(pai_symbol_end,initname(hp2^.name^)));
{$endif i386}
      end
     else
      Comment(V_Error,'Exporting of variables is not supported under netware');
     hp2:=pexported_item(hp2^.next);
   end;
end;


{*****************************************************************************
                                  TLINKERNETWARE
*****************************************************************************}

Constructor TLinkerNetware.Init;
begin
  Inherited Init;
end;


procedure TLinkerNetware.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='nlmconv -T$RES';
     {DllCmd[1]:='ld $OPT -shared -L. -o $EXE $RES';}
     DllCmd[2]:='strip --strip-unneeded $EXE';
   end;
end;


Function TLinkerNetware.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  s,s2         : string;
  found        : boolean;
  ProgNam      : string [80];
  NlmNam       : string [80];
  hp2          : pexported_item;  { for exports }
begin
  WriteResponseFile:=False;

  ProgNam := current_module^.exefilename^;
  i:=Pos(target_os.exeext,ProgNam);
  if i>0 then
    Delete(ProgNam,i,255);
  NlmNam := ProgNam + target_os.exeext;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  if Description <> '' then
    LinkRes.Add('DESCRIPTION "' + Description + '"');
  LinkRes.Add('VERSION '+tostr(dllmajor)+','+tostr(dllminor)+','+tostr(dllrevision));
  LinkRes.Add('SCREENNAME "' + ProgNam + '"');  { for that, we have }
  LinkRes.Add('THREADNAME "' + ProgNam + '"');  { to add comiler directives }
  if stacksize > 1024 then
  begin
    str (stacksize, s);
    LinkRes.Add ('STACKSIZE '+s);
  end;

  { add objectfiles, start with nwpre always }
  LinkRes.Add ('INPUT '+FindObjectFile('nwpre',''));

  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.Add ('INPUT ' + FindObjectFile (s,''));
   end;

  { output file (nlm) }
  LinkRes.Add ('OUTPUT ' + NlmNam);

  { start and stop-procedures }
  LinkRes.Add ('START _Prelude');  { defined in rtl/netware/nwpre.pp }
  LinkRes.Add ('EXIT _Stop');

  //if not (cs_link_strip in aktglobalswitches) then
  { ahhhggg: how do i detect if we have debug-symbols ? }
  LinkRes.Add ('DEBUG');

  { Write staticlibraries, is that correct ? }
  if not StaticLibFiles.Empty then
   begin
     While not StaticLibFiles.Empty do
      begin
        S:=lower (StaticLibFiles.Get);
        if s<>'' then
         begin
           i:=Pos(target_os.staticlibext,S);
           if i>0 then
            Delete(S,i,255);
           S := S + '.imp';
           S := librarysearchpath.FindFile(S,found)+S;
           LinkRes.Add('IMPORT @'+s);
         end
      end;
   end;

  if not SharedLibFiles.Empty then
   begin
     While not SharedLibFiles.Empty do
      begin
        {becuase of upper/lower case mix, we may get duplicate
         names but nlmconv ignores that.
         Here we are setting the import-files for nlmconv. I.e. for
         the module clib or clib.nlm we add IMPORT @clib.imp and also
         the module clib.nlm (autoload)
         ? may it be better to set autoload's via StaticLibFiles ? }
        S:=lower (SharedLibFiles.Get);
        if s<>'' then
         begin
           s2:=s;
           i:=Pos(target_os.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           S := S + '.imp';
           S := librarysearchpath.FindFile(S,found)+S;
           LinkRes.Add('IMPORT @'+s);
           LinkRes.Add('MODULE '+s2);
         end
      end;
   end;

  { write exports }
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) do
   begin
     if not hp2^.is_var then
      begin
        { Export the Symbol
          Warning: The Symbol is converted to upper-case if not explicitly
          specified by >>Exports BlaBla NAME 'BlaBla';<< }
        Comment(V_Debug,'Exporting '+hp2^.name^);
        LinkRes.Add ('EXPORT '+hp2^.name^);
      end
     else
      { really ? }
      Comment(V_Error,'Exporting of variables is not supported under netware');
     hp2:=pexported_item(hp2^.next);
   end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;


function TLinkerNetware.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  DynLinkStr : string[60];
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module^.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(BinStr),CmdStr,true,false);

  { Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
    RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;

end.
{
  $Log$
  Revision 1.3  2000-10-31 22:02:55  peter
    * symtable splitted, no real code changes

  Revision 1.2  2000/09/24 15:06:31  peter
    * use defines.inc

  Revision 1.1  2000/09/11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

}
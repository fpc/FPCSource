{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

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

    Currently generating NetWare-NLM's only work under Linux and win32.
    (see http://home.arcor.de/armin.diehl/fpcnw for binutils working
    with win32) while not included in fpc-releases.

    The following compiler-swiches are supported for NetWare:
    $DESCRIPTION    : NLM-Description, will be displayed at load-time
    $M              : For Stack-Size, Heap-Size will be ignored
    $VERSION x.x.x  : Sets Major, Minor and Revision
    $SCREENNAME     : Sets the ScreenName
    $THREADNAME     : Sets cirrent threadname

    Sorry, Displaying copyright does not work with nlmconv from gnu bunutils
    but there is a patch available.

    Exports will be handled like in win32:
    procedure bla;
    begin
    end;

    exports foo name 'Bar';

    The path to the import-Files (from netware-sdk, see developer.novell.com)
    must be specified by the library-path. All external modules are defined
    as autoload. (Note: the import-files have to be in unix-format for exe2nlm)
    By default, the most import files are included in freepascal.

    i.e. Procedure ConsolePrintf (p:pchar); cdecl; external 'clib.nlm';
    sets IMPORT @clib.imp and MODULE clib.

    If you dont have nlmconv, compile gnu-binutils with
       ./configure --enable-targets=i386-linux,i386-netware
       make all

    Debugging is currently only possible at assembler level with nwdbg, written
    by Jan Beulich. (or with my modified RDebug) Nwdbg supports symbols but it's
    not a source-level debugger. You can get nwdbg from developer.novell.com.
    To enter the debugger from your program, call _EnterDebugger (defined in unit system).

    A sample program:

    Program Hello;
    (*$DESCRIPTION HelloWorldNlm*)
    (*$VERSION 1.2.3*)
    (*$ScreenName Hello*)
    (*$M 8192,8192*)
    begin
      writeLn ('hello world');
    end.

    compile with:
    ppc386 -Tnetware hello

    ToDo:
      - No duplicate imports and autoloads
      - No debug symbols
      - libc support (needs new target)
      - prelude support (needs new compiler switch)
      - a lot of additional units from nwsdk

****************************************************************************
}
unit t_nwm;

{$i fpcdefs.inc}

interface


implementation

  uses
    cutils,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmcpu,cpubase,symsym,
    import,export,link,i_nwm;

  type
    timportlibnetware=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(const func,module:string;index:longint;const name:string);override;
      procedure importvariable(const varname,module:string;const name:string);override;
      procedure generatelib;override;
    end;

    texportlibnetware=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkernetware=class(texternallinker)
    private
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
    end;


{*****************************************************************************
                               TIMPORTLIBNETWARE
*****************************************************************************}

procedure timportlibnetware.preparelib(const s : string);
begin
end;


procedure timportlibnetware.importprocedure(const func,module : string;index : longint;const name : string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
   begin
     aktprocdef.setmangledname(name);
     aktprocdef.has_mangledname:=true;
   end
  else
    message(parser_e_empty_import_name);
end;


procedure timportlibnetware.importvariable(const varname,module:string;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  aktvarsym.set_mangledname(name);
  exclude(aktvarsym.varoptions,vo_is_dll_var);
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


procedure texportlibnetware.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Comment(V_Error,'can''t export with index under netware');
     exit;
   end;
  { use pascal name is none specified }
  if (hp.options and eo_name)=0 then
    begin
       hp.name:=stringdup(hp.sym.name);
       hp.options:=hp.options or eo_name;
    end;
  { now place in correct order }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) and
     (hp.name^>hp2.name^) do
    hp2:=texported_item(hp2.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2.name^=hp.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp.name^);
      exit;
    end;
  if hp2=texported_item(current_module._exports.first) then
    current_module._exports.insert(hp)
  else if assigned(hp2) then
    begin
       hp.next:=hp2;
       hp.previous:=hp2.previous;
       if assigned(hp2.previous) then
         hp2.previous.next:=hp;
       hp2.previous:=hp;
    end
  else
    current_module._exports.concat(hp);
end;


procedure texportlibnetware.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibnetware.generatelib;
var
  hp2 : texported_item;
begin
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if (not hp2.is_var) and
        (hp2.sym.typ=procsym) then
      begin
        { the manglednames can already be the same when the procedure
          is declared with cdecl }
        if tprocsym(hp2.sym).defs^.def.mangledname<>hp2.name^ then
         begin
{$ifdef i386}
           { place jump in codesegment }
           codesegment.concat(Tai_align.Create_op(4,$90));
           codeSegment.concat(Tai_symbol.Createname_global(hp2.name^,0));
           codeSegment.concat(Taicpu.Op_sym(A_JMP,S_NO,current_library.newasmsymbol(tprocsym(hp2.sym).defs^.def.mangledname)));
           codeSegment.concat(Tai_symbol_end.Createname(hp2.name^));
{$endif i386}
         end;
      end
     else
      //Comment(V_Error,'Exporting of variables is not supported under netware');
      Message1(parser_e_no_export_of_variables_for_target,'netware');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERNETWARE
*****************************************************************************}

Constructor TLinkerNetware.Create;
begin
  Inherited Create;
end;


procedure TLinkerNetware.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='nlmconv -T$RES';
     {DllCmd[2]:='strip --strip-unneeded $EXE';}
   end;
end;


Function TLinkerNetware.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  s,s2         : string;
  ProgNam      : string [80];
  NlmNam       : string [80];
  hp2          : texported_item;  { for exports }
  p            : byte;
begin
  WriteResponseFile:=False;

  ProgNam := current_module.exefilename^;
  i:=Pos(target_info.exeext,ProgNam);
  if i>0 then
    Delete(ProgNam,i,255);
  NlmNam := ProgNam + target_info.exeext;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  p := Pos ('"', Description);
  while (p > 0) do
  begin
    delete (Description,p,1);
    p := Pos ('"', Description);
  end;
  if Description <> '' then
    LinkRes.Add('DESCRIPTION "' + Description + '"');
  LinkRes.Add('VERSION '+tostr(dllmajor)+','+tostr(dllminor)+','+tostr(dllrevision));

  p := Pos ('"', nwscreenname);
  while (p > 0) do
  begin
    delete (nwscreenname,p,1);
    p := Pos ('"', nwscreenname);
  end;
  p := Pos ('"', nwthreadname);
  while (p > 0) do
  begin
    delete (nwthreadname,p,1);
    p := Pos ('"', nwthreadname);
  end;
  p := Pos ('"', nwcopyright);
  while (p > 0) do
  begin
    delete (nwcopyright,p,1);
    p := Pos ('"', nwcopyright);
  end;

  if nwscreenname <> '' then
    LinkRes.Add('SCREENNAME "' + nwscreenname + '"');
  if nwthreadname <> '' then
    LinkRes.Add('THREADNAME "' + nwthreadname + '"');
  if nwcopyright <> '' then
    LinkRes.Add('COPYRIGHT "' + nwcopyright + '"');

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
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.Add ('INPUT ' + FindObjectFile (s,''));
   end;

  { output file (nlm) }
  LinkRes.Add ('OUTPUT ' + NlmNam);

  { start and stop-procedures }
  LinkRes.Add ('START _Prelude');  { defined in rtl/netware/nwpre.pp }
  LinkRes.Add ('EXIT _Stop');
  LinkRes.Add ('CHECK FPC_NW_CHECKFUNCTION');

  if not (cs_link_strip in aktglobalswitches) then
    LinkRes.Add ('DEBUG');

  { Write staticlibraries, is that correct ? }
  if not StaticLibFiles.Empty then
   begin
     While not StaticLibFiles.Empty do
      begin
        S:=lower (StaticLibFiles.GetFirst);
        if s<>'' then
         begin
       {ad: that's a hack !
        whith -XX we get the .a files as static libs (in addition to the
        imported libraries}
       if (pos ('.a',s) <> 0) OR (pos ('.A', s) <> 0) then
       begin
         LinkRes.Add ('INPUT '+FindObjectFile(s,''));
       end else
       begin
             i:=Pos(target_info.staticlibext,S);
             if i>0 then
               Delete(S,i,255);
             S := S + '.imp';
             librarysearchpath.FindFile(S,s);
             LinkRes.Add('IMPORT @'+s);
       end;
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
        S:=lower (SharedLibFiles.GetFirst);
        if s<>'' then
         begin
           s2:=s;
           i:=Pos(target_info.sharedlibext,S);
           if i>0 then
             Delete(S,i,255);
           S := S + '.imp';
           librarysearchpath.FindFile(S,s);
           LinkRes.Add('IMPORT @'+s);
           LinkRes.Add('MODULE '+s2);
         end
      end;
   end;

  { write exports }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if not hp2.is_var then
      begin
        { Export the Symbol }
        Comment(V_Debug,'Exporting '+hp2.name^);
        LinkRes.Add ('EXPORT '+hp2.name^);
      end
     else
      { really, i think it is possible }
      Comment(V_Error,'Exporting of variables is not supported under netware');
     hp2:=texported_item(hp2.next);
   end;

{ Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

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
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.exefilename^);
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


{*****************************************************************************
                                     Initialize
*****************************************************************************}


initialization
  RegisterLinker(ld_i386_netware,TLinkerNetware);
  RegisterImport(system_i386_netware,TImportLibNetware);
  RegisterExport(system_i386_netware,TExportLibNetware);
  RegisterTarget(system_i386_netware_info);
end.
{
  $Log$
  Revision 1.27  2002-08-11 13:24:20  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.26  2002/07/26 21:15:46  florian
    * rewrote the system handling

  Revision 1.25  2002/07/01 18:46:35  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/05/18 13:34:27  peter
    * readded missing revisions

  Revision 1.23  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.21  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.20  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.19  2002/04/19 15:46:05  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.18  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.17  2002/03/30 09:09:47  armin
  + support check-function for netware

  Revision 1.16  2002/03/29 17:19:51  armin
  + allow exports for netware

  Revision 1.15  2002/03/19 20:23:57  armin
  + smart linking now works with netware

  Revision 1.14  2002/03/04 17:54:59  peter
    * allow oridinal labels again

  Revision 1.13  2002/03/03 13:00:39  hajny
    * importprocedure fix by Armin Diehl

}

{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) sunos target

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
unit t_sunos;

{$i fpcdefs.inc}

interface

{ copy from t_linux
// Up to now we use gld since the solaris ld seems not support .res-files}
{-$DEFINE LinkTest} { DON't del link.res and write Info }
{$DEFINE GnuLd} {The other is not implemented }

implementation

  uses
    cutils,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasm,cpuasm,cpubase,symsym,
    import,export,link;

  type
    timportlibsunos=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(const func,module:string;index:longint;const name:string);override;
      procedure importvariable(const varname,module:string;const name:string);override;
      procedure generatelib;override;
    end;

    texportlibsunos=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkersunos=class(tlinker)
    private
      Glibc2,
      Glibc21 : boolean;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
    end;


{*****************************************************************************
                               TIMPORTLIBsunos
*****************************************************************************}

procedure timportlibsunos.preparelib(const s : string);
begin
{$ifDef LinkTest}
  WriteLN('Prepare import: ',s);
{$EndIf}
end;


procedure timportlibsunos.importprocedure(const func,module : string;index : longint;const name : string);
begin
  { insert sharedlibrary }
{$ifDef LinkTest}
  WriteLN('Import: f:',func,' m:',module,' n:',name);
{$EndIf}
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
    aktprocdef.setmangledname(name)
  else
    message(parser_e_empty_import_name);
end;


procedure timportlibsunos.importvariable(const varname,module:string;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  aktvarsym.set_mangledname(name);
  exclude(aktvarsym.varoptions,vo_is_dll_var);
end;


procedure timportlibsunos.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBsunos
*****************************************************************************}

procedure texportlibsunos.preparelib(const s:string);
begin
end;


procedure texportlibsunos.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,'SunOS');
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


procedure texportlibsunos.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibsunos.generatelib;
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
           codeSegment.concat(Taicpu.Op_sym(A_JMP,S_NO,newasmsymbol(tprocsym(hp2.sym).defs^.def.mangledname)));
           codeSegment.concat(Tai_symbol_end.Createname(hp2.name^));
{$endif i386}
         end;
      end
     else
      Message1(parser_e_no_export_of_variables_for_target,'SunOS');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERSUNOS
*****************************************************************************}

Constructor TLinkersunos.Create;
begin
  Inherited Create;
  LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib;/opt/sfw/lib',true);
{$ifdef  LinkTest}
     if (cs_link_staticflag in aktglobalswitches) then  WriteLN('ForceLinkStaticFlag');
     if (cs_link_static in aktglobalswitches) then  WriteLN('LinkStatic-Flag');
     if (cs_link_shared in aktglobalswitches) then  WriteLN('LinkSynamicFlag');
{$EndIf}
end;


procedure TLinkersunos.SetDefaultInfo;
{
  This will also detect which libc version will be used
}
begin
  Glibc2:=false;
  Glibc21:=false;
  with Info do
   begin
{$IFDEF GnuLd}
     ExeCmd[1]:='gld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE $RES';
     DllCmd[1]:='gld $OPT -shared -L. -o $EXE $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     DynamicLinker:=''; { Gnu uses the default }
     Glibc21:=false;
{$ELSE}
    Not Implememted
{$ENDIF}
(* Linux Stuff not needed?
     { first try glibc2 } // muss noch gendert werden
     if FileExists(DynamicLinker) then
      begin
        Glibc2:=true;
        { Check for 2.0 files, else use the glibc 2.1 stub }
        if FileExists('/lib/ld-2.0.*') then
         Glibc21:=false
        else
         Glibc21:=true;
      end
     else
      DynamicLinker:='/lib/ld-linux.so.1';
*)
   end;

end;


Function TLinkersunos.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : TStringListItem;
  s,s2         : string;
  linkdynamic,
  linklibc     : boolean;
begin
  WriteResponseFile:=False;
{ set special options for some targets }
  linkdynamic:=not(SharedLibFiles.empty);
{  linkdynamic:=false; // da nicht getestet }
  linklibc:=(SharedLibFiles.Find('c')<>nil);
  prtobj:='prt0';
  cprtobj:='cprt0';
  gprtobj:='gprt0';
  if cs_profile in aktmoduleswitches then
   begin
     prtobj:=gprtobj;
     if not glibc2 then
      AddSharedLibrary('gmon');
     AddSharedLibrary('c');
     linklibc:=true;
   end
  else
   begin
     if linklibc then
       prtobj:=cprtobj
      else
       AddSharedLibrary('c'); { quick hack: this sunos implementation needs alwys libc }
   end;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+HPath.Str+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+HPath.Str+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(FindObjectFile(prtobj,''));
  { try to add crti and crtbegin if linking to C }
  if linklibc then { Needed in sunos? }
   begin
{     if librarysearchpath.FindFile('crtbegin.o',s) then
      LinkRes.AddFileName(s);}
     if librarysearchpath.FindFile('crti.o',s) then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(s)
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     LinkRes.Add('INPUT(');
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.GetFirst;
        if s<>'c' then
         begin
           i:=Pos(target_info.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s);
         end
        else
         begin
           linklibc:=true;
           linkdynamic:=false; { libc will include the ld-sunos (war ld-linux) for us }
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      LinkRes.Add('-lc');
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then begin
      LinkRes.Add('-lgcc');
     end;
     if linkdynamic and (Info.DynamicLinker<>'') then { gld has a default, DynamicLinker is not set in sunos }
       LinkRes.AddFileName(Info.DynamicLinker);
     LinkRes.Add(')');
   end;
  { objects which must be at the end }
  if linklibc then {needed in sunos ? }
   begin
     if {librarysearchpath.FindFile('crtend.o',s1) or}
        librarysearchpath.FindFile('crtn.o',s2) then
      begin
        LinkRes.Add('INPUT(');
{        LinkRes.AddFileName(s1);}
        LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;
{ Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;


function TLinkersunos.MakeExecutable:boolean;
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
  if (cs_link_staticflag in aktglobalswitches) then
    StaticStr:='-Bstatic';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;
  { sunos sets DynamicLinker, but gld will (hopefully) defaults to -Bdynamic and add the default-linker }
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
{$IFNDEF LinkTest}
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);
{$ENDIF}
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkersunos.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module.sharedlibfilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  success:=DoExec(FindUtil(binstr),cmdstr,true,false);

{ Strip the library ? }
  if success and (cs_link_strip in aktglobalswitches) then
   begin
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',current_module.sharedlibfilename^);
     success:=DoExec(FindUtil(binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
{$IFNDEF LinkTest}
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);
{$ENDIF}
  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
       target_i386_sunos_info : ttargetinfo =
          (
            target       : target_i386_sunos;
            name         : 'SunOS/ELF for i386';
            shortname    : 'SunOS';
            flags        : [tf_under_development];
            cpu          : cpu_i386;
            unit_env     : 'SUNOSUNITS';
            extradefines : 'UNIX;SOLARIS;LIBC';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '.def';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            sharedlibext : '.so';
            staticlibext : '.a';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            sharedClibext : '.so';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : 'lib';
            Cprefix      : '';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_i386_as;
            assemextern  : as_i386_as;
            link         : ld_i386_sunos;
            linkextern   : ld_i386_sunos;
            ar           : ar_gnu_ar;
            res          : res_none;
            script       : script_unix;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 1;
                varalignmin     : 0;
                varalignmax     : 1;
                localalignmin   : 0;
                localalignmax   : 1;
                paraalign       : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 262144;
            DllScanSupported:false;
            use_function_relative_addresses : true
          );


initialization
  RegisterLinker(ld_i386_sunos,TLinkerSunos);
  RegisterImport(target_i386_sunos,TImportLibSunos);
  RegisterExport(target_i386_sunos,TExportLibSunos);
  RegisterTarget(target_i386_sunos_info);
end.
{
  $Log$
  Revision 1.22  2002-05-16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.20  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.19  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.18  2002/04/19 15:46:05  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.17  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.16  2002/03/04 19:10:14  peter
    * removed compiler warnings

  Revision 1.15  2001/12/15 05:29:36  carl
  + crtbegin.o and crtend.o now not linked anymore since they can cause conflicts with
     GCC and the native C library...

  Revision 1.14  2001/12/09 03:37:38  carl
  * Updated SunOS stack size

  Revision 1.13  2001/11/02 22:58:12  peter
    * procsym definition rewrite

  Revision 1.12  2001/09/18 11:32:00  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.11  2001/09/17 21:29:16  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.10  2001/08/12 17:57:07  peter
    * under development flag for targets

  Revision 1.9  2001/08/07 18:47:15  peter
    * merged netbsd start
    * profile for win32

  Revision 1.8  2001/07/01 20:16:21  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.7  2001/06/28 19:46:25  peter
    * added override and virtual for constructors

  Revision 1.6  2001/06/03 15:15:32  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.5  2001/06/02 19:22:44  peter
    * extradefines field added

  Revision 1.4  2001/04/21 15:34:01  peter
    * fixed writing of end objects to not output an empty INPUT()

  Revision 1.3  2001/04/18 22:02:04  peter
    * registration of targets and assemblers

  Revision 1.2  2001/04/13 01:22:22  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.1  2001/02/26 19:43:11  peter
    * moved target units to subdir
}

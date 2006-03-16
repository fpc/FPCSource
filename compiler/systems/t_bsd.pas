{
    Copyright (c) 1998-2002 by Peter Vreman (original Linux)
              (c) 2000      by Marco van de Voort (FreeBSD mods)

    This unit implements support import,export,link routines
    for the (i386)FreeBSD target

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
unit t_bsd;

{$i fpcdefs.inc}

interface


implementation

  uses
    cutils,cclasses,
{$ifdef USE_SYSUTILS}
    sysutils,
{$else USE_SYSUTILS}
    dos,
{$endif USE_SYSUTILS}
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmdata,aasmcpu,cpubase,symsym,symdef,
    import,export,link,i_bsd,
    cgutils,cgbase,cgobj,cpuinfo;

  type
    tdarwinimported_item = class(timported_item)
       procdef : tprocdef;
    end;

    timportlibdarwin=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tglobalvarsym;const name,module:string);override;
      procedure generatelib;override;
      procedure generatesmartlib;override;
    end;

    timportlibbsd=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tglobalvarsym;const name,module:string);override;
      procedure generatelib;override;
    end;

    texportlibbsd=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkerbsd=class(texternallinker)
    private
      LdSupportsNoResponseFile : boolean;
      LibrarySuffix : Char;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
    end;



{*****************************************************************************
                             TIMPORTLIBDARWIN
*****************************************************************************}

    procedure timportlibdarwin.preparelib(const s : string);
      begin
         if current_asmdata.asmlists[al_imports]=nil then
           current_asmdata.asmlists[al_imports]:=TAsmList.create;
      end;


    procedure timportlibdarwin.importprocedure(aprocdef:tprocdef;const module : string;index : longint;const name : string);
      begin
        { insert sharedlibrary }
{        current_module.linkothersharedlibs.add(SplitName(module),link_always); }
      end;


    procedure timportlibdarwin.importvariable(vs:tglobalvarsym;const name,module:string);
      begin
        { insert sharedlibrary }
{        current_module.linkothersharedlibs.add(SplitName(module),link_always); }
        { the rest is handled in the nppcld.pas tppcloadnode }
        vs.set_mangledname(name);
      end;


    procedure timportlibdarwin.generatesmartlib;
      begin
         generatelib;
       end;


    procedure timportlibdarwin.generatelib;
      begin
      end;


{*****************************************************************************
                               TIMPORTLIBBSD
*****************************************************************************}

procedure timportlibbsd.preparelib(const s : string);
begin
end;


procedure timportlibbsd.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_always);
end;


procedure timportlibbsd.importvariable(vs:tglobalvarsym;const name,module:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_always);
  { reset the mangledname and turn off the dll_var option }
  vs.set_mangledname(name);
  exclude(vs.varoptions,vo_is_dll_var);
end;


procedure timportlibbsd.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBBSD
*****************************************************************************}

procedure texportlibbsd.preparelib(const s:string);
begin
end;


procedure texportlibbsd.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,'*bsd/darwin');
     exit;
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
    current_module._exports.concat(hp)
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


procedure texportlibbsd.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibbsd.generatelib;  // straight t_linux copy for now.
var
  hp2 : texported_item;
  sym : tasmsymbol;
  r : treference;
begin
  new_section(current_asmdata.asmlists[al_procedures],sec_code,'',0);
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if (not hp2.is_var) and
        (hp2.sym.typ=procsym) then
      begin
        { the manglednames can already be the same when the procedure
          is declared with cdecl }
        if tprocsym(hp2.sym).first_procdef.mangledname<>hp2.name^ then
         begin
           { place jump in al_procedures }
           current_asmdata.asmlists[al_procedures].concat(tai_align.create(target_info.alignment.procalign));
           current_asmdata.asmlists[al_procedures].concat(Tai_symbol.Createname_global(hp2.name^,AT_FUNCTION,0));
           if (cs_create_pic in aktmoduleswitches) and
             { other targets need to be checked how it works }
             (target_info.system in [system_i386_freebsd]) then
             begin
{$ifdef x86}
               sym:=current_asmdata.newasmsymbol(tprocsym(hp2.sym).first_procdef.mangledname,AB_EXTERNAL,AT_FUNCTION);
               reference_reset_symbol(r,sym,0);
               if cs_create_pic in aktmoduleswitches then
                 r.refaddr:=addr_pic
               else
                 r.refaddr:=addr_full;
               current_asmdata.asmlists[al_procedures].concat(taicpu.op_ref(A_JMP,S_NO,r));
{$endif x86}
             end
           else
             cg.a_jmp_name(current_asmdata.asmlists[al_procedures],tprocsym(hp2.sym).first_procdef.mangledname);
           current_asmdata.asmlists[al_procedures].concat(Tai_symbol_end.Createname(hp2.name^));
         end;
      end
     else
      Message1(parser_e_no_export_of_variables_for_target,'*bsd/darwin');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERBSD
*****************************************************************************}

Constructor TLinkerBSD.Create;
begin
  Inherited Create;
  if not Dontlinkstdlibpath Then
   if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
     LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true)
   else
     { Mac OS X doesn't have a /lib }
     LibrarySearchPath.AddPath('/usr/lib',true)
end;


procedure TLinkerBSD.SetDefaultInfo;
{
  This will also detect which libc version will be used
}
begin
  LibrarySuffix:=' ';
  LdSupportsNoResponseFile := (target_info.system in [system_m68k_netbsd,system_powerpc_darwin,system_i386_darwin]);
  with Info do
   begin
     if LdSupportsNoResponseFile then
       begin
         if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
           begin
             ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE `cat $RES`';
             DllCmd[1]:='ld $OPT -shared -L. -o $EXE `cat $RES`'
           end
         else
           begin
             ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -multiply_defined suppress -L. -o $EXE `cat $RES`';
             DllCmd[1]:='libtool $OPT -dynamic -multiply_defined suppress  -L. -o $EXE `cat $RES`'
           end
       end
     else
       begin
         ExeCmd[1]:='ld $OPT $DYNLINK $STATIC  $GCSECTIONS $STRIP -L. -o $EXE $RES';
         DllCmd[1]:='ld $OPT $INIT $FINI $SONAME -shared -L. -o $EXE $RES';
       end;
     if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
       DllCmd[2]:='strip --strip-unneeded $EXE'
     else
       DllCmd[2]:='strip -x $EXE';
     { first try glibc2 }
{$ifdef GLIBC2} {Keep linux code in place. FBSD might go to a different
                                glibc too once}
     DynamicLinker:='/lib/ld-linux.so.2';
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
{$else}
      DynamicLinker:='';
{$endif}
   end;
end;


Function TLinkerBSD.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : TStringListItem;
  s,s1,s2      : string;
  linkpthread,
  linkdynamic,
  linklibc     : boolean;
  Fl1,Fl2      : Boolean;

begin
  WriteResponseFile:=False;
{ set special options for some targets }
  if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
    begin
      linkdynamic:=not(SharedLibFiles.empty);
      linklibc:=(SharedLibFiles.Find('c')<>nil);
      linkpthread:=(SharedLibFiles.Find('pthread')<>nil);
      if (target_info.system =system_i386_freebsd) and linkpthread Then
        Begin
          if not (cs_link_pthread in aktglobalswitches) Then
            begin
              {delete pthreads from list, in this case it is in libc_r}
              SharedLibFiles.Remove(SharedLibFiles.Find('pthread').str);
              LibrarySuffix:='r';
            end;
        End;
      prtobj:='prt0';
      cprtobj:='cprt0';
      gprtobj:='gprt0';
      if cs_profile in aktmoduleswitches then
       begin
         prtobj:=gprtobj;
         AddSharedLibrary('c');
         LibrarySuffix:='p';
         linklibc:=true;
       end
      else
       begin
         if linklibc then
          prtobj:=cprtobj;
       end;
    end
  else
    begin
      { for darwin: always link dynamically against libc }
      linklibc := true;
      if not(isdll) then
        if not(cs_profile in aktmoduleswitches) then
          begin
             if librarysearchpath.FindFile('crt1.o',s) then
             prtobj:=s
            else
             prtobj:='/usr/lib/crt1.o';
          end
        else
          prtobj:='/usr/lib/gcrt1.o'
      else
        prtobj:='';
    end;


  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  if (not isdll) then
    begin
      case target_info.system of
        system_powerpc_darwin:
           LinkRes.Add('-arch ppc');
        system_i386_darwin:
           LinkRes.Add('-arch i386');
      end;
  end;
  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add(maybequoted('-L'+HPath.Str))
     else
       LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add(maybequoted('-L'+HPath.Str))
     else
       LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  if not LdSupportsNoResponseFile then
    LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(FindObjectFile(prtobj,'',false));
  { try to add crti and crtbegin if linking to C }
  if linklibc and
     not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
   begin
     if librarysearchpath.FindFile('crtbegin.o',s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crti.o',s) then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(maybequoted(s));
   end;
  if not LdSupportsNoResponseFile then
   LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     if not LdSupportsNoResponseFile then
       LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(maybequoted(s))
      end;
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     if not LdSupportsNoResponseFile then
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
           linkdynamic:=false; { libc will include the ld-* for us }
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
       Begin
         If LibrarySuffix=' ' Then
          LinkRes.Add('-lc')
         else
          LinkRes.Add('-lc_'+LibrarySuffix);
         If LibrarySuffix='r' Then
             LinkRes.Add('-lc');
       end;
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then
      LinkRes.Add('-lgcc');
     if linkdynamic and (Info.DynamicLinker<>'') then
      LinkRes.AddFileName(Info.DynamicLinker);
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;
  { objects which must be at the end }
  if linklibc and
     not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
   begin
     Fl1:=librarysearchpath.FindFile('crtend.o',s1);
     Fl2:=librarysearchpath.FindFile('crtn.o',s2);
     if Fl1 or Fl2 then
      begin
        LinkRes.Add('INPUT(');
         If Fl1 Then
        LinkRes.AddFileName(s1);
        If Fl2 Then
         LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;
  { ignore the fact that our relocations are in non-writable sections, }
  { will be fixed once we have pic support                             }
  if isdll and
     (target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
    LinkRes.Add('-read_only_relocs suppress');
{ Write and Close response }
  linkres.writetodisk;
  linkres.Free;

  WriteResponseFile:=True;
end;


function TLinkerBSD.MakeExecutable:boolean;
var
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
  DynLinkStr : string[60];
  GCSectionsStr,
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';
  GCSectionsStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
    begin
      if (target_info.system=system_m68k_netbsd) and
         ((cs_link_on_target in aktglobalswitches) or
          (target_info.system=source_info.system)) then
        StaticStr:='-Bstatic'
      else
        StaticStr:='-static';
    end;
  if (cs_link_strip in aktglobalswitches) then
   if not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
     StripStr:='-s'
   else
     StripStr:='-x';

  if (cs_link_smart in aktglobalswitches) and
     (tf_smartlink_sections in target_info.flags) then
   GCSectionsStr:='--gc-sections';

  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;

  if CShared Then
   begin
   if  not(target_info.system in [system_powerpc_darwin,system_i386_darwin]) then
     DynLinKStr:=DynLinkStr+' --shared'
    else
     DynLinKStr:=DynLinkStr+' -dynamic'; // one dash!
   end;
{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,LdSupportsNoResponseFile);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerBSD.MakeSharedLibrary:boolean;
var
  InitStr,
  FiniStr,
  SoNameStr : string[80];
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

  InitStr:='-init FPC_LIB_START';
  FiniStr:='-fini FPC_LIB_EXIT';
  SoNameStr:='-soname '+SplitFileName(current_module.sharedlibfilename^);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
{$ifndef darwin}
  Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
{$else darwin}
{$ifdef USE_SYSUTILS}
  Replace(cmdstr,'$EXE',maybequoted(ExpandFileName(current_module.sharedlibfilename^)));
{$else USE_SYSUTILS}
  Replace(cmdstr,'$EXE',maybequoted(FExpand(current_module.sharedlibfilename^)));
{$endif USE_SYSUTILS}
{$endif darwin}
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$INIT',InitStr);
  Replace(cmdstr,'$FINI',FiniStr);
  Replace(cmdstr,'$SONAME',SoNameStr);

  success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,LdSupportsNoResponseFile);

{ Strip the library ? }
  if success and (cs_link_strip in aktglobalswitches) then
   begin
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
     success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef x86_64}
  RegisterExternalLinker(system_x86_64_FreeBSD_info,TLinkerBSD);
  RegisterImport(system_x86_64_freebsd,timportlibbsd);
  RegisterExport(system_x86_64_freebsd,texportlibbsd);
  RegisterTarget(system_x86_64_freebsd_info);
{$endif}
{$ifdef i386}
  RegisterExternalLinker(system_i386_FreeBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_i386_NetBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_i386_OpenBSD_info,TLinkerBSD);
  RegisterImport(system_i386_freebsd,timportlibbsd);
  RegisterExport(system_i386_freebsd,texportlibbsd);
  RegisterTarget(system_i386_freebsd_info);
  RegisterImport(system_i386_netbsd,timportlibbsd);
  RegisterExport(system_i386_netbsd,texportlibbsd);
  RegisterTarget(system_i386_netbsd_info);
  RegisterImport(system_i386_openbsd,timportlibbsd);
  RegisterExport(system_i386_openbsd,texportlibbsd);
  RegisterTarget(system_i386_openbsd_info);
  RegisterExternalLinker(system_i386_darwin_info,TLinkerBSD);
  RegisterImport(system_i386_darwin,timportlibdarwin);
  RegisterExport(system_i386_darwin,texportlibbsd);
  RegisterTarget(system_i386_darwin_info);
{$endif i386}
{$ifdef m68k}
//  RegisterExternalLinker(system_m68k_FreeBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_m68k_NetBSD_info,TLinkerBSD);
  RegisterImport(system_m68k_netbsd,timportlibbsd);
  RegisterExport(system_m68k_netbsd,texportlibbsd);
  RegisterTarget(system_m68k_netbsd_info);
{$endif m68k}
{$ifdef powerpc}
//  RegisterExternalLinker(system_m68k_FreeBSD_info,TLinkerBSD);
  RegisterExternalLinker(system_powerpc_darwin_info,TLinkerBSD);
  RegisterImport(system_powerpc_darwin,timportlibdarwin);
  RegisterExport(system_powerpc_darwin,texportlibbsd);
  RegisterTarget(system_powerpc_darwin_info);
  RegisterExternalLinker(system_powerpc_netbsd_info,TLinkerBSD);
  RegisterImport(system_powerpc_netbsd,timportlibbsd);
  RegisterExport(system_powerpc_netbsd,texportlibbsd);
  RegisterTarget(system_powerpc_netbsd_info);
{$endif powerpc}
end.

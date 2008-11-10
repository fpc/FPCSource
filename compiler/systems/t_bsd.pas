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
    sysutils,
    cutils,cfileutl,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmdata,aasmcpu,cpubase,symsym,symdef,
    import,export,link,comprsrc,rescmn,i_bsd,expunix,
    cgutils,cgbase,cgobj,cpuinfo,ogbase;

  type
    timportlibdarwin=class(timportlib)
      procedure generatelib;override;
    end;

    timportlibbsd=class(timportlib)
      procedure generatelib;override;
    end;

    texportlibbsd=class(texportlibunix)
    end;

    texportlibdarwin=class(texportlibbsd)
      procedure setinitname(list: TAsmList; const s: string); override;
      procedure setfininame(list: TAsmList; const s: string); override;
    end;

    tlinkerbsd=class(texternallinker)
    private
      LdSupportsNoResponseFile : boolean;
      LibrarySuffix : Char;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
      Function GetDarwinPrtobjName(isdll: boolean): TCmdStr;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
      procedure LoadPredefinedLibraryOrder; override;
    end;



{*****************************************************************************
                             TIMPORTLIBDARWIN
*****************************************************************************}

    procedure timportlibdarwin.generatelib;
      begin
      end;


{*****************************************************************************
                             TEXPORTLIBDARWIN
*****************************************************************************}

    procedure texportlibdarwin.setinitname(list: TAsmList; const s: string);
      begin
        list.concat(tai_directive.create(asd_mod_init_func,''));
        list.concat(tai_align.create(sizeof(pint)));
        list.concat(Tai_const.Createname(s,0));
      end;


    procedure texportlibdarwin.setfininame(list: TAsmList; const s: string);
      begin
        list.concat(tai_directive.create(asd_mod_term_func,''));
        list.concat(tai_align.create(sizeof(pint)));
        list.concat(Tai_const.Createname(s,0));
      end;


{*****************************************************************************
                               TIMPORTLIBBSD
*****************************************************************************}

    procedure timportlibbsd.generatelib;
      var
        i : longint;
        ImportLibrary : TImportLibrary;
      begin
        for i:=0 to current_module.ImportLibraryList.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(current_module.ImportLibraryList[i]);
            current_module.linkothersharedlibs.add(ImportLibrary.Name,link_always);
          end;
      end;


{*****************************************************************************
                                  TLINKERBSD
*****************************************************************************}

Constructor TLinkerBSD.Create;
begin
  Inherited Create;
  if not Dontlinkstdlibpath Then
   if not(target_info.system in systems_darwin) then
     LibrarySearchPath.AddPath(sysrootpath,'/lib;/usr/lib;/usr/X11R6/lib',true)
   else
     { Mac OS X doesn't have a /lib }
     LibrarySearchPath.AddPath(sysrootpath,'/usr/lib',true)
end;


procedure TLinkerBSD.SetDefaultInfo;
{
  This will also detect which libc version will be used
}
begin
  LibrarySuffix:=' ';
  LdSupportsNoResponseFile := (target_info.system in ([system_m68k_netbsd]+systems_darwin));
  with Info do
   begin
     if LdSupportsNoResponseFile then
       begin
         if not(target_info.system in systems_darwin) then
           begin
             ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE `cat $RES`';
             DllCmd[1]:='ld $OPT -shared -L. -o $EXE `cat $RES`'
           end
         else
           begin
             ExeCmd[1]:='ld $PRTOBJ $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -multiply_defined suppress -L. -o $EXE `cat $RES`';
             if (apptype<>app_bundle) then
               DllCmd[1]:='libtool $PRTOBJ $OPT -dynamic -multiply_defined suppress -L. -o $EXE `cat $RES`'
             else
               DllCmd[1]:='ld $PRTOBJ $OPT -dynamic -bundle -multiply_defined suppress -L. -o $EXE `cat $RES`'
           end
       end
     else
       begin
         ExeCmd[1]:='ld $OPT $DYNLINK $STATIC  $GCSECTIONS $STRIP -L. -o $EXE $RES';
         DllCmd[1]:='ld $OPT $INIT $FINI $SONAME -shared -L. -o $EXE $RES';
       end;
     if not(target_info.system in systems_darwin) then
       DllCmd[2]:='strip --strip-unneeded $EXE'
     else
       DllCmd[2]:='strip -x $EXE';
      DynamicLinker:='';
   end;
end;

procedure TLinkerBSD.LoadPredefinedLibraryOrder;
// put your linkorder/linkalias overrides here.
// Note: assumes only called when reordering/aliasing is used.
Begin
  if not(target_info.system in systems_darwin) then
    begin
      if (target_info.system =system_i386_freebsd) and
         not (cs_link_no_default_lib_order in  current_settings.globalswitches) Then
        Begin
          LinkLibraryOrder.add('gcc','',15);
          LinkLibraryOrder.add('c','',50);		     // c and c_p mutual. excl?
          LinkLibraryOrder.add('c_p','',55);
          LinkLibraryOrder.add('pthread','',75);	     // pthread and c_r should be mutually exclusive
          LinkLibraryOrder.add('c_r','',76);
          LinkLibraryOrder.add('kvm','',80);		     // must be before ncurses
          if (cs_link_pthread in current_settings.globalswitches) Then     // convert libpthread to libc_r.
            LinkLibraryAliases.add('pthread','c_r');
        end;
    end
else
    begin
          LinkLibraryOrder.add('gcc','',15);
          LinkLibraryOrder.add('c','',50);
   end;
End;


Function TLinkerBSD.GetDarwinPrtobjName(isdll: boolean): TCmdStr;
begin
  if not(isdll) then
    if not(cs_profile in current_settings.moduleswitches) then
      begin
        if not librarysearchpath.FindFile('crt1.o',false,result) then
          result:='/usr/lib/crt1.o';
      end
    else
      begin
        if not librarysearchpath.FindFile('gcrt1.o',false,result) then
          result:='/usr/lib/gcrt1.o';
      end
  else
    begin
      if (apptype=app_bundle) then
        begin
          if not librarysearchpath.FindFile('bundle1.o',false,result) then
            result:='/usr/lib/bundle1.o'
        end
      else
        result:=''
    end;
end;    


Function TLinkerBSD.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : TCmdStrListItem;
  s,s1,s2      : TCmdStr;
  linkdynamic,
  linklibc     : boolean;
  Fl1,Fl2      : Boolean;
  IsDarwin     : Boolean;
  ReOrder      : Boolean;

begin
  WriteResponseFile:=False;
  ReOrder:=False;
  IsDarwin:=target_info.system in systems_darwin;
{ set special options for some targets }
  if not IsDarwin Then
    begin
      prtobj:='prt0';
      cprtobj:='cprt0';
      gprtobj:='gprt0';
      linkdynamic:=not(SharedLibFiles.empty);
      linklibc:=(SharedLibFiles.Find('c')<>nil);
      // this one is a bit complex.
      // Only reorder for now if -XL or -XO params are given
      // or when -Xf.
      reorder:= linklibc and
                (
                  ReorderEntries
                   or
                  (cs_link_pthread in current_settings.globalswitches));
      if cs_profile in current_settings.moduleswitches then
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
      // after this point addition of shared libs not allowed.
    end
  else
    begin
      { for darwin: always link dynamically against libc }
      linklibc := true;
{$ifdef MACOSX104ORHIGHER}
      { not sure what this is for, but gcc always links against it }
      if not(cs_profile in current_settings.moduleswitches) then
        AddSharedLibrary('SystemStubs')
      else
        AddSharedLibrary('SystemStubs_profile');
{$endif MACOSX104ORHIGHER}
      reorder:=reorderentries;
      prtobj:='';
    end;

  if reorder Then
     ExpandAndApplyOrder(SharedLibFiles);

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  if (target_info.system in systems_darwin) and
     (sysrootpath<>'') then
    begin
      LinkRes.Add('-syslibroot');
      LinkRes.Add(sysrootpath);
    end;

  if (not isdll) or
     (apptype=app_bundle) then
    begin
      if (target_info.system in systems_darwin) then
        begin
          LinkRes.Add('-arch');
          case target_info.system of
            system_powerpc_darwin:
              LinkRes.Add('ppc');
            system_i386_darwin:
              LinkRes.Add('i386');
            system_powerpc64_darwin:
              LinkRes.Add('ppc64');
            system_x86_64_darwin:
              LinkRes.Add('x86_64');
            system_arm_darwin:
              LinkRes.Add('arm');
          end;
      end;
  end;
  { Write path to search libraries }
  HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add('-L'+HPath.Str)
     else
       LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TCmdStrListItem(HPath.Next);
   end;
  HPath:=TCmdStrListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     if LdSupportsNoResponseFile then
       LinkRes.Add('-L'+HPath.Str)
     else
       LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TCmdStrListItem(HPath.Next);
   end;

  if (target_info.system in systems_darwin) then
    begin
      HPath:=TCmdStrListItem(current_module.localframeworksearchpath.First);
      while assigned(HPath) do
       begin
         LinkRes.Add('-F'+HPath.Str);
         HPath:=TCmdStrListItem(HPath.Next);
       end;
      HPath:=TCmdStrListItem(FrameworkSearchPath.First);
      while assigned(HPath) do
       begin
         LinkRes.Add('-F'+HPath.Str);
         HPath:=TCmdStrListItem(HPath.Next);
       end;
    end;

  if not LdSupportsNoResponseFile then
    LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(FindObjectFile(prtobj,'',false));
  { try to add crti and crtbegin if linking to C }
  if linklibc and
     not IsDarwin Then
   begin
     if librarysearchpath.FindFile('crtbegin.o',false,s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crti.o',false,s) then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      if LdSupportsNoResponseFile then
        LinkRes.AddFileName(s)
      else
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
        if LdSupportsNoResponseFile then
          LinkRes.AddFileName(s)
        else
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
        if (s<>'c') or reorder then
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
     if linklibc and not reorder then
       Begin
         If LibrarySuffix=' ' Then
          LinkRes.Add('-lc')
         else
          LinkRes.Add('-lc_'+LibrarySuffix);
         If LibrarySuffix='r' Then
             LinkRes.Add('-lc');
       end;
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in current_settings.globalswitches) then
      LinkRes.Add('-lgcc');
     if linkdynamic and (Info.DynamicLinker<>'') then
      LinkRes.AddFileName(Info.DynamicLinker);
     if not LdSupportsNoResponseFile then
       LinkRes.Add(')');
   end;
   
  { frameworks for Darwin }
  if IsDarwin then
    while not FrameworkFiles.empty do
      begin
        LinkRes.Add('-framework');
        LinkRes.Add(FrameworkFiles.GetFirst);
      end;
     
  { objects which must be at the end }
  if linklibc and
     not IsDarwin Then
   begin
     Fl1:=librarysearchpath.FindFile('crtend.o',false,s1);
     Fl2:=librarysearchpath.FindFile('crtn.o',false,s2);
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
{ Write and Close response }
  linkres.writetodisk;
  linkres.Free;

  WriteResponseFile:=True;
end;


function TLinkerBSD.MakeExecutable:boolean;
var
  binstr,
  cmdstr,
  extdbgbinstr,
  extdbgcmdstr: TCmdStr;
  linkscript: TAsmScript;
  DynLinkStr : string[60];
  GCSectionsStr,
  StaticStr,
  StripStr   : string[40];
  success : boolean;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';
  GCSectionsStr:='';
  if (cs_link_staticflag in current_settings.globalswitches) then
    begin
      if (target_info.system=system_m68k_netbsd) and
         ((cs_link_on_target in current_settings.globalswitches) or
          (target_info.system=source_info.system)) then
        StaticStr:='-Bstatic'
      else
        StaticStr:='-static';
    end;
  if (cs_link_strip in current_settings.globalswitches) then
    if (target_info.system in systems_darwin) then
      StripStr:='-x'
    else
      StripStr:='-s';

  if (cs_link_smart in current_settings.globalswitches) and
     (tf_smartlink_sections in target_info.flags) then
    if not(target_info.system in systems_darwin) then
      GCSectionsStr:='--gc-sections'
    else
      GCSectionsStr:='-dead_strip';

   if(not(target_info.system in systems_darwin) and
      (cs_profile in current_settings.moduleswitches)) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;

  if CShared Then
   begin
   if not(target_info.system in systems_darwin) then
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
  if (target_info.system in systems_darwin) then
    Replace(cmdstr,'$PRTOBJ',GetDarwinPrtobjName(false));
  BinStr:=FindUtil(utilsprefix+BinStr);

  { create dsym file? }
  extdbgbinstr:='';
  extdbgcmdstr:='';
  if (target_info.system in systems_darwin) and
     (target_dbg.id in [dbg_dwarf2,dbg_dwarf3]) and
     (cs_link_separate_dbg_file in current_settings.globalswitches) then
    begin
      extdbgbinstr:=FindUtil(utilsprefix+'dsymutil');
      extdbgcmdstr:=maybequoted(current_module.exefilename^);
    end;

  if (LdSupportsNoResponseFile) and
     not(cs_link_nolink in current_settings.globalswitches) then
    begin
      { we have to use a script to use the IFS hack }
      linkscript:=TAsmScriptUnix.create(outputexedir+'ppaslink');
      linkscript.AddLinkCommand(BinStr,CmdStr,'');
      if (extdbgcmdstr<>'') then
        linkscript.AddLinkCommand(extdbgbinstr,extdbgcmdstr,'');
      linkscript.WriteToDisk;
      BinStr:=linkscript.fn;
      if not path_absolute(BinStr) then
        BinStr:='./'+BinStr;
      CmdStr:='';
    end;

  success:=DoExec(BinStr,CmdStr,true,LdSupportsNoResponseFile);
  if (success and
      (extdbgbinstr<>'') and
      (cs_link_nolink in current_settings.globalswitches)) then
    success:=DoExec(extdbgbinstr,extdbgcmdstr,false,LdSupportsNoResponseFile);

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   begin
     DeleteFile(outputexedir+Info.ResName);
     if LdSupportsNoResponseFile Then
       begin
         DeleteFile(linkscript.fn);
         linkscript.free
       end; 
   end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerBSD.MakeSharedLibrary:boolean;
var
  InitStr,
  FiniStr,
  SoNameStr : string[80];
  linkscript: TAsmScript;
  binstr,
  cmdstr,
  extdbgbinstr,
  extdbgcmdstr  : TCmdStr;
  exportedsyms: text;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

  InitStr:='-init FPC_LIB_START';
  FiniStr:='-fini FPC_LIB_EXIT';
  SoNameStr:='-soname '+ExtractFileName(current_module.sharedlibfilename^);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
{$ifndef darwin}
  Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
{$else darwin}
  Replace(cmdstr,'$EXE',maybequoted(ExpandFileName(current_module.sharedlibfilename^)));
{$endif darwin}
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$INIT',InitStr);
  Replace(cmdstr,'$FINI',FiniStr);
  Replace(cmdstr,'$SONAME',SoNameStr);
  if (target_info.system in systems_darwin) then
    Replace(cmdstr,'$PRTOBJ',GetDarwinPrtobjName(true));
  BinStr:=FindUtil(utilsprefix+BinStr);

  { create dsym file? }
  extdbgbinstr:='';
  extdbgcmdstr:='';
  if (target_info.system in systems_darwin) and
     (target_dbg.id in [dbg_dwarf2,dbg_dwarf3]) and
     (cs_link_separate_dbg_file in current_settings.globalswitches) then
    begin
      extdbgbinstr:=FindUtil(utilsprefix+'dsymutil');
      extdbgcmdstr:=maybequoted(current_module.sharedlibfilename^);
    end;

  if (target_info.system in systems_darwin) then
    begin
      { exported symbols for darwin }
      if not texportlibunix(exportlib).exportedsymnames.empty then
        begin
          assign(exportedsyms,outputexedir+'linksyms.fpc');
          rewrite(exportedsyms);
          repeat
            writeln(exportedsyms,texportlibunix(exportlib).exportedsymnames.getfirst);
          until texportlibunix(exportlib).exportedsymnames.empty;
          close(exportedsyms);
          cmdstr:=cmdstr+' -exported_symbols_list '+maybequoted(outputexedir)+'linksyms.fpc';
        end;
    end;

  if (LdSupportsNoResponseFile) and
     not(cs_link_nolink in current_settings.globalswitches) then
    begin
      { we have to use a script to use the IFS hack }
      linkscript:=TAsmScriptUnix.create(outputexedir+'ppaslink');
      linkscript.AddLinkCommand(BinStr,CmdStr,'');
      if (extdbgbinstr<>'') then
        linkscript.AddLinkCommand(extdbgbinstr,extdbgcmdstr,'');
      linkscript.WriteToDisk;
      BinStr:=linkscript.fn;
      if not path_absolute(BinStr) then
        BinStr:='./'+BinStr;
      CmdStr:='';
    end;

  success:=DoExec(BinStr,cmdstr,true,LdSupportsNoResponseFile);
  if (success and
      (extdbgbinstr<>'') and
      (cs_link_nolink in current_settings.globalswitches)) then
    success:=DoExec(extdbgbinstr,extdbgcmdstr,false,LdSupportsNoResponseFile);

{ Strip the library ? }
  if success and (cs_link_strip in current_settings.globalswitches) then
   begin
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
     success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,false,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    begin
      DeleteFile(outputexedir+Info.ResName);
      if LdSupportsNoResponseFile Then
        begin
          DeleteFile(linkscript.fn);
          linkscript.free
        end;
      if (target_info.system in systems_darwin) then
        DeleteFile(outputexedir+'linksyms.fpc');
    end;     

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

  RegisterExternalLinker(system_x86_64_darwin_info,TLinkerBSD);
  RegisterImport(system_x86_64_darwin,timportlibdarwin);
  RegisterExport(system_x86_64_darwin,texportlibdarwin);
  RegisterTarget(system_x86_64_darwin_info);
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
  RegisterExport(system_i386_darwin,texportlibdarwin);
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
  RegisterExport(system_powerpc_darwin,texportlibdarwin);
  RegisterTarget(system_powerpc_darwin_info);

  RegisterExternalLinker(system_powerpc_netbsd_info,TLinkerBSD);
  RegisterImport(system_powerpc_netbsd,timportlibbsd);
  RegisterExport(system_powerpc_netbsd,texportlibbsd);
  RegisterTarget(system_powerpc_netbsd_info);
{$endif powerpc}
{$ifdef powerpc64}
  RegisterExternalLinker(system_powerpc64_darwin_info,TLinkerBSD);
  RegisterImport(system_powerpc64_darwin,timportlibdarwin);
  RegisterExport(system_powerpc64_darwin,texportlibdarwin);
  RegisterTarget(system_powerpc64_darwin_info);
{$endif powerpc64}
{$ifdef arm}
  RegisterExternalLinker(system_arm_darwin_info,TLinkerBSD);
  RegisterImport(system_arm_darwin,timportlibdarwin);
  RegisterExport(system_arm_darwin,texportlibdarwin);
  RegisterTarget(system_arm_darwin_info);
{$endif arm}

  RegisterRes(res_elf_info,TWinLikeResourceFile);
  RegisterRes(res_macho_info,TWinLikeResourceFile);
end.

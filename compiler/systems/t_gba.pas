{
    This unit implements support import,export,link routines
    for the (arm) GameBoy Advance target
    
    Copyright (c) 2001-2002 by Peter Vreman

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
unit t_gba;

{$i fpcdefs.inc}


interface

  uses
    symsym,symdef,
    import,export,link;

  type
    tlinkergba=class(texternallinker)
    private
      libctype:(libc5,glibc2,glibc21,uclibc);
      Function  WriteResponseFile : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
    end;


implementation


  uses
    cutils,cclasses,verbose,systems,globtype,globals,
    symconst,script,fmodule,dos,aasmbase,aasmtai,aasmcpu,
    cpubase,cgobj,i_gba;



{*****************************************************************************
                                  TLINKERLINUX
*****************************************************************************}

Constructor TLinkerGba.Create;
begin
  Inherited Create;
  if not Dontlinkstdlibpath Then
    LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true);
end;


procedure TLinkerGba.SetDefaultInfo;
{
  This will also detect which libc version will be used
}
begin
  with Info do
   begin
     //ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE $RES';
     // Here we call ld with right options for GBA
     ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -Ttext 0x08000000 -Tbss 0x03000000 -L. -o $EXE $RES';
     DllCmd[1]:='ld $OPT $INIT $FINI $SONAME -shared -L. -o $EXE $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     DynamicLinker:='/lib/ld-linux.so.2';
     libctype:=glibc2;
   end;
end;


Function TLinkerGba.WriteResponseFile: Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : TStringListItem;
  s,s1,s2      : string;
  found1,
  found2,
  linklibc     : boolean;
begin
  WriteResponseFile:=False;
{ set special options for some targets }
  linklibc:=(SharedLibFiles.Find('c')<>nil);
  prtobj:='prt0';
    case libctype of
       glibc21:
         begin
           cprtobj:='cprt21';
           gprtobj:='gprt21';
         end;
       uclibc:
         begin
           cprtobj:='ucprt0';
           gprtobj:='ugprt0';
         end
       else
         cprtobj:='cprt0';
         gprtobj:='gprt0';
     end;
  
  if cs_profile in aktmoduleswitches then
   begin
     prtobj:=gprtobj;
     if not(libctype in [glibc2,glibc21]) then
       AddSharedLibrary('gmon');
     AddSharedLibrary('c');
     linklibc:=true;
   end
  else
   begin
     if linklibc then
      prtobj:=cprtobj;
   end;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+maybequoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(maybequoted(FindObjectFile(prtobj,'',false)));
  { try to add crti and crtbegin if linking to C }
  if linklibc then
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
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(maybequoted(s))
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
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      LinkRes.Add('-lc');
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then
      LinkRes.Add('-lgcc');
     LinkRes.Add(')');
   end;

  { objects which must be at the end }
  if linklibc and (libctype<>uclibc) then
   begin
     found1:=librarysearchpath.FindFile('crtend.o',s1);
     found2:=librarysearchpath.FindFile('crtn.o',s2);
     if found1 or found2 then
      begin
        LinkRes.Add('INPUT(');
        if found1 then
         LinkRes.AddFileName(s1);
        if found2 then
         LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;
{ Write and Close response }
  linkres.writetodisk;
  linkres.Free;

  WriteResponseFile:=True;
end;


function TLinkerGba.MakeExecutable:boolean;
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
  GCSectionsStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
    StaticStr:='-static';
  if (cs_link_strip in aktglobalswitches) then
    StripStr:='-s';
  if (cs_link_smart in aktglobalswitches) and
     (tf_smartlink_sections in target_info.flags) then
    GCSectionsStr:='--gc-sections';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   begin
     DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;
     if cshared Then
       DynLinkStr:='--shared ' + DynLinkStr;
     if rlinkpath<>'' Then
       DynLinkStr:='--rpath-link '+rlinkpath + ' '+ DynLinkStr;
   End;

{ Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(utilsprefix+BinStr),CmdStr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
    RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;



{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_arm_gba_info,TLinkerGba);
  RegisterTarget(system_arm_gba_info);
end.

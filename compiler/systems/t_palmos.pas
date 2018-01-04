{
    Copyright (c) 2001-2002 by Peter Vreman

    This unit implements support import, export, link routines
    for the PalmOS target

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
unit t_palmos;

{$i fpcdefs.inc}

interface

  uses
    link;

  type
    tlinkerPalmOS=class(texternallinker)
    private
       Function  WriteResponseFile : Boolean;
    public
       constructor Create; override;
       procedure SetDefaultInfo; override;
       procedure InitSysInitUnitName; override;
       function  MakeExecutable:boolean; override;
    end;


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,cscript,fmodule,i_palmos,
       comprsrc;

{****************************************************************************
                               TLinkerPalmOS
****************************************************************************}

Constructor TLinkerPalmOS.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerPalmOS.SetDefaultInfo;
begin
  with Info do
   begin
     //ExeCmd[1]:='ldpalm $OPT $STRIP -N -dy -T $SCRIPT -o $EXE @$RES';

     { This is based on my successful experiment with prc-tools remix.
       Anyone who has more insight into this Palm magic, feel free to fix. (KB) }
     ExeCmd[1]:='ld $OPT $STRIP --embedded-relocs --no-check-sections -N -dy -o $EXE $RES';
     ExeCmd[2]:='build-prc $EXE.prc "$APPNAME" $APPID $EXE';
   end;
end;


procedure TLinkerPalmOS.InitSysInitUnitName;
begin
  sysinitunit:='si_prc';
end;


Function TLinkerPalmOS.WriteResponseFile : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TCmdStrListItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

  { Write path to search libraries }
  HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TCmdStrListItem(HPath.Next);
   end;
  HPath:=TCmdStrListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+HPath.Str+')');
     HPath:=TCmdStrListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  if not (target_info.system in systems_internal_sysinit) then
    begin
      { add objectfiles, start with crt0 always  }
      { using crt0, we should stick C compatible }
      LinkRes.AddFileName(FindObjectFile('crt0','',false));
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

  { currently the PalmOS target must be linked always against the C lib }
  {LinkRes.Add('-lcrt');}

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  linklibc:=false;
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
      linklibc:=true;
   end;
  { be sure that libc is the last lib }
  if linklibc then
   begin
     LinkRes.Add('-lc');
     LinkRes.Add('-lgcc');
   end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.Free;

  WriteResponseFile:=True;
end;


function TLinkerPalmOS.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StripStr : string[40];
  i : longint;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Create some replacements }
  StripStr:='';
  if (cs_link_strip in current_settings.globalswitches) then
   StripStr:='-s';

  { Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  success:=false;
  for i:=1 to 2 do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     binstr:=FindUtil(utilsprefix+BinStr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',MaybeQuoted(current_module.exefilename));
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',MaybeQuoted(outputexedir+Info.ResName));
        Replace(cmdstr,'$STRIP',StripStr);
//        Replace(cmdstr,'$SCRIPT',FindUtil('palm.ld'));
        Replace(cmdstr,'$APPNAME',palmos_applicationname);
        Replace(cmdstr,'$APPID',palmos_applicationid);

        success:=DoExec(binstr,cmdstr,(i=1),false);
        if not success then
         break;
      end;
   end;

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef m68k}
  RegisterTarget(system_m68k_palmos_info);
  RegisterLinker(ld_palmos,TLinkerPalmOS);
  RegisterRes(res_m68k_palmos_info,TResourceFile);
{$endif m68k}
{$ifdef arm}
  RegisterTarget(system_arm_palmos_info);
  RegisterLinker(ld_palmos,TLinkerPalmOS);
  RegisterRes(res_arm_palmos_info,TResourceFile);
{$endif arm}
end.

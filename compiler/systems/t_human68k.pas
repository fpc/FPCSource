{
    Copyright (c) 2020 by Free Pascal Development Team

    This unit implements support import, export, link routines
    for the Human 68k (a.k.a. Sharp X68000) target

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
unit t_human68k;

{$i fpcdefs.inc}

interface

    uses
      rescmn, comprsrc, link;

type
  PLinkerHuman68k = ^TLinkerHuman68k;
  TLinkerHuman68k = class(texternallinker)
    private
      Origin: DWord;
      UseVLink: boolean;
      function WriteResponseFile(isdll: boolean): boolean;
      procedure SetHuman68kInfo;
      function MakeHuman68kExe: boolean;
    public
      constructor Create; override;
      procedure SetDefaultInfo; override;
      procedure InitSysInitUnitName; override;
      function  MakeExecutable: boolean; override;
  end;


implementation

    uses
       sysutils,cutils,cfileutl,cclasses,aasmbase,
       globtype,globals,systems,verbose,cscript,fmodule,i_human68k;


constructor TLinkerHuman68k.Create;
begin
  UseVLink:=(cs_link_vlink in current_settings.globalswitches);

  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerHuman68k.SetHuman68kInfo;
begin
  with Info do
   begin
    if not UseVLink then
     begin
      ExeCmd[1]:='ld -b xfile -m human68kxfile $DYNLINK $GCSECTIONS $OPT $MAP -d -n -o $EXE $RES';
     end
    else
     begin
      ExeCmd[1]:='vlink -b xfile $FLAGS $GCSECTIONS $OPT $STRIP $MAP -o $EXE -T $RES';
     end;
   end;
end;


procedure TLinkerHuman68k.SetDefaultInfo;
begin
  if target_info.system = system_m68k_human68k then
    SetHuman68kInfo;
end;


procedure TLinkerHuman68k.InitSysInitUnitName;
begin
  sysinitunit:='si_prc';
end;


function TLinkerHuman68k.WriteResponseFile(isdll: boolean): boolean;
var
  linkres  : TLinkRes;
  HPath    : TCmdStrListItem;
  s        : string;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);
  if UseVLink and (source_info.dirsep <> '/') then
    LinkRes.fForceUseForwardSlash:=true;

  { Write path to search libraries }
  HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
    begin
      s:=HPath.Str;
      if (cs_link_on_target in current_settings.globalswitches) then
        s:=ScriptFixFileName(s);
      LinkRes.Add('-L'+s);
      HPath:=TCmdStrListItem(HPath.Next);
    end;
  HPath:=TCmdStrListItem(LibrarySearchPath.First);
  while assigned(HPath) do
    begin
      s:=HPath.Str;
      if s<>'' then
        LinkRes.Add('SEARCH_DIR("'+s+'")');
      HPath:=TCmdStrListItem(HPath.Next);
    end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  if not (target_info.system in systems_internal_sysinit) then
    begin
      s:=FindObjectFile('prt0','',false);
      LinkRes.AddFileName(maybequoted(s));
    end;
  while not ObjectFiles.Empty do
    begin
      s:=ObjectFiles.GetFirst;
      if s<>'' then
        begin
          { vlink doesn't use SEARCH_DIR for object files }
          if UseVLink then
             s:=FindObjectFile(s,'',false);
          LinkRes.AddFileName(maybequoted(s));
       end;
    end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
    begin
      { vlink doesn't need, and doesn't support GROUP }
      if not UseVLink then
        begin
          LinkRes.Add(')');
          LinkRes.Add('GROUP(');
        end;
      while not StaticLibFiles.Empty do
        begin
          S:=StaticLibFiles.GetFirst;
          LinkRes.AddFileName(maybequoted(s));
        end;
    end;

  LinkRes.Add(')');

  { Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;
end;


function TLinkerHuman68k.MakeHuman68kExe: boolean;
var
  BinStr,
  CmdStr  : TCmdStr;
  StripStr: string[40];
  DynLinkStr : ansistring;
  GCSectionsStr : string;
  FlagsStr : string;
  MapStr: string;
  ExeName: string;
begin
  StripStr:='';
  GCSectionsStr:='';
  DynLinkStr:='';
  MapStr:='';
  FlagsStr:='';

  if (cs_link_map in current_settings.globalswitches) then
    if UseVLink then
      MapStr:='-M'+maybequoted(ScriptFixFileName(current_module.mapfilename))
    else
      MapStr:='-Map '+maybequoted(ScriptFixFileName(current_module.mapfilename));
  if (cs_link_strip in current_settings.globalswitches) then
    StripStr:='-s';
  if rlinkpath<>'' then
    DynLinkStr:='--rpath-link '+rlinkpath;
  if UseVLink then
    begin
      if create_smartlink_sections then
        GCSectionsStr:='-gc-all -sc';
    end
  else
    if (cs_link_smart in current_settings.globalswitches) and
       create_smartlink_sections then
      GCSectionsStr:='--gc-sections';

  ExeName:=current_module.exefilename;

  { Call linker }
  SplitBinCmd(Info.ExeCmd[1],BinStr,CmdStr);
  binstr:=FindUtil(utilsprefix+BinStr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(ExeName)));
  Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
  Replace(cmdstr,'$MAP',MapStr);
  Replace(cmdstr,'$FLAGS',FlagsStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);

  MakeHuman68kExe:=DoExec(BinStr,CmdStr,true,false);
end;


function TLinkerHuman68k.MakeExecutable:boolean;
var
  success : boolean;
  bootfile : TScript;
  ExeName: String;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Write used files and libraries }
  WriteResponseFile(false);

  success:=MakeHuman68kExe;

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;




{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_human68k,TLinkerHuman68k);
  RegisterTarget(system_m68k_human68k_info);
end.

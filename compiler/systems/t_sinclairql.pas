{
    Copyright (c) 2020 by Free Pascal Development Team

    This unit implements support import, export, link routines
    for the m68k Sinclair QL target

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
unit t_sinclairql;

{$i fpcdefs.inc}

interface

    uses
      rescmn, comprsrc, link;

type
  PLinkerSinclairQL = ^TLinkerSinclairQL;
  TLinkerSinclairQL = class(texternallinker)
    private
      Origin: DWord;
      UseVLink: boolean;
      function WriteResponseFile(isdll: boolean): boolean;
      procedure SetSinclairQLInfo;
      function MakeSinclairQLExe: boolean;
    public
      constructor Create; override;
      procedure SetDefaultInfo; override;
      procedure InitSysInitUnitName; override;
      function  MakeExecutable: boolean; override;
  end;


implementation

    uses
       sysutils,cutils,cfileutl,cclasses,aasmbase,
       globtype,globals,systems,verbose,cscript,fmodule,i_sinclairql;


    const
       DefaultOrigin = $20000;


constructor TLinkerSinclairQL.Create;
begin
  UseVLink:=(cs_link_vlink in current_settings.globalswitches);

  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerSinclairQL.SetSinclairQLInfo;
begin
  if ImageBaseSetExplicity then
    Origin:=ImageBase
  else
    Origin:=DefaultOrigin;

  with Info do
   begin
    if not UseVLink then
     begin
      ExeCmd[1]:='ld $DYNLINK $OPT -d -n -o $EXE $RES';
     end
    else
     begin
      ExeCmd[1]:='vlink -b rawbin1 $FLAGS $GCSECTIONS $OPT $STRIP -o $EXE -T $RES';
     end;
   end;
end;


procedure TLinkerSinclairQL.SetDefaultInfo;
begin
  if target_info.system = system_m68k_sinclairql then
    SetSinclairQLInfo;
end;


procedure TLinkerSinclairQL.InitSysInitUnitName;
begin
  sysinitunit:='si_prc';
end;


function TLinkerSinclairQL.WriteResponseFile(isdll: boolean): boolean;
var
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

  with LinkRes do
    begin
      Add('');
      Add('SECTIONS');
      Add('{');
      Add('  . = 0x'+hexstr(Origin,8)+';');
      Add('  .text : { *(.text .text.* _CODE _CODE.* ) }');
      Add('  .data : { *(.data .data.* .rodata .rodata.* .fpc.* ) }');
      Add('  .bss  : { *(_BSS _BSS.*) *(.bss .bss.*) *(_BSSEND _BSSEND.*) *(_HEAP _HEAP.*) *(.stack .stack.*) *(_STACK _STACK.*) }');
      Add('}');
    end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;
end;


function TLinkerSinclairQL.MakeSinclairQLExe: boolean;
var
  BinStr,
  CmdStr  : TCmdStr;
  StripStr: string[40];
  DynLinkStr : string;
  GCSectionsStr : string;
  FlagsStr : string;
  ExeName: string;
begin
  StripStr:='';
  GCSectionsStr:='';
  DynLinkStr:='';
  FlagsStr:='';

  if (cs_link_strip in current_settings.globalswitches) then
    StripStr:='-s';
  if rlinkpath<>'' then
    DynLinkStr:='--rpath-link '+rlinkpath;
  if UseVLink then
    begin
      if create_smartlink_sections then
        GCSectionsStr:='-gc-all -sc';
    end;

  ExeName:=current_module.exefilename;
  if apptype = app_gui then
    Replace(ExeName,target_info.exeext,'.prg');

  { Call linker }
  SplitBinCmd(Info.ExeCmd[1],BinStr,CmdStr);
  binstr:=FindUtil(utilsprefix+BinStr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(ExeName)));
  Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
  Replace(cmdstr,'$FLAGS',FlagsStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);

  MakeSinclairQLExe:=DoExec(BinStr,CmdStr,true,false);
end;


function TLinkerSinclairQL.MakeExecutable:boolean;
var
  success : boolean;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Write used files and libraries }
  WriteResponseFile(false);

  success:=MakeSinclairQLExe;

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;




{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_sinclairql,TLinkerSinclairQL);
  RegisterTarget(system_m68k_sinclairql_info);
end.

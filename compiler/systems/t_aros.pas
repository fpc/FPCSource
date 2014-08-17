{
    Copyright (c) 2004-2006 by Free Pascal Development Team

    This unit implements support import, export, link routines
    for the aros targets (arosOS/i386, arosOS/x86_64)

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
unit t_aros;

{$i fpcdefs.inc}

interface

    uses
      rescmn, comprsrc, import, export,  link, ogbase;


type

  timportlibaros=class(timportlib)
    procedure generatelib; override;
  end;
  
  
  PLinkeraros = ^TLinkeraros;
  TLinkeraros = class(texternallinker)
    private
      function WriteResponseFile(isdll: boolean): boolean;
      procedure Setaros386Info;
      procedure Setarosx86_64Info;
      function Makearos386Exe: boolean;
      function Makearosx86_64Exe: boolean;
    public
      constructor Create; override;
      procedure SetDefaultInfo; override;
      function  MakeExecutable: boolean; override;
  end;


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_aros;


procedure timportlibaros.generatelib;
var
  i: longint;
  ImportLibrary: TImportLibrary;
begin
  for i:=0 to current_module.ImportLibraryList.count -1 do
  begin
    ImportLibrary := TImportlibrary(current_module.ImportLibraryList[i]);
    current_module.linkothersharedlibs.add(ImportLibrary.Name, link_always);
  end;
end;

{****************************************************************************
                               TLinkeraros
****************************************************************************}



constructor TLinkeraros.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;

procedure TLinkeraros.Setaros386Info;
begin
  with Info do begin
    //ExeCmd[1]:='collect-aros $OPT -d -n -o $EXE $RES';
    ExeCmd[1]:='i386-aros-ld $OPT -d -n -o $EXE $RES';
  end;
end;

procedure TLinkeraros.Setarosx86_64Info;
begin
  with Info do begin
    ExeCmd[1]:='x86_64-aros-ld $OPT -defsym=__AROS__=1 -d -q -n -o $EXE $RES';
  end;
end;

procedure TLinkeraros.SetDefaultInfo;
begin
  case (target_info.system) of
    system_i386_aros:      Setaros386Info;
    system_x86_64_aros:   Setarosx86_64Info;
  end;
end;


function TLinkeraros.WriteResponseFile(isdll: boolean): boolean;
var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TCmdStrListItem;
  s,s1     : string;
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
    s1 := Unix2AmigaPath(maybequoted(s)); 
    if trim(s1)<>'' then
     LinkRes.Add('SEARCH_DIR('+s1+')');
    HPath:=TCmdStrListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  s:=FindObjectFile('prt0','',false);
  LinkRes.AddFileName(s);
  while not ObjectFiles.Empty do
   begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
     begin
      LinkRes.AddFileName(Unix2AmigaPath(maybequoted(s)));
     end;
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
    LinkRes.Add(')');
    LinkRes.Add('GROUP(');
    while not StaticLibFiles.Empty do
     begin
      S:=StaticLibFiles.GetFirst;
      LinkRes.AddFileName(Unix2AmigaPath(maybequoted(s)));
     end;
   end;

  if (cs_link_on_target in current_settings.globalswitches) then
   begin
    LinkRes.Add(')');

    { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
      here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
    linklibc:=false;
    while not SharedLibFiles.Empty do
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
        LinkRes.Add('-l'+s);
        linklibc:=true;
       end;
     end;
    { be sure that libc&libgcc is the last lib }
    if linklibc then
     begin
      LinkRes.Add('-lc');
      LinkRes.Add('-lgcc');
     end;
   end
  else
   begin
    while not SharedLibFiles.Empty do
     begin
      S:=SharedLibFiles.GetFirst;
      LinkRes.Add('lib'+s+target_info.staticlibext);
     end;
    LinkRes.Add(')');
   end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;
end;


function TLinkeraros.Makearos386Exe: boolean;
var
  BinStr,
  CmdStr  : TCmdStr;
  StripStr: string[40];
begin
  StripStr:='';
  if (cs_link_strip in current_settings.globalswitches) then StripStr:='-s';

  { Call linker }
  SplitBinCmd(Info.ExeCmd[1],BinStr,CmdStr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(current_module.exefilename)));
  Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
 
  //Replace(cmdstr,'$EXE',Unix2AmigaPath(maybequoted(ScriptFixFileName(current_module.exefilename^))));
 // Replace(cmdstr,'$RES',Unix2AmigaPath(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
  Replace(cmdstr,'$STRIP',StripStr);
  Makearos386Exe:=DoExec(FindUtil(BinStr),CmdStr,true,false);
end;


function TLinkeraros.Makearosx86_64Exe: boolean;
var
  BinStr,
  CmdStr  : TCmdStr;
  StripStr: string[40];
begin
  StripStr:='';
  if (cs_link_strip in current_settings.globalswitches) then StripStr:='-s';

  { Call linker }
  SplitBinCmd(Info.ExeCmd[1],BinStr,CmdStr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$EXE',Unix2AmigaPath(maybequoted(ScriptFixFileName(current_module.exefilename))));
  Replace(cmdstr,'$RES',Unix2AmigaPath(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
  Replace(cmdstr,'$STRIP',StripStr);
  Makearosx86_64Exe:=DoExec(FindUtil(BinStr),CmdStr,true,false);
end;


function TLinkeraros.MakeExecutable:boolean;
var
  success : boolean;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Write used files and libraries }
  WriteResponseFile(false);

  case (target_info.system) of
    system_i386_aros:      success:=Makearos386Exe;
    system_x86_64_aros:   success:=Makearosx86_64Exe;
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
{$ifdef i386}
  RegisterLinker(ld_aros,TLinkeraros);
  RegisterTarget(system_i386_aros_info);
{$endif i386}
{$ifdef x86_64}
  RegisterLinker(ld_aros,TLinkeraros);
  RegisterTarget(system_x86_64_aros_info);
{$endif x86_64}
  RegisterRes(res_elf_info, TWinLikeResourceFile);
end.

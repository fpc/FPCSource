{
    Copyright (c) 2004 by Free Pascal Development Team

    This unit implements support import, export, link routines
    for the MorphOS (PowerPC) target

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
unit t_morph;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_morph,link;

    type
       PlinkerMorphOS=^TlinkerMorphOS;
       TlinkerMorphOS=class(texternallinker)
       private
          Function  WriteResponseFile(isdll:boolean) : Boolean;
       public
          constructor Create; override;
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
       end;

{$IFDEF MORPHOS}
{ * PathConv is implemented in the system unit! * }
function PathConv(path: string): string; external name 'PATHCONV';
{$ELSE}
function PathConv(path: string): string;
begin
  PathConv:=path;
end;
{$ENDIF}

{****************************************************************************
                               TLinkerMorphOS
****************************************************************************}

Constructor TLinkerMorphOS.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerMorphOS.SetDefaultInfo;
begin
  with Info do
   begin
    if (cs_link_on_target in current_settings.globalswitches) then
     begin
      ExeCmd[1]:='ld $OPT -o $EXE $RES';
      ExeCmd[2]:='strip --strip-unneeded --remove-section .comment $EXE';
     end
    else
     begin
      ExeCmd[1]:='fpcvlink -b elf32amiga $OPT $STRIP -o $EXE -T $RES';
     end;
   end;
end;


Function TLinkerMorphOS.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TCmdStrListItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

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
     LinkRes.Add('SEARCH_DIR('+PathConv(maybequoted(s))+')');
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
      { vlink doesn't use SEARCH_DIR for object files }
      if not(cs_link_on_target in current_settings.globalswitches) then
       s:=FindObjectFile(s,'',false);
      LinkRes.AddFileName(PathConv(maybequoted(s)));
     end;
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
    { vlink doesn't need, and doesn't support GROUP }
    if (cs_link_on_target in current_settings.globalswitches) then
     begin
      LinkRes.Add(')');
      LinkRes.Add('GROUP(');
     end;
    while not StaticLibFiles.Empty do
     begin
      S:=StaticLibFiles.GetFirst;
      LinkRes.AddFileName(PathConv(maybequoted(s)));
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


function TLinkerMorphOS.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StripStr: string[40];
begin

  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

  if not (cs_link_on_target in current_settings.globalswitches) then
   begin
    StripStr:='';
    if (cs_link_strip in current_settings.globalswitches) then
     StripStr:='-s -P __abox__';
   end;

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  if not(cs_link_on_target in current_settings.globalswitches) then
   begin
    Replace(cmdstr,'$EXE',PathConv(maybequoted(ScriptFixFileName(current_module.exefilename^))));
    Replace(cmdstr,'$RES',PathConv(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
    Replace(cmdstr,'$STRIP',StripStr);
   end
  else
   begin
    Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(current_module.exefilename^)));
    Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
   end;
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Stripping Enabled? }
  { For MorphOS a separate strip command is needed, to avoid stripping }
  { __abox__ symbol, which is required to be present in current MorphOS }
  { executables. }
  if (cs_link_on_target in current_settings.globalswitches) then
   begin
    if success and (cs_link_strip in current_settings.globalswitches) then
     begin
      SplitBinCmd(Info.ExeCmd[2],binstr,cmdstr);
      Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
      success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);
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
  RegisterExternalLinker(system_powerpc_morphos_info,TLinkerMorphOS);
  RegisterTarget(system_powerpc_morphos_info);
end.

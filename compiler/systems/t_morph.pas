{
    $Id$
    Copyright (c) 2004 by Free Pascal Development Team

    This unit implements support import,export,link routines
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
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_morph;

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
     if (cs_link_on_target in aktglobalswitches) then begin
        ExeCmd[1]:='ld $OPT -o $EXE --script $RES';
        ExeCmd[2]:='strip --strip-unneeded --remove-section .comment $EXE';
     end else
        ExeCmd[1]:='ld $OPT $STRIP -o $EXE $RES'
   end;
end;


Function TLinkerMorphOS.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TStringListItem;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     s:=HPath.Str;
     if not (cs_link_on_target in aktglobalswitches) then
       s:=GetShortName(s)
     else
       s:=ScriptFixFileName(s);
     LinkRes.Add('-L'+s);
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     s:=HPath.Str;
     if not (cs_link_on_target in aktglobalswitches) then
       s:=GetShortName(s);
     if s<>'' then
       LinkRes.Add('SEARCH_DIR('+s+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  s:=FindObjectFile('prt0','',false);
  if not (cs_link_on_target in aktglobalswitches) then
    s:=GetShortName(s);
  LinkRes.AddFileName(s);
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if not (cs_link_on_target in aktglobalswitches) then
       s:=GetShortName(s);
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
        if not (cs_link_on_target in aktglobalswitches) then
          s:=GetShortName(s);
        LinkRes.AddFileName(s);
      end;
     LinkRes.Add(')');
   end;

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

{ Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;

end;


function TLinkerMorphOS.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  StripStr : string[40];
begin

  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StripStr:='';
  if (cs_link_strip in aktglobalswitches) then
    StripStr:='-s';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  if pos(' ',current_module.exefilename^)>0 then
    Replace(cmdstr,'$EXE','"'+ScriptFixFileName(current_module.exefilename^)+'"')
  else
    Replace(cmdstr,'$EXE',ScriptFixFileName(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',ScriptFixFileName(outputexedir+Info.ResName));
  Replace(cmdstr,'$STRIP',StripStr);
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Stripping Enabled? }
  { Under MorphOS a separate strip command is needed, to avoid stripping } 
  { __abox__ symbol, which is required to be present in current MorphOS }
  { executables. }
  if success and (cs_link_strip in aktglobalswitches) then
    begin
      SplitBinCmd(Info.ExeCmd[2],binstr,cmdstr);
      Replace(cmdstr,'$EXE',current_module.exefilename^);
      success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,true,false);
    end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }

end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_powerpc_morphos_info,TLinkerMorphOS);
  RegisterTarget(system_powerpc_morphos_info);
end.
{
  $Log$
  Revision 1.5  2004-06-07 23:44:37  karoly
    + fixed stripping support

  Revision 1.4  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.3  2004/04/09 01:32:46  karoly
   * disable stripping in mos linking scripts.

  Revision 1.2  2004/04/08 17:11:02  karoly
   * added external linker support based on 1.0 amiga support

  Revision 1.1  2004/02/13 05:46:58  karoly
   * added powerpc-morphos target


}

{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Amiga target

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
       constructor Create;override;
       procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
    end;


implementation

    uses
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_palmos;

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
     ExeCmd[1]:='ldpalm $OPT $STRIP -N -dy -T $SCRIPT -o $EXE @$RES';
     ExeCmd[2]:='build-prc $EXE.prc "$APPNAME" $APPID $EXE *.bin';
   end;
end;


Function TLinkerPalmOS.WriteResponseFile : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : PStringQueueItem;
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
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;

  { add objectfiles, start with crt0 always  }
  { using crt0, we should stick C compatible }
  LinkRes.AddFileName(FindObjectFile('crt0',''));

  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('-(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(s)
      end;
     LinkRes.Add('-)');
   end;

  { currently the PalmOS target must be linked always against the C lib }
  LinkRes.Add('-lcrt');

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
  cmdstr  : string;
  success : boolean;
  StripStr : string[40];
  i : longint;
begin
  if not(cs_link_extern in aktglobalswitches) then
    Message1(exec_i_linking,current_module^.exefilename^);

  { Create some replacements }
  StripStr:='';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

  { Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  success:=false;
  for i:=1 to 2 do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',MaybeQuote(current_module.exefilename^));
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$STRIP',StripStr);
        Replace(cmdstr,'$SCRIPT',FindUtil('palm.ld'));
        Replace(cmdstr,'$APPNAME',palmos_applicationname);
        Replace(cmdstr,'$APPID',palmos_applicationid);
        success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false);
        if not success then
         break;
      end;
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
{$ifdef m68k}
  RegisterTarget(target_m68k_palmos_info);
{$endif m68k}
end.
{
  $Log$
  Revision 1.1  2002-09-06 15:03:50  carl
    * moved files to systems directory

  Revision 1.14  2002/07/26 21:15:46  florian
    * rewrote the system handling

  Revision 1.13  2002/07/01 18:46:35  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.12  2002/05/18 13:34:27  peter
    * readded missing revisions

  Revision 1.11  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.9  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.8  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.7  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

}

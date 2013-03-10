{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i8086) MS-DOS target

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
unit t_msdos;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,
       fmodule,i_msdos,
       link,aasmbase;

    type
      TExternalLinkerMsDosTLink=class(texternallinker)
      private
         Function  WriteResponseFile(isdll:boolean) : Boolean;
      public
         constructor Create;override;
         procedure SetDefaultInfo;override;
         function  MakeExecutable:boolean;override;
      end;


{****************************************************************************
                               TExternalLinkerMsDosTLink
****************************************************************************}

Constructor TExternalLinkerMsDosTLink.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TExternalLinkerMsDosTLink.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='tlink $RES';
   end;
end;


Function TExternalLinkerMsDosTLink.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  s        : string;
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

  { Add all options to link.res instead of passing them via command line:
    DOS command line is limited to 126 characters! }

  { add objectfiles, start with prt0 always }
  LinkRes.Add(GetShortName(FindObjectFile('prt0','',false)) + ' +');
  while not ObjectFiles.Empty do
  begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
      LinkRes.Add(GetShortName(s) + ' +');
  end;
  LinkRes.Add(', ' + maybequoted(current_module.exefilename));

  { Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;


function TExternalLinkerMsDosTLink.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Write used files and libraries and our own tlink script }
  WriteResponsefile(false);

  { Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$RES','@'+maybequoted(outputexedir+Info.ResName));
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_i8086_msdos_info,TExternalLinkerMsDosTLink);
  RegisterTarget(system_i8086_msdos_info);
end.

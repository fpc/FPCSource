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

{$define USE_LINKER_WLINK}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,
       fmodule,i_msdos,
       link,aasmbase;

    type
      { Borland TLINK support }
      TExternalLinkerMsDosTLink=class(texternallinker)
      private
         Function  WriteResponseFile(isdll:boolean) : Boolean;
      public
         constructor Create;override;
         procedure SetDefaultInfo;override;
         function  MakeExecutable:boolean;override;
      end;

      { the ALINK linker from http://alink.sourceforge.net/ }
      TExternalLinkerMsDosALink=class(texternallinker)
      private
         Function  WriteResponseFile(isdll:boolean) : Boolean;
      public
         constructor Create;override;
         procedure SetDefaultInfo;override;
         function  MakeExecutable:boolean;override;
      end;

      { the (Open) Watcom linker }
      TExternalLinkerMsDosWLink=class(texternallinker)
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
     ExeCmd[1]:='tlink $OPT $RES';
   end;
end;


Function TExternalLinkerMsDosTLink.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  s        : string;
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
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;

{****************************************************************************
                               TExternalLinkerMsDosALink
****************************************************************************}

{ TExternalLinkerMsDosALink }

function TExternalLinkerMsDosALink.WriteResponseFile(isdll: boolean): Boolean;
Var
  linkres  : TLinkRes;
  s        : string;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

  { Add all options to link.res instead of passing them via command line:
    DOS command line is limited to 126 characters! }

  { add objectfiles, start with prt0 always }
  LinkRes.Add(maybequoted(FindObjectFile('prt0','',false)));
  while not ObjectFiles.Empty do
  begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
      LinkRes.Add(maybequoted(s));
  end;
  LinkRes.Add('-oEXE');
  LinkRes.Add('-o ' + maybequoted(current_module.exefilename));

  { Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;

constructor TExternalLinkerMsDosALink.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;

procedure TExternalLinkerMsDosALink.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='alink $OPT $RES';
   end;
end;

function TExternalLinkerMsDosALink.MakeExecutable: boolean;
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
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;



{****************************************************************************
                               TExternalLinkerMsDosWLink
****************************************************************************}

{ TExternalLinkerMsDosWLink }

function TExternalLinkerMsDosWLink.WriteResponseFile(isdll: boolean): Boolean;
Var
  linkres  : TLinkRes;
  s        : string;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

  { Add all options to link.res instead of passing them via command line:
    DOS command line is limited to 126 characters! }

  LinkRes.Add('option quiet');

  if paratargetdbg in [dbg_dwarf2,dbg_dwarf3,dbg_dwarf4] then
    LinkRes.Add('debug dwarf');

  { add objectfiles, start with prt0 always }
  case current_settings.x86memorymodel of
    mm_tiny:    LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0t','',false)));
    mm_small:   LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0s','',false)));
    mm_medium:  LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0m','',false)));
    mm_compact: LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0c','',false)));
    mm_large:   LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0l','',false)));
    mm_huge:    LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0h','',false)));
  end;
  while not ObjectFiles.Empty do
  begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
      LinkRes.Add('file ' + maybequoted(s));
  end;
  while not StaticLibFiles.Empty do
  begin
    s:=StaticLibFiles.GetFirst;
    if s<>'' then
      LinkRes.Add('library '+MaybeQuoted(s));
  end;
  if apptype=app_com then
    LinkRes.Add('format dos com')
  else
    LinkRes.Add('format dos');
  LinkRes.Add('option dosseg');
  if (cs_link_map in current_settings.globalswitches) then
    LinkRes.Add('option map='+maybequoted(ChangeFileExt(current_module.exefilename,'.map')));
  LinkRes.Add('name ' + maybequoted(current_module.exefilename));

  { Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;

constructor TExternalLinkerMsDosWLink.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;

procedure TExternalLinkerMsDosWLink.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='wlink $OPT $RES';
   end;
end;

function TExternalLinkerMsDosWLink.MakeExecutable: boolean;
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
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
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
{$if defined(USE_LINKER_TLINK)}
  RegisterLinker(ld_msdos,TExternalLinkerMsDosTLink);
{$elseif defined(USE_LINKER_ALINK)}
  RegisterLinker(ld_msdos,TExternalLinkerMsDosALink);
{$elseif defined(USE_LINKER_WLINK)}
  RegisterLinker(ld_msdos,TExternalLinkerMsDosWLink);
{$else}
  {$fatal no linker defined}
{$endif}
  RegisterTarget(system_i8086_msdos_info);
end.

{
    Copyright (c) 2003 by Wiktor Sywula

    This unit implements support import, export, link routines
    for the (i386) Watcom target

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
unit t_watcom;

{$i fpcdefs.inc}

interface


implementation

    uses
       link,
       SysUtils,
       cclasses,cutils,cfileutl,globtype,globals,
       systems,verbose,script,fmodule,i_watcom;


  type
    tlinkerwatcom=class(texternallinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
       constructor Create;override;
       procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
{       function  MakeSharedLibrary:boolean;override;}
    end;


{****************************************************************************
                               TLinkerWatcom
****************************************************************************}

Constructor TLinkerWatcom.Create;
begin
  Inherited Create;
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerWatcom.SetDefaultInfo;
begin
  with Info do
     ExeCmd[1]:='wlink system causeway option quiet option nocaseexact $OPT $STRIP name $EXE @$RES';
end;

Function TLinkerWatcom.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  {i        : longint;}
  s        : string;
  {linklibc : boolean;}
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write object files, start with prt0 }
  LinkRes.Add('file '+GetShortName(FindObjectFile('prt0','',false)));
  if not ObjectFiles.Empty then
     While not ObjectFiles.Empty do
      begin
        S:=ObjectFiles.GetFirst;
        LinkRes.AddFileName('file '+GetShortName(s));
      end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName('file '+GetShortName(s));
      end;

(*

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  linklibc:=false;
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.Get;
     if s<>'c' then
      begin
        i:=Pos(target_os.sharedlibext,S);
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
*)
{ Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;
end;


function TLinkerWatcom.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StripStr : string[40];
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  StripStr:='debug dwarf all';
  if (cs_link_strip in current_settings.globalswitches) then
   StripStr:='';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$STRIP',StripStr);
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;

{function TLinkerWatcom.MakeSharedLibrary:boolean;
begin
  MakeSharedLibrary:=false;
end;}

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_i386_watcom_info,TLinkerWatcom);
  RegisterTarget(system_i386_watcom_info);
end.

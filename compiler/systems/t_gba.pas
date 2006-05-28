{
    This unit implements support import,export,link routines
    for the (arm) GameBoy Advance target
    
    Copyright (c) 2001-2002 by Peter Vreman

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
unit t_gba;

{$i fpcdefs.inc}

interface


implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_gba;

    type
       TlinkerGBA=class(texternallinker)
       private
          Function  WriteResponseFile: Boolean;
       public
          constructor Create; override;
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
       end;



{*****************************************************************************
                                  TLINKERGBA
*****************************************************************************}

Constructor TLinkerGba.Create;
begin
  Inherited Create;
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkerGba.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE $RES';
   end;
end;


Function TLinkerGba.WriteResponseFile: Boolean;
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
    if (cs_link_on_target in aktglobalswitches) then
     s:=ScriptFixFileName(s);
    LinkRes.Add('-L'+s);
    HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
    s:=HPath.Str;
    if s<>'' then
     LinkRes.Add('SEARCH_DIR('+(maybequoted(s))+')');
    HPath:=TStringListItem(HPath.Next);
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
      if not(cs_link_on_target in aktglobalswitches) then
       s:=FindObjectFile(s,'',false);
      LinkRes.AddFileName((maybequoted(s)));
     end;
   end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
    { vlink doesn't need, and doesn't support GROUP }
    if (cs_link_on_target in aktglobalswitches) then 
     begin
      LinkRes.Add(')');
      LinkRes.Add('GROUP(');
     end;
    while not StaticLibFiles.Empty do
     begin
      S:=StaticLibFiles.GetFirst;
      LinkRes.AddFileName((maybequoted(s)));
     end;
   end;
  
  if (cs_link_on_target in aktglobalswitches) then 
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


function TLinkerGba.MakeExecutable:boolean;
var
  binstr  : string;
  cmdstr  : TCmdStr;
  success : boolean;
  StripStr: string[40];
begin
  //if not(cs_link_extern in aktglobalswitches) then
  if not(cs_link_nolink in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Write used files and libraries }
  WriteResponseFile();

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  if not(cs_link_on_target in aktglobalswitches) then
   begin
    Replace(cmdstr,'$EXE',(maybequoted(ScriptFixFileName(current_module.exefilename^))));
    Replace(cmdstr,'$RES',(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
    Replace(cmdstr,'$STRIP',StripStr);
   end
  else
   begin
    Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(current_module.exefilename^)));
    Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
   end;
  success:=DoExec(FindUtil(BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_nolink in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }


end;



{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_arm_gba_info,TLinkerGba);
  RegisterTarget(system_arm_gba_info);
end.

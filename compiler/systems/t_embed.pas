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
unit t_embed;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_embed,link,
       cpuinfo;

    type
       TlinkerEmbedded=class(texternallinker)
       private
          Function  WriteResponseFile: Boolean;
       public
          constructor Create; override;
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
       end;



{*****************************************************************************
                                  TlinkerEmbedded
*****************************************************************************}

Constructor TlinkerEmbedded.Create;
begin
  Inherited Create;
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TlinkerEmbedded.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld -g $OPT $DYNLINK $STATIC $GCSECTIONS $STRIP -L. -o $EXE -T $RES';
   end;
end;


Function TlinkerEmbedded.WriteResponseFile: Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TCmdStrListItem;
  s,s1,s2  : TCmdStr;
  prtobj,
  cprtobj  : string[80];
  linklibc : boolean;
  found1,
  found2   : boolean;
begin
  WriteResponseFile:=False;
  linklibc:=(SharedLibFiles.Find('c')<>nil);
{$ifdef ARM}
  prtobj:='';
{$else ARM}
  prtobj:='prt0';
{$endif ARM}
  cprtobj:='cprt0';
  if linklibc then
    prtobj:=cprtobj;

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
     LinkRes.Add('SEARCH_DIR('+(maybequoted(s))+')');
    HPath:=TCmdStrListItem(HPath.Next);
   end;

  LinkRes.Add('INPUT (');
  { add objectfiles, start with prt0 always }
  //s:=FindObjectFile('prt0','',false);
  if prtobj<>'' then
    begin
      s:=FindObjectFile(prtobj,'',false);
      LinkRes.AddFileName(s);
    end;
  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     if librarysearchpath.FindFile('crtbegin.o',false,s) then
      LinkRes.AddFileName(s);
     if librarysearchpath.FindFile('crti.o',false,s) then
      LinkRes.AddFileName(s);
   end;

  while not ObjectFiles.Empty do
   begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
     begin
      { vlink doesn't use SEARCH_DIR for object files }
      if not(cs_link_on_target in current_settings.globalswitches) then
       s:=FindObjectFile(s,'',false);
      LinkRes.AddFileName((maybequoted(s)));
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
      LinkRes.AddFileName((maybequoted(s)));
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

  { objects which must be at the end }
  if linklibc then
   begin
     found1:=librarysearchpath.FindFile('crtend.o',false,s1);
     found2:=librarysearchpath.FindFile('crtn.o',false,s2);
     if found1 or found2 then
      begin
        LinkRes.Add('INPUT(');
        if found1 then
         LinkRes.AddFileName(s1);
        if found2 then
         LinkRes.AddFileName(s2);
        LinkRes.Add(')');
      end;
   end;

{$ifdef ARM}
  case current_settings.controllertype of
    ct_none:
      ;
    ct_lpc2114,
    ct_lpc2124,
    ct_lpc2194:
      with linkres do
        begin
          Add('ENTRY(_START)');
          Add('MEMORY');
          Add('{');
          Add('    flash : ORIGIN = 0, LENGTH = 256K');
          Add('    ram : ORIGIN = 0x40000000, LENGTH = 16K');
          Add('}');
        end;
      ct_at91sam7s256,
      ct_at91sam7se256,
      ct_at91sam7x256,
      ct_at91sam7xc256:
      with linkres do
        begin
          Add('ENTRY(_START)');
          Add('MEMORY');
          Add('{');
          Add('    flash : ORIGIN = 0, LENGTH = 256K');
          Add('    ram : ORIGIN = 0x200000, LENGTH = 64K');
          Add('}');
          Add('_stack_end = 0x20FFFC;');
        end;

    else
      internalerror(200902011);
  end;

  with linkres do
    begin
      Add('SECTIONS');
      Add('{');
      Add('     .text :');
      Add('    {');
      Add('    *(.init, .init.*)');
      Add('    *(.text, .text.*)');
      Add('    *(.strings)');
      Add('    *(.rodata, .rodata.*)');
      Add('    *(.comment)');
      Add('    _etext = .;');
      Add('    } >flash');
      Add('    .data :');
      Add('    {');
      Add('    _data = .;');
      Add('    *(.data, .data.*)');
      Add('    KEEP (*(.fpc .fpc.n_version .fpc.n_links))');
      Add('    _edata = .;');
      Add('    } >ram AT >flash');
      Add('    .bss :');
      Add('    {');
      Add('    _bss_start = .;');
      Add('    *(.bss, .bss.*)');
      Add('    *(COMMON)');
      Add('    } >ram');
      Add('. = ALIGN(4);');
      Add('_bss_end = . ;');
      Add('}');
      Add('_end = .;');
    end;
{$endif ARM}

  { Write and Close response }
  linkres.writetodisk;
  linkres.free;

  WriteResponseFile:=True;

end;


function TlinkerEmbedded.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
  StaticStr,
  GCSectionsStr,
  DynLinkStr,
  StripStr: string;
begin
  { for future use }
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';

  GCSectionsStr:='--gc-sections';
  //if not(cs_link_extern in current_settings.globalswitches) then
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Write used files and libraries }
  WriteResponseFile();

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  if not(cs_link_on_target in current_settings.globalswitches) then
   begin
    Replace(cmdstr,'$EXE',(maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename^,'.elf')))));
    Replace(cmdstr,'$RES',(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
    Replace(cmdstr,'$STATIC',StaticStr);
    Replace(cmdstr,'$STRIP',StripStr);
    Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
    Replace(cmdstr,'$DYNLINK',DynLinkStr);
   end
  else
   begin
    Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename^,'.elf'))));
    Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
    Replace(cmdstr,'$STATIC',StaticStr);
    Replace(cmdstr,'$STRIP',StripStr);
    Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
    Replace(cmdstr,'$DYNLINK',DynLinkStr);
   end;
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

{ Remove ReponseFile }
  if success and not(cs_link_nolink in current_settings.globalswitches) then
   DeleteFile(outputexedir+Info.ResName);

{ Post process }
  if success then
    begin
      success:=DoExec(FindUtil(utilsprefix+'objcopy'),'-O ihex '+
        ChangeFileExt(current_module.exefilename^,'.elf')+' '+
        ChangeFileExt(current_module.exefilename^,'.hex'),true,false);
    end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef arm}
  RegisterExternalLinker(system_arm_embedded_info,TlinkerEmbedded);
  RegisterTarget(system_arm_embedded_info);
{$endif arm}

{$ifdef avr}
  RegisterExternalLinker(system_avr_embedded_info,TlinkerEmbedded);
  RegisterTarget(system_avr_embedded_info);
{$endif avr}
end.

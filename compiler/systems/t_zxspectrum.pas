{
    Copyright (c) 2005-2020 by Free Pascal Compiler team

    This unit implements support import, export, link routines
    for the ZX Spectrum Target

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
unit t_zxspectrum;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,comphook,cscript,fmodule,i_zxspectrum,link,
       cpuinfo;

    type

       { TLinkerZXSpectrum_SdccSdld - the sdld linker from the SDCC project ( http://sdcc.sourceforge.net/ ) }

       TLinkerZXSpectrum_SdccSdld=class(texternallinker)
       private
          FOrigin: Word;
          Function  WriteResponseFile: Boolean;
       public
          procedure SetDefaultInfo; override;
          function  MakeExecutable:boolean; override;
          function postprocessexecutable(const fn : string;isdll:boolean):boolean;
       end;


{*****************************************************************************
                          TLinkerZXSpectrum_SdccSdld
*****************************************************************************}

function TLinkerZXSpectrum_SdccSdld.WriteResponseFile: Boolean;
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
  {$if defined(ARM)}
    LinkStr  : string;
  {$endif}
  begin
    WriteResponseFile:=False;
    linklibc:=(SharedLibFiles.Find('c')<>nil);
    prtobj:='prt0';
    cprtobj:='cprt0';
    if linklibc then
      prtobj:=cprtobj;

    { Open link.res file }
    LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

    { Write the origin (i.e. the program load address) }
    LinkRes.Add('-b _CODE='+tostr(FOrigin));

    { Write path to search libraries }
(*    HPath:=TCmdStrListItem(current_module.locallibrarysearchpath.First);
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
    { add objectfiles, start with prt0 always }*)
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
{      if (cs_link_on_target in current_settings.globalswitches) then
       begin
        LinkRes.Add(')');
        LinkRes.Add('GROUP(');
       end;}
      while not StaticLibFiles.Empty do
       begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.Add('-l'+maybequoted(s));
       end;
     end;

(*    if (cs_link_on_target in current_settings.globalswitches) then
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
     end;*)

    { objects which must be at the end }
    (*if linklibc then
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
     end;*)

    { Write and Close response }
    linkres.writetodisk;
    linkres.free;

    WriteResponseFile:=True;
  end;

procedure TLinkerZXSpectrum_SdccSdld.SetDefaultInfo;
  const
    ExeName='sdldz80';
  begin
    FOrigin:={32768}23800;
    with Info do
     begin
       ExeCmd[1]:=ExeName+' -n $OPT -i $MAP $EXE -f $RES'
     end;
  end;

function TLinkerZXSpectrum_SdccSdld.MakeExecutable: boolean;
  var
    binstr,
    cmdstr,
    mapstr: TCmdStr;
    success : boolean;
    StaticStr,
    //GCSectionsStr,
    DynLinkStr,
    StripStr,
    FixedExeFileName: string;
  begin
    { for future use }
    StaticStr:='';
    StripStr:='';
    mapstr:='';
    DynLinkStr:='';
    FixedExeFileName:=maybequoted(ScriptFixFileName(ChangeFileExt(current_module.exefilename,'.ihx')));

(*    GCSectionsStr:='--gc-sections';
    //if not(cs_link_extern in current_settings.globalswitches) then
    if not(cs_link_nolink in current_settings.globalswitches) then
     Message1(exec_i_linking,current_module.exefilename);*)

    if (cs_link_map in current_settings.globalswitches) then
     mapstr:='-mw';

  { Write used files and libraries }
    WriteResponseFile();

  { Call linker }
    SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
    Replace(cmdstr,'$OPT',Info.ExtraOptions);
    if not(cs_link_on_target in current_settings.globalswitches) then
     begin
      Replace(cmdstr,'$EXE',FixedExeFileName);
      Replace(cmdstr,'$RES',(maybequoted(ScriptFixFileName(outputexedir+Info.ResName))));
      Replace(cmdstr,'$STATIC',StaticStr);
      Replace(cmdstr,'$STRIP',StripStr);
      Replace(cmdstr,'$MAP',mapstr);
      //Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
      Replace(cmdstr,'$DYNLINK',DynLinkStr);
     end
    else
     begin
      Replace(cmdstr,'$EXE',FixedExeFileName);
      Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
      Replace(cmdstr,'$STATIC',StaticStr);
      Replace(cmdstr,'$STRIP',StripStr);
      Replace(cmdstr,'$MAP',mapstr);
      //Replace(cmdstr,'$GCSECTIONS',GCSectionsStr);
      Replace(cmdstr,'$DYNLINK',DynLinkStr);
     end;
    success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

  { Remove ReponseFile }
    if success and not(cs_link_nolink in current_settings.globalswitches) then
     DeleteFile(outputexedir+Info.ResName);

  { Post process }
    if success and not(cs_link_nolink in current_settings.globalswitches) then
      success:=PostProcessExecutable(FixedExeFileName,false);

    MakeExecutable:=success;   { otherwise a recursive call to link method }
  end;

function TLinkerZXSpectrum_SdccSdld.postprocessexecutable(const fn: string; isdll: boolean): boolean;
  begin
    result:=DoExec(FindUtil(utilsprefix+'ihx2tzx'),' '+fn,true,false);
  end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef z80}
  RegisterLinker(ld_zxspectrum,TLinkerZXSpectrum_SdccSdld);
  RegisterTarget(system_z80_zxspectrum_info);
{$endif z80}
end.

{
    $Id$
    Copyright (c) 2001 by Peter Vreman

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

{$i defines.inc}

interface

  uses
    link;

  type
    tlinkerPalmOS=class(tlinker)
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
       globtype,globals,systems,verbose,script,fmodule;

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

{$ifdef m68k}
    const
       target_m68k_palmos_info : ttargetinfo =
          (
            target       : target_m68k_PalmOS;
            name         : 'PalmOS';
            shortname    : 'palmos';
            flags        : [tf_code_small,tf_static_a5_based];
            cpu          : m68k;
            short_name   : 'PALMOS';
            unit_env     : 'PALMUNITS';
            extradefines : '';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            staticlibprefix : 'libp';
            sharedlibprefix : 'lib';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_m68k_as;
            assemextern  : as_m68k_as;
            link         : ld_m68k_palmos;
            linkextern   : ld_m68k_palmos;
            ar           : ar_m68k_ar;
            res          : res_none;
            script       : script_unix;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 4;
            size_of_longint : 4;
            heapsize     : 128*1024;
            maxheapsize  : 32768*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          );
{$endif m68k}

initialization
{$ifdef m68k}
  RegisterTarget(target_m68k_palmos_info);
{$endif m68k}
end.
{
  $Log$
  Revision 1.7  2002-04-15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.6  2001/09/17 21:29:16  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.5  2001/08/19 11:22:24  peter
    * palmos support from v10 merged

  Revision 1.4  2001/08/07 18:47:15  peter
    * merged netbsd start
    * profile for win32

  Revision 1.3  2001/06/03 15:15:32  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.2  2001/06/02 19:22:44  peter
    * extradefines field added

  Revision 1.1  2001/04/18 22:02:04  peter
    * registration of targets and assemblers

}

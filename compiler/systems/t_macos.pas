{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

    This unit implements support import,export,link routines for MacOS.

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
unit t_macos;

{$i fpcdefs.inc}

interface

  uses
     import,symsym,symdef,link;

  type
    timportlibmacos=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tglobalvarsym;const name,module:string);override;
      procedure generatelib;override;
    end;

    tlinkermpw=class(texternallinker)
    private
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
    end;

implementation

    uses
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule,i_macos,
       symconst;

{*****************************************************************************
                               TIMPORTLIBMACOS
*****************************************************************************}

procedure timportlibmacos.preparelib(const s : string);
begin
end;


procedure timportlibmacos.importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
end;


procedure timportlibmacos.importvariable(vs:tglobalvarsym;const name,module:string);
begin
  { insert sharedlibrary }
  current_module.linkothersharedlibs.add(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  vs.set_mangledname(name);
  exclude(vs.varoptions,vo_is_dll_var);
end;


procedure timportlibmacos.generatelib;
begin
end;

{*****************************************************************************
                                  TLINKERMPW
*****************************************************************************}

Constructor TLinkerMPW.Create;
begin
  Inherited Create;
  //LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true);
end;


procedure TLinkerMPW.SetDefaultInfo;

begin
  with Info do
   begin
     ExeCmd[1]:='Execute $RES'; {The link.res file contains the whole link command.}
     //ExeCmd[1]:='PPCLink $OPT $DYNLINK $STATIC $STRIP -tocdataref off -dead on -o $EXE -@filelist $RES';
     //DllCmd[1]:='PPCLink $OPT $INIT $FINI $SONAME -shared -o $EXE -@filelist $RES';
   end;
end;


Function TLinkerMPW.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  s: string;

begin
  WriteResponseFile:=False;
  { Open link.res file }
  linkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  with linkRes do
    begin
      {#182 is escape char in MPW (analog to backslash in unix). The space}
      {ensures there is whitespace separating items.}
      Add('PPCLink '#182);

      { Add MPW standard libraries}
      if apptype = app_cui then
          Add('"{PPCLibraries}PPCSIOW.o" '#182);

      if (apptype = app_tool) or (apptype = app_cui) then
          Add('"{PPCLibraries}PPCToolLibs.o" '#182);

      Add('"{SharedLibraries}InterfaceLib" '#182);
      Add('"{SharedLibraries}StdCLib" '#182);
      Add('"{SharedLibraries}MathLib" '#182);
      Add('"{PPCLibraries}StdCRuntime.o" '#182);
      Add('"{PPCLibraries}PPCCRuntime.o" '#182);

      {Add main objectfiles}
      while not ObjectFiles.Empty do
        begin
          s:=ObjectFiles.GetFirst;
          if s<>'' then
            Add(s+' '#182);
        end;

      {Add last lines of the link command}
      if apptype = app_tool then
        Add('-t "MPST" -c "MPS " '#182);

      if apptype = app_cui then {If SIOW, to avoid some warnings.}
        Add('-ignoredups __start -ignoredups .__start -ignoredups main -ignoredups .main -ignoredups qd '#182);

      Add('-tocdataref off -sym on -dead on -o '+ ScriptFixFileName(current_module.exefilename^));

      Add('Exit If "{Status}" != 0');

      {Add mac resources}
      if apptype = app_cui then
        begin
          Add('Rez -append "{RIncludes}"SIOW.r -o ' + ScriptFixFileName(current_module.exefilename^));
          Add('Exit If "{Status}" != 0');
        end;
    end;

  { Write and Close response }
  linkres.writetodisk;
  linkres.Free;

  WriteResponseFile:=True;
end;


function TLinkerMPW.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  DynLinkStr : string[60];
  StaticStr,
  StripStr   : string[40];

  s: string;

begin
  //TODO Only external link in MPW is possible, otherwise yell.

  if not(cs_link_extern in aktglobalswitches) then
    Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
(*
  StaticStr:='';
  StripStr:='';
  DynLinkStr:='';
  if (cs_link_staticflag in aktglobalswitches) then
   StaticStr:='-static';
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';
  If (cs_profile in aktmoduleswitches) or
     ((Info.DynamicLinker<>'') and (not SharedLibFiles.Empty)) then
   DynLinkStr:='-dynamic-linker='+Info.DynamicLinker;
*)

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(ScriptFixFileName(current_module.exefilename^)));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',maybequoted(ScriptFixFileName(outputexedir+Info.ResName)));
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);

  with AsmRes do
    begin
      WriteResponseFile(false);
      success:= true;

      if cs_link_on_target in aktglobalswitches then
        success:=DoExec('SetFile', ' -c ''MPS '' -t ''TEXT'' ' +
                     ScriptFixFileName(outputexedir+Info.ResName),true,false);

      if success then
        success:=DoExec('Execute',CmdStr,true,false);
    end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;



{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
{$ifdef m68k}
  RegisterTarget(system_m68k_macos_info);
  RegisterImport(system_m68k_macos,timportlibmacos);
{$endif m68k}
{$ifdef powerpc}
  RegisterExternalLinker(system_powerpc_macos_info,TLinkerMPW);
  RegisterTarget(system_powerpc_macos_info);
  RegisterImport(system_powerpc_macos,timportlibmacos);
{$endif powerpc}
end.
{
  $Log$
  Revision 1.17  2004-12-28 22:00:15  olle
    + suppression of link varning of 'qd'

  Revision 1.16  2004/12/22 16:32:46  peter
    * maybequoted() added

  Revision 1.15  2004/11/19 16:30:24  peter
    * fixed setting of mangledname when importing

  Revision 1.14  2004/11/11 19:31:33  peter
    * fixed compile of powerpc,sparc,arm

  Revision 1.13  2004/10/25 15:38:41  peter
    * heap and heapsize removed
    * checkpointer fixes

  Revision 1.12  2004/09/13 16:13:04  olle
    + When link on target, the script sets file type on link.res

  Revision 1.11  2004/08/20 10:30:00  olle
    + made fpc work as an MPW tool, by itself calling asm and link.

  Revision 1.10  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.9  2004/05/11 18:24:39  olle
    + added GUI apptype to MacOS

  Revision 1.8  2004/04/06 22:44:22  olle
    + Status checks in scripts
    + Scripts support apptype tool
    + Added some ScriptFixFileName

  Revision 1.7  2004/02/19 20:40:20  olle
    + Support for Link on target especially for MacOS
    + TLinkerMPW
    + TAsmScriptMPW

}

{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Handles the resource files handling

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
unit comprsrc;

{$i fpcdefs.inc}

interface

type
   presourcefile=^tresourcefile;
   tresourcefile=object
   private
      fname : string;
   public
      constructor Init(const fn:string);
      destructor Done;
      procedure  Compile;virtual;
   end;

procedure CompileResourceFiles;


implementation

uses
{$IFDEF USE_SYSUTILS}
  SysUtils,
{$ELSE USE_SYSUTILS}
  dos,
{$ENDIF USE_SYSUTILS}
  Systems,cutils,Globtype,Globals,Verbose,Fmodule,
  Script;

{****************************************************************************
                              TRESOURCEFILE
****************************************************************************}

constructor tresourcefile.init(const fn:string);
begin
  fname:=fn;
end;


destructor tresourcefile.done;
begin
end;


procedure tresourcefile.compile;
var
  respath,
  srcfilepath : dirstr;
  n       : namestr;
{$IFDEF USE_SYSUTILS}
{$ELSE USE_SYSUTILS}
  e       : extstr;
{$ENDIF USE_SYSUTILS}
  s,
  resobj,
  resbin   : string;
  resfound,
  objused  : boolean;
begin
  resbin:='';
  resfound:=false;
  if utilsdirectory<>'' then
   resfound:=FindFile(target_res.resbin+source_info.exeext,utilsdirectory,resbin);
  if not resfound then
   resfound:=FindExe(target_res.resbin,resbin);
  { get also the path to be searched for the windres.h }
{$IFDEF USE_SYSUTILS}
  respath := SplitPath(resbin);
{$ELSE USE_SYSUTILS}
  fsplit(resbin,respath,n,e);
{$ENDIF USE_SYSUTILS}
  if (not resfound) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message(exec_e_res_not_found);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
{$IFDEF USE_SYSUTILS}
  srcfilepath := SplitPath(current_module.mainsource^);
{$ELSE USE_SYSUTILS}
  fsplit(current_module.mainsource^,srcfilepath,n,e);
{$ENDIF USE_SYSUTILS}
  if not path_absolute(fname) then
    fname:=srcfilepath+fname;
  resobj:=ForceExtension(fname,target_info.resobjext);
  s:=target_res.rescmd;
  ObjUsed:=(pos('$OBJ',s)>0);
  Replace(s,'$OBJ',maybequoted(resobj));
  Replace(s,'$RES',maybequoted(fname));
  { windres doesn't like empty include paths }
  if respath='' then
    respath:='.';
  Replace(s,'$INC',maybequoted(respath));
  if (target_info.system = system_i386_win32) and
     (srcfilepath<>'') then
    s:=s+' --include '+maybequoted(srcfilepath);
{ Execute the command }
  if not (cs_link_extern in aktglobalswitches) then
   begin
     Message1(exec_i_compilingresource,fname);
     Message2(exec_d_resbin_params,resbin,s);
{$IFDEF USE_SYSUTILS}
     try
       if ExecuteProcess(resbin,s) <> 0 then
       begin
         Message(exec_e_error_while_linking);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
       end;
     except
       on E:EOSError do
       begin
         Message(exec_e_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
       end
     end;
{$ELSE USE_SYSUTILS}
     swapvectors;
     exec(resbin,s);
     swapvectors;
     if (doserror<>0) then
      begin
        Message(exec_e_cant_call_linker);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end
     else
      if (dosexitcode<>0) then
       begin
         Message(exec_e_error_while_linking);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
       end;
{$ENDIF USE_SYSUTILS}
    end;
  { Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
    AsmRes.AddLinkCommand(resbin,s,'');
  if ObjUsed then
    current_module.linkotherofiles.add(resobj,link_allways);
end;


procedure CompileResourceFiles;
var
  hr : presourcefile;
begin
  { OS/2 (EMX) must be processed elsewhere (in the linking/binding stage).
    same with MacOS}
  if not (target_info.system in [system_i386_os2,
                                 system_i386_emx,system_powerpc_macos]) then
   While not current_module.ResourceFiles.Empty do
    begin
      case target_info.system of
        system_m68k_palmos,
        system_i386_win32,
        system_i386_linux,
        system_i386_wdosx :
          hr:=new(presourcefile,init(current_module.ResourceFiles.getfirst));
        else
          Message(scan_e_resourcefiles_not_supported);
      end;
      hr^.compile;
      dispose(hr,done);
    end;
end;


end.

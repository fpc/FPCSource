{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
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
  respath : dirstr;
  n       : namestr;
  e       : extstr;
  s,
  resobj,
  resbin   : string;
  resfound : boolean;
begin
  resbin:='';
  if utilsdirectory<>'' then
   resfound:=FindFile(target_res.resbin+source_info.exeext,utilsdirectory,resbin);
  if not resfound then
   resfound:=FindExe(target_res.resbin,resbin);
  { get also the path to be searched for the windres.h }
  fsplit(resbin,respath,n,e);
  if (not resfound) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message(exec_e_res_not_found);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
  resobj:=ForceExtension(current_module.objfilename^,target_info.resobjext);
  s:=target_res.rescmd;
  Replace(s,'$OBJ',resobj);
  Replace(s,'$RES',fname);
  Replace(s,'$INC',respath);
{ Exec the command }
  if not (cs_link_extern in aktglobalswitches) then
   begin
     Message1(exec_i_compilingresource,fname);
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
    end;
  { Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
    AsmRes.AddLinkCommand(resbin,s,'');
  current_module.linkotherofiles.add(resobj,link_allways);
end;


procedure CompileResourceFiles;
var
  hr : presourcefile;
begin
(* OS/2 (EMX) must be processed elsewhere (in the linking/binding stage). *)
  if target_info.target <> target_i386_os2 then
   While not current_module.ResourceFiles.Empty do
    begin
      case target_info.target of
        target_i386_win32,target_i386_wdosx:
          hr:=new(presourcefile,init(current_module.ResourceFiles.getfirst));
        else
          Message(scan_e_resourcefiles_not_supported);
      end;
      hr^.compile;
      dispose(hr,done);
    end;
end;


end.
{
  $Log$
  Revision 1.13  2002-05-16 19:46:35  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.11  2002/04/04 18:32:37  carl
  + added wdosx support (patch from Pavel)

  Revision 1.10  2001/08/07 18:47:12  peter
    * merged netbsd start
    * profile for win32

  Revision 1.9  2001/04/18 22:01:53  peter
    * registration of targets and assemblers

  Revision 1.8  2001/03/13 21:01:14  peter
    * string fixes

  Revision 1.7  2001/02/20 21:41:17  peter
    * new fixfilename, findfile for unix. Look first for lowercase, then
      NormalCase and last for UPPERCASE names.

  Revision 1.6  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/09/24 15:06:14  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/08/04 22:00:51  peter
    * merges from fixes

  Revision 1.2  2000/07/13 11:32:38  michael
  + removed logs

}

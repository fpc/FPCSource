{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

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
interface

procedure CompileResourceFiles;


implementation

uses
  Dos,
  Systems,Globtype,Globals,Verbose,Files,
  Script;

procedure CompileResourceFiles;
var
  resnr : longint;
  s     : string;

  procedure CompileResource(const fn:string);
  var
    s,
    resobj,
    respath,
    resbin : string;
    resfound : boolean;
  begin
    if utilsdirectory<>'' then
      begin
         respath:=Search(target_res.resbin+source_os.exeext,
           utilsdirectory,resfound);
      end
    else
      respath:=Search(target_res.resbin+source_os.exeext,'.;'+exepath+';'+dos.getenv('PATH'),resfound);
    resbin:=respath+target_res.resbin+source_os.exeext;
    if (not resfound) and not(cs_link_extern in aktglobalswitches) then
     begin
       Message(exec_w_res_not_found);
       aktglobalswitches:=aktglobalswitches+[cs_link_extern];
     end;
    resobj:=ForceExtension(current_module^.objfilename^,target_info.resobjext);
    s:=target_res.rescmd;
    Replace(s,'$OBJ',resobj);
    Replace(s,'$RES',fn);
    Replace(s,'$INC',respath);
  { Exec the command }
    if not (cs_link_extern in aktglobalswitches) then
     begin
       Message1(exec_i_compilingresource,fn);
       swapvectors;
       exec(resbin,s);
       swapvectors;
       if (doserror<>0) then
        begin
          Message(exec_w_cant_call_linker);
          aktglobalswitches:=aktglobalswitches+[cs_link_extern];
        end
       else
        if (dosexitcode<>0) then
         begin
           Message(exec_w_error_while_linking);
           aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         end;
      end;
    { Update asmres when externmode is set }
    if cs_link_extern in aktglobalswitches then
      AsmRes.AddLinkCommand(resbin,s,'');
    current_module^.linkofiles.insert(resobj);
  end;

begin
  resnr:=0;
  While not Current_module^.ResourceFiles.Empty do
   begin
     S:=Current_module^.ResourceFiles.Get;
     CompileResource(s);
   end;
end;


end.
{
  $Log$
  Revision 1.2  1999-01-06 12:56:01  peter
    * fixed typo :(

  Revision 1.1  1999/01/06 12:39:46  peter
    * renamed resource -> comprsrc (conflicted with FV)

  Revision 1.1  1998/12/28 23:26:25  peter
    + resource file handling ($R directive) for Win32

}

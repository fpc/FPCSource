{
    $Id$
    Copyright (c) 1998 by the FPC development team

    This unit handles the linker and binder calls for programs and
    libraries

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
Unit link;

Interface

uses cobjects;

Type
    TLinker = Object
       LinkToC,                           { Should we link to the C libs? }
       Strip             : Boolean;       { Strip symbols ? }
       ObjectFiles,
       SharedLibFiles,
       StaticLibFiles    : TStringContainer;
       LibrarySearchPath,                 { Search path for libraries }
       ExeName,                           { FileName of the exe to be created }
       SharedLibName,
       StaticLibName,                     { FileName of the lib to be created }
       LinkOptions       : string;        { Additional options to the linker }
       DynamicLinker     : String[80];    { What Dynamic linker ? }
       LinkResName       : String[32];    { Name of response file }
     { Methods }
       Constructor Init;
       Destructor Done;
       Procedure SetExeName(const s:string);
       Procedure SetLibName(const s:string);
       function  FindObjectFile(s : string) : string;
       function  FindLibraryFile(s:string;const ext:string) : string;
       Procedure AddObject(const S : String);
       Procedure AddStaticLibrary(const S : String);
       Procedure AddSharedLibrary(const S : String);
       Function  FindLinker:String;      { Find linker, sets Name }
       Function  DoExec(const command,para:string;info,useshell:boolean):boolean;
       Function  WriteResponseFile:Boolean;
       Function  MakeExecutable:boolean;
       Procedure MakeStaticLibrary(const path:string;filescnt:longint);
       Procedure MakeSharedLibrary;
     end;
     PLinker=^TLinker;

Var
  Linker : TLinker;


Implementation

uses
  Script,globals,systems,verbose
{$ifdef linux}
  ,linux
{$endif}
  ,dos;

{$ifndef linux}
Procedure Shell(command:string);
{ This is already defined in the linux.ppu for linux, need for the *
  expansion under linux }
var
  comspec : string;
begin
  comspec:=getenv('COMSPEC');
  Exec(comspec,' /C '+command);
end;
{$endif}



Constructor TLinker.Init;
begin
  ObjectFiles.Init;
  SharedLibFiles.Init;
  StaticLibFiles.Init;
  ObjectFiles.Doubles:=False;
  SharedLibFiles.Doubles:=False;
  StaticLibFiles.Doubles:=False;
  LinkToC:=False;
  Strip:=false;
  LinkOptions:='';
  ExeName:='';
  SharedLibName:='';
  StaticLibName:='';
  ObjectSearchPath:='';
  LibrarySearchPath:='';
{$ifdef linux}
  DynamicLinker:='/lib/ld-linux.so.1';
{$else}
  DynamicLinker:='';
{$endif}
  LinkResName:='link.res';
end;


Destructor TLinker.Done;
begin
end;


Procedure TLinker.SetExeName(const s:string);
var
  path : dirstr;
  name : namestr;
  ext  : extstr;
begin
  FSplit(s,path,name,ext);
  ExeName:=Path+Name+target_os.ExeExt;
end;


Procedure TLinker.SetLibName(const s:string);
var
  path : dirstr;
  name : namestr;
  ext  : extstr;
begin
  FSplit(s,path,name,ext);
  SharedLibName:=Path+Name+target_os.SharedLibExt;
  StaticLibName:=Path+Name+target_os.StaticLibExt;
end;


var
  LastLDBin : string;
Function TLinker.FindLinker:string;
var
  ldfound : boolean;
begin
  if LastLDBin='' then
   begin
     LastLDBin:=FindExe(target_link.linkbin,ldfound);
     if (not ldfound) and (not externlink) then
      begin
        Message1(exec_w_linker_not_found,LastLDBin);
        externlink:=true;
      end;
     if ldfound then
      Message1(exec_u_using_linker,LastLDBin);
   end;
  FindLinker:=LastLDBin;
end;


{ searches an object file }
function TLinker.FindObjectFile(s:string) : string;
var
  found : boolean;
begin
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  s:=FixFileName(s);
  if FileExists(s) then
   begin
     Findobjectfile:=s;
     exit;
   end;
  findobjectfile:=search(s,'.;'+unitsearchpath+';'+exepath,found)+s;
  if (not externlink) and (not found) then
   Message1(exec_w_objfile_not_found,s);
end;


{ searches an library file }
function TLinker.FindLibraryFile(s:string;const ext:string) : string;
var
  found : boolean;
begin
  if pos('.',s)=0 then
   s:=s+ext;
//  s:=FixFileName(s);
  if FileExists(s) then
   begin
     FindLibraryFile:=s;
     exit;
   end;
  findlibraryfile:=search(s,'.;'+librarysearchpath+';'+exepath,found)+s;
  if (not externlink) and (not found) then
   Message1(exec_w_libfile_not_found,s);
end;


Procedure TLinker.AddObject(const S : String);
begin
  ObjectFiles.Insert(FindObjectFile(s));
end;


Procedure TLinker.AddSharedLibrary(const S:String);
begin
  SharedLibFiles.Insert (S);
{ SharedLibFiles.Insert(FindLibraryFile(s,target_os.sharedlibext)); }
end;


Procedure TLinker.AddStaticLibrary(const S:String);
begin
  StaticLibFiles.Insert(FindLibraryFile(s,target_os.staticlibext));
end;


Function TLinker.DoExec(const command,para:string;info,useshell:boolean):boolean;
begin
  DoExec:=true;
  if not externlink then
   begin
     swapvectors;
     if useshell then
      shell(command+' '+para)
     else
      exec(command,para);
     swapvectors;
     if (dosexitcode<>0) then
      begin
        Message(exec_w_error_while_linking);
        DoExec:=false;
        exit;
      end
     else
      if (dosError<>0) then
       begin
         Message(exec_w_cant_call_linker);
         ExternLink:=true;
       end;
   end;
  if externlink then
   begin
     if info then
      AsmRes.AddLinkCommand(Command,Para,ExeName)
     else
      AsmRes.AddLinkCommand(Command,Para,'');
   end;
end;


Function TLinker.WriteResponseFile : Boolean;
Var
  LinkResponse : Text;
  i            : longint;
  prtobj,s     : string;
begin
{ Open linkresponse and write header }
  assign(linkresponse,inputdir+LinkResName);
  rewrite(linkresponse);

{ set special options for some targets }
  prtobj:='prt0';
  case target_info.target of
   target_Win32 : prtobj:='';
   target_linux : begin
                    if cs_profile in aktswitches then
                     begin
                       prtobj:='gprt0';
                       AddSharedLibrary('gmon');
                       AddSharedLibrary('c');
                     end;
                  end;
  end;

  writeln(linkresponse,target_link.inputstart);

{ add objectfiles, start with prt0 always }
  if prtobj<>'' then
   Writeln(linkresponse,FindObjectFile(prtobj));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      Writeln(linkresponse,s);
   end;

{ Write sharedlibraries like -l<lib> }
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.Get;
     i:=Pos(target_os.sharedlibext,S);
     if i>0 then
      Delete(S,i,255);
     writeln(linkresponse,target_link.libprefix+s);
   end;

  writeln(linkresponse,target_link.inputend);

{ Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     Writeln(LinkResponse,target_link.GroupStart);
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        writeln(linkresponse,s)
      end;
     Writeln(LinkResponse,target_link.GroupEnd);
   end;

{ Close response }
  close(linkresponse);
  WriteResponseFile:=True;
end;


Function TLinker.MakeExecutable:boolean;
var
  bindbin    : string[80];
  bindfound  : boolean;
  _stacksize,i,
  _heapsize  : longint;
  s,s2       : string;
  dummy      : file;
  success    : boolean;
begin
{$ifdef linux}
  if LinkToC then
   begin
     AddObject('/usr/lib/crt0.o');
     AddObject('lprt');
     AddStaticLibrary('libc.a');
     AddStaticLibrary('libgcc.a');
   end;
{$endif Linux}

  If not SharedLibFiles.Empty then
   LinkOptions:='-dynamic-linker='+DynamicLinker+' '+LinkOptions;
  if Strip then
   LinkOptions:=LinkOptions+target_link.stripopt;

  S2:=LibrarySearchPath;
  Writeln ('Librarysearchpath : ',S2);
  Repeat
    I:=Pos(';',S2);
    If I<>0 then
      begin
      S:=Copy(S2,1,I-1);
      Delete (S2,1,I);
      end
    else
      S:=S2;
    If S<>'' then
      LinkOptions:=LinkOptions+' -L'+s;
  until S='';
   
{ Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  if not externlink then
   Message1(exec_i_linking,ExeName);
  s:=target_link.linkcmd;
  Replace(s,'$EXE',exename);
  Replace(s,'$OPT',LinkOptions);
  Replace(s,'$RES',inputdir+LinkResName);
  Writeln ('Linker options : ',s);
  success:=DoExec(FindLinker,s,true,false);

{Bind}
  if target_info.target=target_os2 then
   begin
   {Calculate the stack and heap size in kilobytes, rounded upwards.}
     _stacksize:=(stacksize+1023) shr 10;
   {Minimum stacksize for EMX is 32K.}
     if _stacksize<32 then
      _stacksize:=32;
     str(_stacksize,s);
     _heapsize:=(heapsize+1023) shr 10;
     str(_heapsize,s2);
     bindbin:=FindExe('emxbind',bindfound);
     if (not bindfound) and (not externlink) then
      begin
        Message(exec_w_binder_not_found);
        externlink:=true;
      end;
     DoExec(bindbin,'-k'+s+' -o '+exename+'.exe '+exename+' -aim -s'+s2,false,false);
   end;
{Remove ReponseFile}
  if (success) and (not externlink) then
   begin
     assign(dummy,LinkResName);
     {$I-}
      erase(dummy);
     {$I+}
     i:=ioresult;
   end;
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Procedure TLinker.MakeStaticLibrary(const path:string;filescnt:longint);
var
  arbin   : string;
  arfound : boolean;
  cnt     : longint;
  i       : word;
  f       : file;
begin
  arbin:=FindExe('ar',arfound);
  if (not arfound) and (not externlink) then
   begin
     Message(exec_w_ar_not_found);
     externlink:=true;
   end;
  DoExec(arbin,'rs '+staticlibname+' '+FixPath(path)+'*'+target_info.objext,false,true);
{ Clean up }
  if (not writeasmfile) and (not externlink) then
   begin
     for cnt:=1to filescnt do
      begin
        assign(f,FixPath(path)+'as'+tostr(cnt)+target_info.objext);
        {$I-}
         erase(f);
        {$I+}
        i:=ioresult;
      end;
     {$I-}
      rmdir(path);
     {$I+}
     i:=ioresult;
   end;
end;


Procedure TLinker.MakeSharedLibrary;
begin
  DoExec(FindLinker,' -shared -o '+sharedlibname+' link.res',false,false);
end;


end.
{
  $Log$
  Revision 1.8  1998-05-11 13:07:54  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.7  1998/05/08 09:21:20  michael
  * Added missing -Fl message to messages file.
  * Corrected mangling of file names when doing Linklib
  * -Fl now actually WORKS.
  * Librarysearchpath is now a field in linker object.

  Revision 1.6  1998/05/06 09:26:49  peter
    * fixed ld call with shell

  Revision 1.4  1998/05/04 17:54:25  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.3  1998/04/16 10:54:30  daniel
  * Fixed linking for OS/2.
}

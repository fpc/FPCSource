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

{ Needed for LFN support in path to the executable }
{$ifdef GO32V2}
  {$define ALWAYSSHELL}
{$endif}

uses cobjects,files;

Type
    TLinker = Object
       Glibc2,
       Glibc21,
       LinkToC,                           { Should we link to the C libs? }
       Strip             : Boolean;       { Strip symbols ? }
       ObjectFiles,
       SharedLibFiles,
       StaticLibFiles    : TStringContainer;
       LinkOptions       : string;        { Additional options to the linker }
       DynamicLinker     : String[80];    { What Dynamic linker ? }
       LinkResName       : String[32];    { Name of response file }
     { Methods }
       Constructor Init;
       Destructor Done;
       procedure AddModuleFiles(hp:pmodule);
       function  FindObjectFile(s : string) : string;
       function  FindLibraryFile(s:string;const ext:string) : string;
       Procedure AddObject(const S : String);
       Procedure AddStaticLibrary(const S : String);
       Procedure AddSharedLibrary(S : String);
       Function  FindLinker:String;      { Find linker, sets Name }
       Function  DoExec(const command,para:string;info,useshell:boolean):boolean;
       Function  WriteResponseFile:Boolean;
       Function  MakeExecutable:boolean;
       Procedure MakeStaticLibrary(filescnt:longint);
       Procedure MakeSharedLibrary;
       procedure postprocessexecutable(const n : string);virtual;
     end;
     PLinker=^TLinker;

Var
  Linker : PLinker;

procedure InitLinker;
procedure DoneLinker;


Implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  globtype,systems,
  script,globals,verbose,ppu
{$ifdef i386}
  ,win_targ
  ,dos_targ
{$endif}
{$ifdef linux}
  ,linux
{$endif}
  ,gendef
  ;

{$ifndef linux}
Procedure Shell(const command:string);
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
  ObjectFiles.Init_no_double;
  SharedLibFiles.Init_no_double;
  StaticLibFiles.Init_no_double;
  LinkToC:=(cs_link_toc in aktglobalswitches);
  Strip:=(cs_link_strip in aktglobalswitches);
  LinkOptions:=ParaLinkOptions;
  DynamicLinker:=ParaDynamicLinker;
  LinkResName:='link.res';
  Glibc2:=false;
  Glibc21:=false;
  if target_info.target=target_i386_linux then
   begin
     if DynamicLinker='' then
      begin
        { first try glibc2 }
        DynamicLinker:='/lib/ld-linux.so.2';
        if FileExists(DynamicLinker) then
         begin
           Glibc2:=true;
           { also glibc 2.1 / 2.1.1 / 2.1.2 ? }
           if FileExists('/lib/ld-2.1.so') or
              FileExists('/lib/ld-2.1.1.so') or
              FileExists('/lib/ld-2.1.2.so') then
            Glibc21:=true;
         end
        else
         DynamicLinker:='/lib/ld-linux.so.1';
      end;
     AddPathToList(LibrarySearchPath,'/lib;/usr/lib;/usr/X11R6/lib',true);
   end;
end;


Destructor TLinker.Done;
begin
  ObjectFiles.Done;
  SharedLibFiles.Done;
  StaticLibFiles.Done;
end;


procedure TLinker.AddModuleFiles(hp:pmodule);
var
  mask : longint;
begin
  with hp^ do
   begin
   { link unit files }
     if (flags and uf_no_link)=0 then
      begin
        { create mask which unit files need linking }
        mask:=link_allways;
        if hp^.is_unit then
         begin
           { static linking ? }
           if (cs_link_static in aktglobalswitches) then
            begin
              if (flags and uf_static_linked)=0 then
                Comment(V_Error,'unit '+modulename^+' can''t be static linked')
              else
                mask:=mask or link_static;
            end;
           { smart linking ? }
           if (cs_link_smart in aktglobalswitches) then
            begin
              if (flags and uf_smart_linked)=0 then
               begin
                 { if smart not avail then try static linking }
                 if (flags and uf_static_linked)<>0 then
                  begin
                    Comment(V_Warning,'unit '+modulename^+' can''t be smart linked, switching to static linking');
                    mask:=mask or link_static;
                  end
                 else
                  Comment(V_Error,'unit '+modulename^+' can''t be smart or static linked');
               end
              else
               mask:=mask or link_smart;
            end;
           { shared linking }
           if (cs_link_shared in aktglobalswitches) then
            begin
              if (flags and uf_shared_linked)=0 then
               begin
                 { if shared not avail then try static linking }
                 if (flags and uf_static_linked)<>0 then
                  begin
                    Comment(V_Warning,'unit '+modulename^+' can''t be shared linked, switching to static linking');
                    mask:=mask or link_static;
                  end
                 else
                  Comment(V_Error,'unit '+modulename^+' can''t be shared or static linked');
               end
              else
               mask:=mask or link_shared;
            end;
         end
        else
         begin
           { for programs link always static }
           mask:=mask or link_static;
         end;
        { unit files }
        while not linkunitofiles.empty do
         AddObject(linkunitofiles.getusemask(mask));
        while not linkunitstaticlibs.empty do
         AddStaticLibrary(linkunitstaticlibs.getusemask(mask));
        while not linkunitsharedlibs.empty do
         AddSharedLibrary(linkunitsharedlibs.getusemask(mask));
      end;
   { Other needed .o and libs, specified using $L,$LINKLIB,external }
     mask:=link_allways;
     while not linkotherofiles.empty do
      AddObject(linkotherofiles.Getusemask(mask));
     while not linkotherstaticlibs.empty do
      AddStaticLibrary(linkotherstaticlibs.Getusemask(mask));
     while not linkothersharedlibs.empty do
      AddSharedLibrary(linkothersharedlibs.Getusemask(mask));
   end;
end;


var
  LastLDBin : string;
Function TLinker.FindLinker:string;
var
  ldfound : boolean;
begin
  if LastLDBin='' then
   begin
     if utilsdirectory<>'' then
       LastLDBin:=Search(target_link.linkbin+source_os.exeext,utilsdirectory,ldfound)+
         target_link.linkbin+source_os.exeext;
     if LastLDBin='' then
       LastLDBin:=FindExe(target_link.linkbin,ldfound);
     if (not ldfound) and not(cs_link_extern in aktglobalswitches) then
      begin
        Message1(exec_w_linker_not_found,LastLDBin);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end;
     if ldfound then
      Message1(exec_t_using_linker,LastLDBin);
   end;
  FindLinker:=LastLDBin;
end;


{ searches an object file }
function TLinker.FindObjectFile(s:string) : string;
var
  found : boolean;
begin
  findobjectfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+target_info.objext;
  s:=FixFileName(s);
  if FileExists(s) then
   begin
     Findobjectfile:=s;
     exit;
   end;
  { find object file
     1. cwd
     2. unit search path
     3. local object path
     4. global object path
     5. exepath }
  found:=false;
  findobjectfile:=search(s,'.',found)+s;
  if (not found) then
   findobjectfile:=search(s,unitsearchpath,found)+s;
  if (not found) and assigned(current_module^.localobjectsearchpath) then
   findobjectfile:=search(s,current_module^.localobjectsearchpath^,found)+s;
  if (not found) then
   findobjectfile:=search(s,objectsearchpath,found)+s;
  if (not found) then
   findobjectfile:=search(s,exepath,found)+s;
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_objfile_not_found,s);
end;


{ searches an library file }
function TLinker.FindLibraryFile(s:string;const ext:string) : string;
var
  found : boolean;
begin
  findlibraryfile:='';
  if s='' then
   exit;
  if pos('.',s)=0 then
   s:=s+ext;
  if FileExists(s) then
   begin
     FindLibraryFile:=s;
     exit;
   end;
  { find libary
     1. cwd
     2. local libary dir
     3. global libary dir
     4. exe path of the compiler }
  found:=false;
  findlibraryfile:=search(s,'.',found)+s;
  if (not found) and assigned(current_module^.locallibrarysearchpath) then
   findlibraryfile:=search(s,current_module^.locallibrarysearchpath^,found)+s;
  if (not found) then
   findlibraryfile:=search(s,librarysearchpath,found)+s;
  if (not found) then
   findlibraryfile:=search(s,exepath,found)+s;
  if not(cs_link_extern in aktglobalswitches) and (not found) then
   Message1(exec_w_libfile_not_found,s);
end;


Procedure TLinker.AddObject(const S : String);
begin
  ObjectFiles.Insert(FindObjectFile(s));
end;


Procedure TLinker.AddSharedLibrary(S:String);
begin
{ remove prefix 'lib' }
  if Copy(s,1,length(target_os.libprefix))=target_os.libprefix then
   Delete(s,1,length(target_os.libprefix));
{ remove extension if any }
  if Copy(s,length(s)-length(target_os.sharedlibext)+1,length(target_os.sharedlibext))=target_os.sharedlibext then
   Delete(s,length(s)-length(target_os.sharedlibext)+1,length(target_os.sharedlibext)+1);
{ ready to be inserted }
  SharedLibFiles.Insert (S);
end;


Procedure TLinker.AddStaticLibrary(const S:String);
begin
  StaticLibFiles.Insert(FindLibraryFile(s,target_os.staticlibext));
end;


Function TLinker.DoExec(const command,para:string;info,useshell:boolean):boolean;
begin
  DoExec:=true;
  if not(cs_link_extern in aktglobalswitches) then
   begin
     swapvectors;
{$ifdef ALWAYSSHELL}
     shell(command+' '+para);
{$else}
     if useshell then
      shell(command+' '+para)
     else
      exec(command,para);
{$endif}
     swapvectors;
     if (doserror<>0) then
      begin
         Message(exec_w_cant_call_linker);
         aktglobalswitches:=aktglobalswitches+[cs_link_extern];
         DoExec:=false;
      end
     else
      if (dosexitcode<>0) then
       begin
        Message(exec_w_error_while_linking);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
        DoExec:=false;
       end;
   end;
{ Update asmres when externmode is set }
  if cs_link_extern in aktglobalswitches then
   begin
     if info then
      AsmRes.AddLinkCommand(Command,Para,current_module^.exefilename^)
     else
      AsmRes.AddLinkCommand(Command,Para,'');
   end;
end;


Function TLinker.WriteResponseFile : Boolean;
Var
  LinkResponse : Text;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  s,s2         : string;
  found,linux_link_c,
  linkdynamic,
  linklibc     : boolean;

  procedure WriteRes(const s:string);
  begin
    if s<>'' then
     WriteLn(Linkresponse,s);
  end;

  procedure WriteResFileName(const s:string);
  begin
    if s<>'' then
     begin
       if not(s[1] in ['a'..'z','A'..'Z','/','\','.']) then
         Write(Linkresponse,'.',DirSep);
       WriteLn(Linkresponse,s);
     end;
  end;

begin
  WriteResponseFile:=False;
  linux_link_c:=false;
{ set special options for some targets }
  linkdynamic:=not(SharedLibFiles.empty);
  linklibc:=SharedLibFiles.Find('c');
  prtobj:='prt0';
  cprtobj:='cprt0';
  gprtobj:='gprt0';
  if glibc21 then
   begin
     cprtobj:='cprt21';
     gprtobj:='gprt21';
   end;
  case target_info.target of
   target_m68k_Palmos,
   target_i386_Win32 :
     begin
       if DLLsource then
         prtobj:='wdllprt0'
       else
         prtobj:='wprt0';
     end;
   target_m68k_linux,
   target_i386_linux :
     begin
       if cs_profile in aktmoduleswitches then
        begin
          prtobj:=gprtobj;
          if not glibc2 then
           AddSharedLibrary('gmon');
          AddSharedLibrary('c');
          linklibc:=true;
        end
       else
        begin
          if linklibc then
           prtobj:=cprtobj;
        end;
       if linklibc then
         linux_link_c:=true;
     end;
  end;

{ Fix command line options }
  If (DynamicLinker<>'') and (not SharedLibFiles.Empty) then
   LinkOptions:='-dynamic-linker='+DynamicLinker+' '+LinkOptions;
  if Strip and not(cs_debuginfo in aktmoduleswitches) and
                           not (Target_Link.StripBind) then
   LinkOptions:=LinkOptions+' '+target_link.stripopt;

{ Open linkresponse and write header }
  assign(linkresponse,current_module^.outpath^+LinkResName);
  {$I-}
   rewrite(linkresponse);
  {$I+}
  if ioresult<>0 then
   exit;

  { Write library searchpath }
  if assigned(current_module^.locallibrarysearchpath) then
   begin
     S2:=current_module^.locallibrarysearchpath^;
     Repeat
       i:=Pos(';',S2);
       If i=0 then
        i:=255;
       S:=Copy(S2,1,i-1);
       If S<>'' then
         WriteRes(target_link.libpathprefix+s+target_link.libpathsuffix);
       Delete (S2,1,i);
     until S2='';
   end;
  S2:=LibrarySearchPath;
  Repeat
    i:=Pos(';',S2);
    If i=0 then
     i:=255;
    S:=Copy(S2,1,i-1);
    If S<>'' then
      WriteRes(target_link.libpathprefix+s+target_link.libpathsuffix);
    Delete (S2,1,i);
  until S2='';

  WriteRes(target_link.inputstart);
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   WriteResFileName(FindObjectFile(prtobj));
  { try to add crti and crtbegin, they are normally not required, but
    adding can sometimes be usefull }
  if linux_link_c then
   begin
     s:=search('crtbegin.o',librarysearchpath,found)+'crtbegin.o';
     if found then
      WriteResFileName(s);
     s:=search('crti.o',librarysearchpath,found)+'crti.o';
     if found then
      WriteResFileName(s);
   end;
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      WriteResFileName(s);
   end;
  if linux_link_c then
   begin
     s:=search('crtend.o',librarysearchpath,found)+'crtend.o';
     if found then
      WriteResFileName(s);
     s:=search('crtn.o',librarysearchpath,found)+'crtn.o';
     if found then
      WriteResFileName(s);
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.Get;
     if s<>'c' then
      begin
        i:=Pos(target_os.sharedlibext,S);
        if i>0 then
         Delete(S,i,255);
        WriteRes(target_link.libprefix+s);
      end
     else
      begin
        linklibc:=true;
        linkdynamic:=false; { C add's it automaticly }
      end;
   end;
  { be sure that libc is the last lib }
  { arghhhh  this is wrong for DJGPP !!!
    DJGPP need gcc after c lib (umod...) (PM) }
  if linklibc then
   WriteRes(target_link.libprefix+'c');
  { add libgcc after ! }
  if linklibc and (target_info.target=target_i386_go32v2) then
   WriteRes(target_link.libprefix+'gcc');
  if linkdynamic and (DynamicLinker<>'') then
   WriteResFileName(DynamicLinker);
  WriteRes(target_link.inputend);

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     WriteRes(target_link.GroupStart);
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        WriteResFileName(s)
      end;
     WriteRes(target_link.GroupEnd);
   end;

{ Close response }
  close(linkresponse);
  WriteResponseFile:=True;
end;


function TLinker.MakeExecutable:boolean;
var
  bindbin    : string[80];
  bindfound  : boolean;
  s          : string;
  success    : boolean;
  ii         : longint;
begin
  { can be changed after InitLinker
    for DLLs due to relocation problems PM }
  Strip:=(cs_link_strip in aktglobalswitches);
{$ifdef linux}
  if LinkToC then
   begin
     AddObject('/usr/lib/crt0.o');
     AddObject('lprt');
     AddStaticLibrary('libc.a');
     AddStaticLibrary('libgcc.a');
   end;
{$endif Linux}

{ Write used files and libraries }
  WriteResponseFile;

{ Call linker }
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);
  s:=target_link.linkcmd;
  if DLLsource then
    Replace(s,'$EXE',current_module^.sharedlibfilename^)
  else
    Replace(s,'$EXE',current_module^.exefilename^);
  Replace(s,'$OPT',LinkOptions);
  Replace(s,'$RES',current_module^.outpath^+LinkResName);
  success:=DoExec(FindLinker,s,true,false);
{Bind}
  if (target_link.bindbin[1]<>'') and
     ((target_info.target<>target_i386_win32) or
     (RelocSection and not Deffile.empty)) then
   for ii:=1 to target_link.binders do
   begin
     s:=target_link.bindcmd[ii];
     Replace(s,'$OPT',LinkOptions);
     Replace(s,'$RES',current_module^.outpath^+LinkResName);
     if DLLsource then
       Replace(s,'$EXE',current_module^.sharedlibfilename^)
     else
       Replace(s,'$EXE',current_module^.exefilename^);
     {Size of the heap when an EMX program runs in OS/2.}
     Replace(s,'$HEAPMB',tostr((maxheapsize+1048575) shr 20));
     {Size of the stack when an EMX program runs in OS/2.}
     Replace(s,'$STACKKB',tostr((stacksize+1023) shr 10));
     {When an EMX program runs in DOS, the heap and stack share the
      same memory pool. The heap grows upwards, the stack grows downwards.}
     Replace(s,'$DOSHEAPKB',tostr((stacksize+maxheapsize+1023) shr 10));
     if Strip and Target_Link.StripBind then
       Replace (S, '$STRIP', Target_Link.StripOpt)
     else
       Replace (S, '$STRIP', '');
     if utilsdirectory<>'' then
       begin
          bindbin:=Search(target_link.bindbin[ii]+source_os.exeext,
            utilsdirectory,bindfound)+target_link.bindbin[ii]+source_os.exeext;
       end
     else
       bindbin:=FindExe(target_link.bindbin[ii],bindfound);
     if (not bindfound) and not (cs_link_extern in aktglobalswitches) then
      begin
        Message1(exec_w_binder_not_found,bindbin);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end;
     DoExec(bindbin,s,false,false);
   end;

{ Post processor executable }
  if success and
     not(cs_link_extern in aktglobalswitches) then
   begin
     if DLLsource then
      postprocessexecutable(current_module^.sharedlibfilename^)
     else
      postprocessexecutable(current_module^.exefilename^);
   end;

{Remove ReponseFile}
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(current_module^.outpath^+LinkResName);
  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Procedure TLinker.MakeStaticLibrary(filescnt:longint);
{
  FilesCnt holds the amount of .o files created, if filescnt=0 then
  no smartlinking is used
}
var
  smartpath,
  s,
  arbin   : string;
  arfound : boolean;
  cnt     : longint;
begin
  smartpath:=current_module^.path^+FixPath(FixFileName(current_module^.modulename^)+target_info.smartext,false);
{ find ar binary }
  if utilsdirectory<>'' then
    begin
       arbin:=Search(target_ar.arbin+source_os.exeext,
         utilsdirectory,arfound)+target_ar.arbin+source_os.exeext;
    end
  else
    arbin:=FindExe(target_ar.arbin,arfound);
  if (not arfound) and not(cs_link_extern in aktglobalswitches) then
   begin
     Message(exec_w_ar_not_found);
     aktglobalswitches:=aktglobalswitches+[cs_link_extern];
   end;
  s:=target_ar.arcmd;
  Replace(s,'$LIB',current_module^.staticlibfilename^);
  if filescnt=0 then
    Replace(s,'$FILES',current_module^.objfilename^)
  else
    Replace(s,'$FILES',FixFileName(smartpath+current_module^.asmprefix^+'*'+target_info.objext));
  DoExec(arbin,s,false,true);
{ Clean up }
  if not(cs_asm_leave in aktglobalswitches) then
   if not(cs_link_extern in aktglobalswitches) then
    begin
      if filescnt=0 then
       RemoveFile(current_module^.objfilename^)
      else
       begin
         for cnt:=1 to filescnt do
          if not RemoveFile(FixFileName(smartpath+current_module^.asmprefix^+tostr(cnt)+target_info.objext)) then
           RemoveFile(FixFileName(smartpath+current_module^.asmprefix^+'e'+tostr(cnt)+target_info.objext));
         RemoveDir(smartpath);
       end;
    end
   else
    begin
      if filescnt=0 then
       AsmRes.AddDeleteCommand(current_module^.objfilename^)
      else
       begin
         AsmRes.AddDeleteCommand(smartpath+current_module^.asmprefix^+'*'+target_info.objext);
         AsmRes.Add('rmdir '+smartpath);
       end;
    end;
end;


Procedure TLinker.MakeSharedLibrary;
var
  s : string;
begin
  s:=' -shared -o $LIB $FILES';
  Replace(s,'$LIB',current_module^.sharedlibfilename^);
  Replace(s,'$FILES',current_module^.objfilename^);
  if DoExec(FindLinker,s,false,false) then
   RemoveFile(current_module^.objfilename^);
end;

procedure tlinker.postprocessexecutable(const n:string);
begin
end;


procedure InitLinker;
begin
  case target_info.target of
{$ifdef i386}
    target_i386_Win32 :
      linker:=new(plinkerwin32,Init);
    target_i386_Go32v2 :
      linker:=new(plinkergo32v2,Init);
{$endif i386}
    else
      linker:=new(plinker,Init);
  end;
end;


procedure DoneLinker;
begin
  if assigned(linker) then
   dispose(linker,done);
end;


end.
{
  $Log$
  Revision 1.67  1999-08-16 15:35:23  pierre
    * fix for DLL relocation problems
    * external bss vars had wrong stabs for pecoff
    + -WB11000000 to specify default image base, allows to
      load several DLLs with debugging info included
      (relocatable DLL are stripped because the relocation
       of the .Stab section is misplaced by ldw)

  Revision 1.66  1999/08/11 17:26:34  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

  Revision 1.65  1999/08/10 12:51:16  pierre
    * bind_win32_dll removed (Relocsection used instead)
    * now relocsection is true by default ! (needs dlltool
      for DLL generation)

  Revision 1.64  1999/07/30 23:19:45  peter
    * fixed placing of dynamiclinker in link.res (should be the last after
      all other libraries)

  Revision 1.63  1999/07/29 01:31:39  peter
    * fixed shared library linking for glibc2 systems

  Revision 1.62  1999/07/27 11:05:51  peter
    * glibc 2.1.2 support

  Revision 1.61  1999/07/18 10:19:53  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.60  1999/07/07 20:33:53  peter
    * warning instead of error when switching to static linking

  Revision 1.59  1999/07/05 16:21:26  peter
    * fixed linking for units without linking necessary

  Revision 1.58  1999/07/03 00:29:51  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.57  1999/06/28 16:02:31  peter
    * merged

  Revision 1.54.2.3  1999/06/28 15:55:40  peter
    * also search path if not found in utilsdirectory

  Revision 1.54.2.2  1999/06/18 09:51:55  peter
    * always use shell() for go32v2 to support LFN

  Revision 1.54.2.1  1999/06/15 13:51:56  peter
    * also check ld-2.1.so for glibc 2.1, previous was only for 2.1.1

  Revision 1.54  1999/06/02 13:25:35  hajny
    * fixed stripping symbols for OS/2

  Revision 1.53  1999/05/04 21:44:44  florian
    * changes to compile it with Delphi 4.0

  Revision 1.52  1999/05/03 21:30:30  peter
    + glibc 2.1

  Revision 1.51  1999/04/28 23:42:33  pierre
   * removing of temporary directory with -s option

  Revision 1.50  1999/04/25 14:31:48  daniel
  * Bug fixed in linking: compiling files on another drive than the one you
  currently use you is done correctly.

  Revision 1.49  1999/03/25 16:55:30  peter
    + unitpath,librarypath,includepath,objectpath directives

  Revision 1.48  1999/03/23 16:22:43  peter
    * crtbegin/crtend only added if found

  Revision 1.47  1999/02/05 16:45:47  michael
  + Fixed gluing of options

  Revision 1.46  1999/02/05 08:54:26  pierre
    + linkofiles splitted inot linkofiles and linkunitfiles
      because linkofiles must be stored with directory
      to enabled linking of different objects with same name
      in a different directory

  Revision 1.45  1999/01/29 10:33:07  peter
    * objectsearchpath is now also searched if an object is not found

  Revision 1.44  1999/01/27 13:07:58  pierre
   * problem related with libc : go32v2 and linux differences

  Revision 1.43  1999/01/25 15:02:13  peter
    * link libc always as last

  Revision 1.42  1998/12/11 00:03:19  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.41  1998/12/01 23:39:46  pierre
   * postprocessexec for win32 changed

  Revision 1.40  1998/12/01 12:51:20  peter
    * fixed placing of ppas.sh and link.res when using -FE

  Revision 1.39  1998/11/30 13:26:23  pierre
    * the code for ordering the exported procs/vars was buggy
    + added -WB to force binding (Ozerski way of creating DLL)
      this is off by default as direct writing of .edata section seems
      OK

  Revision 1.38  1998/11/30 09:43:13  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.37  1998/10/26 22:23:31  peter
    + fixpath() has an extra option to allow a ./ as path

  Revision 1.36  1998/10/22 15:18:44  florian
    + switch -vx for win32 added

  Revision 1.35  1998/10/19 18:06:23  peter
    * use no_double

  Revision 1.34  1998/10/16 13:37:18  florian
    + switch -FD added to specify the path for utilities

  Revision 1.33  1998/10/14 13:38:22  peter
    * fixed path with staticlib/objects in ppufiles

  Revision 1.32  1998/10/14 11:03:55  daniel
  * Forgot to dereference a pointer.

  Revision 1.31  1998/10/14 11:01:21  daniel
  * Staticlibfilename no longer not include a path. Correction when calling
  ar.

  Revision 1.30  1998/10/13 13:10:18  peter
    * new style for m68k/i386 infos and enums

  Revision 1.29  1998/10/13 08:19:34  pierre
    + source_os is now set correctly for cross-processor compilers
      (tos contains all target_infos and
       we use CPU86 and CPU68 conditionnals to
       get the source operating system
       this only works if you do not undefine
       the source target  !!)
    * several cg68k memory leaks fixed
    + started to change the code so that it should be possible to have
      a complete compiler (both for m68k and i386 !!)

  Revision 1.28  1998/10/08 23:28:56  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.27  1998/10/06 17:16:52  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.26  1998/09/29 15:23:05  peter
    * remove also the end files for smartlinking

  Revision 1.25  1998/09/10 15:25:31  daniel
  + Added maxheapsize.
  * Corrected semi-bug in calling the assembler and the linker

  Revision 1.24  1998/09/07 18:32:45  peter
    * fixed for m68k

  Revision 1.23  1998/09/03 17:39:04  florian
    + better code for type conversation longint/dword to real type

  Revision 1.22  1998/09/01 09:01:00  peter
    + glibc2 support

  Revision 1.21  1998/08/31 12:26:26  peter
    * m68k and palmos updates from surebugfixes

  Revision 1.20  1998/08/19 10:06:14  peter
    * fixed filenames and removedir which supports slash at the end

  Revision 1.19  1998/08/17 09:17:47  peter
    * static/shared linking updates

  Revision 1.18  1998/08/14 21:56:34  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.17  1998/08/14 18:16:08  peter
    * return after a failed call will now add it to ppas

  Revision 1.16  1998/08/12 19:28:15  peter
    * better libc support

  Revision 1.15  1998/08/10 14:50:02  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.14  1998/06/17 14:10:13  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.13  1998/06/08 22:59:46  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.12  1998/06/04 23:51:44  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.11  1998/05/27 00:20:31  peter
    * some scanner optimizes
    * automaticly aout2exe for go32v1
    * fixed dynamiclinker option which was added at the wrong place

  Revision 1.10  1998/05/22 12:32:47  peter
    * fixed -L on the commandline, Dos commandline is only 128 bytes

  Revision 1.9  1998/05/12 10:46:59  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.8  1998/05/11 13:07:54  peter
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

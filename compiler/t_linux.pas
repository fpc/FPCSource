{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Linux target

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
unit t_linux;

{$i defines.inc}

interface

  uses
    import,export,link;

  type
    pimportliblinux=^timportliblinux;
    timportliblinux=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
    end;

    pexportliblinux=^texportliblinux;
    texportliblinux=object(texportlib)
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
    end;

    plinkerlinux=^tlinkerlinux;
    tlinkerlinux=object(tlinker)
    private
      Glibc2,
      Glibc21 : boolean;
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Init;
      procedure SetDefaultInfo;virtual;
      function  MakeExecutable:boolean;virtual;
      function  MakeSharedLibrary:boolean;virtual;
    end;


implementation

  uses
    cutils,verbose,cobjects,systems,globtype,globals,
    symconst,script,
    fmodule,aasm,cpuasm,cpubase,symtable{$IFDEF NEWST},symbols{$ENDIF NEWST};

{*****************************************************************************
                               TIMPORTLIBLINUX
*****************************************************************************}

procedure timportliblinux.preparelib(const s : string);
begin
end;


procedure timportliblinux.importprocedure(const func,module : string;index : longint;const name : string);
begin
  { insert sharedlibrary }
  current_module^.linkothersharedlibs.insert(SplitName(module),link_allways);
  { do nothing with the procedure, only set the mangledname }
  if name<>'' then
    aktprocsym^.definition^.setmangledname(name)
  else
    message(parser_e_empty_import_name);
end;


procedure timportliblinux.importvariable(const varname,module:string;const name:string);
begin
  { insert sharedlibrary }
  current_module^.linkothersharedlibs.insert(SplitName(module),link_allways);
  { reset the mangledname and turn off the dll_var option }
  aktvarsym^.setmangledname(name);
  exclude(aktvarsym^.varoptions,vo_is_dll_var);
end;


procedure timportliblinux.generatelib;
begin
end;


{*****************************************************************************
                               TEXPORTLIBLINUX
*****************************************************************************}

procedure texportliblinux.preparelib(const s:string);
begin
end;


procedure texportliblinux.exportprocedure(hp : pexported_item);
var
  hp2 : pexported_item;
begin
  { first test the index value }
  if (hp^.options and eo_index)<>0 then
   begin
     Message1(parser_e_no_export_with_index_for_target,'linux');
     exit;
   end;
  { use pascal name is none specified }
  if (hp^.options and eo_name)=0 then
    begin
       hp^.name:=stringdup(hp^.sym^.name);
       hp^.options:=hp^.options or eo_name;
    end;
  { now place in correct order }
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) and
     (hp^.name^>hp2^.name^) do
    hp2:=pexported_item(hp2^.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2^.name^=hp^.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp^.name^);
      exit;
    end;
  if hp2=pexported_item(current_module^._exports^.first) then
    current_module^._exports^.insert(hp)
  else if assigned(hp2) then
    begin
       hp^.next:=hp2;
       hp^.previous:=hp2^.previous;
       if assigned(hp2^.previous) then
         hp2^.previous^.next:=hp;
       hp2^.previous:=hp;
    end
  else
    current_module^._exports^.concat(hp);
end;


procedure texportliblinux.exportvar(hp : pexported_item);
begin
  hp^.is_var:=true;
  exportprocedure(hp);
end;


procedure texportliblinux.generatelib;
var
  hp2 : pexported_item;
begin
  hp2:=pexported_item(current_module^._exports^.first);
  while assigned(hp2) do
   begin
     if not hp2^.is_var then
      begin
{$ifdef i386}
        { place jump in codesegment }
        codesegment^.concat(new(pai_align,init_op(4,$90)));
        codesegment^.concat(new(pai_symbol,initname_global(hp2^.name^,0)));
        codesegment^.concat(new(paicpu,op_sym(A_JMP,S_NO,newasmsymbol(hp2^.sym^.mangledname))));
        codesegment^.concat(new(pai_symbol_end,initname(hp2^.name^)));
{$endif i386}
      end
     else
      Message1(parser_e_no_export_of_variables_for_target,'linux');
     hp2:=pexported_item(hp2^.next);
   end;
end;


{*****************************************************************************
                                  TLINKERLINUX
*****************************************************************************}

Constructor TLinkerLinux.Init;
begin
  Inherited Init;
  LibrarySearchPath.AddPath('/lib;/usr/lib;/usr/X11R6/lib',true);
end;


procedure TLinkerLinux.SetDefaultInfo;
{
  This will also detect which libc version will be used
}
begin
  Glibc2:=false;
  Glibc21:=false;
  with Info do
   begin
     ExeCmd[1]:='ld $OPT $DYNLINK $STATIC $STRIP -L. -o $EXE $RES';
     DllCmd[1]:='ld $OPT -shared -L. -o $EXE $RES';
     DllCmd[2]:='strip --strip-unneeded $EXE';
     { first try glibc2 }
     DynamicLinker:='/lib/ld-linux.so.2';
     if FileExists(DynamicLinker) then
      begin
        Glibc2:=true;
        { Check for 2.0 files, else use the glibc 2.1 stub }
        if FileExists('/lib/ld-2.0.*') then
         Glibc21:=false
        else
         Glibc21:=true;
      end
     else
      DynamicLinker:='/lib/ld-linux.so.1';
     {$ifdef BSD}
      DynamicLinker:='';
     {$endif}
   end;

end;


Function TLinkerLinux.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  cprtobj,
  gprtobj,
  prtobj       : string[80];
  HPath        : PStringQueueItem;
  s            : string;
  found,
  linkdynamic,
  linklibc     : boolean;
begin
  WriteResponseFile:=False;
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

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+HPath^.Data^+')');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+HPath^.Data^+')');
     HPath:=HPath^.Next;
   end;

  LinkRes.Add('INPUT(');
  { add objectfiles, start with prt0 always }
  if prtobj<>'' then
   LinkRes.AddFileName(FindObjectFile(prtobj,''));
  { try to add crti and crtbegin if linking to C }
  if linklibc then
   begin
     s:=librarysearchpath.FindFile('crtbegin.o',found)+'crtbegin.o';
     if found then
      LinkRes.AddFileName(s);
     s:=librarysearchpath.FindFile('crti.o',found)+'crti.o';
     if found then
      LinkRes.AddFileName(s);
   end;
  { main objectfiles }
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;
  { objects which must be at the end }
  if linklibc then
   begin
     s:=librarysearchpath.FindFile('crtend.o',found)+'crtend.o';
     if found then
      LinkRes.AddFileName(s);
     s:=librarysearchpath.FindFile('crtn.o',found)+'crtn.o';
     if found then
      LinkRes.AddFileName(s);
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(s)
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     LinkRes.Add('INPUT(');
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.Get;
        if s<>'c' then
         begin
           i:=Pos(target_os.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s);
         end
        else
         begin
           linklibc:=true;
           linkdynamic:=false; { libc will include the ld-linux for us }
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      LinkRes.Add('-lc');
     { when we have -static for the linker the we also need libgcc }
     if (cs_link_staticflag in aktglobalswitches) then
      LinkRes.Add('-lgcc');
     if linkdynamic and (Info.DynamicLinker<>'') then
      LinkRes.AddFileName(Info.DynamicLinker);
     LinkRes.Add(')');
   end;
{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;


function TLinkerLinux.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  DynLinkStr : string[60];
  StaticStr,
  StripStr   : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
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

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module^.exefilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  Replace(cmdstr,'$STATIC',StaticStr);
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$DYNLINK',DynLinkStr);
  success:=DoExec(FindUtil(BinStr),CmdStr,true,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerLinux.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.sharedlibfilename^);

{ Write used files and libraries }
  WriteResponseFile(true);

{ Call linker }
  SplitBinCmd(Info.DllCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',current_module^.sharedlibfilename^);
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  Replace(cmdstr,'$RES',outputexedir+Info.ResName);
  success:=DoExec(FindUtil(binstr),cmdstr,true,false);

{ Strip the library ? }
  if success and (cs_link_strip in aktglobalswitches) then
   begin
     SplitBinCmd(Info.DllCmd[2],binstr,cmdstr);
     Replace(cmdstr,'$EXE',current_module^.sharedlibfilename^);
     success:=DoExec(FindUtil(binstr),cmdstr,true,false);
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;


end.
{
  $Log$
  Revision 1.7  2000-09-24 21:33:47  peter
    * message updates merges

  Revision 1.6  2000/09/24 15:06:31  peter
    * use defines.inc

  Revision 1.5  2000/09/10 20:26:55  peter
    * bsd patches from marco

  Revision 1.4  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/07/13 12:08:28  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
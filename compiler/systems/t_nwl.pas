{
    Copyright (c) 1998-2004 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Netware libc target

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

    Currently generating NetWare-NLM's only work under Linux and win32.
    (see http://home.arcor.de/armin.diehl/fpcnw for binutils working
    with win32) while not included in fpc-releases.

    The following compiler-swiches are supported for NetWare:
    $DESCRIPTION    : NLM-Description, will be displayed at load-time
    $M              : For Stack-Size, Heap-Size will be ignored
                      32K is the accepted minimum
    $VERSION x.x.x  : Sets Major, Minor and Revision
    $SCREENNAME     : Sets the ScreenName
    $THREADNAME     : Sets current threadname

    Additional parameters for the nlmvonv-inputfile can be passed with
    -k, i.e. -kREENTRANT will add the option REENTRANT to the nlmconv
    inputfile. A ; will be converted into a newline

    Exports will be handled like in win32:
    procedure bla; cdecl;
    begin
    end;

    exports foo name 'Bar';

    The path to the import-Files must be specified by the library-path.
    All external modules are defined as autoload. (Note: the import-files have
    to be in unix-format for exe2nlm)
    By default, the most import files are included in freepascal.

    e.g. function getgrnam(name:Pchar):Pgroup;cdecl;external 'libc' 'getgrnam';
    sets IMPORT @libc.imp and MODULE libc.
    To avoid setting the autoload, use ! in the name, e.g.
    procedure EnterDebugger;cdecl;external '!netware' name 'EnterDebugger';

    Function simply defined as external work without generating autoload and
    IMPORT but you will get a warning from nlmconv.

    If you dont have nlmconv, compile gnu-binutils with
       ./configure --enable-targets=i386-netware
       make all

    Debugging is possible with gdb and a converter from gdb to ndi available
    at http://home.arcor.de/armin.diehl/gdbnw

    A sample program:

    Program Hello;
    (*$DESCRIPTION HelloWorldNlm*)
    (*$VERSION 1.2.3*)
    (*$ScreenName Hello*)
    (*$M 60000,60000*)
    begin
      writeLn ('hello world');
    end.

    compile with:
    ppc386 -Tnetwlibc hello

    Libraries are supported but this needs at least netware 5.1 sp6,
    6.0 sp3 or netware 6.5

    In case there is a xdc file with the same name as the nlm name,
    this file will be used for nlmconv. Otherwise a temp xdc will
    be created and used.

****************************************************************************
}
unit t_nwl;

{$i fpcdefs.inc}

interface


implementation

{$ifdef netwlibc}
{$define netware}
{$endif}

  uses
    SysUtils,
    cutils,cfileutl,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmdata,aasmcpu,cpubase,symsym,symdef,
    import,export,link,i_nwl,ogbase
    {$ifdef netware} ,dos {$endif}
    ;

  type
    timportlibnetwlibc=class(timportlib)
      procedure generatelib;override;
    end;

    texportlibnetwlibc=class(texportlib)
      procedure preparelib(const s : string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkernetwlibc=class(texternallinker)
    private
      NLMConvLinkFile: TLinkRes;  {for second pass, fist pass is ld}
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeNetwareLoadableModule (isLib : boolean):boolean;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
    end;

Const tmpLinkFileName = '~link~tmp.o';
      minStackSize = 32768;

{*****************************************************************************
                               TIMPORTLIBNETWARE
*****************************************************************************}

    procedure timportlibnetwlibc.generatelib;
      var
        i : longint;
        ImportLibrary : TImportLibrary;
      begin
        for i:=0 to current_module.ImportLibraryList.Count-1 do
          begin
            ImportLibrary:=TImportLibrary(current_module.ImportLibraryList[i]);
            current_module.linkothersharedlibs.add(ImportLibrary.Name,link_always);
          end;
      end;


{*****************************************************************************
                               TEXPORTLIBNETWARE
*****************************************************************************}

procedure texportlibnetwlibc.preparelib(const s:string);
begin
end;


procedure texportlibnetwlibc.exportprocedure(hp : texported_item);
var
  hp2 : texported_item;
begin
  { first test the index value }
  if (hp.options and eo_index)<>0 then
   begin
     Comment(V_Error,'can''t export with index under netware');
     exit;
   end;
  { use pascal name is none specified }
  if (hp.options and eo_name)=0 then
    begin
       hp.name:=stringdup(hp.sym.name);
       hp.options:=hp.options or eo_name;
    end;
  { now place in correct order }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) and
     (hp.name^>hp2.name^) do
    hp2:=texported_item(hp2.next);
  { insert hp there !! }
  if assigned(hp2) and (hp2.name^=hp.name^) then
    begin
      { this is not allowed !! }
      Message1(parser_e_export_name_double,hp.name^);
      exit;
    end;
  if hp2=texported_item(current_module._exports.first) then
    current_module._exports.insert(hp)
  else if assigned(hp2) then
    begin
       hp.next:=hp2;
       hp.previous:=hp2.previous;
       if assigned(hp2.previous) then
         hp2.previous.next:=hp;
       hp2.previous:=hp;
    end
  else
    current_module._exports.concat(hp);
end;


procedure texportlibnetwlibc.exportvar(hp : texported_item);
begin
  hp.is_var:=true;
  exportprocedure(hp);
end;


procedure texportlibnetwlibc.generatelib;
var
  hp2 : texported_item;
  pd  : tprocdef;
begin
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if (not hp2.is_var) and
        (hp2.sym.typ=procsym) then
      begin
        { the manglednames can already be the same when the procedure
          is declared with cdecl }
        pd:=tprocdef(tprocsym(hp2.sym).ProcdefList[0]);
        if pd.mangledname<>hp2.name^ then
         begin
{$ifdef i386}
           { place jump in al_procedures }
           current_asmdata.asmlists[al_procedures].concat(Tai_align.Create_op(4,$90));
           current_asmdata.asmlists[al_procedures].concat(Tai_symbol.Createname_global(hp2.name^,AT_FUNCTION,0));
           current_asmdata.asmlists[al_procedures].concat(Taicpu.Op_sym(A_JMP,S_NO,current_asmdata.RefAsmSymbol(pd.mangledname)));
           current_asmdata.asmlists[al_procedures].concat(Tai_symbol_end.Createname(hp2.name^));
{$endif i386}
         end;
      end
     else
      //Comment(V_Error,'Exporting of variables is not supported under netware');
      Message1(parser_e_no_export_of_variables_for_target,'netware');
     hp2:=texported_item(hp2.next);
   end;
end;


{*****************************************************************************
                                  TLINKERNETWARE
*****************************************************************************}

Constructor TLinkerNetwlibc.Create;
begin
  Inherited Create;
end;


procedure TLinkerNetwlibc.SetDefaultInfo;
begin
  with Info do
   begin
     {$ifndef netware}
     ExeCmd[1]:= FindUtil(utilsprefix+'ld') + ' -Ur -T $RES $STRIP -o $TMPOBJ';
     ExeCmd[2]:= FindUtil(utilsprefix+'nlmconv') + ' -T$RES';
     {$else}
     {for running on netware we need absolute pathes since ld has another working directory}
     ExeCmd[1]:= FindUtil(utilsprefix+'ld') + ' -Ur -T '+FExpand(outputexedir+Info.ResName)+' $STRIP -o '+Fexpand(outputexedir+tmpLinkFileName);
     ExeCmd[2]:= FindUtil(utilsprefix+'nlmconv') + ' -T'+FExpand(outputexedir+'n'+Info.ResName);
     {$endif}
   end;
end;


Function TLinkerNetwlibc.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres      : TLinkRes;
  i            : longint;
  s,s2,s3      : TCmdStr;
  ProgNam      : string [80];
  NlmNam       : string [80];
  hp2          : texported_item;  { for exports }
  p            : byte;
begin
  WriteResponseFile:=False;

  ProgNam := current_module.exefilename;
  i:=Pos(target_info.exeext,ProgNam);
  if i>0 then
    Delete(ProgNam,i,255);
  NlmNam := ProgNam + target_info.exeext;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);             {for ld}
  NLMConvLinkFile:=TLinkRes.Create(outputexedir+'n'+Info.ResName,true); {for nlmconv, written in CreateExeFile}

  p := Pos ('"', Description);
  while (p > 0) do
  begin
    delete (Description,p,1);
    p := Pos ('"', Description);
  end;
  if Description <> '' then
    NLMConvLinkFile.Add('DESCRIPTION "' + Description + '"');
  NLMConvLinkFile.Add('VERSION '+tostr(dllmajor)+','+tostr(dllminor)+','+tostr(dllrevision));

  p := Pos ('"', nwscreenname);
  while (p > 0) do
  begin
    delete (nwscreenname,p,1);
    p := Pos ('"', nwscreenname);
  end;
  p := Pos ('"', nwthreadname);
  while (p > 0) do
  begin
    delete (nwthreadname,p,1);
    p := Pos ('"', nwthreadname);
  end;
  p := Pos ('"', nwcopyright);
  while (p > 0) do
  begin
    delete (nwcopyright,p,1);
    p := Pos ('"', nwcopyright);
  end;

  if nwscreenname <> '' then
    NLMConvLinkFile.Add('SCREENNAME "' + nwscreenname + '"');
  if nwthreadname <> '' then
    NLMConvLinkFile.Add('THREADNAME "' + nwthreadname + '"');
  if nwcopyright <> '' then
    NLMConvLinkFile.Add('COPYRIGHT "' + nwcopyright + '"');

  if stacksize < minStackSize then stacksize := minStackSize;
  str (stacksize, s);
  NLMConvLinkFile.Add ('STACKSIZE '+s);
  {$ifndef netware}
  NLMConvLinkFile.Add ('INPUT '+outputexedir+tmpLinkFileName);
  {$else}
  NLMConvLinkFile.Add ('INPUT '+FExpand(outputexedir+tmpLinkFileName));
  {$endif}

  { add objectfiles, start with nwpre always }
  LinkRes.Add ('INPUT(');
  s2 := FindObjectFile('nwplibc','',false);
  if s2 = '' then
    s2 := FindObjectFile('libcpre.gcc','',false);
  Comment (V_Debug,'adding Object File '+s2);
  {$ifndef netware} LinkRes.Add (s2); {$else} LinkRes.Add (FExpand(s2)); {$endif}

  if isDll then  {needed to provide main}
    s2 := FindObjectFile('nwl_dlle','',false)
  else
    s2 := FindObjectFile('nwl_main','',false);
  Comment (V_Debug,'adding Object File '+s2);
  {$ifndef netware} LinkRes.Add (s2); {$else} LinkRes.Add (FExpand(s2)); {$endif}

  { main objectfiles, add to linker input }
  while not ObjectFiles.Empty do
  begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
    begin
      s2 := FindObjectFile (s,'',false);
      Comment (V_Debug,'adding Object File '+s2);
      {$ifndef netware} LinkRes.Add (s2); {$else} LinkRes.Add (FExpand(s2)); {$endif}
    end;
  end;
  LinkRes.Add (')');

  { output file (nlm), add to nlmconv }
  {$ifndef netware}
  NLMConvLinkFile.Add ('OUTPUT ' + NlmNam);
  {$else}
  NLMConvLinkFile.Add ('OUTPUT ' + FExpand(NlmNam));
  {$endif}

  { start and stop-procedures }
  NLMConvLinkFile.Add ('START _LibCPrelude');
  NLMConvLinkFile.Add ('EXIT _LibCPostlude');
  NLMConvLinkFile.Add ('CHECK _LibCCheckUnload');
  NLMConvLinkFile.Add ('REENTRANT');            { needed by older libc versions }

  if not (cs_link_strip in current_settings.globalswitches) then
  begin
    NLMConvLinkFile.Add ('DEBUG');
    Comment(V_Debug,'DEBUG');
  end;

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add ('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=lower (StaticLibFiles.GetFirst);
        if s<>'' then
        begin
          {ad: that's a hack !
           whith -XX we get the .a files as static libs (in addition to the
           imported libraries}
         if (pos ('.a',s) <> 0) OR (pos ('.A', s) <> 0) then
         begin
           S2 := FindObjectFile(s,'',false);
           {$ifndef netware} LinkRes.Add (s2); {$else} LinkRes.Add (FExpand(s2)); {$endif}
           Comment(V_Debug,'adding Object File (StaticLibFiles) '+S2);
         end else
         begin
           i:=Pos(target_info.staticlibext,S);
           if i>0 then
             Delete(S,i,255);
           S := S + '.imp'; S2 := '';
           librarysearchpath.FindFile(S,false,S2);
           {$ifdef netware}
           Comment(V_Debug,'IMPORT @'+s2);
           s2 := FExpand (S2);
           {$endif}
           NLMConvLinkFile.Add('IMPORT @'+S2);
           Comment(V_Debug,'IMPORT @'+s2);
         end;
        end
      end;
     LinkRes.Add (')');
   end;

  if not SharedLibFiles.Empty then
   begin
     While not SharedLibFiles.Empty do
      begin
        {because of upper/lower case mix, we may get duplicate
         names but nlmconv ignores that.
         Here we are setting the import-files for nlmconv. I.e. for
         the module libc or libc.nlm we add IMPORT @libc.imp and also
         the module libc.nlm (autoload)
         If a lib name begins with !, only the IMPORT will be generated
         ? may it be better to set autoload's via StaticLibFiles ? }
        S:=lower (SharedLibFiles.GetFirst);
        if s<>'' then
         begin
           s2:=s;
           i:=Pos(target_info.sharedlibext,S);
           if i>0 then
             Delete(S,i,255);
           if s[1] = '!' then
           begin  // special, with ! only the imp will be included but no module is autoloaded, needed i.e. for netware.imp inlcuded in libc ndk
             delete (s,1,1);
             S := S + '.imp';
             librarysearchpath.FindFile(S,false,S3);
             {$ifdef netware}
             Comment(V_Debug,'IMPORT @'+S3);
             S3 := FExpand (S3);
             {$endif}
             NLMConvLinkFile.Add('IMPORT @'+S3);
             Comment(V_Debug,'IMPORT @'+S3);
           end else
           begin
             S := S + '.imp';
             librarysearchpath.FindFile(S,false,S3);
             {$ifdef netware}
             Comment(V_Debug,'IMPORT @'+S3);
             S3 := FExpand (S3);
             {$endif}
             NLMConvLinkFile.Add('IMPORT @'+S3);
             NLMConvLinkFile.Add('MODULE '+s2);
             Comment(V_Debug,'MODULE '+S2);
             Comment(V_Debug,'IMPORT @'+S3);
           end;
         end
      end;
   end;

  { write exports }
  hp2:=texported_item(current_module._exports.first);
  while assigned(hp2) do
   begin
     if not hp2.is_var then
      begin
        { Export the Symbol }
        Comment(V_Debug,'EXPORT '+hp2.name^);
        NLMConvLinkFile.Add ('EXPORT '+hp2.name^);
      end
     else
      { really, i think it is possible }
      {Comment(V_Error,'Exporting of variables is not supported under netware');}
      Message1(parser_e_no_export_of_variables_for_target,'netware');
     hp2:=texported_item(hp2.next);
   end;

{ Write and Close response for ld, response for nlmconv is in NLMConvLinkFile(not written) }
  linkres.writetodisk;
  LinkRes.Free;

{ pass options from -k to nlmconv, ; is interpreted as newline }
  s := ParaLinkOptions;
  while(Length(s) > 0) and (s[1] = ' ') do
    delete (s,1,1);
  p := pos ('"',s);
  while p > 0 do
  begin
    delete (s,p,1);
    p := pos ('"',s);
  end;

  p := pos (';',s);
  while p > 0 do
  begin
    s2 := copy(s,1,p-1);
    comment (V_Debug,'adding "'+s2+'" to nlmvonv input');
    NLMConvLinkFile.Add(s2);
    delete (s,1,p);
    p := pos (';',s);
  end;
  if s <> '' then
  begin
    comment (V_Debug,'adding "'+s+'" to nlmvonv input');
    NLMConvLinkFile.Add(s);
  end;

  WriteResponseFile:=True;
end;

Const
  xdc : Array[0..127] of char = (
       'B','A','G','F',#2,#0,#0,#0,#1,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,
       #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#2,#0,#0,#0,#0,#0,#0,#0,#16,#0,#0,
       #0,#7,'M','P','K','_','B','a','g',#0,#0,#0,#0,#0,#0,#0,#0,#11,'M','T',
       ' ','S','a','f','e',' ','N','L','M',#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,
       #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,
       #0,#0,#0,#0,#0,#0,#1,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0);


function TLinkerNetwlibc.MakeNetwareLoadableModule (isLib : boolean):boolean;
var
  binstr,
  cmdstr : TCmdStr;
  xdcname : string;
  success  : boolean;
  StripStr : string[2];
  xdcpresent,usexdc : boolean;
  f : file;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
   Message1(exec_i_linking,current_module.exefilename);

{ Create some replacements }
  StripStr:='';

  if (cs_link_strip in current_settings.globalswitches) then
   StripStr:='-s';

{ Write used files and libraries and create Headerfile for
  nlmconv in NLMConvLinkFile }
  WriteResponseFile(isLib);
  if isLib then
    NLMConvLinkFile.Add('FLAG_ON 1024');  {0x400 Specifies whether the NLM is a shared library.}

{ if we have a xdc file, dont touch it, otherwise create a new
  one and remove it after nlmconv }
  xdcname := ChangeFileExt(current_module.exefilename,'.xdc');
  xdcpresent := FileExists (xdcname,false);
  if not xdcpresent then
  begin
    assign (f,xdcname);
    rewrite(f,1);
    if ioresult = 0 then
    begin
      blockwrite (f,xdc,sizeof(xdc));
      close(f);
      usexdc := (IOResult = 0);
    end else
      usexdc := false;
  end else
    usexdc := true;

  if usexdc then
    NLMConvLinkFile.Add('XDCDATA '+xdcname);

{ Call linker, this will generate a new object file that will be passed
  to nlmconv. Otherwise we could not create nlms without debug info }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename));
  Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$STRIP',StripStr);
  Replace(cmdstr,'$TMPOBJ',maybequoted(outputexedir+tmpLinkFileName));
  Comment (v_debug,'Executing '+BinStr+' '+cmdstr);
  success:=DoExec(BinStr,CmdStr,true,false);

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

{ Call nlmconv }
  if success then
  begin
    NLMConvLinkFile.writetodisk;
    NLMConvLinkFile.Free;
    SplitBinCmd(Info.ExeCmd[2],binstr,cmdstr);
    Replace(cmdstr,'$RES',maybequoted(outputexedir+'n'+Info.ResName));
    Comment (v_debug,'Executing '+BinStr+' '+cmdstr);
    success:=DoExec(BinStr,CmdStr,true,false);
    if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    begin
      DeleteFile(outputexedir+'n'+Info.ResName);
      DeleteFile(outputexedir+tmpLinkFileName);
      if not xdcpresent then
        if usexdc then
          DeleteFile (xdcname);
    end;
  end;

  MakeNetwareLoadableModule:=success;   { otherwise a recursive call to link method }
end;

function TLinkerNetwlibc.MakeExecutable:boolean;
begin
  MakeExecutable := MakeNetwareLoadableModule (false);
end;


function TLinkerNetwlibc.MakeSharedLibrary:boolean;
begin
  MakeSharedLibrary := MakeNetwareLoadableModule (true);
end;

{*****************************************************************************
                                     Initialize
*****************************************************************************}


initialization
  RegisterLinker(ld_netwlibc,TLinkerNetwlibc);
  RegisterImport(system_i386_netwlibc,TImportLibNetwlibc);
  RegisterExport(system_i386_netwlibc,TExportLibNetwlibc);
  RegisterTarget(system_i386_netwlibc_info);
end.

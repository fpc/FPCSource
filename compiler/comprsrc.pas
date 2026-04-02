{
    Copyright (c) 1998-2008 by Florian Klaempfl

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

  uses
    Systems, cstreams, cscript, compilerbase;

type
   tresoutput = (roRES, roOBJ);

   tresourcefile = class(TAbstractResourceFile)
   private
      FCompiler: TCompilerBase;
      fname : ansistring;
   protected
      function SetupCompilerArguments(output: tresoutput; const OutName :
      ansistring; respath: ansistring; out ObjUsed : boolean) : ansistring; virtual;
      property Compiler: TCompilerBase read FCompiler;
   public
      constructor Create(const fn : ansistring;acompiler: TCompilerBase);override;
      function Compile(output: tresoutput; const OutName: ansistring) : boolean; virtual;
      procedure PostProcessResourcefile(const s : ansistring);virtual;
      function IsCompiled(const fn : ansistring) : boolean;virtual;
      procedure Collect(const fn : ansistring);virtual;
      procedure EndCollect; virtual;
   end;

   TWinLikeResourceFile = class(tresourcefile)
   private
      fResScript : TScript;
      fScriptName : ansistring;
      fCollectCount : integer;
   protected
      function SetupCompilerArguments(output: tresoutput; const OutName :
        ansistring; respath: ansistring; out ObjUsed : boolean) : ansistring; override;
   public
      constructor Create(const fn : ansistring;acompiler: TCompilerBase);override;
      destructor Destroy; override;
      function Compile(output: tresoutput; const OutName: ansistring) : boolean; override;
      function IsCompiled(const fn : ansistring) : boolean;override;
      procedure Collect(const fn : ansistring);override;
      procedure EndCollect; override;
   end;

   TJVMRawResourceFile = class(TWinLikeResourceFile)
   private
   protected
   public
      function Compile(output: tresoutput; const OutName: ansistring) : boolean; override;
      function IsCompiled(const fn : ansistring) : boolean;override;
   end;


procedure CompileResourceFiles;
procedure CollectResourceFiles;

implementation

uses
  SysUtils,
  cutils,cfileutl,cclasses,
  Globtype,Globals,Verbose,Fmodule, comphook,cpuinfo,rescmn,compiler;

{****************************************************************************
                              TRESOURCEFILE
****************************************************************************}

constructor tresourcefile.create(const fn : ansistring;acompiler: TCompilerBase);
begin
  FCompiler:=acompiler;
  fname:=fn;
end;


procedure tresourcefile.PostProcessResourcefile(const s : ansistring);
begin
end;


function tresourcefile.IsCompiled(const fn: ansistring): boolean;
begin
  Result:=CompareText(ExtractFileExt(fn), compiler.target.info.resobjext) = 0;
end;

procedure tresourcefile.Collect(const fn: ansistring);
begin
  if fn='' then
    exit;
  fname:=fn;
  Compile(roOBJ, ChangeFileExt(fn, compiler.target.info.resobjext));
end;

procedure tresourcefile.EndCollect;
begin

end;

function tresourcefile.SetupCompilerArguments(output: tresoutput; const OutName
  : ansistring; respath: ansistring; out ObjUsed : boolean) : ansistring;
var
  s : TCmdStr;
begin
  if output=roRES then
    begin
      if compiler.globals.RCForceFPCRes then
        s:=FPCResRCArgs
      else
        s:=compiler.target.res.rccmd;
      Replace(s,'$RES',maybequoted(OutName));
      Replace(s,'$RC',maybequoted(fname));
      ObjUsed:=False;
    end
  else
    begin
      s:=compiler.target.res.rescmd;
      ObjUsed:=(pos('$OBJ',s)>0);
      Replace(s,'$OBJ',maybequoted(OutName));
      Replace(s,'$RES',maybequoted(fname));
    end;
  Result:=s;
end;

function tresourcefile.compile(output: tresoutput; const OutName: ansistring)
  : boolean;

  Function SelectBin(Const Bin1,Bin2 : String) : String;
  begin
    If (Bin1<>'') then
      SelectBin:=Bin1
    else
      SelectBin:=Bin2;
  end;

var
  respath,
  s,
  bin,
  resbin   : TCmdStr;
  resfound,
  objused  : boolean;
begin
  Result:=true;
  if output=roRES then
    if compiler.globals.RCForceFPCRes then
      Bin:=SelectBin(compiler.globals.RCCompiler,FPCResUtil)
    else
      Bin:=SelectBin(compiler.globals.RCCompiler,compiler.target.res.rcbin)
  else
    Bin:=SelectBin(compiler.globals.ResCompiler,compiler.target.res.resbin);
  if bin='' then
  begin
    Result:=false;
    exit;
  end;
  resfound:=false;
  if compiler.globals.utilsdirectory<>'' then
    resfound:=FindFile(compiler.globals.utilsprefix+bin+source_info.exeext,compiler.globals.utilsdirectory,false,resbin);
  if not resfound then
    begin
      resfound:=FindExe(compiler.globals.utilsprefix+bin,false,resbin);
      if not resfound and (compiler.globals.utilsprefix<>'') and ( (output=roRES) or (Pos('$ARCH', compiler.target.res.rescmd)<>0) ) then
        { Search for resource compiler without utilsprefix, if RC->RES compiler is called }
        { or RES->OBJ compiler supports different architectures. }
        resfound:=FindExe(bin,false,resbin);
    end;
  { get also the path to be searched for the windres.h }
  respath:=ExtractFilePath(resbin);
  if (not resfound) and not(cs_link_nolink in compiler.globals.current_settings.globalswitches) then
   begin
     compiler.verbose.Message1(exec_e_res_not_found, compiler.globals.utilsprefix+bin+source_info.exeext);
     compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_nolink];
     Result:=false;
   end;
  s:=SetupCompilerArguments(output,OutName,respath,objused);
{ Execute the command }
{ Always try to compile resources. but don't complain if cs_link_nolink }
  if resfound then
   begin
     compiler.verbose.Message1(exec_i_compilingresource,fname);
     compiler.verbose.Message2(exec_d_resbin_params,resbin,s);
     compiler.verbose.FlushOutput;
     try
       if RequotedExecuteProcess(resbin,s) <> 0 then
       begin
         if not (cs_link_nolink in compiler.globals.current_settings.globalswitches) then
           compiler.verbose.Message(exec_e_error_while_compiling_resources);
         compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_nolink];
         Result:=false;
       end;
     except
       on E:EOSError do
       begin
         if not (cs_link_nolink in compiler.globals.current_settings.globalswitches) then
           compiler.verbose.Message1(exec_e_cant_call_resource_compiler, resbin);
         compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_nolink];
         Result:=false;
       end
     end;
    end;
  { Update asmres when externmode is set and resource compiling failed }
  if (not Result) and (cs_link_nolink in compiler.globals.current_settings.globalswitches) then
    AsmRes.AddLinkCommand(resbin,s,OutName);
  if Result and (output=roOBJ) and ObjUsed then
    compiler.current_module.linkunitofiles.add(OutName,link_always);
end;

constructor TWinLikeResourceFile.Create(const fn : ansistring;acompiler: TCompilerBase);
begin
  inherited Create(fn,acompiler);
  fResScript:=nil;
  fCollectCount:=0;
  if (tf_use_8_3 in compiler.target.info.flags) then
    fScriptName:=ChangeFileExt(fn,'.rls')
  else
    fScriptName:=ChangeFileExt(fn,'.reslst');
end;

destructor TWinLikeResourceFile.Destroy;
begin
  if fResScript<>nil then
    fResScript.Free;
    fResScript := nil;
  inherited;
end;

function TWinLikeResourceFile.SetupCompilerArguments(output: tresoutput; const
  OutName : ansistring; respath : ansistring; out ObjUsed : boolean) : ansistring;
var
  srcfilepath,
  preprocessorbin,
  s : TCmdStr;
  arch,
  subarch: ansistring;

  function WindresFileName(filename: TCmdStr): TCmdStr;
  // to be on the safe side, for files that are passed to the preprocessor,
  // only give short file names with forward slashes to windres
  var
    i: longint;
  begin
    Result := GetShortName(filename);
    for I:=1 to Length(Result) do
    if Result[I] in AllowDirectorySeparators then
      Result[i]:='/';
    Result:=maybequoted(Result);
  end;

begin
  srcfilepath:=ExtractFilePath(compiler.current_module.mainsource);
  if output=roRES then
    begin
      if compiler.globals.RCForceFPCRes then
        s:=FPCResRCArgs
      else
        s:=compiler.target.res.rccmd;
      if (compiler.target.res.rcbin = 'windres') and not compiler.globals.RCForceFPCRes then
        Replace(s,'$RC',WindresFileName(fname))
      else
        Replace(s,'$RC',maybequoted(fname));
      Replace(s,'$RES',maybequoted(OutName));
      ObjUsed:=False;
    end
  else
    begin
      s:=compiler.target.res.rescmd;
      if (res_external_file in compiler.target.res.resflags) then
        ObjUsed:=false
      else
        ObjUsed:=(pos('$OBJ',s)>0);
      Replace(s,'$OBJ',maybequoted(OutName));
      subarch:='all';
      arch:=cpu2str[compiler.target.cpu];
      if (compiler.target.info.cpu=systems.cpu_arm) then
        begin
          //Differentiate between arm and armeb
          if (compiler.target.info.endian=endian_big) then
            arch:=arch+'eb';
        end;
      if compiler.target.info.cpu=cpu_powerpc64 then
        begin
          { differentiate between ppc64 and ppc64le }
          if compiler.target.info.endian=endian_little then
            arch:=arch+'le';
        end;
      Replace(s,'$ARCH',arch);
      if compiler.target.info.system=system_arm_ios then
        subarch:=lower(cputypestr[compiler.globals.current_settings.cputype]);
      Replace(s,'$SUBARCH',subarch);
      case compiler.target.info.endian of
        endian_little : Replace(s,'$ENDIAN','littleendian');
        endian_big : Replace(s,'$ENDIAN','bigendian');
      end;
      //call resource compiler with debug switch
      if (status.verbosity and V_Debug)<>0 then
        Replace(s,'$DBG','-v')
      else
        Replace(s,'$DBG','');
      if fCollectCount=0 then
        s:=s+' '+maybequoted(fname)
      else
        s:=s+' '+maybequoted('@'+fScriptName);
    end;
  { windres doesn't like empty include paths }
  if respath='' then
    respath:='.';
  Replace(s,'$INC',maybequoted(respath));
  if (output=roRes) and (compiler.target.res.rcbin='windres') and not compiler.globals.RCForceFPCRes then
  begin
    { try to find a preprocessor }
    preprocessorbin := respath+'cpp'+source_info.exeext;
    if FileExists(preprocessorbin,true) then
      s:='--preprocessor='+preprocessorbin+' '+s;
    if (srcfilepath<>'') then
      s:='--include '+WindresFileName(srcfilepath)+' '+s;
  end;
  Result:=s;
end;

function TWinLikeResourceFile.compile(output: tresoutput;
  const OutName: ansistring) : boolean;
begin
  Result:=inherited compile(output,OutName);
  //delete fpc-res.lst file if things went well
  if Result and (output=roOBJ) then
    DeleteFile(fScriptName);
end;

function TWinLikeResourceFile.IsCompiled(const fn: ansistring): boolean;
const
  ResSignature : array [1..32] of byte =
  ($00,$00,$00,$00,$20,$00,$00,$00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  knownexts : array[1..5] of string[4] = ('.lfm', '.dfm', '.xfm', '.fmx', '.tlb');
var
  f : file;
  oldfmode : byte;
  buf: array[1..32] of byte;
  i: longint;
  ext : shortstring;
begin
  ext:=lower(ExtractFileExt(fn));
  Result:=CompareText(ext, compiler.target.info.resext) = 0;
  if not Result then
    for i:=1 to high(knownexts) do
    begin
      Result:=CompareText(ext, knownexts[i]) = 0;
      if Result then break;
    end;

  if Result or not FileExists(fn, False) then exit;
  oldfmode:=Filemode;
  Filemode:=0;
  assign(f,fn);
  reset(f,1);
  BlockRead(f, buf, SizeOf(buf), i);
  close(f);
  Filemode:=oldfmode;

  if i<>SizeOf(buf) then
    exit;

  for i:=1 to 32 do
    if buf[i]<>ResSignature[i] then
      exit;

  Result:=True;
end;

procedure TWinLikeResourceFile.Collect(const fn: ansistring);
begin
  if fResScript=nil then
    fResScript:=TScript.Create(fScriptName,compiler);
  fResScript.Add(maybequoted_for_script(fn,script_fpcres));
  inc(fCollectCount);
end;

procedure TWinLikeResourceFile.EndCollect;
begin
  if fResScript<>nil then
  begin
    fResScript.WriteToDisk;
    FreeAndNil(fResScript);
    Compile(roOBJ,ChangeFileExt(fname,compiler.target.info.resobjext));
  end;
end;


{****************************************************************************
                              TJVMRawResourceFile
****************************************************************************}

function TJVMRawResourceFile.Compile(output: tresoutput; const OutName: ansistring): boolean;
  begin
    if output<>roOBJ then
      internalerror(2011081703);
    result:=inherited;
  end;


function TJVMRawResourceFile.IsCompiled(const fn: ansistring): boolean;
  begin
    internalerror(2011081704);
    result:=true;
  end;


function CopyResFile(inf,outf : TCmdStr) : boolean;
var
  compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
var
  src,dst : TCCustomFileStream;
begin
  { Copy .res file to units output dir. }
  Result:=false;
  src:=CFileStreamClass.Create(inf,fmOpenRead or fmShareDenyNone);
  if CStreamError<>0 then
    begin
      compiler.verbose.Message1(exec_e_cant_open_resource_file, src.FileName);
      compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_nolink];
      exit;
    end;
  dst:=CFileStreamClass.Create(compiler.current_module.outputpath+outf,fmCreate);
  if CStreamError<>0 then
    begin
      compiler.verbose.Message1(exec_e_cant_write_resource_file, dst.FileName);
      compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_nolink];
      exit;
    end;
  dst.CopyFrom(src,src.Size);
  dst.Free;
  dst := nil;
  src.Free;
  src := nil;
  Result:=true;
end;

procedure CompileResourceFiles;
var
  compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
var
  resourcefile : tresourcefile;
  res: TCmdStrListItem;
  p,s : TCmdStr;
  outfmt : tresoutput;
begin
  { Don't do anything for systems supporting resources without using resource
    file classes (e.g. Mac OS). They process resources elsewhere. }
  if ((compiler.target.info.res<>res_none) and (compiler.target.res.resourcefileclass=nil)) or
     (res_no_compile in compiler.target.res.resflags) then
    exit;

  p:=ExtractFilePath(ExpandFileName(compiler.current_module.mainsource));
  res:=TCmdStrListItem(compiler.current_module.ResourceFiles.First);
  while res<>nil do
    begin
      if compiler.target.info.res=res_none then
        compiler.verbose.Message(scan_e_resourcefiles_not_supported);
      s:=res.FPStr;
      if not path_absolute(s) then
        s:=p+s;
      if not FileExists(s, True) then
        begin
          compiler.verbose.Message1(exec_e_cant_open_resource_file, s);
          compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_nolink];
          exit;
        end;
      resourcefile:=TResourceFile(resinfos[compiler.target.info.res]^.resourcefileclass.create(s,compiler));
      if resourcefile.IsCompiled(s) then
        begin
          resourcefile.free;
          resourcefile := nil;
          if AnsiCompareFileName(IncludeTrailingPathDelimiter(ExpandFileName(compiler.current_module.outputpath)), p) <> 0 then
            begin
              { Copy .res file to units output dir. Otherwise .res file will not be found
                when only compiled units path is available }
              res.FPStr:=ExtractFileName(res.FPStr); //store file name only in PPU.
              if not CopyResFile(s,res.FPStr) then exit;
            end;
        end
      else
        begin
          res.FPStr:=ExtractFileName(res.FPStr);
          if (compiler.target.res.rcbin='') and (compiler.globals.RCCompiler='') then
            begin
              { if target does not have .rc to .res compiler, create obj }
              outfmt:=roOBJ;
              res.FPStr:=ChangeFileExt(res.FPStr,compiler.target.info.resobjext);
            end
          else
            begin
              outfmt:=roRES;
              res.FPStr:=ChangeFileExt(res.FPStr,compiler.target.info.resext);
            end;
          resourcefile.compile(outfmt, compiler.current_module.outputpath+res.FPStr);
          resourcefile.free;
          resourcefile := nil;
        end;
      res:=TCmdStrListItem(res.Next);
    end;
end;


procedure CollectResourceFiles;
var
  compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
var
  resourcefile : tresourcefile;

  procedure ProcessModule(u : tmodule);
  var
    res : TCmdStrListItem;
    s   : TCmdStr;
  begin
    res:=TCmdStrListItem(u.ResourceFiles.First);
    while assigned(res) do
      begin
        if path_absolute(res.FPStr) then
          s:=res.FPStr
        else
          begin
            s:=u.path+res.FPStr;
            if not FileExists(s,True) then
              s:=u.outputpath+res.FPStr;
          end;
        resourcefile.Collect(s);
        res:=TCmdStrListItem(res.Next);
      end;
  end;

var
  hp : tused_unit;
  s : TCmdStr;
begin
  if (compiler.target.info.res=res_none) or ((compiler.target.res.resbin='')
    and (compiler.globals.ResCompiler='')) then
      exit;
//  if cs_link_nolink in compiler.globals.current_settings.globalswitches then
//    exit;
  s:=ChangeFileExt(compiler.current_module.ppufilename,compiler.target.info.resobjext);
  if (res_arch_in_file_name in compiler.target.res.resflags) then
    s:=ChangeFileExt(s,'.'+cpu2str[compiler.target.cpu]+compiler.target.info.resobjext);
  resourcefile:=TResourceFile(resinfos[compiler.target.info.res]^.resourcefileclass.create(s,compiler));
  hp:=tused_unit(compiler.usedunits.first);
  while assigned(hp) do
    begin
      ProcessModule(hp.u);
      hp:=tused_unit(hp.next);
    end;
  ProcessModule(compiler.current_module);
  { Finish collection }
  resourcefile.EndCollect;
  resourcefile.free;
  resourcefile := nil;
end;

procedure initglobals(ACompilerGlobals: TCompilerGlobals);
begin
  ACompilerGlobals.ResCompiler:='';
  ACompilerGlobals.RCCompiler:='';
  ACompilerGlobals.RCForceFPCRes:=false;
end;

initialization
  register_initdone_proc(@initglobals,nil);
end.

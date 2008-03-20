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
    Systems, cstreams, Script;

type
   tresoutput = (roRES, roOBJ);

   tresourcefile = class(TAbstractResourceFile)
   private
      fname : ansistring;
   protected
      function SetupCompilerArguments(output: tresoutput; const OutName :
      ansistring; respath: ansistring; out ObjUsed : boolean) : ansistring; virtual;
   public
      constructor Create(const fn : ansistring);override;
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
      constructor Create(const fn : ansistring);override;
      destructor Destroy; override;
      function Compile(output: tresoutput; const OutName: ansistring) : boolean; override;
      function IsCompiled(const fn : ansistring) : boolean;override;
      procedure Collect(const fn : ansistring);override;
      procedure EndCollect; override;
   end;

procedure CompileResourceFiles;
procedure CollectResourceFiles;

Var
  ResCompiler : String;
  RCCompiler  : String;

implementation

uses
  SysUtils,
  cutils,cfileutl,cclasses,
  Globtype,Globals,Verbose,Fmodule, comphook;

{****************************************************************************
                              TRESOURCEFILE
****************************************************************************}

constructor tresourcefile.create(const fn : ansistring);
begin
  fname:=fn;
end;


procedure tresourcefile.PostProcessResourcefile(const s : ansistring);
begin
end;


function tresourcefile.IsCompiled(const fn: ansistring): boolean;
begin
  Result:=CompareText(ExtractFileExt(fn), target_info.resobjext) = 0;
end;

procedure tresourcefile.Collect(const fn: ansistring);
begin
  if fn='' then
    exit;
  fname:=fn;
  Compile(roOBJ, ChangeFileExt(fn, target_info.resobjext));
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
      s:=target_res.rccmd;
      Replace(s,'$RES',maybequoted(OutName));
      Replace(s,'$RC',maybequoted(fname));
      ObjUsed:=False;
    end
  else
    begin
      s:=target_res.rescmd;
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
    Bin:=SelectBin(RCCompiler,target_res.rcbin)
  else
    Bin:=SelectBin(ResCompiler,target_res.resbin);
  if bin='' then
  begin
    Result:=false;
    exit;
  end;
  resfound:=false;
  if utilsdirectory<>'' then
    resfound:=FindFile(utilsprefix+bin+source_info.exeext,utilsdirectory,false,resbin);
  if not resfound then
    resfound:=FindExe(utilsprefix+bin,false,resbin);
  { get also the path to be searched for the windres.h }
  respath:=ExtractFilePath(resbin);
  if (not resfound) and not(cs_link_nolink in current_settings.globalswitches) then
   begin
     Message1(exec_e_res_not_found, bin);
     current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
     Result:=false;
   end;
  s:=SetupCompilerArguments(output,OutName,respath,objused);
{ Execute the command }
{ Always try to compile resources. but don't complain if cs_link_nolink }
  if resfound then
   begin
     Message1(exec_i_compilingresource,fname);
     Message2(exec_d_resbin_params,resbin,s);
     FlushOutput;
     try
       if ExecuteProcess(resbin,s) <> 0 then
       begin
         if not (cs_link_nolink in current_settings.globalswitches) then
           Message(exec_e_error_while_compiling_resources);
         current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
         Result:=false;
       end;
     except
       on E:EOSError do
       begin
         if not (cs_link_nolink in current_settings.globalswitches) then
           Message1(exec_e_cant_call_resource_compiler, resbin);
         current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
         Result:=false;
       end
     end;
    end;
  { Update asmres when externmode is set and resource compiling failed }
  if (not Result) and (cs_link_nolink in current_settings.globalswitches) then
    AsmRes.AddLinkCommand(resbin,s,OutName);
  if Result and (output=roOBJ) and ObjUsed then
    current_module.linkunitofiles.add(OutName,link_always);
end;

constructor TWinLikeResourceFile.Create(const fn : ansistring);
begin
  inherited Create(fn);
  fResScript:=nil;
  fCollectCount:=0;
  if (tf_use_8_3 in target_info.flags) then
    fScriptName:=ChangeFileExt(fn,'.rls')
  else
    fScriptName:=ChangeFileExt(fn,'.reslst');
end;

destructor TWinLikeResourceFile.Destroy;
begin
  if fResScript<>nil then
    fResScript.Free;
  inherited;
end;

function TWinLikeResourceFile.SetupCompilerArguments(output: tresoutput; const
  OutName : ansistring; respath : ansistring; out ObjUsed : boolean) : ansistring;
var
  srcfilepath,
  preprocessorbin,
  s : TCmdStr;
  arch : ansistring;
begin
  srcfilepath:=ExtractFilePath(current_module.mainsource^);
  if output=roRES then
    begin
      s:=target_res.rccmd;
      Replace(s,'$RES',maybequoted(OutName));
      Replace(s,'$RC',maybequoted(fname));
      ObjUsed:=False;
    end
  else
    begin
      s:=target_res.rescmd;
      if (res_external_file in target_res.resflags) then
        ObjUsed:=false
      else
        ObjUsed:=(pos('$OBJ',s)>0);
      Replace(s,'$OBJ',maybequoted(OutName));
      arch:=cpu2str[target_cpu];
      //Differentiate between arm and armeb
      if (source_info.cpu=cpu_arm) and (source_info.endian=endian_big) then
        arch:=arch+'eb';
      Replace(s,'$ARCH',arch);
      case target_info.endian of
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
        s:=s+' @'+fScriptName;
    end;
  { windres doesn't like empty include paths }
  if respath='' then
    respath:='.';
  Replace(s,'$INC',maybequoted(respath));
  if (output=roRes) and (target_res.rcbin='windres') then
  begin
    if (srcfilepath<>'') then
      s:=s+' --include '+maybequoted(srcfilepath);
    { try to find a preprocessor }
    preprocessorbin := respath+'cpp'+source_info.exeext;
    if FileExists(preprocessorbin,true) then
      s:=s+' --preprocessor='+preprocessorbin;
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
  dfmexts : array[1..3] of string[4] = ('.lfm', '.dfm', '.xfm');
var
  f : file;
  oldfmode : byte;
  buf: array[1..32] of byte;
  i: longint;
  ext : shortstring;
begin
  ext:=lower(ExtractFileExt(fn));
  Result:=CompareText(ext, target_info.resext) = 0;
  if not Result then
    for i:=1 to high(dfmexts) do
    begin
      Result:=CompareText(ext, dfmexts[i]) = 0;
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
    fResScript:=TScript.Create(fScriptName);
  fResScript.Add(MaybeQuoted(fn));
  inc(fCollectCount);
end;

procedure TWinLikeResourceFile.EndCollect;
begin
  if fResScript<>nil then
  begin
    fResScript.WriteToDisk;
    FreeAndNil(fResScript);
    Compile(roOBJ,ChangeFileExt(fname,target_info.resobjext));
  end;
end;


function CopyResFile(inf,outf : TCmdStr) : boolean;
var
  src,dst : TCFileStream;
begin
  { Copy .res file to units output dir. }
  Result:=false;
  src:=TCFileStream.Create(inf,fmOpenRead or fmShareDenyNone);
  if CStreamError<>0 then
    begin
      Message1(exec_e_cant_open_resource_file, src.FileName);
      Include(current_settings.globalswitches, cs_link_nolink);
      exit;
    end;
  dst:=TCFileStream.Create(current_module.outputpath^+outf,fmCreate);
  if CStreamError<>0 then
    begin
      Message1(exec_e_cant_write_resource_file, dst.FileName);
      Include(current_settings.globalswitches, cs_link_nolink);
      exit;
    end;
  dst.CopyFrom(src,src.Size);
  dst.Free;
  src.Free;
  Result:=true;
end;
 
procedure CompileResourceFiles;
var
  resourcefile : tresourcefile;
  res: TCmdStrListItem;
  p,s : TCmdStr;
  outfmt : tresoutput;
begin
  { Don't do anything for systems supporting resources without using resource
    file classes (e.g. Mac OS). They process resources elsewhere. }
  if (target_info.res<>res_none) and (target_res.resourcefileclass=nil) then
    exit;

  p:=ExtractFilePath(current_module.mainsource^);
  res:=TCmdStrListItem(current_module.ResourceFiles.First);
  while res<>nil do
    begin
      if target_info.res=res_none then
        Message(scan_e_resourcefiles_not_supported);
      s:=res.FPStr;
      if not path_absolute(s) then
        s:=p+s;
      if not FileExists(s, True) then
        begin
          Message1(exec_e_cant_open_resource_file, s);
          Include(current_settings.globalswitches, cs_link_nolink);
          exit;
        end;
      resourcefile:=TResourceFile(resinfos[target_info.res]^.resourcefileclass.create(s));
      if resourcefile.IsCompiled(s) then
        begin
          resourcefile.free;
          if AnsiCompareText(current_module.outputpath^, p) <> 0 then
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
          if (target_res.rcbin='') and (RCCompiler='') then
            begin
              { if target does not have .rc to .res compiler, create obj }
              outfmt:=roOBJ;
              res.FPStr:=ChangeFileExt(res.FPStr,target_info.resobjext);
            end
          else
            begin
              outfmt:=roRES;
              res.FPStr:=ChangeFileExt(res.FPStr,target_info.resext);
            end;
          resourcefile.compile(outfmt, current_module.outputpath^+res.FPStr);
          resourcefile.free;
        end;
      res:=TCmdStrListItem(res.Next);
    end;
end;


procedure CollectResourceFiles;
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
            s:=u.path^+res.FPStr;
            if not FileExists(s,True) then
              s:=u.outputpath^+res.FPStr;
          end;
        resourcefile.Collect(s);
        res:=TCmdStrListItem(res.Next);
      end;
  end;
  
var
  hp : tused_unit;
  s : TCmdStr;
begin
  if (target_info.res=res_none) or ((target_res.resbin='')
    and (ResCompiler='')) then
      exit;
//  if cs_link_nolink in current_settings.globalswitches then
//    exit;
  s:=ChangeFileExt(current_module.ppufilename^,target_info.resobjext);
  if (res_arch_in_file_name in target_res.resflags) then
    s:=ChangeFileExt(s,'.'+cpu2str[target_cpu]+target_info.resobjext);
  resourcefile:=TResourceFile(resinfos[target_info.res]^.resourcefileclass.create(s));
  hp:=tused_unit(usedunits.first);
  while assigned(hp) do
    begin
      ProcessModule(hp.u);
      hp:=tused_unit(hp.next);
    end;
  ProcessModule(current_module);
  { Finish collection }
  resourcefile.EndCollect;
  resourcefile.free;
end;

end.

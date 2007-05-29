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

  uses
    Systems, cstreams;

type
   tresoutput = (roRES, roOBJ);

   tresourcefile = class(TAbstractResourceFile)
   private
      fname : ansistring;
   public
      constructor Create(const fn : ansistring);override;
      procedure Compile(output: tresoutput; const OutName: ansistring);virtual;
      procedure PostProcessResourcefile(const s : ansistring);virtual;
      function IsCompiled(const fn : ansistring) : boolean;virtual;
      procedure Collect(const fn : ansistring);virtual;
   end;
   
   TWinLikeResourceFile = class(tresourcefile)
   private
      FOut: TCFileStream;
   public
      function IsCompiled(const fn : ansistring) : boolean;override;
      procedure Collect(const fn : ansistring);override;
   end;

procedure CompileResourceFiles;
procedure CollectResourceFiles;


implementation

uses
  SysUtils,
  cutils,cfileutils,cclasses,
  Globtype,Globals,Verbose,Fmodule,
  Script;
  
const
  GlobalResName = 'fpc-res';

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


procedure tresourcefile.compile(output: tresoutput; const OutName: ansistring);
var
  respath,
  srcfilepath,
  s,
  bin,
  resbin   : TCmdStr;
  resfound,
  objused  : boolean;
begin
  if output=roRES then
    bin:=target_res.rcbin
  else
    bin:=target_res.resbin;
  if bin='' then
    exit;
  resfound:=false;
  if utilsdirectory<>'' then
    resfound:=FindFile(utilsprefix+bin+source_info.exeext,utilsdirectory,false,resbin);
  if not resfound then
    resfound:=FindExe(utilsprefix+bin,false,resbin);
  { get also the path to be searched for the windres.h }
  respath:=ExtractFilePath(resbin);
  if (not resfound) and not(cs_link_nolink in current_settings.globalswitches) then
   begin
     Message(exec_e_res_not_found);
     current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
   end;
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
      ObjUsed:=(pos('$OBJ',s)>0);
      Replace(s,'$OBJ',maybequoted(OutName));
      Replace(s,'$RES',maybequoted(fname));
    end;
  { windres doesn't like empty include paths }
  if respath='' then
    respath:='.';
  Replace(s,'$INC',maybequoted(respath));
  if (target_res.resbin='windres') and
     (srcfilepath<>'') then
    s:=s+' --include '+maybequoted(srcfilepath);
{ Execute the command }
  if not (cs_link_nolink in current_settings.globalswitches) then
   begin
     Message1(exec_i_compilingresource,fname);
     Message2(exec_d_resbin_params,resbin,s);
     FlushOutput;
     try
       if ExecuteProcess(resbin,s) <> 0 then
       begin
         Message(exec_e_error_while_linking);
         current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
       end;
     except
       on E:EOSError do
       begin
         Message(exec_e_cant_call_linker);
         current_settings.globalswitches:=current_settings.globalswitches+[cs_link_nolink];
       end
     end;
    end;
  if output=roOBJ then
    PostProcessResourcefile(OutName);
  { Update asmres when externmode is set }
  if cs_link_nolink in current_settings.globalswitches then
    AsmRes.AddLinkCommand(resbin,s,'');
  if (output=roOBJ) and ObjUsed then
    current_module.linkunitofiles.add(OutName,link_always);
end;


function TWinLikeResourceFile.IsCompiled(const fn: ansistring): boolean;
const
  ResSignature : array [1..32] of byte =
  ($00,$00,$00,$00,$20,$00,$00,$00,$FF,$FF,$00,$00,$FF,$FF,$00,$00,
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
var
  f : file;
  oldfmode : byte;
  buf: array[1..32] of byte;
  i: longint;
begin
  Result:=CompareText(ExtractFileExt(fn), target_info.resext) = 0;
  if Result then exit;
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
const
  zeroes: array[1..3] of byte = (0,0,0);
var
  fs: TCFileStream;
  i: longint;
begin
  if fn='' then
    begin
      if FOut<>nil then
        begin
          FOut.Free;
          Compile(roOBJ,ChangeFileExt(fname,target_info.resobjext));
        end;
    end
  else
    begin
      fs:=TCFileStream.Create(fn,fmOpenRead or fmShareDenyNone);
      if CStreamError<>0 then
        begin
          fs.Free;
          Comment(V_Error,'Can''t open resource file: '+fn);
          exit;
        end;
      if FOut=nil then
        FOut:=TCFileStream.Create(fname,fmCreate)
      else
        fs.Seek(32, soFromBeginning);
      FOut.CopyFrom(fs, fs.Size-fs.Position);
      fs.Free;
      { align resource to dword }
      i:=4 - FOut.Position mod 4;
      if i<4 then
        FOut.WriteBuffer(zeroes, i);
    end;
end;


procedure CompileResourceFiles;
var
  resourcefile : tresourcefile;
  res: TCmdStrListItem;
  p,s : TCmdStr;
  src,dst : TCFileStream;
  outfmt : tresoutput;
begin
  { OS/2 (EMX) must be processed elsewhere (in the linking/binding stage).
    same with MacOS}
  if target_info.system in [system_i386_os2,system_i386_emx,system_powerpc_macos] then exit;

  p:=ExtractFilePath(current_module.mainsource^);
  res:=TCmdStrListItem(current_module.ResourceFiles.First);
  while res<>nil do
    begin
      if target_info.res=res_none then
        Message(scan_e_resourcefiles_not_supported);
      s:=res.FPStr;
      if not path_absolute(s) then
        s:=p+s;
      resourcefile:=TResourceFile(resinfos[target_info.res]^.resourcefileclass.create(s));
      if resourcefile.IsCompiled(s) then
        begin
          resourcefile.free;
          if CompareText(current_module.outputpath^, p) <> 0 then
            begin
              { Copy .res file to units output dir }
              res.FPStr:=ExtractFileName(res.FPStr);
              src:=TCFileStream.Create(s,fmOpenRead or fmShareDenyNone);
              if CStreamError<>0 then
                begin
                  Comment(V_Error,'Can''t open resource file: '+src.FileName);
                  exit;
                end;
              dst:=TCFileStream.Create(current_module.outputpath^+res.FPStr,fmCreate);
              if CStreamError<>0 then
                begin
                  Comment(V_Error,'Can''t create resource file: '+dst.FileName);
                  exit;
                end;
              dst.CopyFrom(src,src.Size);
              dst.Free;
              src.Free;
            end;
        end
      else
        begin
          res.FPStr:=ExtractFileName(res.FPStr);
          if target_res.rcbin='' then
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
  if (target_info.res=res_none) or (target_res.rcbin='') then
    exit;
  s:=main_module.outputpath^+GlobalResName+target_info.resext;
  resourcefile:=TResourceFile(resinfos[target_info.res]^.resourcefileclass.create(s));
  hp:=tused_unit(usedunits.first);
  while assigned(hp) do
    begin
      ProcessModule(hp.u);
      hp:=tused_unit(hp.next);
    end;
  ProcessModule(current_module);
  { Finish collection }
  resourcefile.Collect('');
  resourcefile.free;
end;

end.

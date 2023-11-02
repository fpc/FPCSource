{
    Copyright (c) 2000-2002 by Florian Klaempfl

    This file is the "loader" for the Free Pascal compiler

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

 ****************************************************************************}
program fpc;

{$mode objfpc}{$H+}

  uses
     Sysutils;

  const
{$ifdef UNIX}
    exeext='';
{$else UNIX}
  {$ifdef HASAMIGA}
    exeext='';
  {$else}
    {$ifdef NETWARE}
      exeext='.nlm';
    {$else}
      {$ifdef ATARI}
        exeext='.ttp';
      {$else}
        exeext='.exe';
      {$endif ATARI}
    {$endif NETWARE}
  {$endif HASAMIGA}
{$endif UNIX}

Const
{$ifdef darwin}
  { the mach-o format supports "fat" binaries whereby }
  { a single executable contains machine code for     }
  { several architectures -> it is counter-intuitive  }
  { and non-standard to use different binary names    }
  { for cross-compilers vs. native compilers          }
  CrossSuffix = '';
{$else not darwin}
  CrossSuffix = 'ross';
{$endif not darwin}


  procedure error(const s : string);

  begin
     writeln('Error: ',s);
     halt(1);
  end;

  function processortosuffix(processorstr : string ) : String;

  begin
    case processorstr of
      'aarch64': Result := 'a64';
      'arm': Result := 'arm';
      'avr': Result := 'avr';
      'i386': Result := '386';
      'i8086': Result := '8086';
      'jvm': Result := 'jvm';  
      'loongarch64': Result:='loongarch64';
      'm68k': Result := '68k';
      'mips': Result := 'mips';
      'mipsel': Result := 'mipsel';
      'powerpc': Result := 'ppc';
      'powerpc64': Result := 'ppc64';
      'riscv32': Result := 'rv32';
      'riscv64': Result := 'rv64';
      'sparc': Result := 'sparc';
      'sparc64': Result := 'sparc64';
      'x86_64': Result := 'x64';
      'xtensa': Result := 'xtensa';
      'z80': Result := 'z80';
      'wasm32': Result := 'wasm32'
      else
        error('Illegal processor type "'+processorstr+'"');
    end;
  end;

  procedure InitPlatform(out ppcbin,processorname : string);

  begin
    {$ifdef i386}
         ppcbin:='ppc386';
         processorname:='i386';
    {$endif i386}
    {$ifdef m68k}
         ppcbin:='ppc68k';
         processorname:='m68k';
    {$endif m68k}
    {$ifdef powerpc}
         ppcbin:='ppcppc';
         processorname:='powerpc';
    {$endif powerpc}
    {$ifdef powerpc64}
         ppcbin:='ppcppc64';
         processorname:='powerpc64';
    {$endif powerpc64}
    {$ifdef arm}
         ppcbin:='ppcarm';
         processorname:='arm';
    {$endif arm}
    {$ifdef aarch64}
         ppcbin:='ppca64';
         processorname:='aarch64';
    {$endif aarch64}
    {$ifdef sparc}
         ppcbin:='ppcsparc';
         processorname:='sparc';
    {$endif sparc}
    {$ifdef sparc64}
         ppcbin:='ppcsparc64';
         processorname:='sparc64';
    {$endif sparc64}
    {$ifdef x86_64}
         ppcbin:='ppcx64';
         processorname:='x86_64';
    {$endif x86_64}
    {$ifdef mipsel}
         ppcbin:='ppcmipsel';
         processorname:='mipsel';
    {$else : not mipsel}
      {$ifdef mips}
         ppcbin:='ppcmips';
         processorname:='mips';
      {$endif mips}
    {$endif not mipsel}
    {$ifdef riscv32}
         ppcbin:='ppcrv32';
         processorname:='riscv32';
    {$endif riscv32}
    {$ifdef riscv64}
         ppcbin:='ppcrv64';
         processorname:='riscv64';
    {$endif riscv64}
    {$ifdef xtensa}
         ppcbin:='ppcxtensa';
         processorname:='xtensa';
    {$endif xtensa}
    {$ifdef wasm32}
         ppcbin:='ppcwasm32';
         processorname:='wasm32';
    {$endif wasm32}
    {$ifdef loongarch64}
         ppcbin:='ppcloongarch64';
         processorname:='loongarch64';
    {$endif loongarch64}
  end;

  function SplitPath(Const HStr:String):String;
    var
      i : longint;
    begin
      i:=Length(Hstr);
      while (i>0) and not(Hstr[i] in ['\','/']) do
       dec(i);
      SplitPath:=Copy(Hstr,1,i);
    end;


  function FileExists ( Const F : String) : Boolean;
    var
      Info : TSearchRec;
    begin
      FileExists:= findfirst(F,fareadonly+faarchive+fahidden,info)=0;
      findclose(Info);
    end;

  var
    warn : Boolean;
    ShowErrno : Boolean;
    extrapath : ansistring;

  function findexe(var ppcbin:string): boolean;
    var
      path : string;
    begin
      { add .exe extension }
      findexe:=false;
      ppcbin:=ppcbin+exeext;

      if (extrapath<>'') and (extrapath[length(extrapath)]<>DirectorySeparator) then
        extrapath:=extrapath+DirectorySeparator;
      { get path of fpc.exe }
      path:=splitpath(paramstr(0));
      { don't try with an empty extra patch, this might have strange results
        if the current directory contains a compiler
      }
      if (extrapath<>'') and FileExists(extrapath+ppcbin) then
       begin
         ppcbin:=extrapath+ppcbin;
         findexe:=true;
       end
      else if (path<>'') and FileExists(path+ppcbin) then
       begin
         ppcbin:=path+ppcbin;
         findexe:=true;
       end
      else
       begin
         path:=ExeSearch(ppcbin,getenvironmentvariable('PATH'));
         if path<>'' then
          begin
            ppcbin:=path;
            findexe:=true;
          end
       end;
    end;

    function findcompiler(basecompiler,cpusuffix,exesuffix : string) : string;

    begin
      Result:=basecompiler;
      if exesuffix<>'' then
        Result:=Result+'-'+exesuffix;
      if not findexe(Result) then
        begin
          if cpusuffix<>'' Then
            begin
              Result:='ppc'+cpusuffix;
              if exesuffix<>'' then
                result:=result+'-'+exesuffix;
              if not findexe(result) then
                result:='';
            end;
        end;
    end;

    procedure CheckSpecialProcessors(processorstr,processorname,ppcbin,cpusuffix,exesuffix : string);

    begin
      { -PB is a special code that will show the
        default compiler and exit immediately. It's
        main usage is for Makefile }
      if processorstr='B' then
        begin
          { report the full name of the ppcbin }
          writeln(findcompiler(ppcbin,cpusuffix,exesuffix));
          halt(0);
        end;
      { -PP is a special code that will show the
         processor and exit immediately. It's
         main usage is for Makefile }
      if processorstr='P' then
        begin
          { report the processor }
          writeln(processorname);
          halt(0);
        end;
      end;

Function FindConfigFile(const aFile : string; const aCompiler : String) : String;
// Adapted from check_configfile(fn:string; var foundfn:string):boolean;
{
  Order to read configuration file :
  Unix:
   1 - current dir
   2 - ~/.fpc.cfg
   3 - configpath
   4 - /etc
  Windows:
   1 - current dir
   2 - home dir of user or all users
   3 - config path
   4 - next to binary
  Other:
   1 - current dir
   3 - config path
   4 - next to binary
}

var
  {$ifdef unix}sl : rawbytestring;{$endif}
  {$ifdef unix}hs,{$endif} aSearchPath,exepath,configpath : string;

  Procedure AddToPath(aDir : String);

  begin
    if aDir='' then exit;
    if (aSearchPath<>'') then
      aSearchPath:=aSearchPath+PathSeparator;
    aSearchPath:=aSearchPath+IncludeTrailingPathDelimiter(SetDirSeparators(aDir));
  end;

begin
  if FileExists(aFile) then
    Exit(aFile);
  ExePath:=SetDirSeparators(ExtractFilePath(paramstr(0)));
  aSearchPath:='';
  { retrieve configpath }
  configpath:=SetDirSeparators(GetEnvironmentVariable('PPC_CONFIG_PATH'));
{$ifdef Unix}
  hs:=SetDirSeparators(GetEnvironmentVariable('HOME'));
  if (hs<>'') then
    begin
    Result:=IncludeTrailingPathDelimiter(hs)+'.'+aFile;
    if FileExists(Result) then
      exit;
    end;
  if configpath='' then
    begin
    {  
      We need to search relative to compiler binary, not relative to FPC binary.
      Beware of symlinks !
    }
    hs:=aCompiler;
    While FileGetSymLinkTarget(hs,sl) do
      begin
      if copy(sl,1,1)<>'/' then
        hs:=ExpandFileName(ExtractFilePath(hs)+sl)
      else
        hs:=sl;  
      end;
    ExePath:=ExtractFilePath(hs);
    configpath:=ExpandFileName(ExePath+'../etc/');
    end;
{$endif}
  AddToPath(ConfigPath);
{$ifdef WINDOWS}
  AddToPath(GetEnvironmentVariable('USERPROFILE'));
  AddToPath(GetEnvironmentVariable('ALLUSERSPROFILE'));
{$endif WINDOWS}
{$ifdef Unix}
  AddToPath('/etc/');
{$else}
  AddToPath(exepath);
{$endif}
  Result:=FileSearch(aFile,aSearchPath);
end;

Procedure CheckWarn(aOpt : String);

Var
  Len,I : integer;

begin
  Len:=Length(aOpt);
  For I:=1 to Len do
    begin
    if (aOpt[i]='w') then
      Warn:=(I=Len) or (aOpt[i+1]<>'-');
    if (aOpt[i]='q') then
      ShowErrNo:=(I=Len) or (aOpt[i+1]<>'-');
    end;
end;

procedure SetExeSuffix(var ExeSuffix : string; aValue : string);

begin
  if ExeSuffix='' then
    exesuffix :=aValue
  else if Warn then
    begin
    Write('Warning: ');
    if ShowErrNo then
      Write('(99999) ');
    Writeln('Compiler version already set to: ',ExeSuffix);
    end;
end;

Procedure ProcessConfigFile(aFileName : String; var ExeSuffix : String);

  Function Stripline(aLine : String) : string;

  Var
    P : integer;

  begin
    if (aLine<>'') and (aLine[1]=';') then exit;
    Pos('#',aLine); // no ifdef or include.
    if P=0 then
      P:=Length(aLine)+1;
    Result:=Copy(aLine,1,P-1);
  end;

Var
  aFile : Text;
  aLine : String;

begin
  Assign(aFile,aFileName);
  {$push}{$I-}
  filemode:=0;
  Reset(aFile);
  {$pop}
  if ioresult<>0 then
    Error('Cannot open config file: '+aFileName);
  While not EOF(aFile) do
    begin
    ReadLn(aFile,aLine);
    aLine:=StripLine(aLine);
    if aLine='' then
      continue;
    if Copy(aLine,1,2)='-V' then
      SetExeSuffix(ExeSuffix,Copy(aLine,3,Length(aLine)-2));
    end;
  {$i+}
  Close(aFile);
end;

  var
     s,CfgFile: ansistring;
     CPUSuffix, ExeSuffix, SourceCPU, ppcbin, TargetName, TargetCPU: string;
     PPCCommandLine: array of ansistring;
     PPCCommandLineLen: longint;
     i : longint;
     errorvalue     : Longint;

     Procedure AddToCommandLine(S : String);

     begin
       PPCCommandLine [PPCCommandLineLen] := S;
       Inc(PPCCommandLineLen);
     end;

begin
  ppccommandline := [];
  setlength(ppccommandline, paramcount);
  ppccommandlinelen := 0;
  cpusuffix := '';        // if not empty, signals attempt at cross
  // compiler.
  extrapath := '';
  initplatform(ppcbin, SourceCPU);
  exesuffix := '';                      { Default is just the name }
  if ParamCount = 0 then
  begin
    SetLength(PPCCommandLine, 1);
    AddToCommandLine('-?F'+ParamStr(0));
  end
  else
    for i := 1 to paramcount do
    begin
      s := ParamStr(i);
      if pos('-t', s) = 1 then
      begin
        targetname := copy(s, 3, length(s)-2);
        AddToCommandLine(S);
      end
      else if pos('-V', s) = 1 then
        SetExeSuffix(ExeSuffix,copy(s, 3, length(s)-2))
      else
      begin
        if pos('-P', s) = 1 then
           begin
             TargetCPU:=copy(s,3,length(s)-2);
             CheckSpecialProcessors(TargetCPU,SourceCPU,ppcbin,cpusuffix,exesuffix);
             if TargetCPU <> SourceCPU then
               begin
                 cpusuffix:=processortosuffix(TargetCPU);
                 ppcbin:='ppc'+crosssuffix+cpusuffix;
               end;
           end
        else if pos('-Xp',s)=1 then
          extrapath:=copy(s,4,length(s)-3)
        else
        begin
          if pos('-h', s) = 1 then
            AddToCommandLine('-hF'+ParamStr(0))
          else if pos('-?', s) = 1 then
            AddToCommandLine('-?F'+ParamStr(0))
          else
            begin
            AddToCommandLine(S);
            if pos('-v', s) = 1 then
              CheckWarn(Copy(S,3,length(S)-2));
            end;
        end;
      end;
    end;
     ppcbin := findcompiler(ppcbin, cpusuffix, exesuffix);
     if (TargetName<>'') then
       begin
       S:='fpc-'+lowercase(TargetName)+'.cfg';
       CfgFile:=FindConfigFile(s,ppcbin);
       if CfgFile='' then
         Error('Cannot find subtarget config file: '+s);
       ProcessConfigFile(CfgFile,ExeSuffix);
       end;
     SetLength(ppccommandline, ppccommandlinelen);

     { call ppcXXX }
     try
       errorvalue:=ExecuteProcess(ppcbin,ppccommandline);
     except
       on e : exception do
         error(ppcbin+' can''t be executed, error message: '+e.message);
     end;
     if (errorvalue<>0) and
        (paramcount<>0) then
       error(ppcbin+' returned an error exitcode');
     halt(errorvalue);
  end.


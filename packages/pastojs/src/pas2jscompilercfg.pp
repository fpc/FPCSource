{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Michael Van Canneyt

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    Config file handling for compiler, depends on filesystem.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit Pas2JSCompilerCfg;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  {$IFDEF NodeJS}
  node.fs,
  {$ENDIF}
  System.SysUtils, System.Classes, Pas2Js.Files.Utils, Pas2Js.Files.Fs, Pas2Js.Compiler.Base;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$IFDEF NodeJS}
  node.fs,
  {$ENDIF}
  SysUtils, Classes, Pas2jsFileUtils, Pas2JSFS, Pas2jsCompiler;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TPas2JSFileConfigSupport = Class(TPas2JSConfigSupport)
    function FindDefaultConfig: String; override;
    function GetReader(aFileName: string): TSourceLineReader; override;
  end;

implementation

function TPas2JSFileConfigSupport.GetReader(aFileName: string): TSourceLineReader;

Var
  CacheFile: TPas2jsFile;

begin
  CacheFile:=Compiler.FS.LoadFile(aFilename);
  Result:=CacheFile.CreateLineReader(true);
end;

Function TPas2JSFileConfigSupport.FindDefaultConfig : String;
var
  Tried: TStringList;

  function TryConfig(aFilename: string): boolean;
  begin
    Result:=false;
    if aFilename='' then exit;
    aFilename:=ExpandFileName(aFilename);
    if Tried.IndexOf(aFilename)>=0 then exit;
    Tried.Add(aFilename);
    if Compiler.ShowDebug or Compiler.ShowTriedUsedFiles then
      Compiler.Log.LogMsgIgnoreFilter(nConfigFileSearch,[aFilename]);
    if not Compiler.FS.FileExists(aFilename) then exit;
    FindDefaultConfig:=aFilename;
    Result:=true;
  end;

var
  aFilename: String;

begin
  Result:='';
  Tried:=TStringList.Create;
  try
    // first try HOME directory
    aFilename:=ChompPathDelim(GetEnvironmentVariablePJ('HOME'));
    if aFilename<>'' then
      begin
      aFilename:=aFilename+PathDelim{$IFDEF UNIX}+'.'{$ENDIF}+DefaultConfigFile;
      if TryConfig(aFileName) then
        exit;
      end;

    // then try compiler directory
    if (Compiler.CompilerExe<>'') then
    begin
      aFilename:=ExtractFilePath(Compiler.CompilerExe);
      if aFilename<>'' then
      begin
        aFilename:=IncludeTrailingPathDelimiter(aFilename)+DefaultConfigFile;
        if TryConfig(aFilename) then
          exit;
      end;
      // resolve symlinks and then search
      aFilename:=GetPhysicalFilename(Compiler.CompilerExe,false);
      if (aFilename<>'') and (aFilename<>Compiler.CompilerExe) then
      begin
        aFilename:=ExtractFilePath(aFilename);
        aFilename:=IncludeTrailingPathDelimiter(aFilename)+DefaultConfigFile;
        if TryConfig(aFilename) then
          exit;
      end;
    end;

    // finally try global directory
    {$IFDEF Unix}
    if TryConfig('/etc/'+DefaultConfigFile) then
      exit;
    {$ENDIF}

  finally
    Tried.Free;
  end;
end;

end.


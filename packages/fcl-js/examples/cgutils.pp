{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team
    Original author: Michael van Canneyt

    Some helper routines for the CGI code generator app

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cgutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, pascodegen, tstopas, strutils;

Type
   TSettings = record
     basedir : string;
     cachefile : string;
     rawcachefile : string;
   end;

   { TLoggingConverter }

   TLoggingConverter = Class(TTypescriptToPas)
   private
     FLogs: TStrings;
     procedure DoMyLog(Sender: TObject; LogType: TCodegenLogType; const Msg: String);
   Public
     Constructor Create(Aowner: TComponent); override;
     Property Logs : TStrings Read FLogs Write Flogs;
   end;

Procedure GetDeclarationFileNames(const BaseDir,aDir : String; aList: TStrings);
Procedure ConvertFile(Const BaseDir,aFileName,aUnitName,aliases,extraunits : String; Skipweb : Boolean; aOptions : TConversionOptions; aPascal,aLog : TStrings);
Function GetOutputUnitName(Const aFileName,aUnitName : String) : string;
Function GetInputFileName(Const BaseDir,aFileName : String) : string;
Function GetSettings : TSettings;

implementation

uses inifiles;

function GetSettings: TSettings;

begin
  Result.BaseDir:=ExtractFilePath(ParamStr(0));
  Result.CacheFile:=GetTempDir(True)+'definitelytypedcache.lst';
  Result.RawCacheFile:=GetTempDir(True)+'definitelytypedrawcache.lst';
  With TIniFile.Create(GetAppConfigFile(True)) do
    try
      Result.BaseDir:=ReadString('Settings','BaseDir',Result.BaseDir);
      Result.cachefile:=ReadString('Settings','CacheDir',Result.CacheFile);
      Result.rawcachefile:=ReadString('Settings','RawCacheFile',Result.RawCacheFile);
    finally
      Free;
    end;
  With TStringList.Create do
    try
      Add(Result.BaseDir);
      Add(Result.cachefile);
      Add(Result.rawcachefile);
      SaveToFile('/tmp/settings.txt');
    finally
      Free;
    end;
end;

procedure GetDeclarationFileNames(const BaseDir, aDir: String; aList: TStrings);

Var
  Info : TSearchRec;
  D,FN : string;

begin
  D:=IncludeTrailingPathDelimiter(aDir);
  if FindFirst(D+'*.d.ts',0,Info)=0 then
    try
      Repeat
      FN:=Info.Name;
      if (FN<>'.') and (FN<>'..') then
        Alist.Add(ExtractRelativePath(BaseDir,D+FN));
      until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
   if FindFirst(D+AllFilesMask,faDirectory,Info)=0 then
      try
        Repeat
          if (Info.Attr and faDirectory)<>0 then
            begin
            FN:=Info.Name;
            if (FN<>'.') and (FN<>'..') then
              GetDeclarationFileNames(BaseDir,D+FN,aList);
            end;
        until FindNext(Info)<>0;
      finally
        FindClose(Info);
      end;
end;

function GetInputFileName(const BaseDir, aFileName: String): string;

Var
  BD,FN : String;

begin
  BD:=IncludeTrailingPathDelimiter(BaseDir);
  FN:=BD+aFileName;
  if Not FileExists(FN) then
    if Not DirectoryExists(BD+aFileName) then
      FN:=''
    else
      begin
      FN:=IncludeTrailingPathDelimiter(BD+aFileName)+'index.d.ts';
      if not FileExists(FN) then
        Fn:='';
      end;
  if FN='' then
    Raise ETSToPas.Create('No such file: '+aFileName);
  Result:=FN;
end;

function GetOutputUnitName(const aFileName, aUnitName: String): string;

Var
  UN : String;
begin
  UN:=aUnitName;
  if aUnitName='' then
    begin
    UN:=ChangeFileExt(ChangeFileExt(ExtractFileName(aFilename),''),'');
    if UN='index' then
      UN:=ExtractFileName(ExcludeTrailingPathDelimiter(ExtractFilePath(aFileName)));
    end;
  Result:=UN;
end;

procedure AddWebAliases(S: Tstrings);

begin
  With S do
    begin
    {$i web.inc}
    end;
end;

procedure AddJSAliases(S: Tstrings);

begin
  With S do
    begin
    Add('Object=TJSObject');
    Add('Function=TJSFunction');
    Add('RegExp=TJSRegexp');
    Add('Promise=TJSPromise');
    Add('Date=TJSDate');
    Add('Array=TJSArray');
    Add('Iterator=TJSIterator');
    Add('IteratorResult=TJSIteratorResult');
    Add('AsyncIterator=TJSAsyncIterator');
    Add('ArrayBuffer=TJSArrayBuffer');
    Add('Set=TJSSet');
    Add('Map=TJSMap');
    Add('BufferSource=TJSBufferSource');
    Add('DataView=TJSDataView');
    Add('Int8Array=TJSInt8Array');
    Add('Int8ClampedArray=TJSInt8ClampedArray');
    Add('Int16Array=TJSInt16Array');
    Add('Int32Array=TJSInt32Array');
    Add('Uint8Array=TJSUInt8Array');
    Add('Uint8ClampedArray=TJSUInt8ClampedArray');
    Add('Uint16Array=TJSUInt16Array');
    Add('Uint32Array=TJSUInt32Array');
    Add('Float32Array=TJSFloat32Array');
    Add('Float64Array=TJSFloat64Array');
    Add('JSON=TJSJSON');
    Add('TextDecoder=TJSTextDecoder');
    Add('TextEncoder=TJSTextEncoder');
    Add('SyntaxError=TJSSyntaxError');
    Add('Error=TJSError');
    end;
end;

procedure ConvertFile(const BaseDir, aFileName, aUnitName, aliases, extraunits: String; Skipweb: Boolean;
  aOptions: TConversionOptions; aPascal, aLog: TStrings);

Var
  L : TLoggingConverter;
  S,UN,Fn : String;

begin
  FN:=GetInputFileName(BaseDir,aFileName);
  UN:=GetOutputUnitName(FN,aUnitName);
  L:=TLoggingConverter.Create(Nil);
  try
    L.Options:=aOptions;
    L.InputFileName:=FN;
    L.OutputUnitName:=UN;
    L.Logs:=aLog;
    L.ExtraUnits:=extraunits;
    for S in SplitString(Aliases,', ') do
      L.TypeAliases.Add(S);
    AddJSAliases(L.TypeAliases);
    if not SkipWeb then
      AddWebAliases(L.TypeAliases);
    L.Execute;
    aPascal.Assign(L.Source);
  finally
    L.Free;
  end;
end;

{ TLoggingConverter }

procedure TLoggingConverter.DoMyLog(Sender: TObject; LogType: TCodegenLogType; const Msg: String);

Var
  S : String;

begin
  Str(LogType,S);
  If Assigned(Flogs) then
    Flogs.Add('['+S+']: '+Msg);
end;

constructor TLoggingConverter.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  OnLog:=@DoMyLog;
end;

end.


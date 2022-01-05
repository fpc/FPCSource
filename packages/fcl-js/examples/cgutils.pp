unit cgutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, pascodegen, tstopas;

Type
   TSettings = record
     basedir : string;
     cachefile : string;
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
Procedure ConvertFile(Const BaseDir,aFileName,aUnitName : String; aOptions : TConversionOptions; aPascal,aLog : TStrings);
Function GetOutputUnitName(Const aFileName,aUnitName : String) : string;
Function GetInputFileName(Const BaseDir,aFileName : String) : string;
Function GetSettings : TSettings;

implementation

uses inifiles;

Function GetSettings : TSettings;

begin
  Result.BaseDir:=ExtractFilePath(ParamStr(0));
  Result.CacheFile:=GetTempDir(True)+'definitelytypedcache.lst';
  With TIniFile.Create(GetAppConfigFile(True)) do
    try
      Result.BaseDir:=ReadString('Settings','BaseDir',Result.BaseDir);
      Result.cachefile:=ReadString('Settings','CacheDir',Result.CacheFile);
    finally
      Free;
    end;
end;

Procedure GetDeclarationFileNames(Const BaseDir,aDir : String; aList: TStrings);

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

Function GetInputFileName(Const BaseDir,aFileName : String) : string;

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

Function GetOutputUnitName(Const aFileName,aUnitName : String) : string;

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

Procedure ConvertFile(Const BaseDir,aFileName,aUnitName : String; aOptions : TConversionOptions; aPascal,aLog : TStrings);

Var
  L : TLoggingConverter;
  UN,Fn : String;

begin
  FN:=GetInputFileName(BaseDir,aFileName);
  UN:=GetOutputUnitName(FN,aUnitName);
  L:=TLoggingConverter.Create(Nil);
  try
    L.Options:=aOptions;
    L.InputFileName:=FN;
    L.OutputUnitName:=UN;
    L.Logs:=aLog;
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


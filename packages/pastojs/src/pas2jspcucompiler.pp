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
    FileSystem aware compiler descendent with support for PCU files.
}
unit pas2jspcucompiler;

{$mode objfpc}{$H+}

{$I pas2js_defines.inc}

{$IF defined(VerboseUnitQueue) or defined(VerbosePCUFiler)}
{$DEFINE ReallyVerbose}
{$ENDIF}

interface

uses
  SysUtils,Classes,
  pastree,
  pas2jscompiler, pas2jsfs, pas2jsfscompiler, Pas2JsFiler;

Type
  TFilerPCUSupport = Class(TPCUSupport)
  Private
    // This is the format that will be written.
    FPCUFormat : TPas2JSPrecompileFormat;
    // This is the format that will be read.
    FFoundFormat : TPas2JSPrecompileFormat;
    FPrecompileInitialFlags: TPCUInitialFlags;
    FPCUReader: TPCUCustomReader;
    FPCUReaderStream: TStream;
    function OnPCUConverterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    function OnPCUConverterIsTypeInfoUsed(Sender: TObject; El: TPasElement): boolean;
    function OnWriterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    procedure OnFilerGetSrc(Sender: TObject; aFilename: string; out p: PChar; out Count: integer);
  Public
    constructor create(aCompilerFile: TPas2JSCompilerFile; aFormat: TPas2JSPrecompileFormat);
    Destructor destroy; override;
    Function Compiler : TPas2JSCompiler;
    Function HandleException(E: exception) : Boolean; override;
    function FindPCU(const UseUnitName: string): string;override;
    function FindPCU(const UseUnitName: string; out aFormat: TPas2JSPrecompileFormat): string;
    Function HasReader : Boolean; override;
    Function ReadContinue: Boolean; override;
    Function ReadCanContinue : Boolean; override;
    Procedure SetInitialCompileFlags; override;
    Procedure WritePCU; override;
    procedure CreatePCUReader; override;
    Procedure ReadUnit; override;
    property PrecompileInitialFlags: TPCUInitialFlags read FPrecompileInitialFlags;
  end;

  { TPas2jsPCUCompilerFile }

  TPas2jsPCUCompilerFile = Class(TPas2jsCompilerFile)
    Function CreatePCUSupport: TPCUSupport; override;
  end;


  { TPas2jsPCUCompiler }

  TPas2jsPCUCompiler = Class(TPas2JSFSCompiler)
  Private
    FPrecompileFormat : TPas2JSPrecompileFormat;
  Protected
    procedure WritePrecompiledFormats; override;
    function CreateCompilerFile(const UnitFileName: String): TPas2jsCompilerFile; override;
    Procedure HandleOptionPCUFormat(Value : string) ; override;
  end;

implementation

uses fppas2js, pscanner, pas2jslogger, pasresolveeval, jstree, pas2jsfileutils;



{$IFDEF HASPAS2JSFILER}

{ ---------------------------------------------------------------------
  TFilerPCUSupport
  ---------------------------------------------------------------------}

{ TFilerPCUSupport }

constructor TFilerPCUSupport.create(aCompilerFile: TPas2JSCompilerFile; aFormat: TPas2JSPrecompileFormat);
begin
  Inherited Create(aCompilerFile);
  FPCUFormat:=AFormat;
  FPrecompileInitialFlags:=TPCUInitialFlags.Create;
end;

destructor TFilerPCUSupport.destroy;
begin
  FreeAndNil(FPrecompileInitialFlags);
  FreeAndNil(FPCUReader);
  FreeAndNil(FPCUReaderStream);
  inherited destroy;
end;

function TFilerPCUSupport.Compiler: TPas2JSCompiler;
begin
  Result:=MyFile.Compiler;
end;

Function TFilerPCUSupport.HandleException(E: Exception) : Boolean;

begin
  Result:=False;
  if E is EPas2JsReadError then
    begin
    Result:=True;
    if EPas2JsReadError(E).Owner is TPCUCustomReader then
      begin
        MyFile.Log.Log(mtError,E.Message,0,MyFile.PCUFilename);
      end else begin
        MyFile.Log.Log(mtError,E.Message);
      end;
    Compiler.Terminate(ExitCodePCUError);
    end
  else if (E is EPas2JsWriteError) then
    begin
    MyFile.Log.Log(mtFatal,E.ClassName+':'+E.Message);
    Compiler.Terminate(ExitCodeErrorInternal);
    Result:=True;
    end
end;

function TFilerPCUSupport.FindPCU(const UseUnitName: string): string;

begin
  Result:=FindPCU(UseUnitName,FFoundFormat);
end;

function TFilerPCUSupport.HasReader: Boolean;
begin
  Result:=Assigned(FPCUReader);
end;

function TFilerPCUSupport.ReadContinue: Boolean;
begin
  Result:=FPCUReader.ReadContinue;
end;

function TFilerPCUSupport.ReadCanContinue: Boolean;
begin
  Result:=FPCUReader.ReadCanContinue;
end;

procedure TFilerPCUSupport.SetInitialCompileFlags;
begin
  PrecompileInitialFlags.ParserOptions:=MyFile.Parser.Options;
  PrecompileInitialFlags.ModeSwitches:=MyFile.Scanner.CurrentModeSwitches;
  PrecompileInitialFlags.BoolSwitches:=MyFile.Scanner.CurrentBoolSwitches;
  PrecompileInitialFlags.ConverterOptions:=MyFile.GetInitialConverterOptions;
  PrecompileInitialFlags.TargetPlatform:=Compiler.TargetPlatform;
  PrecompileInitialFlags.TargetProcessor:=Compiler.TargetProcessor;
end;

procedure TFilerPCUSupport.CreatePCUReader;
var
  aFile: TPas2jsFile;
  s: String;
begin
  if MyFile.PCUFilename='' then
    RaiseInternalError(20180312144742,MyFile.PCUFilename);
  if FPCUReader<>nil then
    RaiseInternalError(20180312142938,GetObjName(FPCUReader));
  if FFoundFormat=nil then
    RaiseInternalError(20180312142954,'');
  FPCUReader:=FFoundFormat.ReaderClass.Create;
  FPCUReader.SourceFilename:=ExtractFileName(MyFile.PCUFilename);

  if MyFile.ShowDebug then
    MyFile.Log.LogMsg(nParsingFile,[QuoteStr(MyFile.PCUFilename)]);
  aFile:=Compiler.FS.LoadFile(MyFile.PCUFilename,true);
  if aFile=nil then
    RaiseInternalError(20180312145941,MyFile.PCUFilename);
  FPCUReaderStream:=TMemoryStream.Create;
  s:=aFile.Source;
  //writeln('TPas2jsCompilerFile.CreatePCUReader ',PCUFilename,'-----START-----');
  //writeln(s);
  //writeln('TPas2jsCompilerFile.CreatePCUReader ',PCUFilename,'-----END-------');
  if s<>'' then
  begin
    FPCUReaderStream.Write(s[1],length(s));
    FPCUReaderStream.Position:=0;
  end;
end;

procedure TFilerPCUSupport.ReadUnit;
begin
  FPCUReader.ReadPCU(MyFile.PascalResolver,FPCUReaderStream);
  SetPasModule(MyFile.PascalResolver.RootElement);
  SetReaderState(prsCanContinue);
end;

function TFilerPCUSupport.FindPCU(const UseUnitName: string; out  aFormat: TPas2JSPrecompileFormat): string;

  function SearchInDir(DirPath: string): boolean;
  var
    i: Integer;
    CurFormat: TPas2JSPrecompileFormat;
    Filename: String;
  begin
    if DirPath='' then exit(false);
    DirPath:=IncludeTrailingPathDelimiter(DirPath);
    for i:=0 to PrecompileFormats.Count-1 do
    begin
      CurFormat:=PrecompileFormats[i];
      if not CurFormat.Enabled then continue;
      Filename:=DirPath+UseUnitName+'.'+CurFormat.Ext;
      if Compiler.FS.PCUExists(Filename) then
      begin
        FindPCU:=Filename;
        aFormat:=CurFormat;
        exit(true);
      end;
    end;
    Result:=false;
  end;

var
  L : TstringList;
  i: Integer;

begin
  Result:='';
  aFormat:=nil;
  L:=TstringList.Create;
  try
    Compiler.FS.GetPCUDirs(L,MyFile.FileResolver.BaseDirectory);
    for i:=0 to L.Count-1 do
      if SearchInDir(L[i]) then exit;
  finally
    L.Free;
  end;
end;

function TFilerPCUSupport.OnWriterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  Result:=MyFile.UseAnalyzer.IsUsed(El);
end;

procedure TFilerPCUSupport.WritePCU;

Const
  AllowCompressed =
{$IFDEF DisablePCUCompressed}false{$ELSE}true{$ENDIF};

var
  Writer: TPCUWriter;
  ms: TMemoryStream;
  DestDir: String;
  JS: TJSElement;
  FN : String;

begin
  if FPCUFormat=Nil then
    exit; // Don't write
  if MyFile.PasModule.ClassType<>TPasModule then
  begin
    {$IFDEF REALLYVERBOSE}
    writeln('TPas2jsCompilerFile.WritePCU not a unit: ',MyFile.PasFilename,' skip');
    {$ENDIF}
    exit;
  end;

  if (MyFile.PCUFilename<>'') or (FPCUReader<>nil) then
  begin
    {$IFDEF REALLYVERBOSE}
    writeln('TPas2jsCompilerFile.WritePCU already precompiled "',MyFile.PCUFilename,'" Reader=',GetObjName(FPCUReader));
    {$ENDIF}
    exit;
  end;

  // Determine output filename
  FN:=ExtractFilenameOnly(MyFile.PasFilename)+'.'+FPCUFormat.Ext;
  if Compiler.FS.UnitOutputPath<>'' then
    FN:=Compiler.FS.UnitOutputPath+FN
  else
    FN:=ExtractFilePath(MyFile.PasFilename)+FN;
  // Set as our filename
  SetPCUFilename(FN);
  {$IFDEF REALLYVERBOSE}
  writeln('TPas2jsCompilerFile.WritePCU precompiling ',MyFile.PCUFilename);
  {$ENDIF}

  JS:=nil;
  ms:=TMemoryStream.Create;
  Writer:=FPCUFormat.WriterClass.Create;
  try
    Writer.GUID:=Compiler.PrecompileGUID;
    Writer.OnGetSrc:=@OnFilerGetSrc;
    Writer.OnIsElementUsed:=@OnWriterIsElementUsed;

    // create JavaScript for procs, initialization, finalization
    MyFile.CreateConverter;
    MyFile.Converter.Options:=MyFile.Converter.Options+[coStoreImplJS];
    MyFile.Converter.OnIsElementUsed:=@OnPCUConverterIsElementUsed;
    MyFile.Converter.OnIsTypeInfoUsed:=@OnPCUConverterIsTypeInfoUsed;
    JS:=MyFile.Converter.ConvertPasElement(MyFile.PasModule,MyFile.PascalResolver);
    MyFile.Converter.Options:=MyFile.Converter.Options-[coStoreImplJS];
    {$IFDEF REALLYVERBOSE}
    writeln('TPas2jsCompilerFile.WritePCU create pcu ... ',MyFile.PCUFilename);
    {$ENDIF}
    Writer.WritePCU(MyFile.PascalResolver,MyFile.Converter,PrecompileInitialFlags,ms,AllowCompressed);
    {$IFDEF REALLYVERBOSE}
    writeln('TPas2jsCompilerFile.WritePCU precompiled ',MyFile.PCUFilename);
    {$ENDIF}

    MyFile.Log.LogMsg(nWritingFile,[QuoteStr(Compiler.FS.FormatPath(MyFile.PCUFilename))],'',0,0,
               not (coShowLineNumbers in Compiler.Options));

    // check output directory
    DestDir:=ChompPathDelim(ExtractFilePath(MyFile.PCUFilename));
    if (DestDir<>'') and not Compiler.FS.DirectoryExists(DestDir) then
    begin
      {$IFDEF REALLYVERBOSE}
      writeln('TPas2jsCompilerFile.WritePCU output dir not found "',DestDir,'"');
      {$ENDIF}
      MyFile.Log.LogMsg(nOutputDirectoryNotFound,[QuoteStr(Compiler.FS.FormatPath(DestDir))]);
      Compiler.Terminate(ExitCodeFileNotFound);
    end;
    if Compiler.FS.DirectoryExists(MyFile.PCUFilename) then
    begin
      {$IFDEF REALLYVERBOSE}
      writeln('TPas2jsCompilerFile.WritePCU file is folder "',DestDir,'"');
      {$ENDIF}
      MyFile.Log.LogMsg(nFileIsFolder,[QuoteStr(Compiler.FS.FormatPath(MyFile.PCUFilename))]);
      Compiler.Terminate(ExitCodeWriteError);
    end;

    ms.Position:=0;
    Compiler.FS.SaveToFile(ms,MyFile.PCUFilename);
    {$IFDEF REALLYVERBOSE}
    writeln('TPas2jsCompilerFile.WritePCU written ',MyFile.PCUFilename);
    {$ENDIF}
  finally
    JS.Free;
    Writer.Free;
    ms.Free;
  end;
end;

procedure TFilerPCUSupport.OnFilerGetSrc(Sender: TObject; aFilename: string;
  out p: PChar; out Count: integer);
var
  SrcFile: TPas2jsFile;
begin
  if Sender=nil then
    RaiseInternalError(20180311135558,aFilename);
  SrcFile:=MyFile.Compiler.FS.LoadFile(aFilename);
  if SrcFile=nil then
    RaiseInternalError(20180311135329,aFilename);
  p:=PChar(SrcFile.Source);
  Count:=length(SrcFile.Source);
end;

function TFilerPCUSupport.OnPCUConverterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  if (coKeepNotUsedPrivates in MyFile.Compiler.Options) then
    Result:=true
  else
    Result:=MyFile.UseAnalyzer.IsUsed(El);
end;

function TFilerPCUSupport.OnPCUConverterIsTypeInfoUsed(Sender: TObject;
  El: TPasElement): boolean;
begin
  if Sender=nil then ;
  if El=nil then ;
  // PCU does not need precompiled typeinfo
  Result:=false;
end;

{ TPas2jsPCUCompiler }



procedure TPas2jsPCUCompiler.WritePrecompiledFormats;

Var
  I : Integer;

begin
  if PrecompileFormats.Count>0 then
  begin
    writeHelpLine('   -JU<x> : Create precompiled units in format x.');
    for i:=0 to PrecompileFormats.Count-1 do
      with PrecompileFormats[i] do
        writeHelpLine('     -JU'+Ext+' : '+Description);
    writeHelpLine('     -JU- : Disable prior -JU<x> option. Do not create precompiled units.');
  end;
end;

function TPas2jsPCUCompiler.CreateCompilerFile(const UnitFileName: String): TPas2jsCompilerFile;
begin
  Result:=TPas2JSPCUCompilerFile.Create(Self,UnitFileName);
end;

procedure TPas2jsPCUCompiler.HandleOptionPCUFormat(Value: string);

Var
  Found : Boolean;
  I : integer;
  PF: TPas2JSPrecompileFormat;
begin
  Found:=false;
  for i:=0 to PrecompileFormats.Count-1 do
  begin
    PF:=PrecompileFormats[i];
    if not SameText(Value,PF.Ext) then continue;
      FPrecompileFormat:=PrecompileFormats[i];
    Found:=true;
  end;
  if not Found then
    ParamFatal('invalid precompile output format (-JU) "'+Value+'"');
end;



{ TPas2jsPCUCompilerFile }

function TPas2jsPCUCompilerFile.CreatePCUSupport: TPCUSupport;

Var
  PF: TPas2JSPrecompileFormat;

begin
  // Note that if no format was preset, no files will be written
  PF:=(Compiler as TPas2jsPCUCompiler).FPrecompileFormat;
  if PF<>Nil then
    Result:=TFilerPCUSupport.Create(Self,PF)
  else
    Result:=Nil;
end;
{$ENDIF}

end.


{
compiler.pas

Compiling, Linking and Registering in Emulator methods

Copyright (C) 2006-2007 Felipe Monteiro de Carvalho

This file is part of MkSymbian build tool.

MkSymbian is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

MkSymbian is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating MkSymbian into proprietary programs.
}
unit compiler;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Process,
  constants;

type

  { TCompiler }

  TCompiler = class(TObject)
  private
    AProcess: TProcess;
    CurrentDirectory: string;
    MakeFolder, MakePartialFolder, BindingsUnitsFolder: string;
  public
    opts: TMkSymbianOptions;
    constructor Create;
    destructor Destroy; override;
    procedure FileCopy(source, dest: string);
    procedure MakeBuildPascal;
    procedure MakeBuildCpp;
    procedure MakeBuildBindings;
    procedure BuildUIDFile;
    procedure BuildResource(AFileName: string);
    procedure RegisterInEmulator;
  end;

var
  vCompiler: TCompiler;

implementation

uses sdkutil, projectparser;

{ TCompiler }

{*******************************************************************
*  TCompiler.Create ()
*
*  DESCRIPTION:    Initializes the compiler controlling object
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
constructor TCompiler.Create;
begin
  inherited Create;

  AProcess := TProcess.Create(nil);

  CurrentDirectory := ExtractFilePath(ParamStr(0));
  MakePartialFolder := Copy(CurrentDirectory, 3, Length(CurrentDirectory) - 2);
  MakeFolder := IncludeTrailingBackslash(CurrentDirectory);
  
  { When compiling the bindings we use a relative directory to get the output dir }
  BindingsUnitsFolder := MakeFolder + '../../units/i386-symbian/';

  AProcess.Options := AProcess.Options + [poWaitOnExit];
end;

{*******************************************************************
*  TCompiler.Destroy ()
*
*  DESCRIPTION:    Finalizes the compiler controlling object
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TCompiler.Destroy;
begin
  AProcess.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TCompiler.FileCopy ()
*
*  DESCRIPTION:    Copyes a file from source to dest
*
*  PARAMETERS:     source  - Source file
*                  dest    - Destination file
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.FileCopy(source, dest: string);
var
  SourceStream, DestStream: TFileStream;
begin
  SourceStream := TFileStream.Create(source, fmOpenRead);
  try
    DestStream := TFileStream.Create(dest, fmCreate);
    try
      DestStream.CopyFrom(SourceStream, 0);
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

{*******************************************************************
*  TCompiler.MakeBuildPascal ()
*
*  DESCRIPTION:    Builds and links a Object Pascal project
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.MakeBuildPascal;
var
  STR_LINK_FLAGSUDEB, STR_EPOCBLDUDEB, STR_LINK_OBJSUDEB: string;
  STR_FPC_RTL_OBJECTS: string;
  i: Integer;
begin

  WriteLn('');
  WriteLn('Preparations for compiling');
  WriteLn('');

  // First command

{  AProcess.CommandLine := 'perl -S makmake.pl  -D ' + MakePartialFolder + 'QHELLOWORLD WINSCW';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;}

  { Creation of directories }

  ForceDirectories(vSDKUtil.SDKFolder + 'EPOC32\DATA\Z\private\10003a3f\apps');

  ForceDirectories(vSDKUtil.SDKFolder + 'EPOC32\RELEASE\WINSCW\UDEB\Z\private\10003a3f\apps');

  ForceDirectories(MakeFolder + 'WINSCW\UDEB');

  { Compilation }

  WriteLn('');
  WriteLn('Compiling file ' + vProject.MainSource);
  WriteLn('');
  
  AProcess.CommandLine := vProject.CompilerPath + ' -a -s -Fu' + vProject.RTLUnitsDir +
    ' -Tsymbian QPasHello.pas';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;

  WriteLn('');
  WriteLn('Assembling file QPasHello.s');
  WriteLn('');

  AProcess.CommandLine := vProject.AssemblerPath + ' QPasHello.s -o QPasHello.o';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;

  { UID File }

  BuildUIDFile;

  { Linking }

  STR_LINK_FLAGSUDEB := '-msgstyle gcc -stdlib "' +
    vSDKUtil.SDKPartialFolder + 'EPOC32\RELEASE\WINSCW\UDEB\EEXE.LIB" -m' +
    ' "?_E32Bootstrap@@YGXXZ" -subsystem windows -g ' +
    vSDKUtil.SDKPartialFolder + 'EPOC32\RELEASE\WINSCW\UDEB\EUSER.LIB ' +
    '-o "' + MakeFolder + 'QPasHello.exe" -noimplib';
  STR_EPOCBLDUDEB := MakeFolder + 'WINSCW\UDEB';

  STR_LINK_OBJSUDEB :=
    ' ' + MakeFolder + UID_OBJECT_FILENAME;

  for i := 0 to vProject.ObjectFiles.Count - 1 do
   STR_LINK_OBJSUDEB := STR_LINK_OBJSUDEB +
    ' ' + MakeFolder + vProject.ObjectFiles.Strings[i];

  STR_FPC_RTL_OBJECTS :=
    ' ' + vProject.RTLUnitsDir + 'system.o' +
    ' ' + vProject.RTLUnitsDir + 'symbian.o' +
    ' ' + vProject.RTLUnitsDir + 'ctypes.o' +
    ' ' + vProject.RTLUnitsDir + 'objpas.o' +
    ' ' + vProject.RTLUnitsDir + 'pbeexe.o';

  WriteLn('');
  WriteLn('Linking stage');
  WriteLn('');

  AProcess.CommandLine := vSDKUtil.SDKFolder + Str_Path_CWTools +
    'mwldsym2.exe ' + STR_LINK_FLAGSUDEB +
    ' -l ' + MakeFolder +
    ' -search ' + STR_LINK_OBJSUDEB + STR_FPC_RTL_OBJECTS;
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;

  FileCopy(MakeFolder + 'QPasHello.exe',
   vSDKUtil.SDKPartialFolder + 'EPOC32\RELEASE\WINSCW\UDEB\' + 'QPasHello.exe');
end;

{*******************************************************************
*  TCompiler.MakeBuildCpp ()
*
*  DESCRIPTION:    Builds and links a C++ project
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.MakeBuildCpp;
var
  STR_LINK_FLAGSUDEB, STR_EPOCBLDUDEB, STR_LINK_OBJSUDEB,
  STR_CWUFLAGS, STR_CWDEFS, STR_INCDIR, STR_CWUDEB: string;
begin

  WriteLn('');
  WriteLn('Preparations for compiling');
  WriteLn('');
  
  // First command

  AProcess.CommandLine := 'perl -S makmake.pl  -D ' + MakePartialFolder + 'QHELLOWORLD WINSCW';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;

  { Creation of directories }

  ForceDirectories(vSDKUtil.SDKFolder + 'EPOC32\DATA\Z\private\10003a3f\apps');

  ForceDirectories(vSDKUtil.SDKFolder + 'EPOC32\RELEASE\WINSCW\UDEB\Z\private\10003a3f\apps');

  ForceDirectories(MakeFolder + 'WINSCW\UDEB');

//  TODO: Check if this can be safely removed
//  ForceDirectories(MakeFolder + 'QHelloWorld\WINSCW');

  { Compilation }

  STR_CWUFLAGS := '-wchar_t off -align 4 -warnings on ' +
    '-w nohidevirtual,nounusedexpr -msgstyle gcc -enum int -str pool -exc ms -trigraphs on  -nostdinc';
  STR_CWDEFS := '-d "__SYMBIAN32__" -d "__CW32__" -d "__WINS__" -d "__WINSCW__" -d "__EXE__" -d "__SUPPORT_CPP_EXCEPTIONS__" ';
  STR_INCDIR := '-cwd source -i- ' +
    '-i "' + vSDKUtil.SDKPartialFolder + 'EPOC32\include" ' +
    '-i "' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant" ' +
    '-i "' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant\ " ' +
    '-include "UIQ_3.0.hrh"';
  STR_CWUDEB := 'mwccsym2.exe -g -O0 -inline off ' + STR_CWUFLAGS + ' -d _DEBUG -d _UNICODE ' + STR_CWDEFS + STR_INCDIR;

  WriteLn('');
  WriteLn('Compiling file ' + vProject.MainSource);
  WriteLn('');

  AProcess.CommandLine := STR_CWUDEB +
    ' -o "' + MakeFolder + 'WINSCW\UDEB\' + vProject.MainSourceNoExt + '.o"' +
    ' -c "' + MakeFolder + 'src\' + vProject.MainSource + '"';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;

  { UID File }

  BuildUIDFile;

  { Linking }

  STR_LINK_FLAGSUDEB := '-msgstyle gcc' +
    ' -stdlib "' + vSDKUtil.SDKPartialFolder + 'EPOC32\RELEASE\WINSCW\UDEB\EEXE.LIB"' +
    ' -m "?_E32Bootstrap@@YGXXZ" -subsystem windows' +
    ' -g ' + vSDKUtil.SDKPartialFolder + 'EPOC32\RELEASE\WINSCW\UDEB\EUSER.LIB' +
    ' -o "' + vSDKUtil.SDKPartialFolder + 'EPOC32\RELEASE\WINSCW\UDEB\' + vProject.MainSourceNoExt + '.exe"' +
    ' -noimplib';
  STR_EPOCBLDUDEB := MakeFolder + 'WINSCW\UDEB';
  STR_LINK_OBJSUDEB := vProject.MainSourceNoExt + '.o ' + UID_OBJECT_FILENAME;

  WriteLn('');
  WriteLn('Linking stage');
  WriteLn('');

  AProcess.CommandLine := 'mwldsym2.exe ' + STR_LINK_FLAGSUDEB +
    ' -l ' + STR_EPOCBLDUDEB +
    ' -search ' + STR_LINK_OBJSUDEB;
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;
end;

{*******************************************************************
*  TCompiler.MakeBuildBindings ()
*
*  DESCRIPTION:    Builds and links the C interface for the symbian libraries
*
*                  Note the we use a output directory relative to the current directory
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.MakeBuildBindings;
var
  STR_CWUFLAGS, STR_CWDEFS, STR_INCDIR, STR_CWUDEB, STR_CWCOMPILER: string;
begin
  { Makes sure that the output directory exists }

  SysUtils.ForceDirectories(BindingsUnitsFolder);
  
  { Compilation }

  STR_CWUFLAGS := '-wchar_t off -align 4 -warnings on ' +
    '-w nohidevirtual,nounusedexpr -msgstyle gcc -enum int -str pool -exc ms -trigraphs on  -nostdinc';
  STR_CWDEFS := '-d "__SYMBIAN32__" -d "__CW32__" -d "__WINS__" -d "__WINSCW__" -d "__EXE__" -d "__SUPPORT_CPP_EXCEPTIONS__" ';
  STR_INCDIR := '-cwd source -i-' +
    ' -i "' + vSDKUtil.SDKPartialFolder + 'EPOC32\include"' +
    ' -i "' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant"' +
    ' -i "' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant\ "' +
    ' -include "UIQ_3.0.hrh"';
  STR_CWCOMPILER := vSDKUtil.SDKFolder + Str_Path_CWTools + 'mwccsym2.exe';
  STR_CWUDEB := STR_CWCOMPILER + ' -g -O0 -inline off ' + STR_CWUFLAGS + ' -d _DEBUG -d _UNICODE ' + STR_CWDEFS + STR_INCDIR;

  WriteLn('');
  WriteLn('Compiling file pbeexe.cpp');
  WriteLn('');

  AProcess.CommandLine := STR_CWUDEB +
    ' -o "' + BindingsUnitsFolder + 'pbeexe.o" ' +
    '-c "' + MakePartialFolder + 'pbeexe.cpp"';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;
end;

{*******************************************************************
*  TCompiler.BuildUIDFile ()
*
*  DESCRIPTION:    Generates and compiles a UID file
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.BuildUIDFile;
var
  Str_UIDFile: string;
  UIDFile: TFileStream;
  STR_CWUFLAGS, STR_CWDEFS, STR_INCDIR, STR_CWUDEB, STR_CWCOMPILER: string;
begin
  { First creates the UID file }

  WriteLn('');
  WriteLn('Creating UID file');
  WriteLn('');

  Str_UIDFile :=
    '// mksymbian-generated uid source file' + LineEnding +
    '#include <e32cmn.h>' + LineEnding +
    '#pragma data_seg(".SYMBIAN")' + LineEnding +
    '__EMULATOR_IMAGE_HEADER2(0x1000007a,' + vProject.UID2 + ',' + vProject.UID3 +
   ',EPriorityForeground,0x00000000u,0x00000000u,0x01000001,0,0x00010000,0)' + LineEnding +
    '#pragma data_seg()' + LineEnding;

  UIDFile := TFileStream.Create(UID_SOURCE_FILENAME, fmCreate);
  try
    UIDFile.Write(Pointer(Str_UIDFile)^, Length(Str_UIDFile));
  finally
    UIDFile.Free;
  end;
  
  { Compilation }

  STR_CWUFLAGS := '-wchar_t off -align 4 -warnings on ' +
    '-w nohidevirtual,nounusedexpr -msgstyle gcc -enum int -str pool -exc ms -trigraphs on  -nostdinc';
  STR_CWDEFS := '-d "__SYMBIAN32__" -d "__CW32__" -d "__WINS__" -d "__WINSCW__" -d "__EXE__" -d "__SUPPORT_CPP_EXCEPTIONS__" ';
  STR_INCDIR := '-cwd source -i- ' +
    ' -i "' + vSDKUtil.SDKPartialFolder + 'EPOC32\include" ' +
    ' -i "' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant" ' +
    ' -i "' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant\ "' +
    ' -include "UIQ_3.0.hrh"';
  STR_CWCOMPILER := vSDKUtil.SDKFolder + Str_Path_CWTools + 'mwccsym2.exe';
  STR_CWUDEB := STR_CWCOMPILER + ' -g -O0 -inline off ' + STR_CWUFLAGS + ' -d _DEBUG -d _UNICODE ' + STR_CWDEFS + STR_INCDIR;

  WriteLn('');
  WriteLn('Compiling file ' + UID_SOURCE_FILENAME);
  WriteLn('');

  AProcess.CommandLine := STR_CWUDEB +
    ' -o "' + MakeFolder + UID_OBJECT_FILENAME + '"' +
    ' -c "' + MakeFolder + UID_SOURCE_FILENAME + '"';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;
end;

{*******************************************************************
*  TCompiler.BuildResource ()
*
*  DESCRIPTION:    Builds a resource file
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.BuildResource(AFileName: string);
begin
  WriteLn('');
  WriteLn('Preprocessing resource file: ' + AFileName);
  WriteLn('');

  AProcess.CommandLine := vSDKUtil.SDKFolder + Str_Path_Cpp +
    ' -lang-c++' +
    ' -I ' + vSDKUtil.SDKPartialFolder + 'EPOC32\include' +
    ' -I ' + vSDKUtil.SDKPartialFolder + 'epoc32\include\variant' +
    ' ' + MakeFolder + AFileName +
    ' ' + MakeFolder + ChangeFileExt(AFileName, STR_RESOURCE_TMP_EXT);
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;

  WriteLn('');
  WriteLn('Building resource file: ' + AFileName);
  WriteLn('');

  AProcess.CommandLine := vSDKUtil.SDKFolder + Str_Path_RComp +
    ' -v -u' +
    ' -o"' + MakeFolder + ChangeFileExt(AFileName, STR_RESOURCE_EXT) + '"' +
    ' -s"' + MakeFolder + ChangeFileExt(AFileName, STR_RESOURCE_TMP_EXT) + '"';
  WriteLn(AProcess.CommandLine);
  AProcess.Execute;
end;

{*******************************************************************
*  TCompiler.RegisterInEmulator ()
*
*  DESCRIPTION:    Registers a software in the emulator
*                  At this point the resource file must already be compiled
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCompiler.RegisterInEmulator;
var
  StrFrom, StrTo: string;
begin
  WriteLn('');
  WriteLn('Registering the software on the emulator');
  WriteLn('');

  StrFrom := MakeFolder + ChangeFileExt(vProject.MainResource, STR_RESOURCE_EXT);
  StrTo := vSDKUtil.SDKFolder + Str_Path_Emulator_Registration +
   ChangeFileExt(vProject.MainResource, STR_RESOURCE_EXT);

  WriteLn('');
  WriteLn('Copying file: ', StrFrom);
  WriteLn('To: ', StrTo);
  WriteLn('');

  FileCopy(StrFrom, StrTo);
end;

end.


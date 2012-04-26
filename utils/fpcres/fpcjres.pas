{

    FPCRes - Free Pascal Resource Converter
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi
    Copyright (C) 2011 by Jonas Maebe

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{
  Java's internal resource file handling system is based on simply loading
  files from withing the package name space. We reserve the namespace
  org.freepascal.rawresources for this purpose.

  This program creates a jar file (= zip file) containing all specified files.
}

program fpcjres;

{$MODE OBJFPC} {$H+}

uses
  SysUtils, Classes, paramparser, msghandler, jarsourcehandler,
  zipper
  ;

const
  halt_no_err = 0;
  halt_param_err = 1;
  halt_read_err = 2;
  halt_write_err = 3;

  progname = 'fpcjres';
  progversion = '1.0'; //to distinguish from the old fpcres

  fpcversion = {$INCLUDE %FPCVERSION%};
  host_arch = {$INCLUDE %FPCTARGETCPU%};
  host_os = {$INCLUDE %FPCTARGETOS%};
  build_date = {$INCLUDE %DATE%};


var
  params : TParameters = nil;
  resources : TZipper = nil;
  sourcefiles : TJarSourceFiles = nil;

procedure ShowVersion;
begin
  writeln(progname+' - resource file converter, version '+progversion+' ['+build_date+'], FPC '+fpcversion);
  writeln('Host platform: '+host_os+' - '+host_arch);
  writeln('Copyright (c) 2008 by Giulio Bernardi.');
  writeln('Copyright (c) 2011 by Jonas Maebe.');
end;

procedure ShowHelp;
begin
  ShowVersion;
  writeln('Syntax: '+progname+' [options] <inputfile> [<inputfile>...] [-o <outputfile>]');
  writeln;
  writeln('Options:');
  writeln('  --help, -h, -?       Show this screen.');
  writeln('  --version, -V        Show program version.');
  writeln('  --verbose, -v        Be verbose.');
  writeln('  --input, -i <x>      Ignored for compatibility.');
  writeln('  --output, -o <x>     Set the output file name.');
  writeln('  @<file>              Read more options from file <file>');
end;

const
  SOutputFileAlreadySet = 'Output file name already set.';
  SUnknownParameter = 'Unknown parameter ''%s''';
  SArgumentMissing = 'Argument missing for option ''%s''';
  SUnknownObjFormat = 'Unknown file format ''%s''';
  SUnknownMachine = 'Unknown architecture ''%s''';
  SFormatArchMismatch = 'Architecture %s is not available for %s format';
  SNoInputFiles = 'No input files';
  SNoOutputFile = 'No output file name specified';
  SCannotReadConfFile ='Can''t read config file ''%s''';
  
  SCantOpenFile = 'Can''t open file ''%s''';
  SUnknownInputFormat = 'No known file format detected for file ''%s''';
  
  SCantCreateDirHier = 'Can''t create directory hierarchy ''%s''';
  SCantCreateFile = 'Can''t create file ''%s''';

function GetCurrentTimeMsec : longint;
var h,m,s,ms : word;
begin
  DecodeTime(Time,h,m,s,ms);
  Result:=h*3600*1000 + m*60*1000 + s*1000 + ms;
end;

procedure CheckInputFiles;
begin
  if params.InputFiles.Count=0 then
  begin
    Messages.DoError(SNoInputFiles);
    halt(halt_param_err);
  end;
end;

procedure CheckOutputFile;
var tmp : string;
begin
  if params.OutputFile<>'' then exit;
  if params.InputFiles.Count>1 then
  begin
    Messages.DoError(SNoOutputFile);
    halt(halt_param_err);
  end;
  tmp:=ChangeFileExt(ExtractFileName(params.InputFiles[0]),
    '.jar');
  if lowercase(tmp)=lowercase(params.InputFiles[0]) then
    tmp:=tmp+'.jar';
  params.OutputFile:=tmp;
end;

procedure ParseParams;
var msg : string;
begin
  Messages.DoVerbose('parsing command line parameters');
  msg:='';
  if ParamCount = 0 then
  begin
    ShowHelp;
    halt(halt_no_err);
  end;
  params:=TParameters.Create;
  try
    params.Parse;
  except
    on e : EOutputFileAlreadySetException do msg:=SOutputFileAlreadySet;
    on e : EUnknownParameterException do msg:=Format(SUnknownParameter,[e.Message]);
    on e : EArgumentMissingException do msg:=Format(SArgumentMissing,[e.Message]);
    on e : EUnknownObjFormatException do msg:=Format(SUnknownObjFormat,[e.Message]);
    on e : EUnknownMachineException do msg:=Format(SUnknownMachine,[e.Message]);
    on e : ECannotReadConfFile do msg:=Format(SCannotReadConfFile,[e.Message]);
  end;
  Messages.Verbose:=params.Verbose;
  if msg<>'' then
  begin
    Messages.DoError(msg);
    halt(halt_param_err);
  end;
  if params.Version then
  begin
    ShowVersion;
    halt(halt_no_err);
  end;
  if params.Help then
  begin
    ShowHelp;
    halt(halt_no_err);
  end;

  CheckInputFiles;
  CheckOutputFile;

  Messages.DoVerbose('finished parsing command line parameters');
end;

procedure LoadSourceFiles;
var msg : string;
begin
  msg:='';
  resources:=TZipper.Create;
  sourcefiles:=TJarSourceFiles.Create;
  sourcefiles.FileList.AddStrings(params.InputFiles);
  try
    sourcefiles.Load(resources);
  except
    on e : ECantOpenFileException do msg:=Format(SCantOpenFile,[e.Message]);
    on e : EUnknownInputFormatException do msg:=Format(SUnknownInputFormat,[e.Message]);
    on e : Exception do
    begin
      if e.Message='' then msg:=e.ClassName
      else msg:=e.Message;
    end;
  end;
  if msg<>'' then
  begin
    Messages.DoError(msg);
    halt(halt_read_err);
  end;
end;


procedure WriteOutputFile;
var
  msg : string;
  outfile: THandle;
  removedirlevel: longint;
begin
  { create the "resbasedir" hierarchy, since that directory has to exist for
    TZipper to be able to add it. If it already exists, make sure we don't
    remove it }
  if DirectoryExists('org') then
    if DirectoryExists('org'+DirectorySeparator+'freepascal') then
      if DirectoryExists(resbasedir) then
        removedirlevel:=0
      else
        removedirlevel:=1
    else
      removedirlevel:=2
  else
    removedirlevel:=3;
  try
    ForceDirectories(resbasedir);
  except
    Messages.DoError(Format(SCantCreateDirHier,[resbasedir]));
    halt(halt_write_err);
  end;
  try
    Messages.DoVerbose(Format('Trying to write output file %s...',[params.OutputFile]));
    try
      { will be overwritten by the tzipper }
      outfile:=FileCreate(params.OutputFile,fmCreate or fmShareDenyWrite,438);
      FileClose(outfile);
    except
      Messages.DoError(Format(SCantCreateFile,[params.OutputFile]));
      halt(halt_write_err);
    end;
    try
      Messages.DoVerbose(Format('Writing output file %s...',[params.OutputFile]));
      resources.FileName:=params.OutputFile;
      resources.ZipAllFiles;
    except
      on e : Exception do
      begin
        if e.Message='' then msg:=e.ClassName
        else msg:=e.Message;
        Messages.DoError(msg);
        halt(halt_write_err);
      end;
    end;
    Messages.DoVerbose(Format('Output file %s written',[params.OutputFile]));
  finally
    if removedirlevel>0 then
      begin
        if removedirlevel>1 then
          begin
            if removedirlevel>2 then
              RemoveDir(resbasedir);
            RemoveDir('org'+DirectorySeparator+'freepascal');
          end;
        RemoveDir('org');
      end;
  end;
end;

procedure Cleanup;
begin
  Messages.DoVerbose('Cleaning up');
  if Resources<>nil then Resources.Free;
  if SourceFiles<>nil then SourceFiles.Free;
  if Params<>nil then Params.Free;
end;

var before, elapsed : longint;

begin
  try
    before:=GetCurrentTimeMsec;
    ParseParams;
    LoadSourceFiles;
    WriteOutputFile;
    elapsed:=GetCurrentTimeMsec-before;
    if elapsed<0 then elapsed:=24*3600*1000 + elapsed;
    Messages.DoVerbose(Format('Time elapsed: %d.%d seconds',[elapsed div 1000,(elapsed mod 1000) div 10]));
  finally
    Cleanup;
  end;
end.

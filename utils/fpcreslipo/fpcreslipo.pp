{

    FPCResLipo - Free Pascal External Resource Thinner
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program fpcreslipo;

{$MODE OBJFPC} {$H+}

uses
  SysUtils, Classes, paramparser, msghandler, sourcehandler,
  resource, externalreader, externalwriter;
  
const
  halt_no_err = 0;
  halt_param_err = 1;
  halt_read_err = 2;
  halt_write_err = 3;

  progname = 'fpcreslipo';
  progversion = '1.0';

  fpcversion = {$INCLUDE %FPCVERSION%};
  host_arch = {$INCLUDE %FPCTARGETCPU%};
  host_os = {$INCLUDE %FPCTARGETOS%};
  build_date = {$INCLUDE %DATE%};

var
  params : TParameters = nil;
  sourcefiles : TSourceFiles = nil;
  outResources : TResources = nil;

procedure ShowVersion;
begin
  writeln(progname+' - external resource file thinner, version '+progversion+' ['+build_date+'], FPC '+fpcversion);
  writeln('Host platform: '+host_os+' - '+host_arch);
  writeln('Copyright (c) 2008 by Giulio Bernardi.');
end;

procedure ShowHelp;
begin
  ShowVersion;
  writeln('Syntax: '+progname+' [options] <inputfile> [<inputfile>...] -o <outputfile>');
  writeln;
  writeln('Options:');
  writeln('  --help, -h, -?     Show this screen.');
  writeln('  --version, -V      Show program version.');
  writeln('  --verbose, -v      Be verbose.');
  writeln('  --output, -o <x>   Set the output file name.');
  writeln('  --endian, -e <x>   Set shared file endianess (big, little)');
  writeln('                       default is big');
  writeln;
  writeln('Example:');
  writeln('  '+progname+' myprog.i386.fpcres myprog.powerpc.fpcres -o myprog.fpcres');
  writeln;
  writeln('  strips common resources from the two input files and puts them in the');
  writeln('  output file');
end;

const
  SOutputFileAlreadySet = 'Output file name already set.';
  SUnknownParameter = 'Unknown parameter ''%s''';
  SArgumentMissing = 'Argument missing for option ''%s''';
  SUnknownEndianess = 'Unknown endianess ''%s''';
  SNoInputFiles = 'No input files';
  STooFewInputFiles = 'At least two input files must be specified';
  SNoOutputFile = 'No output file name specified';
  SCantOpenFile = 'Can''t open file ''%s''';
  SUnknownInputFormat = 'No known file format detected for file ''%s''';
  SCantCreateFile = 'Can''t create file ''%s''';

function GetCurrentTimeMsec : longint;
var h,m,s,ms : word;
begin
  DecodeTime(Time,h,m,s,ms);
  Result:=h*3600*1000 + m*60*1000 + s*1000 + ms;
end;

procedure CheckInputFiles;
begin
  if params.InputFiles.Count<2 then
  begin
    case params.InputFiles.Count of
      0 : Messages.DoError(SNoInputFiles);
      1 : Messages.DoError(STooFewInputFiles);
    end;
    halt(halt_param_err);
  end;
end;

procedure CheckOutputFile;
begin
  if params.OutputFile<>'' then exit;
  Messages.DoError(SNoOutputFile);
  halt(halt_param_err);
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
    on e : EUnknownEndianessException do msg:=Format(SUnknownEndianess,[e.Message]);
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
    i : integer;
begin
  msg:='';
  sourcefiles:=TSourceFiles.Create;
  try
    for i:=0 to params.InputFiles.Count-1 do
      sourcefiles.NewSourceFile(params.InputFiles[i]);
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

procedure ProcessFiles;
begin
  Messages.DoVerbose('processing input files...');
  outResources:=TResources.Create;
  sourcefiles.Process(outResources);
  Messages.DoVerbose('input files processed.');
end;

function WriteOutputFile : boolean;
var aStream : TFileStream;
    aWriter : TExternalResourceWriter;
    msg : string;
begin
  if outResources.Count=0 then
  begin
    Result:=false;
    Messages.DoVerbose('Nothing to do');
    exit;
  end;
  Result:=true;
  Messages.DoVerbose(Format('Trying to create file %s...',[params.OutputFile]));
  try
    aStream:=TFileStream.Create(params.OutputFile,fmCreate or fmShareDenyWrite);
  except
    Messages.DoError(Format(SCantCreateFile,[params.OutputFile]));
    halt(halt_write_err);
  end;
  try
    aWriter:=TExternalResourceWriter.Create;
    aWriter.Endianess:=params.Endianess;
    try
      try
        outResources.WriteToStream(aStream,aWriter);
      except
        on e : Exception do
        begin
          if e.Message='' then msg:=e.ClassName
          else msg:=e.Message;
          Messages.DoError(msg);
          halt(halt_write_err);
        end;
      end;
      Messages.DoVerbose(Format('%d resources written.',[outResources.Count]));
      Messages.DoVerbose(Format('File %s written',[params.OutputFile]));
    finally
      aWriter.Free;
    end;
  finally
    aStream.Free;
  end;
  FreeAndNil(outResources);
end;

procedure UpdateFiles;
var msg : string;
begin
  try
    sourcefiles.Update;
  except
    on e : ECantCreateFileException do msg:=Format(SCantCreateFile,[e.Message]);
    on e : Exception do
    begin
      if e.Message='' then msg:=e.ClassName
      else msg:=e.Message;
    end;
  end;
  if msg<>'' then
  begin
    Messages.DoError(msg);
    halt(halt_write_err);
  end;
end;

procedure Cleanup;
begin
  Messages.DoVerbose('Cleaning up');
  if OutResources<>nil then OutResources.Free;
  if SourceFiles<>nil then SourceFiles.Free;
  if Params<>nil then Params.Free;
end;

var before, elapsed : longint;

begin
  try
    before:=GetCurrentTimeMsec;
    ParseParams;
    LoadSourceFiles;
    ProcessFiles;
    if WriteOutputFile then
      UpdateFiles;
    elapsed:=GetCurrentTimeMsec-before;
    if elapsed<0 then elapsed:=24*3600*1000 + elapsed;
    Messages.DoVerbose(Format('Time elapsed: %d.%d seconds',[elapsed div 1000,(elapsed mod 1000) div 10]));
  finally
    Cleanup;
  end;
end.

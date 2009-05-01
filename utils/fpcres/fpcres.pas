{

    FPCRes - Free Pascal Resource Converter
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Note: This program is not the old fpcres by Simon Kissel }

program fpcres;

{$MODE OBJFPC} {$H+}

uses
  SysUtils, Classes, paramparser, target, msghandler, sourcehandler,
  resource,
//readers
  resreader, coffreader, winpeimagereader, elfreader, machoreader,
  externalreader, dfmreader,
//writers
  reswriter, coffwriter, elfwriter, machowriter, externalwriter,
//misc
  elfconsts, cofftypes, machotypes, externaltypes
  ;
  
const
  halt_no_err = 0;
  halt_param_err = 1;
  halt_read_err = 2;
  halt_write_err = 3;

  progname = 'fpcres';
  progversion = '2.0'; //to distinguish from the old fpcres

  fpcversion = {$INCLUDE %FPCVERSION%};
  host_arch = {$INCLUDE %FPCTARGETCPU%};
  host_os = {$INCLUDE %FPCTARGETOS%};
  build_date = {$INCLUDE %DATE%};

var
  params : TParameters = nil;
  resources : TResources = nil;
  sourcefiles : TSourceFiles = nil;

procedure ShowVersion;
begin
  writeln(progname+' - resource file converter, version '+progversion+' ['+build_date+'], FPC '+fpcversion);
  writeln('Host platform: '+host_os+' - '+host_arch);
  writeln('Copyright (c) 2008 by Giulio Bernardi.');
end;

procedure ShowHelp;
begin
  ShowVersion;
  writeln('Syntax: '+progname+' [options] <inputfile> [<inputfile>...] [-o <outputfile>]');
  writeln;
  writeln('Options:');
  writeln('  --help, -h, -?     Show this screen.');
  writeln('  --version, -V      Show program version.');
  writeln('  --verbose, -v      Be verbose.');
  writeln('  --input, -i <x>    Ignored for compatibility.');
  writeln('  --output, -o <x>   Set the output file name.');
  writeln('  -of <format>       Set the output file format. Supported formats:');
  writeln('                       res, elf, coff, mach-o, external');
  writeln('  --arch, -a <name>  Set object file architecture. Supported architectures:');
  writeln('                       i386, x86_64, arm (coff)');
  writeln('                       i386, x86_64, powerpc, powerpc64, arm, armeb, m68k,');
  writeln('                       sparc, alpha, ia64 (elf)');
  writeln('                       i386, x86_64, powerpc, powerpc64 (mach-o)');
  writeln('                       bigendian, littleendian (external)');
  writeln('  @<file>            Read more options from file <file>');
  writeln('Default output target: '+TargetToStr(currenttarget));
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
  
  SCantCreateFile = 'Can''t create file ''%s''';

function GetCurrentTimeMsec : longint;
var h,m,s,ms : word;
begin
  DecodeTime(Time,h,m,s,ms);
  Result:=h*3600*1000 + m*60*1000 + s*1000 + ms;
end;

procedure CheckTarget;
begin
  //if user explicitally set a format, use it
  if params.Target.objformat<>ofNone then
    CurrentTarget.objformat:=params.Target.objformat;
  //if no machine was specified, check if current is ok for this format,
  //otherwise pick the default one for that format
  if params.Target.machine=mtNone then
  begin
    if not (CurrentTarget.machine in ObjFormats[CurrentTarget.objformat].machines) then
      CurrentTarget.machine:=GetDefaultMachineForFormat(CurrentTarget.objformat);
  end
  else CurrentTarget.machine:=params.Target.machine;

  if not (CurrentTarget.machine in ObjFormats[CurrentTarget.objformat].machines) then
  begin
    Messages.DoError(Format(SFormatArchMismatch,[
     MachineToStr(CurrentTarget.machine),ObjFormatToStr(CurrentTarget.objformat)]));
    halt(halt_param_err);
  end;
  Messages.DoVerbose('target set to '+TargetToStr(CurrentTarget));
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
    ObjFormats[CurrentTarget.objformat].ext);
  if lowercase(tmp)=lowercase(params.InputFiles[0]) then
    tmp:=tmp+ObjFormats[CurrentTarget.objformat].ext;
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

  CheckTarget;
  CheckInputFiles;
  CheckOutputFile;

  Messages.DoVerbose('finished parsing command line parameters');
end;

procedure LoadSourceFiles;
var msg : string;
begin
  msg:='';
  resources:=TResources.Create;
  sourcefiles:=TSourceFiles.Create;
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

function SetUpResWriter : TResResourceWriter;
begin
  Result:=TResResourceWriter.Create;
end;

function SetUpElfWriter : TElfResourceWriter;
begin
  Result:=TElfResourceWriter.Create;
  case CurrentTarget.machine of
//    mtnone :
    mti386 : Result.MachineType:=emti386;
    mtx86_64 : Result.MachineType:=emtx86_64;
    mtppc : Result.MachineType:=emtppc;
    mtppc64 : Result.MachineType:=emtppc64;
    mtarm : Result.MachineType:=emtarm;
    mtarmeb : Result.MachineType:=emtarmeb;
    mtm68k : Result.MachineType:=emtm68k;
    mtsparc : Result.MachineType:=emtsparc;
    mtalpha : Result.MachineType:=emtalpha;
    mtia64 : Result.MachineType:=emtia64;
  end;
end;

function SetUpCoffWriter : TCoffResourceWriter;
begin
  Result:=TCoffResourceWriter.Create;
  case CurrentTarget.machine of
//    mtnone :
    mti386 : Result.MachineType:=cmti386;
    mtarm : Result.MachineType:=cmtarm;
    mtx86_64 : Result.MachineType:=cmtx8664;
  end;
end;

function SetUpMachOWriter : TMachOResourceWriter;
begin
  Result:=TMachOResourceWriter.Create;
  case CurrentTarget.machine of
//    mtnone :
    mti386 : Result.MachineType:=mmti386;
    mtx86_64 : Result.MachineType:=mmtx86_64;
    mtppc : Result.MachineType:=mmtpowerpc;
    mtppc64 : Result.MachineType:=mmtpowerpc64;
    mtarm : Result.MachineType:=mmtarm;
  end;
end;


function SetUpExternalWriter : TExternalResourceWriter;
begin
  Result:=TExternalResourceWriter.Create;
  case CurrentTarget.machine of
//    mtnone :
    mtBigEndian : Result.Endianess:=EXT_ENDIAN_BIG;
    mtLittleEndian : Result.Endianess:=EXT_ENDIAN_LITTLE;
  end;
end;

procedure WriteOutputFile;
var aStream : TFileStream;
    aWriter : TAbstractResourceWriter;
    msg : string;
begin
  Messages.DoVerbose(Format('Trying to create output file %s...',[params.OutputFile]));
  try
    aStream:=TFileStream.Create(params.OutputFile,fmCreate or fmShareDenyWrite);
  except
    Messages.DoError(Format(SCantCreateFile,[params.OutputFile]));
    halt(halt_write_err);
  end;
  try
    Messages.DoVerbose('Setting up resource writer...');
    case CurrentTarget.objformat of
      ofRes   : aWriter:=SetUpResWriter;
      ofElf   : aWriter:=SetUpElfWriter;
      ofCoff  : aWriter:=SetUpCoffWriter;
      ofMachO : aWriter:=SetUpMachOWriter;
      ofExt   : aWriter:=SetUpExternalWriter;
    end;
    try
      Messages.DoVerbose(Format('Writing output file %s...',[params.OutputFile]));
      try
        resources.WriteToStream(aStream,aWriter);
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
      aWriter.Free;
    end;
  finally
    aStream.Free;
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

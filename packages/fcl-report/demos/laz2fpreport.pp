program laz2fpreport;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, fpjson, fpreport, fplazreport, fpreportstreamer;

type

  { TLazToFPReport }

  TLazToFPReport = class(TCustomApplication)
  Private
    FLazReport : TFPLazReport;
    FInputFile,
    FOutputFile : String;
    FFormatOutput : Boolean;
    FVerbose : Boolean;
    procedure Convert;
    procedure DoVerbose(Sender: TOBject; const Msg: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Const aMsg :String); virtual;
  end;

{ TLazToFPReport }

procedure TLazToFPReport.DoRun;

var
  ErrorMsg: String;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hi:o:vf', ['help','input:','output:','verbose','format']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    WriteHelp(ErrorMsg);
  FInputFile:=GetOptionValue('i','input');
  if FInputFile='' then
    WriteHelp('No input file specified.');
  FOutputFile:=GetOptionValue('o','output');
  If FOutputFile='' then
    FOutputFile:=ChangeFileExt(FinputFile,'.json');
  FFormatOutput:=HasOption('f','format');
  FVerbose:=HasOption('v','verbose');
  if FVerbose then
    FLazReport.OnLog:=@DoVerbose;
  Convert;
  Terminate;
end;

procedure TLazToFPReport.Convert;

Var
  S : TFPReportJSONStreamer;
  F : TFileStream;
  J : TJSONStringType;

begin

  FLazReport.LoadFromFile(FInputFile);
  F:=Nil;
  S:=TFPReportJSONStreamer.Create(Self);
  try
    FLazReport.WriteElement(S);
    if FFormatOutput then
      J:=S.JSON.FormatJSON()
    else
      J:=S.JSON.AsJSON;
    F:=TFileStream.Create(FOutputFile,fmCreate);
    F.Write(J[1],Length(J)); // Single byte type.
  finally
    F.Free;
    S.Free;
  end;
end;

procedure TLazToFPReport.DoVerbose(Sender: TOBject; const Msg: String);
begin
  if FVerbose then
    Writeln(StdErr,Msg);
end;

constructor TLazToFPReport.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FLazReport:=TFPLazReport.Create(Self);
end;

destructor TLazToFPReport.Destroy;
begin
  FreeAndNil(FLazReport);
  inherited Destroy;
end;

procedure TLazToFPReport.WriteHelp(const aMsg: String);

begin
  if (aMsg<>'') then
    Writeln('Error : ',aMsg);
  writeln('Usage: ', ExeName, ' [options] -i filename');
  Writeln('Where options are: ');
  Writeln('-f --format           Write formatted JSON to output file');
  Writeln('-h --help             This help message');
  Writeln('-i --input=filename   input file name, must be a .lrf file, in XML format.');
  Writeln('-o --output=filename  output file name.');
  Writeln('                      If not specified, input file with extension changed to .json is used.');
  Writeln('-v --verbose          Print some diagnostic information');
  Halt(Ord(aMsg<>''));
end;

var
  Application: TLazToFPReport;

begin
  Application:=TLazToFPReport.Create(nil);
  Application.Title:='LazReport to FPReport Converter';
  Application.Run;
  Application.Free;
end.


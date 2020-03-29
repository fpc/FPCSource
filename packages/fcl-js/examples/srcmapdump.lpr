program srcmapdump;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, JSSrcMap;

type

  { TSrcMapDump }

  TSrcMapDump = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DumpSrcMap(SrcMapFile, aGeneratedFilename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSrcMapDump }

procedure TSrcMapDump.DoRun;
var
  ErrorMsg, SrcMapFilename, GeneratedFilename: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hs:g:', 'help srcmap: generatedfile:');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not HasOption('s','srcmap') then begin
    writeln('missing -s >srcmap>');
    WriteHelp;
    Terminate;
    Exit;
  end;
  SrcMapFilename:=ExpandFileName(GetOptionValue('s','srcmap'));

  if not HasOption('g','generatedfile') then begin
    writeln('missing -g <generatedfile>');
    WriteHelp;
    Terminate;
    Exit;
  end;
  GeneratedFilename:=ExpandFileName(GetOptionValue('g','generatedfile'));

  DumpSrcMap(SrcMapFilename,GeneratedFilename);

  // stop program loop
  Terminate;
end;

procedure TSrcMapDump.DumpSrcMap(SrcMapFile, aGeneratedFilename: string);
var
  SrcMap: TSourceMap;
  GenSrc: TStringList;
  i: Integer;
  GenSrcLine, InfoLine: String;
begin
  GenSrc:=TStringList.Create;
  SrcMap:=TSourceMap.Create(aGeneratedFilename);
  try
    SrcMap.LoadFromFile(SrcMapFile);
    GenSrc.LoadFromFile(aGeneratedFilename);
    for i:=1 to GenSrc.Count do begin
      GenSrcLine:=GenSrc[i-1];
      DebugSrcMapLine(i,GenSrcLine,SrcMap,InfoLine);
      writeln(GenSrcLine);
      writeln(InfoLine);
    end;
  finally
    SrcMap.Free;
    GenSrc.Free;
  end;
end;

constructor TSrcMapDump.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSrcMapDump.Destroy;
begin
  inherited Destroy;
end;

procedure TSrcMapDump.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
  writeln;
  writeln('-s <srcmap>');
  writeln('-g <generatedfile>');
end;

var
  Application: TSrcMapDump;
begin
  Application:=TSrcMapDump.Create(nil);
  Application.Title:='SrcMapDump';
  Application.Run;
  Application.Free;
end.


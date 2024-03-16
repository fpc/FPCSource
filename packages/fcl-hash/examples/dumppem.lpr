{
  This file is part of the Free Component Library.
  Copyright (c) 2023 by the Free Pascal team.

  Demo to dump the contents of a PEM file

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program dumppem;

uses sysutils, types, custapp, classes, fpasn, basenenc;

Type

  { TDumpApplication }

  TDumpApplication = class(TCustomApplication)
  private
    FRaw : Boolean;
    procedure DumpAsnList(aList: TStrings);
    procedure DumpASNList(Prefix: string; aList: TStrings; AStart, aCount: Integer);
    function GetBytes(FN: String) : TBytes;
    procedure ShowASN(FN: String);
    procedure Usage(aError : String);

  Protected
    procedure DoRun; override;
  end;


Procedure TDumpApplication.DumpASNList(Prefix : string; aList : TStrings; AStart,aCount : Integer);

var
  I : Integer;
  ASize,aType : Integer;

begin
  I:=aStart;
  While I<=aCount do
    begin
    ASNParse_GetItem(aList,i,aType,aSize);
    writeln(Prefix,'ASNType=',ASNtypeToString(aType),' ASNSize=',aSize,' S="',aList[i],'"');
    Inc(I);
    end;
end;

Procedure TDumpApplication.DumpAsnList(aList : TStrings);

begin
  DumpASNList('',aList,0,aList.Count-1);
end;

function TDumpApplication.GetBytes(FN : String) : TBytes;

Var
  L : TStrings;
  S : String;
  I : Integer;

begin
  if FRaw then
    Result:=GetFileContents(FN)
  else
    begin
    L:=TStringList.Create;
    try
      L.LoadFromFile(FN);
      S:='';
      For I:=1 to L.Count-2 do
        S:=S+Trim(L[i]);
      Result:=BaseNenc.Base64.Decode(S);
    finally
      L.Free;
    end;
    end;
end;

Procedure TDumpApplication.ShowASN(FN : String);

var
  Bytes : TBytes;
  L : TStrings;

begin
  Writeln('ASN.1 Contents of '+FN);
  Bytes:=GetBytes(FN);
  if HasOption('d','debug') then
    Writeln(ASNDebug(Bytes))
  else
    begin
    L:=TStringList.Create;
    try
      ASNParse(Bytes,L);
      DumpAsnList(L);
    finally
      L.Free;
    end;
    end;
end;

procedure TDumpApplication.Usage(aError: String);
begin
  if (aError<>'') then
    Writeln(aError);
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [options] FileName1 [FileName2..FileNameN]');
  Writeln('Where options is one of:');
  Writeln('-h  --help  This help');
  Writeln('-r  --raw   Treat filenames as raw byte dumps (default is to assume .PEM format)');
  Writeln('-d  --debug Use debug routine to dump content.');
  ExitCode:=Ord(aError<>'');
end;

procedure TDumpApplication.DoRun ;

const
  Short = 'hrd';
  Long : Array of string = ('help','raw','debug');

var
  S,FN : String;
  NonOpt : TStringDynArray;

begin
  Terminate;
  S:=CheckOptions(Short,Long);
  if S='' then
    begin
    NonOpt:=GetNonOptions(Short,Long);
    if 0=Length(NonOpt) then
      S:='One or more filenames must be specified';
    end;
  if (S<>'') or HasOption('h','help') then
    Usage(S)
  else
    begin
    for FN in nonOpt do
      ShowAsn(FN);
    end;
end;

begin
  With TDumpApplication.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.

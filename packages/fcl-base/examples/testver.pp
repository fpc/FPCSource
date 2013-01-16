program testver;

{$mode objfpc}{$H+}

uses
  Classes, fileinfo
  { you can add units after this };

{$R *.res}

Var
  I : Integer;

  PV : TProgramVersion;
  VQ : TVersionQuad;

begin
  With TFileVersionInfo.Create(Nil) do
    try
      FileName:=ParamStr(0);
      Translation:='123';
      Filter.Add('Fileversion');
      Enabled:=True;
      Writeln('Using translation : ',TRanslation);
      For I:=0 to VersionStrings.Count-1 do
        Writeln(VersionStrings[i]);
    Finally
      Free;
    end;
  if GetProgramVersion(VQ) then
    begin
    Writeln('Version: ',VQ[1],'.',VQ[2],'.',VQ[3],' build: ',VQ[4]);
    Writeln('Version (short) : ',versionQuadToStr(VQ));
    Writeln('Compare to 1.0 : ',CompareVersionQuads(VQ,StrToVersionQuad('1.0.0.0')));
    end;
  if GetProgramVersion(PV) then
    begin
    Writeln('Version: ',PV.Major,'.',PV.Minor,'.',PV.Revision,' build: ',PV.Build);
    Writeln('Version (short) : ',ProgramVersionToStr(PV));
    Writeln('Compare to 1.0 : ',CompareProgramVersion(VQ,StrToVersionQuad('1.0.0.0')));
    end;
end.


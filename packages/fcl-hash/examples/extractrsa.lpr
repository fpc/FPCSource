{
  This file is part of the Free Component Library.
  Copyright (c) 2023 by the Free Pascal team.

  Demo to dump the RSA private key in a PEM file

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program extractrsa;

uses sysutils, types, custapp, classes, fpasn, basenenc, fprsa;

Type

  { TExtractRSAApplication }

  TDumpApplication = class(TCustomApplication)
  private
    FRaw : Boolean;
    procedure DumpX509RSA(Key: TX509RSAPrivateKey);
    function GetBytes(FN: String) : TBytes;
    procedure ShowASN(FN: String);
    procedure Usage(aError : String);

  Protected
    procedure DoRun; override;
  end;


Procedure TDumpApplication.DumpX509RSA(Key : TX509RSAPrivateKey);

  function DumpBytes(B : TBytes) : string;

  begin
    Result:=Base16.Encode(B,True);
  end;

begin
  Writeln('X509 RSA Private Key:');
  With key do
    begin
    Writeln('Version: ',Version);
    Writeln('Modulus         (m/n): ',DumpBytes(Modulus));
    Writeln('PublicExponent  (e)  : ',DumpBytes(PublicExponent));
    Writeln('PrivateExponent (d)  : ',DumpBytes(PrivateExponent));
    Writeln('Prime1          (p)  : ',DumpBytes(Prime1));
    Writeln('Prime2          (q)  : ',DumpBytes(Prime2));
    Writeln('Exponent1       (dp) : ',DumpBytes(Exponent1));
    Writeln('Exponent2       (dq) : ',DumpBytes(Exponent2));
    Writeln('Coefficient     (qi) : ',DumpBytes(Coefficient));
    end;
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
  RSA : TX509RSAPrivateKey;

begin
  Writeln('ASN.1 Contents of '+FN);
  Bytes:=GetBytes(FN);
  X509RsaPrivateKeyInitFromDER(Rsa,Bytes);
  DumpX509RSA(RSA);
end;

procedure TDumpApplication.Usage(aError: String);
begin
  if (aError<>'') then
    Writeln(aError);
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [options] FileName1 [FileName2..FileNameN]');
  Writeln('Where options is one of:');
  Writeln('-h  --help  This help');
  Writeln('-r  --raw   Treat filenames as raw byte dumps (default is to assume .PEM format)');
  ExitCode:=Ord(aError<>'');
end;

procedure TDumpApplication.DoRun ;

const
  Short = 'hr';
  Long : Array of string = ('help','raw');

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

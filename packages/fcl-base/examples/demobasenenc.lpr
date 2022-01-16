{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by Michael Van Canneyt,
    member of the Free Pascal development team

    Demo program for Base16,Base32,Base32-hex,Base32-crockford, Base64,Base64url encoding/decoding

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program demobasenenc;

uses sysutils, basenenc, typinfo, custapp, Classes;

Type

  { TDemoApp }

  TDemoApp = Class(TCustomApplication)
  private
    FEncoder : TStandardEncoder;
    FPadding,
      FDoDecode : Boolean;
    procedure ParseOptions;
    procedure Usage(aError: String);
  Protected
    Procedure DoRun; override;
  end;

{ TDemoApp }

procedure TDemoApp.Usage(aError : String);

Var
  Enc : TStandardEncoder;

begin
  if (aError<>'') then
    Writeln('Error : ',aError);
  Writeln('Usage ', ExtractFileName(Self.ExeName),' [options]');
  Writeln('Where options is one or more of');
  Writeln('-h --help            This message');
  Writeln('-e --encode=ENC      Encode input to output using algorithm ENC, one of');
  For Enc in TStandardEncoder do
    Writeln('                   ',Copy(GetEnumName(TypeInfo(TStandardEncoder),Ord(Enc)),3,MaxInt));
  Writeln('-d --decode=ENC      Encode input to output using algorithm ENC, one of the above');
  Writeln('-i --input=FileName  Set input filename. Required.');
  Writeln('-o --output=FileName Set input filename. Required.');
  Writeln('-p --pad             Use Padding when encoding.');
  ExitCode:=Ord(aError<>'');
end;

procedure TDemoApp.ParseOptions;

Var
  S : String;
  I : Integer;

begin
  FDoDecode:=False;
  S:=CheckOptions('hi:o:e:p',['help','input:','output:','encode:','decode:','pad']);
  if Not (HasOption('i','input') and HasOption('o','output')) then
    S:='Input and output filename are required';
  if (S<>'') or HasOption('h','help') then
    begin
    Usage(S);
    Exit;
    end;
  FPadding:=HasOption('p','pad');
  S:=GetOptionValue('e','encode');
  if S='' then
    begin
    S:=GetOptionValue('d','decode');
    if S<>'' then
      FDoDecode:=True;
    end;
  if (S='') then
    S:='base64';
  i:=GetEnumValue(TypeInfo(TStandardEncoder),S);
  if I=-1 then
    i:=GetEnumValue(TypeInfo(TStandardEncoder),'se'+S);
  if I=-1 then
    begin
    Usage('Not a valid algorithm: '+s);
    Exit;
    end;
  FEncoder:=TStandardEncoder(I);
end;

procedure TDemoApp.DoRun;


Var
  B,Res : TBytes;
  F : TFileStream;
  Coder : TAlphabetEncoder;

begin
  B:=[];
  Terminate;
  Parseoptions;
  if ExitCode<>0 then
    exit;
  F:=TFileStream.Create(GetOptionValue('i','input'),fmOpenRead or fmShareDenyWrite);
  try
    SetLength(B,F.Size);
    F.ReadBuffer(B,F.Size);
  finally
    F.Free;
  end;
  Coder:=GetStandardEncoder(FEncoder);
  if FDoDecode then
    Res:=Coder.Decode(PByte(B),Length(B))
  else
    Res:=TEncoding.UTF8.GetAnsiBytes(Coder.Encode(PByte(B),Length(B),FPadding));
  F:=TFileStream.Create(GetOptionValue('o','output'),fmCreate);
  try
    F.WriteBuffer(Res,Length(Res))
  finally
    F.Free;
  end;
end;

begin
  CustomApplication:=TDemoApp.Create(Nil);
  CustomApplication.Initialize;
  CustomApplication.Run;
  CustomApplication.Free;
end.


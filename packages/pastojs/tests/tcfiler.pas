{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript precompile class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
   ./testpas2js --suite=TTestPrecompile.TestPC_EmptyUnit
}
unit tcfiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, PScanner, PasResolver, PasResolveEval, PParser,
  FPPas2Js, Pas2JsFiler,
  tcmodules;

type

  { TCustomTestPrecompile }

  TCustomTestPrecompile = Class(TCustomTestModule)
  private
    FInitialFlags: TPJUInitialFlags;
    FPJUReader: TPasToJsReader;
    FPJUWriter: TPasToJsWriter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure WriteReadUnit; virtual;
    procedure StartParsing; override;
  public
    property PJUWriter: TPasToJsWriter read FPJUWriter write FPJUWriter;
    property PJUReader: TPasToJsReader read FPJUReader write FPJUReader;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
  end;

  { TTestPrecompile }

  TTestPrecompile = class(TCustomTestPrecompile)
  published
    procedure Test_Base256VLQ;
    procedure TestPC_EmptyUnit;
  end;

implementation

{ TCustomTestPrecompile }

procedure TCustomTestPrecompile.SetUp;
begin
  inherited SetUp;
  FInitialFlags:=TPJUInitialFlags.Create;
end;

procedure TCustomTestPrecompile.TearDown;
begin
  FreeAndNil(FPJUWriter);
  FreeAndNil(FPJUReader);
  FreeAndNil(FInitialFlags);
  inherited TearDown;
end;

procedure TCustomTestPrecompile.WriteReadUnit;
var
  ms: TMemoryStream;
  PJU: string;
  ReadResolver: TPasResolver;
  ReadFileResolver: TFileResolver;
  ReadScanner: TPascalScanner;
  ReadParser: TPasParser;
begin
  FPJUWriter:=TPasToJsWriter.Create;
  FPJUReader:=TPasToJsReader.Create;
  ms:=TMemoryStream.Create;
  try
    try
      PJUWriter.WriteModule(Engine,ms,InitialFlags);
    except
      on E: Exception do
      begin
        {$IFDEF VerbosePas2JS}
        writeln('TCustomTestPrecompile.WriteReadUnit WRITE failed');
        {$ENDIF}
        Fail('Write failed: '+E.Message);
      end;
    end;

    try
      SetLength(PJU,ms.Size);
      System.Move(ms.Memory^,PJU[1],length(PJU));

      writeln('TCustomTestPrecompile.WriteReadUnit PJU START-----');
      writeln(dbgmem(PJU));
      writeln('TCustomTestPrecompile.WriteReadUnit PJU END-------');

      ReadFileResolver:=TFileResolver.Create;
      ReadScanner:=TPascalScanner.Create(ReadFileResolver);
      ReadResolver:=TPasResolver.Create;
      ReadParser:=TPasParser.Create(ReadScanner,ReadFileResolver,ReadResolver);
      ReadResolver.CurrentParser:=ReadParser;
      try
        PJUReader.ReadModule(ReadResolver,PJU);
      finally
        ReadParser.Free;
        ReadScanner.Free;
        ReadResolver.Free; // free parser before resolver
        ReadFileResolver.Free;
      end;
    except
      on E: Exception do
      begin
        {$IFDEF VerbosePas2JS}
        writeln('TCustomTestPrecompile.WriteReadUnit READ failed');
        {$ENDIF}
        Fail('Read failed: '+E.Message);
      end;
    end;
  finally
    ms.Free;
  end;
end;

procedure TCustomTestPrecompile.StartParsing;
begin
  inherited StartParsing;
  FInitialFlags.ParserOptions:=Parser.Options;
  FInitialFlags.ModeSwitches:=Scanner.CurrentModeSwitches;
  FInitialFlags.BoolSwitches:=Scanner.CurrentBoolSwitches;
  // ToDo: defines
  FInitialFlags.ResolverOptions:=Engine.Options;
  FInitialFlags.PasTojsOptions:=Converter.Options;
  FInitialFlags.TargetPlatform:=Converter.TargetPlatform;
  FInitialFlags.TargetProcessor:=Converter.TargetProcessor;
end;

{ TTestPrecompile }

procedure TTestPrecompile.Test_Base256VLQ;

  procedure Test(i: MaxPrecInt);
  var
    s: String;
    p: PByte;
    j: NativeInt;
  begin
    s:=EncodeVLQ(i);
    p:=PByte(s);
    j:=DecodeVLQ(p);
    if i<>j then
      Fail('Encode/DecodeVLQ OrigIndex='+IntToStr(i)+' Code="'+s+'" NewIndex='+IntToStr(j));
  end;

  procedure TestStr(i: MaxPrecInt; Expected: string);
  var
    Actual: String;
  begin
    Actual:=EncodeVLQ(i);
    AssertEquals('EncodeVLQ('+IntToStr(i)+')',Expected,Actual);
  end;

var
  i: Integer;
begin
  TestStr(0,#0);
  TestStr(1,#2);
  TestStr(-1,#3);
  for i:=-8200 to 8200 do
    Test(i);
  Test(High(MaxPrecInt));
  Test(High(MaxPrecInt)-1);
  Test(Low(MaxPrecInt)+2);
  Test(Low(MaxPrecInt)+1);
  //Test(Low(MaxPrecInt)); such a high number is not needed by pastojs
end;

procedure TTestPrecompile.TestPC_EmptyUnit;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  ConvertUnit;
  WriteReadUnit;
end;

Initialization
  RegisterTests([TTestPrecompile]);
end.


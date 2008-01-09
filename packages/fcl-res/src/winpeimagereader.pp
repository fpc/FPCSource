{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader for PE image files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit winpeimagereader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource, coffreader;

type

  { TWinPEImageResourceReader }

  TWinPEImageResourceReader = class (TAbstractResourceReader)
  private
    fDescription: string;
    fExtensions: string;
    fCoffReader : TCoffResourceReader;
  protected
    function CheckDosStub(aStream : TStream) : boolean;
    function CheckPESignature(aStream : TStream) : boolean;
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

const
  PESignatureOffset = $3C;

{ TWinPEImageResourceReader }

function TWinPEImageResourceReader.CheckDosStub(aStream: TStream): boolean;
var w : word;
    lw : longword;
begin
  Result:=false;
  try
    aStream.ReadBuffer(w,2);
  except
    on e : EReadError do exit;
  end;

  {$IFDEF ENDIAN_BIG}
  w:=SwapEndian(w);
  {$ENDIF}
  Result:=w=$5A4D; //MZ
  if not Result then exit;
  aStream.Position:=PESignatureOffset;
  aStream.ReadBuffer(lw,4);
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  aStream.Position:=lw;
end;

function TWinPEImageResourceReader.CheckPESignature(aStream: TStream): boolean;
var lw : longword;
begin
  aStream.ReadBuffer(lw,4);
  {$IFDEF ENDIAN_BIG}
  lw:=SwapEndian(lw);
  {$ENDIF}
  Result:=lw=$00004550; //PE#0#0
end;

function TWinPEImageResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TWinPEImageResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TWinPEImageResourceReader.Load(aResources: TResources; aStream: TStream);
begin
  if not CheckMagic(aStream) then
    raise EResourceReaderWrongFormatException.Create('');
  CallSubReaderLoad(fCoffReader,aResources,aStream);
end;

function TWinPEImageResourceReader.CheckMagic(aStream: TStream): boolean;
begin
  Result:=CheckDosStub(aStream) and CheckPESignature(aStream);
end;

constructor TWinPEImageResourceReader.Create;
begin
  fExtensions:='.exe .dll .bpl';
  fDescription:='Win32 PE image resource reader';
  fCoffReader:=TCoffResourceReader.Create;
end;

destructor TWinPEImageResourceReader.Destroy;
begin
  fCoffReader.Free;
end;

initialization
  TResources.RegisterReader('.exe',TWinPEImageResourceReader);
  TResources.RegisterReader('.dll',TWinPEImageResourceReader);
  TResources.RegisterReader('.bpl',TWinPEImageResourceReader);

end.

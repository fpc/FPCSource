{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi
    Copyright (c) 2024 by Nikolay Nikolov

    Resource writer for WebAssembly files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit wasmwriter;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC} {$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Resources.Resource, System.Resources.WebAssembly.Consts, System.Resources.WebAssembly.Types;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, resource, wasmconsts, wasmtypes;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TWasmResourceWriter }

  TWasmResourceWriter = class(TAbstractResourceWriter)
  private
    fExtensions : string;
    fDescription : string;
    FWasmSections: array [TWasmSectionID] of TMemoryStream;
    procedure WriteWasmSection(aStream: TStream; wsid: TWasmSectionID);
    procedure WriteWasmSectionIfNotEmpty(aStream: TStream; wsid: TWasmSectionID);
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

procedure WriteUleb(aStream: TStream; v: uint64);
var
  b: byte;
begin
  repeat
    b:=byte(v) and 127;
    v:=v shr 7;
    if v<>0 then
      b:=b or 128;
    aStream.WriteByte(b);
  until v=0;
end;

{ TWasmResourceWriter }

procedure TWasmResourceWriter.WriteWasmSection(aStream: TStream;
  wsid: TWasmSectionID);
var
  b: byte;
begin
  b:=ord(wsid);
  aStream.WriteByte(b);
  WriteUleb(aStream,FWasmSections[wsid].size);
  aStream.CopyFrom(FWasmSections[wsid],0);
end;

procedure TWasmResourceWriter.WriteWasmSectionIfNotEmpty(aStream: TStream;
  wsid: TWasmSectionID);
begin
  if FWasmSections[wsid].size>0 then
    WriteWasmSection(aStream,wsid);
end;

function TWasmResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TWasmResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TWasmResourceWriter.Write(aResources: TResources; aStream: TStream);
const
  DataSegmentCount = 0;
begin
  WriteUleb(FWasmSections[wsiData],DataSegmentCount);
  WriteUleb(FWasmSections[wsiDataCount],DataSegmentCount);

  aStream.WriteBuffer(WasmModuleMagic,SizeOf(WasmModuleMagic));
  aStream.WriteBuffer(WasmVersion,SizeOf(WasmVersion));
  WriteWasmSection(aStream,wsiDataCount);
  WriteWasmSection(aStream,wsiData);
end;

constructor TWasmResourceWriter.Create;
var
  i: TWasmSectionID;
begin
  fExtensions:='.o .or';
  fDescription:='WebAssembly resource writer';
  for i in TWasmSectionID do
    FWasmSections[i] := TMemoryStream.Create;
end;

destructor TWasmResourceWriter.Destroy;
var
  i: TWasmSectionID;
begin
  for i in TWasmSectionID do
    FreeAndNil(FWasmSections[i]);
  inherited Destroy;
end;

initialization
  TResources.RegisterWriter('.o',TWasmResourceWriter);
  TResources.RegisterWriter('.or',TWasmResourceWriter);

end.

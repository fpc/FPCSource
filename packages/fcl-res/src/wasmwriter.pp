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
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Write(aResources : TResources; aStream : TStream); override;
  public
    constructor Create; override;
  end;

implementation

{ TWasmResourceWriter }

function TWasmResourceWriter.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TWasmResourceWriter.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TWasmResourceWriter.Write(aResources: TResources; aStream: TStream);
begin
  aStream.WriteBuffer(WasmModuleMagic,SizeOf(WasmModuleMagic));
  aStream.WriteBuffer(WasmVersion,SizeOf(WasmVersion));
end;

constructor TWasmResourceWriter.Create;
begin
  fExtensions:='.o .or';
  fDescription:='WebAssembly resource writer';
end;

initialization
  TResources.RegisterWriter('.o',TWasmResourceWriter);
  TResources.RegisterWriter('.or',TWasmResourceWriter);

end.

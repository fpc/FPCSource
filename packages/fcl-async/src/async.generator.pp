{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Generators and Yielding Iterators (Phase 4, Step 1)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

unit async.generator;

interface

uses
  SysUtils, Classes, async.types, async.fiber;

type
  { Generic Generator for yielding sequences }
  generic TGenerator<T> = class
  public type
    TYieldProc = procedure(const AValue: T) of object;
    TProducer = procedure(const Yield: TYieldProc);
    TMethodProducer = procedure(const Yield: TYieldProc) of object;

    TEnumerator = record
    private
      FGenerator: specialize TGenerator<T>;
    public
      function MoveNext: Boolean;
      function GetCurrent: T;
      property Current: T read GetCurrent;
    end;
  private
    FProducer: TProducer;
    FMethodProducer: TMethodProducer;
    FFiber: TFiber;
    FCurrentValue: T;
    FHasValue: Boolean;
    FDone: Boolean;
    FException: Exception;

    procedure DoYield(const AValue: T);
    procedure FiberProc;
  public
    constructor Create(const AProducer: TProducer; AStackSize: Cardinal = 64 * 1024); overload;
    constructor Create(const AProducer: TMethodProducer; AStackSize: Cardinal = 64 * 1024); overload;
    destructor Destroy; override;

    function MoveNext: Boolean;
    function GetCurrent: T;
    function GetEnumerator: TEnumerator;

    property Current: T read GetCurrent;
    property Done: Boolean read FDone;
  end;

implementation

{ TGenerator<T> }

procedure TGenerator.DoYield(const AValue: T);
begin
  FCurrentValue := AValue;
  FHasValue := True;
  TFiber.Yield;
end;

procedure TGenerator.FiberProc;
begin
  try
    if Assigned(FProducer) then
      FProducer(@DoYield)
    else if Assigned(FMethodProducer) then
      FMethodProducer(@DoYield);
  except
    on E: Exception do
      FException := Exception(AcquireExceptionObject);
  end;
  FDone := True;
end;

constructor TGenerator.Create(const AProducer: TProducer; AStackSize: Cardinal);
begin
  inherited Create;
  FProducer := AProducer;
  FMethodProducer := nil;
  FDone := False;
  FHasValue := False;
  FException := nil;
  FFiber := TFiber.Create(@FiberProc, AStackSize);
end;

constructor TGenerator.Create(const AProducer: TMethodProducer; AStackSize: Cardinal);
begin
  inherited Create;
  FProducer := nil;
  FMethodProducer := AProducer;
  FDone := False;
  FHasValue := False;
  FException := nil;
  FFiber := TFiber.Create(@FiberProc, AStackSize);
end;

destructor TGenerator.Destroy;
begin
  FFiber.Free;
  FException.Free;
  inherited Destroy;
end;

function TGenerator.MoveNext: Boolean;
begin
  if FDone then
    Exit(False);

  FHasValue := False;
  FFiber.Resume;

  if FException <> nil then
    raise FException;

  Result := FHasValue and not FDone;
end;

function TGenerator.GetCurrent: T;
begin
  Result := FCurrentValue;
end;

function TGenerator.GetEnumerator: TEnumerator;
begin
  Result.FGenerator := Self;
end;

{ TGenerator<T>.TEnumerator }

function TGenerator.TEnumerator.MoveNext: Boolean;
begin
  Result := FGenerator.MoveNext;
end;

function TGenerator.TEnumerator.GetCurrent: T;
begin
  Result := FGenerator.GetCurrent;
end;

end.

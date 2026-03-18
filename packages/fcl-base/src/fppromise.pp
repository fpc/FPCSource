{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team
    
    Promise API for Object Pascal

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit FPPromise;
{$ENDIF}

{$mode objfpc}
{$h+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

type
  TSimpleProc = reference to procedure;
  TRejectProc = reference to procedure(AReason: string);

  { TFPPromiseBase — abstract base for both promise variants }

  TFPPromiseBase = class
  protected
    FCatchHandler: TRejectProc;
    FFinallyHandler: TSimpleProc;
    FRunning: Boolean;
    FSettled: Integer;
    FStepCount: Integer;
    function TrySettle: Boolean;
    procedure DoReject(const AError: string);
    procedure DoComplete;
    procedure ExecuteStep(AIndex: Integer); virtual; abstract;
  public
    procedure Run;
  end;

  { TFPPromise — simple promise (parameterless resolve) }

  TFPAsyncStep = reference to procedure(AResolve: TSimpleProc; AReject: TRejectProc; AComplete: TSimpleProc);
  TFPAsyncSimpleStep = reference to procedure(AResolve: TSimpleProc; AReject: TRejectProc);
  TFPAsyncStepArray = array of TFPAsyncStep;

  TFPPromise = class(TFPPromiseBase)
  private
    FSteps: TFPAsyncStepArray;
  protected
    procedure ExecuteStep(AIndex: Integer); override;
  public
    constructor Create(AStep: TFPAsyncStep); overload;
    constructor Create(AStep: TFPAsyncSimpleStep); overload;
    destructor Destroy; override;
    class function New(AStep: TFPAsyncStep): TFPPromise; overload; static;
    class function New(AStep: TFPAsyncSimpleStep): TFPPromise; overload; static;
    function ThenDo(AStep: TFPAsyncStep): TFPPromise; overload;
    function ThenDo(AStep: TFPAsyncSimpleStep): TFPPromise; overload;
    function Catch(AHandler: TRejectProc): TFPPromise;
    function FinallyDo(AHandler: TSimpleProc): TFPPromise;
  end;

  { generic TFPValuePromise<T> — value-carrying promise }

  generic TFPValuePromise<T> = class(TFPPromiseBase)
  public type
    TResolveProc = reference to procedure(AValue: T);
    TFirstStep = reference to procedure(AResolve: TResolveProc; AReject: TRejectProc; AComplete: TSimpleProc);
    TNextStep = reference to procedure(AValue: T; AResolve: TResolveProc; AReject: TRejectProc; AComplete: TSimpleProc);
    TNextStepArray = array of TNextStep;
  private
    FFirstStep: TFirstStep;
    FNextSteps: TNextStepArray;
    FCurrentValue: T;
  protected
    procedure ExecuteStep(AIndex: Integer); override;
  public
    constructor Create(AStep: TFirstStep);
    destructor Destroy; override;
    class function New(AStep: TFirstStep): TFPValuePromise; static;
    function ThenDo(AStep: TNextStep): TFPValuePromise;
    function Catch(AHandler: TRejectProc): TFPValuePromise;
    function FinallyDo(AHandler: TSimpleProc): TFPValuePromise;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SyncObjs
{$ELSE}
  SyncObjs
{$ENDIF}
  ;

{ TFPPromiseBase }

function TFPPromiseBase.TrySettle: Boolean;
var
  LPrevious: Integer;
begin
  LPrevious := InterlockedCompareExchange(FSettled, 1, 0);
  Result := LPrevious = 0;
end;

procedure TFPPromiseBase.DoReject(const AError: string);
begin
  if not TrySettle() then
    Exit;
  try
    if Assigned(FCatchHandler) then
      FCatchHandler(AError);
    if Assigned(FFinallyHandler) then
      FFinallyHandler();
  finally
    Free;
  end;
end;

procedure TFPPromiseBase.DoComplete;
begin
  if not TrySettle() then
    Exit;
  try
    if Assigned(FFinallyHandler) then
      FFinallyHandler();
  finally
    Free;
  end;
end;

procedure TFPPromiseBase.Run;
begin
  if FRunning then
    Exit;
  FRunning := True;
  ExecuteStep(0);
end;

{ TFPPromise }

class function TFPPromise.New(AStep: TFPAsyncStep): TFPPromise;
begin
  Result := TFPPromise.Create(AStep);
end;

class function TFPPromise.New(AStep: TFPAsyncSimpleStep): TFPPromise;
begin
  Result := TFPPromise.Create(AStep);
end;

constructor TFPPromise.Create(AStep: TFPAsyncStep);
begin
  inherited Create();
  SetLength(FSteps, 4);
  FSteps[0] := AStep;
  FStepCount := 1;
end;

constructor TFPPromise.Create(AStep: TFPAsyncSimpleStep);
begin
  Create(
    procedure(AResolve: TSimpleProc; AReject: TRejectProc; AComplete: TSimpleProc)
    begin
      AStep(AResolve, AReject);
    end
  );
end;

destructor TFPPromise.Destroy;
begin
  FSteps := nil;
  inherited;
end;

function TFPPromise.ThenDo(AStep: TFPAsyncStep): TFPPromise;
begin
  if FStepCount >= Length(FSteps) then
    SetLength(FSteps, FStepCount * 2);
  FSteps[FStepCount] := AStep;
  Inc(FStepCount);
  Result := Self;
end;

function TFPPromise.ThenDo(AStep: TFPAsyncSimpleStep): TFPPromise;
begin
  Result := ThenDo(
    procedure(AResolve: TSimpleProc; AReject: TRejectProc; AComplete: TSimpleProc)
    begin
      AStep(AResolve, AReject);
    end
  );
end;

function TFPPromise.Catch(AHandler: TRejectProc): TFPPromise;
begin
  FCatchHandler := AHandler;
  Result := Self;
end;

function TFPPromise.FinallyDo(AHandler: TSimpleProc): TFPPromise;
begin
  FFinallyHandler := AHandler;
  Result := Self;
end;

procedure TFPPromise.ExecuteStep(AIndex: Integer);
var
  LCalled: Integer;
begin
  if InterlockedCompareExchange(FSettled, 0, 0) <> 0 then
    Exit;

  if AIndex >= FStepCount then
  begin
    DoComplete();
    Exit;
  end;

  LCalled := 0;

  try
    FSteps[AIndex](
      procedure
      begin
        if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
          Exit;
        ExecuteStep(AIndex + 1);
      end,
      procedure(AError: string)
      begin
        if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
          Exit;
        DoReject(AError);
      end,
      procedure
      begin
        if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
          Exit;
        DoComplete();
      end
    );
  except
    on E: Exception do
    begin
      if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
        Exit;
      DoReject(E.Message);
    end;
  end;
end;

{ TFPValuePromise }

class function TFPValuePromise.New(AStep: TFirstStep): TFPValuePromise;
begin
  Result := TFPValuePromise.Create(AStep);
end;

constructor TFPValuePromise.Create(AStep: TFirstStep);
begin
  inherited Create();
  FFirstStep := AStep;
  FStepCount := 1;
end;

destructor TFPValuePromise.Destroy;
begin
  FFirstStep := nil;
  FNextSteps := nil;
  inherited;
end;

function TFPValuePromise.ThenDo(AStep: TNextStep): TFPValuePromise;
var
  LNextCount: Integer;
begin
  LNextCount := FStepCount - 1;
  if LNextCount >= Length(FNextSteps) then
    if Length(FNextSteps) = 0 then
      SetLength(FNextSteps, 4)
    else
      SetLength(FNextSteps, Length(FNextSteps) * 2);
  FNextSteps[LNextCount] := AStep;
  Inc(FStepCount);
  Result := Self;
end;

function TFPValuePromise.Catch(AHandler: TRejectProc): TFPValuePromise;
begin
  FCatchHandler := AHandler;
  Result := Self;
end;

function TFPValuePromise.FinallyDo(AHandler: TSimpleProc): TFPValuePromise;
begin
  FFinallyHandler := AHandler;
  Result := Self;
end;

procedure TFPValuePromise.ExecuteStep(AIndex: Integer);
var
  LCalled: Integer;
begin
  if InterlockedCompareExchange(FSettled, 0, 0) <> 0 then
    Exit;

  if AIndex >= FStepCount then
  begin
    DoComplete();
    Exit;
  end;

  LCalled := 0;

  try
    if AIndex = 0 then
      FFirstStep(
        procedure(AValue: T)
        begin
          if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
            Exit;
          FCurrentValue := AValue;
          ExecuteStep(AIndex + 1);
        end,
        procedure(AError: string)
        begin
          if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
            Exit;
          DoReject(AError);
        end,
        procedure
        begin
          if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
            Exit;
          DoComplete();
        end
      )
    else
      FNextSteps[AIndex - 1](
        FCurrentValue,
        procedure(AValue: T)
        begin
          if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
            Exit;
          FCurrentValue := AValue;
          ExecuteStep(AIndex + 1);
        end,
        procedure(AError: string)
        begin
          if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
            Exit;
          DoReject(AError);
        end,
        procedure
        begin
          if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
            Exit;
          DoComplete();
        end
      );
  except
    on E: Exception do
    begin
      if InterlockedCompareExchange(LCalled, 1, 0) <> 0 then
        Exit;
      DoReject(E.Message);
    end;
  end;
end;

end.

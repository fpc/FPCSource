{ %target=win32,win64 }
{ TDataSet.SetBlockReadSize is protected and virtual: a descendant in another unit overrides it, and
  setting BlockReadSize through a TDataSet reference reaches the override }
program tdbblockread1;

{$mode delphi}{$h+}

uses
  SysUtils, DB;

type
  TBareDataSet = class(TDataSet)
  protected
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    procedure SetBlockReadSize(AValue: Integer); override;
  public
    Calls: Integer;
    Seen: Integer;
  end;

function TBareDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grEOF;
end;

procedure TBareDataSet.InternalClose;
begin
end;

procedure TBareDataSet.InternalHandleException;
begin
end;

procedure TBareDataSet.InternalInitFieldDefs;
begin
end;

procedure TBareDataSet.InternalOpen;
begin
end;

function TBareDataSet.IsCursorOpen: Boolean;
begin
  Result := False;
end;

procedure TBareDataSet.SetBlockReadSize(AValue: Integer);
begin
  Inc(Calls);
  Seen := AValue;
  inherited SetBlockReadSize(AValue);
end;

procedure Check(aOk: Boolean; const aWhat: string; aExit: Integer);
begin
  if not aOk then begin
    Writeln('failed: ', aWhat);
    Halt(aExit);
  end;
end;

var
  lBare: TBareDataSet;
  lDataSet: TDataSet;
  lRaised: Boolean;
begin
  lBare := TBareDataSet.Create(nil);
  lBare.Seen := -1;
  lDataSet := lBare;

  lDataSet.BlockReadSize := 0;
  Check((lBare.Calls = 1) and (lBare.Seen = 0), 'BlockReadSize := 0 through a TDataSet reaches the override', 1);
  Check(lDataSet.BlockReadSize = 0, 'BlockReadSize reads 0', 2);

  { on an inactive dataset the inherited body refuses a positive size, after the override has seen it }
  lRaised := False;
  try
    lDataSet.BlockReadSize := 3;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  Check((lBare.Calls = 2) and (lBare.Seen = 3), 'BlockReadSize := 3 reaches the override', 3);
  Check(lRaised, 'an inactive dataset refuses a positive BlockReadSize', 4);

  lBare.Free;
  Writeln('ok');
end.

program project1;

{$mode objfpc}{$H+}

type

  { TOrgObject }

  TOriginal=class
  protected
    procedure SetReadOnly(const AValue: boolean); virtual;
  public
    property readonly:boolean write SetReadOnly;
  end;

  { TDerived }

  TDerived=class(TOriginal)
  protected
    procedure SetReadOnly(const AValue: boolean); override;
  end;

var
 count1, count2: longint;

{ TDerived }

procedure TDerived.SetReadOnly(const AValue: boolean);
begin
  if (count2>0) then
    halt(1);
  inc(count2);
  WriteLn('TDerived.SetReadOnly');
  inherited;
  inherited ReadOnly := AValue;
end;

{ TOrgObject }

procedure TOriginal.SetReadOnly(const AValue: boolean);
begin
  if (count1>1) then
    halt(2);
  inc(count1);
  WriteLn('TOriginal.SetReadOnly');
end;

var
  D: TDerived;
begin
  D := TDerived.Create;
  D.ReadOnly := True;
  D.Free;
  if (count1<>2) or
     (count2<>1) then
    halt(3);
end.

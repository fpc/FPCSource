program tstringlistexchange;

{$mode objfpc}{$H+}

uses
  Classes;

type
  TMyStringList = class(TStringList)
  protected
    ExchangeCount: LongInt;
    procedure ExchangeItems(aLeft, aRight: Integer); override;
  end;
  
procedure TMyStringList.ExchangeItems(aLeft, aRight: Integer);
begin
  Inc(ExchangeCount);
  inherited ExchangeItems(aLeft, aRight);
end;

procedure FillStringList(aList: TStrings);
begin
  aList.Add('Beta');
  aList.Add('Gamma');
  aList.Add('Alpha');
  aList.Add('Delta');
end;

type
  TDummy = class
    ExchangeCount: LongInt;
    procedure Change(aSender: TObject);
  end;
  
procedure TDummy.Change(aSender: TObject);
begin
  Inc(ExchangeCount);
end;

var
  sl: TStringList;
  msl: TMyStringList;
  dummy: TDummy;
begin
  dummy := TDummy.Create;
  try
    sl := TStringList.Create;
    try
      FillStringList(sl);
      sl.OnChange := @dummy.Change;
      sl.Sort;
      // only OnChange call in TStringList.Sort
      if dummy.ExchangeCount <> 1 then
        Halt(1);
    finally
      sl.Free;
    end;

    dummy.ExchangeCount := 0;
    
    msl := TMyStringList.Create;
    try
      FillStringList(msl);
      msl.OnChange := @dummy.Change;
      msl.Sort;
      // TMyStringList.ExchangeItems called 5 times
      if msl.ExchangeCount <> 5 then
        Halt(1);
      // OnChange called once in Sort
      if dummy.ExchangeCount <> 1 then
        Halt(1);
    finally
      msl.Free;
    end;
  finally
    dummy.Free;
  end;
end.
{$MODE objfpc}
uses SysUtils, Classes;
type
  TFirstClass = class
    constructor Create;
    destructor Destroy; override;
  end;
  TSecondClass = class(TFirstClass)
    constructor Create;
    destructor Destroy; override;
  end;

constructor TFirstClass.Create;
begin
  raise Exception.Create('');
end;

destructor TFirstClass.Destroy;
begin
  WriteLn('TFirstClass.Destroy');
  inherited Destroy;
end;

constructor TSecondClass.Create;
begin
  inherited Create;
end;

destructor TSecondClass.Destroy;
begin
  WriteLn('TSecondClass.Destroy');
end;

var
  o: TSecondClass;
begin
  try
    try
      o := TSecondClass.Create;
    finally
      o.Free;
    end;
  except
    on e: Exception do
      WriteLn('Exception: ', e.Message);
  end;
end.

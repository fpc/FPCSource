program tisconstvalue2;

{$mode Delphi}

// example taken from https://stackoverflow.com/a/30417597

type
  TFlavor = (Tasty, Nasty);

  TIntegerHelper = record helper for Integer
  private
    function GetTastyPoint: Integer;
    function GetNastyPoint: Integer;
  public
    function GetSomething(Flavor: TFlavor): Integer; inline;
  end;

  function TIntegerHelper.GetTastyPoint: Integer;
  begin
    Result := 10;
  end;

  function TIntegerHelper.GetNastyPoint: Integer;
  begin
    Result := -10;
  end;

  function TIntegerHelper.GetSomething(Flavor: TFlavor): Integer;
  begin
    if IsConstValue(Flavor) then
    begin
      if Flavor = Tasty then
        Result := Self.GetTastyPoint
      else
        Result := Self.GetNastyPoint;
    end
    else
    begin
      Result := 0;
    end;
  end;

var
  i: Integer;
  n: TFlavor;

begin
  i := 100000.GetSomething(Tasty);
  if i <> 10 then
    Halt(1);

  n := Tasty;
  i := 100000.GetSomething(Nasty);
  if i <> -10 then
    Halt(2);

  i := 100000.GetSomething(n);
  if i <> 0 then
    Halt(3);

  Writeln('Ok');
end.

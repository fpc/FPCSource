{ a helper nested in a record with a generic method, all declared in the program,
  stays active after the record's declaration and after its methods }
program trhlp47;

{$mode delphi}

type
  TRec = record
    v: Integer;
  end;

  TBox = record
    v: Integer;
    class procedure Dummy<T>; static;
    function Inner(const aRec: TRec): Integer;
  public type
    THelper = record helper for TRec
      function Which: Integer;
    end;
  end;

  TOther = record
    v: Integer;
  end;

  TBox2 = record
    v: Integer;
    class procedure Dummy<T>; static;
  public type
    TOtherHelper = record helper for TOther
      function Which: Integer;
    end;
  end;

  TOtherGlobalHelper = record helper for TOther
    function Which: Integer;
  end;

class procedure TBox.Dummy<T>;
begin
end;

function TBox.Inner(const aRec: TRec): Integer;
begin
  Result := aRec.Which;
end;

function TBox.THelper.Which: Integer;
begin
  Result := 1;
end;

class procedure TBox2.Dummy<T>;
begin
end;

function TBox2.TOtherHelper.Which: Integer;
begin
  Result := 2;
end;

function TOtherGlobalHelper.Which: Integer;
begin
  Result := 3;
end;

var
  r: TRec;
  o: TOther;
  b: TBox;
begin
  r.v := 0;
  if r.Which <> 1 then
    Halt(1);
  if b.Inner(r) <> 1 then
    Halt(2);
  if r.Which <> 1 then
    Halt(3);
  o.v := 0;
  if o.Which <> 3 then
    Halt(4);
  Writeln('ok');
end.

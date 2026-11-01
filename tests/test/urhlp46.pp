unit urhlp46;

{$mode delphi}

interface

type
  TRec = record
    v: Integer;
  end;

  TBox = record
    v: Integer;
    class operator Implicit(a: Integer): TBox;
    class procedure Dummy<T>; static;
  public type
    THelper = record helper for TRec
      function Which: Integer;
    end;
  end;

  TRec2 = record
    v: Integer;
  end;

  TBox2 = record
    v: Integer;
    class operator Implicit(const a: TBox2): Integer;
    class procedure Dummy<T>; static;
  public type
    THelper2 = record helper for TRec2
      function Which: Integer;
    end;
  end;

implementation

class operator TBox2.Implicit(const a: TBox2): Integer;
begin
  Result := a.v;
end;

class procedure TBox2.Dummy<T>;
begin
end;

function TBox2.THelper2.Which: Integer;
begin
  Result := 2;
end;

class operator TBox.Implicit(a: Integer): TBox;
begin
  Result.v := a;
end;

class procedure TBox.Dummy<T>;
begin
end;

function TBox.THelper.Which: Integer;
begin
  Result := 1;
end;

end.

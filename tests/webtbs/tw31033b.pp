{ %NORUN }

program tw31033;

{$MODESWITCH RESULT}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH OUT}

Type
  generic TGData<T> = record
    public type
      PGData = ^specialize TGData<T>;

    public
      b: T;
      n: PGData;
  end;

generic Procedure DoSomething<T>(out p: specialize TGData<T>.PGData); forward;

generic Procedure DoSomething<T>(out p: specialize TGData<T>.PGData);
  Begin
    new(p);

    p^.b := default(T);
    p^.n := nil
  End;

var
  pl: specialize TGData<LongInt>.PGData;
  ps: specialize TGData<ShortString>.PGData;
begin
  specialize DoSomething<LongInt>(pl);
  specialize DoSomething<ShortString>(pl);
end.

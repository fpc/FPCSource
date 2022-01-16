{ %NORUN }

{$MODE DELPHI}

type
  TA<T: record> = record
  end;

  TUnamanagedRec = record
    x: integer;
  end;

  TManagedRec = record
    s: string;
  end;

  TEnum = (e1, e2, e3);

  TObj = object
  end;

var
  a: TA<TUnamanagedRec>;
  b: TA<TManagedRec>;
  d: TA<Single>;
  e: TA<Integer>;
  f: TA<TEnum>;
  g: TA<TObj>;
begin
end.
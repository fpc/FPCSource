{ %NORUN }

program tw30524a;

{$ifdef FPC}
{$MODE DELPHI}
{$endif}

{uses
  Generics.Defaults;}

type
  Tuple<T> = record
    Item1: T;
    class operator Equal( a, b: Tuple<T> ): Boolean; // FPC Error: Compilation raised exception internally
    class operator NotEqual( a, b: Tuple<T> ): Boolean;
  end;

  Tuple = record
    class function Create<T>( Item1: T ): Tuple<T>; overload; static;
  end;

class function Tuple.Create<T>( Item1: T ): Tuple<T>;
begin
  Result.Item1 := Item1;
end;

class operator Tuple<T>.Equal( a, b: Tuple<T> ): Boolean;
begin
  Result := False;//TEqualityComparer<T>.Default.Equals( a.Item1, b.Item1 );
end;

class operator Tuple<T>.NotEqual( a, b: Tuple<T> ): Boolean;
begin
  Result := not( a = b );
end;

var
  t: Tuple<LongInt>;
begin
  t := Tuple.Create<LongInt>(42);
end.

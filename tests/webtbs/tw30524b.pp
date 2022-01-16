{ %NORUN }

program tw30524b;

{$MODE objfpc}
{$modeswitch advancedrecords}

{uses
  Generics.Defaults;}

type
  generic Tuple<T> = record
    Item1: T;
    class operator =( a, b: specialize Tuple<T> ): Boolean; // FPC Error: Compilation raised exception internally
    class operator <>( a, b: specialize Tuple<T> ): Boolean;
  end;

  TTuple = record
    generic class function Create<T>( Item1: T ): specialize Tuple<T>; overload; static;
  end;

generic class function TTuple.Create<T>( Item1: T ): specialize Tuple<T>;
begin
  Result.Item1 := Item1;
end;

class operator Tuple.=( a, b: specialize Tuple<T> ): Boolean;
begin
  Result := False;//TEqualityComparer<T>.Default.Equals( a.Item1, b.Item1 );
end;

class operator Tuple.<>( a, b: specialize Tuple<T> ): Boolean;
begin
  Result := not( a = b );
end;

var
  t: specialize Tuple<LongInt>;
begin
  t := TTuple.specialize Create<LongInt>(42);
end.

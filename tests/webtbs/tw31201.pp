unit tw31201;

{$mode delphi}{$H+}

interface

type
  Tuple<T> = record
    Item1: T;
  end;

  Tuple = record
    class function Create<T>(Item1: T): Tuple<T>; overload; static;
  end;

implementation

class function Tuple.Create<T>(Item1: T): Tuple<T>;
begin
  Result.Item1:=Item1;
end;

initialization
  Writeln(Tuple.Create<LongInt>(42).Item1);
end.

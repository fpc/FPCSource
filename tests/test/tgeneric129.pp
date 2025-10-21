{ %NORUN }

program tgeneric129;

{$mode delphi}

type
  Vector<T> = record
  public
    class operator Add(const left: TArray<T>; const right: Vector<T>): Vector<T>; inline;
  end;

class operator Vector<T>.Add(const left: TArray<T>; const right: Vector<T>): Vector<T>;
begin
end;

begin
end.

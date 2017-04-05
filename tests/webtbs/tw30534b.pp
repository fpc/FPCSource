{ %NORUN }

{$mode delphi}

type
  TNullable<T: record> = record
  public type
    PT = ^T;
  public
    class operator Implicit(A: PT): TNullable<T>;
    class operator Implicit(A: T): TNullable<T>;
    class operator Equal(A: TNullable<T>; B: PT): Boolean;
    class operator Equal(A: TNullable<T>; B: T): Boolean;
  end;

  TRec = record
  end;

class operator TNullable<T>.Implicit(A: PT): TNullable<T>;
begin
end;

class operator TNullable<T>.Implicit(A: T): TNullable<T>;
begin
end;

class operator TNullable<T>.Equal(A: TNullable<T>; B: PT): Boolean;
begin
end;

class operator TNullable<T>.Equal(A: TNullable<T>; B: T): Boolean;
begin
end;

var
  x: TNullable<integer>;
  y: TNullable<TRec>;
begin
  x := nil;
  x := 1;
  if x = nil then ;
  if x = 1 then ;
end.
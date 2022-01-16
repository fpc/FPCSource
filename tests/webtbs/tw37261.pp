program Project1;

{$mode delphi}

type

  { TClass }

  TClass = class
  public
    class function ToString<T>(A: T; B: string): string; reintroduce;
  end;

{ TClass }

class function TClass.ToString<T>(A: T; B: string): string;
begin end;

begin
  TClass.ToString<integer>(1, '');
end.

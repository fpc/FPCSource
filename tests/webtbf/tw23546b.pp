{ %FAIL }

{$MODE DELPHI}

type
  Wrapper<T> = record
  strict private
    type ThisWrapper = Wrapper<T>;  { #21539 }
  public
    FWrapper: array [0..0] of ThisWrapper;
  end;

var
  wr: Wrapper<Byte>;

begin
end.

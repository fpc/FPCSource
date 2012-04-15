program tw18567;

{$mode delphi}

type
  TSomeRecord <TData> = record
    data: TData;
    class operator Explicit(a: TData) : TSomeRecord <TData>;
  end;

  class operator TSomeRecord <TData>.Explicit (a: TData): TSomeRecord <TData>;
  begin

  end;

begin
end.

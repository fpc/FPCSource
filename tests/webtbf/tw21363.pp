{ %FAIL }

{$MODE DELPHI}

type
  TWrapper<T> = record
    procedure Z(a: TWrapper);
      { TWrapper is an unspecialized generic type identifier and should not be
        accepted here }
  end;

procedure TWrapper<T>.Z(a: TWrapper);
begin
end;

var
  wr: TWrapper<Integer>;

begin
  wr.Z(wr);
end.

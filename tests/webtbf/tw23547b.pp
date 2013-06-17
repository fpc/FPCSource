{ %FAIL }

{$MODE DELPHI}

type
  SmallWrapper<T> = class end;
  Wrapper<T: SmallWrapper<>> = record end;

begin
end.

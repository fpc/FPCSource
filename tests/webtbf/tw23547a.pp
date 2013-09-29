{ %FAIL }

{$MODE DELPHI}

type Wrapper<T: Wrapper<>> = record end;

begin
end.

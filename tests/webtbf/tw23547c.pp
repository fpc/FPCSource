{ %FAIL }

{$MODE DELPHI}

type
  SmallWrapper<T> = class end;
  Wrapper<T: SmallWrapper<Byte, Byte>> = record end;

begin
end.

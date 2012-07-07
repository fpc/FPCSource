{ %FAIL }

program tw22219;
{$MODE DELPHI}

type
  TWrapper<P, Q> = record end;
  TWrapper<R> = record end;
  AmbiguousPointer = ^TWrapper;

var
  Z: AmbiguousPointer;

begin

end.

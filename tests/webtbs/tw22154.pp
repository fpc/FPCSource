program tw22154;

{$MODE DELPHI}

type
  TWrapper<T> = class
    procedure Z;
  end;

procedure TWrapper<T>.Z;
const
  A0: array [0..0] of Integer = (0);     { OK }
  A1: array [0..1] of Integer = (0, 1);  { Comma not exepcted }
begin
end;

begin
end.

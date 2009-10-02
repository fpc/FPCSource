{ %fail }

{$mode objfpc}

type
  tc = class
    strict private
      fa: longint;
  end;

  tcc = class(tc)
    property a: longint read fa;
  end;

begin
end.

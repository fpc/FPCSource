{ %NORUN }

program tw30832;

{$mode objfpc}

type
  generic TTest<T> = class
    procedure Test;
  end;

procedure TTest.Test;
begin
  try
    Writeln(Default(T));
  finally
    Writeln('Finally');
  end;
end;

generic procedure Test<T>;
begin
  try
    Writeln(Default(T));
  finally
    Writeln('Finally');
  end;
end;

begin
  specialize Test<LongInt>;
end.

{ %opt=-O3 }
program Project1;
{$mode objfpc}{$H+}

uses SysUtils;

var
  bar: boolean;

procedure Foo;
begin
  while true do begin
    try
      try
        try
          if bar then
            raise Exception.Create('');
          Randomize;
        finally
          Randomize;
          try
            Randomize;
          finally
            Randomize;
          end;
        end;
        Randomize;
      finally
        Randomize;
      end;
      Randomize;
    except
      bar := false;
      Randomize;
      halt(0);
    end;
  end;
end;

begin
  bar := true;
  Foo;
end.

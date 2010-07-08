{ %opt=-Si }

program project1;

{$mode objfpc}{$H+}

var
  global: boolean;

  function TestInlineExcept : boolean; inline;
  begin
    try
      result := true;
    except
      result := false;
    end;
    global:=true;
  end;

begin
  writeln('before');
  if TestInlineExcept then begin
    writeln('TestInlineExcept: true');
  end else begin
    writeln('TestInlineExcept: false');
  end;
  writeln('after');
  if not global then
    halt(1);
end.

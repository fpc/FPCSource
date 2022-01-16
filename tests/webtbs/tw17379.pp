{$mode macpas}
{$warnings off}
program recursivefunctionparam;

function first( function test( theint: integer): boolean): integer;
begin
  test(2);
end;

function find: integer;
  var
    l: longint;

  function test( theint: integer): boolean;
  begin
    if (theint = 1) then
      first( test)
    else
      begin
        writeln('nested procvar call, l = ', l);
        if l<>1234567890 then
          halt(1);
      end;
    find:=0;
  end;

begin
  l:=1234567890;
  test(1)
end;

begin
  find;
end.

{ %NORUN }

program tfuncref32;

{$modeswitch functionreferences-}

type
  reference = record
  end;

  someref = reference;

var
  somevar: reference;

begin

end.

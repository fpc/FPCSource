{ %FAIL }
{ This shouldc not compile, cdecl'ed constructor are not allowed }

{$mode objfpc}

type
  tmyobject = object
    constructor create; cdecl;
  end;


  constructor tmyobject.create;cdecl;
  begin
  end;


Begin
end.

{ %FAIL }
{ This shouldc not compile, cdecl'ed constructor are not allowed }

{$mode objfpc}

type
  tmyclass = class
    constructor create; cdecl;
  end;


  constructor tmyclass.create;cdecl;
  begin
  end;


Begin
end.

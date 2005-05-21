{ %FAIL }
{ This shouldc not compile, cdecl'ed constructor are not allowed }

{$mode objfpc}

type
  tmyclass = class
    destructor destroy; cdecl;
  end;


  destructor tmyclass.destroy;cdecl;
  begin
  end;


Begin
end.

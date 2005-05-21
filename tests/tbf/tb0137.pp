{ %FAIL }
{ This shouldc not compile, cdecl'ed constructor are not allowed }

{$mode objfpc}

type
  tmyclass = object
    destructor done; cdecl;
  end;


  destructor tmyclass.done;cdecl;
  begin
  end;


Begin
end.

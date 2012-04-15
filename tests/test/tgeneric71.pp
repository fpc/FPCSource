{ %NORUN }

{ This tests that one can use a specialization of another generic which was
  introduced in the currently parsed generic can be used as a parameter type
  in a procedure variable introduced in the current generic as well }
program tgeneric71;

{$mode objfpc}

type
  generic TSomeGeneric<T> = class

  end;

  generic TSomeOtherGeneric<T> = class
  type
    TSomeGenericT = specialize TSomeGeneric<T>;
    TSomeProc = procedure(aParam: TSomeGenericT);
  end;

begin

end.

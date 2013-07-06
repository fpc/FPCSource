{ %NORUN }

program tw24071;

{$mode delphi}

type
  TA<T> = class
  end;

  TB<T> = class
  public
  type
    TC = class;
    TC = class(TA<TC>)
    private
      procedure Foo; virtual; abstract;
    end;
  end;

var
  X: TB<Integer>;
begin
end.

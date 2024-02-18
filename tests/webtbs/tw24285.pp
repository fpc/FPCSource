{ %NORUN }

program tw24285;

{$mode delphi}{$H+}

type
  TA<T> = class
  end;

  TB<T> = class
  public
    type
      TA = class(TA<T>)
      end;
  end;

type
  TBLongInt = TB<LongInt>;

begin

end.

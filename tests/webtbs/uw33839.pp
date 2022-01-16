unit uw33839;
{$mode delphi} // mode does not matter!
interface
  procedure testme(const a:integer);overload;
  procedure testme(const a:cardinal);overload;
  procedure testme(const a:double);overload;
  procedure testme<T>(const a:T);overload;
implementation
  procedure testme(const a:integer);overload;
  begin
    //
  end;

  procedure testme(const a:cardinal);overload;
  begin
    //
  end;

  procedure testme(const a:double);overload;
  begin
    //
  end;

  procedure testme<T>(const a:T);overload;
  begin
    //
  end;
end.

unit uw19697;

{$mode objfpc}{$H+}

interface

type
  generic TGenericClass<T> = class
  private
    class var
      FItems: array of T;
  public
    class procedure Init;
  end;

implementation

class procedure TGenericClass.Init;
begin
  SetLength(FItems, 1);
end;

end.


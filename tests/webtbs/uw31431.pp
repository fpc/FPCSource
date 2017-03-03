unit uw31431;

{$mode objfpc}

interface

type
  TFoo = class
  private
    type
      TID = Integer;
  protected
    type
      TID2 = Integer;
  end;

  generic TBar<T> = class
  private
    FID: TFoo.TID;
    FID2: TFoo.TID2;
  end;

implementation

end.


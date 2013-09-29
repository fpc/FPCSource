{$mode objfpc}
type
  TRec = record
    i : longint;
  end;

  generic TGeneric<T>=class(TObject)
    procedure Test(v : T);
  end;

procedure TGeneric.Test(v : T);
  begin
    with v do
      begin
        i:=1;
      end;
  end;

type
  TC = specialize TGeneric<TRec>;

begin
end.


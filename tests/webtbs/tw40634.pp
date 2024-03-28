{ %NORUN }

program tw40634;
{$mode objfpc}{$H+}
uses uw40634a, uw40634b;

type
  TWatchesSupplier = class(specialize TWatchesSupplierClassTemplate<TObject>, IDbgWatchesSupplierIntf)
  //TWatchesSupplier = class(specialize TWatchesSupplierClassTemplate<TObject>)
  //TWatchesSupplier = class(TNormalClass, IDbgWatchesSupplierIntf)
  protected
    procedure DoFoo;
  end;

{ TWatchesSupplier }

procedure TWatchesSupplier.DoFoo;
begin
  if Monitor <> nil then;
end;

begin
end.


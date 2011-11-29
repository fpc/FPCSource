{ %fail }

unit tw20661;

{$mode objfpc}{$H+}

interface

const
  PrescriptionStorageIntfId = '{C2F3C9F6-657C-4974-841A-4EBFF33B2180}';//'blik.prescriptionstorage';
  DatasetProviderIntfId = '{B0B1501A-9266-48EA-B2E0-7EF23511D799}';

type
  IDatasetPool = interface
    ['{F866EB5B-5B32-438E-918E-A56B031C73DA}']
    procedure ReleaseDataset(Instance: pointer);
  end;

  { TBlikServices }

  TBlikServices = class
  public
    procedure ReleaseDataset(Instance: pointer);
  end;

var
  Services: TBlikServices;

implementation

{ TBlikServices }

procedure TBlikServices.ReleaseDataset(Instance: pointer);
begin
  IDatasetPool.ReleaseDataset(Instance);
end;

end.



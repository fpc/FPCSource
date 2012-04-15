program tw20577a;

{$mode delphi}{$H+}

type

  TSimpleHashBucket<T> = record
     HashCode : Integer;
     Value : T;
  end;

  TSimpleHashBucketArray<T> = array of TSimpleHashBucket<T>;

  { TSimpleHash }

  TSimpleHash<T> = class
    private
    FBuckets : TSimpleHashBucketArray<T>;
  procedure test;
  end;

{ TSimpleHash<T> }

procedure TSimpleHash<T>.test;
var
  oldBuckets : TSimpleHashBucketArray<T>;
begin
  oldBuckets := FBuckets;

end;

begin
end.

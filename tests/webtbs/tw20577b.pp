program tw20577b;

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
    type
      THashBucket = TSimpleHashBucket<T>;
    var
      FBuckets: array of THashBucket;
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

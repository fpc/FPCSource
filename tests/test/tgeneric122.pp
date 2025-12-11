{$mode objfpc}

// test that the check on published property is performed during specialization
// The below should work.

type
  {$M+}
  generic myclass<T> = class
  Private
    FData : T;
  published
    property Data : T read FData;
  end;

  TA = specialize myclass<integer>;
//  TB = specialize myclass<text>;

begin
end.  
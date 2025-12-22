{%FAIL}
{$mode objfpc}

// test that the check on published property is performed during specialization
// The below should not work, since text is not a publishable type.

type
  {$M+}
  generic myclass<T> = class
  Private
    FData : T;
  published
    property Data : T read FData;
  end;

  TB = specialize myclass<text>;

begin
end.  
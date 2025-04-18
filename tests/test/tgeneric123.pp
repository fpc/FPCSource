{%FAIL}
{$mode objfpc}

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
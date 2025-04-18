{$mode objfpc}

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
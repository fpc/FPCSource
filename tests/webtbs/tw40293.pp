unit tw40293;
{$mode objfpc}{$H+}
interface

type
  ftest1 = class
    private
      procedure setValue(value: specialize TArray<String>);
      function getValue(): specialize TArray<String>;
  end;


implementation

uses sysutils; // compile error if uncommented

procedure ftest1.setValue(value: specialize TArray<String>);
  begin
  end;

function ftest1.getValue(): specialize TArray<String>;
  begin
  end;

end.


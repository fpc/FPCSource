unit uchlp82; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

type
  TFoo = class
  strict private
    Test1: Integer;
  private
    Test2: Integer;
  strict protected
    Test3: Integer;
  protected
    Test4: Integer;
  end;

implementation

end.


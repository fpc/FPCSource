unit uchlp32c; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp32a;

type
  TFooHelperB = class helper for TFoo
    procedure Method2;
  end;

implementation

procedure TFooHelperB.Method2;
begin

end;

end.


unit uchlp32b; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  uchlp32a;

type
  TFooHelperA = class helper for TFoo
    procedure Method1;
  end;

implementation

procedure TFooHelperA.Method1;
begin

end;

end.


unit uhlp43;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TInterfaceHelper = class helper for TObject
    class function Test: Integer;
  end;

function DoTest: Integer;

implementation

class function TInterfaceHelper.Test: Integer;
begin
  Result := 1;
end;

type
  TImplementationHelper = class helper for TObject
    class function Test: Integer;
  end;

class function TImplementationHelper.Test: Integer;
begin
  Result := 2;
end;

function DoTest: Integer;
begin
  Result := TObject.Test;
end;

end.

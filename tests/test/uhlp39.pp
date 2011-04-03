unit uhlp39;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TObjectHelper1 = class helper for TObject
    class procedure Test1;
  end;

  TObjectHelper2 = class helper for TObject
    class procedure Test2;
  end;

implementation

class procedure TObjectHelper1.Test1;
begin

end;

class procedure TObjectHelper2.Test2;
begin

end;

end.

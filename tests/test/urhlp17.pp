unit urhlp17;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TTest1 = record
  end;

  TTest2 = record
  end;

  TTest3 = record
  end;

  TTestHub = record
  strict private
    type
      TTest1Helper = record helper for TTest1
        procedure Test;
      end;
  private
    type
      TTest2Helper = record helper for TTest2
        procedure Test;
      end;
  public
    type
      TTest3Helper = record helper for TTest3
        procedure Test;
      end;
  end;

implementation

procedure TTestHub.TTest1Helper.Test;
begin

end;

procedure TTestHub.TTest2Helper.Test;
begin

end;

procedure TTestHub.TTest3Helper.Test;
begin

end;

end.

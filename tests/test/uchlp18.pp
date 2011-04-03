unit uchlp18;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TTest1 = class
  end;

  TTest2 = class
  end;

  TTest3 = class
  end;

  TTest4 = class
  end;

  TTest5 = class
  end;

  TTest6 = class
  end;

{$M+}
  TTestHub = class
  strict private
    type
      TTest1Helper = class helper for TTest1
        procedure Test;
      end;
  private
    type
      TTest2Helper = class helper for TTest2
        procedure Test;
      end;
  strict protected
    type
      TTest3Helper = class helper for TTest3
        procedure Test;
      end;
  protected
    type
      TTest4Helper = class helper for TTest4
        procedure Test;
      end;
  public
    type
      TTest5Helper = class helper for TTest5
        procedure Test;
      end;
  published
    type
      TTest6Helper = class helper for TTest6
        procedure Test;
      end;
  end;
{$M-}

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

procedure TTestHub.TTest4Helper.Test;
begin

end;

procedure TTestHub.TTest5Helper.Test;
begin

end;

procedure TTestHub.TTest6Helper.Test;
begin

end;

end.

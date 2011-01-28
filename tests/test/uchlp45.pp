unit uchlp45; 

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

type
  TFoo = class
  end;

  TFooHelper = class helper for TFoo
  strict private
    procedure Test1;
  private
    procedure Test2;
  strict protected
    procedure Test3;
  protected
    procedure Test4;
  public
    procedure Test5;
  end;

implementation

procedure TFooHelper.Test1;
begin

end;

procedure TFooHelper.Test2;
begin

end;

procedure TFooHelper.Test3;
begin

end;

procedure TFooHelper.Test4;
begin

end;

procedure TFooHelper.Test5;
begin

end;

end.


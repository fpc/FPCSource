unit uhlp31;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

type
  TFoo = class
  end;

{$M+}
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
  published
    procedure Test6;
  end;
{$M-}

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

procedure TFooHelper.Test6;
begin

end;

end.


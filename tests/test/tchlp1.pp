{%NORUN}

{ checks for support of the class helper syntax in mode objfpc }
program tchlp1;

{$mode objfpc}

type
  TObjectHelper = class helper for TObject
    procedure SomePublicMethod;
  strict private
    procedure SomeStrictPrivateMethod;
  private
    procedure SomePrivateMethod;
  strict protected
    procedure SomeStrictProtectedMethod;
  protected
    procedure SomeProtectedMethod;
  public
    procedure SomePublicMethod2;
  end;

procedure TObjectHelper.SomePublicMethod;
begin
end;

procedure TObjectHelper.SomeStrictPrivateMethod;
begin
end;

procedure TObjectHelper.SomePrivateMethod;
begin
end;

procedure TObjectHelper.SomeStrictProtectedMethod;
begin
end;

procedure TObjectHelper.SomeProtectedMethod;
begin
end;

procedure TObjectHelper.SomePublicMethod2;
begin
end;

begin

end.


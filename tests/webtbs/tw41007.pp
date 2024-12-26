{ %NORUN }

program tw41007;

{$mode delphi}
{$modeswitch implicitfunctionspecialization}

uses
  uw41007;

  procedure Test(A: string; B: string); overload;
  begin
  end;

  procedure Test<T>; overload;
  begin

  end;

begin
  Test('aa', 'bb');
end.


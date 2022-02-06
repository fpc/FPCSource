{ %NORUN }

program tanonfunc32;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ Test that there's no internal error when REGVAR optimizations are enabled }

{$optimization regvar}

type
  TProc = reference to procedure;

  TObj = class
    Str: string;
  end;

procedure GlobalProc(AObj: TObj);

  procedure NestedProc(AProc: TProc);
  begin
    AObj.Str := '';
  end;

begin
  NestedProc(
    procedure
    begin
      AObj.Str := '';
    end)
end;

begin
end.

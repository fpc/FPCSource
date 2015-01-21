{ %FAIL }

program tw26456;
{$modeswitch advancedrecords}
type
  THelper = record helper for xxxxxxx
    procedure test;
  end;

  procedure THelper.test;
  begin
  end;

begin
end.


{ %norun }

program bug11349;

{$R+,Q+}

const
  WS_BORDER = DWORD($800000);

var
  Style: Cardinal;

begin
  Style := 0;
  Style := Style and not WS_BORDER;
end.


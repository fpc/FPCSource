program prec;

uses SysUtils;

var
  i:Integer;
  v: Double;
  s: string;

begin
  v := 1.0000000000001;
  for i := 1 to 20 do
    begin
      s := FloatToStrF(v, ffGeneral, i, 0);
      WriteLn(i, ' ', s);
      if (i < 14) then
        begin
          if (s <> '1') then
            begin
              writeln('error');
              halt(1);
            end;
        end
      else
        if (s <> '1'+DecimalSeparator+'0000000000001') then
          begin
            writeln('error');
            halt(1);
          end;
    end;
end.

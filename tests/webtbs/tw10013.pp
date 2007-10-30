program rcerror;

{$MODE DELPHI} {$H+} {$R+}

uses SysUtils;

var
  ws: WideString;
  //wc: WideChar;
  i: Integer;

begin
  ws := UTF8Decode('something');

  WriteLn;
  WriteLn('str: "', UTF8Encode(ws), '"');
  WriteLn('len (must be 9) : ', Length(ws));
  WriteLn;

  for i := 1 to Length(ws) * 2 + 1 do
  begin

    Write('Try to access ws[', i, ']');

    try

      ws[i] := ws[i];
      //wc := ws[i];
      //ws[i] := wc;

      if i > Length(ws) then
        begin
          writeln(' FAULT');
          halt(1);
        end
      else
        WriteLn(' OK');

    except

      on e : Exception do
      begin
        if (e is ERangeError) and (i > Length(ws)) then
          WriteLn(' OK (got a range-check error as expected)');
      end;

    end;

  end;
end.


{$mode objfpc}
uses
  sysutils;

resourcestring sMyNewErrorMessage = 'Illegal value: %d';

begin
  try
    raise Exception.CreateResFmt(@sMyNewErrorMessage, [-1]);
  except
    on e : exception do
      begin
        if e.message='Illegal value: -1' then
          halt(0)
        else
          begin
            writeln('error : ',e.message);
            halt(1);
          end;
      end;
  end;
end.

{$mode objfpc}

uses
  SysUtils;
begin
  try
    StrToCurr('46198723647893247891326489732164897321649');
  except
    on EConverterror do
     halt(0)
  end;
end.


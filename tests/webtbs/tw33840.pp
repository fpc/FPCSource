{ %NORUN }

program tw33840;

{$mode delphi}

uses
  SysUtils;

const
  NULLSTRING = '';
begin
  if String.IsNullOrWhiteSpace(NULLSTRING) then
    Writeln('String is empty');
end.

program project1;

{$mode objfpc}{$H+}

uses
  Math,SysUtils;

begin
  if FormatFloat('0.#',Nan)<>'NaN' then
    begin
      WriteLn(FormatFloat('0.#',Nan));
      halt(1);
    end;
end.

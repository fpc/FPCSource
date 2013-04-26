program tw23204;

{$mode Delphi}{$H+}

uses
  uw23204;

var
  cur_p: TP;

function DropP:TPs;
begin
  result := [cur_p.AType];
end;


begin
  cur_p.AType:=pt_1;
  if DropP<>[pt_1] then
    halt(1);
end.

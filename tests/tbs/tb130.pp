{ Old file: tbs0147.pp }
{ function b; is not allowed in implementation          OK 0.99.7 (PFV) }

{$mode tp}
unit tb130;
interface

function b:boolean;

implementation

function b;
begin
end;

end.

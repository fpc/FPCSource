unit cycle3_changec_intf_cat;

{$mode objfpc}

interface

const Catty = 3;

function Jump(w : word): word;

implementation

uses cycle3_changec_intf_ant;

function Jump(w : word): word;
begin
  Result:=w*11*Factor; // changed
end;

end.

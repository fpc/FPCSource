{$mode objfpc}
{$R+}

type  Tconstexprint=record
        overflow:boolean;
        case signed:boolean of
          false:
            (uvalue:qword);
          true:
            (svalue:int64);
      end;

operator := (const u:qword):Tconstexprint;
begin
  if (u<>high(int64)+100) then
    halt(1);
  result.overflow:=false;
  result.signed:=false;
  result.uvalue:=u;
end;

operator := (const s:int64):Tconstexprint;
begin
  if (s<>-128) then
    halt(2);
  result.overflow:=false;
  result.signed:=true;
  result.svalue:=s;
end;


var
  value : tconstexprint;
begin
  // Here it should choose the int64 code instead of qword
  value:=-128;
  // Here it should choose the qword
  value:=high(int64)+100;
end.

{ %NORUN }

{$mode objfpc}
{$MODESWITCH AdvancedRecords}
program tw29609;


type t = record
        class var      v : Boolean;
        class function f : Boolean;  static;
        class property p : Boolean  read v;
        end;


class function t.f : Boolean;
begin
Result := p;    // "Error: Pointer to object expected"
end;


begin
end.

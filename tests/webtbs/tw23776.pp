{ %NORUN }

program tw23776;
{$mode objfpc}
type
generic tg<T>= class
    public
        class function len(a: T): integer;
end;
    class function tg.len(a: T): integer;
    begin
        result:= length(a);
    end;
begin
end.

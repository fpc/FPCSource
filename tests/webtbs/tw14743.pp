{$mode delphi}
program tbreintroduce;

type
    parent = object
        constructor init;
    end;

    child = object(parent)
        constructor init(a : byte);reintroduce;
    end;

constructor parent.init;
begin
end;

constructor child.init(a : byte);
begin
end;

begin
end.

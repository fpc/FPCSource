{ Old file: tbs0264.pp }
{ methodpointer bugss                                   OK 0.99.12b (FK) }

{$MODE DELPHI}

type
    a = class
        c : procedure of object;

        constructor create; virtual;
        destructor destroy; override;

        procedure e; virtual;
        procedure f; virtual;
    end;

constructor a.create;
begin
    c := e;
end;

destructor a.destroy;
begin
end;

procedure a.e;
begin
    Writeln('E');
    c := f;
end;

procedure a.f;
begin
    Writeln('F');
    c := e;
end;

var
    z : a;

begin
    z := a.create;
    z.c;
    z.c;
    z.c;
    z.free;
end.

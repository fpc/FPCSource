{ %version=1.1 }

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
        TCls = class
                public
                        procedure build (a, b, c: LongInt);overload;
                        procedure build (a, b, c, d: LongInt);overload;
                end;

const
    err : boolean=true;

procedure TCls.build (a, b, c: LongInt);

        procedure subproc;
        begin
                writeln ('a, b, c');
        err:=false;
        end;

begin
        subproc;
end;

procedure TCls.build (a, b, c, d: LongInt);

        procedure subproc;
        begin
                writeln ('a, b, c, d');
        end;

begin
end;

var
        C: TCls;

begin
        C := TCls.create;
        C.build (1, 2, 3);
    if err then
     halt(1);
end.

{$mode objfpc}{$H+}
{$modeswitch AnonymousFunctions}
{$modeswitch FunctionReferences}
{$modeswitch AdvancedRecords}
{$COperators On}

program tw40061;

uses
        sysutils;

type
        prtype1int = reference to procedure(i: integer);
        TSomeRec = record
        strict private
                a: integer;
                b: integer;
                procedure update;
                procedure update10;
                procedure update20;
        public
                class procedure main10; static;
                class procedure main20; static;
        end;

procedure TSomeRec.update;
        procedure primary(i: integer = 0);
                begin
                        for i := 0 to b do inc(a);
                end;
        begin
                primary;
        end;

procedure TSomeRec.update10;
        procedure primary(i: integer = 0);
                begin
                        for i := 1 to b do inc(a);
                        writeln('update10 a = ', a);
                end;
        begin
                update;
                b += 4;
                primary;
        end;

procedure TSomeRec.update20;
        procedure primary(const f: prtype1int);
                begin
                        f(0);
                end;
        begin
                writeln('update20.0 a = ', a, ' b = ', b);
                update;
                b += 4;
                writeln('update20.1 a = ', a, ' b = ', b);
                primary(procedure (i: integer)
                begin
                        writeln('update20.2 a = ', a, ' b = ', b);
                        for i := 1 to b do inc(a);
                        writeln('update20.3 a = ', a, ' b = ', b);
                end);
        end;

class procedure TSomeRec.main10; static;
        function primary: TSomeRec;
                begin
                        result.a := 0;
                        result.b := 10;
                        result.update10;
                end;
        begin
                writeln(format('main10 a = %d', [primary.a]));
        end;

class procedure TSomeRec.main20; static;
        function primary: TSomeRec;
                begin
                        result.a := 0;
                        result.b := 10;
                        result.update20;
                        if result.a <> 25 then
                          halt(1);
                end;
        begin
                writeln(format('main20 a = %d', [primary.a]));
        end;

procedure primary(const n: integer=0; i: integer=0; a: integer=0);
        begin
                writeln('primary a = ', a);
                for i := 1 to n do procedure begin a += 1; end();
                writeln('primary a = ', a);
        end;

procedure main;
        begin
                //TSomeRec.main10;
                TSomeRec.main20;
                //primary(10);
        end;

begin
        main;
end.


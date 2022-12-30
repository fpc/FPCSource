{ %NORUN }

program tw40062;
{$mode objfpc}{$H+}
{$modeswitch AnonymousFunctions}
{$modeswitch AdvancedRecords}

uses
        sysutils;

type
        TSomeRec = record
                a: integer;
                procedure print;
                function text: string;
                procedure something;
                class procedure main; static;
        end;

function some_fun_0: TSomeRec;
        begin
                result.a := 4;
        end;

procedure TSomeRec.print;
        begin
                writeln('a = ', a);
        end;

function TSomeRec.text: string;
        begin
                result := format('a = %d', [a]);
        end;

procedure main;
        begin
                some_fun_0().print;
                (function: TSomeRec begin result.a := 5 end()).print;
                writeln((function: TSomeRec begin result.a := 10 end()).text);
        end;

procedure TSomeRec.something;
        begin
                (function: TSomeRec begin result.a := 5 end()).print;
                writeln((function: TSomeRec begin result.a := 10 end()).text);
        end;

class procedure TSomeRec.main; static;
        function primary: TSomeRec;
                begin
                        result.a := 20;
                end;
        begin
                some_fun_0().print;
                primary.something;
        end;

begin
        main;
        TSomeRec.main;
end.


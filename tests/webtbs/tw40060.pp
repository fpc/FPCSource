{ %NORUN }

program tw40060;
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

class procedure TSomeRec.main; static;
        begin
                some_fun_0().print;
                (function: TSomeRec begin result.a := 5 end()).print;
                writeln((function: TSomeRec begin result.a := 10 end()).text);
        end;

begin
        main;
        TSomeRec.main;
end.


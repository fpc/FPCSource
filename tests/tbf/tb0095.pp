{ %FAIL }
{ Old file: tbs0356.pp }
{  }

unit tb0297;
interface
uses sysutils;
type

   Foo =
        packed record
                Dates : array[1..11] of Date;
        end;
implementation
end.

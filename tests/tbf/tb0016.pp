{ %FAIL }
{ Old file: tbf0100.pp }
{ a unit may only occure once in uses                   OK 0.99.6 (PM) }

unit tbs0100;
interface
uses dos;
implementation
uses dos;             { Not Allowed in BP7}
end.

unit uw24801;
{$MODE objfpc}{$H+}
interface
uses
  Classes,sysutils;
  function f: string; inline;   //causing internal error
implementation

function f: string;
var msg : string;
begin
//***** this is the block causing internal error
      try
        raise exception.create('asdfasdf');
      except
        on E: exception do begin
          msg := E.Message;
          result:=msg;
        end;
      end;
end;
end.   

{ Old file: tbs0140a.pp }
{  }


unit ub0120;

interface

uses tb0120;

procedure Message(var O:TObject);

implementation

procedure Message(var O:TObject);
 begin writeln('Message') end;
end.

{ Old file: tbs0140a.pp }
{  }


unit tb123;

interface

uses tb122;

procedure Message(var O:TObject);

implementation

procedure Message(var O:TObject);
 begin writeln('Message') end;
end.

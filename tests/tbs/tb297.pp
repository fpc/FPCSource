{ Old file: tbs0346b.pp }
{  }

unit tbs0346b;
interface

{ this uses system.word }
procedure p(w:word);

implementation
uses
  tbs0346a;

{ this uses tbs0346a.word }
procedure p(w:word);
begin
end;

end.

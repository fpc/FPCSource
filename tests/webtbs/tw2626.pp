{ Source provided for Free Pascal Bug Report 2626 }
{ Submitted by "Jose Santos" on  2003-08-10 }
{ e-mail: jcas@netcabo.pt }
program ShowBug(Input, Output);
{$MODE DELPHI}
type
  TBug = class
         public
            constructor Create; overload;
            constructor Create(var S: String); overload;
         end;

  constructor TBug.Create;
  begin
  end;

  constructor TBug.Create(var S: String);
  begin
    S:='Test';
  end;

var
  Bug: TBug;

begin
  Bug:=TBug.Create;
end.

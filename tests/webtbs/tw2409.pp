{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2409 }
{ Submitted by "Mattias Gaertner" on  2003-03-07 }
{ e-mail: nc-gaertnma@netcologne.de }
unit tw2409;

{$mode objfpc}{$H+}

interface

type
  HDC = type integer;

  TMyClass = class
    procedure DoSomething(H: HDC);
  end;

implementation

{ TMyClass }

procedure TMyClass.DoSomething(H: HDC);
begin

end;

end.

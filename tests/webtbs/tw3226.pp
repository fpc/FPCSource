{ Source provided for Free Pascal Bug Report 3226 }
{ Submitted by "marco" on  2004-08-03 }
{ e-mail:  }
program Project2;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type Ty = type Pointer;

procedure p(y : ty);
  begin
    halt(0);
  end;

procedure p(y : pointer);
  begin
    writeln('type p = type pointer; problem');
    halt(1);
  end;

var y : ty;

begin
  p(y);
end.

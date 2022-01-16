{ %OPT=-CE }
{ %result=217 }

{ Source provided for Free Pascal Bug Report 4100 }
{ Submitted by "Markus Roberts" on  2005-06-20 }
{ e-mail: Markus@reality.com }
{$ifdef unix}
  {$linklib c}
{$endif}

    uses SysUtils;
    var
        X,Y,Z : Extended;
        I     : integer;
    begin
        X := 0.0;
        Y := 1.0;
        Z := Y/X;
        I := 7;
        writeln('test');
        writeln(StrToFloat('1e7'));
        writeln(Z);
        end.


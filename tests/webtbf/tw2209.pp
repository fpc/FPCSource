{ %opt=-Sew -vw }
{ %fail }

{ Source provided for Free Pascal Bug Report 2209 }
{ Submitted by "Erik Scheffers" on  2002-10-30 }
{ e-mail: eriks@win.tue.nl }
{$ifdef fpc}{$mode delphi}{$endif}

procedure case_bug;
var uninitialized : integer;

begin
    case (uninitialized) of
       1 :   writeln('it''s a one!');
       else  writeln('it''s something else...');
    end
end;

begin
end.

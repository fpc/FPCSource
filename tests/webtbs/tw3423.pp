{ Source provided for Free Pascal Bug Report 3423 }
{ Submitted by "Bram Kuijvenhoven" on  2004-12-02 }
{ e-mail: kuifware@hotmail.com }
{$MODE OBJFPC}{$H+}

var
  a:ansistring;
  s:shortstring;

begin

  a:='';
  s:='';
  a:=a+s;

end.

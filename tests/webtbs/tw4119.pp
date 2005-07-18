{ Source provided for Free Pascal Bug Report 4119 }
{ Submitted by "C Western" on  2005-06-26 }
{ e-mail: mftq75@dsl.pipex.com }

{$mode delphi}

uses StrUtils;

function mypos(s1,s2 : widestring) : integer;overload;
  begin
    result:=pos(s1,s2);
  end;

function mypos(s1,s2 : ansistring) : integer;overload;
  begin
    result:=pos(s1,s2);
  end;

function mypos(s1,s2 : shortstring) : integer;overload;
  begin
    result:=pos(s1,s2);
  end;

var
 s:AnsiString;
 p:ShortString;
 ws:widestring;
begin
  s:=DupeString('a',300)+'b';
  ws:=s;
  p:='b';
  WriteLn(MyPos('b',s));
  WriteLn(MyPos(p,s));
  WriteLn(MyPos(p,ws));
end.

{$mode delphi}

program tnestcallpass1;

type
  tncp1_c = class
    procedure test;
  end;


procedure tncp1_c.test;

var
  l: longint;

  function nest(const s: unicodestring): longint;
    begin
      l:=5;
      if length(s)=5 then
        nest:=l
      else
        nest:=3;
    end;

begin
  nest('abcdef');
end;


begin
end.

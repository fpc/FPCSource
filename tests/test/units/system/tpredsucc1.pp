{$mode objfpc}

type
  tmyclass = class
    i : integer;
    function GetI : Integer;
    procedure SetI(const _i : Integer);
    property i1 : integer read GetI write SetI;
    property i2 : integer read i write i;
  end;


function tmyclass.GetI : Integer;
  begin
    Result:=i;
  end;

procedure tmyclass.SetI(const _i : Integer);
  begin
    i:=_i;
  end;

var
  myclass : tmyclass;

begin
  myclass:=tmyclass.create;
  myclass.i1:=1;

  myclass.i1:=pred(myclass.i1);
  myclass.i1:=succ(myclass.i1);

  myclass.i2:=pred(myclass.i2);
  myclass.i2:=succ(myclass.i2);

  if myclass.i<>1 then
    halt(1);

  myclass.free;

  writeln('ok');
end.

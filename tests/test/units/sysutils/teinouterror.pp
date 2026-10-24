{ EInOutError constructors that set ErrorCode, issue #41806 }
program teinouterror;

{$mode objfpc}{$h+}

uses
  SysUtils;

var
  lCaught: Boolean;

begin
  lCaught:=False;
  try
    raise EInOutError.Create('Test',1234);
  except
    on E: EInOutError do
      begin
      lCaught:=True;
      if E.Message<>'Test' then
        halt(1);
      if E.ErrorCode<>1234 then
        halt(2);
      end;
  end;
  if not lCaught then
    halt(3);

  lCaught:=False;
  try
    raise EInOutError.CreateFmt('File %s, code %d',['a.txt',5],2);
  except
    on E: EInOutError do
      begin
      lCaught:=True;
      if E.Message<>'File a.txt, code 5' then
        halt(11);
      if E.ErrorCode<>2 then
        halt(12);
      end;
  end;
  if not lCaught then
    halt(13);

  lCaught:=False;
  try
    raise EInOutError.Create('Plain');
  except
    on E: EInOutError do
      begin
      lCaught:=True;
      if E.Message<>'Plain' then
        halt(21);
      if E.ErrorCode<>0 then
        halt(22);
      end;
  end;
  if not lCaught then
    halt(23);

  lCaught:=False;
  try
    raise EInOutError.CreateFmt('Value %d',[7]);
  except
    on E: EInOutError do
      begin
      lCaught:=True;
      if E.Message<>'Value 7' then
        halt(31);
      if E.ErrorCode<>0 then
        halt(32);
      end;
  end;
  if not lCaught then
    halt(33);
  writeln('ok');
end.

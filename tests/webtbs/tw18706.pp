{$MODE DELPHI}

type

  TExecProc = procedure of object;

  TA = class
  public
    procedure P1; overload; virtual;
    procedure P1(const param: boolean); overload; virtual;
  end;

  TB = class(TA)
  public
    procedure P1(const param: boolean); override;
  end;

procedure ShowProc(p: TExecProc);
begin
  p;
end;

procedure TA.P1;
begin
  writeln('1');
end;

procedure TA.P1(const param: boolean);
begin
  writeln('2');
  halt(1);
end;

procedure TB.P1(const param: boolean);
begin
  writeln('3');
  halt(2);
end;

var
  a: TA;
  b: TB;
begin
  a := TA.Create;
  b := TB.Create;

  ShowProc(a.P1); // compile and execute correctly
  ShowProc(b.P1); // error on compile !!! but here should be call TA.P1 !!!
end.

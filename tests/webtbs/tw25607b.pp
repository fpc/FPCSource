program E02;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

type
  TA = class
    constructor Create(A: Integer = 0); overload;
  end;

  TB = class(TA)
    constructor Create(A: Integer); overload;
  end;

  TClassB = class of TB;

var
  tacalled,
  tbcalled: boolean;

constructor TA.Create(A: Integer = 0);
begin
  WriteLn('TA.Create');
  tacalled:=true;
end;

constructor TB.Create(A: Integer);
begin
  WriteLn('TB.Create');
  tbcalled:=true;
end;

var
  B: TB;
  ClassB: TClassB;
begin
  B := TB.Create; // TA.Create (VMT is not used
                  // compiler can determine)
  if not tacalled then
    halt(1);
  if tbcalled then
    halt(2);
  tacalled:=false;

  B.Create; // call TA.Create because of VMT rules
  B.Free;
  if not tacalled then
    halt(3);
  if tbcalled then
    halt(4);
  tacalled:=false;

  ClassB := TB;
  B := ClassB.Create; // call TA.Create because of VMT rules
  B.Free;
  if not tacalled then
    halt(5);
  if tbcalled then
    halt(6);
  tacalled:=false;
end.

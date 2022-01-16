program E01;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

type
  TA = class
    constructor Create(A: Integer = 0); overload; virtual;
  end;

  TB = class(TA)
    constructor Create(A: Integer); overload; override;
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
                  // compiler can determine) -- in Delphi;
                  // In FPC, because TB.Create is used, we
                  // call TB.Create
  if tacalled then
    halt(1);
  if not tbcalled then
    halt(2);
  tbcalled:=false;

  B.Create; // call TB.Create because of VMT rules
  B.Free;
  if tacalled then
    halt(3);
  if not tbcalled then
    halt(4);
  tbcalled:=false;

  ClassB := TB;
  B := ClassB.Create; // call TB.Create because of VMT rules
  B.Free;
  if tacalled then
    halt(5);
  if not tbcalled then
    halt(6);
end.

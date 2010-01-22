{ %TARGET=win32,win64,wince}

program tdispinterface2;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type

  { IIE }

  IIE = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Disp300; dispid 300;
    property Disp1: integer;
    procedure Disp2;
    property Disp402: wordbool dispid 402;
  end;

var
  cur_dispid: longint;

{$HINTS OFF}
procedure DoDispCallByID(res : Pointer; const disp : IDispatch;desc : PDispDesc; params : Pointer);
begin
  if desc^.dispid <> cur_dispid then
    halt(cur_dispid);
end;
{$HINTS ON}

var
  II: IIE;
begin
  DispCallByIDProc := @DoDispCallByID;
  cur_dispid := 300;
  II.Disp300;
  cur_dispid := 1;
  II.Disp1 := 1;
  cur_dispid := 2;
  II.Disp2;
  cur_dispid := 402;
  II.Disp402 := True;
end.
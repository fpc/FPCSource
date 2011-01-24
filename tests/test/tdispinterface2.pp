{ %TARGET=win32,win64,wince}

program tdispinterface2;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  Variants;

type

  { IIE }

  IIE = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Disp300; dispid 300;
    property Disp1: integer;
    procedure Disp2;
    property Disp402: wordbool dispid 402;
    procedure DispArg1(Arg: IUnknown);
    procedure DispArg2(Arg: IDispatch);
    function DispArg3(var Arg: wordbool): widestring;
    property DispProp[index: OleVariant]: Integer;
  end;

var
  cur_dispid: longint;
  cur_argtype: byte;
  cur_restype: byte;
  cur_calltype: byte;

{$HINTS OFF}
  procedure DoDispCallByID(res: Pointer; const disp: IDispatch; desc: PDispDesc;
    params: Pointer);
  begin
    if desc^.dispid <> cur_dispid then
      halt(cur_dispid);
  end;

  procedure DoDispCallByIDArg(res: Pointer; const disp: IDispatch; desc: PDispDesc;
    params: Pointer);
  begin
    if desc^.calldesc.argcount <> 1 then
      halt(4);
    if desc^.calldesc.argtypes[0] <> cur_argtype then
      halt(cur_argtype);
    if desc^.restype <> cur_restype then
      halt($FF);
  end;

  procedure DoDispCallByIDProp(res: Pointer; const disp: IDispatch; desc: PDispDesc;
    params: Pointer);
  begin
    if desc^.calldesc.calltype <> cur_calltype then
      halt(5);
    // put: arg #0 is value, arg #1 is index (in Delphi: vice-versa)
    // get: arg #0 is index
    if desc^.calldesc.argtypes[ord(cur_calltype=4)] <> cur_argtype then
      halt(6);  
    if cur_calltype=4 then
    begin
      if desc^.calldesc.argcount <> 2 then
        halt(7);
      if desc^.calldesc.argtypes[0] <> cur_restype then
        halt(8);
      if desc^.restype <> 0 then
        halt(9);
    end;
  end;

{$HINTS ON}

var
  II: IIE;
  B: wordbool;
begin
  // check dispid values
  writeln('Testing dispid values...');
  DispCallByIDProc := @DoDispCallByID;
  cur_dispid := 300;
  II.Disp300;
  cur_dispid := 1;
  II.Disp1 := 1;
  cur_dispid := 2;
  II.Disp2;
  cur_dispid := 402;
  II.Disp402 := True;
  // check arguments
  writeln('Testing arguments...');
  DispCallByIDProc := @DoDispCallByIDArg;
  cur_restype := varempty;
  cur_argtype := varunknown;
  II.DispArg1(nil);
  cur_argtype := vardispatch;
  II.DispArg2(nil);
  cur_restype := varolestr;
  cur_argtype := varboolean or $80;
  B := False;
  II.DispArg3(B);

  writeln('Testing properties...');
  DispCallByIDProc := @DoDispCallByIDProp;
  cur_calltype := 2;  // propertyget
  cur_argtype := varvariant;
  cur_restype := varinteger;
  II.DispProp[1];
  II.DispProp['abc'];

  cur_calltype := 4; // propertyput
  II.DispProp[1] := 11;
  II.DispProp['abc'] := 12;

end.
{ %version=1.1}

{ Source provided for Free Pascal Bug Report 2176 }
{ Submitted by "Rimgaudas" on  2002-10-14 }
{ e-mail: rimga@ktl.mii.lt }
{$ifdef fpc}{$mode delphi}{$endif}

uses
  SysUtils;

type
  ii= interface
  ['{616D9683-88DC-4D1C-B847-1293DDFBACF7}']
    procedure Show;stdcall;
  end;

  Twii= class(TInterfacedObject, ii)
    s: string;
    procedure Show;stdcall;
  end;

  procedure Twii.Show;stdcall;
  begin
    WriteLn(s);
  end;

var
  wii: twii;
  i: ii;

begin
  try
    wii:= Twii.create;
    wii.s:='OK';
    i:= ii(wii);
    i.Show;        //writes nothing
  except           //does not excepts
    WriteLn('Problem');
    halt(1);
  end;
  //in delphi it works OK
end.

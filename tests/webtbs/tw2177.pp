{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 2177 }
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

  Twii= class(TObject, ii)
    s: string;
    function QueryInterface(constref IID: TGUID; out Obj): Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    procedure Show;stdcall;
  end;

  {________doomy interfaces______}
  function Twii.QueryInterface(constref IID: TGUID; out Obj): Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  begin
    result:= -1;
  end;

  function Twii._AddRef: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  begin
    result:= -1;
  end;

  function Twii._Release: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  begin
    result:= -1;
  end;
  {________doomy interfaces______}


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
    i.Show;
  except       //excepts
    on EInvalidCast do WriteLn('Invalid Cast');
  else
    WriteLn('Problem');
    halt(1);
  end;
end.

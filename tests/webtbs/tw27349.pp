{ %NORUN }

program tw27349;

{$mode delphi}
{.$mode objfpc}
{.$modeswitch advancedrecords}

type

  C = class

   type

    tmyintf = class(TInterfacedObject, iinterface)
     function _AddRef : longint; stdcall;
    end;

  end;

  R = record

   type

    tmyintf = class(TInterfacedObject, iinterface)
     function _AddRef : longint; stdcall;
    end;

  end;

function C.tmyintf._AddRef: longint; stdcall;
begin
 result := inherited _AddRef; // OK
end;

function R.tmyintf._AddRef: longint; stdcall;
begin
 result := inherited _AddRef; // FAIL
end;

begin
end.


{ %fail }

{ Source provided for Free Pascal Bug Report 2721 }
{ Submitted by "marcov" on  2003-10-07 }
{ e-mail:  }
{$mode Delphi}

type
  TIntfPersistent = class
  public
    function _Release: Integer; virtual; cdecl;
    end;

  t2 = class(Tintfpersistent)
    public
     // Different calling convention
     function _Release: Integer; override; stdcall;
    end;

function TIntfPersistent._Release: Integer;
begin
end;

function t2._Release: Integer;
begin
end;

begin
end.

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
     function _Release: Integer; override; stdcall;
    end;


begin
end.


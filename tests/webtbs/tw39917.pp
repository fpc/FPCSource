{ %NORUN }
{ %OPT=-O3 }

program tw39917;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
{$optimization on}

{ Test optimization.  The self param passed to DlgAsync() and Checkout() several
  anon functions deep in TView.CopyFile cause compiler fatal internal error
  200409241.  

  If optimization is off there is no error.  If CopyFile is not an object method
  then there is no problem (though values passed to DlgASync) and CheckOut must
  be changed.  If DlgASync() and CheckOut() receive a value other than self there
  is no problem.
}

type
  TProc = reference to procedure;
  TModalDoneProc = reference to procedure(ASuccess: Boolean);
  TView = class(TObject)
    procedure CopyFile;
  end;

procedure DlgAsync(AOwner: TObject; const AOnModalDone: TModalDoneProc);
begin
end;

function Checkout(AOwner: TObject): Boolean;
begin
Result := True;
end;

procedure TView.CopyFile;

  procedure Prompt(const AOnModalDone: TProc);
  begin
  end;

begin
Prompt(
  procedure

    procedure Prompt(const AOnModalDone: TProc);
    begin
    if True then
      DlgAsync(Self,
        procedure(ASuccess: Boolean)
        begin
        if ASuccess and CheckOut(Self) then
          AOnModalDone()
        end)
    end;

  begin
  Prompt(
    procedure
    begin
    end)
  end)
end;

begin
end.


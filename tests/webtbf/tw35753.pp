{ %fail }
{ %OPT=-vw -Sew }

type
      TRegister = (
        TRegisterLowEnum := Low(longint),
        TRegisterHighEnum := High(longint)
      );
const
       NR_INVALID    = tregister($fffffffff);
begin
end.

{ inlining is not compatible with get_caller_frame/get_frame }
{$inline off}
type
  PointerLocal = procedure(_EBP: Pointer);

procedure proccall(p: codepointer);
begin
{$ifndef FPC_LOCALS_ARE_STACK_REG_RELATIVE}
  PointerLocal(p)(get_caller_frame(get_frame,get_pc_addr));
{$else}
PointerLocal(p)(get_frame);
{$endif}  

end;

procedure t1;
var
  l : longint;

  procedure t2;

    procedure t3;

      procedure t4;
        begin
          l := 5;
        end;

      begin { t3 }
        proccall(@t4);
      end;

    begin { t2 }
      t3;
    end;

  begin { t1 }
    l := 0;
    t2;
    if (l <> 5) then
      halt(1);
  end;

begin
  t1;
end.

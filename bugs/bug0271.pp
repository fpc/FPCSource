{$MODE OBJFPC}

{
  The problem is that only a entry to FPC_ABSTRACTERROR is generated and the
  load will be done using the mangledname for the procedure which doesn't
  exists.
  Adding poabstractmethod to po_compatibility_options is required.
}

type
    int32 = longint;

    tscanline = packed record
        x1, x2, y : int32;
    end;

    pcolorindex = int32;

    tcproc = procedure(const scan : tscanline;
        const data : pcolorindex) of object;

    a = class
        _copyscan : tcproc;

        procedure proc1(const scan : tscanline;
            const data : pcolorindex); virtual; abstract;

        procedure setproc;
    end;

procedure a.setproc;
begin
    _copyscan := @proc1;
end;

begin
end.

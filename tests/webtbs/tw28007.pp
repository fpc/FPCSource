program error_record;

type

  TPackedBool = bitpacked record
    b0: Boolean;
    b1: Boolean;
    b2: Boolean;
    b3: Boolean;
    b4: Boolean;
    b5: Boolean;
    b6: Boolean;
    b7: Boolean;
  end;

var
  B: ByteBool;
  PackedBool: TPackedBool;

begin
(*
    - OK on x86, x86_64 compiler
    - ERROR on cross arm compiler
    - OK on cross arm compiler if we do typecast:
        B := ByteBool(PackedBool.b0);
                                                    *)

  B := PackedBool.b0;
end.

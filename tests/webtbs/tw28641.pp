{ %norun }

{$mode delphi}

program Test;

type
  TMD5DigestHelper = record
  public type
    TMD5Buffer = array[0..63] of Byte;

    TMD5Context = record
      Buffer: TMD5Buffer;
    end;
  private
    function Func_F(const X, Y, Z: LongWord): LongWord;
  public
    Context: TMD5Context;
  end;

function TMD5DigestHelper.Func_F(const X, Y, Z: LongWord): LongWord;
begin
  Result := (X and Y) or ((not X) and Z);
end;

begin
end.

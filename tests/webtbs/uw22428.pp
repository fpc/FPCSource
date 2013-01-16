unit uw22428;

{$MODE OBJFPC}
{$modeswitch advancedrecords}

interface

type
  generic TWrapper<T> = record
    class procedure Test; static;
  end;

implementation

class procedure TWrapper.Test;
begin

end;

end.

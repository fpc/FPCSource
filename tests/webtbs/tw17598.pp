{ %NORUN }

{$mode macpas}
{$extendedsyntax off}
{$modeswitch exceptions+}
{$modeswitch class+}

program tw17598;

  uses
    sysutils;

  type
    EMyException =
      class( Exception)
        constructor Create( theMessage: Ansistring);
      end;

constructor EMyException.Create( theMessage: Ansistring);
    begin
      inherited Create( theMessage)
    end;

begin
  try
    raise EMyException.Create( 'my exception raised')
  except
    ShowException( ExceptObject, ExceptAddr)
  end
end.

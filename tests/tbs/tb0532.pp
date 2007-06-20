{%skiptarget=wince}

program tb0532;

{Append was the recommended way to open devices in TP.
 A pitfall is that you cannot seek to the end of a device.

 It has to work on modern platforms too, because:
  - Rewrite will destroy the device on platforms where devices are
    files.
  - Reset doesn't allow writing to the device.
}

{$I+}

var null:text;

begin
{$ifdef Unix}
  assign(null,'/dev/null');
{$else}
  assign(null,'NUL');
{$endif}
  append(null);
  writeln(null,'Text disappearing into the black hole.');
  close(null);
end.

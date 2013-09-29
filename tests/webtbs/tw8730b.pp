{ %norun }
{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos,aix,android }
{ %NEEDLIBRARY }

{$mode delphi}

{$ifdef darwin}
{$PIC+}
{$endif darwin}

{$ifdef CPUX86_64}
{$ifndef WINDOWS}
{$PIC+}
{$endif WINDOWS}
{$endif CPUX86_64}

library tw8730b;

{$ifndef windows}
  {$linklib tw8730a}
{$endif}


uses uw8730b;

exports
{$if defined(darwin)}
Lib2Func name '_Lib2Func';
{$else}
Lib2Func;
{$endif}

end.

//= END OF FILE ===============================================================

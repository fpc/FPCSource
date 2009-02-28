
// see also 4.2 variable declaration in ref manual
//http://www.freepascal.org/docs-html/ref/refse19.html

// The curterm6 case in the manual warrants a separate bug report

{$if defined(darwin) or defined(os2) or defined(emx) or defined(palmos) or defined(symbian) or defined(watcom) or defined(wdosx) or defined(win32) or defined(wince)}
{$define underscoreprefix}
{$endif}

var  
  curterm1 : integer =1 ;  
 
  curterm2 : integer =2 ; cvar;  
 
 // these three are truly external -> no init is possible.

  curterm3 : integer  ; cvar; external;  
{$ifdef underscoreprefix}
  curterm4 : integer  ; external name '_curterm4b';    
{$ifdef unix}
 {$ifdef darwin}
  {Êon darwin, dll-vars always get the c-prefix, because otherwise
    you run into trouble because e.g. global symbols cannot start
    with 'L' (such symbols are always interpreted as local symbols
  }
  curterm5 : integer  ; external 'libc' name 'curterm5b';  
 {$else darwin}
  curterm5 : integer  ; external 'libc' name '_curterm5b';  
 {$endif}
 
{$endif}
{$else underscoreprefix}
  curterm4 : integer  ; external name 'curterm4b';    
{$ifdef unix}
  { on windows, this would actually search in libc.dll }
  curterm5 : integer  ; external 'libc' name 'curterm5b';  
{$endif}
{$endif underscoreprefix}
 
 
  curterm7 : integer =7 ; cvar;  export;  
  curterm8 : integer =8 ; cvar;  public;  
{$ifdef underscoreprefix}
  curterm9 : integer =9 ; export name '_me';  
  curterm10 : integer =10 ; public name '_ma';  
{$else underscoreprefix}
  curterm9 : integer =9 ; export name 'me';  
  curterm10 : integer =10 ; public name 'ma';  
{$endif underscoreprefix}
 
  curterm11 : integer =11 ;

// some declarations to make it compile in theory


 { the cvar modifier makes sure the c-prefix gets added if necessary }
 curterm3b : integer = 3; cvar ; export name 'curterm3';  
 curterm4b :integer = 4; cvar; export;
{$ifdef unix}
 curterm5b :integer = 5; cvar; export;
{$endif}

// we need a references too, so we print them.

begin
  if (curterm1<>1) then
    halt(1);

  if (curterm2<>2) then
    halt(2);

  if (curterm3<>3) then
    halt(3);

  if (curterm4<>4) then
    halt(4);

{$ifdef unix}
  if (curterm5<>5) then
    halt(5);
{$endif}

  if (curterm7<>7) then
    halt(7);

  if (curterm8<>8) then
    halt(8);

  if (curterm9<>9) then
    halt(9);

  if (curterm10<>10) then
    halt(10);

  if (curterm11<>11) then
    halt(11);
end.
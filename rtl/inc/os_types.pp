UNIT os_types;

{ 
  Note: 
  This file is meant as a helper for porting C and C++ interfaces to FreePascal. 
  It got required since FPC's resolving of the integer type depends on selected
  slang but not much on the target platform like its state of the art in C. 
  When porting API interfaces its recomended to use the types defined here.
  2000-Mar-18   alex    <AlexS@freepage.de>
}

INTERFACE

TYPE
// ordinal types
{$ifdef Go32v1}
  tOS_INT  = LongInt;
  tOS_UINT = DWord;
{$define OS_TYPES}
{$endif}
{$ifdef Go32v2}
  tOS_INT  = LongInt;
  tOS_UINT = DWord;
{$define OS_TYPES}
{$endif}
{$ifdef WIN16}
  tOS_INT  = SmallInt;
  tOS_UINT = Word;
{$define OS_TYPES}
{$endif}
{$ifdef WIN32}
  tOS_INT  = LongInt;
  tOS_UINT = DWord;
{$define OS_TYPES}
{$endif}
{$ifdef Netware}
  tOS_INT  = LongInt;
  tOS_UINT = DWord;
{$define OS_TYPES}
{$endif}
{$ifdef WIN64}
  tOS_INT  = Comp;  { 8 byte signed ordinal }
  tOS_UINT = QWord; { 8 byte unsigned ordinal }{ possibly still needs to be defined }
{$define OS_TYPES}
{$endif}
{$ifdef Unix}
{ TODO - how can we decide if we run on a 32 or a 64 bit linux platform ??? }
  tOS_INT  = LongInt;
  tOS_UINT = DWord;
{$define OS_TYPES}
{$endif}
{$ifdef OS2}
{ TODO - just an assumption }
  tOS_INT  = LongInt;
  tOS_UINT = DWord;
{$define OS_TYPES}
{$endif}

{$ifdef OS_TYPES}
// derive pointers from base types
  ptOS_INT = ^tOS_INT;
  ptOS_UINT = ^tOS_UINT;
{$else}
{$warning In Unit OS_Types: no case for your target present }
{$endif}

IMPLEMENTATION

{begin}{of init}
end.

  $Log$
  Revision 1.4  2002-03-16 12:46:09  armin
  + Netware added

  Revision 1.3  2000/11/13 13:40:04  marco
   * Renamefest

  Revision 1.2  2000/07/13 11:33:45  michael
  + removed logs
 
}

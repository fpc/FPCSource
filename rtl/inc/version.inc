{------------------------------------------------------------------------------
               Define VER_ABOVE_xx For easier Version support 
------------------------------------------------------------------------------}

{$IFDEF VER0_99_0}
  {$DEFINE VER_ABOVE0_99_0}
{$ENDIF VER0_99_0}

{$IFDEF VER0_99}
  {$DEFINE VER_ABOVE0_99}
{$ENDIF VER0_99}

{$IFDEF VER0_9_8}
  {$DEFINE VER_ABOVE0_9_8}
{$ENDIF VER0_9_8}

{$IFDEF VER0_9_7}
  {$DEFINE VER_ABOVE0_9_7}
{$ENDIF VER0_9_7}

{$IFDEF VER0_9_6}
  {$DEFINE VER_ABOVE0_9_6}
{$ENDIF VER0_9_6}

{$IFDEF VER0_9_5}
  {$DEFINE VER_ABOVE0_9_5}
{$ENDIF VER0_9_5}

{$IFDEF VER0_9_4}
  {$DEFINE VER_ABOVE0_9_4}
{$ENDIF VER0_9_4}

{$IFDEF VER0_9_3}
  {$DEFINE VER_ABOVE0_9_3}
{$ENDIF VER0_9_3}

{$IFDEF VER0_9_2}
  {$DEFINE VER_ABOVE0_9_2}
{$ENDIF VER0_9_2}

{$IFDEF VER0_9_1}
  {$DEFINE VER_ABOVE0_9_1}
{$ENDIF VER0_9_1}

{$IFDEF VER0_9}
  {$DEFINE VER_ABOVE0_9}
{$ENDIF VER0_9}

{$IFDEF VER0_6_6}
  {$DEFINE VER_ABOVE0_6_6}
{$ENDIF VER0_6_6}

{$IFDEF VER0_6_5}
  {$DEFINE VER_ABOVE0_6_5}
{$ENDIF VER0_6_5}

{$IFDEF VER0_6_4}
  {$DEFINE VER_ABOVE0_6_4}
{$ENDIF VER0_6_4}

{$IFDEF VER0_6_3}
  {$DEFINE VER_ABOVE0_6_3}
{$ENDIF VER0_6_3}

{$IFDEF VER0_6_2}
  {$DEFINE VER_ABOVE0_6_2}
{$ENDIF VER0_6_2}

{$IFDEF VER0_6_1}
  {$DEFINE VER_ABOVE0_6_1}
{$ENDIF VER0_6_1}

{$IFDEF VER0_6}
  {$DEFINE VER_ABOVE0_6}
{$ENDIF VER0_6}

{------------------------------------------------------------------------------
       Higher versions always include lower versions, so define them also 
------------------------------------------------------------------------------}

{$IFDEF VER_ABOVE0_99_0}
  {$DEFINE VER_ABOVE0_99}
{$ENDIF VER_ABOVE0_99_0}

{$IFDEF VER_ABOVE0_99}
  {$DEFINE VER_ABOVE0_9_8}
{$ENDIF VER_ABOVE0_99}

{$IFDEF VER_ABOVE0_9_8}
  {$DEFINE VER_ABOVE0_9_7}
{$ENDIF VER_ABOVE0_9_8}

{$IFDEF VER_ABOVE0_9_7}
  {$DEFINE VER_ABOVE0_9_6}
{$ENDIF VER_ABOVE0_9_7}

{$IFDEF VER_ABOVE0_9_6}
  {$DEFINE VER_ABOVE0_9_5}
{$ENDIF VER_ABOVE0_9_6}

{$IFDEF VER_ABOVE0_9_5}
  {$DEFINE VER_ABOVE0_9_4}
{$ENDIF VER_ABOVE0_9_5}

{$IFDEF VER_ABOVE0_9_4}
  {$DEFINE VER_ABOVE0_9_3}
{$ENDIF VER_ABOVE0_9_4}

{$IFDEF VER_ABOVE0_9_3}
  {$DEFINE VER_ABOVE0_9_2}
{$ENDIF VER_ABOVE0_9_3}

{$IFDEF VER_ABOVE0_9_2}
  {$DEFINE VER_ABOVE0_9_1}
{$ENDIF VER_ABOVE0_9_2}

{$IFDEF VER_ABOVE0_9_1}
  {$DEFINE VER_ABOVE0_9}
{$ENDIF VER_ABOVE0_9_1}

{$IFDEF VER_ABOVE0_9}
  {$DEFINE VER_ABOVE0_6_6}
{$ENDIF VER_ABOVE0_9}

{$IFDEF VER_ABOVE0_6_6}
  {$DEFINE VER_ABOVE0_6_5}
{$ENDIF VER_ABOVE0_6_6}

{$IFDEF VER_ABOVE0_6_5}
  {$DEFINE VER_ABOVE0_6_4}
{$ENDIF VER_ABOVE0_6_5}

{$IFDEF VER_ABOVE0_6_4}
  {$DEFINE VER_ABOVE0_6_3}
{$ENDIF VER_ABOVE0_6_4}

{$IFDEF VER_ABOVE0_6_3}
  {$DEFINE VER_ABOVE0_6_2}
{$ENDIF VER_ABOVE0_6_3}

{$IFDEF VER_ABOVE0_6_2}
  {$DEFINE VER_ABOVE0_6_1}
{$ENDIF VER_ABOVE0_6_2}

{$IFDEF VER_ABOVE0_6_1}
  {$DEFINE VER_ABOVE0_6}
{$ENDIF VER_ABOVE0_6_1}


{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.2  1998/01/26 12:00:36  michael
  + Added log at the end


  
  Working file: rtl/inc/version.inc
  description:
  ----------------------------
  revision 1.1
  date: 1998/01/11 02:44:07;  author: michael;  state: Exp;
  + Initial implementation (By Peter Vreman)
  =============================================================================
}

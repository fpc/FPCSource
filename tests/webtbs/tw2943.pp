{ Source provided for Free Pascal Bug Report 2943 }
{ Submitted by "marco (gory bugs department)" on  2004-02-06 }
{ e-mail:  }
{ $mode Delphi}
{$define linux}



{$IFDEF LINUX}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IFDEF CompilerVersion}
    //Important:  Don't use CompilerVersion here as
    //$IF's are evaluated before $IFDEF's
    //and Kylix 1 does not have CompilerVersion defined at all.
       {$IF RTLVersion = 14.1}
         {$DEFINE KYLIX2}
         {$DEFINE USEZLIBUNIT}
         {$DEFINE KYLIX1ORABOVE}
         {$DEFINE KYLIX2ORABOVE}
       {$IFEND}
       {$IF RTLVersion = 14.5}
         {$DEFINE KYLIX3}
         {$DEFINE USEZLIBUNIT}
         {$DEFINE KYLIX1ORABOVE}
         {$DEFINE KYLIX2ORABOVE}
       {$IFEND}
    {$ELSE}
       //CompilerVersion is not defined under Kylix 1
       {$DEFINE KYLIX1}
       {$DEFINE KYLIX1ORABOVE}
    {$ENDIF}

  {$ENDIF}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE OVERLOADING}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL6O}
{$ENDIF}

begin
end.

#!/usr/bin/perl
# FPC.PERL script. Accompagnies fpc.sty
# by Michael Van Canneyt <michael@tfdec1.fys.kuleuven.ac.be>
# December 1996
#
# Extension to LaTeX2HTML, to translate fpc style commands.
#

package main;

$fpcresult='';

sub FPCinternalproc{
local ($name, $decl, $desc, $errors, $seealso) = @_ ;
local ($result) = '';

 $result  = "<H2>$name</H2>\n<P>\n" ;
 $result .= "<H3>Declaration:</H3>\n<P>\n<TT>$decl</TT>\n<P>\n" ;
 $result .= "<H3>Description:</H3>\n<P>\n$desc\n<P>\n" ;
 $result .= "<H3>Errors:</H3>\n<P>\n$errors\n<P>\n" ;
 $result .= "<H3>See Also:</H3>\n<P>\n$seealso\n<P>\n" ;
 $result ;
}

sub FPCListing{
local ($name, $decl, $desc, $errors, $seealso) = @_ ;
local ($result) = '';

 $result  = "<H2>$name</H2>\n<P>\n" ;
 $result .= "<H3>Declaration:</H3>\n<P>\n<TT>$decl</TT>\n<P>\n" ;
 $result .= "<H3>Description:</H3>\n<P>\n$desc\n<P>\n" ;
 $result .= "<H3>Errors:</H3>\n<P>\n$errors\n<P>\n" ;
 $result .= "<H3>See Also:</H3>\n<P>\n$seealso\n<P>\n" ;
 $result ;
}

sub do_cmd_procedure
{
 $fpcresult  = "<H2>$_[0]</H2>\n<P>\n" ;
 $fpcresult .= "<H3>Declaration:</H3>\n<P>\n<TT>Procedure $_[0] $_[1]</TT>\n<P>\n" ;
 $fpcresult .= "<H3>Description:</H3>\n<P>\n$_[2]\n<P>\n" ;
 $fpcresult .= "<H3>Errors:</H3>\n<P>\n$_[3]\n<P>\n" ;
 $fpcresult .= "<H3>See Also:</H3>\n<P>\n$_[4]\n<P>\n" ;
 $fpcresult ;
}

sub do_cmd_Procedure
{
 $fpcresult  = "<H2>$_[0]</H2>\n<P>\n" ;
 $fpcresult .= "<H3>Declaration:</H3>\n<P>\n<TT>Procedure $_[0];</TT>\n<P>\n" ;
 $fpcresult .= "<H3>Description:</H3>\n<P>\n$_[1]\n<P>\n" ;
 $fpcresult .= "<H3>Errors:</H3>\n<P>\n$_[2]\n<P>\n" ;
 $fpcresult .= "<H3>See Also:</H3>\n<P>\n$_[3]\n<P>\n" ;
 $fpcresult ;
}

sub do_cmd_Function
{
 $fpcresult  = "<H2>$_[0]</H2>\n<P>\n" ;
 $fpcresult .= "<H3>Declaration:</H3>\n<P>\n<TT>Function $_[0] : $_[1]</TT>\n<P>\n" ;
 $fpcresult .= "<H3>Description:</H3>\n<P>\n$_[2]\n<P>\n" ;
 $fpcresult .= "<H3>Errors:</H3>\n<P>\n$_[3]\n<P>\n" ;
 $fpcresult .= "<H3>See Also:</H3>\n<P>\n$_[4]\n<P>\n" ;
 $fpcresult ;
}

sub do_cmd_function
{
 $fpcresult  = "<H2>$_[0]</H2>\n<P>\n" ;
 $fpcresult .= "<H3>Declaration:</H3>\n<P>\n<TT>Function $_[0] $_[1] : $_[2]</TT>\n<P>\n" ;
 $fpcresult .= "<H3>Description:</H3>\n<P>\n$_[3]\n<P>\n" ;
 $fpcresult .= "<H3>Errors:</H3>\n<P>\n$_[4]\n<P>\n" ;
 $fpcresult .= "<H3>See Also:</H3>\n<P>\n$_[5]\n<P>\n" ;
 $fpcresult ;
}

sub do_cmd_var{
local ($_) = @_;
"<TT>$_</TT>" ;
}

sub do_cmd_linux{
"LinuX" ;
}

sub do_cmd_dos{
"DOS" ;
}

sub do_cmd_msdos{
"MS-DOS" ;
}

sub do_cmd_windowsnt{
"Windows NT" ;
}

sub do_cmd_ostwo{
"OS/2" ;
}

sub do_cmd_seep{
"" ;
}

sub do_cmd_seef{
"" ;
}

sub do_cmd_seem{
"" ;
}

# For testing purposes
# print do_cmd_procedure ("Proc1","decl1","desc1","err1","see1");
# print do_cmd_Procedure ("Proc2","desc2","err2","see2");
# print do_cmd_function  ("Fun1","fdecl1","ftype1","fdesc1","ferr1","fsee1");
# print do_cmd_Function  ("Fun2","ftype2","fdesc2","ferr2","fsee2");

1; # required...
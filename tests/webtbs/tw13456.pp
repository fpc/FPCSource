program test;
{$ifdef fpc}
{$mode delphi$}{$H+}
{$endif}
uses SysUtils;
var
	s: string;
	s1: string[3];
begin
	s1:='255';
	s:='This is a big long string. This is going to be '+
		'A really big long string. The idea is it needs '+
		'to be over '+s1+' characters to see if FPC can '+
		'handle it. I have a program that isn''t '+
		'building because of the '+s1+' character string '+
		'limit. I can''t imagine why I''ve run into '+
		'this now. It should be wokring just dandy. '+
		'But for some reason it wasn''t so I thought a '+
		'test program outside of the project I was '+
		'working on may be beneficial to analysis.';

       if (length(s)<=255) then
         halt(1);	
end.


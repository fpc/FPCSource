{ Program to test OS-specific features of the system unit }
{ routines to test:                                       }
{   mkdir()                                               }
{   chdir()                                               }
{ This program shoulf not be executed in a roto directory }
{ Creates the following directory, and sets it as the     }
{ current directory.                                      }
{    ../testdir                                           }


Program tdir;
{$I+}

var
 s: string;
Begin
   Writeln('changing to parent directory...');
   chdir('..');
   Writeln('making directory...');
   mkdir('testdir');
   Writeln('going into the newly created directory...');
   chdir('testdir');
   Writeln('making directory...');
   mkdir('testdir2');
   WriteLn('removing directory ...');
   rmdir('testdir2');
   WriteLn('going directory up ...');
   chdir('..');
   WriteLn('removing directory ...');
   rmdir('testdir');
   WriteLn('getting current directory...');
   getdir(0,s);
   WriteLn(s);
end.

{
 $Log$
 Revision 1.2  2001-10-20 17:26:13  peter
   * several fixes to run also with kylix

 Revision 1.1  2001/07/14 04:25:17  carl
 system unit testing : basic directory services

}


FPCUnit 
   
   This is a port to Free Pascal of the JUnit core framework.
   see http://www.junit.org
   A great thank you goes to Kent Beck and Erich Gamma:
   "Never in the field of software development was so much owed by so many to 
   so few lines of code." (M.Fowler)
   
I've tried to follow as closely as possible the original JUnit code base,
so, as a side effect, developers already familiar with JUnit will find themselves
at home with this port to Free Pascal. If you are new to unit testing and test driven 
development, you can find a huge reference to articles and
howto's on the JUnit home page:
http://junit.sourceforge.net/#Documentation
http://www.junit.org/news/article/index.htm.

A simple example of a console test runner application that was used to write FPCUnit itself
is included in the demo directory. The tests are located in fpcunitests.pp, they can be used as
examples to see how to construct the tests and the test suites.

To be able to trace the line numbers of the
test errors (unhandled exceptions) it is required to use the -gl option 
to compile the project:. 
eg. $ fpc -Sd -gl testrunner.pp
If you don't like this additional feature you can disable the {$SHOWLINEINFO} directive
in the testresults.pp unit.

Usage:
-l or --list to show a list of registered tests
default format is xml, add --format=latex to output the list as latex source
-a or --all to run all the tests and show the results in xml format
The results can be redirected to an xml file,
for example: ./testrunner --all > results.xml
use --suite=MyTestSuiteName to run only the tests in a single test suite class

To use the simple console test runner in your own project, you can just edit the 
suiteconfig.pp unit to include your own units containing your tests instead of the unit
fpcunittests and register your tests in the RegisterUnitTests procedure like this:

unit suiteconfig;

interface

uses
  >>> Add the unit(s) containing your tests here;

procedure RegisterUnitTests;

implementation

uses
  testregistry;

procedure RegisterUnitTests;
begin
  //register your tests here
>>>  RegisterTests([TYourFirstTest, TYourSecondTest, TYourThirdTest,... ]);
end;

end.



Happy coding,
Dean Zobec




   
    
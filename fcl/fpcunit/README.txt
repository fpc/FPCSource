FPCUnit 
   
   This is a port to Free Pascal of the JUnit core framework.
   see http://www.junit.org
   A great thank you goes to Kent Beck and Erich Gamma:
   "Never in the field of software development was so much 
   owed by so many to so few lines of code." (M.Fowler)

   
In building FPCUnit I've tried to follow as closely as possible the original JUnit code base,so, as a side effect, developers already familiar with JUnit will find themselves at home with this port to Free Pascal. If you are new to unit testing and test driven development, you can find a huge reference to articles and howto's on the JUnit home page:
http://junit.sourceforge.net/#Documentation
http://www.junit.org/news/article/index.htm.

THE ASSERTIONS AND TEST CASES
To construct a test case you have to define your testcase class, inherited from TTestCase or from a descendant of TTestCase.
You'll tipically inherit from a descendant to add custom made asserts to your test case.
In fact there is a bunch of basic assertions defined as class methods of TAssert class, but they will never be able to cover all the possible types of assertions that can be necessary, especially when you would like to work with custom types.
E.g. you could have a class TMoney where you would like to assert whether two instances are equal.
Suppose you define that two instances of TMoney are equal if they have the same amount and the same
currency unit ( [10 EUR] = [10 EUR] <> [10 USD] ).
function TMoney.Equals(aMoney: TMoney): Boolean;
begin
  Result := (Amount = aMoney.Amount) and (CurrencyUnit = aMoney.CurrecyUnit); 
end;

In this case you'll probably like to define your own assertions:
TMoneyTestCase = class (TTestCase)
public
  class procedure AssertEquals(const aMessage: string; Expected, Actual: TMoney); overload;
  class procedure AssertEquals(Expected, Actual: TMoney); overload;
end;

class procedure TMoneyTestCase.AssertEquals(const aMessage: string; Expected, Actual: TMoney); 
begin
  AssertTrue(aMessage + ': expected <' + Expected.AsString + '> but was <' + Actual.AsString + '>'.
    Expected.Equals(Actual));
end;

and you'll inherit all your testcases from TMoneyTestCase;


Your testcase class will have a set of published methods, one for each test.
The framework picks up all the published methods and registers them as tests in the suite.
It's easy to temporarily disable a test from the suite: just move the declaration to the public section.


The fact that all assertions are static (class methods) make's it possible to use them outside of the test case, by simply adding fpcunit to your uses clause: 
e.g. by calling TAssert.AssertEqual(....)
It can be usefull when using the Mock Object techniques, the so called "Endo testing".

THE SAMPLE CONSOLE TEST RUNNERS
A simple example of a console test runner application that was used to write FPCUnit itself is included in the demo directory. The tests are located in fpcunitests.pp, they can be used as
examples to see how to construct the tests and the test suites.

To be able to trace the line numbers of the test errors (unhandled exceptions) it is required to use the -gl option 
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

To use the simple console test runner in your own project, you can just edit the suiteconfig.pp unit to include your own units containing your tests instead of the unit fpcunittests and register your tests in the RegisterUnitTests procedure like this:

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

There's another, more manual,  approach to construct your test suites.
Take a look at the TTestRunner constructor in the fpcunit/tests/frameworktest.pp for an example:

you can construct your test suite as in this example:
  
  FSuite := TTestSuite.Create;
  FSuite.TestName := 'my tests';
  FSuite.AddTestSuiteFromClass(TMyTestCase);
  FSuite.AddTest(TMySuiteTest.Suite());


this line uses a class function to construct a TTestSuite manually and adds it to 
set of tests to be runned:

FSuite.AddTest(TMySuiteTest.Suite());


class function TMySuiteTest.Suite: TTestSuite;
begin
  Result := TTestSuite.Create('TSuiteTest');
  Result.AddTest(TSuiteTest.CreateWithName('testOne'));
  Result.AddTest(TSuiteTest.CreateWithName('testTwo'));
  Result.AddTest(TSuiteTest.CreateWithName('testThree'));
end;

TESTING IN ISOLATION: SETUP AND TEARDOWN

All the constructed tests are isolated from each other:
it's important that the test that was run does not have any influence on the other tests that follow, there should be no cascading errors.
The tests should succeed in whichever order they are runned.
That's why the framework constructs each test by making a new instance of your TTestCase for each published method, so that every method runs in a freshly initialized environment.
You have other means of isolation: the Setup and Teardown method provided by TTestCase that are runned before and after the execution of every test. You can initialize the environment
in the setup method and finalize it in TearDown. You'll tipically override this two methods and costruct all the instances that are needed to run the test in Setup and destroy them in TearDown.
Anyway: there's a risk of someone doing initialization in the field, so this is why with Kent Beck and Erich Gamma decided to build a separate instance of TTestcase for each test to run. It's important to understand this concept.
See this small Martin Fowler's article on the subject:
http://www.martinfowler.com/bliki/JunitNewInstance.html

FPCUNIT AND LAZARUS

There is a GUI runner for those working with Lazarus and
Vincent Snijders recently integrated the fpcunit GUI runner in the Lazarus IDE.
Now it's very simple to set the Lazarus IDE to automatically construct your testing aplication.
Look under lazarus/components/fpcunit directory in the Lazarus source distribution.
1) Open and COMPILE the fpcunittestrunner.lpk 
2) Open and INSTALL the fpcunitide.lpk located in the /ide subdirectory

Now, by chosing File > New... > Project > FPCUnitApplication from the Lazarus menu you'll have a GUI runner and a simple TTestCase stub ready to be runned.
You can add additional templates for your test units by selecting
File > New... > File > FPCUnitTestCase

After running the tests a colored bar will show the results: the color of the bar is green when all the tests were successful, fuchsia, when only failures were encountered, red when errors are present (unexpected exceptions were rised). For each error the runner will show the unit and the line number where the exception occurred to make it easier to fix the test.

SUGGESTED READINGS
for those new to unit testing and for those that would like to improve their unit testing techniques I would suggest the following book:
Andy Hunt, Dave Thomas: Pragmatic Unit Testing in Java with JUnit (2003 Pragmatic Programmers LLC)



AKNOWLEDGMENTS
A big thank you to Michael Van Canneyt for the encouragement to adapt the framework for Free Pascal. He reviewed all the code and made it compatible with objfpc mode.
A special thank you to Vincent Snijders for his constant patience, encouragement and presence and his usefull advices.
He proved to be a unit testing expert and the first one to use the framework in his constant hunting for Lazarus bugs. He integrated the FPCUnit GUI test runner into Lazarus, to make it easier to build the tests with Lazarus.

Happy coding,
Dean Zobec
 

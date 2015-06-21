import org.freepascal.test.*;


public class JavaClass
{

public static void main(String[] args) throws java.lang.Exception
{
  TMyClass t = new TMyClass();

  tintfclass intfclass;
  tintfclass2 intfclass2;
  tinterface1 intf1;
  tinterface3 intf3;
  tinterface4 intf4;
  Object obj = new trec();
  System.out.println(((trec)obj).a);
 
  // check referencing a nested class
  tisclass1.tisclass1nested nestedclass = new tisclass1.tisclass1nested();
  

  System.out.println("t.test(10,8) should return 3: "+t.test(10,8));
  System.out.println("t.test(20,1) should return -1: "+t.test(20,1));
  t.setintfield(123);
  System.out.println("t.getintfield should return 123: "+t.getintfield());
  t.setstaticbytefield((byte)42);
  System.out.println("t.getstaticbytefield should return 42: "+t.getstaticbytefield());
  System.out.println("myrec.a should return 42: "+test.myrec.a);
  System.out.println("myrec.b should return 1234: "+test.myrec.b);
  System.out.println("TMyClass.rec.c should return 5678: "+TMyClass.rec.c);
  System.out.println("test.tcl should return 4: "+test.tcl);
  System.out.println("test.tcrec.a should return 1: "+test.tcrec.a);
  System.out.println("test.tcrec.e should return 5: "+test.tcrec.e);
  System.out.println("test.tcnestrec.r.d should return 4: "+test.tcnestrec.r.d);
  System.out.println("test.tcnestrec.r.arr[1] should return 6: "+test.tcnestrec.arr[1]);
  TMyClass.settestglobal(654321);
  System.out.println("TMyClass.gettestglobal should return 654321: "+TMyClass.gettestglobal());
  System.out.println("TMyClass.staticmul3(3) should return 9: "+TMyClass.staticmul3(3));
  System.out.println("testset should return 0: "+test.testset());
  System.out.println("testloop should return 0: "+test.testloop());
  System.out.println("testfloat should return 0: "+test.testfloat());
  System.out.println("testint2real should return 0: "+test.testint2real());
  System.out.println("testcnvint1 should return 0: "+test.testcnvint1());
  System.out.println("TestCmpListOneShort should return 0: "+test.TestCmpListOneShort());
  System.out.println("TestCmpListTwoShort should return 0: "+test.TestCmpListTwoShort());
  System.out.println("TestCmpListOneWord should return 0: "+test.TestCmpListOneWord());
  System.out.println("TestCmpListTwoWord should return 0: "+test.TestCmpListTwoWord());
  System.out.println("TestCmpListRangesOneShort should return 0: "+test.TestCmpListRangesOneShort());
  System.out.println("TestCmpListRangesTwoShort should return 0: "+test.TestCmpListRangesTwoShort());
  System.out.println("TestCmpListRangesOneWord should return 0: "+test.TestCmpListRangesOneWord());
  System.out.println("TestCmpListRangesTwoWord should return 0: "+test.TestCmpListRangesTwoWord());
  System.out.println("TestCmpListRangesThreeWord should return 0: "+test.TestCmpListRangesThreeWord());
  System.out.println("TestCmpListOneInt64 should return 0: "+test.TestCmpListOneInt64());
  System.out.println("TestCmpListTwoInt64 should return 0: "+test.TestCmpListTwoInt64());
  System.out.println("TestCmpListThreeInt64 should return 0: "+test.TestCmpListThreeInt64());
  System.out.println("TestCmpListRangesOneInt64 should return 0: "+test.TestCmpListRangesOneInt64());
  System.out.println("TestCmpListRangesTwoInt64 should return 0: "+test.TestCmpListRangesTwoInt64());
  System.out.println("testsqr should return 0: "+test.testsqr());
  System.out.println("testtrunc should return 0: "+test.testtrunc());
  System.out.println("testdynarr should return 0: "+test.testdynarr());
  System.out.println("testdynarr2 should return 0: "+test.testdynarr2());
  System.out.println("testbitcastintfloat should return 0: "+test.testbitcastintfloat());
  System.out.println("testis should return 0: "+test.testis());
  System.out.println("testneg should return 0: "+test.testneg());
  System.out.println("testtry1 should return 0: "+test.testtry1());
  System.out.println("testtry2 should return 0: "+test.testtry2());
  System.out.println("testtryfinally1 should return 0: "+test.testtryfinally1());
  System.out.println("testtryfinally2 should return 0: "+test.testtryfinally2());
  System.out.println("testtryfinally3 should return 0: "+test.testtryfinally3());
  System.out.println("testsmallarr1 should return 0: "+test.testsmallarr1());
  System.out.println("testsmallarr2 should return 0: "+test.testsmallarr2());
  System.out.println("testsmallarr3 should return 0: "+test.testsmallarr3());
  System.out.println("testsmallarr4 should return 0: "+test.testsmallarr4());
  System.out.println("testopenarr1 should return 0: "+test.testopenarr1());
  System.out.println("testopenarr2 should return 0: "+test.testopenarr2());
  System.out.println("testopenarr3 should return 0: "+test.testopenarr3());
  System.out.println("testopendynarr should return 0: "+test.testopendynarr());
  System.out.println("testrec1 should return 0: "+test.testrec1());
  System.out.println("testrec2 should return 0: "+test.testrec2());
  System.out.println("testopenarr1rec should return 0: "+test.testopenarr1rec());
  System.out.println("test.unitintconst should be 3: "+test.unitintconst);
  System.out.println("test.unitfloatconst should be 2.0: "+test.unitfloatconst);
  System.out.println("test.unitdoubleconst should be 0.1: "+test.unitdoubleconst);
  System.out.println("TMyclass.classintconst should be 4: "+TMyClass.classintconst);
  System.out.println("TMyclass.classfloatconst should be 3.0: "+TMyClass.classfloatconst);
  
  System.out.println();

  intfclass = new tintfclass();
  intf1 = intfclass;
  intfclass2 = new tintfclass2();

  System.out.println("intfclass.test(int) should return 10: "+intfclass.test(9));
  System.out.println("intf1.test(int) should return 10: "+intf1.test(9));
  System.out.println("intfclass.test(byte) should return 11: "+intfclass.test((byte)9));
  System.out.println("intfclass2.intf4test(int64) should return -2: "+intfclass2.intf4test((long)-12345*2-133));
  System.out.println("tinterface2.iconst should be 4: "+tinterface2.iconst);
  
  intfclass.Free();

  System.out.println("  *** Note: string tests expect that Java source file is compiled with '-encoding utf-8' and test is run with '-Dfile.encoding=UTF-8'");
  System.out.println("testunicodestring should return ~ê∂êºîƒ~©¬ -- ê = \u00ea ⊗ = \u2297: "+test.testunicodestring());
  System.out.println("  equal: "+test.testunicodestring().equals("~ê∂êºîƒ~©¬"));
  System.out.println("testunicodestring2 should return <\\\r\n\">: <"+test.testunicodestring2()+">");
  System.out.println("  equal: "+test.testunicodestring2().equals("\\\r\n\""));
  System.out.println("testunicodestring3 should return abcdef: "+test.testunicodestring3("abc"));
  System.out.println("  equal: "+test.testunicodestring3("abc").equals("abcdef"));
  System.out.println("testunicodestring4 should return ax2def: "+test.testunicodestring4("abcdef"));
  System.out.println("  equal: "+test.testunicodestring4("abcdef").equals("ax2def"));
  System.out.println("testunicodestring5 should return abcdefghij: "+test.testunicodestring5());
  System.out.println("  equal: "+test.testunicodestring5().equals("abcdefghij"));
  System.out.println("testunicodestring6 should return abcdefghi: "+test.testunicodestring6());
  System.out.println("  equal: "+test.testunicodestring6().equals("abcdefghi"));
  System.out.println("testunicodestring7 should return xbcdefghi: "+test.testunicodestring7());
  System.out.println("  equal: "+test.testunicodestring7().equals("xbcdefghi"));

  /* regular expression to transform numerical print statements into tests with exceptions:
   * search: System\.out\.println\(".*should (?:return|be) ([^:]*): "\+([^\r]*)\);
   * replace: if (\2 != \1)\r    throw new Exception("Invalid result for \2");
   */

  if (t.test(10,8) != 3)
    throw new Exception("Invalid result for t.test(10,8)");
  if (t.test(20,1) != -1)
    throw new Exception("Invalid result for t.test(20,1)");
  if (t.getintfield() != 123)
    throw new Exception("Invalid result for t.getintfield()");
  if (t.getstaticbytefield() != 42)
    throw new Exception("Invalid result for t.getstaticbytefield()");
  if (test.myrec.a != 42)
    throw new Exception("Invalid result for test.myrec.a");
  if (test.myrec.b != 1234)
    throw new Exception("Invalid result for test.myrec.b");
  if (test.tcl != 4)
    throw new Exception("Invalid result for test.tcl");
  if (test.tcrec.a != 1)
    throw new Exception("Invalid result for test.tcrec.a");
  if (test.tcrec.e != 5)
    throw new Exception("Invalid result for test.tcrec.e");
  if (test.tcnestrec.r.d != 4)
    throw new Exception("Invalid result for test.tcnestrec.r.d");
  if (test.tcnestrec.arr[1] != 6)
    throw new Exception("Invalid result for test.tcnestrec.arr[1]");
  if (TMyClass.gettestglobal() != 654321)
    throw new Exception("Invalid result for TMyClass.gettestglobal()");
  if (TMyClass.staticmul3(3) != 9)
    throw new Exception("Invalid result for TMyClass.staticmul3(3)");
  if (test.testset() != 0)
    throw new Exception("Invalid result for test.testset()");
  if (test.testloop() != 0)
    throw new Exception("Invalid result for test.testloop()");
  if (test.testfloat() != 0)
    throw new Exception("Invalid result for test.testfloat()");
  if (test.testint2real() != 0)
    throw new Exception("Invalid result for test.testint2real()");
  if (test.testcnvint1() != 0)
    throw new Exception("Invalid result for test.testcnvint1()");
  if (test.TestCmpListOneShort() != 0)
    throw new Exception("Invalid result for test.TestCmpListOneShort()");
  if (test.TestCmpListTwoShort() != 0)
    throw new Exception("Invalid result for test.TestCmpListTwoShort()");
  if (test.TestCmpListOneWord() != 0)
    throw new Exception("Invalid result for test.TestCmpListOneWord()");
  if (test.TestCmpListTwoWord() != 0)
    throw new Exception("Invalid result for test.TestCmpListTwoWord()");
  if (test.TestCmpListRangesOneShort() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesOneShort()");
  if (test.TestCmpListRangesTwoShort() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesTwoShort()");
  if (test.TestCmpListRangesOneWord() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesOneWord()");
  if (test.TestCmpListRangesTwoWord() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesTwoWord()");
  if (test.TestCmpListRangesThreeWord() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesThreeWord()");
  if (test.TestCmpListOneInt64() != 0)
    throw new Exception("Invalid result for test.TestCmpListOneInt64()");
  if (test.TestCmpListTwoInt64() != 0)
    throw new Exception("Invalid result for test.TestCmpListTwoInt64()");
  if (test.TestCmpListThreeInt64() != 0)
    throw new Exception("Invalid result for test.TestCmpListThreeInt64()");
  if (test.TestCmpListRangesOneInt64() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesOneInt64()");
  if (test.TestCmpListRangesTwoInt64() != 0)
    throw new Exception("Invalid result for test.TestCmpListRangesTwoInt64()");
  if (test.testsqr() != 0)
    throw new Exception("Invalid result for test.testsqr()");
  if (test.testtrunc() != 0)
    throw new Exception("Invalid result for test.testtrunc()");
  if (test.testdynarr() != 0)
    throw new Exception("Invalid result for test.testdynarr()");
  if (test.testdynarr2() != 0)
    throw new Exception("Invalid result for test.testdynarr2()");
  if (test.testbitcastintfloat() != 0)
    throw new Exception("Invalid result for test.testbitcastintfloat()");
  if (test.testis() != 0)
    throw new Exception("Invalid result for test.testis()");
  if (test.testneg() != 0)
    throw new Exception("Invalid result for test.testneg()");
  if (test.testtry1() != 0)
    throw new Exception("Invalid result for test.testtry1()");
  if (test.testtry2() != 0)
    throw new Exception("Invalid result for test.testtry2()");
  if (test.testtryfinally1() != 0)
    throw new Exception("Invalid result for test.testtryfinally1()");
  if (test.testtryfinally2() != 0)
    throw new Exception("Invalid result for test.testtryfinally2()");
  if (test.testtryfinally3() != 0)
    throw new Exception("Invalid result for test.testtryfinally3()");
  if (test.testsmallarr1() != 0)
    throw new Exception("Invalid result for test.testsmallarr1()");
  if (test.testsmallarr2() != 0)
    throw new Exception("Invalid result for test.testsmallarr2()");
  if (test.testsmallarr3() != 0)
    throw new Exception("Invalid result for test.testsmallarr3()");
  if (test.testsmallarr4() != 0)
    throw new Exception("Invalid result for test.testsmallarr4()");
  if (test.testopenarr1() != 0)
    throw new Exception("Invalid result for test.testopenarr1()");
  if (test.testopenarr2() != 0)
    throw new Exception("Invalid result for test.testopenarr2()");
  if (test.testopenarr3() != 0)
    throw new Exception("Invalid result for test.testopenarr3()");
  if (test.testopendynarr() != 0)
    throw new Exception("Invalid result for test.testopendynarr()");
  if (test.unitintconst != 3)
    throw new Exception("Invalid result for test.unitintconst");
  if (test.unitfloatconst != 2.0)
    throw new Exception("Invalid result for test.unitfloatconst");
  if (test.unitdoubleconst != 0.1)
    throw new Exception("Invalid result for test.unitdoubleconst");
  if (TMyClass.classintconst != 4)
    throw new Exception("Invalid result for TMyClass.classintconst");
  if (TMyClass.classfloatconst != 3.0)
    throw new Exception("Invalid result for TMyClass.classfloatconst");
  if (TMyClass.classdoubleconst != 0.3)
    throw new Exception("Invalid result for TMyClass.classdoubleconst");
  if (intfclass.test(9) != 10)
    throw new Exception("Invalid result for intfclass.test(9)");
  if (intf1.test(9) != 10)
    throw new Exception("Invalid result for intf1.test(9)");
  if (intfclass.test((byte)9) != 11)
    throw new Exception("Invalid result for intfclass.test((byte)9)");
  if (intfclass2.intf4test((long)-12345*2-133) != -2)
    throw new Exception("Invalid result for intfclass2.intf4test((long)-12345*2-133)");
  if (tinterface2.iconst != 4)
    throw new Exception("Invalid result for tinterface2.iconst");
  if (test.testrec1() != 0)
    throw new Exception("Invalid result for test.testrec1()");
  if (test.testopenarr1rec() != 0)
    throw new Exception("Invalid result for test.testopenarr1rec()");
  if (test.testrec2() != 0)
    throw new Exception("Invalid result for test.testrec2()");


}

}

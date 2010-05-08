// test code for class renaming
// compile with
// gcc -fno-exceptions -c -o $OS/$PLATFORM/cpptcl2.o cpptcl2.cpp

class TestClass {
	public:
		static void TestProc();
};

class testclass {
	public:
		static void TestProc();
};

void TestClass::TestProc() {};
void testclass::TestProc() {};

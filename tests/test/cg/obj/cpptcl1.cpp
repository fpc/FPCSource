// compile as
// gcc -fno-exceptions -c -o cpptestclass1.o cpptestclass1.cpp

class TestClass
{
  public:
    static void Test1();
    /* boolean */
    static void Test2(bool aArg1);
    /* unsigned ordinals */
    static void Test3(unsigned char aArg1);
    static void Test4(unsigned short aArg1);
    static void Test5(unsigned int aArg1);
    static void Test6(unsigned long long aArg1);
    /* signed ordinals */
    static void Test7(signed char aArg1);
    static void Test8(signed short aArg1);
    static void Test9(signed int aArg1);
    static void Test10(signed long long aArg1);
    /* floating point */
    static void Test11(float aArg1);
    static void Test12(double aArg1);
    /* chars */
    static void Test13(char aArg1);
    static void Test14(wchar_t aArg1);
    /* pointers */
    static void Test15(void* aArg1);
    static void Test16(char* aArg1);
    static void Test17(wchar_t* aArg1);
    static void Test18(unsigned int* aArg1);
    static void Test19(float* aArg1);
    /* by reference */
    static void Test20(signed int& aArg1);
    static void Test21(unsigned int& aArg1);
    static void Test22(void*& aArg1);
    static void Test23(char& aArg1);
    static void Test24(float& aArg1);
    /* combinations */
    static void Test25(unsigned char aArg1, unsigned short aArg2, unsigned int aArg3, unsigned long long aArg4);
    static void Test26(void* aArg1, char& aArg2, float aArg3);
};

void TestClass::Test1() { };
/* boolean */
void TestClass::Test2(bool aArg1){ };
/* unsigned ordinals */
void TestClass::Test3(unsigned char aArg1){ };
void TestClass::Test4(unsigned short aArg1){ };
void TestClass::Test5(unsigned int aArg1){ };
void TestClass::Test6(unsigned long long aArg1){ };
/* signed ordinals */
void TestClass::Test7(signed char aArg1){ };
void TestClass::Test8(signed short aArg1){ };
void TestClass::Test9(signed int aArg1){ };
void TestClass::Test10(signed long long aArg1){ };
/* floating point */
void TestClass::Test11(float aArg1){ };
void TestClass::Test12(double aArg1){ };
/* chars */
void TestClass::Test13(char aArg1){ };
void TestClass::Test14(wchar_t aArg1){ };
/* pointers */
void TestClass::Test15(void* aArg1){ };
void TestClass::Test16(char* aArg1){ };
void TestClass::Test17(wchar_t* aArg1){ };
void TestClass::Test18(unsigned int* aArg1){ };
void TestClass::Test19(float* aArg1){ };
/* by reference */
void TestClass::Test20(signed int& aArg1){ };
void TestClass::Test21(unsigned int& aArg1){ };
void TestClass::Test22(void*& aArg1){ };
void TestClass::Test23(char& aArg1){ };
void TestClass::Test24(float& aArg1){ };
/* combinations */
void TestClass::Test25(unsigned char aArg1, unsigned short aArg2, unsigned int aArg3, unsigned long long aArg4){ };
void TestClass::Test26(void* aArg1, char& aArg2, float aArg3){ };


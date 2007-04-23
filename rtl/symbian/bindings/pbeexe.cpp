// pbeexe.cpp
#include <e32std.h>

extern "C"
{

/* Pascal Entry point declaration */
int Pascal_E32Main();

/* class User : public UserHeap */

/* User::InfoPrint */
void User_InfoPrint()
{
     // Define a non-modifiable compile time allocated
     // descriptor (Symbian OS string)
     _LIT(KQHelloWorldString, "My Hello World");
     // show an indication
     User::InfoPrint(KQHelloWorldString);
}

}

/* Symbian OS Entry Point */
TInt E32Main()
{
     Pascal_E32Main();

     return KErrNone;
}

type
 WebNSUInteger = culong;
 WebNSInteger = clong;
 JSObjectRef = OpaqueType;
 JSGlobalContextRef = OpaqueType;

{

we need to set these or we loose lots of WebKit code!

#define WEBKIT_VERSION_1_0    0x0100
#define WEBKIT_VERSION_1_1    0x0110
#define WEBKIT_VERSION_1_2    0x0120
#define WEBKIT_VERSION_1_3    0x0130
#define WEBKIT_VERSION_2_0    0x0200
#define WEBKIT_VERSION_3_0    0x0300
#define WEBKIT_VERSION_3_1    0x0310
#define WEBKIT_VERSION_4_0    0x0400
#define WEBKIT_VERSION_LATEST 0x9999

#define WEBKIT_VERSION_MIN_REQUIRED WEBKIT_VERSION_LATEST

#ifndef WEBKIT_VERSION_MAX_ALLOWED
    #if WEBKIT_VERSION_MIN_REQUIRED > WEBKIT_VERSION_LATEST
        #define WEBKIT_VERSION_MAX_ALLOWED WEBKIT_VERSION_MIN_REQUIRED
    #else
        #define WEBKIT_VERSION_MAX_ALLOWED WEBKIT_VERSION_LATEST
    #endif
#endif

}



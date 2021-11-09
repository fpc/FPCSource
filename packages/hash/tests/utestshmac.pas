// See all test cases in: http://tools.ietf.org/html/rfc2202

unit UTestsHMAC;

{$mode objfpc}{$H+}

interface

uses
  HMAC, FPCUnit, TestRegistry;

type

  { TTestHMACMD5 }

  TTestHMACMD5 = class(TTestCase)
  published
    {
      test_case =     1
      key =           0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b
      key_len =       16
      data =          "Hi There"
      data_len =      8
      digest =        0x9294727a3638bb1c13f48ef8158bfc9d
    }
    procedure Test1;
    {
      test_case =     2
      key =           "Jefe"
      key_len =       4
      data =          "what do ya want for nothing?"
      data_len =      28
      digest =        0x750c783e6ab0b503eaa86e310a5db738
    }
    procedure Test2;
    {
      test_case =     3
      key =           0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      key_len         16
      data =          0xdd repeated 50 times
      data_len =      50
      digest =        0x56be34521d144c88dbb8c733f0e8b3f6
    }
    procedure Test3;
    {
      test_case =     4
      key =           0x0102030405060708090a0b0c0d0e0f10111213141516171819
      key_len         25
      data =          0xcd repeated 50 times
      data_len =      50
      digest =        0x697eaf0aca3a3aea3a75164746ffaa79
    }
    procedure Test4;
    {
      test_case =     5
      key =           0x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c
      key_len =       16
      data =          "Test With Truncation"
      data_len =      20
      digest =        0x56461ef2342edc00f9bab995690efd4c
      digest-96       0x56461ef2342edc00f9bab995
    }
    procedure Test5;
    {
      test_case =     6
      key =           0xaa repeated 80 times
      key_len =       80
      data =          "Test Using Larger Than Block-Size Key - Hash Key First"
      data_len =      54
      digest =        0xaa4ae5e15272d00e95705637ce8a3b55ed402112
    }
    procedure Test6;
    {
      test_case =     7
      key =           0xaa repeated 80 times
      key_len =       80
      data =          "Test Using Larger Than Block-Size Key and Larger
                      Than One Block-Size Data"
      data_len =      73
      digest =        0x6f630fad67cda0ee1fb1f562db3aa53e
    }
    procedure Test7;
  end;

  { TTestHMACSHA1 }

  TTestHMACSHA1 = class(TTestCase)
  published
    {
      test_case =     1
      key =           0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b
      key_len =       20
      data =          "Hi There"
      data_len =      8
      digest =        0xb617318655057264e28bc0b6fb378c8ef146be00
    }
    procedure Test1;
    {
      test_case =     2
      key =           "Jefe"
      key_len =       4
      data =          "what do ya want for nothing?"
      data_len =      28
      digest =        0xeffcdf6ae5eb2fa2d27416d5f184df9c259a7c79
    }
    procedure Test2;
    {
      test_case =     3
      key =           0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      key_len =       20
      data =          0xdd repeated 50 times
      data_len =      50
      digest =        0x125d7342b9ac11cd91a39af48aa17b4f63f175d3
    }
    procedure Test3;
    {
      test_case =     4
      key =           0x0102030405060708090a0b0c0d0e0f10111213141516171819
      key_len =       25
      data =          0xcd repeated 50 times
      data_len =      50
      digest =        0x4c9007f4026250c6bc8414f9bf50c86c2d7235da
    }
    procedure Test4;
    {
      test_case =     5
      key =           0x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c
      key_len =       20
      data =          "Test With Truncation"
      data_len =      20
      digest =        0x4c1a03424b55e07fe7f27be1d58bb9324a9a5a04
      digest-96 =     0x4c1a03424b55e07fe7f27be1
    }
    procedure Test5;
    {
      test_case =     6
      key =           0xaa repeated 80 times
      key_len =       80
      data =          "Test Using Larger Than Block-Size Key - Hash Key First"
      data_len =      54
      digest =        0xaa4ae5e15272d00e95705637ce8a3b55ed402112
    }
    procedure Test6;
    {
      test_case =     7
      key =           0xaa repeated 80 times
      key_len =       80
      data =          "Test Using Larger Than Block-Size Key and Larger
                      Than One Block-Size Data"
      data_len =      73
      digest =        0xe8e99d0f45237d786d6bbaa7965c7808bbff1a91
      data_len =      20
      digest =        0x4c1a03424b55e07fe7f27be1d58bb9324a9a5a04
      digest-96 =     0x4c1a03424b55e07fe7f27be1
    }
    procedure Test7;
  end;

implementation

{ TTestHMACMD5 }

procedure TTestHMACMD5.Test1;
begin
  AssertEquals('9294727a3638bb1c13f48ef8158bfc9d',
    HMACMD5(StringOfChar(#$0b, 16), 'Hi There'));
end;

procedure TTestHMACMD5.Test2;
begin
  AssertEquals('750c783e6ab0b503eaa86e310a5db738', HMACMD5('Jefe',
    'what do ya want for nothing?'));
end;

procedure TTestHMACMD5.Test3;
begin
  AssertEquals('56be34521d144c88dbb8c733f0e8b3f6',
    HMACMD5(StringOfChar(#$aa, 16), StringOfChar(#$dd, 50)));
end;

procedure TTestHMACMD5.Test4;
begin
  AssertEquals('697eaf0aca3a3aea3a75164746ffaa79', HMACMD5(#$01+#$02+#$03+#$04+
    #$05+#$06+#$07+#$08+#$09+#$0a+#$0b+#$0c+#$0d+#$0e+#$0f+#$10+#$11+#$12+#$13+
    #$14+#$15+#$16+#$17+#$18+#$19, StringOfChar(#$cd, 50)));
end;

procedure TTestHMACMD5.Test5;
var
  S: string;
begin
  S := HMACMD5(StringOfChar(#$0c, 16), 'Test With Truncation');
  AssertEquals('56461ef2342edc00f9bab995690efd4c', S);
  SetLength(S, 24);
  AssertEquals('56461ef2342edc00f9bab995', S);
end;

procedure TTestHMACMD5.Test6;
begin
  AssertEquals('6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd',
    HMACMD5(StringOfChar(#$aa, 80),
      'Test Using Larger Than Block-Size Key - Hash Key First'));
end;

procedure TTestHMACMD5.Test7;
begin
  AssertEquals('6f630fad67cda0ee1fb1f562db3aa53e',
    HMACMD5(StringOfChar(#$aa, 80),
      'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'));
end;

{ TTestHMACSHA1 }

procedure TTestHMACSHA1.Test1;
begin
  AssertEquals('b617318655057264e28bc0b6fb378c8ef146be00',
    HMACSHA1(StringOfChar(#$0b, 20), 'Hi There'));
end;

procedure TTestHMACSHA1.Test2;
begin
  AssertEquals('effcdf6ae5eb2fa2d27416d5f184df9c259a7c79', HMACSHA1('Jefe',
    'what do ya want for nothing?'));
end;

procedure TTestHMACSHA1.Test3;
begin
  AssertEquals('125d7342b9ac11cd91a39af48aa17b4f63f175d3',
    HMACSHA1(StringOfChar(#$aa, 20), StringOfChar(#$dd, 50)));
end;

procedure TTestHMACSHA1.Test4;
begin
  AssertEquals('4c9007f4026250c6bc8414f9bf50c86c2d7235da',
    HMACSHA1(#$01+#$02+#$03+#$04+#$05+#$06+#$07+#$08+#$09+#$0a+#$0b+#$0c+#$0d+
      #$0e+#$0f+#$10+#$11+#$12+#$13+#$14+#$15+#$16+#$17+#$18+#$19,
      StringOfChar(#$cd, 50)));
end;

procedure TTestHMACSHA1.Test5;
var
  S: string;
begin
  S := HMACSHA1(StringOfChar(#$0c, 20), 'Test With Truncation');
  AssertEquals('4c1a03424b55e07fe7f27be1d58bb9324a9a5a04', S);
  SetLength(S, 24);
  AssertEquals('4c1a03424b55e07fe7f27be1', S);
end;

procedure TTestHMACSHA1.Test6;
begin
  AssertEquals('aa4ae5e15272d00e95705637ce8a3b55ed402112',
    HMACSHA1(StringOfChar(#$aa, 80),
      'Test Using Larger Than Block-Size Key - Hash Key First'));
end;

procedure TTestHMACSHA1.Test7;
begin
  AssertEquals('e8e99d0f45237d786d6bbaa7965c7808bbff1a91',
    HMACSHA1(StringOfChar(#$aa, 80),
      'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data'));
end;

initialization
  RegisterTest(TTestHMACMD5);
  RegisterTest(TTestHMACSHA1);

end.


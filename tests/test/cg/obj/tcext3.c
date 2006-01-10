#include <stdint.h>

struct struct_arr1 {
  int8_t v[1];
  };

struct struct_arr2 {
  int8_t v[2];
  };

struct struct_arr3 {
  int8_t v[3];
  };

struct struct_arr4 {
  int8_t v[4];
  };

struct struct_arr5 {
  int8_t v[5];
  };

struct struct_arr6 {
  int8_t v[6];
  };

struct struct_arr7 {
  int8_t v[7];
  };
  
struct struct_arr8 {
  int8_t v[8];
  };
  
struct struct_arr9 {
  int8_t v[9];
  };

struct struct_arr10 {
  int8_t v[10];
  };

struct struct_arr11 {
  int8_t v[11];
  };

struct struct_arr15 {
  int8_t v[15];
  };

struct struct_arr16 {
  int8_t v[16];
  };

struct struct_arr17{
  int8_t v[17];
  };


struct struct_arr27 {
  int8_t v[27];
  };

struct struct_arr31 {
  int8_t v[31];
  };

struct struct_arr32 {
  int8_t v[32];
  };

struct struct_arr33 {
  int8_t v[33];
  };


struct struct1 {
  int8_t v;
  };

struct struct2 {
  int16_t v;
  };


struct struct3 {
  int16_t v1;
  int8_t v2;
  };

struct struct4 {
  int32_t v;
  };

struct struct5 {
  int32_t v1;
  int8_t v2;
  };

struct struct6 {
  int32_t v1;
  int16_t v2;
  };

struct struct7 {
  int32_t v1;
  int16_t v2;
  int8_t v3;
  };

struct struct8 {
  int64_t v;
  };

struct struct9 {
  int64_t v1;
  int8_t v2;
  };

struct struct10 {
  int64_t v1;
  int16_t v2;
  };

struct struct11 {
  int64_t v1;
  int16_t v2;
  int8_t v3;
  };

struct struct12 {
  int64_t v1;
  int32_t v2;
  };

struct struct13 {
  int64_t v1;
  int32_t v2;
  int8_t v3;
  };

struct struct14 {
  int64_t v1;
  int32_t v2;
  int16_t v3;
  };

struct struct15 {
  int64_t v1;
  int32_t v2;
  int16_t v3;
  int8_t v4;
  };

struct struct16 {
  int64_t v1;
  int64_t v2;
  };

struct struct31 {
  int64_t v1;
  int64_t v2;
  int64_t v3;
  int32_t v4;
  int16_t v5;
  int8_t v6;
  };

int64_t pass1(struct struct1 s) {
  return s.v;
}

int64_t pass2(struct struct2 s) {
  return s.v;
}

int64_t pass3(struct struct3 s) {
  return s.v1 + s.v2;
}

int64_t pass4(struct struct4 s) {
  return s.v;
}

int64_t pass5(struct struct5 s) {
  return s.v1 + s.v2;
}

int64_t pass6(struct struct6 s) {
  return s.v1 + s.v2;
}

int64_t pass7(struct struct7 s) {
  return s.v1 + s.v2 + s.v3;
}

int64_t pass8(struct struct8 s) {
  return s.v;
}

int64_t pass9(struct struct9 s) {
  return s.v1 + s.v2;
}

int64_t pass10(struct struct10 s) {
  return s.v1 + s.v2;
}

int64_t pass11(struct struct11 s) {
  return s.v1 + s.v2 + s.v3;
}

int64_t pass12(struct struct12 s) {
  return s.v1 + s.v2;
}

int64_t pass13(struct struct13 s) {
  return s.v1 + s.v2 + s.v3;
}

int64_t pass14(struct struct14 s) {
  return s.v1 + s.v2 + s.v3;
}

int64_t pass15(struct struct15 s) {
  return s.v1 + s.v2 + s.v3 + s.v4;
}

int64_t pass16(struct struct16 s) {
  return s.v1 + s.v2;
}

int64_t pass31(struct struct31 s) {
  return s.v1 + s.v2 + s.v3 + s.v4 + s.v5 + s.v6;
}

int64_t pass_arr1(struct struct_arr1 s) {
  int result = 0, i;
  for (i = 0; i < 1; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr2(struct struct_arr2 s) {
  int result = 0, i;
  for (i = 0; i < 2; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr3(struct struct_arr3 s) {
  int result = 0, i;
  for (i = 0; i < 3; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr4(struct struct_arr4 s) {
  int result = 0, i;
  for (i = 0; i < 4; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr5(struct struct_arr5 s) {
  int result = 0, i;
  for (i = 0; i < 5; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr6(struct struct_arr6 s) {
  int result = 0, i;
  for (i = 0; i < 6; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr7(struct struct_arr7 s) {
  int result = 0, i;
  for (i = 0; i < 7; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr8(struct struct_arr8 s) {
  int result = 0, i;
  for (i = 0; i < 8; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr9(struct struct_arr9 s) {
  int result = 0, i;
  for (i = 0; i < 9; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr10(struct struct_arr10 s) {
  int result = 0, i;
  for (i = 0; i < 10; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr11(struct struct_arr11 s) {
  int result = 0, i;
  for (i = 0; i < 11; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr15(struct struct_arr15 s) {
  int result = 0, i;
  for (i = 0; i < 15; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr16(struct struct_arr16 s) {
  int result = 0, i;
  for (i = 0; i < 16; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr17(struct struct_arr17 s) {
  int result = 0, i;
  for (i = 0; i < 17; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr27(struct struct_arr27 s) {
  int result = 0, i;
  for (i = 0; i < 27; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr31(struct struct_arr31 s) {
  int result = 0, i;
  for (i = 0; i < 31; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr32(struct struct_arr32 s) {
  int result = 0, i;
  for (i = 0; i < 32; i++)
    result += s.v[i];
  return result;
}

int64_t pass_arr33(struct struct_arr33 s) {
  int result = 0, i;
  for (i = 0; i < 33; i++)
    result += s.v[i];
  return result;
}

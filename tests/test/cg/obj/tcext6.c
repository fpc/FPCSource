#include <stdint.h>

struct struct1 {
  float v;
  };

struct struct2 {
  double v;
  };


struct struct3 {
  float v1;
  float v2;
  };

struct struct4 {
  double v1;
  float v2;
  };

struct struct5 {
  double v1;
  double v2;
  };

struct struct6 {
  double v1;
  float v2;
  float v3;
  };

struct struct7 {
  float v1;
  int32_t v2;
  double v3;
  };

struct struct8 {
  union {
    float v1;
    double d;
    };
  };

struct struct9 {
  int64_t v1;
  float v2;
  };

struct struct10 {
  int64_t v1;
  int16_t v2;
  float v3;
  };

struct struct11 {
  int64_t v1;
  double v2;
  };

struct struct12 {
  int64_t v1;
  float v2;
  float v3;
  };

struct struct13 {
  double v1;
  int64_t v2;
  };

struct struct14 {
  double v1;
  int32_t v2;
  int16_t v3;
  };

struct struct15 {
  double v1;
  int32_t v2;
  float v3;
  };

struct struct16 {
  float v1;
  float v2;
  float v3;
  float v4;
  };

struct struct17 {
  float v1;
  double v2;
  };

struct struct31 {
  long double v1;
  float v2;
  };

float pass1(struct struct1 s) {
  return s.v;
}

double pass2(struct struct2 s) {
  return s.v;
}

float pass3(struct struct3 s) {
  return s.v1 + s.v2;
}

double pass4(struct struct4 s) {
  return s.v1 + s.v2;
}

double pass5(struct struct5 s) {
  return s.v1 + s.v2;
}

double pass6(struct struct6 s) {
  return s.v1 + s.v2;
}

double pass7(struct struct7 s) {
  return s.v1 + s.v2 + s.v3;
}

double pass8(struct struct8 s) {
  return s.d;
}

int64_t pass9(struct struct9 s) {
  return s.v1 + (int64_t)s.v2;
}

int64_t pass10(struct struct10 s) {
  return s.v1 + s.v2 + (int64_t)s.v3;
}

int64_t pass11(struct struct11 s) {
  return s.v1 + (int64_t)s.v2;
}

int64_t pass12(struct struct12 s) {
  return s.v1 + (int64_t)s.v2 + (int64_t)s.v3;
}

int64_t pass13(struct struct13 s) {
  return (int64_t)s.v1 + s.v2;
}

int64_t pass14(struct struct14 s) {
  return (int64_t)s.v1 + s.v2 + s.v3;
}

int64_t pass15(struct struct15 s) {
  return (int64_t)s.v1 + s.v2 + (int64_t)s.v3;
}

float pass16(struct struct16 s) {
  return s.v1 + s.v2 + s.v3 + s.v4;
}

float pass17(struct struct17 s) {
  return s.v1 + s.v2;
}

long double pass31(struct struct31 s) {
  return s.v1 + s.v2;
}



struct struct1 pass1a(char b, struct struct1 s) {
  return s;
}

struct struct2 pass2a(char b, struct struct2 s) {
  return s;
}

struct struct3 pass3a(char b, struct struct3 s) {
  return s;
}

struct struct4 pass4a(char b, struct struct4 s) {
  return s;
}

struct struct5 pass5a(char b, struct struct5 s) {
  return s;
}

struct struct6 pass6a(char b, struct struct6 s) {
  return s;
}

struct struct7 pass7a(char b, struct struct7 s) {
  return s;
}

struct struct8 pass8a(char b, struct struct8 s) {
  return s;
}

struct struct9 pass9a(char b, struct struct9 s) {
  return s;
}

struct struct10 pass10a(char b, struct struct10 s) {
  return s;
}

struct struct11 pass11a(char b, struct struct11 s) {
  return s;
}

struct struct12 pass12a(char b, struct struct12 s) {
  return s;
}

struct struct13 pass13a(char b, struct struct13 s) {
  return s;
}

struct struct14 pass14a(char b, struct struct14 s) {
  return s;
}

struct struct15 pass15a(char b, struct struct15 s) {
  return s;
}

struct struct16 pass16a(char b, struct struct16 s) {
  return s;
}

struct struct17 pass17a(char b, struct struct17 s) {
  return s;
}

struct struct31 pass31a(char b, struct struct31 s) {
  return s;
}

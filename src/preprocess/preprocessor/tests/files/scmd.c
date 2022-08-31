#define E4(...) E3(E3(E3(E3(E3(E3(E3(E3(E3(E3(__VA_ARGS__))))))))))
#define E3(...) E2(E2(E2(E2(E2(E2(E2(E2(E2(E2(__VA_ARGS__))))))))))
#define E2(...) E1(E1(E1(E1(E1(E1(E1(E1(E1(E1(__VA_ARGS__))))))))))
#define E1(...) __VA_ARGS__

#define EMPTY()
#define CAT(a,b) CAT_(a,b)
#define CAT_(a,b) a##b
#define FX(f,x) f(x)
#define TUPLE_TAIL(x,...) (__VA_ARGS__)
#define TUPLE_AT_1(x,y,...) y
#define CHECK(...) TUPLE_AT_1(__VA_ARGS__,)

// reverse 8 arguments at a time, defer to LOOP is there are less then 8 arguments
#define SIMD_() SIMD
#define SIMD_END_END ,SIMD1
#define SIMD(a,b,c,d,e,f,g,h,...) CHECK(SIMD_END_##h,SIMD0)(a,b,c,d,e,f,g,h,__VA_ARGS__)
#define SIMD1 LOOP
#define SIMD0(a,b,c,d,e,f,g,h,...) SIMD_ EMPTY() ()(__VA_ARGS__),h,g,f,e,d,c,b,a

// reverse 1 argument at a time
#define LOOP_() LOOP
#define LOOP_END_END ,LOOP1
#define LOOP(x,...) CHECK(LOOP_END_##x,LOOP0)(x,__VA_ARGS__)
#define LOOP1(x,...) 
#define LOOP0(x,...) LOOP_ EMPTY() ()(__VA_ARGS__),x

#define REVERSE(...) FX(TUPLE_TAIL,E4(SIMD(__VA_ARGS__,END,END,END,END,END,END,END,END)))

REVERSE(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)

#define ADD_000(f) 0 f(0,
#define ADD_001(f) 1 f(0,
#define ADD_010(f) 1 f(0,
#define ADD_011(f) 0 f(1,
#define ADD_100(f) 1 f(0,
#define ADD_101(f) 0 f(1,
#define ADD_110(f) 0 f(1,
#define ADD_111(f) 1 f(1,
#define ADD_(c,x,y,n) ADD_##c##x##y(ADD_##n)

#define ADD_8(c,x,y) ADD_(c,x,y,7)
#define ADD_7(c,x,y) ,ADD_(c,x,y,6)
#define ADD_6(c,x,y) ,ADD_(c,x,y,5)
#define ADD_5(c,x,y) ,ADD_(c,x,y,4)
#define ADD_4(c,x,y) ,ADD_(c,x,y,3)
#define ADD_3(c,x,y) ,ADD_(c,x,y,2)
#define ADD_2(c,x,y) ,ADD_(c,x,y,1)
#define ADD_1(c,x,y) ,ADD_(c,x,y,0)
#define ADD_0(c,x,y) 

#define FX(f,...) f(__VA_ARGS__)
#define SCAN(...) __VA_ARGS__
#define ADD_8BIT(x,y) (FX(ADD_8BIT_,SCAN x, SCAN y))
#define ADD_8BIT_(x0,x1,x2,x3,x4,x5,x6,x7,y0,y1,y2,y3,y4,y5,y6,y7) \
    ADD_8(0,x0,y0)x1,y1)x2,y2)x3,y3)x4,y4)x5,y5)x6,y6)x7,y7),)

// bits are reversed (LSB,...,MSB):
ADD_8BIT((0,1,0,1,0,1,0,0),(0,0,1,0,0,1,1,0))

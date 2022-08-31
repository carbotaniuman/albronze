#define AT_0(a,b,c,d,e,f,g,h) a
#define AT_1(a,b,c,d,e,f,g,h) b
#define AT_2(a,b,c,d,e,f,g,h) c
#define AT_3(a,b,c,d,e,f,g,h) d
#define AT_4(a,b,c,d,e,f,g,h) e
#define AT_5(a,b,c,d,e,f,g,h) f
#define AT_6(a,b,c,d,e,f,g,h) g
#define AT_7(a,b,c,d,e,f,g,h) h

#define INC_(n) AT_##n(1,2,3,4,5,6,7,0)
#define DEC_(n) AT_##n(7,0,1,2,3,4,5,6)
#define INC(n) INC_(n)
#define DEC(n) DEC_(n)

INC(6) // 7
INC(DEC(INC(INC(1)))) // 3

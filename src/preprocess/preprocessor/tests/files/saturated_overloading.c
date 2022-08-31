#define TUPLE_AT_1(x0,x1,...) x1

#define COMMA_N(x) ,x
#define FOO(...) FOO_I(__VA_ARGS__,COMMA_N(FOO3),COMMA_N(FOO2),COMMA_N(FOO1),)(__VA_ARGS__)
#define FOO_I(x0,x1,x2,o,...) TUPLE_AT_1(o,FOOn,)

FOO(1)
FOO(1,2)
FOO(1,2,3)
FOO(1,2,3,4)
FOO(1,2,3,4,5)

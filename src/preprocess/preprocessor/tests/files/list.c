#define LIST_HEAD(a,b) a
#define LIST_TAIL(a,b) b

LIST_HEAD(1,(2,(3,))) // 1
LIST_TAIL(1,(2,(3,))) // (2,(3,))

#define TUPLE_AT_1(x,y,...) y
#define CHECK(...) TUPLE_AT_1(__VA_ARGS__,)
#define LIST_END(...) ,0
#define LIST_IS_END(x) CHECK(LIST_END x,1)

LIST_IS_END((9,))          // 0
LIST_IS_END(LIST_TAIL(9,)) // 1

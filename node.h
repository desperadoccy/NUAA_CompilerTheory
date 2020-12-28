typedef enum { TYPE_CONTENT, TYPE_INDEX, TYPE_OP } NodeEnum;
typedef struct {
    int name;
    int num;
    struct NodeTag *node[1];
} OpNode;

typedef struct NodeTag {
    NodeEnum type;
    union {
        int content;
        int index;
        OpNode op;
    };
} Node;

extern int Var[26];
/* A Bison parser, made by GNU Bison 3.7.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30704

/* Bison version string.  */
#define YYBISON_VERSION "3.7.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "cm.y"

#define YYPARSER /* distinguishes Yacc output from other code files */
#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *

static TreeNode * savedTree; /* stores syntax tree for later return */

static int yylex(void);
int yyerror(char*);

#line 86 "parser.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.hpp"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_MINIMUM_TOKEN = 3,              /* MINIMUM_TOKEN  */
  YYSYMBOL_ID = 4,                         /* ID  */
  YYSYMBOL_NUM = 5,                        /* NUM  */
  YYSYMBOL_ELSE = 6,                       /* ELSE  */
  YYSYMBOL_IF = 7,                         /* IF  */
  YYSYMBOL_INT = 8,                        /* INT  */
  YYSYMBOL_RETURN = 9,                     /* RETURN  */
  YYSYMBOL_VOID = 10,                      /* VOID  */
  YYSYMBOL_WHILE = 11,                     /* WHILE  */
  YYSYMBOL_PLUS = 12,                      /* PLUS  */
  YYSYMBOL_MINUS = 13,                     /* MINUS  */
  YYSYMBOL_TIMES = 14,                     /* TIMES  */
  YYSYMBOL_OVER = 15,                      /* OVER  */
  YYSYMBOL_LT = 16,                        /* LT  */
  YYSYMBOL_LE = 17,                        /* LE  */
  YYSYMBOL_GT = 18,                        /* GT  */
  YYSYMBOL_GE = 19,                        /* GE  */
  YYSYMBOL_EQ = 20,                        /* EQ  */
  YYSYMBOL_NE = 21,                        /* NE  */
  YYSYMBOL_ASSIGN = 22,                    /* ASSIGN  */
  YYSYMBOL_SEMI = 23,                      /* SEMI  */
  YYSYMBOL_COMMA = 24,                     /* COMMA  */
  YYSYMBOL_LPAREN = 25,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 26,                    /* RPAREN  */
  YYSYMBOL_LBRACK = 27,                    /* LBRACK  */
  YYSYMBOL_RBRACK = 28,                    /* RBRACK  */
  YYSYMBOL_LBRACE = 29,                    /* LBRACE  */
  YYSYMBOL_RBRACE = 30,                    /* RBRACE  */
  YYSYMBOL_ENDFILE = 31,                   /* ENDFILE  */
  YYSYMBOL_ERROR = 32,                     /* ERROR  */
  YYSYMBOL_MAXIMUM_TOKEN = 33,             /* MAXIMUM_TOKEN  */
  YYSYMBOL_LOWER_ELSE = 34,                /* LOWER_ELSE  */
  YYSYMBOL_YYACCEPT = 35,                  /* $accept  */
  YYSYMBOL_program = 36,                   /* program  */
  YYSYMBOL_declaration_list = 37,          /* declaration_list  */
  YYSYMBOL_declaration = 38,               /* declaration  */
  YYSYMBOL_var_declaration = 39,           /* var_declaration  */
  YYSYMBOL_type_specifier = 40,            /* type_specifier  */
  YYSYMBOL_fun_declaration = 41,           /* fun_declaration  */
  YYSYMBOL_params = 42,                    /* params  */
  YYSYMBOL_param_list = 43,                /* param_list  */
  YYSYMBOL_param = 44,                     /* param  */
  YYSYMBOL_compound_stmt = 45,             /* compound_stmt  */
  YYSYMBOL_local_declarations = 46,        /* local_declarations  */
  YYSYMBOL_statement_list = 47,            /* statement_list  */
  YYSYMBOL_statement = 48,                 /* statement  */
  YYSYMBOL_expression_stmt = 49,           /* expression_stmt  */
  YYSYMBOL_selection_stmt = 50,            /* selection_stmt  */
  YYSYMBOL_iteration_stmt = 51,            /* iteration_stmt  */
  YYSYMBOL_return_stmt = 52,               /* return_stmt  */
  YYSYMBOL_expression = 53,                /* expression  */
  YYSYMBOL_var = 54,                       /* var  */
  YYSYMBOL_simple_expression = 55,         /* simple_expression  */
  YYSYMBOL_relop = 56,                     /* relop  */
  YYSYMBOL_additive_expression = 57,       /* additive_expression  */
  YYSYMBOL_addop = 58,                     /* addop  */
  YYSYMBOL_term = 59,                      /* term  */
  YYSYMBOL_mulop = 60,                     /* mulop  */
  YYSYMBOL_factor = 61,                    /* factor  */
  YYSYMBOL_call = 62,                      /* call  */
  YYSYMBOL_args = 63,                      /* args  */
  YYSYMBOL_arg_list = 64,                  /* arg_list  */
  YYSYMBOL__id = 65,                       /* _id  */
  YYSYMBOL__num = 66                       /* _num  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   98

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  35
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  32
/* YYNRULES -- Number of rules.  */
#define YYNRULES  65
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  104

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   289


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    34,    34,    39,    41,    46,    48,    53,    55,    60,
      62,    66,    71,    73,    78,    80,    85,    87,    92,    97,
     100,   104,   107,   111,   113,   115,   117,   119,   124,   126,
     131,   134,   139,   144,   146,   151,   153,   158,   160,   165,
     167,   172,   174,   176,   178,   180,   182,   187,   189,   194,
     196,   200,   202,   207,   209,   214,   216,   218,   220,   225,
     230,   233,   237,   239,   244,   249
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "MINIMUM_TOKEN", "ID",
  "NUM", "ELSE", "IF", "INT", "RETURN", "VOID", "WHILE", "PLUS", "MINUS",
  "TIMES", "OVER", "LT", "LE", "GT", "GE", "EQ", "NE", "ASSIGN", "SEMI",
  "COMMA", "LPAREN", "RPAREN", "LBRACK", "RBRACK", "LBRACE", "RBRACE",
  "ENDFILE", "ERROR", "MAXIMUM_TOKEN", "LOWER_ELSE", "$accept", "program",
  "declaration_list", "declaration", "var_declaration", "type_specifier",
  "fun_declaration", "params", "param_list", "param", "compound_stmt",
  "local_declarations", "statement_list", "statement", "expression_stmt",
  "selection_stmt", "iteration_stmt", "return_stmt", "expression", "var",
  "simple_expression", "relop", "additive_expression", "addop", "term",
  "mulop", "factor", "call", "args", "arg_list", "_id", "_num", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289
};
#endif

#define YYPACT_NINF (-71)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-14)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      14,   -71,   -71,    33,    14,   -71,   -71,    50,   -71,   -71,
     -71,   -71,    21,   -71,    42,    54,    31,    50,    34,    51,
     -71,   -71,    46,    35,    37,    14,    53,    49,   -71,   -71,
     -71,   -71,   -71,    14,   -71,    50,     5,    -8,    55,     2,
      56,   -71,    13,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
      59,    57,   -71,    52,    22,   -71,   -71,    26,   -71,    13,
     -71,    60,    13,    58,   -71,    13,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,   -71,    13,    13,   -71,   -71,    13,    13,
      13,    61,   -71,    62,   -71,   -71,   -71,    43,    22,   -71,
     -71,    63,    66,    64,    38,    38,   -71,    13,   -71,    72,
     -71,   -71,    38,   -71
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     9,    10,     0,     2,     4,     5,     0,     6,     1,
       3,    64,     0,     7,     0,     0,    10,     0,     0,    12,
      15,    65,     0,    16,     0,     0,     0,     0,    20,    11,
      14,     8,    17,    22,    19,     0,     0,     0,     0,     0,
       0,    29,     0,    18,    24,    21,    23,    25,    26,    27,
       0,    56,    36,    40,    48,    52,    57,    37,    58,     0,
      33,     0,     0,     0,    28,     0,    49,    50,    41,    42,
      43,    44,    45,    46,     0,     0,    53,    54,     0,    61,
       0,     0,    34,     0,    55,    35,    56,    39,    47,    51,
      63,     0,    60,     0,     0,     0,    59,     0,    38,    30,
      32,    62,     0,    31
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -71,   -71,   -71,    81,    65,   -12,   -71,   -71,   -71,    68,
      67,   -71,   -71,   -63,   -71,   -71,   -71,   -71,   -39,   -70,
     -71,   -71,    12,   -71,    19,   -71,    17,   -71,   -71,   -71,
      -6,    82
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     3,     4,     5,     6,     7,     8,    18,    19,    20,
      44,    33,    36,    45,    46,    47,    48,    49,    50,    51,
      52,    74,    53,    75,    54,    78,    55,    56,    91,    92,
      57,    58
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      61,    12,    17,    63,    86,    86,    11,    21,    86,    11,
      21,    23,    38,    17,    39,    13,    40,    11,    21,    15,
      81,    35,     1,    83,     2,    60,    85,    42,    41,    37,
      42,    99,   100,     9,    28,    43,    76,    77,    42,   103,
      90,    93,    11,    21,    13,    38,    14,    39,    15,    40,
       1,    79,    16,    80,    11,    66,    67,   -13,   101,    21,
      24,    41,    27,    42,    66,    67,    28,    28,    68,    69,
      70,    71,    72,    73,    26,    25,    31,    32,   102,    65,
      59,    62,    64,    82,    84,    10,    87,    94,    95,    96,
      97,    29,    98,    30,    88,    89,     0,    22,    34
};

static const yytype_int8 yycheck[] =
{
      39,     7,    14,    42,    74,    75,     4,     5,    78,     4,
       5,    17,     7,    25,     9,    23,    11,     4,     5,    27,
      59,    33,     8,    62,    10,    23,    65,    25,    23,    35,
      25,    94,    95,     0,    29,    30,    14,    15,    25,   102,
      79,    80,     4,     5,    23,     7,    25,     9,    27,    11,
       8,    25,    10,    27,     4,    12,    13,    26,    97,     5,
      26,    23,    27,    25,    12,    13,    29,    29,    16,    17,
      18,    19,    20,    21,    28,    24,    23,    28,     6,    22,
      25,    25,    23,    23,    26,     4,    74,    26,    26,    26,
      24,    24,    28,    25,    75,    78,    -1,    15,    33
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     8,    10,    36,    37,    38,    39,    40,    41,     0,
      38,     4,    65,    23,    25,    27,    10,    40,    42,    43,
      44,     5,    66,    65,    26,    24,    28,    27,    29,    45,
      44,    23,    28,    46,    39,    40,    47,    65,     7,     9,
      11,    23,    25,    30,    45,    48,    49,    50,    51,    52,
      53,    54,    55,    57,    59,    61,    62,    65,    66,    25,
      23,    53,    25,    53,    23,    22,    12,    13,    16,    17,
      18,    19,    20,    21,    56,    58,    14,    15,    60,    25,
      27,    53,    23,    53,    26,    53,    54,    57,    59,    61,
      53,    63,    64,    53,    26,    26,    26,    24,    28,    48,
      48,    53,     6,    48
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    35,    36,    37,    37,    38,    38,    39,    39,    40,
      40,    41,    42,    42,    43,    43,    44,    44,    45,    46,
      46,    47,    47,    48,    48,    48,    48,    48,    49,    49,
      50,    50,    51,    52,    52,    53,    53,    54,    54,    55,
      55,    56,    56,    56,    56,    56,    56,    57,    57,    58,
      58,    59,    59,    60,    60,    61,    61,    61,    61,    62,
      63,    63,    64,    64,    65,    66
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     3,     6,     1,
       1,     6,     1,     1,     3,     1,     2,     4,     4,     2,
       0,     2,     0,     1,     1,     1,     1,     1,     2,     1,
       5,     7,     5,     2,     3,     3,     1,     1,     4,     3,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     3,     1,     1,     1,     3,     1,     1,     1,     4,
       1,     0,     3,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
# ifndef YY_LOCATION_PRINT
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: declaration_list  */
#line 35 "cm.y"
          { savedTree = makehead(yyvsp[0]); }
#line 1207 "parser.cpp"
    break;

  case 3: /* declaration_list: declaration_list declaration  */
#line 40 "cm.y"
          { yyval = addSibling(yyvsp[-1], yyvsp[0]); }
#line 1213 "parser.cpp"
    break;

  case 4: /* declaration_list: declaration  */
#line 42 "cm.y"
          { yyval = yyvsp[0]; }
#line 1219 "parser.cpp"
    break;

  case 5: /* declaration: var_declaration  */
#line 47 "cm.y"
          { yyval = yyvsp[0]; }
#line 1225 "parser.cpp"
    break;

  case 6: /* declaration: fun_declaration  */
#line 49 "cm.y"
          { yyval = yyvsp[0]; }
#line 1231 "parser.cpp"
    break;

  case 7: /* var_declaration: type_specifier _id SEMI  */
#line 54 "cm.y"
          { yyval = newVariableDeclarationNode(yyvsp[-2], yyvsp[-1]); }
#line 1237 "parser.cpp"
    break;

  case 8: /* var_declaration: type_specifier _id LBRACK _num RBRACK SEMI  */
#line 56 "cm.y"
          { yyval = newArrayDeclarationNode(yyvsp[-5], yyvsp[-4], yyvsp[-2]); }
#line 1243 "parser.cpp"
    break;

  case 9: /* type_specifier: INT  */
#line 61 "cm.y"
          { yyval = newTokenTypeNode(INT); }
#line 1249 "parser.cpp"
    break;

  case 10: /* type_specifier: VOID  */
#line 63 "cm.y"
          { yyval = newTokenTypeNode(VOID); }
#line 1255 "parser.cpp"
    break;

  case 11: /* fun_declaration: type_specifier _id LPAREN params RPAREN compound_stmt  */
#line 67 "cm.y"
          { yyval = newFunctionDeclarationNode(yyvsp[-5], yyvsp[-4], yyvsp[-2], yyvsp[0]); }
#line 1261 "parser.cpp"
    break;

  case 12: /* params: param_list  */
#line 72 "cm.y"
          { yyval = yyvsp[0]; }
#line 1267 "parser.cpp"
    break;

  case 13: /* params: VOID  */
#line 74 "cm.y"
          { yyval = NULL; }
#line 1273 "parser.cpp"
    break;

  case 14: /* param_list: param_list COMMA param  */
#line 79 "cm.y"
          { yyval = addSibling(yyvsp[-2], yyvsp[0]); }
#line 1279 "parser.cpp"
    break;

  case 15: /* param_list: param  */
#line 81 "cm.y"
          { yyval = yyvsp[0]; }
#line 1285 "parser.cpp"
    break;

  case 16: /* param: type_specifier _id  */
#line 86 "cm.y"
          { yyval = newVariableParameterNode(yyvsp[-1], yyvsp[0]); }
#line 1291 "parser.cpp"
    break;

  case 17: /* param: type_specifier _id LBRACK RBRACK  */
#line 88 "cm.y"
          { yyval = newArrayParameterNode(yyvsp[-3], yyvsp[-2]); }
#line 1297 "parser.cpp"
    break;

  case 18: /* compound_stmt: LBRACE local_declarations statement_list RBRACE  */
#line 93 "cm.y"
          { yyval = newCompoundStatementNode(yyvsp[-2], yyvsp[-1]); }
#line 1303 "parser.cpp"
    break;

  case 19: /* local_declarations: local_declarations var_declaration  */
#line 98 "cm.y"
          { yyval = addSibling(yyvsp[-1], yyvsp[0]); }
#line 1309 "parser.cpp"
    break;

  case 20: /* local_declarations: %empty  */
#line 100 "cm.y"
          { yyval = NULL; }
#line 1315 "parser.cpp"
    break;

  case 21: /* statement_list: statement_list statement  */
#line 105 "cm.y"
          { yyval = addSibling(yyvsp[-1], yyvsp[0]); }
#line 1321 "parser.cpp"
    break;

  case 22: /* statement_list: %empty  */
#line 107 "cm.y"
          { yyval = NULL; }
#line 1327 "parser.cpp"
    break;

  case 23: /* statement: expression_stmt  */
#line 112 "cm.y"
          { yyval = yyvsp[0]; }
#line 1333 "parser.cpp"
    break;

  case 24: /* statement: compound_stmt  */
#line 114 "cm.y"
          { yyval = yyvsp[0]; }
#line 1339 "parser.cpp"
    break;

  case 25: /* statement: selection_stmt  */
#line 116 "cm.y"
          { yyval = yyvsp[0]; }
#line 1345 "parser.cpp"
    break;

  case 26: /* statement: iteration_stmt  */
#line 118 "cm.y"
          { yyval = yyvsp[0]; }
#line 1351 "parser.cpp"
    break;

  case 27: /* statement: return_stmt  */
#line 120 "cm.y"
          { yyval = yyvsp[0]; }
#line 1357 "parser.cpp"
    break;

  case 28: /* expression_stmt: expression SEMI  */
#line 125 "cm.y"
          { yyval = newExpressionStatementNode(yyvsp[-1]); }
#line 1363 "parser.cpp"
    break;

  case 29: /* expression_stmt: SEMI  */
#line 127 "cm.y"
          { yyval = NULL; }
#line 1369 "parser.cpp"
    break;

  case 30: /* selection_stmt: IF LPAREN expression RPAREN statement  */
#line 132 "cm.y"
          { yyval = newSelectionStatementNode(yyvsp[-2], yyvsp[0], NULL); }
#line 1375 "parser.cpp"
    break;

  case 31: /* selection_stmt: IF LPAREN expression RPAREN statement ELSE statement  */
#line 135 "cm.y"
          { yyval = newSelectionStatementNode(yyvsp[-4], yyvsp[-2], yyvsp[0]); }
#line 1381 "parser.cpp"
    break;

  case 32: /* iteration_stmt: WHILE LPAREN expression RPAREN statement  */
#line 140 "cm.y"
          { yyval = newIterationStatementNode(yyvsp[-2], yyvsp[0]); }
#line 1387 "parser.cpp"
    break;

  case 33: /* return_stmt: RETURN SEMI  */
#line 145 "cm.y"
          { yyval = newReturnStatementNode(NULL); }
#line 1393 "parser.cpp"
    break;

  case 34: /* return_stmt: RETURN expression SEMI  */
#line 147 "cm.y"
          { yyval = newReturnStatementNode(yyvsp[-1]); }
#line 1399 "parser.cpp"
    break;

  case 35: /* expression: var ASSIGN expression  */
#line 152 "cm.y"
         { yyval = newAssignExpressionNode(yyvsp[-2], yyvsp[0]); }
#line 1405 "parser.cpp"
    break;

  case 36: /* expression: simple_expression  */
#line 154 "cm.y"
         { yyval = yyvsp[0]; }
#line 1411 "parser.cpp"
    break;

  case 37: /* var: _id  */
#line 159 "cm.y"
          { yyval = yyvsp[0]; }
#line 1417 "parser.cpp"
    break;

  case 38: /* var: _id LBRACK expression RBRACK  */
#line 161 "cm.y"
          { yyval = newArrayNode(yyvsp[-3], yyvsp[-1]); }
#line 1423 "parser.cpp"
    break;

  case 39: /* simple_expression: additive_expression relop additive_expression  */
#line 166 "cm.y"
          { yyval = newComparisonExpressionNode(yyvsp[-2], yyvsp[-1], yyvsp[0]); }
#line 1429 "parser.cpp"
    break;

  case 40: /* simple_expression: additive_expression  */
#line 168 "cm.y"
          { yyval = yyvsp[0]; }
#line 1435 "parser.cpp"
    break;

  case 41: /* relop: LT  */
#line 173 "cm.y"
          { yyval = newTokenTypeNode(LT); }
#line 1441 "parser.cpp"
    break;

  case 42: /* relop: LE  */
#line 175 "cm.y"
          { yyval = newTokenTypeNode(LE); }
#line 1447 "parser.cpp"
    break;

  case 43: /* relop: GT  */
#line 177 "cm.y"
          { yyval = newTokenTypeNode(GT); }
#line 1453 "parser.cpp"
    break;

  case 44: /* relop: GE  */
#line 179 "cm.y"
          { yyval = newTokenTypeNode(GE); }
#line 1459 "parser.cpp"
    break;

  case 45: /* relop: EQ  */
#line 181 "cm.y"
          { yyval = newTokenTypeNode(EQ); }
#line 1465 "parser.cpp"
    break;

  case 46: /* relop: NE  */
#line 183 "cm.y"
          { yyval = newTokenTypeNode(NE); }
#line 1471 "parser.cpp"
    break;

  case 47: /* additive_expression: additive_expression addop term  */
#line 188 "cm.y"
          { yyval = newAdditiveExpressionNode(yyvsp[-2], yyvsp[-1], yyvsp[0]); }
#line 1477 "parser.cpp"
    break;

  case 48: /* additive_expression: term  */
#line 190 "cm.y"
          { yyval = yyvsp[0]; }
#line 1483 "parser.cpp"
    break;

  case 49: /* addop: PLUS  */
#line 195 "cm.y"
          { yyval = newTokenTypeNode(PLUS); }
#line 1489 "parser.cpp"
    break;

  case 50: /* addop: MINUS  */
#line 197 "cm.y"
          { yyval = newTokenTypeNode(MINUS); }
#line 1495 "parser.cpp"
    break;

  case 51: /* term: term mulop factor  */
#line 201 "cm.y"
          { yyval = newMultiplicativeExpressionNode(yyvsp[-2], yyvsp[-1], yyvsp[0]); }
#line 1501 "parser.cpp"
    break;

  case 52: /* term: factor  */
#line 203 "cm.y"
          { yyval = yyvsp[0]; }
#line 1507 "parser.cpp"
    break;

  case 53: /* mulop: TIMES  */
#line 208 "cm.y"
          { yyval = newTokenTypeNode(TIMES); }
#line 1513 "parser.cpp"
    break;

  case 54: /* mulop: OVER  */
#line 210 "cm.y"
          { yyval = newTokenTypeNode(OVER); }
#line 1519 "parser.cpp"
    break;

  case 55: /* factor: LPAREN expression RPAREN  */
#line 215 "cm.y"
          { yyval = yyvsp[-1]; }
#line 1525 "parser.cpp"
    break;

  case 56: /* factor: var  */
#line 217 "cm.y"
          { yyval = yyvsp[0]; }
#line 1531 "parser.cpp"
    break;

  case 57: /* factor: call  */
#line 219 "cm.y"
          { yyval = yyvsp[0]; }
#line 1537 "parser.cpp"
    break;

  case 58: /* factor: _num  */
#line 221 "cm.y"
          { yyval = yyvsp[0]; }
#line 1543 "parser.cpp"
    break;

  case 59: /* call: _id LPAREN args RPAREN  */
#line 226 "cm.y"
          { yyval = newCallNode(yyvsp[-3], yyvsp[-1]); }
#line 1549 "parser.cpp"
    break;

  case 60: /* args: arg_list  */
#line 231 "cm.y"
          { yyval = yyvsp[0]; }
#line 1555 "parser.cpp"
    break;

  case 61: /* args: %empty  */
#line 233 "cm.y"
          { yyval = NULL; }
#line 1561 "parser.cpp"
    break;

  case 62: /* arg_list: arg_list COMMA expression  */
#line 238 "cm.y"
          { yyval = addSibling(yyvsp[-2], yyvsp[0]); }
#line 1567 "parser.cpp"
    break;

  case 63: /* arg_list: expression  */
#line 240 "cm.y"
          { yyval = yyvsp[0]; }
#line 1573 "parser.cpp"
    break;

  case 64: /* _id: ID  */
#line 245 "cm.y"
          { yyval = newVariableNode(tokenString); }
#line 1579 "parser.cpp"
    break;

  case 65: /* _num: NUM  */
#line 250 "cm.y"
          { yyval = newConstantNode(tokenString); }
#line 1585 "parser.cpp"
    break;


#line 1589 "parser.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;
#endif


/*-------------------------------------------------------.
| yyreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 253 "cm.y"


int yyerror(char * message)
{
  	fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  	fprintf(listing, "Current token: ");
  	printToken(yychar,tokenString);
  	return 0;
}

static int yylex(void)
{
  	TokenType tok = getToken();
  	if (tok == ENDFILE) return 0;
  	if (tok == ERROR)
    {
      	fprintf(listing,"Lexical analyze error at line %d\n",lineno);
      	fprintf(listing,"Current token: %s",tokenString);
      	return 0;
    }
  	return tok;
}

TreeNode * parse(void)
{
  	yyparse();
  	return savedTree;
}


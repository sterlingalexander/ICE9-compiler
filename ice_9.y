%{
#include <cstdio>
#include <stdlib.h>
#include <vector>
#include <iostream>
#include <sstream>
#include <string>
#include <stack>
#include <map>
using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
extern "C" int yyparse();
extern "C" int yylineno();
extern "C" char* yytext();
extern "C" FILE *yyin;
extern int line_num;
#define YYDEBUG 1

/* ======================================================== */
/* Structs */
/* ======================================================== */

struct typeRec  {
   string type;      // type name, only int, bool, string, and array
   int size;         // only for arrays
   typeRec *next;    // pointer to next type record
   bool canAssign;   // flag to mark some var's non-assignable.
   
   void print()  {
   cout << "\t **== Type Record ==**\n";
   cout << "\t\tType Name: " << type << "\n";
   cout << "\t\tSize (array only): " << size << "\n";
   cout << "\t\tPointer to next typeRec: " << next << "\n";
   cout << "\t\tAssignable?: " << canAssign << "\n";
   }
};

struct ptnode  {
      bool assignable;
      int val;
      string sval;
      typeRec *type;
      string id;
      string op;
      
      void print()  {
         cout << "\tassignable is set to --> " << assignable << "\n";
         cout << "\tval is set to --> " << val << "\n";
         cout << "\tstring is set to --> " << sval << "\n";
         if (type) cout << "\ttype is set to --> " << type->type << "\n";
         else cout << "\tType has a NULL pointer!\n";
         cout << "\tid is set to --> " << id << "\n";
         cout << "\top is set to --> " << op << "\n";
      }
   };

/* ======================================================== */
/*  GLOBALS - treat with care!  */
/* ======================================================== */

stack<string> idStack;
map<string, typeRec*> varSymbols;
map<string, typeRec*> typeSymbols;
vector<map<string,typeRec*> > scopeVector;
int DEBUG = 0;                   // set to 1 for verbose debug information
int LOOPCOUNT = 0;
int SCOPECOUNT = 0;

/* ======================================================== */
/* Function prototypes */
/* ======================================================== */

void yyerror(const char *s);
void printVarSymbols(); 
void semanticError(int line_num, string error, string var);
bool isNum(string s);
typeRec* getBaseType(ptnode *p);

/* ======================================================== */
/* Functions */
/* ======================================================== */

/* Creates global symbol table for var types */
void initTypeTable()  {
   
   if (DEBUG) cout << "Begin base type addition to type table...\n";
   typeRec *intval = new typeRec;   intval->type = "int";     intval->next = NULL;
   typeRec *sval = new typeRec;     sval->type = "string";     sval->next = NULL;
   typeRec *bval = new typeRec;     bval->type = "bool";     bval->next = NULL;
   
   typeSymbols["int"] = intval;
   typeSymbols["string"] = sval;
   typeSymbols["bool"] = bval;
   if (DEBUG) cout << "Successful addition of base types to the type table...\n";
}

typeRec* getSymbolType(string s)  {
   if (DEBUG) cout << "Begin call: getSymbolType(" << s << ")...\n";
   if ( varSymbols.find(s) != varSymbols.end() ) {
      typeRec *t = (varSymbols.find(s))->second;
      if (DEBUG) cout << "Returning from call:  getSymbolType(" << s << ")...successful lookup ==> " << t->type << "\n";
      return t;
   }
   else  {
      if (DEBUG) cout << "Returning from call:  getSymbolType(" << s << ")...no type info in symbol table\n";
      return NULL;
   }
}

typeRec* getType(string s)  {
   if (DEBUG) cout << "Begin call: getType(" << s << ")...\n";
   if ( typeSymbols.find(s) != typeSymbols.end() )  {
      if (DEBUG) cout << "Returning from call: getType(" << s << ")...success on lookup\n";
      return (typeSymbols.find(s))->second;
   }
   else  {
      if (DEBUG) cout << "Returning from call: getType(" << s << ")...failed on lookup\n";
      return NULL;
   }
}

bool varDefined(char *c, bool flag=false)  {
   string s = string(c);
   if (DEBUG) cout << "Begin call: varDefined(" << s << ", " << flag << ")...\n";
   if (varSymbols.find(s) != varSymbols.end() )  {
      if (DEBUG) cout << "Returning from call: varDefined(" << s << ")...success on lookup\n";
      return true;
   }
   else  {
      if (flag)  {
         for (int i = SCOPECOUNT - 1; i >= 0 ; i--)  {
            if (scopeVector[i].find(s) != scopeVector[i].end() )  {
         if (DEBUG) cout << "Returning from call: varDefined(" << s << ")...success on lookup in scope " << SCOPECOUNT << "\n";
         return true;
            }
         }
      if (DEBUG) cout << "Returning from call: varDefined(" << s << ")...failed lookup\n";
      return false;
      }
   }  
}

bool typeDefined(char *c)  {
   string s = string(c);
   if (DEBUG) cout << "Begin call: typeDefined(" << s << ")...\n";
   if (typeSymbols.find(s) != typeSymbols.end() )  {
      if (DEBUG) cout << "Returning from call: typeDefined(" << s << ")...success on lookup\n";
      return true;
   }
   else  {
      if (DEBUG) cout << "Returning from call: typeDefined(" << s << ")...failure on lookup\n";
      return false;
   }
}

/* String conversions for pushing values to stack  */
void pushIdStack(int val)  {
   if (DEBUG) cout << "Begin call: pushIdStack(" << val << ")...\n";
   ostringstream c;
   c << val;
   idStack.push(c.str());
   if (DEBUG) cout << "Returning from call: pushIdStack(" << val << ")...\n";
}

/* String conversions for pushing values to stack  */
void pushIdStack(char *val)  {
   string s = string(val);
   if (DEBUG) cout << "Begin call: pushIdStack(" << val << ")...\n";
   idStack.push(s);
   if (DEBUG) cout << "Returning from call: pushIdStack(" << val << ")...\n";
}

typeRec* getBaseType(ptnode *p)  {
   
   if (p != NULL)  {
      if (DEBUG) cout << "Begin call: getBaseType...\n";  //(" << p->val << "-.-" << p->sval <<")...\n";
      typeRec *ptr = p->type;
      if (ptr == NULL)  {
         if (DEBUG) cout << "End call: getBaseType...Call was initiated by a NULL pointer!\n";
         return NULL;
      }
      while ( ptr->next != NULL )  {
         ptr = ptr->next;
      }
      if (DEBUG) cout << "Returning from call: getBaseType" /*(" << p->val << "-.-" << p->sval  <<") */ <<"...node type is " << ptr->type << "\n";
      return ptr;
   }  
}

typeRec* getBaseType(string s)  {
   if (DEBUG) cout << "Begin call: getBaseType(" << s << ")...\n";
   if (varSymbols.find(s) != varSymbols.end() )  {
      typeRec *t = varSymbols.find(s)->second;
      while (t->next != NULL)  {
         t = t->next;
      }
      return t;
   }
   if ( SCOPECOUNT )  {
      for (int i = SCOPECOUNT - 1; i >= 0 ; i--)  {
         if (scopeVector[i].find(s) != scopeVector[i].end() )  {
            if (DEBUG) cout << "Returning from call: varDefined(" << s << ")...success on lookup in scope " << SCOPECOUNT << "\n";
            typeRec *t = scopeVector[i].find(s)->second;
            while (t->next != NULL)  {
               t = t->next;
            }
            return t;
         }
      }
   }
   return NULL;
}

void popIdStackToTable(typeRec *baseType, map<string, typeRec*> &table)  {
   if (DEBUG) cout << "\tBegin call: popIdStackToTable(" << baseType->type << ", MAPTYPE)...\n";
   if (DEBUG) cout << "\t--== type pointer has value ==> " << baseType << "\n";
   if (DEBUG) baseType->print();
   baseType->canAssign = true;                   // make all vars created this way assignable
   if ( baseType == NULL )  {
      semanticError(line_num, "Type %s is referenced but not defined", baseType->type);
   }
   int arrayIndex[30] = {0};                 // array of index values
   int i = 0;                                // MANUAL counter
   typeRec *base = new typeRec;              // base type record pointer
   
   while ( !idStack.empty() )  {            // while there are symbols to process
   
      /* if we find an array index... */
      while ( idStack.top() == "]" || idStack.top() == "[" )  {
         
         idStack.pop();                     // remove leading ]
         arrayIndex[i] = atoi(idStack.top().c_str());      // set index value for later use
         idStack.pop();  idStack.pop();    // remove int and trailing [
         i++;                                // increment MANUAL counter
         }
      if (DEBUG) cout << idStack.top() << " was typed as a(n) " << baseType->type ;   // debug printing
      
      if (i > 0) {         // if there are array indexes
         if (DEBUG) cout << "[" << arrayIndex[0] << "]";
         base->type = "array";  base->size = arrayIndex[0];  base->canAssign = true; // set values for "root" record in map
         typeRec *curr = base;          // get pointer to base typerec struct
         typeRec *next;                 // another pointer to move things forward
         for (int j = 1; j < i; j++)  {      // while there are still indexes in the array 
            next = new typeRec;              // create new type record
            if (DEBUG) cout << "[" << arrayIndex[j] << "]";         // debug printing
            next->type = "array"; next->size=arrayIndex[j]; next->next = NULL;  // init values
            next->canAssign = true;                // allow assignment
            curr->next = next;  curr = next;       // insert new record into chain and advance pointers
            arrayIndex[j] = 0;                     // reset array values to zero
         }
         curr->next = baseType;                        // links to base type record in type symbol table
         table[idStack.top()] = base;        // insert symbol info into table (map)
      }
      else  {
         string idval = idStack.top();
         base = baseType;
         table[idStack.top()] = base;        // add symbol and type to table
         }
      i = 0;                  // manually reset counter
      if (DEBUG) cout << endl;           // debug printing
      idStack.pop();         // remove symbol
   }
   if (DEBUG) cout << "Returning from call: popIdStackToTable(" << baseType->type << ", MAPTYPE)...\n";
}  

void printAllSymbols()  {
   
   cout << endl << "Here is the primitive symbol table!\n\n";
   
   for(map<string,typeRec*>::iterator it = varSymbols.begin(); it != varSymbols.end(); ++it) {
      typeRec *curr = it->second;
      
      cout << it->first << "\t"; 
      while (curr->type == "array" )  {
         cout << "[" << curr->size << "]";
         if (curr->next == NULL)
            break;
         curr = curr->next;
         }
      cout << "<-" << curr->type << "-> \n";
   }
   cout << endl << "Here is the primitive type table!\n\n";
   
   for(map<string,typeRec*>::iterator it = typeSymbols.begin(); it != typeSymbols.end(); ++it) {
      typeRec *curr = it->second;
      
      cout << it->first << "\t"; 
      while (curr->type == "array" )  {
         cout << "[" << curr->size << "]";
         if (curr->next == NULL)
            break;
         curr = curr->next;
         }
      cout << "<-" << curr->type << "-> \n";
   }
}

%}

// Defining a C union holding each of the types of tokens that Flex could return, and have Bison
// use that union instead of "int" for the definition of "yystype":
%union {
	char *idval;
	int intval;
	char *sval;
   struct ptnode *nptr;  
   struct typeRec *tptr;
}

// define the "terminal symbol" token types, and associate each with a field of the union:
%token <idval> ID
%token <intval> INT
%token <sval> STRING
%token <sval> TOK_TYPE
%token <sval> TOK_WRITE
%token <sval> TOK_WRITES
%token <sval> TOK_COMMENT
%token <sval> TOK_COLON
%token <sval> TOK_VAR
%token <sval> TOK_ASSIGN
%token <sval> TOK_INT
%token <sval> TOK_EQUALS
%token <sval> TOK_LESS_EQUAL
%token <sval> TOK_GREATER_EQUAL
%token <sval> TOK_NOT_EQUAL
%token <sval> TOK_TRUE
%token <sval> TOK_FALSE
%token <sval> TOK_READ
%token <sval> TOK_IF
%token <sval> TOK_FI
%token <sval> TOK_DO
%token <sval> TOK_OD
%token <sval> TOK_FA
%token <sval> TOK_AF
%token <sval> TOK_TO
%token <sval> TOK_BREAK
%token <sval> TOK_EXIT
%token <sval> TOK_RETURN
%token <sval> TOK_ARROW
%token <sval> TOK_ELSE
%token <sval> TOK_SQUARE
%token <sval> TOK_PROC
%token <sval> TOK_END
%token <sval> TOK_FORWARD
%type <nptr> Factor TypeID VarType _Term Term Stmt Assign_or_Expr Expr MathExp _MathExp 
%type <nptr> _LogicExp ElseIfOrElse Stmts
%type <sval> If

%%
Program:            Head Opt_Stmts          { if (DEBUG) cout << "\t=== Program:  Head Opt_Stmts ===\n"; }
        
Head:               Var Head                { if (DEBUG) cout << "\t=== Head:  Var Head ===\n"; }
                    | Type Head             { if (DEBUG) cout << "\t=== Head:  Type Head ===\n"; }
                    | Forward Head          { if (DEBUG) cout << "\t=== Head:  Forward Head ===\n"; }
                    | Proc Head             { if (DEBUG) cout << "\t=== Head:  Proc Head ===\n"; }
                    |              

Opt_Stmts:          Stmts                   { if (DEBUG) cout << "\t=== Opt_Stmts:  Stmts ===\n"; }
                    |

Stmts:              Stmt Opt_Stmts        { if (DEBUG) cout << "\t=== Stmts: Stmt Opt_Stmts ===\n";
                                            $$ = $1; 
                                          }

Stmt:               If                    { if (DEBUG) cout << "\t=== Stmt:  If ===\n";
                                            ptnode *t = new ptnode();  t->op = string($1); $$ = t; 
                                          }
                    | Do
                    | Fa
                    | TOK_BREAK ';'       { if (LOOPCOUNT <= 0) 
                                             semanticError(line_num, "'%s' outside of a loop not permitted", string($1)); }
                    | TOK_EXIT ';'
                    | TOK_WRITE Expr ';'  { if (getBaseType($2) == getType("bool"))
                                             semanticError(line_num, "\"write\" accepts only base types of \"string\" and \"int\"", "");
                                          }
                    | TOK_WRITES Expr ';' { if (getBaseType($2) == getType("bool"))
                                             semanticError(line_num, "\"writes\" accepts only base types of \"string\" and \"int\"", "");
                                          }
                    | TOK_RETURN ';'
                    | ';'
                    | ID Assign_or_Expr ';'           {  if (DEBUG)  {
                                                            cout << line_num << ": ===> Stmt:  ID Assign_or_Expr --> rule section\n";
                                                            cout << "$2 has val:  " << $2 << endl;
                                                            // if (DEBUG) $2->print();
                                                            if ($2->type != NULL)  {
                                                               typeRec *t = $2->type;
                                                               cout << "\tt has val: " << t << "\n";
                                                               cout << "Type of Assign_or_Expr is " << t->type << endl;
                                                            }
                                                         }
                                                         else  
                                                            if (DEBUG) cout << "Assign_or_Expr currently has no node pointer...\n";
                                                         /* Actual Method */
                                                         if (getBaseType($1) != getBaseType($2))
                                                         semanticError(line_num, "Type mismatch error or expecting assignment.", "");
                                                      }
                    | '(' ExprList ')' Half_Expr ';'
                    | TOK_INT Half_Expr ';'
                    | TOK_TRUE Half_Expr ';'
                    | TOK_FALSE Half_Expr ';'
                    | TOK_READ Half_Expr ';'
                    | STRING ';'
                    | '-' Expr ';'
                    | '?' Expr ';'

Assign_or_Expr:     TOK_ASSIGN Expr                      { $$ = $2 }
                    | '(' ExprList ')' Half_Expr
                    | '[' Expr ']' Half_Expr_or_Assign
                    | Half_Expr

Half_Expr_or_Assign: '[' Expr ']' Half_Expr_or_Assign
                    | TOK_ASSIGN Expr
                    | Half_Expr

Half_Expr:          _Term _MathExp _LogicExp

If:                 TOK_IF Expr TOK_ARROW Stmts ElseIf_or_End TOK_FI { 
                                                   if (DEBUG) cout << "\t=== Now evaluating If statement ===\n";
                                                   if (DEBUG) cout << "Type of Expr is ==> " << getBaseType($2)->type << "\n";
                                                   if (getBaseType($2) != getType("bool"))
                                                      semanticError(line_num, "If statement requires a boolean condition.", "");
                                                   }

ElseIf_or_End:      TOK_SQUARE ElseIfOrElse         { if (DEBUG) cout << "\t=== ElseIf_or_End:  TOK_SQUARE ElseIfOrElse ===\n";  }
                    |

ElseIfOrElse:       TOK_ELSE TOK_ARROW Stmts                { if (DEBUG) cout << "\t=== ElseIfOrElse TOK_ELSE TOK_ARROW Stmts ===\n";
                                                              if (DEBUG) cout << "Leaving === ElseIfOrElse TOK_ELSE TOK_ARROW Stmts ===\n";
                                                            }
                    | Expr TOK_ARROW Stmts ElseIf_or_End    { if (DEBUG) cout << "Type of Expr is ==> " << getBaseType($1)->type << "\n";
                                                                if (getBaseType($1) != getType("bool"))
                                                                    semanticError(line_num, "If statement requires a boolean condition.", "");
                                                            }

Var:                TOK_VAR VarList ';'

VarList:            VarType _VarList
_VarList:           ',' VarType _VarList
                    |

VarType:            IDList TOK_COLON TypeID        {  if (DEBUG) cout << "Attempting call to popIdStackToTable....\n";
                                                      popIdStackToTable($3->type, varSymbols);  }

IDList:             ID _IDList                     { if (varDefined($1)) 
                                                      semanticError(line_num, "Previous definition of %s exists.", string($1));
                                                      pushIdStack($1);
                                                   }

_IDList:            ',' ID _IDList                 { if (varDefined($2)) 
                                                      semanticError(line_num, "Previous definition of %s exists.", string($2));
                                                      pushIdStack($2);
                                                   }
                    |

TypeID:             ID ConstArrayIndex             { ptnode *t = new ptnode();  
                                                      t->type = getType(string($1));  $$ = t; }

ConstArrayIndex:    '[' INT ']' ConstArrayIndex    { pushIdStack("[");  pushIdStack($2); pushIdStack("]");  }
                    |                           

Type:               TOK_TYPE ID {
                                 if (typeDefined($2))
                                    semanticError(line_num, "Previous definition of type %s exists.", string($2));
                                 pushIdStack($2);
                                 } TOK_EQUALS TypeID ';'  {  if (DEBUG) cout << "\t=== Type:  TOK_TYPE ID TOK_EQUALS TypeID ; ===\n";
                                                           popIdStackToTable($5->type, typeSymbols);
                                                       }

Do:                 TOK_DO { LOOPCOUNT++; } Expr TOK_ARROW { SCOPECOUNT++;
                                                               scopeVector.push_back(varSymbols);
                                                               varSymbols.clear();
                                                            } Opt_Stmts TOK_OD   { 
                                                                  
                                          LOOPCOUNT--;
                                          SCOPECOUNT--;           // decrement scope counter
                                          varSymbols.clear();     // clear varSymbols of temp scope vars
                                          varSymbols.insert(scopeVector.back().begin(), scopeVector.back().end()); // restore under scope
                                          if (DEBUG) cout << "Do condition base type is ==> " << getBaseType($3)->type << "\n";
                                          if (getBaseType($3) != getType("bool"))
                                             semanticError(line_num, 
                                                "Do loop test condition must evaluate to type \"bool\"", "");
                                                               }

Fa:                 TOK_FA { LOOPCOUNT++; } ID {
                                    if (DEBUG) cout << "***** Inserting loop var into symbol table...\n";
                                    if (varDefined($3)) 
                                       semanticError(line_num, "Previous definition of %s exists.", string($1));
                              string s = string($3); typeRec *t = getType("int"); t->canAssign = false;
                              varSymbols[s] = t; if (DEBUG) cout << "***** Success!\n"; 
                                 } TOK_ASSIGN Expr TOK_TO Expr TOK_ARROW { SCOPECOUNT++;
                                                               scopeVector.push_back(varSymbols);
                                                               varSymbols.clear();
                                                                         } Opt_Stmts TOK_AF   { 
                                       LOOPCOUNT--;
                                       SCOPECOUNT--;           // decrement scope counter
                                       varSymbols.clear();     // clear varSymbols of temp scope vars
                                       varSymbols.insert(scopeVector.back().begin(), scopeVector.back().end()); // restore under scope                                       
                                       if (getBaseType($6) != getType("int") || getBaseType($8) != getType("int"))
                                          semanticError(line_num, "Range expressions in a \"for all\" loop must evaluate to integers", "");
                                                                                                            }

Proc:               TOK_PROC ID '(' DecList ')' ReturnType ProcBody TOK_END

ReturnType:         TOK_COLON ID
                    |

ProcBody:           BodyHead Opt_Stmts

BodyHead:           Type BodyHead
                    | Var BodyHead
                    |

Forward:            TOK_FORWARD ID '(' DecList ')' ReturnType ';'

DecList:            DecType _DecList
                    |

_DecList:           ',' DecType _DecList
                    |

DecType:            IDList TOK_COLON ID

Expr:               MathExp _LogicExp     { if (DEBUG) cout << "\t=== Expr:  MathExp _LogicExp\n";
                                             if (DEBUG) cout << "\t\tTYPE OF _LOGICEXP ==> " << $2->type << "\n";
                                             if (DEBUG) cout << "\t\tTYPE OF MATHEXP   ==> " << $1->type << "\n";
                                             if ($2->op != "cmp")  {
                                                $$ = $1;
                                            }
                                            else  {
                                                if ( getBaseType($1) != getBaseType($2))
                                                   semanticError(line_num, "Comparison on invalid types.", "");
                                                if ( getBaseType($1) == getType("string") || getBaseType($1) == getType("string"))
                                                   semanticError(line_num, "Comparison of string literals", "");
                                                ptnode *t = new ptnode();  t->type = getType("bool");
                                                $$ = t;
                                            }
                                          }

_LogicExp:          '<' MathExp                 {  if (DEBUG) cout << line_num << ":  _LogicExp: TOK_EQUALS MathExp\n";
                                                   if (getBaseType($2) != getType("int"))
                                                      semanticError(line_num, "Attempted relational comparison on non-integer base type.", "");
                                                   ptnode *t = new ptnode();  t->type = getBaseType($2);
                                                   t->op = "cmp";  $$ = t;
                                                }
                    | '>' MathExp               {  if (DEBUG) cout << line_num << ":  _LogicExp: TOK_EQUALS MathExp\n";
                                                   if (getBaseType($2) != getType("int"))
                                                      semanticError(line_num, "Attempted relational comparison on non-integer base type.", "");
                                                   ptnode *t = new ptnode();  t->type = getBaseType($2);
                                                   t->op = "cmp";  $$ = t;
                                                }
                    | TOK_LESS_EQUAL MathExp    {  if (DEBUG) cout << line_num << ":  _LogicExp: TOK_EQUALS MathExp\n";
                                                   if (getBaseType($2) != getType("int"))
                                                      semanticError(line_num, "Attempted relational comparison on non-integer base type.", "");
                                                   ptnode *t = new ptnode();  t->type = getBaseType($2);
                                                   t->op = "cmp";  $$ = t;
                                                }
                    | TOK_GREATER_EQUAL MathExp {  if (DEBUG) cout << line_num << ":  _LogicExp: TOK_EQUALS MathExp\n";
                                                   if (getBaseType($2) != getType("int"))
                                                      semanticError(line_num, "Attempted relational comparison on non-integer base type.", "");
                                                   ptnode *t = new ptnode();  t->type = getBaseType($2);
                                                   t->op = "cmp";  $$ = t;
                                                }
                    | TOK_NOT_EQUAL MathExp     {  if (DEBUG) cout << line_num << ":  _LogicExp: TOK_NOT_EQUALS MathExp\n";
                                                   ptnode *t = new ptnode();  t->type = getBaseType($2);
                                                   t->op = "cmp";  $$ = t;
                                                }
                    | TOK_EQUALS MathExp        {  if (DEBUG) cout << line_num << ":  _LogicExp: TOK_EQUALS MathExp\n";
                                                   ptnode *t = new ptnode();  t->type = getBaseType($2);
                                                   t->op = "cmp";  $$ = t;
                                                }
                    |

MathExp:            Term _MathExp         { if (DEBUG) cout << line_num << ": ===> MathExp:  Term _MathExp  --> rule section\n";
                                             if (DEBUG) $2->print();
                                             if ($2->type != NULL)  {
                                                if  (getBaseType($2) == getType("string"))
                                                   semanticError(line_num, "Attempted an arithmetic operation with a string", "");
                                                if (DEBUG) cout << "\tTerm ==> " << $1; 
                                                if (DEBUG) cout << " -_- _MathExp ==> " << $2 << endl;
                                                if (getBaseType($1) != getBaseType($2))
                                                   semanticError(line_num, "Type mismatch error.", "");
                                            }
                                            else 
                                             $$ = $1;
                                          }

_MathExp:           '+' Term _MathExp     { if (DEBUG) cout << "\t=== Evaluating _MathExp plus rules ===\n";
                                                $$ = $2; 
                                          }
                    | '-' Term _MathExp   { if (DEBUG) cout << "\t=== Evaluating _MathExp minus rules ===\n";
                                             if ($3->type == NULL)  {
                                                $$ = $2; 
                                             }
                                             else if (getBaseType($$) != getType("int") &&
                                                   getBaseType($2) != getType("int"))  { 
                                                semanticError(line_num, "Type mismatch error near \"-\".", "");
                                             }
                                             else  {
                                                $$ = $2;
                                             }
                                          }
                    |                     { ptnode *t = new ptnode();  t->type = NULL;  $$ = t; }
                                          

Term:               Factor _Term          { if (DEBUG) cout << line_num << ": ===> Term:  Factor _Term  --> rule section\n";
                                             //if (DEBUG) $2->print();
                                             if ($2->type != NULL)  {
                                             if (DEBUG) cout << "\tFactor ==> " << $1; 
                                             if (DEBUG) cout << " -_- Term ==> " << $2 << endl;
                                             if (getBaseType($1) != getBaseType($2))
                                                semanticError(line_num, "Type mismatch error.", "");
                                            }
                                            else 
                                             $$ = $1;
                                          }

_Term:              '*' Factor _Term      { if (DEBUG) cout << "\t=== Type check of mult operator... ===\n";
                                             if ($3->type != NULL)  {
                                                if (getBaseType($2) != getBaseType($3))
                                                   semanticError(line_num, "Type mismatch error near \"*\" operator.", "");
                                                $$ = $2;
                                             }
                                             else  {
                                             if ( getBaseType($2) != getType("int") && getBaseType($2) != getType("bool"))
                                                semanticError(line_num, "Type mismatch error near \"*\" operator.", "");
                                             ptnode *t = new ptnode();  t->type = getBaseType($2);  $$ = t;
                                             }
                                          }
                    | '/' Factor _Term    { if (DEBUG) cout << "\t=== Type check of divide operator... ===\n";                                             
                                             if ( getBaseType($2) != getType("int"))
                                                semanticError(line_num, "Type mismatch error near \"/\" operator.", "");
                                             ptnode *t = new ptnode();  t->type = getBaseType($2);  $$ = t;
                                          }
                    | '%' Factor _Term    { if (DEBUG) cout << "\t=== Type check of mod operator... ===\n";
                                             if ( getBaseType($2) != getType("int"))
                                                semanticError(line_num, "Type mismatch error near \"%\" operator.", "");
                                             ptnode *t = new ptnode();  t->type = getBaseType($2);  $$ = t;
                                          }
                    |                     { ptnode *t = new ptnode();  t->type = NULL;  $$ = t; }

Factor:             '-' Factor         { if (DEBUG) cout << "\t=== Type check of unary minus... ===\n";
                                          if ( getBaseType($2) != getType("int") && getBaseType($2) != getType("bool"))
                                          semanticError(line_num, "Unary minus applied to value with type %s", ($2->type)->type);
                                          $$ = $2;
                                       }
                    | '?' Factor       { if (DEBUG) cout << "\t=== Type check conversion of bool to int... ===\n";
                                          if ( getBaseType($2) != getType("bool"))
                                             semanticError(line_num, "Attempted to convert non-boolean to integer", "");
                                          ptnode *t = new ptnode();  t->type = getType("int");
                                          if ($2->sval == "true") t->val = 1;
                                          else t->val = 0;
                                          $$ = t;
                                       }
                    | '(' Expr ')'     { if (DEBUG) cout << "\t=== Moving expression out of parenthesis ===\n";
                                          $$ = $2;
                                       }
                    | INT              { ptnode *t = new ptnode();                    
                                          t->type = getType("int");  
                                          t->val = $1;  t->sval = "";
                                          $$ = t;  }
                    | STRING           { ptnode *t = new ptnode();  t->type = getType("string");
                                          t->val = -1; t->sval = $1;  $$ = t;  }
                    | ID OptSuffix     { if (DEBUG) cout << line_num << ": ===> Factor:  ID  --> rule section\n";
                                          if ( !varDefined($1, true) ) 
                                          semanticError(line_num, "Use of variable %s attempted with no declaration.", string($1));
                                          ptnode *t = new ptnode();  t->type = getSymbolType($1); $$ = t;
                                          if (DEBUG) cout << "$$ assigned pointer pointing to ==> " << t << endl;
                                       }
                    | TOK_TRUE         { ptnode *t = new ptnode();  t->type = getType("bool");
                                          t->val = -1; t->sval = $1;  $$ = t;  }
                    | TOK_FALSE        { ptnode *t = new ptnode();  t->type = getType("bool");
                                          t->val = -1; t->sval = $1;  $$ = t;  }
                    | TOK_READ         { ptnode *t = new ptnode();  t->type = getType("int");
                                          t->val = -1; t->sval = $1;  $$ = t;  }

OptSuffix:          '(' ExprList ')'         { if (DEBUG) cout << "\t=== OptSuffix:  \'(\' ExprList \')\' ===\n"; }
                    | ArrayIndex             { if (DEBUG) cout << "\t=== OptSuffix:  ArrayIndex ===\n"; }
                   
ArrayIndex:         '[' Expr ']' ArrayIndex  { if (DEBUG) cout << "\t=== ArrayIndex:  \'[\' Expr \']\' ArrayIndex ===\n"; }
                    |

ExprList:           Expr _ExprList           { if (DEBUG) cout << "\t=== ExprList:  Expr _ExprList ===\n"; }
                    |

_ExprList:          ',' Expr _ExprList       { if (DEBUG) cout << "\t=== _ExprList:  \',\' Expr _ExprList ===\n"; }
                    |

%%

main(int argc, char* argv[])  {
    
    if (argc > 1) DEBUG = atoi(argv[1]);
    initTypeTable();
//    yydebug = 1;
//    FILE *infile = fopen(argv[1], "r");
//    if (!infile)  {
//        cout << "Input file not found.\n";
//        return -1;
//    }
    
	// set flex to read from it instead of defaulting to STDIN:
//	yyin = infile;
	
	// parse through the input until there is no more:
	do {
		yyparse();
	} while (!feof(yyin));
	
   if (DEBUG) printAllSymbols();
}

void semanticError(int line_num, string error, string var)  {

   cout << "Error near line " << line_num << ":  ";
   printf(error.c_str(), var.c_str());
   cout << "\n\n";
   exit(0);
}

/*
void yyerror(const char *s) {
	printf("Line %d: %s near %s\n", line_num, s, yytext);
	// stop on error
	exit(-1);
}
*/

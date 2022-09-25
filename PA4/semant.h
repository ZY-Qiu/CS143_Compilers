#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <map>
#include <list>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

typedef SymbolTable<Symbol, Symbol> AttrTable;
typedef SymbolTable<Symbol, method_class> MethodTable;


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassName {
public:
  // each class has an attrTable and a methodTable
  
  AttrTable *attrTable;
  MethodTable *methodTable;

  // class instance
  Class_ cls;

  ClassName(): attrTable(new AttrTable()), methodTable(new MethodTable()), cls(NULL) {}
  ClassName(Class_ c): attrTable(new AttrTable()), methodTable(new MethodTable()), cls(c) {}
};

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  // self defined features here
  std::map<Symbol, ClassName> className;
  std::list<Symbol> getInheritancePath(Symbol type);
  void addAttrTable(Symbol name, AttrTable *curr_at);
  bool checkInheritance(Symbol child, Symbol parent);
  Symbol LUB(Symbol c1, Symbol c2);
};


#endif


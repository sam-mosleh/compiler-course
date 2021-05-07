#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable
{
private:
  int semant_errors;
  void install_basic_classes();
  void check_parent_exists();
  void check_parent_is_not_primitive_type();
  void check_cyclic_inheritance();
  void check_main_class_exists();
  ostream &error_stream;
  std::map<Symbol, Class_> class_map;
  std::map<Symbol, std::map<Symbol, attr_class *> > class_attributes;
  std::map<Symbol, std::map<Symbol, method_class *> > class_methods;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  ostream &semant_error(Symbol filename, tree_node *t);

  void fill_and_check_for_redefinition(Classes classes);
  void fill_and_check_feature_redefinition(Class_ cls);
  bool is_subclass(Symbol from, Symbol to);
  bool not_conforming(Symbol self_class, Symbol child, Symbol parent);
};

#endif

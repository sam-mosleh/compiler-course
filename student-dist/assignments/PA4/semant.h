#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <set>
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
  void fill_and_check_for_redefinition(Classes classes);
  void fill_and_check_feature_redefinition(Class_ cls);
  void check_parent_exists();
  void check_parent_is_not_primitive_type();
  void check_cyclic_inheritance();
  void check_main_class_exists();
  ostream &error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  ostream &semant_error(Symbol filename, tree_node *t);

  std::map<Symbol, Class_> class_map;
  std::map<Symbol, std::map<Symbol, attr_class *> > class_attributes;
  std::map<Symbol, std::map<Symbol, method_class *> > class_methods;

  bool is_subclass(Symbol from, Symbol to);
  bool not_conforming(Symbol self_class, Symbol child, Symbol parent);
  Symbol closest_parent(Symbol self_class, Symbol cls_a, Symbol cls_b);
  bool attribute_exists(Symbol attribute_name, Symbol class_name);
  method_class *get_method(Symbol method_name, Symbol class_name);
  bool has_no(Symbol class_name);
};

class Environment
{
public:
  // Current Filename
  Symbol filename;
  // Current Class Name
  Symbol classname;
  // Current Symbol Table
  SymbolTable<Symbol, Entry> symbol_table;

  Environment(Symbol);
  void add_all_attributes_to_symbol_table_from_class(Symbol class_name);
};

#endif

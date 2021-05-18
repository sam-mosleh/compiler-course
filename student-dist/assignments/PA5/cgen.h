#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

std::map<Symbol, int> tag;
std::map<Symbol, int> last_child_tag;
std::map<Symbol, std::vector<std::string> > class_method_names;
std::map<Symbol, std::map<std::string, int> > class_method_offset;
std::map<Symbol, std::map<Symbol, int> > class_attribute_offset;


enum Basicness
{
   Basic,
   NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
private:
   CgenNode *root_object;
   List<CgenNode> *nds;
   ostream &str;

   std::vector<CgenNode *> nodes;
   std::map<Symbol, int> size;

   // The following methods emit code for
   // constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_nametab();
   void code_objtab();
   void code_disptab();
   void code_protobj();
   void code_attributes_with_default_value(CgenNode *node);
   void code_object_initializer();
   void code_class_method();

   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   int set_class_tags_and_sizes(CgenNode *current, int class_tag, int class_size);
   void set_class_offsets();
   int set_class_method_offset(CgenNode *current, Symbol class_name);
   int set_class_attribute_offset(CgenNode *current, Symbol class_name);

public:
   CgenClassTable(Classes, ostream &str);
   void code();
   CgenNodeP root();
};

class CgenNode : public class__class
{
private:
   CgenNodeP parentnd;       // Parent of class
   List<CgenNode> *children; // Children of class
   Basicness basic_status;   // `Basic' if class is basic
                             // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst
{
private:
   int val;

public:
   BoolConst(int);
   void code_def(ostream &, int boolclasstag);
   void code_ref(ostream &) const;
};

class Environment
{
public:
   // Current Class Name
   Symbol class_name;
   // Current File Name
   StringEntry *file_name;
   // [Argument Name, Position]
   std::map<Symbol, int> method_arguments;
   // [Attribute Name]
   std::vector<Symbol> variables;

   Environment(CgenNode *);
   void add_method_arguments_of(method_class *);
   int get_variable_offset(Symbol);
};

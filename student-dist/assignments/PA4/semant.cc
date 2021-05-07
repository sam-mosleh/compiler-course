

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

ClassTable *classtable;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

bool ClassTable::is_subclass(Symbol from, Symbol to)
{
    if (class_map.find(from) == class_map.end())
        return false;
    Class_ cls = class_map[from];

    Symbol parent = cls->get_parent();
    if (parent == to)
        return true;
    else
        return is_subclass(parent, to);
}

bool ClassTable::not_conforming(Symbol self_class, Symbol child, Symbol parent)
{
    if (child == SELF_TYPE)
        child = self_class;
    if (parent == SELF_TYPE)
        parent = self_class;
    return child != parent && !is_subclass(child, parent);
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
    install_basic_classes();

    fill_and_check_for_redefinition(classes);

    check_parent_exists();

    check_parent_is_not_primitive_type();

    check_cyclic_inheritance();

    check_main_class_exists();
}

void ClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object,
               No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);
    class_map[Object] = Object_class;

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
        class_(IO,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                  SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                  SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               filename);
    class_map[IO] = IO_class;

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int,
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               filename);
    class_map[Int] = Int_class;

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);
    class_map[Bool] = Bool_class;

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
        class_(Str,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           append_Features(
                               single_Features(attr(val, Int, no_expr())),
                               single_Features(attr(str_field, prim_slot, no_expr()))),
                           single_Features(method(length, nil_Formals(), Int, no_expr()))),
                       single_Features(method(concat,
                                              single_Formals(formal(arg, Str)),
                                              Str,
                                              no_expr()))),
                   single_Features(method(substr,
                                          append_Formals(single_Formals(formal(arg, Int)),
                                                         single_Formals(formal(arg2, Int))),
                                          Str,
                                          no_expr()))),
               filename);
    class_map[Str] = Str_class;
}

void ClassTable::fill_and_check_for_redefinition(Classes classes)
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        Class_ cls = classes->nth(i);
        Symbol name = cls->get_name();

        if (name == Object || name == IO || name == Int || name == Bool || name == Str || name == SELF_TYPE)
        {
            semant_error(cls) << "Redefinition of basic class SELF_TYPE." << std::endl;
        }
        else if (class_map.find(name) != class_map.end())
        {
            semant_error(cls) << "Class " << name << " was previously defined." << std::endl;
        }
        else
        {
            class_map[name] = cls;
            fill_and_check_feature_redefinition(cls);
        }
    }
}

void ClassTable::fill_and_check_feature_redefinition(Class_ cls)
{
    Symbol class_name = cls->get_name();
    Features class_features = cls->get_features();

    for (int i = class_features->first(); class_features->more(i); i = class_features->next(i))
    {
        Feature feature = class_features->nth(i);
        // cout<<classname<< ":" << class_features->nth(i)->get_name()<<endl;
        if (attr_class *attr = dynamic_cast<attr_class *>(feature))
        {
            // Its an attribute
            Symbol attr_name = attr->get_name();
            if (class_attributes[class_name].find(attr_name) != class_attributes[class_name].end())
            {
                semant_error(cls) << "Attribute " << attr_name << " is multiply defined in class " << class_name << std::endl;
            }
            else
            {
                class_attributes[class_name][attr_name] = attr;
            }
        }
        else if (method_class *method = dynamic_cast<method_class *>(feature))
        {
            // Its a method
            Symbol method_name = method->get_name();
            if (class_methods[class_name].find(method_name) != class_methods[class_name].end())
            {
                semant_error(cls) << "Method " << method_name << " is multiply defined in class " << class_name << std::endl;
            }
            else
            {
                class_methods[class_name][method_name] = method;
            }
        }
    }
}

void ClassTable::check_parent_exists()
{
    std::map<Symbol, Class_>::iterator it;
    for (it = class_map.begin(); it != class_map.end(); ++it)
    {
        Symbol class_name = it->first;
        Class_ cls = it->second;
        Symbol parent = cls->get_parent();
        if (class_name != Object && class_map.find(parent) == class_map.end())
        {
            semant_error(cls) << "Class "
                              << class_name
                              << " inherits from an undefined class "
                              << parent
                              << std::endl;
        }
    }
}

void ClassTable::check_parent_is_not_primitive_type()
{
    std::map<Symbol, Class_>::iterator it;
    for (it = class_map.begin(); it != class_map.end(); ++it)
    {
        Symbol class_name = it->first;
        Class_ cls = it->second;
        Symbol parent = cls->get_parent();
        if (parent == Int || parent == Bool || parent == Str)
        {
            semant_error(cls) << "Class "
                              << class_name
                              << " cannot inherit primitive type "
                              << parent
                              << std::endl;
        }
    }
}

void ClassTable::check_cyclic_inheritance()
{
    std::map<Symbol, Class_>::iterator it;
    for (it = class_map.begin(); it != class_map.end(); ++it)
    {
        Symbol class_name = it->first;
        Class_ cls = it->second;
        if (is_subclass(class_name, class_name))
        {
            semant_error(cls) << "Class "
                              << class_name
                              << ", or an ancestor of "
                              << class_name
                              << ", is involved in an inheritance cycle"
                              << std::endl;
        }
    }
}

void ClassTable::check_main_class_exists()
{
    if (class_map.find(Main) == class_map.end())
    {
        semant_error() << "Class Main is not defined." << std::endl;
    }
    else
    {
        Class_ main_class = class_map[Main];
        if (class_methods[Main].find(main_meth) == class_methods[Main].end())
        {
            semant_error(main_class) << "No 'main' method in class Main" << std::endl;
        }
    }
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    check();

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void program_class::check()
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        classes->nth(i)->check();
    }
}

void class__class::check()
{
    SymbolTable<Symbol, Entry> symbol_table;
    symbol_table.enterscope();

    // Environment env(name);

    symbol_table.addid(self, SELF_TYPE);

    /*for (std::map<Symbol, attr_class *>::iterator it = env.parent_attributes.begin();
         it != env.parent_attributes.end();
         it++)
        symbol_table->addid(it->first, it->second->get_type());*/

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        cout << "Handling FEATURE " << (features->nth(i)->get_name()) << endl;
        // features->nth(i)->check(env);
    }

    symbol_table.exitscope();
}
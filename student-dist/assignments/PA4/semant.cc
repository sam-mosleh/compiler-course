

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

Environment::Environment(Symbol classname)
{
    Class_ cls = classtable->class_map[classname];
    this->filename = cls->get_filename();
    this->classname = classname;
}

void Environment::add_all_attributes_to_symbol_table_from_class(Symbol class_name)
{
    if (class_name != Object)
    {
        Symbol parent_name = classtable->class_map[class_name]->get_parent();
        add_all_attributes_to_symbol_table_from_class(parent_name);
    }

    for (std::map<Symbol, attr_class *>::iterator it = classtable->class_attributes[class_name].begin();
         it != classtable->class_attributes[class_name].end();
         it++)
        symbol_table.addid(it->first, it->second->get_type());
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

bool is_primitive(Symbol class_name)
{
    return class_name == Int || class_name == Bool || class_name == Str;
}

bool is_basic_class(Symbol class_name)
{
    return class_name == Object || class_name == IO || class_name == SELF_TYPE || is_primitive(class_name);
}

void ClassTable::fill_and_check_for_redefinition(Classes classes)
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        Class_ cls = classes->nth(i);
        Symbol name = cls->get_name();

        if (is_basic_class(name))
        {
            semant_error(cls) << "Redefinition of basic class " << name << std::endl;
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
        if (is_primitive(parent))
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
        else
        {
            Formals formals = class_methods[Main][main_meth]->get_formals();
            if (formals->len())
            {
                semant_error(main_class) << "'main' method in class Main should have no arguments" << std::endl;
            }
        }
    }
}

bool ClassTable::attribute_exists(Symbol attribute_name, Symbol class_name)
{
    if (class_attributes[class_name].find(attribute_name) != class_attributes[class_name].end())
        return true;
    if (class_name != Object)
    {
        Symbol parent_name = classtable->class_map[class_name]->get_parent();
        if (attribute_exists(attribute_name, parent_name))
            return true;
    }
    return false;
}

method_class *ClassTable::get_method(Symbol method_name, Symbol class_name)
{
    if (class_methods[class_name].find(method_name) != class_methods[class_name].end())
        return class_methods[class_name][method_name];
    if (class_name != Object)
    {
        Symbol parent_name = classtable->class_map[class_name]->get_parent();
        if (method_class *result = get_method(method_name, parent_name))
            return result;
    }
    return NULL;
}

Symbol ClassTable::closest_parent(Symbol self_class, Symbol cls_a, Symbol cls_b)
{
    Symbol target = cls_a;
    if (target == SELF_TYPE)
        target = self_class;
    while (not_conforming(self_class, cls_b, target))
    {
        target = class_map[target]->get_parent();
    }
    return target;
}

bool ClassTable::has_no(Symbol class_name)
{
    return class_map.find(class_name) == class_map.end();
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

    classtable = new ClassTable(classes);

    if (!classtable->errors())
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
    Environment env(name);
    env.symbol_table.enterscope();

    env.add_all_attributes_to_symbol_table_from_class(parent);
    env.symbol_table.addid(self, SELF_TYPE);

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        // cout << "FEATURE " << (features->nth(i)->get_name()) << endl;
        features->nth(i)->check(env);
    }

    env.symbol_table.exitscope();
}

void method_class::check(Environment &env)
{
    Class_ cls = classtable->class_map[env.classname];
    Symbol type = return_type;
    if (type == SELF_TYPE)
        type = env.classname;

    if (classtable->has_no(type))
    {
        classtable->semant_error(env.filename, this) << "Undefined return type "
                                                     << type
                                                     << " in Method "
                                                     << name
                                                     << std::endl;
    }

    // Get derived method if any
    if (method_class *parent_method = classtable->get_method(name, cls->get_parent()))
    {
        Formals parent_formals = parent_method->get_formals();

        if (parent_formals->len() != formals->len())
        {
            classtable->semant_error(env.filename, this) << "Incompatible number of formal parameters in redefined method "
                                                         << name
                                                         << std::endl;
            return;
        }

        for (int i = formals->first(); formals->more(i); i = formals->next(i))
        {
            Formal f = formals->nth(i), parent_f = parent_formals->nth(i);
            if (f->get_type() != parent_f->get_type())
            {
                classtable->semant_error(env.filename, f) << "In redefined method "
                                                          << name
                                                          << ", parameter type "
                                                          << f->get_type()
                                                          << " is different from original type "
                                                          << parent_f->get_type()
                                                          << std::endl;
            }
        }

        if (parent_method->get_type() != return_type)
        {
            classtable->semant_error(env.filename, this) << "In redefined method "
                                                         << name
                                                         << ", return type "
                                                         << return_type
                                                         << " is different from original return type "
                                                         << parent_method->get_type()
                                                         << std::endl;
        }
    }

    env.symbol_table.enterscope();

    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        formals->nth(i)->check(env);
    }

    expr->check(env);

    env.symbol_table.exitscope();

    Symbol expr_type = expr->get_type();
    if (classtable->not_conforming(env.classname, expr_type, return_type))
    {
        classtable->semant_error(env.filename, this) << "Inferred return type "
                                                     << expr_type
                                                     << " of method "
                                                     << name
                                                     << " does not conform to declared return type "
                                                     << return_type
                                                     << std::endl;
    }
}

void attr_class::check(Environment &env)
{
    Class_ cls = classtable->class_map[env.classname];
    if (name == self)
    {
        classtable->semant_error(env.filename, this) << "'self' cannot be the name of an attribute."
                                                     << std::endl;
    }

    if (classtable->has_no(type_decl))
    {
        classtable->semant_error(env.filename, this) << "Class "
                                                     << type_decl
                                                     << " of Attribute "
                                                     << name
                                                     << " is undefined."
                                                     << std::endl;
    }
    if (classtable->attribute_exists(name, cls->get_parent()))
    {
        classtable->semant_error(env.filename, this) << "Attribute "
                                                     << name
                                                     << " is an attribute of an inherited class."
                                                     << std::endl;
    }

    init->check(env);

    Symbol init_type = init->get_type();
    if (init_type != No_type && classtable->not_conforming(env.classname, init_type, type_decl))
    {
        classtable->semant_error(env.filename, this) << "Inferred type "
                                                     << init_type
                                                     << " of initialization of attribute "
                                                     << name
                                                     << "does not conform to declared type"
                                                     << type_decl
                                                     << std::endl;
    }
}

void formal_class::check(Environment &env)
{
    if (env.symbol_table.probe(name) != NULL)
    {
        classtable->semant_error(env.filename, this) << "Formal parameter "
                                                     << name
                                                     << " is multiply defined."
                                                     << std::endl;
    }
    if (type_decl == SELF_TYPE)
    {
        classtable->semant_error(env.filename, this) << "Formal parameter "
                                                     << name
                                                     << "cannot have type SELF_TYPE."
                                                     << std::endl;
    }
    if (classtable->has_no(type_decl))
    {
        classtable->semant_error(env.filename, this) << "Class "
                                                     << type_decl
                                                     << " of Formal "
                                                     << name
                                                     << " is undefined."
                                                     << std::endl;
    }
    if (name == self)
    {
        classtable->semant_error(env.filename, this) << "'self' cannot be the name of a formal parameter."
                                                     << std::endl;
    }
    env.symbol_table.addid(name, type_decl);
}

void static_dispatch_class::check(Environment &env)
{
    if (classtable->has_no(type_name))
    {
        classtable->semant_error(env.filename, this) << "Static dispatch to undefined class "
                                                     << type_name
                                                     << std::endl;
        type = Object;
        return;
    }

    expr->check(env);

    Symbol expr_type = expr->get_type();

    if (classtable->not_conforming(env.classname, expr_type, type_name))
    {
        classtable->semant_error(env.filename, this) << "Expression type "
                                                     << expr_type
                                                     << " does not conform to declared static dispatch type "
                                                     << type_name
                                                     << std::endl;
    }

    if (classtable->class_methods[type_name].find(name) == classtable->class_methods[type_name].end())
    {
        classtable->semant_error(env.filename, this) << "Dispatch to undefined method "
                                                     << name
                                                     << std::endl;
        type = Object;
        return;
    }
    Formals formals = classtable->class_methods[type_name][name]->get_formals();
    Symbol func_ret_type = classtable->class_methods[type_name][name]->get_type();
    if (func_ret_type == SELF_TYPE)
        func_ret_type = expr->get_type();

    if (actual->len() != formals->len())
    {
        classtable->semant_error(env.filename, this) << "Method "
                                                     << name
                                                     << " called with wrong number of arguments."
                                                     << std::endl;
    }
    else
    {
        for (int i = actual->first(); actual->more(i); i = actual->next(i))
        {
            Expression act = actual->nth(i);
            act->check(env);
            Symbol formal_type = formals->nth(i)->get_type(),
                   actual_type = act->get_type();
            if (classtable->not_conforming(env.classname, actual_type, formal_type))
            {
                classtable->semant_error(env.filename, this) << "In call of method "
                                                             << name
                                                             << ", type "
                                                             << actual_type
                                                             << " of parameter "
                                                             << formals->nth(i)->get_name()
                                                             << " does not conform to declared type "
                                                             << formal_type
                                                             << std::endl;
            }
        }
    }

    type = func_ret_type;
}

void dispatch_class::check(Environment &env)
{
    expr->check(env);

    Symbol expr_type = expr->get_type();

    if (expr_type == SELF_TYPE)
        expr_type = env.classname;

    method_class *method = classtable->get_method(name, expr_type);
    if (method == NULL)
    {
        classtable->semant_error(env.filename, this) << "Dispatch to undefined method "
                                                     << name
                                                     << std::endl;
        type = Object;
        return;
    }

    Formals formals = method->get_formals();
    Symbol func_ret_type = method->get_type();
    if (func_ret_type == SELF_TYPE)
        func_ret_type = expr->get_type();

    if (actual->len() != formals->len())
    {
        classtable->semant_error(env.filename, this) << "Method "
                                                     << name
                                                     << " called with wrong number of arguments"
                                                     << std::endl;
    }
    else
    {
        for (int i = actual->first(); actual->more(i); i = actual->next(i))
        {
            Expression act = actual->nth(i);
            act->check(env);
            Symbol formal_type = formals->nth(i)->get_type(),
                   actual_type = act->get_type();
            if (classtable->not_conforming(env.classname, actual_type, formal_type))
            {
                classtable->semant_error(env.filename, this) << "In call of method "
                                                             << name
                                                             << ", type "
                                                             << actual_type
                                                             << " of parameter "
                                                             << formals->nth(i)->get_name()
                                                             << " does not conform to declared type "
                                                             << formal_type
                                                             << std::endl;
            }
        }
    }

    type = func_ret_type;
}

void assign_class::check(Environment &env)
{
    if (name == self)
    {
        classtable->semant_error(env.filename, this) << "Cannot assign to 'self'"
                                                     << std::endl;
        type = Object;
        return;
    }

    Symbol var_type = env.symbol_table.lookup(name);
    if (var_type == NULL)
    {
        classtable->semant_error(env.filename, this) << "Assignment to undeclared variable "
                                                     << name
                                                     << std::endl;
        type = Object;
        return;
    }

    expr->check(env);
    Symbol expr_type = expr->get_type();

    if (classtable->not_conforming(env.classname, expr_type, var_type))
    {
        classtable->semant_error(env.filename, this) << "Type "
                                                     << expr_type
                                                     << " of assigned expression does not conform to declared type "
                                                     << var_type
                                                     << " of identifier "
                                                     << name
                                                     << std::endl;
    }

    type = expr_type;
}

void cond_class::check(Environment &env)
{
    pred->check(env);
    then_exp->check(env);
    else_exp->check(env);

    if (pred->get_type() != Bool)
    {
        classtable->semant_error(env.filename, this) << "Predicate of 'if' does not have type Bool"
                                                     << std::endl;
    }
    type = classtable->closest_parent(env.classname, then_exp->get_type(), else_exp->get_type());
}

void loop_class::check(Environment &env)
{
    pred->check(env);
    if (pred->get_type() != Bool)
    {
        classtable->semant_error(env.filename, this) << "Loop condition does not have type Bool"
                                                     << std::endl;
    }
    body->check(env);
    type = Object;
}

Symbol branch_class::check(Environment &env)
{
    env.symbol_table.enterscope();
    env.symbol_table.addid(name, type_decl);

    expr->check(env);

    env.symbol_table.exitscope();
    return expr->get_type();
}

void typcase_class::check(Environment &env)
{
    expr->check(env);
    Symbol expr_type = expr->get_type();

    std::set<Symbol> case_type_set;

    type = NULL;

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        Case c = cases->nth(i);
        Symbol case_type = c->get_type();
        if (case_type_set.find(case_type) != case_type_set.end())
        {
            classtable->semant_error(env.filename, this) << "Duplicate branch "
                                                         << case_type
                                                         << " in case statement"
                                                         << std::endl;
            type = Object;
            return;
        }
        case_type_set.insert(case_type);

        Symbol branch_expr_type = c->check(env);

        if (type == NULL)
        {
            type = branch_expr_type;
        }
        else
        {
            type = classtable->closest_parent(env.classname, type, branch_expr_type);
        }
    }
}

void block_class::check(Environment &env)
{
    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        Expression expr = body->nth(i);
        expr->check(env);
        type = expr->get_type();
    }
}

void let_class::check(Environment &env)
{
    init->check(env);
    Symbol init_type = init->get_type();
    if (init_type != No_type && classtable->not_conforming(env.classname, init_type, type_decl))
    {
        classtable->semant_error(env.filename, this) << "Inferred type "
                                                     << init_type
                                                     << " of initialization of "
                                                     << identifier
                                                     << " does not conform to identifier's declared type"
                                                     << type_decl
                                                     << std::endl;
    }
    env.symbol_table.enterscope();

    if (identifier != self)
    {
        env.symbol_table.addid(identifier, type_decl);
    }
    else
    {
        classtable->semant_error(env.filename, this) << "'self' cannot be bound in a 'let' expression"
                                                     << std::endl;
    }

    body->check(env);
    type = body->get_type();

    env.symbol_table.exitscope();
}

void Expression_class::check_argument_is_int(Environment &env, Expression e1, Expression e2, std::string op)
{
    e1->check(env);
    e2->check(env);
    Symbol t1 = e1->get_type(),
           t2 = e2->get_type();
    if (t1 != Int || t2 != Int)
    {
        classtable->semant_error(env.filename, this) << "non-Int arguments: "
                                                     << t1
                                                     << " "
                                                     << op
                                                     << " "
                                                     << t2
                                                     << std::endl;
    }
}

void Expression_class::check_argument_is_int(Environment &env, Expression e1, std::string op)
{
    e1->check(env);
    Symbol t1 = e1->get_type();
    if (t1 != Int)
    {
        classtable->semant_error(env.filename, this) << "Argument of "
                                                     << op
                                                     << " has type "
                                                     << t1
                                                     << " instead of Int"
                                                     << std::endl;
    }
}

void plus_class::check(Environment &env)
{
    check_argument_is_int(env, e1, e2, "+");
    type = Int;
}

void sub_class::check(Environment &env)
{
    check_argument_is_int(env, e1, e2, "-");
    type = Int;
}

void mul_class::check(Environment &env)
{
    check_argument_is_int(env, e1, e2, "*");
    type = Int;
}

void divide_class::check(Environment &env)
{
    check_argument_is_int(env, e1, e2, "/");
    type = Int;
}

void neg_class::check(Environment &env)
{
    check_argument_is_int(env, e1, "~");
    type = Int;
}

void lt_class::check(Environment &env)
{
    check_argument_is_int(env, e1, e2, "<");
    type = Bool;
}

// TODO: need some extra work
void eq_class::check(Environment &env)
{
    e1->check(env);
    e2->check(env);
    Symbol t1 = e1->get_type(),
           t2 = e2->get_type();
    if ((is_primitive(t1) || is_primitive(t2)) && t1 != t2)
    {
        classtable->semant_error(env.filename, this) << "Illegal comparison with a basic type"
                                                     << std::endl;
    }
    type = Bool;
}

void leq_class::check(Environment &env)
{
    check_argument_is_int(env, e1, e2, "<=");
    type = Bool;
}

void comp_class::check(Environment &env)
{
    e1->check(env);
    Symbol t1 = e1->get_type();
    if (t1 != Bool)
    {
        classtable->semant_error(env.filename, this) << "Argument of 'not' has type "
                                                     << t1
                                                     << " instead of Bool."
                                                     << std::endl;
    }
    type = Bool;
}

void int_const_class::check(Environment &env)
{
    type = Int;
}

void bool_const_class::check(Environment &env)
{
    type = Bool;
}

void string_const_class::check(Environment &env)
{
    type = Str;
}

void new__class::check(Environment &env)
{
    if (classtable->has_no(type_name))
    {
        classtable->semant_error(env.filename, this) << "'new' used with undefined class "
                                                     << type_name
                                                     << std::endl;
    }
    type = type_name;
}

void isvoid_class::check(Environment &env)
{
    e1->check(env);
    type = Bool;
}

void no_expr_class::check(Environment &env)
{
    type = No_type;
}

void object_class::check(Environment &env)
{
    // cout << get_line_number() << ":LOOKUP "<<name<<(symbol_table->lookup(name)==NULL)<<"$"<<(symbol_table->probe(name)==NULL)<<endl;
    Symbol object_type = env.symbol_table.lookup(name);
    if (object_type == NULL)
    {
        classtable->semant_error(env.filename, this) << "Undeclared identifier "
                                                     << name
                                                     << std::endl;
        type = Object;
    }
    else
    {
        type = object_type;
    }
}

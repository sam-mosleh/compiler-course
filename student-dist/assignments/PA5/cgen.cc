
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <algorithm>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;

int label_count = 0;

Environment::Environment(CgenNode *node)
{
  class_name = node->get_name();
  file_name = stringtable.lookup_string(node->filename->get_string());
}

void Environment::add_method_arguments_of(method_class *method)
{
  int arg_counter = 0;
  for (int i = method->formals->first(); method->formals->more(i); i = method->formals->next(i))
  {
    formal_class *f = dynamic_cast<formal_class *>(method->formals->nth(i));
    method_arguments[f->name] = arg_counter++;
  }
}

int Environment::get_variable_offset(Symbol s)
{
  std::vector<Symbol>::reverse_iterator it = std::find(variables.rbegin(), variables.rend(), s);
  if (it != variables.rend())
  {
    return std::distance(it.base(), variables.end()) + 1;
  }
  else
  {
    return 0;
  }
}

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

static char *gc_init_names[] =
    {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] =
    {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream &s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream &s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream &s)
{
  s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream &s)
{
  s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream &s)
{
  s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst &b, ostream &s)
{
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream &s)
{
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream &s)
{
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream &s)
{
  s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream &s)
{
  s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream &s)
{
  s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream &s)
{
  s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream &s)
{
  s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream &s)
{
  s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream &s)
{
  s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream &s)
{
  s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream &s)
{
  s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream &s)
{
  s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream &s)
{
  s << JAL << address << endl;
}

static void emit_return(ostream &s)
{
  s << RET << endl;
}

static void emit_gc_assign(ostream &s)
{
  s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream &s)
{
  s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s)
{
  s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s)
{
  s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream &s)
{
  s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s)
{
  s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l, s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream &s)
{
  s << BRANCH;
  emit_label_ref(l, s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream &str)
{
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

//
// Pop the stack on the register. The stack shrinks towards larger addresses.
//
static void emit_pop(char *reg, ostream &str)
{
  emit_addiu(SP, SP, 4, str);
  emit_load(reg, 0, SP, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream &s)
{
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream &s)
{
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s);  // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char *)A1)
    emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                                              // label
    << WORD << stringclasstag << endl                                     // tag
    << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
    << WORD;

  emit_disptable_ref(Str, s);

  s << endl; // dispatch table
  s << WORD;
  lensym->code_ref(s);
  s << endl;                    // string length
  emit_string_constant(s, str); // ascii string
  s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                           // label
    << WORD << intclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
    << WORD;

  emit_disptable_ref(Int, s);

  s << endl;                // dispatch table
  s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                            // label
    << WORD << boolclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
    << WORD;

  emit_disptable_ref(Bool, s);

  s << endl;                // dispatch table
  s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n"
      << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL;
  emit_protobj_ref(main, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(integer, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(string, str);
  str << endl;
  str << GLOBAL;
  falsebool.code_ref(str);
  str << endl;
  str << GLOBAL;
  truebool.code_ref(str);
  str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << tag[Int] << endl;
  str << BOOLTAG << LABEL
      << WORD << tag[Bool] << endl;
  str << STRINGTAG << LABEL
      << WORD << tag[Str] << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("String"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), str);
  str << endl
      << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str, boolclasstag);
  truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str, tag[Str]);
  inttable.code_string_table(str, tag[Int]);
  code_bools(tag[Bool]);
}

void CgenClassTable::code_nametab()
{
  str << CLASSNAMETAB << LABEL;
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    char *classname = (*it)->get_name()->get_string();
    str << WORD;
    stringtable.lookup_string(classname)->code_ref(str);
    str << endl;
  }
}

void CgenClassTable::code_objtab()
{
  str << CLASSOBJTAB << LABEL;
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    str << WORD;
    emit_protobj_ref((*it)->get_name(), str);
    str << endl;
    str << WORD;
    emit_init_ref((*it)->get_name(), str);
    str << endl;
  }
}

void CgenClassTable::code_disptab()
{
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    Symbol class_name = (*it)->get_name();
    emit_disptable_ref(class_name, str);
    str << LABEL;
    for (std::vector<std::string>::iterator it = class_method_names[class_name].begin(); it != class_method_names[class_name].end(); ++it)
      str << WORD << (*it) << endl;
  }
}

void CgenClassTable::code_protobj()
{
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    Symbol class_name = (*it)->get_name();

    // Add -1 eye catcher
    str << WORD << "-1" << endl;

    emit_protobj_ref(class_name, str);
    str << LABEL;
    str << WORD << tag[class_name] << endl
        << WORD << size[class_name] << endl
        << WORD;
    emit_disptable_ref(class_name, str);
    str << endl;
    code_attributes_with_default_value(*it);
  }
}

void CgenClassTable::code_attributes_with_default_value(CgenNode *current)
{
  if (current == NULL)
    return;
  code_attributes_with_default_value(current->get_parentnd());
  Features features = current->features;
  for (int i = features->first(); features->more(i); i = features->next(i))
  {
    Feature f = features->nth(i);
    if (attr_class *attr = dynamic_cast<attr_class *>(f))
    {
      Symbol type = attr->type_decl;
      if (type == Int)
      {
        str << WORD;
        inttable.lookup_string("0")->code_ref(str);
        str << endl;
      }
      else if (type == Bool)
      {
        str << WORD;
        falsebool.code_ref(str);
        str << endl;
      }
      else if (type == Str)
      {
        str << WORD;
        stringtable.lookup_string("")->code_ref(str);
        str << endl;
      }
      else
      {
        str << WORD << 0 << endl;
      }
    }
  }
}

void CgenClassTable::code_object_initializer()
{
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    Symbol class_name = (*it)->get_name();
    emit_init_ref(class_name, str);
    str << LABEL;

    // Store FP and Self
    emit_addiu(SP, SP, -(DEFAULT_OBJFIELDS * 4), str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 4, str);
    emit_move(SELF, ACC, str);

    if (class_name != Object)
    {
      // initialize parent class first
      str << JAL;
      emit_init_ref((*it)->get_parent(), str);
      str << endl;
    }

    Features fs = (*it)->features;
    std::vector<attr_class *> all_attributes;

    for (int i = fs->first(); fs->more(i); i = fs->next(i))
    {
      attr_class *attr = dynamic_cast<attr_class *>(fs->nth(i));
      if (attr != NULL)
        all_attributes.push_back(attr);
    }

    Environment env(*it);

    for (std::vector<attr_class *>::iterator attr_it = all_attributes.begin(); attr_it != all_attributes.end(); ++attr_it)
    {
      if (!(*attr_it)->init->is_empty())
      {
        (*attr_it)->init->code(str, env);
        emit_store(ACC, class_attribute_offset[class_name][(*attr_it)->name], SELF, str);
      }
    }

    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, DEFAULT_OBJFIELDS * 4, str);
    emit_return(str);
  }
}

void CgenClassTable::code_class_method()
{
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    if (!(*it)->basic())
    {
      Environment env(*it);
      Features fs = (*it)->features;
      for (int i = fs->first(); fs->more(i); i = fs->next(i))
      {
        if (method_class *method = dynamic_cast<method_class *>(fs->nth(i)))
        {
          env.add_method_arguments_of(method);

          emit_method_ref((*it)->name, method->name, str);
          str << LABEL;

          emit_addiu(SP, SP, -(4 * 4), str);
          emit_store(FP, 4, SP, str);
          emit_store(SELF, 3, SP, str);
          emit_store(RA, 2, SP, str);
          emit_addiu(FP, SP, 4, str);
          emit_move(SELF, ACC, str);

          method->expr->code(str, env);

          emit_load(FP, 4, SP, str);
          emit_load(SELF, 3, SP, str);
          emit_load(RA, 2, SP, str);
          emit_addiu(SP, SP, (4 + method->formals->len()) * 4, str);
          emit_return(str);

          env.method_arguments.clear();
        }
      }
    }
  }
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();
  set_class_tags_and_sizes(root_object, 0, DEFAULT_OBJFIELDS);
  set_class_offsets();

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

  // The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                     Basic, this));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                     Basic, this));
  addid(prim_slot,
        new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                     Basic, this));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  root_object = new CgenNode(
      class_(Object,
             No_class,
             append_Features(
                 append_Features(
                     single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                     single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                 single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
             filename),
      Basic, this);
  install_class(root_object);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(
      new CgenNode(
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
                 filename),
          Basic, this));

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(
      new CgenNode(
          class_(Int,
                 Object,
                 single_Features(attr(val, prim_slot, no_expr())),
                 filename),
          Basic, this));

  //
  // Bool also has only the "val" slot.
  //
  install_class(
      new CgenNode(
          class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
          Basic, this));

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(
      new CgenNode(
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
                 filename),
          Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
  {
    return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
    set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

int number_of_attributes(CgenNode *current)
{
  int attributes = 0;
  Features features = current->features;
  for (int i = features->first(); features->more(i); i = features->next(i))
  {
    Feature f = features->nth(i);
    if (attr_class *attr = dynamic_cast<attr_class *>(f))
      attributes++;
  }
  return attributes;
}

//
// CgenClassTable::set_class_tags_and_sizes
//
int CgenClassTable::set_class_tags_and_sizes(CgenNode *current, int class_tag, int parent_size)
{
  nodes.push_back(current);
  Symbol name = current->get_name();
  stringtable.add_string(name->get_string());
  tag[name] = class_tag++;

  int class_size = parent_size + number_of_attributes(current);
  size[name] = class_size;

  std::vector<CgenNode *> children;
  for (List<CgenNode> *l = current->get_children(); l; l = l->tl())
    children.push_back(l->hd());
  for (std::vector<CgenNode *>::reverse_iterator it = children.rbegin(); it != children.rend(); it++)
  {
    class_tag = set_class_tags_and_sizes(*it, class_tag, class_size);
  }

  // for (List<CgenNode> *l = current->get_children(); l; l = l->tl())
  //   class_tag = set_class_tags_and_sizes(l->hd(), class_tag, class_size);

  last_child_tag[name] = class_tag - 1;

  return class_tag;
}

void CgenClassTable::set_class_offsets()
{
  for (std::vector<CgenNode *>::iterator it = nodes.begin(); it != nodes.end(); ++it)
  {
    set_class_attribute_offset(*it, (*it)->get_name());
    set_class_method_offset(*it, (*it)->get_name());
  }
}

std::string static_method_name(Symbol class_name, Symbol method_name)
{
  std::string result(class_name->get_string());
  result.append(".");
  result.append(method_name->get_string());
  return result;
}

int CgenClassTable::set_class_method_offset(CgenNode *current, Symbol class_name)
{
  if (current == NULL)
    return 0;
  int method_offset = set_class_method_offset(current->get_parentnd(), class_name);
  Features features = current->features;

  for (int i = features->first(); features->more(i); i = features->next(i))
  {
    Feature f = features->nth(i);
    if (method_class *method = dynamic_cast<method_class *>(f))
    {
      Symbol method_name = method->name;
      std::string static_name = static_method_name(current->get_name(), method_name);
      std::string dynamic_name = static_method_name(class_name, method_name);
      class_method_names[class_name].push_back(static_name);
      class_method_offset[class_name][static_name] = method_offset;
      class_method_offset[class_name][dynamic_name] = method_offset;
      if (cgen_debug)
        cout << "CLASS " << class_name << " METHOD " << static_name << " OFFSET " << method_offset << endl;
      method_offset++;
    }
  }
  return method_offset;
}

int CgenClassTable::set_class_attribute_offset(CgenNode *current, Symbol class_name)
{
  if (current == NULL)
    return DEFAULT_OBJFIELDS;
  int attribute_offset = set_class_attribute_offset(current->get_parentnd(), class_name);
  Features features = current->features;

  for (int i = features->first(); features->more(i); i = features->next(i))
  {
    Feature f = features->nth(i);
    if (attr_class *attr = dynamic_cast<attr_class *>(f))
    {
      class_attribute_offset[class_name][attr->name] = attribute_offset++;
    }
  }
  return attribute_offset;
}

void CgenClassTable::code()
{
  if (cgen_debug)
    cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug)
    cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug)
    cout << "coding constants" << endl;
  code_constants();

  //                 Add your code to emit
  //                   - prototype objects
  //                   - class_nameTab
  //                   - dispatch tables
  if (cgen_debug)
    cout << "coding class name table" << endl;
  code_nametab(); // Class names list

  if (cgen_debug)
    cout << "coding class object table" << endl;
  code_objtab(); // Class objects list

  if (cgen_debug)
    cout << "coding class dispatch table" << endl;
  code_disptab(); // Dispatch tables

  if (cgen_debug)
    cout << "coding prototype objects" << endl;
  code_protobj(); // Prototype objects

  if (cgen_debug)
    cout << "coding global text" << endl;
  code_global_text();

  //                 Add your code to emit
  //                   - object initializer
  //                   - the class methods
  //                   - etc...

  if (cgen_debug)
    cout << "coding object initializers" << endl;
  code_object_initializer(); // Object initializers

  if (cgen_debug)
    cout << "coding class methods" << endl;
  code_class_method(); // Class methods
}

CgenNodeP CgenClassTable::root()
{
  return probe(Object);
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) : class__class((const class__class &)*nd),
                                                                       parentnd(NULL),
                                                                       children(NULL),
                                                                       basic_status(bstatus)
{
  // stringtable.add_string(name->get_string()); // Add class name to string table
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, Environment &env)
{
  expr->code(s, env);
  if (int variable_offset = env.get_variable_offset(name))
  {
    emit_store(ACC, variable_offset, SP, s);
  }
  else if (env.method_arguments.find(name) != env.method_arguments.end())
  {
    int argument_offset = 3 + env.method_arguments.size() - env.method_arguments[name];
    emit_store(ACC, argument_offset, FP, s);
  }
  else if (class_attribute_offset[env.class_name].find(name) != class_attribute_offset[env.class_name].end())
  {
    emit_store(ACC, class_attribute_offset[env.class_name][name], SELF, s);
  }
}

void if_acc_is_null_abort(char *abort_function, int line_number, ostream &s, Environment &env)
{
  int dont_abort_label = label_count++;
  // if ACC is null then call abort function
  emit_bne(ACC, ZERO, dont_abort_label, s);
  // ACC is null
  emit_load_string(ACC, env.file_name, s);
  emit_load_imm(T1, line_number, s);
  emit_jal(abort_function, s);
  emit_label_def(dont_abort_label, s);
}

void static_dispatch_class::code(ostream &s, Environment &env)
{
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    actual->nth(i)->code(s, env);
    emit_push(ACC, s);
  }

  expr->code(s, env);

  if_acc_is_null_abort("_dispatch_abort", get_line_number(), s, env);

  // T1 = typename->dispatch list
  emit_partial_load_address(T1, s), emit_disptable_ref(type_name, s), s << endl;

  std::string static_name = static_method_name(type_name, name);
  int offset = class_method_offset[env.class_name][static_name];
  // T1 = typename->dispatch_pointer + offset
  emit_load(T1, offset, T1, s);
  // set $ra to next instruction and jump to $t1
  emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s, Environment &env)
{
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    actual->nth(i)->code(s, env);
    emit_push(ACC, s);
  }
  // Class object code
  expr->code(s, env);

  if_acc_is_null_abort("_dispatch_abort", get_line_number(), s, env);
  // ACC is not null
  // T1 = ACC->dispatch_pointer
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);

  int offset;
  std::string static_name;
  if (expr->get_type() == SELF_TYPE)
  {
    static_name = static_method_name(env.class_name, name);
    offset = class_method_offset[env.class_name][static_name];
  }
  else
  {
    static_name = static_method_name(expr->get_type(), name);
    offset = class_method_offset[expr->get_type()][static_name];
  }

  // T1 = ACC->dispatch_pointer + offset
  emit_load(T1, offset, T1, s);
  // set $ra to next instruction and jump to $t1
  emit_jalr(T1, s);
}

void cond_class::code(ostream &s, Environment &env)
{
  int else_label = label_count++, fi_label = label_count++;
  pred->code(s, env);

  // T1 = ACC->int
  emit_fetch_int(T1, ACC, s);
  // GOTO else if T1 == 0
  emit_beqz(T1, else_label, s);
  // THEN
  then_exp->code(s, env);
  emit_branch(fi_label, s);
  // ELSE
  emit_label_def(else_label, s);
  else_exp->code(s, env);
  // FI
  emit_label_def(fi_label, s);
}

void loop_class::code(ostream &s, Environment &env)
{
  int pred_label = label_count++, loopend_label = label_count++;
  emit_label_def(pred_label, s);
  pred->code(s, env);

  // T1 = ACC->int
  emit_fetch_int(T1, ACC, s);
  // GOTO loopend if T1 == 0
  emit_beqz(T1, loopend_label, s);

  body->code(s, env);
  // GOTO pred
  emit_branch(pred_label, s);

  emit_label_def(loopend_label, s);
}

bool compare_branches_by_type_tag_descending(branch_class *a, branch_class *b)
{
  return tag[a->type_decl] > tag[b->type_decl];
}

void typcase_class::code(ostream &s, Environment &env)
{
  expr->code(s, env);

  int end_case_label = label_count++;

  std::vector<branch_class *> branches;
  for (int i = cases->first(); cases->more(i); i = cases->next(i))
    if (branch_class *branch = dynamic_cast<branch_class *>(cases->nth(i)))
      branches.push_back(branch);
  // Branches have to get sorted by type tag in descending order
  std::sort(branches.begin(), branches.end(), compare_branches_by_type_tag_descending);

  if_acc_is_null_abort("_case_abort2", get_line_number(), s, env);
  // ACC is not null
  // T2 = ACC->tag
  emit_load(T2, TAG_OFFSET, ACC, s);

  for (std::vector<branch_class *>::size_type i = 0; i < branches.size(); i++)
  {
    branch_class *b = branches[i];
    int next_label = label_count++,
        class_tag = tag[b->type_decl],
        maximum_child_tag = last_child_tag[b->type_decl];
    // if T2 not in [tag...maximum_child_tag] GOTO next_label
    emit_blti(T2, class_tag, next_label, s);
    emit_bgti(T2, maximum_child_tag, next_label, s);
    // T2 found between [tag...maximum_child_tag]
    b->expr->code(s, env);
    // GOTO end of case
    emit_branch(end_case_label, s);
    // Next checking state
    emit_label_def(next_label, s);
  }
  // T2 not found
  emit_jal("_case_abort", s);

  emit_label_def(end_case_label, s);
}

void block_class::code(ostream &s, Environment &env)
{
  for (int i = body->first(); body->more(i); i = body->next(i))
  {
    body->nth(i)->code(s, env);
  }
}

void let_class::code(ostream &s, Environment &env)
{
  init->code(s, env);
  if (init->is_empty())
  {
    if (type_decl == Str)
    {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    }
    else if (type_decl == Int)
    {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    }
    else if (type_decl == Bool)
    {
      emit_load_bool(ACC, falsebool, s);
    }
    else
    {
      emit_move(ACC, ZERO, s);
    }
  }
  emit_push(ACC, s);
  env.variables.push_back(identifier);

  body->code(s, env);

  env.variables.pop_back();
  emit_pop(ACC, s);
}

void pre_op(Expression e1, Expression e2, ostream &s, Environment &env)
{
  e1->code(s, env);
  emit_push(ACC, s);
  e2->code(s, env);
  // Copy object (ACC pointer)
  emit_jal("Object.copy", s);
  // T2 = ACC->int
  emit_fetch_int(T2, ACC, s);
  // T1 = *e1
  emit_pop(T1, s);
  // T1 = e1->int
  emit_fetch_int(T1, T1, s);
}

void plus_class::code(ostream &s, Environment &env)
{
  pre_op(e1, e2, s, env);
  // T1 += T2
  emit_add(T1, T1, T2, s);
  // ACC->int = T1
  emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s, Environment &env)
{
  pre_op(e1, e2, s, env);
  // T1 -= T2
  emit_sub(T1, T1, T2, s);
  // ACC->int = T1
  emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s, Environment &env)
{
  pre_op(e1, e2, s, env);
  // T1 *= T2
  emit_mul(T1, T1, T2, s);
  // ACC->int = T1
  emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s, Environment &env)
{
  pre_op(e1, e2, s, env);
  // T1 /= / T2
  emit_div(T1, T1, T2, s);
  // ACC->int = T1
  emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s, Environment &env)
{
  e1->code(s, env);
  emit_jal("Object.copy", s);
  // T1 = ACC->int
  emit_fetch_int(T1, ACC, s);
  // T1 = -T1
  emit_neg(T1, T1, s);
  // ACC->int = T1
  emit_store_int(T1, ACC, s);
}

void pre_int_comparison(Expression e1, Expression e2, ostream &s, Environment &env)
{
  e1->code(s, env);
  emit_push(ACC, s);

  e2->code(s, env);

  // T1 = *e1
  emit_pop(T1, s);

  // T1 = T1->int, T2 = ACC->int
  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, ACC, s);

  emit_load_bool(ACC, truebool, s);
}

void post_comparison(int label, ostream &s)
{
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label, s);
}

void lt_class::code(ostream &s, Environment &env)
{
  pre_int_comparison(e1, e2, s, env);

  int label = label_count++;
  emit_blt(T1, T2, label, s);

  post_comparison(label, s);
}

void eq_class::code(ostream &s, Environment &env)
{
  e1->code(s, env);
  emit_push(ACC, s);

  e2->code(s, env);

  // T1 = *e1
  emit_pop(T1, s);
  // T2 = ACC (a pointer)
  emit_move(T2, ACC, s);

  int label = label_count++;

  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, T2, label, s);
  emit_load_bool(A1, falsebool, s);
  emit_jal("equality_test", s);
  emit_label_def(label, s);
}

void leq_class::code(ostream &s, Environment &env)
{
  pre_int_comparison(e1, e2, s, env);

  int label = label_count++;
  emit_bleq(T1, T2, label, s);

  post_comparison(label, s);
}

void set_acc_to_compliment_of(char *reg, ostream &s)
{
  int out_of_branch_label = label_count++;
  // ACC = True
  emit_load_bool(ACC, truebool, s);
  // GOTO label if reg == 0
  emit_beqz(reg, out_of_branch_label, s);
  // ACC = False
  emit_load_bool(ACC, falsebool, s);
  // Label
  emit_label_def(out_of_branch_label, s);
}

void comp_class::code(ostream &s, Environment &env)
{
  e1->code(s, env);
  // T1 = ACC->int
  emit_fetch_int(T1, ACC, s);
  set_acc_to_compliment_of(T1, s);
}

void int_const_class::code(ostream &s, Environment &env)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s, Environment &env)
{
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s, Environment &env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Environment &env)
{
  if (type_name != SELF_TYPE)
  {
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(type_name, s);
    s << endl;

    emit_jal("Object.copy", s);
    s << JAL;
    emit_init_ref(type_name, s);
    s << endl;
  }
  else
  {
    // T1 = Class Object list
    emit_load_address(T1, CLASSOBJTAB, s);
    // T2 = Current class tag
    emit_load(T2, 0, SELF, s);
    // T2 *= 8
    emit_sll(T2, T2, 3, s);
    // T1 += T2, now T1 points to the class protObj
    emit_addu(T1, T1, T2, s);
    // Save T1
    emit_push(T1, s);
    // Copy new T1 to ACC
    emit_load(ACC, 0, T1, s);
    emit_jal("Object.copy", s);
    // Load T1
    emit_pop(T1, s);
    // T1 = Class init
    emit_load(T1, 1, T1, s);
    emit_jalr(T1, s);
  }
}

void isvoid_class::code(ostream &s, Environment &env)
{
  e1->code(s, env);
  emit_move(T1, ACC, s);
  set_acc_to_compliment_of(T1, s);
}

void no_expr_class::code(ostream &s, Environment &env)
{
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, Environment &env)
{
  if (name == self)
  {
    emit_move(ACC, SELF, s);
  }
  else if (int variable_offset = env.get_variable_offset(name))
  {
    emit_load(ACC, variable_offset, SP, s);
  }
  else if (env.method_arguments.find(name) != env.method_arguments.end())
  {
    int argument_offset = 3 + env.method_arguments.size() - env.method_arguments[name];
    emit_load(ACC, argument_offset, FP, s);
  }
  else if (class_attribute_offset[env.class_name].find(name) != class_attribute_offset[env.class_name].end())
  {
    emit_load(ACC, class_attribute_offset[env.class_name][name], SELF, s);
  }
}

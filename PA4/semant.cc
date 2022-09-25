

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <list>
#include <set>
#include <vector>
#include <sstream>

static bool TESTING = false;
static std::ostringstream nop_sstream;
static std::ostream &log = TESTING ? std::cout : nop_sstream;

extern int semant_debug;
extern char *curr_filename;

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
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}


Class_ curr_class = NULL;
static ClassTable* classtable;

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    this->install_basic_classes();

    log << "First pass: checking class definition..." << endl;
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        // check redefinition of SELF_TYPE
        // must acquire the symbol name of each class, thus have to modify the cool-tree.h file
        if(classes->nth(i)->getName() == SELF_TYPE) {
            // report error
            this->semant_error(classes->nth(i)) << "Error: SELF_TYPE redefinition" << endl;
            return;
        }

        // check for redifinition of classes
        if(this->className.find(classes->nth(i)->getName()) == this->className.end()) {
            ClassName cn((class__class *)classes->nth(i));
            this->className.insert(std::make_pair(classes->nth(i)->getName(), cn));
        }
        else {
            // error
            this->semant_error(classes->nth(i)) << "Error: class redefinition" << endl;
            return;
        }
    }
    // check Main class and main method are defined
    if(this->className.find(Main) == this->className.end()) {
        this->semant_error() << "Class Main is not defined." << endl;
        return;
    }
    else {
        curr_class = this->className[Main].cls;
        Features f = curr_class->getFeatures();
        bool find_main = false;
        for(int i = f->first(); f->more(i); i = f->next(i)) {
            if(f->nth(i)->isMethod() && f->nth(i)->getName() == main_meth) {
                find_main = true;
            }
        }
        if(!find_main) {
            this->semant_error(curr_class) << "Error: main method not defined in Main class" << endl;
        }
    }
    
    log << "Second pass: checking class inheritance graph..." << endl;
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        Symbol parent = curr_class->getParent();
        log << "    - Checking class " << curr_class->getName() << endl;
        // check if parent of each class exists
        if(this->className.find(parent) == this->className.end()) {
            this->semant_error(curr_class) << "Error: parent class undefined" << endl;
            return;
        }
        // check if it inherits from Int/String/Bool/SELF_TYPE
        if(parent == Int || parent == Str || parent == Bool || parent == SELF_TYPE) {
            this->semant_error(curr_class) << "Error: parent class " << parent << endl;
            return;
        }
        // check if is a DAG -> not cyclic inheritance
        log << "        - Checking cyclic inheritance of " << curr_class->getName() << endl;
        while(parent != Object) {
            log << "        - Parent is " << parent << endl;
            if(parent == classes->nth(i)->getName()) {
                this->semant_error(curr_class) << "Error: cyclic inheritance" << endl;
                return;
            }
            curr_class = this->className[parent].cls;
            parent = curr_class->getParent();
        }
    }
}

void ClassTable::install_basic_classes() {

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

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

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

    
    // add 5 base classes into the class table
    this->className[Object] = ClassName(Object_class);
    this->className[IO] = ClassName(IO_class);
    this->className[Int] = ClassName(Int_class);
    this->className[Bool] = ClassName(Bool_class);
    this->className[Str] = ClassName(Str_class);

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

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 


/*
    Get the inheritance path from current class upto Object
*/
std::list<Symbol> ClassTable::getInheritancePath(Symbol type) {
    if(type == SELF_TYPE) {
        type = curr_class->getName();
    }
    std::list<Symbol> path;
    // Object's parent is No_class
    for(Symbol t = type; t != No_class; t = classtable->className[t].cls->getParent()) {
        path.push_back(t);
    }
    return path;
}

/*
    Add attributes of one class into a specific attribute table
*/
void ClassTable::addAttrTable(Symbol name, AttrTable *curr_at) {
    Features fs = this->className[name].cls->getFeatures();
    curr_at->enterscope();
    for(int i = fs->first(); fs->more(i); i = fs->next(i)) {
        if(!fs->nth(i)->isMethod()) {
            Feature f = fs->nth(i);
            if(f->getName() == self) {
                this->semant_error(curr_class) << "Error: self as attribute name" << endl;
            }
            if(curr_at->probe(f->getName()) != NULL) {
                this->semant_error(curr_class) << "Error: redifition of attribute in same class" << endl;
            }
            //log << "            - Checking ancestor " << f->getName() << endl;
            if(curr_at->lookup(f->getName()) != NULL) {
                this->semant_error(curr_class) << "Error: redifinition of inherited attribute" << endl;
            }
            log << "        - Adding attributes " << f->getName() << endl;
            curr_at->addid(f->getName(), new Symbol(f->getType()));
        }
    }
}

/*
    Check if parent is an ancestor of child
*/
bool ClassTable::checkInheritance(Symbol child, Symbol parent) {
    // if parent == SELF_TYPE, relation can only hold when child == SELF_TYPE as well
    if(parent == SELF_TYPE) return child == SELF_TYPE;
    // check if parent is ancestor
    if(child == SELF_TYPE) child = curr_class->getName();
    std::list<Symbol> path = this->getInheritancePath(child);
    for(std::list<Symbol>::iterator it = path.begin(); it != path.end(); it++) {
        if(parent == *it) return true;
    }
    return false;
}

/*
    Find the least upper bound of class c1 and c2
*/
Symbol ClassTable::LUB(Symbol c1, Symbol c2) {
    std::list<Symbol> path1 = this->getInheritancePath(c1);
    std::list<Symbol> path2 = this->getInheritancePath(c2);
    // find the first common element in the two paths
    // start from the back of the list, which both of them end with Object
    std::list<Symbol>::reverse_iterator it1 = path1.rbegin();
    std::list<Symbol>::reverse_iterator it2 = path2.rbegin();
    Symbol ret;
    while(it1 != path1.rend() && it2 != path2.rend()) {
        if(*it1 == *it2) {
            ret = *it1;
        }
        else {
            break;
        }
        it1++;
        it2++;
    }
    return ret;
}

/*
    Check the type of the expression
*/
Symbol assign_class::checkExpression() {
    log << "            - Checking assign expression" << endl;
    if(this->name == self) {
        classtable->semant_error(curr_class) << "Error: unallowed assignment to self" << endl;
        type = Object;
        return Object;
    }
    Symbol *attr = classtable->className[curr_class->getName()].attrTable->lookup(this->name);
    Symbol rval = expr->checkExpression();
    //log << "                - Assign expression has type " << rval << endl;

    if(attr == NULL) {
        // for error in type, assign Object
        classtable->semant_error(curr_class) << "Error: variable undefined in assign expression" << endl;
        type = Object;
        return Object;
    }
    else {
        // check if type of variable is the parent of the type of expression
        if(!classtable->checkInheritance(rval, *attr)) {
            classtable->semant_error(curr_class) << "Error: unmatched type in assignment" << endl;
            type = Object;
            return Object;
        }
        else {
            type = *attr;
            return *attr;
        }
    }
}

/*
    Expression expr;
    Symbol type_name;
    Symbol name;
    Expressions actual;
*/
Symbol static_dispatch_class::checkExpression() {
    log << "            - Checking static_dispatch expression" << endl;
    // first check if the class of expr is child of type_name
    Symbol expr_type = this->expr->checkExpression();
    if(!classtable->checkInheritance(expr_type, this->type_name)) {
        classtable->semant_error(curr_class) << "Error: wrrong inheritance relationship in static dispatch" << endl;
        type = Object;
        return Object;
    }
    // then check if the class expr in has method name
    std::list<Symbol> path = classtable->getInheritancePath(this->type_name);
    method_class *mc = NULL;
    for(std::list<Symbol>::iterator it = path.begin(); it != path.end(); it++) {
        if((mc = classtable->className[*it].methodTable->lookup(this->name)) != NULL) {
            break;
        }
    }
    bool err_flag = false;
    if(mc == NULL) {
        classtable->semant_error(curr_class) << "Error: undefined method call in dispatch expression" << endl;
        err_flag = true;
    }
    // then check if the formal list of actual matches with that of mathod, on number and type
    Expression body;
    Symbol body_type;
    for(int i = this->actual->first(); this->actual->more(i); i = this->actual->next(i)) {
        body = this->actual->nth(i);
        body_type = body->checkExpression();
        if(mc != NULL) {
            if(!mc->getFormals()->more(i)) {
                classtable->semant_error(curr_class) << "Error: unmatched number of formals in static dispatch expression" << endl;
                err_flag = true;
            }
            else if(!classtable->checkInheritance(body_type, mc->getFormals()->nth(i)->getType())) {
                classtable->semant_error(curr_class) << "Error: unmatched types of formal in static dispatch expression" << endl;
                err_flag = true;
            }
        }
    }
    if(err_flag) {
        type = Object;
        return Object;
    }
    else {
        type = mc->getType();
        if(type == SELF_TYPE) {
            type = this->type_name;
        }
        return type;
    }
}

/* Expression expr;
   Symbol name;
   Expressions actual;
*/
Symbol dispatch_class::checkExpression() {
    log << "            - Checking dispatch expression" << endl;
    // first check if the class expr is in has method name
    Symbol expr_type = this->expr->checkExpression();
    //if(expr_type == SELF_TYPE) expr_type = curr_class->getName();
    std::list<Symbol> path = classtable->getInheritancePath(expr_type);
    method_class *mc = NULL;
    for(std::list<Symbol>::iterator it = path.begin(); it != path.end(); it++) {
        if((mc = classtable->className[*it].methodTable->lookup(this->name)) != NULL) {
            break;
        }
    }
    bool err_flag = false;
    if(mc == NULL) {
        classtable->semant_error(curr_class) << "Error: undefined method call in dispatch expression" << endl;
        err_flag = true;
    }
    // then check if the formal list of actual matches with that of mathod, on number and type
    Expression body;
    Symbol body_type;
    for(int i = this->actual->first(); this->actual->more(i); i = this->actual->next(i)) {
        body = this->actual->nth(i);
        body_type = body->checkExpression();
        if(mc != NULL) {
            if(!mc->getFormals()->more(i)) {
                classtable->semant_error(curr_class) << "Error: unmatched number of formals in dispatch expression" << endl;
                err_flag = true;
            }
            else if(!classtable->checkInheritance(body_type, mc->getFormals()->nth(i)->getType())) {
                classtable->semant_error(curr_class) << "Error: unmatched types of formal in dispatch expression" << endl;
                err_flag= true;
            }
        }
    }
    if(err_flag) {
        type = Object;
        return Object;
    }
    else {
        type = mc->getType();
        if(type == SELF_TYPE) {
            type = expr_type;
        }
        return type;
    }
}

Symbol cond_class::checkExpression() {
    log << "            - Checking condition expression" << endl;
    // if expr then expr else expr fi
    if(this->pred->checkExpression() != Bool) {
        classtable->semant_error(curr_class) << "Error: non-Booliean type as predicate in if expression" << endl;
        // does not need to return
    }
    Symbol thentype = this->then_exp->checkExpression();
    Symbol elsetype = this->else_exp->checkExpression();
    // elsetype can be No_type, meaning no else branch
    if(elsetype == No_type) {
        type = thentype;
    }
    else {
        type = classtable->LUB(thentype, elsetype);
    }
    return type;
}

Symbol loop_class::checkExpression() {
    log << "            - Checking loop expression" << endl;
    // while expr loop expr pool
    if(this->pred->checkExpression() != Bool) {
        classtable->semant_error(curr_class) << "Error: non-Booliean type as predicate in while-loop expression" << endl;
    }
    this->body->checkExpression();
    type = Object;
    return Object;
}

// branch
// ======
// Symbol name;
// Symbol type_decl;
// Expression expr;
// 
Symbol branch_class::checkCase() {
    log << "            - Checking case_branch expression" << endl;
    classtable->className[curr_class->getName()].attrTable->enterscope();
    classtable->className[curr_class->getName()].attrTable->addid(this->name, new Symbol(this->type_decl));

    Symbol type = this->expr->checkExpression();
    classtable->className[curr_class->getName()].attrTable->exitscope();
    return type;
}

Symbol typcase_class::checkExpression() {
    log << "            - Checking case expression" << endl;
    Symbol expr_type = this->expr->checkExpression();

    // find the declared type of evary branch to find duplication
    // obtain the type of branch expression and use LUB to find return type
    Case branch;
    std::vector<Symbol> branchType;
    std::vector<Symbol> varType;
    for(int i = this->cases->first(); this->cases->more(i); i = this->cases->next(i)) {
        branch = this->cases->nth(i);
        Symbol type = branch->checkCase();
        branchType.push_back(type);
        varType.push_back(dynamic_cast<branch_class*>(branch)->getType());
    }
    // check duplication
    for(int i = 0; i < (int)varType.size() - 1; i++) {
        for(int j = i + 1; j < (int)varType.size(); j++) {
            if(varType[i] == varType[j]) {
                classtable->semant_error(curr_class) << "Error: deplicated type in case branches" << endl;
            }
        }
    }
    // obtain return type
    Symbol ret = branchType[0];
    for(int i = 1; i < (int)branchType.size(); i++) {
        ret = classtable->LUB(ret, branchType[i]);
    }
    type = ret;
    return ret;
}

Symbol block_class::checkExpression() {
    log << "            - Checking block expression" << endl;
    Expressions exprs = this->body;
    for(int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
        type = exprs->nth(i)->checkExpression();
    }
    return type;
}

Symbol let_class::checkExpression() {
    log << "            - Checking let expression" << endl;
    // already divided into nested let expressions
    // self not allowed in let binding
    if(this->identifier == self) {
        classtable->semant_error(curr_class) << "Error: unallowed self in let binding" << endl;
    }
    // enterscope for every decalred variable
    classtable->className[curr_class->getName()].attrTable->enterscope();
    classtable->className[curr_class->getName()].attrTable->addid(this->identifier, new Symbol(this->type_decl));

    Symbol init_type = this->init->checkExpression();
    // init_type == No_type maens no expression
    if(init_type != No_type) {
        if(!classtable->checkInheritance(init_type, this->type_decl)) {
            classtable->semant_error(curr_class) << "Error: unmatched type in let binding" << endl;
        }
    }
    type = this->body->checkExpression();
    classtable->className[curr_class->getName()].attrTable->exitscope();
    return type;
}

Symbol plus_class::checkExpression() {
    log << "            - Checking plus expression" << endl;
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype != Int || rtype != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in plus expression" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol sub_class::checkExpression() {
    log << "            - Checking sub expression" << endl;
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype != Int || rtype != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in sub expression" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol mul_class::checkExpression() {
    log << "            - Checking multiply expression" << endl;
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype != Int || rtype != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in multiply expression" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol divide_class::checkExpression() {
    log << "            - Checking divide expression" << endl;
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype != Int || rtype != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in divide expression" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol neg_class::checkExpression() {
    log << "            - Checking negate ~ expression" << endl;
    // negate, '~' as common
    if(this->e1->checkExpression() != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in neg expression" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol lt_class::checkExpression() {
    log << "            - Checking lt expression" << endl;
    // must be Int comparison
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype != Int || rtype != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in leq expression" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}
Symbol eq_class::checkExpression() {
    log << "            - Checking eq expression" << endl;
    // the '=' sign, means == in other prog-language
    // for Int, Bool, and String, check if left-type == right-type, return bool
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype == Int || rtype == Int || ltype == Bool || rtype == Bool || ltype == Str || rtype == Str ) {
        if(ltype != rtype) {
            classtable->semant_error(curr_class) << "Error: unmatched left and right type in eq expression" << endl;
            type = Object;
        }
        else type = Bool;
    }
    // for all other types, compare freely, only check pointer equality
    else {
        type = Bool;
    }
    return type;
}

Symbol leq_class::checkExpression() {
    log << "            - Checking leq expression" << endl;
    // must be Int comparison
    Symbol ltype = this->e1->checkExpression();
    Symbol rtype = this->e2->checkExpression();
    if(ltype != Int || rtype != Int) {
        classtable->semant_error(curr_class) << "Error: non-Int type in leq expression" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}

Symbol comp_class::checkExpression() {
    log << "            - Checking compare expression" << endl;
    // ancesctor of <, <=, =, only bool type checks
    if(this->e1->checkExpression() != Bool) {
        classtable->semant_error(curr_class) << "Error: non-booliean type in comparison expression" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}

Symbol int_const_class::checkExpression() {
    log << "            - Checking int expression" << endl;
    type = Int;
    return Int;
}

Symbol bool_const_class::checkExpression() {
    log << "            - Checking bool expression" << endl;
    type = Bool;
    return Bool;
}

Symbol string_const_class::checkExpression() {
    log << "            - Checking str expression" << endl;
    type = Str;
    return Str;
}

Symbol new__class::checkExpression() {
    log << "            - Checking new expression" << endl;
    // can be SELF_TYPE
    if(this->getType() != SELF_TYPE && classtable->className.find(this->getType()) == classtable->className.end()) {
        classtable->semant_error(curr_class) << "Error: undefined type in new expression" << endl;
        type = Object;
        return Object;
    }
    else {
        type = this->getType();
        return this->getType();
    }
}

Symbol isvoid_class::checkExpression() {
    log << "            - Checking isvoid expression" << endl;
    this->e1->checkExpression();
    type = Bool;
    return Bool;
}

Symbol no_expr_class::checkExpression() {
    log << "            - Checking no_expr expression" << endl;
    return No_type;
}

Symbol object_class::checkExpression() {
    log << "            - Checking object expression" << endl;
    // this is the class of variable name only, check attrTable for type of it
    if(this->name == self) {
        type = SELF_TYPE;
        return SELF_TYPE;
    }
    Symbol *found_type = classtable->className[curr_class->getName()].attrTable->lookup(this->name);
    if(found_type == NULL) {
        classtable->semant_error(curr_class) << "Error: undefined variable in expression" << endl;
        type = Object;
    }
    else {
        type = *found_type;
    }
    return type;
}


/*
    Check the type of the feature as method
*/
void method_class::checkFeatureType() {
    log << "        - Checking method " << this->getName() << endl;
    // check if the returned expression type confroms to the declared type
    Symbol rettyp = this->getType();
    if(classtable->className.find(rettyp) == classtable->className.end() && rettyp != SELF_TYPE) {
        classtable->semant_error(curr_class) << "Error: undefined return type " << rettyp << "in method" << endl;
    }
    // attribute table of each class contains attributes already
    // add formal parameters 
    classtable->className[curr_class->getName()].attrTable->enterscope();
    Formals fmls = this->getFormals();
    for(int i = fmls->first(); fmls->more(i); i = fmls->next(i)) {
        Formal fml = fmls->nth(i);
        classtable->className[curr_class->getName()].attrTable->addid(fml->getName(), new Symbol(fml->getType()));
    }
    log << "          - Checking method's expression" << endl;
    Symbol exptype = this->getExpr()->checkExpression();
    log << "          - Method's expression type is " << exptype << endl;
    if(exptype == No_type) {
    }
    else if(classtable->checkInheritance(exptype, rettyp) == false) {
        // error
        classtable->semant_error(curr_class) << "Error: unmatched expression type with return type in method" << endl;
    }
    classtable->className[curr_class->getName()].attrTable->exitscope();
    log << "        - Checking method done" << endl;
}

/*
    Check the type of the feature as attribute
*/
void attr_class::checkFeatureType() {
    log << "        - Checking attribute " << this->getName() << endl;
    // check if the initialized type confroms to the declared type
    Symbol exptype = this->getExpr()->checkExpression();
    if(exptype != No_type) {
        if(classtable->checkInheritance(exptype, this->getType()) == false) {
            classtable->semant_error(curr_class) << "Error: unmatched initialized type with declared type in attribute" << endl;
        }
    }
    else {
        log << "            - No initialization" << endl;
    }
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
    // First pass here.
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
    
    log << "Third pass: checking class features definition..." << endl;

    MethodTable *curr_mt;
    Features fs;
    Feature f;
    for(std::map<Symbol, ClassName>::iterator it = classtable->className.begin(); it != classtable->className.end(); it++) {
        log << "    - Checking class " << it->first << endl;
        curr_mt = it->second.methodTable;
        curr_mt->enterscope();
        curr_class = it->second.cls;
        fs = curr_class->getFeatures();

        for(int i = fs->first(); fs->more(i); i = fs->next(i)) {
            f = fs->nth(i);
            if(f->isMethod()) {
                //f = dynamic_cast<method_class*>(f);
                // add method to table
                log << "        - Adding method " << f->getName() << endl;
                curr_mt->addid(f->getName(), new method_class(f->getName(), ((method_class*)(f))->getFormals()->copy_list(),\
                    f->getType(), f->getExpr()->copy_Expression()));
            }
        }
    }

    Formals curr_formals;
    Formal fml;
    std::list<Symbol> path;
    log << "Fourth pass: checking method definition..." << endl;
    for(std::map<Symbol, ClassName>::iterator it = classtable->className.begin(); it != classtable->className.end(); it++) {
        log << "    - Checking class " << it->first << endl;
        curr_class = it->second.cls;
        fs = curr_class->getFeatures();
        curr_mt = it->second.methodTable;
        
        for(int i = fs->first(); fs->more(i); i = fs->next(i)) {
            f = fs->nth(i);
            if(f->isMethod()) {
                log << "        - Checking method " << f->getName() << endl;
                curr_formals = ((method_class*)(f))->getFormals();
                std::set<Symbol> formalNames;
                formalNames.clear();
                // check formal in each method
                for(int j = curr_formals->first(); curr_formals->more(j); j = curr_formals->next(j)) {
                    fml = curr_formals->nth(j);
                    // check if self is the name of some formal parameter
                    if(fml->getName() == self) {
                        classtable->semant_error(curr_class) << "Error: self as name in method's formal list" << endl;
                    }
                    // check if duplicated formal name
                    if(formalNames.find(fml->getName()) != formalNames.end()) {
                        classtable->semant_error(curr_class) << "Error: duplicated name in method's formal list" << endl;
                    }
                    else {
                        formalNames.insert(fml->getName());
                    }
                    // check if type of parameter in formal list undefined
                    if(classtable->className.find(fml->getType()) == classtable->className.end()) {
                        classtable->semant_error(curr_class) << "Error: undefined type of parameter in method's formal list" << endl;
                    }
                }
                // check illegal method overridden
                // check every method with the same name in its ancestry class has same return type, parameter #, and parameter type
                // path start from curr_class's parent to Object
                path = classtable->getInheritancePath(curr_class->getName());
                path.pop_front();
                for(std::list<Symbol>::iterator it = path.begin(); it != path.end(); it++) {
                    Symbol name = *it;
                    log << "            - Checking ancestor " << name << endl;
                    curr_mt = classtable->className[name].methodTable;

                    method_class *method = curr_mt->probe(f->getName());
                    if(method != NULL) {
                        // method found in ancestor
                        Formals ancestor_formals = method->getFormals();
                        int j1 = curr_formals->first();
                        int j2 = ancestor_formals->first();
                        while(curr_formals->more(j1) && ancestor_formals->more(j2)) {
                            // check if the type of each parameter of the two methods matches
                            if(curr_formals->nth(j1)->getType() != ancestor_formals->nth(j2)->getType()) {
                                classtable->semant_error(curr_class) << "Error: unmatched formal type in inherited method" << endl;
                            }
                            j1 = curr_formals->next(j1);
                            j2 = ancestor_formals->next(j2);
                        }
                        // check if the number of parameters of the two methods matches
                        if(curr_formals->more(j1) || ancestor_formals->more(j2)) {
                            classtable->semant_error(curr_class) << "Error: unmatched formal number in inherited method" << endl;
                        }
                        // check if the return type of the two methods matches
                        if(f->getType() != method->getType()) {
                            classtable->semant_error(curr_class) << "Error: unmatched return type in inherited method" << endl;
                        }
                    }
                }
            }
        }
    }

    AttrTable *curr_at;
    log << "Fifth pass: adding attribute definition into table..." << endl;
    for(std::map<Symbol, ClassName>::iterator it = classtable->className.begin(); it != classtable->className.end(); it++) {
        log << "    - checking class " << it->first << endl;
        // have to add attribute of all parent classes to attrTable
        curr_class = it->second.cls;
        curr_at = it->second.attrTable;
        path = classtable->getInheritancePath(curr_class->getName());
        // add attribute into attrTable from ancestor to curr_class
        for(std::list<Symbol>::reverse_iterator it = path.rbegin(); it != path.rend(); it++) {
            classtable->addAttrTable(*it, curr_at);
        }
    }

    log << "Sixth pass: checking type definition..." << endl;
    for(std::map<Symbol, ClassName>::iterator it = classtable->className.begin(); it != classtable->className.end(); it++) {
        log << "    - checking class " << it->first << endl;
        Symbol name = it->first;
        curr_class = it->second.cls;
        fs = curr_class->getFeatures();
        for(int i = fs->first(); fs->more(i); i = fs->next(i)) {
            fs->nth(i)->checkFeatureType();
        }
    }


    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

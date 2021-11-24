#ifndef AST_C
#define AST_C

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef enum {
	AST_NODE_TYPE_LEX,
	AST_NODE_TYPE_LEXS,
	AST_NODE_TYPE_BOOL,

	AST_NODE_TYPE_NAME,
	AST_NODE_TYPE_NAMES,
	
	AST_NODE_TYPE_IMPORT,
	AST_NODE_TYPE_IMPORTS,
	
	AST_NODE_TYPE_PKG,
	
	AST_NODE_TYPE_COMPIL,
	
	AST_NODE_TYPE_CONST_MOD,
	AST_NODE_TYPE_ANN,
	AST_NODE_TYPE_MOD,
	AST_NODE_TYPE_MODS,
	
	AST_NODE_TYPE_TYPE_PARAM,
	AST_NODE_TYPE_TYPE_PARAMS,
	
	AST_NODE_TYPE_CLASS,
	AST_NODE_TYPE_NORM_CLASS,
	
	AST_NODE_TYPE_INTER,
	
	AST_NODE_TYPE_CLASS_OR_INTER,
	AST_NODE_TYPE_CLASS_OR_INTERS,
	
	AST_NODE_TYPE_MEMBER,
	AST_NODE_TYPE_INIT,
	AST_NODE_TYPE_METHOD,
	AST_NODE_TYPE_FIELD,
	AST_NODE_TYPE_CONSTR,
	AST_NODE_TYPE_MEMBERS
} ast_node_type;

typedef struct {
	char* str;
	int len;
} ast_lex;

typedef struct {
	ast_lex** arr;
	int len;
} ast_lexs;

typedef struct {
	ast_lex** ids;
	int len;
} ast_name;

typedef struct {
	bool is_static;
	bool is_star;
	ast_name* name;
} ast_import;

typedef struct {
	ast_import** imports;
	int len;
} ast_imports;

typedef struct {
	ast_name* name;
} ast_pkg;

typedef enum {
	AST_CONST_MOD_PUBLIC, AST_CONST_MOD_PROTECTED, AST_CONST_MOD_PRIVATE, AST_CONST_MOD_ABSTRACT, AST_CONST_MOD_STATIC, AST_CONST_MOD_FINAL, AST_CONST_MOD_STRICTFP, AST_CONST_MOD_SEALED, AST_CONST_MOD_NON_SEALED
} ast_const_mod;

typedef struct {
	ast_name* name;
	bool has_elems;
} ast_ann;

typedef enum { AST_MOD_TYPE_CONST, AST_MOD_TYPE_ANN } ast_mod_type;

typedef struct {
	ast_mod_type type;
	union {
		ast_const_mod cnst;
		ast_ann* ann;
	} mod;
} ast_mod;

typedef struct {
	ast_mod** mods;
	int len;
} ast_mods;

typedef struct {
	ast_lex* id;
	bool has_type_bound;
} ast_type_param;

typedef struct {
	ast_type_param** type_params;
	int len;
} ast_type_params;

typedef struct {
	ast_name** names;
	int len;
} ast_extends;

typedef struct {
	ast_name** names;
	int len;
} ast_impl;

typedef struct {
	ast_name** arr;
	int len;
} ast_names;

typedef enum { AST_MEMBER_TYPE_INIT, AST_MEMBER_TYPE_METHOD, AST_MEMBER_TYPE_FIELD, AST_MEMBER_TYPE_CONSTR } ast_member_type;

typedef struct {

} ast_init;

typedef struct {
	ast_lex* id;
} ast_method;

typedef struct {
	ast_lex* id;
} ast_field;

typedef struct {

} ast_constr;

typedef struct {
	ast_member_type type;
	union {
		ast_init* init;
		ast_method* method;
		ast_field* field;
		ast_constr* constr;
	} val;
} ast_member;

typedef struct {
	ast_member** arr;
	int len;
} ast_members;

typedef struct {
	ast_mods* mods;
	ast_lex* id;
	ast_type_params* type_params;
	ast_name* extends;
	ast_names* impl;
	ast_names* permits;
	ast_members* members;
} ast_norm_class;

typedef enum { AST_CLASS_TYPE_NORM, AST_CLASS_TYPE_ENUM, AST_CLASS_TYPE_RECORD } ast_class_type;

typedef struct {
	ast_class_type type;
	union {
		ast_norm_class* norm;
	} val;
} ast_class;

typedef enum { AST_INTER_TYPE_NORM, AST_INTER_TYPE_ANN } ast_inter_type;

typedef struct {
	ast_inter_type type;
} ast_inter;

typedef enum { AST_CLASS_OR_INTER_TYPE_CLASS, AST_CLASS_OR_INTER_TYPE_INTER } ast_class_or_inter_type;

typedef struct {
	ast_class_or_inter_type type;
	union {
		ast_class* class;
		ast_inter* inter;
	} val;
} ast_class_or_inter;

typedef struct {
	ast_class_or_inter** arr;
	int len;
} ast_class_or_inters;

typedef struct {
	ast_pkg* pkg;
	ast_imports* imports;
	ast_class_or_inters* class_or_inters;
} ast_compil;

typedef struct {
	ast_node_type type;
	union {
		ast_lex* lex;
		ast_lexs* lexs;
		bool bol;
		
		ast_name* name;
		ast_names* names;
		
		ast_import* import;
		ast_imports* imports;
		
		ast_pkg* pkg;
		
		ast_compil* compil;
		
		ast_ann* ann;
		ast_const_mod const_mod;
		ast_mod* mod;
		ast_mods* mods;
		
		ast_type_param* type_param;
		ast_type_params* type_params;
		
		ast_class* class;
		ast_norm_class* norm_class;
		
		ast_inter* inter;
		
		ast_class_or_inter* class_or_inter;
		ast_class_or_inters* class_or_inters;
		
		ast_member* member;
		ast_init* init;
		ast_field* field;
		ast_method* method;
		ast_constr* constr;
		ast_members* members;
	} val;
} ast_node;

void ast_print_indent(int indent) {
	int i;
	for (i = 0; i < indent; i++) {
		printf("\t");
	}
}

void ast_print_bool(bool bol, int indent) {
	ast_print_indent(indent);
	if (bol) printf("true");
	else printf("false");
}

void ast_print_lex(ast_lex* lex, int indent) {
	ast_print_indent(indent);
	printf("%s", lex->str);
}

void ast_print_lexs(ast_lexs* lexs, int indent) {
	int i;
	for (i = 0; i < lexs->len; i++) {
		ast_lex* lex = lexs->arr[i];
		ast_print_lex(lex, indent);
		if (i != lexs->len-1) printf("\n");
	}
}

void ast_print_name(ast_name* name, int indent) {
	ast_print_indent(indent);
	int i;
	for (i = 0; i < name->len; i++) {
		ast_lex* lex = name->ids[i];
		ast_print_lex(lex, 0);
		if (i != name->len-1) {
			printf(".");
		}
	}
}

void ast_print_import(ast_import* import, int indent) {
	ast_print_indent(indent);
	printf("import ");
	
	bool is_static = import->is_static;
	bool is_star = import->is_star;
	ast_name* name = import->name;
	
	if (is_static) printf("static ");
	ast_print_name(name, 0);
	if (is_star) printf(".*");
}

void ast_print_imports(ast_imports* imports, int indent) {
	ast_print_indent(indent);
	printf("imports");
	
	if (imports->len == 0) {
		printf(" -");
	} else {
		int i;
		for (i = 0; i < imports->len; i++) {
			ast_import* import = imports->imports[i];
			printf("\n");
			
			ast_print_indent(indent+1);
			if (import->is_static) printf("static ");
			ast_print_name(import->name, 0);
			if (import->is_star) printf(".*");
		}
	}
}

void ast_print_pkg(ast_pkg* pkg, int indent) {
	ast_print_indent(indent);
	printf("package ");
	
	if (pkg->name == NULL) {
		printf("-");
	} else {
		ast_print_name(pkg->name, 0);
	}
}

void ast_print_const_mod(ast_const_mod mod, int indent) {
	ast_print_indent(indent);
	switch (mod) {
		case AST_CONST_MOD_PUBLIC:
			printf("public");
			break;
		case AST_CONST_MOD_PROTECTED:
			printf("protected");
			break;
		case AST_CONST_MOD_PRIVATE:
			printf("private");
			break;
		case AST_CONST_MOD_ABSTRACT:
			printf("abstract");
			break;
		case AST_CONST_MOD_STATIC:
			printf("static");
			break;
		case AST_CONST_MOD_FINAL:
			printf("final");
			break;
		case AST_CONST_MOD_STRICTFP:
			printf("strictfp");
			break;
		case AST_CONST_MOD_SEALED:
			printf("sealed");
			break;
		case AST_CONST_MOD_NON_SEALED:
			printf("non-sealed");
			break;
	}
}

void ast_print_ann(ast_ann* ann, int indent) {
	ast_print_indent(indent);
	printf("@");
	ast_print_name(ann->name, 0);
	if (ann->has_elems) {
		printf("(...)");
	}
}

void ast_print_mod(ast_mod* mod, int indent) {
	switch (mod->type) {
		case AST_MOD_TYPE_CONST:
			ast_print_const_mod(mod->mod.cnst, indent);
			break;
		case AST_MOD_TYPE_ANN:
			ast_print_ann(mod->mod.ann, indent);
			break;
	}
}

void ast_print_mods(ast_mods* mods, int indent) {
	ast_print_indent(indent);
	printf("modifiers");
	
	if (mods->len == 0) {
		printf(" -");
	} else {
		int i;
		for (i = 0; i < mods->len; i++) {
			ast_mod* mod = mods->mods[i];
			printf("\n");
			ast_print_mod(mod, indent+1);
		}
	}
}

void ast_print_type_param(ast_type_param* type_param, int indent) {
	ast_print_lex(type_param->id, indent);
	
	if (type_param->has_type_bound) {
		printf(" extends ...");
	}
}

void ast_print_type_params(ast_type_params* type_params, int indent) {
	ast_print_indent(indent);
	printf("type parameters");
	
	if (type_params->len == 0) {
		printf(" -");
	} else {
		int i;
		for (i = 0; i < type_params->len; i++) {
			ast_type_param* type_param = type_params->type_params[i];
			printf("\n");
			ast_print_type_param(type_param, indent+1);
		}
	}
}

void ast_print_names(ast_names* names, int indent) {
	int i;
	for (i = 0; i < names->len; i++) {
		ast_name* name = names->arr[i];
		ast_print_name(name, indent);
		if (i != names->len-1) printf("\n");	
	}
}

void ast_print_init(ast_init* init, int indent) {
	ast_print_indent(indent);
	printf("initializer ...");
}

void ast_print_method(ast_method* method, int indent) {
	ast_print_indent(indent);
	printf("method ");
	ast_print_lex(method->id, 0);
	printf(" ...");
}

void ast_print_field(ast_field* field, int indent) {
	ast_print_indent(indent);
	printf("field ...");
}

void ast_print_constr(ast_constr* constr, int indent) {
	ast_print_indent(indent);
	printf("constructor ...");
}

void ast_print_member(ast_member* member, int indent) {
	switch (member->type) {
		case AST_MEMBER_TYPE_INIT:
			ast_print_init(member->val.init, indent);
			break;
		case AST_MEMBER_TYPE_METHOD:
			ast_print_method(member->val.method, indent);
			break;
		case AST_MEMBER_TYPE_FIELD:
			ast_print_field(member->val.field, indent);
			break;
		case AST_MEMBER_TYPE_CONSTR:
			ast_print_constr(member->val.constr, indent);
			break;
	}
}

void ast_print_members(ast_members* members, int indent) {
	int i;
	for (i = 0; i < members->len; i++) {
		ast_member* member = members->arr[i];
		ast_print_member(member, indent);
		if (i != members->len-1) printf("\n");
	}
}

void ast_print_norm_class(ast_norm_class* norm_class, int indent) {
	ast_print_indent(indent);
	printf("class ");
	
	ast_print_lex(norm_class->id, 0);
	
	if (norm_class->mods->len > 0) {
		printf("\n");
		ast_print_mods(norm_class->mods, indent+1);
	}
	
	if (norm_class->type_params->len > 0) {
		printf("\n");
		ast_print_type_params(norm_class->type_params, indent+1);
	}
	
	if (norm_class->extends->len > 0) {
		printf("\n");
		ast_print_indent(indent+1);
		printf("extends ");
		ast_print_name(norm_class->extends, 0);
	}
	
	if (norm_class->impl->len > 0) {
		printf("\n");
		ast_print_indent(indent+1);
		printf("implements\n");
		ast_print_names(norm_class->impl, indent+2);
	}
	
	if (norm_class->permits->len > 0) {
		printf("\n");
		ast_print_indent(indent+1);
		printf("permits\n");
		ast_print_names(norm_class->permits, indent+2);
	}
	
	if (norm_class->members->len > 0) {
		printf("\n");
		ast_print_indent(indent+1);
		printf("members\n");
		ast_print_members(norm_class->members, indent+2);
	}
}

void ast_print_class(ast_class* class, int indent) {
	switch (class->type) {
		case AST_CLASS_TYPE_NORM:
			ast_print_norm_class(class->val.norm, indent);
			break;
		case AST_CLASS_TYPE_ENUM:
			ast_print_indent(indent);
			printf("enum ...");
			break;
		case AST_CLASS_TYPE_RECORD:
			ast_print_indent(indent);
			printf("record ...");
			break;
	}
}

void ast_print_inter(ast_inter* inter, int indent) {
	switch (inter->type) {
		case AST_INTER_TYPE_NORM:
			ast_print_indent(indent);
			printf("interface ...");
			break;
		case AST_INTER_TYPE_ANN:
			ast_print_indent(indent);
			printf("@interface ...");
			break;
	}
}

void ast_print_class_or_inter(ast_class_or_inter* class_or_inter, int indent) {
	switch (class_or_inter->type) {
		case AST_CLASS_OR_INTER_TYPE_CLASS:
			ast_print_class(class_or_inter->val.class, indent);
			break;
		case AST_CLASS_OR_INTER_TYPE_INTER:
			ast_print_inter(class_or_inter->val.inter, indent);
			break;
	}
}

void ast_print_class_or_inters(ast_class_or_inters* cis, int indent) {
	if (cis->len == 0) printf(" -");
	else {
		int i;
		for (i = 0; i < cis->len; i++) {
			ast_class_or_inter* ci = cis->arr[i];
			ast_print_class_or_inter(ci, indent);
			if (i != cis->len-1) printf("\n");
		}
	}
}

void ast_print_compil(ast_compil* compil, int indent) {
	if (compil->pkg->name != NULL) {
		ast_print_pkg(compil->pkg, indent);
	}
	
	if (compil->imports->len > 0) {
		printf("\n");
		ast_print_imports(compil->imports, indent);
	}
	
	if (compil->class_or_inters->len > 0) {
		printf("\n");
		ast_print_class_or_inters(compil->class_or_inters, indent);
	}
}

void ast_print_node(ast_node* node, int indent) {
	switch (node->type) {
		case AST_NODE_TYPE_NAME: {
			ast_name* name = node->val.name;
			ast_print_name(name, indent);
			break;
		}
		case AST_NODE_TYPE_IMPORT: {
			ast_import* import = node->val.import;
			ast_print_import(import, indent);
			break;
		}
		case AST_NODE_TYPE_IMPORTS: {
			ast_imports* imports = node->val.imports;
			ast_print_imports(imports, indent);
			break;
		}
		case AST_NODE_TYPE_LEX: {
			ast_lex* lex = node->val.lex;
			ast_print_lex(lex, indent);
			break;
		}
		case AST_NODE_TYPE_PKG: {
			ast_pkg* pkg = node->val.pkg;
			ast_print_pkg(pkg, indent);
			break;
		}
		case AST_NODE_TYPE_COMPIL: {
			ast_compil* compil = node->val.compil;
			ast_print_compil(compil, indent);
			break;
		}
		case AST_NODE_TYPE_BOOL: {
			ast_print_bool(node->val.bol, indent);
			break;
		}
		case AST_NODE_TYPE_ANN: {
			ast_ann* ann = node->val.ann;
			ast_print_ann(ann, indent);
			break;
		}
		case AST_NODE_TYPE_CONST_MOD: {
			ast_print_const_mod(node->val.const_mod, indent);
			break;
		}
		case AST_NODE_TYPE_MOD: {
			ast_print_mod(node->val.mod, indent);
			break;
		}
		case AST_NODE_TYPE_MODS: {
			ast_print_mods(node->val.mods, indent);
			break;
		}
		case AST_NODE_TYPE_TYPE_PARAM: {
			ast_print_type_param(node->val.type_param, indent);
			break;
		}
		case AST_NODE_TYPE_TYPE_PARAMS: {
			ast_print_type_params(node->val.type_params, indent);
			break;
		}
		case AST_NODE_TYPE_NAMES: {
			ast_print_names(node->val.names, indent);
			break;
		}
		case AST_NODE_TYPE_NORM_CLASS: {
			ast_print_norm_class(node->val.norm_class, indent);
			break;
		}
		case AST_NODE_TYPE_CLASS:
			ast_print_class(node->val.class, indent);
			break;
		case AST_NODE_TYPE_CLASS_OR_INTER:
			ast_print_class_or_inter(node->val.class_or_inter, indent);
			break;
		case AST_NODE_TYPE_CLASS_OR_INTERS:
			ast_print_class_or_inters(node->val.class_or_inters, indent);
			break;
		case AST_NODE_TYPE_INTER:
			ast_print_inter(node->val.inter, indent);
			break;
		case AST_NODE_TYPE_MEMBER:
			ast_print_member(node->val.member, indent);
			break;
		case AST_NODE_TYPE_INIT:
			ast_print_init(node->val.init, indent);
			break;
		case AST_NODE_TYPE_FIELD:
			ast_print_field(node->val.field, indent);
			break;
		case AST_NODE_TYPE_METHOD:
			ast_print_method(node->val.method, indent);
			break;
		case AST_NODE_TYPE_CONSTR:
			ast_print_constr(node->val.constr, indent);
			break;
		case AST_NODE_TYPE_MEMBERS:
			ast_print_members(node->val.members, indent);
			break;
		case AST_NODE_TYPE_LEXS:
			ast_print_lexs(node->val.lexs, indent);
			break;
	}
}

ast_node* ast_mk_node(ast_node_type type) {
	ast_node* node = malloc(sizeof(ast_node));
	node->type = type;
	return node;
}

ast_lex* ast_mk_lex(char* str, int len) {
	ast_lex* lex = malloc(sizeof(ast_lex));
	lex->len = len;
	lex->str = str;
	return lex;
}

ast_lexs* ast_mk_lexs() {
	ast_lexs* lexs = malloc(sizeof(ast_lexs));
	lexs->arr = NULL;
	lexs->len = 0;
	return lexs;
}

ast_name* ast_mk_name() {
	ast_name* name = malloc(sizeof(ast_name));
	name->ids = NULL;
	name->len = 0;
	return name;
}

ast_import* ast_mk_import(bool is_static, bool is_star, ast_name* name) {
	ast_import* import = malloc(sizeof(ast_import));
	import->is_static = is_static;
	import->is_star = is_star;
	import->name = name;
	return import;
}

ast_imports* ast_mk_imports() {
	ast_imports* imports = malloc(sizeof(ast_imports));
	imports->imports = NULL;
	imports->len = 0;
	return imports;
}

ast_pkg* ast_mk_pkg(ast_name* name) {
	ast_pkg* pkg = malloc(sizeof(ast_pkg));
	pkg->name = name;
	return pkg;
}

ast_compil* ast_mk_compil(ast_pkg* pkg, ast_imports* imports, ast_class_or_inters* cis) {
	ast_compil* compil = malloc(sizeof(ast_compil));
	compil->pkg = pkg;
	compil->imports = imports;
	compil->class_or_inters = cis;
	return compil;
}

ast_ann* ast_mk_ann(ast_name* name, bool has_elems) {
	ast_ann* ann = malloc(sizeof(ast_ann));
	ann->name = name;
	ann->has_elems = has_elems;
	return ann;
}

ast_mod* ast_mk_mod(ast_mod_type type) {
	ast_mod* mod = malloc(sizeof(ast_mod));
	mod->type = type;
	return mod;
}

ast_mods* ast_mk_mods() {
	ast_mods* mods = malloc(sizeof(ast_mods));
	mods->mods = NULL;
	mods->len = 0;
	return mods;
}

ast_type_param* ast_mk_type_param(ast_lex* id, bool has_type_bound) {
	ast_type_param* type_param = malloc(sizeof(ast_type_param));
	type_param->id = id;
	type_param->has_type_bound = has_type_bound;
	return type_param;
}

ast_type_params* ast_mk_type_params() {
	ast_type_params* type_params = malloc(sizeof(ast_type_params));
	type_params->type_params = NULL;
	type_params->len = 0;
	return type_params;
}

ast_names* ast_mk_names() {
	ast_names* names = malloc(sizeof(ast_names));
	names->arr = NULL;
	names->len = 0;
	return names;
}

ast_norm_class* ast_mk_norm_class(ast_mods* mods, ast_lex* id, ast_type_params* type_params, ast_name* extends, ast_names* impl, ast_names* permits, ast_members* members) {
	ast_norm_class* norm_class = malloc(sizeof(ast_norm_class));
	norm_class->mods = mods;
	norm_class->id = id;
	norm_class->type_params = type_params;
	norm_class->extends = extends;
	norm_class->impl = impl;
	norm_class->permits = permits;
	norm_class->members = members;
	return norm_class;
}

ast_class* ast_mk_class(ast_class_type type) {
	ast_class* class = malloc(sizeof(ast_class));
	class->type = type;
	return class;
}

ast_class_or_inter* ast_mk_class_or_inter(ast_class_or_inter_type type) {
	ast_class_or_inter* class_or_inter = malloc(sizeof(ast_class_or_inter));
	class_or_inter->type = type;
	return class_or_inter;
}

ast_class_or_inters* ast_mk_class_or_inters() {
	ast_class_or_inters* cis = malloc(sizeof(ast_class_or_inters));
	cis->arr = NULL;
	cis->len = 0;
	return cis;
}

ast_inter* ast_mk_inter(ast_inter_type type) {
	ast_inter* inter = malloc(sizeof(ast_inter));
	inter->type = type;
	return inter;
}

ast_member* ast_mk_member(ast_member_type type) {
	ast_member* member = malloc(sizeof(ast_member));
	member->type = type;
	return member;
}

ast_init* ast_mk_init() {
	ast_init* init = malloc(sizeof(ast_init));
	return init;
}

ast_method* ast_mk_method(ast_lex* id) {
	ast_method* method = malloc(sizeof(ast_method));
	method->id = id;
	return method;
}

ast_field* ast_mk_field() {
	ast_field* field = malloc(sizeof(ast_field));
	return field;
}

ast_constr* ast_mk_constr() {
	ast_constr* constr = malloc(sizeof(ast_constr));
	return constr;
}

ast_members* ast_mk_members() {
	ast_members* members = malloc(sizeof(ast_members));
	members->arr = NULL;
	members->len = 0;
	return members;
}
#endif

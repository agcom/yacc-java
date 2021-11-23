#ifndef AST_C
#define AST_C

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef enum {
	AST_NODE_TYPE_LEX,
	AST_NODE_TYPE_BOOL,

	AST_NODE_TYPE_NAME,
	
	AST_NODE_TYPE_IMPORT,
	AST_NODE_TYPE_IMPORTS,
	
	AST_NODE_TYPE_PKG,
	
	AST_NODE_TYPE_COMPIL,
	
	AST_NODE_TYPE_CONST_MOD,
	AST_NODE_TYPE_ANN,
	AST_NODE_TYPE_MOD,
	AST_NODE_TYPE_MODS
} ast_node_type;

typedef struct {
	char* str;
	int len;
} ast_lex;

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

typedef struct {
	ast_pkg* pkg;
	ast_imports* imports;
	// TODO: classes and interfaces
} ast_compil;

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
	ast_node_type type;
	union {
		ast_lex* lex;
		bool bol;
		
		ast_name* name;
		
		ast_import* import;
		ast_imports* imports;
		
		ast_pkg* pkg;
		
		ast_compil* compil;
		
		ast_ann* ann;
		ast_const_mod const_mod;
		ast_mod* mod;
		ast_mods* mods;
	} val;
} ast_node;

ast_node* ast_mk_node(ast_node_type type) {
	ast_node* node = malloc(sizeof(ast_node));
	node->type = type;
	return node;
}

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

void ast_print_compil(ast_compil* compil, int indent) {
	ast_print_pkg(compil->pkg, indent);
	printf("\n");
	ast_print_imports(compil->imports, indent);
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
	}
}

ast_lex* ast_mk_lex(char* str, int len) {
	ast_lex* lex = malloc(sizeof(ast_lex));
	lex->len = len;
	lex->str = str;
	return lex;
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

ast_compil* ast_mk_compil(ast_pkg* pkg, ast_imports* imports) {
	ast_compil* compil = malloc(sizeof(ast_compil));
	compil->pkg = pkg;
	compil->imports = imports;
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
#endif

#include <string.h>
#include <stdlib.h>

typedef struct node node;

struct node {
	char* lexeme; // Should contain null-terminated string.
	int n;
	node* children;
};

node* mknd(char* lexeme, int n, node* children) {
	node* nd = malloc(sizeof(node));
	
	nd->lexeme = lexeme;
	nd->n = n;
	nd->children = children;
	
	return nd;
}

void rmnd(node* nd) {
	for (int i = 0; i < nd->n; i++) {
		node* child = &(nd->children[i]);
		rmnd(child);
	}
	
	free(nd->lexeme);
	free(nd->children);
	free(nd);
}

void tree(node* node) {
	// TODO
}

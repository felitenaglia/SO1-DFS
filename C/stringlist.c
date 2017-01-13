#include "stringlist.h"
#include <string.h>
#include <stdlib.h>

struct _StrNode {
	char* data;
	struct _StrNode* next;
};

StrList* stringlist_add(StrList* l, char* cadena, int* res)
{
	if (cadena == NULL)
	{
		if (res) *res = -1;
		return l;
	}	
	StrList *new = (StrList*) malloc(sizeof(StrList));
	
	if (new == NULL)
	{
		if (res) *res = -1;
		return l;
	}	
	
	new->data = (char*) malloc(sizeof(char)*(strlen(cadena)+1));
	
	if(new->data == NULL){
		free(new);
		if (res) *res = -1;
		return l;
	}
	strcpy(new->data, cadena);
	new->next = l;
	if (res) *res = 0;
	
	return new;	
}

StrList* stringlist_remove(StrList* lista, char* cadena)
{
	StrList *it, *aux;

	if (lista == NULL || cadena == NULL)
		return lista;
	
	if (!strcmp(lista->data, cadena))
	{
		aux = lista->next;
		free(lista->data);
		free(lista);
		return aux;
	}

	for(it = lista; it->next != NULL; it = it->next)
	{
		if (!strcmp(it->next->data, cadena))
		{
			aux = it->next;
			it->next = aux->next;
			free(aux->data);
			free(aux);
			return lista;
		}
	}

	return lista;
}

int stringlist_find(StrList* lista, char* cadena)
{
	if (lista == NULL || cadena == NULL)
		return 0;
	
	for(; lista != NULL; lista = lista->next)
		if (!strcmp(lista->data, cadena))
			return 1;

	return 0;
}

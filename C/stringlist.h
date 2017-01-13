#ifndef __STRLIST__
#define __STRLIST__

typedef struct _StrNode StrList;

/* Agrega la 'cadena' a la lista 'lista'. 
 * La cadena debe terminar con \0, se almacena una copia
 * Devuelve la nueva lista.
 * Establece el entero apuntado pore res en 0 si se añadió satisfactoriamente
 * o en -1 si no hay memoria suficiente, o si cadena == NULL */
StrList* stringlist_add(StrList* lista, char* cadena, int *res);

/* Elimina la cadena 'cadena' de la lista 'lista'. 
 * Devuelve la nueva lista */
StrList* stringlist_remove(StrList* lista, char* cadena);

/* Devuelve 1 si 'cadena' pertenece a 'lista', si no, devuelve 0 */
int stringlist_find(StrList* lista, char* cadena);

#endif

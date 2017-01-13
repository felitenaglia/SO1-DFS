#ifndef __FLIST__
#define __FLIST__

typedef struct _FileList FileList;
typedef struct _FileNode FileNode;

#define FILENAME_LEN 50

/* Crea una nueva lista de archivos
 * Si 'dir' no es nulo, la lista contendrá los archivos de ese directorio (si el directorio no existe, lo crea)
 * Si es nulo, se crea una lista vacía
 */
FileList* filelist_new(char* dir);

/* Agrega el archivo 'nombre' a la lista 'lista'.
 * Si lo agregó, devuelve 0
 * Si el nombre de archivo es mas largo al permitido (o es NULL), o se produce otro error devuelve -1
 * Si el archivo ya existe, devuelve -2
 */
int filelist_add(FileList* lista, char* nombre);

/* Elimina el archivo 'nombre' de la lista 'lista'. 
 * Si lo eliminó, devuelve 0
 * Si el archivo no existe, devuelve -1
 * Si el archivo esta abierto, devuelve -2
 * Si se produce un error al eliminar el archivo del disco rígido, devuelve -3
 */
int filelist_remove(FileList* lista, char* nombre);

/* Busca el archivo 'nombre' en la lista 'lista'.
 * Si no existe devuelve NULL
 * Si existe, devuelve un puntero al nodo
 */
FileNode* filelist_find(FileList* lista, char* nombre);

/* Abre el archivo 'nombre' de la 'lista'
 * Si existe y lo pudo abrir, devuelve un puntero al nodo
 * Si hubo un error devuelve NULL y setea res con el tipo de error:
 * res = -1 : El archivo no existe
 * res = -2 : El archivo esta abierto
 * res = -3 : Error al abrir el archivo (falló open(...) )
 */
FileNode* filelist_open(FileList* lista, char* nombre, int* res);

/* Si el archivo 'archivo' esta abierto, lo cierra
 * Devuelve 0 si terminó correctamente
 * Si hubo un error (i.e.: El archivo estaba cerrado) devuelve -1
 */
int filelist_close(FileNode* archivo);

/* Lee 'cant' bytes del 'archivo' y lo almacena en 'buff'
 * Devuelve la cantidad de bytes leidos con exito o los siguientes valores de error:
 * -1 : El archivo no es válido o hubo un error E/S
 * -2 : El archivo estaba cerrado
 */
int filelist_read(FileNode* archivo, char* buff, unsigned int cant);

/* Escribe 'cant' bytes desde la direccion apuntada por 'buff' en el 'archivo'
 * Devuelve la cantidad de bytes escritos con exito o los siguientes valores de error:
 * -1 : El archivo no es válido o hubo un error E/S
 * -2 : El archivo estaba cerrado
 */
int filelist_write(FileNode* archivo, char* buff, unsigned int cant);

/* Devuelve una cadena con todos los nombres de los archivos, separados por espacios */
char* filelist_concatnames(FileList* list);

#endif

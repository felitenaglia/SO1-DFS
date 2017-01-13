#include "filelist.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>

struct _FileNode {
	char name[FILENAME_LEN];	// El nombre de archivo
	int fd;						// -1 si está cerrado, si es mayor a -1, indica el nro de descriptor en el sistema de archivos real
	int pos;					// Indica la posición de lectura
	struct _FileNode* next;
};

struct _FileList {
	int dirfd; 					// Indica en que directorio estan los archivos de la lista
	struct _FileNode* ls;		// Lista de archivos
};

// Función interna: Agrega a la lista sin verificar si existe
static int filelist_add_notverify(FileList* l, char* nombre)
{
	if (l == NULL || nombre == NULL || strlen(nombre) >= FILENAME_LEN)
		return -1;
		
	FileNode *new = (FileNode*) malloc(sizeof(FileNode));
	
	if (new == NULL)
		return -1;
	
	new->fd = -1;
	new->pos = 0;
	strcpy(new->name, nombre);
	new->next = l->ls;
	l->ls = new;
	
	return 0;
}

FileList* filelist_new(char* dir)
{
	int dir_desc;
	DIR* dir_ptr;
	struct dirent *entry;
	FileList* newlist = (FileList*) malloc(sizeof(FileList));
	
	if (newlist == NULL)
		return NULL;
	
	newlist->ls = NULL;
	newlist->dirfd = AT_FDCWD; //Por defecto, paths relativas a CWD
	if (dir != NULL)
	{
		mkdir(dir,0755); 
		if ((dir_desc = open(dir, O_RDONLY | O_DIRECTORY)) > -1)
		{
			newlist->dirfd = dir_desc;
			dir_ptr = fdopendir(dir_desc);
			while((entry = readdir(dir_ptr)))
			{
				if (entry->d_name[0] != '.')
					if (filelist_add_notverify(newlist, entry->d_name) == -1)
					{
						fprintf(stderr, "Error al cargar los archivos desde el directorio\n");
						exit(0);
					}
			}
		}else{
			fprintf(stderr, "Error al acceder al directorio %s\n", dir);
			exit(0);			
		}
	}
	return newlist;
}

int filelist_add(FileList* lista, char* nombre)
{
	int cr_fd;

	if (lista == NULL || nombre == NULL || strlen(nombre) >= FILENAME_LEN)
		return -1;

	if (filelist_find(lista, nombre) != NULL)
		return -2;
	
	cr_fd = openat(lista->dirfd, nombre, O_CREAT | O_WRONLY | O_TRUNC, 0664);
	
	if (cr_fd == -1)
		return -1;
	
	close(cr_fd);
	return filelist_add_notverify(lista, nombre);
}

int filelist_remove(FileList* lista, char* nombre)
{
	FileNode *it, *aux;

	if (lista == NULL || lista->ls == NULL || nombre == NULL)
		return -1;
	
	it = lista->ls;

	if (!strcmp(it->name, nombre))
	{
		if(it->fd == -1)
		{
			if (unlinkat(lista->dirfd, nombre,0) == 0)
			{
				lista->ls = it->next;
				free(it);
				return 0;
			}else{
				return -3;
			}
		}else{
			return -2;
		}
	}

	for(; it->next != NULL; it = it->next)
	{
		if (!strcmp(it->next->name, nombre))
		{
			if(it->next->fd == -1)
			{
				if (unlinkat(lista->dirfd, nombre, 0) == 0)
				{
					aux = it->next;
					it->next = aux->next;
					free(aux);
					return 0;
				}else{
					return -3;
				}
			}else{
				return -2;
			}
		}
	}

	return -1;
}

FileNode* filelist_find(FileList* lista, char* nombre)
{
	FileNode *it;
	
	if (lista == NULL || nombre == NULL)
		return NULL;

	for(it = lista->ls; it != NULL; it = it->next)
		if (!strcmp(it->name, nombre))
			return it;

	return NULL;
}

FileNode* filelist_open(FileList* lista, char* nombre, int* res)
{
	FileNode *archivo;
	if (lista == NULL || nombre == NULL)
	{
		if (res) *res = -1;
		return NULL;
	}
	archivo = filelist_find(lista, nombre);
	
	if (archivo == NULL)
	{
		if (res) *res = -1;
		return NULL;
	}

	if (archivo->fd > -1)
	{
		if (res) *res = -2;
		return NULL;
	}

	if ((archivo->fd = openat(lista->dirfd, nombre, O_RDWR | O_APPEND)) == -1)
	{
		if (res) *res = -3;
		return NULL;
	}
		
			
	archivo->pos = 0;
	if (res) *res = 0;
	return archivo;
}

int filelist_close(FileNode* archivo)
{
	int res;

	if (archivo == NULL)
		return -1;

	if (archivo->fd == -1)
		return -1;

	if ((res = close(archivo->fd)) == 0)
		archivo->fd = -1;

	return res;
}

int filelist_read(FileNode* archivo, char* buff, unsigned int cant)
{
	int leido;
	
	if (archivo == NULL)
		return -1;
	
	if (archivo->fd == -1)
		return -2;
	
	if (lseek(archivo->fd, archivo->pos, SEEK_SET) == -1)
		return -1;

	leido = read(archivo->fd, buff, cant);
	archivo->pos = lseek(archivo->fd, 0, SEEK_CUR);
	return leido;
}

int filelist_write(FileNode* archivo, char* buff, unsigned int cant)
{
	if (archivo == NULL)
		return -1;
	
	if (archivo->fd == -1)
		return -2;
	return write(archivo->fd, buff, cant);
}

char* filelist_concatnames(FileList* lista)
{
	FileNode *it;
	int suma = 0;
	char* res;
	
	if (lista == NULL)
		return NULL;
	
	for(it = lista->ls; it != NULL; it = it->next)
		suma += strlen(it->name)+1;
	
	res = (char*) calloc(suma+1, sizeof(char));
	
	if (res != NULL)
	{
		for(it = lista->ls; it != NULL; it = it->next)
		{		
			strcat(res, it->name);
			strcat(res, " ");
		}
	}
	return res;
}

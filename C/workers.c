#include "workers.h"
#include "filelist.h"
#include "stringlist.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define INIT_FDTABLE 16

/* Identifica archivos abiertos
 * local == NULL && w_remoto == -1 && fd == -1 no identifica ningún
 * archivo (es decir, en un array, indica que esa posición está libre)
 */
typedef struct {
	FileNode* local;	// Si no es NULL indica que el archivo pertenece al worker, apuntando a él
	mqd_t w_remoto;		// Si local == NULL indica que el archivo es de otro worker. w_remoto indica a qué worker
	int fd;				// El fd devuelto por el worker remoto al momento de abrirlo
} FileD;

void* worker(void* id);

void initWorkers()
{
	int i, *tmp;
	char nombres[N_WORKERS][MAX_MQNAME];
	struct mq_attr attrib;
	pthread_t thread_wrk;
	
	attrib.mq_flags = 0;
	attrib.mq_maxmsg = MSGMAX;
	attrib.mq_msgsize = MSGSIZE;
	
	for (i=0; i<N_WORKERS; i++)
	{
		snprintf(nombres[i],MAX_MQNAME, "/%d-cola", i);
		mq_unlink(nombres[i]);
		if((mqwrk[i] = mq_open(nombres[i], O_RDWR | O_CREAT , 0664 ,&attrib)) < 0)
		{
			perror("Error al abrir una nueva cola de mensajes");
			exit(0);
		}
	}

	for (i=0; i<N_WORKERS;i++)
	{
		tmp = (int*) malloc(sizeof(int));
		*tmp = i;
		pthread_create(&thread_wrk, NULL, worker, tmp);
	}
}

/* Obtiene el primer Fd libre, en el caso de estar llena, redimensiona la tabla */
int getFd(FileD **tabla, int *size)
{
	int i, ret = -1, desc_size = *size;
	FileD *desc = *tabla;
	for (i = 0; i<desc_size; i++)
		if (desc[i].local == NULL && desc[i].w_remoto == -1)
			return i;
		
	if (ret == -1)
	{	
		*tabla = realloc(*tabla, sizeof(FileD)*2*desc_size);
		if (*tabla == NULL)
		{
			fprintf(stderr, "Error al reservar memoria\n");
			exit(0);
		}
		desc = *tabla;
		for (i=desc_size; i<2*desc_size; i++)
		{
			desc[i].local = NULL;
			desc[i].w_remoto = -1;
			desc[i].fd = -1;
		}
		ret = desc_size;
		*size = 2*desc_size;
	}
	return ret;
}

void* worker(void* arg)
{
	int id, ret, descriptores_size, i, newFd;
	mqd_t mq_this, mq_next, enviar_a;
	Message request;
	char carpeta[MAX_DIR];
	FileList *archivos;
	FileNode *archivo_actual;
	FileD *descriptores;
	char *buff, *buff2;
	StrList *reservados = NULL;
	
	id = *(int*) arg;
	free(arg);

	mq_this = mqwrk[id];
	mq_next = mqwrk[(id + 1) % N_WORKERS];
	
	snprintf(carpeta, MAX_DIR, "../filesystem/%d", id+1);
	archivos = filelist_new(carpeta);
	if (archivos == NULL)
	{
		fprintf(stderr, "Error al reservar memoria\n");
		exit(0);
	}
	
	descriptores_size = INIT_FDTABLE;
	descriptores = (FileD*) malloc(sizeof(FileD)*descriptores_size);
	if (descriptores == NULL)
	{
		fprintf(stderr, "Error al reservar memoria\n");
		exit(0);
	}
	for(i=0; i<INIT_FDTABLE; i++)
	{
		descriptores[i].local = NULL;
		descriptores[i].w_remoto = -1;
		descriptores[i].fd = -1;
	}
	
	while(1)
	{
		mq_receive(mq_this,(char*) &request, sizeof(Message), NULL);
		if (request.tipo == OK || request.tipo == ERROR){
			request.origen = request.cliente;
			request.id_origen = -1;
			enviar_a = request.cliente;
		}else if (request.tipo == REQUEST){
			switch(request.operacion){
				case LSD:
					buff = filelist_concatnames(archivos);
					if (buff == NULL)
					{
						request.origen = request.cliente;
						request.id_origen = -1;
						request.tipo = ERROR;
						request.arg0 = ERRNOMEM;
						enviar_a = request.cliente;
					}else{
						request.origen = mq_this;
						request.id_origen = id;
						request.operacion = W_LSD;
						enviar_a = mq_next;
						request.arg0 = strlen(buff)+1;
						request.arg2 = buff;
					}
					break;
				case W_LSD:
					if (request.id_origen == id){
						request.origen = request.cliente;
						request.id_origen = -1;
						enviar_a = request.cliente;
						buff = request.arg2;
						request.tipo = OK;
						request.operacion = LSD;
					}else{
						buff = filelist_concatnames(archivos);
						if (buff == NULL)
						{
							free(request.arg2);
							enviar_a = request.origen;
							request.operacion = LSD;
							request.tipo = ERROR;
							request.arg0 = ERRNOMEM;
						}else{
							request.arg0 += strlen(buff);
							buff2 = realloc(request.arg2, request.arg0);
							if (buff2 == NULL)
							{
								free(request.arg2);
								enviar_a = request.origen;
								request.operacion = LSD;
								request.tipo = ERROR;
								request.arg0 = ERRNOMEM;
							}else{
								enviar_a = mq_next;
								strcat(buff2, buff);
								request.arg2 = buff2;
							}
							free(buff);
						}
					}
					break;
				case DEL:
					if (request.id_origen == id){
						request.origen = request.cliente;
						enviar_a = request.cliente;
						request.tipo = ERROR;
						request.arg0 = ERRFNOTEXIST;
					}else{
						ret = filelist_remove(archivos, request.arg2);
						enviar_a = request.origen;
						if (ret == -1)
						{
							enviar_a = mq_next;
							if(request.id_origen == -1)
							{
								request.origen = mq_this;
								request.id_origen = id;
							}
						}else if (ret == -2){
							request.tipo = ERROR;
							request.arg0 = ERRFOPEN;
						}else if (ret == -3){
							request.tipo = ERROR;
							request.arg0 = ERRIO;
						}else{
							request.tipo = OK;
						}
					}			
					break;
				case CRE:
					if (request.id_origen == -1)
					{
						if (filelist_find(archivos, request.arg2) != NULL || stringlist_find(reservados, request.arg2)){
							enviar_a = request.cliente;
							request.origen = request.cliente;
							request.id_origen = -1;
							request.tipo = ERROR;
							request.arg0 = ERRFEXIST;
						}else{
							reservados = stringlist_add(reservados, request.arg2, &ret);
							if (ret == 0){
								enviar_a = mq_next;
								request.origen = mq_this;
								request.id_origen = id;
							}else{
								enviar_a = request.cliente;
								request.origen = request.cliente;
								request.id_origen = -1;
								request.tipo = ERROR;
								request.arg0 = ERRNOMEM;
							}
						}
					}else{
						if (request.id_origen == id)
						{
							enviar_a = request.cliente;
							request.origen = request.cliente;
							request.id_origen = -1;
							if (stringlist_find(reservados, request.arg2))
							{
								reservados = stringlist_remove(reservados, request.arg2);
								ret = filelist_add(archivos, request.arg2);
								if (ret == 0)
								{
									request.tipo = OK;
								}else if(ret == -1){
									request.tipo = ERROR;
									request.arg0 = ERRNOMEM;
								}else if(ret == -2){
									request.tipo = ERROR;
									request.arg0 = ERRFEXIST;
								}
							}else{
								request.tipo = ERROR;
								request.arg0 = ERRFEXIST;
							}
						}else{
							if (filelist_find(archivos, request.arg2) || (stringlist_find(reservados, request.arg2) && request.id_origen < id)){
								enviar_a = request.origen;
								request.operacion = CRE_EXISTS;
							}else{
								reservados = stringlist_remove(reservados, request.arg2);
								enviar_a = mq_next;
							}
						}
					}
					break;
				case CRE_EXISTS:
						reservados = stringlist_remove(reservados, request.arg2);
						enviar_a = request.cliente;
						request.origen = request.cliente;
						request.id_origen = -1;
						request.tipo = ERROR;
						request.arg0 = ERRFEXIST;
						request.operacion = CRE;
					break;
				case OPN:
					if (request.id_origen == id){
						request.origen = request.cliente;
						request.id_origen = -1;
						enviar_a = request.cliente;
						request.tipo = ERROR;
						request.arg0 = ERRFNOTEXIST;
					}else{
						archivo_actual = filelist_open(archivos, request.arg2, &ret);
						enviar_a = request.origen;
						if (archivo_actual == NULL && ret == -1)
						{
							enviar_a = mq_next;
							if(request.id_origen == -1)
							{
								request.origen = mq_this;
								request.id_origen = id;
							}
						}else if (archivo_actual == NULL && ret != -1){
							request.tipo = ERROR;
							if (ret == -2)
								request.arg0 = ERRFOPEN;
							else if (ret == -3)
								request.arg0 = ERRIO;
						}else{
							newFd = getFd(&descriptores, &descriptores_size);
							descriptores[newFd].local = archivo_actual;
							if (request.id_origen == -1)
								request.tipo = OK;
							else{
								request.operacion = R_FD;
								request.arg1 = mq_this;
							}
							request.arg0 = newFd;
						}
					}
					break;
				case R_FD:
					request.tipo = OK;
					request.operacion = OPN;
					request.origen = request.cliente;
					request.id_origen = -1;
					enviar_a = request.cliente;
					newFd = getFd(&descriptores, &descriptores_size);
					descriptores[newFd].w_remoto = request.arg1;
					descriptores[newFd].fd = request.arg0;
					request.arg0 = newFd;
					break;
				case WRT:
					if (request.arg0 > descriptores_size || request.arg0 < 0){
						enviar_a = request.origen;
						request.tipo = ERROR;
						request.arg0 = ERRBADFD;
					}else{
						if (descriptores[request.arg0].local != NULL){
							ret = filelist_write(descriptores[request.arg0].local, request.arg2, request.arg1);
							enviar_a = request.origen;
							if (ret == -1)
							{
								request.tipo = ERROR;
								request.arg0 = ERRIO;
							}else if(ret == -2){
								request.tipo = ERROR;
								request.arg0 = ERRBADFD;
							}else{
								request.tipo = OK;
								request.arg0 = ret;
							}
						}else if(descriptores[request.arg0].w_remoto != -1 && descriptores[request.arg0].fd != -1) {
							enviar_a = descriptores[request.arg0].w_remoto;
							request.arg0 = descriptores[request.arg0].fd;
							request.id_origen = id;
							request.origen = mq_this;
						}else{
							enviar_a = request.origen;
							request.tipo = ERROR;
							request.arg0 = ERRBADFD;
						}
					}
					break;
				case REA:
					if (request.arg0 > descriptores_size || request.arg0 < 0){
						enviar_a = request.origen;
						request.tipo = ERROR;
						request.arg0 = ERRBADFD;
					}else{
						if (descriptores[request.arg0].local != NULL){
							enviar_a = request.origen;
							if ((buff = calloc(request.arg1 + 2, sizeof(char))) == NULL)
							{
								request.tipo = ERROR;
								request.arg0 = ERRNOMEM;
							}else{
								ret = filelist_read(descriptores[request.arg0].local, buff, request.arg1);
								if (ret == -1)
								{
									request.tipo = ERROR;
									request.arg0 = ERRIO;
									free(buff);
								}else if(ret == -2){
									request.tipo = ERROR;
									request.arg0 = ERRBADFD;
									free(buff);
								}else{
									request.tipo = OK;
									request.arg0 = ret;
									buff[ret] = '\n';
									request.arg2 = buff;
								}
							}
						}else if(descriptores[request.arg0].w_remoto != -1 && descriptores[request.arg0].fd != -1) {
							enviar_a = descriptores[request.arg0].w_remoto;
							request.arg0 = descriptores[request.arg0].fd;
							request.id_origen = id;
							request.origen = mq_this;
						}else{
							enviar_a = request.origen;
							request.tipo = ERROR;
							request.arg0 = ERRBADFD;
						}
					}
					break;
				case CLO:
					if (request.arg0 > descriptores_size || request.arg0 < 0){
						enviar_a = request.origen;
						request.tipo = ERROR;
						request.arg0 = ERRBADFD;
					}else{
						if (descriptores[request.arg0].local != NULL){
							ret = filelist_close(descriptores[request.arg0].local);
							enviar_a = request.origen;
							if (ret == -1)
							{
								request.tipo = ERROR;
								request.arg0 = ERRBADFD;
							}else if (ret == 0){
								request.tipo = OK;
							}
						}else if(descriptores[request.arg0].w_remoto != -1 && descriptores[request.arg0].fd != -1) {
							enviar_a = descriptores[request.arg0].w_remoto;
							request.arg0 = descriptores[request.arg0].fd;
							request.id_origen = id;
							request.origen = mq_this;
						}else{
							enviar_a = request.origen;
							request.tipo = ERROR;
							request.arg0 = ERRBADFD;
						}
					}
					break;
				default:
					enviar_a = request.origen;
					request.tipo = ERROR;
					request.arg0 = ERRBADCMD;
					break;
			}
		}
		mq_send(enviar_a, (char*) &request, sizeof(Message), 0);
	}
	return NULL;
}

#include <stdio.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <stdlib.h>
#include <ctype.h>
#include "workers.h"

#define BUFF_SIZE 256
#define INIT_FDTABLE 8
#define ERROR_AND_CONTINUE(nro) { send_text(conn_s,ErrStr[nro]); free(user_input); continue; }


char *ErrStr[] = {"ERROR BADFD\n",
                  "ERROR BADARG\n",
                  "ERROR BADCOMMAND\n",
                  "ERROR FILENOTEXIST\n",
                  "ERROR FILEEXISTS\n",
                  "ERROR FILEOPEN\n",
                  "ERROR NOCONNECTION\n",
                  "ERROR ALREADYCONNECTED\n",
                  "ERROR CANTCLOSE\n",
                  "ERROR NOMEMORY\n",
                  "ERROR IOERROR\n" };

int client_id;

pthread_mutex_t semId = PTHREAD_MUTEX_INITIALIZER; 

char *read_line(int fd);
int send_text(int fd, char* str);
void *handleCliente(void* args);
int atonat(char* str);
int checkName(char* str);

int main()
{
	int conn_s=-1, list_s, *conn_temp;
	pthread_t thread_client;
	struct sockaddr_in addr;

	printf("Iniciando workers...\n");
	initWorkers();

	if ( (list_s = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
		fprintf(stderr, "TCP: Error creating listening socket.\n");
		return -1;
	}
	memset(&addr, 0, sizeof(struct sockaddr_in));
	addr.sin_family      = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port        = htons(8000);

	if ( bind(list_s, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)) < 0 ) {
		fprintf(stderr, "TCP: Error calling bind()\n");
		return -1;
	}

	if ( listen(list_s, 10) < 0 ) {
		fprintf(stderr, "TCP: Error calling listen()\n");
		return -1;
	}
	
	printf("Esperando conexiones entrantes...\n");
	while (1) {
		if ( (conn_s = accept(list_s, NULL, NULL) ) < 0 ) {
			fprintf(stderr, "TCP: Error calling accept()\n");
			return -1;
		}
		conn_temp = malloc(sizeof(int));
		*conn_temp = conn_s;
		
		pthread_create(&thread_client, NULL, handleCliente, conn_temp);
	}
	return 0;
}

void *handleCliente(void *args)
{
	char nombre_cola[MAX_MQNAME], *user_input, *command, *keyword, *keyword2, *filename,
	     *descriptor, *size, *data_write, *resto, response[BUFF_SIZE];
	int actual, descriptor_val, size_val, conn_s, i, newFd,
	    *filetable, filetable_size;
	unsigned int seed;

	mqd_t mq_cliente, mq_worker;
	struct mq_attr attrib;
	Message msg_request, msg_response;

	pthread_mutex_lock(&semId);
	actual = client_id;
	client_id++;
	pthread_mutex_unlock(&semId);

	printf("Cliente %d conectado\n",actual);

	seed = time(NULL);
	mq_worker = -1;
	conn_s = *(int*)args;
	free(args);

	attrib.mq_flags = 0;
	attrib.mq_maxmsg = MSGMAX;
	attrib.mq_msgsize = MSGSIZE;

	snprintf(nombre_cola, MAX_MQNAME,"/cl%d", actual);

	mq_unlink(nombre_cola);
	if((mq_cliente = mq_open(nombre_cola, O_RDWR | O_CREAT , 0664 ,&attrib)) < 0)
	{
		perror("Error al abrir una nueva cola de mensajes");
		exit(0);
	}
	
	filetable_size = INIT_FDTABLE;
	filetable = (int*) malloc(sizeof(int)*filetable_size);
	if (filetable == NULL)
	{
		fprintf(stderr, "Error al reservar memoria\n");
		exit(0);
	}
	for(i=0; i<INIT_FDTABLE; i++)
		filetable[i] = -1;

	msg_request.cliente = mq_cliente;
	msg_request.origen = mq_cliente;
	msg_request.id_origen = -1;
	msg_request.tipo = REQUEST;
	while(1)
	{
		if ((user_input = read_line(conn_s)) == NULL){
			close(conn_s);
			conn_s = -1;
			user_input = malloc(sizeof(char)*4);
			strcpy(user_input, "BYE");		//En el caso de que haya una desconexión forzosa, hace como si hubiese llegado un "BYE"
		}
		command = strtok(user_input, " \r\n");
		if (command == NULL){
			free(user_input);
			continue;
		}

		if (strcmp(command, "CON")==0){
			resto = strtok(NULL, " \r\n");
			if (resto != NULL) {
				ERROR_AND_CONTINUE(ERRBADARG);
			} else if (mq_worker == -1){
				mq_worker = mqwrk[rand_r(&seed) % N_WORKERS];
				snprintf(response, BUFF_SIZE, "OK ID %d\n", actual);
				send_text(conn_s, response);
				free(user_input);
				continue;
			} else {
				ERROR_AND_CONTINUE(ERRCONNE);
			}

		}else if (strcmp(command, "LSD")==0){
			resto = strtok(NULL, " \r\n");
			if (resto != NULL) {
				ERROR_AND_CONTINUE(ERRBADARG);
			} else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else{
				msg_request.operacion = LSD;
			}			

		}else if (strcmp(command, "DEL")==0){
			filename = strtok(NULL, "\r\n");
			resto = strtok(NULL, " \r\n");
			if (resto != NULL || filename == NULL || !checkName(filename)){
				ERROR_AND_CONTINUE(ERRBADARG);
			}else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else{
				//arg2 = Nombre de archivo a elminiar
				msg_request.operacion = DEL;
				msg_request.arg2 = filename;
			}

		}else if (strcmp(command, "CRE")==0){
			filename = strtok(NULL, "\r\n");
			resto = strtok(NULL, " \r\n");
			if (resto != NULL || filename == NULL || !checkName(filename)){
				ERROR_AND_CONTINUE(ERRBADARG);
			}else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else{
				//arg2 = Nombre del nuevo archivo
				msg_request.operacion = CRE;
				msg_request.arg2 = filename;
			}

		}else if (strcmp(command, "OPN")==0){
			filename = strtok(NULL, "\r\n");
			resto = strtok(NULL, " \r\n");
			if (resto != NULL || filename == NULL || !checkName(filename)){
				ERROR_AND_CONTINUE(ERRBADARG);
			}else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else{
				//arg2 = Nombre del archivo a abrir
				msg_request.operacion = OPN;
				msg_request.arg2 = filename;
			}

		}else if (strcmp(command, "WRT")==0){
			keyword = strtok(NULL, " ");
			descriptor = strtok(NULL, " ");
			keyword2 = strtok(NULL, " ");
			size = strtok(NULL, " ");
			data_write = strtok(NULL, "");
			
			size_val = atonat(size);
			descriptor_val = atonat(descriptor);
			
			if (size_val < 0 || descriptor_val < 0 || strcmp(keyword, "FD") != 0 || strcmp(keyword2, "SIZE") != 0 || data_write == NULL){
				ERROR_AND_CONTINUE(ERRBADARG);
			}else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else if (descriptor_val >= filetable_size || filetable[descriptor_val]==-1){
				ERROR_AND_CONTINUE(ERRBADFD);
			}else{
				//arg0 = Descriptor en donde escribir los datos
				//arg1 = Longitud del buffer
				//arg2 = Buffer
				msg_request.operacion = WRT;
				msg_request.arg0 = filetable[descriptor_val];
				msg_request.arg1 = strlen(data_write) < size_val ? strlen(data_write) : size_val;
				msg_request.arg2 = data_write;
			}

		}else if (strcmp(command, "REA")==0){
			keyword = strtok(NULL, " ");
			descriptor = strtok(NULL, " ");
			keyword2 = strtok(NULL, " ");
			size = strtok(NULL, " \r\n");

			size_val = atonat(size);
			descriptor_val = atonat(descriptor);
			resto = strtok(NULL, " \r\n");
			if (resto != NULL || size_val < 0 || descriptor_val < 0 || strcmp(keyword, "FD") != 0 || strcmp(keyword2, "SIZE") != 0){
				ERROR_AND_CONTINUE(ERRBADARG);
			}else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else if (descriptor_val >= filetable_size || filetable[descriptor_val]==-1){
				ERROR_AND_CONTINUE(ERRBADFD);
			}else{
				//arg0 = Descriptor desde donde leer el archivo
				//arg1 = Cantidad de caracteres que se deben leer
				msg_request.operacion = REA;
				msg_request.arg0 = filetable[descriptor_val];
				msg_request.arg1 = size_val;
			}

		}else if (strcmp(command, "CLO")==0){
			keyword = strtok(NULL, " ");
			descriptor = strtok(NULL, " \r\n");
			
			descriptor_val = atonat(descriptor);
			resto = strtok(NULL, " \r\n");
			if (resto != NULL || descriptor_val < 0 || strcmp(keyword, "FD") != 0){
				ERROR_AND_CONTINUE(ERRBADARG);
			}else if (mq_worker == -1){
				ERROR_AND_CONTINUE(ERRNOCNC);
			}else if (descriptor_val >= filetable_size || filetable[descriptor_val]==-1){
				ERROR_AND_CONTINUE(ERRBADFD);
			}else{
				//arg0 = Descriptor de archivo en el worker
				//arg1 = Descriptor de archivo local
				msg_request.operacion = CLO;
				msg_request.arg0 = filetable[descriptor_val];
				msg_request.arg1 = descriptor_val;
			}

		}else if (strcmp(command, "BYE")==0){
			resto = strtok(NULL, " \r\n");
			if (resto != NULL) {
				ERROR_AND_CONTINUE(ERRBADARG);
			}else { 
				if (mq_worker != -1){
					msg_request.operacion = CLO;
					for (i=0; i<filetable_size;i++)
					{
						if (filetable[i] != -1)
						{
							msg_request.arg0 = filetable[i];
							mq_send(mq_worker, (char*) &msg_request, sizeof(Message), 0);
							mq_receive(mq_cliente, (char*) &msg_response, sizeof(Message), NULL);
						}
					}
				} 
				free(filetable);
				free(user_input);
				mq_unlink(nombre_cola);
				if (conn_s > -1){		// Si la conexión sigue abierta
					send_text(conn_s, "OK\n");
					close(conn_s);
				}
				printf("Cliente %d desconectado\n", actual);
				break;
			}
		}else{
			ERROR_AND_CONTINUE(ERRBADCMD);
		}
		mq_send(mq_worker, (char*) &msg_request, sizeof(Message), 0);
		mq_receive(mq_cliente, (char*) &msg_response, sizeof(Message), NULL);
		
		if (msg_response.tipo == OK){
			switch(msg_response.operacion){
				case CLO:
					// arg1 = El descriptor local del archivo cerrado
					filetable[msg_response.arg1] = -1;
				case DEL:
				case CRE:
				case WRT:
					send_text(conn_s, "OK\n");
					break;
				case REA:
					//arg0 = Longitud del buffer leido
					//arg2 = Buffer
					snprintf(response, BUFF_SIZE, "OK SIZE %d ", msg_response.arg0);
					send_text(conn_s, response);
					send_text(conn_s, msg_response.arg2);
					free(msg_response.arg2);
					break;
				case LSD:
					//arg2 = Lista de archivos, separada por espacios
					send_text(conn_s, "OK ");
					send_text(conn_s, msg_response.arg2);
					send_text(conn_s, "\n");
					free(msg_response.arg2);				
					break;
				case OPN:
					//arg0 = El descriptor del archivo en el worker
					newFd = -1;
					for (i = 0; i<filetable_size; i++)
						if (filetable[i] == -1)
						{
							newFd = i;
							break;
						}
					if (newFd == -1)
					{	
						filetable = realloc(filetable, sizeof(int)*2*filetable_size);
						if (filetable == NULL)
						{
							fprintf(stderr, "Error al reservar memoria\n");
							exit(0);
						}
						for (i=filetable_size; i<2*filetable_size; i++)
							filetable[i] = -1;
						newFd = filetable_size;
						filetable_size = 2*filetable_size;
					}
					
					filetable[newFd] = msg_response.arg0;
					snprintf(response, BUFF_SIZE, "OK FD %d\n", newFd);
					send_text(conn_s, response);
					break;
				default:
					snprintf(response, BUFF_SIZE, "OK UNEXPECTED %d\n", msg_response.operacion);
					send_text(conn_s, response);
			}
		}else if(msg_response.tipo == ERROR){
			ERROR_AND_CONTINUE(msg_response.arg0);
		}
		free(user_input);
	}
	return NULL;
}

/* Esta función lee del descriptor pasado como argumento, hasta encontrar \n
 * (es decir cuando el usuario apretó "Enter") o un \0.
 * Devuelve la cadena, con el \0 terminal y 
 * sin el salto de linea del final (\n si es netcat o \r\n en telnet)
 * La función llamante debe liberar la memoria reservada */
char *read_line(int fd)
{
	char car, *tmp, *ret = malloc(16*sizeof(char));
	int len_alloc = 16, len_read = 0;
	
	while (read(fd, &car, 1) > 0)
	{
		if (len_alloc == len_read)
		{
				tmp = realloc(ret, 2*len_alloc);
				if (!tmp)
				{
					free(ret);
					return NULL;
				}
				ret = tmp;
				len_alloc = 2*len_alloc;
		}
		ret[len_read] = car;
		len_read++;
		
		if (car == '\0' || car == '\n'){
			ret[len_read-1] = 0;
			if (len_read > 1 && ret[len_read-2] == '\r')
				ret[len_read-2] = 0;
			break;
		}
		
	}
	if (len_read){
		return ret;
	}else{
		free(ret);
		return NULL;
	}
}

/* Envia la cadena apuntada por str
 * al descriptor fd. La cadena debe finalizar con \0 */
int send_text(int fd, char* str)
{
	return write(fd, str, strlen(str));
}

/* Dada una cadena de texto, la convierte a número natural. A diferencia
 * de atoi, esta función falla (devuelve -1), cuando la cadena incluye caracteres
 * no numéricos */
int atonat(char* str)
{
	int res = 0;
	
	if (str == NULL || *str == '\0')
		return -1;

	for (; *str; str++)
	{
		if (isdigit(*str)){
			res = res*10;
			res = res + (*str - '0');
		}else{
			return -1;
		}
	}
	return res;
}

/* Verifica que la cadena apuntada por str solo contenga caracteres válidos
 * para un nombre de archivo: A-Z a-z 0-9 '.'
 */
int checkName(char* str)
{
	if (str == NULL || *str == '\0')
		return 0;

	for (; *str; str++)
		if (!isalnum(*str) && *str != '.')
			return 0;

	return 1;
}


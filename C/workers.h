#ifndef __WORKERS__
#define __WORKERS__

#include <mqueue.h>
#include <fcntl.h>

#define N_WORKERS 5

#define MAX_MQNAME 10
#define MSGSIZE sizeof(Message)
#define MSGMAX 10
#define MAX_DIR 25

mqd_t mqwrk[N_WORKERS];

typedef enum {
	LSD,
	DEL,
	CRE,
	OPN,
	WRT,
	REA,
	CLO,
	W_LSD,
	R_FD,
	CRE_EXISTS,
} OpCode;

typedef enum {
	ERRBADFD,
	ERRBADARG,
	ERRBADCMD,
	ERRFNOTEXIST,
	ERRFEXIST,
	ERRFOPEN,
	ERRNOCNC,
	ERRCONNE,
	ERRCLOSING,
	ERRNOMEM,
	ERRIO
} ErrCode;

typedef enum{
	OK,
	ERROR,
	REQUEST,
} TCode;

// Estructura de los mensajes enviados entre workers y con los procesos socket          
typedef struct {
	mqd_t cliente;		// Indica a que proceso socket hay que responder
	mqd_t origen;		// Indica la cola del proceso que originó el mensaje
	int id_origen;		// Si id_origen == -1, el origen fue un cliente, sino fue un worker (indica el id del worker)
	TCode tipo;			// Indica el tipo de mensaje
	OpCode operacion;	// Indica la operacion asociada al mensaje
	int arg0;			// Argumentos. Depende de cada mensaje
	int arg1;
	char* arg2;
} Message;

void initWorkers();

#endif

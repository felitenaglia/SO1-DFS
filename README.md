# Distributed file system
Final assignment at university. Course: Operating Systems I. Simplified implementation of a distributed file system.
Year: 2013.

## Requirements

The C-part of the project should be compiled with `gcc` and the Erlang-part should be run on its virtual machine.

## Compilation

Under `C`, the `makefile` will make a binary (`server`).

## Usage

Both versions work exactly the same. Because they bind 8000/tcp port, they cannot be run at the same time.

When run it will listen 8000/tcp, waiting for connections. After the connection has been established, it will wait for commands that manage the file system. 

Those commands are:
 - `CON` initializes the connection
 - `LSD` lists all files
 - `DEL <File>` deletes that file (if it exists and if it is not opened)
 - `CRE <File>` creates that file (if it does not exist)
 - `OPN <File>` opens that file. It returns `OK FD <Nmbr>`, where `<Nmbr>` is the file descriptor, if the file exists.
 - `WRT FD <fd> SIZE <count> <payload>` writes `<count>` bytes on the file with file descriptor `<fd>`, from the buffer `<payload>`
 - `REA FD <fd> SIZE <count>` reads `<count>` bytes from the file with file descriptor `<fd>`
 - `CLO FD <fd>` closes the file with file descriptor `<fd>`
 - `BYE` closes the connection and all opened files.

All files are saved in the real filesystem (under `filesystem` directory), each worker having a separate sub-directory. The files are persistent (i.e. they are not deleted when the server shutdowns) and they are shared between both versions.

For more information, a report and the rubric are available under `doc` directory.

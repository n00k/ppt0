#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/dev/ppbus/ppi.h>
#include <sys/dev/ppbus/ppbconf.h>

typedef unsigned char byte;
static int ppi_fd;

static int do_init(char *port)
{
	//char port[] = "/dev/ppi0";

	ppi_fd = open(port, O_RDWR);
	if( ppi_fd < 0 ) {
		return((-1 >> sizeof(int)) | errno);
	}
	return ppi_fd;
}

static int data_out(byte outval)
{
	int n;
	byte val;
    
	val = outval;

	n = ioctl(ppi_fd, PPISDATA, &val);
	if( n < 0 ) {
		return((-1 >> sizeof(int)) | errno);
	}
}

static int ctrl_out(byte outval)
{
	int n;
	byte val = outval;

	n = ioctl(ppi_fd, PPISCTRL, &val);
 	if( n < 0 ) {
		return((-1 >> sizeof(int)) | errno);
	}
}

static int data_in(void)
{
	int n;
	byte val;
 
	n = ioctl(ppi_fd, PPIGDATA, &val);
	if( n < 0 ) {
		return((-1 >> sizeof(int)) | errno);
	}
	return(val);
}

static int ctrl_in(void)
{
	int n;
	byte val;
 
	n = ioctl(ppi_fd, PPIGCTRL, &val);
	if( n < 0 ) {
		return((-1 >> sizeof(int)) | errno);
	}
	return(val);
}

static int stat_in(void)
{
	int n;
	byte val;
 
	n = ioctl(ppi_fd, PPIGSTATUS, &val);
	if( n < 0 ) {
		return((-1 >> sizeof(int)) | errno);
	}
	return(val);
}

int main() {
	int fn, arg, res;
	byte buf[100];

	while (read_cmd(buf) > 0) {
		fn = buf[0];
		arg = buf[1];

		switch (fn) 
		{
			case 0:
				res = do_init(&buf[1]);
				break;
			case 1:
				data_out(arg);
			case 2:
				res = data_in();
				break;
			case 3:
				ctrl_out(arg);
			case 4:
				res = ctrl_in();
				break;
			case 5:
				res = stat_in();
				break;
			default:
				res = 65535;
		}

		if (res >= 0) {
			buf[0] = res;
			write_cmd(buf, 1);
		} else {
			strerror_r(0 - res, buf, sizeof(buf));
			write_cmd(buf, strlen(buf));
		}
		buf[0] = 0;
	}
}

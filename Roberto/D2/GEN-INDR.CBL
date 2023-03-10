	   IDENTIFICATION DIVISION.
	   PROGRAM-ID. GEN-INDR.
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.
	       SELECT CLI-INDR ASSIGN TO DISK
		   ORGANIZATION IS INDEXED
		   ACCESS MODE IS RANDOM
		   RECORD KEY IS ID-CLIENTE.
	   DATA DIVISION.
	   FILE SECTION.
	   FD CLI-INDR.
      *
	   01 REG-CLI.
	       03 ID-CLIENTE PIC 9(03).
		   03 NOM-CLI    PIC X(30).
		   03 CLA-TAR    PIC 9.
		   03 NOM-TAR    PIC X(20).
		   03 ANUA       PIC 9(05)V99.
		   03 FEC-ING    PIC 9(08).
	   
	   WORKING-STORAGE SECTION.
	   77 WKS-FIN            PIC 9.
	   77 WKS-DESC           PIC X.
	   
	   PROCEDURE DIVISION.
	   INICIO.
	       PERFORM 1000-INICIO.
		   PERFORM 2000-PROCESO UNTIL WKS-FIN = 1.
		   PERFORM 3000-FIN.
		   STOP RUN.
		   
	   1000-INICIO.
      *    CREAR ARCHIVO.
      *    PRIMERA VEZ.	  
           OPEN OUTPUT CLI-INDR.
      *    SI YA EXISTE:
      *     OPEN I-O CLI-INDR.	  
	  
	   2000-PROCESO.
	       DISPLAY "ID CLIENTE:" LINE 10 POSITION 05.
		   ACCEPT ID-CLIENTE LINE 10 POSITION 33.
		   DISPLAY "NOMBRE DEL CLIENTE:" LINE 11 POSITION 05.
		   ACCEPT NOM-CLI LINE 11 POSITION 33.
		   DISPLAY "CLAVE DE LA TARJETA:" LINE 12 POSITION 05.
		   ACCEPT CLA-TAR LINE 12 POSITION 33.
		   
		   IF CLA-TAR = 1
		       MOVE "DEBITO" TO NOM-TAR
			   MOVE 1000 TO ANUA
		   ELSE IF CLA-TAR = 2
		         MOVE "CREDITO" TO NOM-TAR
			     MOVE 3500 TO ANUA
		         ELSE IF CLA-TAR = 3
		               MOVE "NOMINA" TO NOM-TAR
			           MOVE 0 TO ANUA.
		   
		   DISPLAY "FECHA DE INGRESO:(AAAAMMDD)" LINE 15 POSITION 05.
		   ACCEPT FEC-ING LINE 15 POSITION 33.
		   
		   WRITE REG-CLI INVALID KEY
		                 DISPLAY "LLAVE INVALIDA".
		   
		   DISPLAY 
			"DESEA CAPTURAR MAS REGISTROS?:(S/N)" LINE 17 POSITION 05.
		   ACCEPT WKS-DESC LINE 17 POSITION 43.
		   IF WKS-DESC = "N"
		       MOVE 1 TO WKS-FIN.
			   
			   
	   3000-FIN.
      *    CERRAR ARCHIVO.
	       CLOSE CLI-INDR.
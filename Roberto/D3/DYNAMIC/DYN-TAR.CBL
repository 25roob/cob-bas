	   IDENTIFICATION DIVISION.
	   PROGRAM-ID. DYN-TAR.
	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.
	       SELECT CLI-INDD ASSIGN TO DISK
		   ORGANIZATION IS INDEXED
		   ACCESS MODE IS DYNAMIC
		   RECORD KEY IS ID-CLIENTE
		ALTERNATE RECORD KEY IS NOM-TAR WITH DUPLICATES
		ALTERNATE RECORD KEY IS FEC-ING WITH DUPLICATES.
	   DATA DIVISION.
	   FILE SECTION.
	   FD CLI-INDD.
      *
	   01 REG-CLI.
	       03 ID-CLIENTE  PIC 9(03).
		   03 NOM-CLI      PIC X(30).
		   03 CLA-TAR      PIC 9.
		   03 NOM-TAR     PIC X(20).
		   03 ANUA           PIC 9(05)V99.
		   03 FEC-ING       PIC 9(08).
	   
	   WORKING-STORAGE SECTION.
	   01 WKS-TIPOT        PIC X(20).
	   77 WKS-FIN            PIC 9.
	   77 WKS-DESC        PIC X.
	   77 WKS-FIND          PIC 9.
	   
	   PROCEDURE DIVISION.
	   INICIO.
	       PERFORM 1000-INICIO.
		   PERFORM 2000-PROCESO UNTIL WKS-FIN = 1.
		   PERFORM 3000-FIN.
		   STOP RUN.
		   
	   1000-INICIO.
		   OPEN INPUT CLI-INDD.
      *           READ CLI-INDD
      *      	    INVALID KEY DISPLAY "ERROR" LINE 10 POSITION 05.
	  
	       MOVE SPACES TO NOM-TAR.
	       
		   START CLI-INDD KEY IS GREATER OR EQUAL TO NOM-TAR.
		   
	  
	  2000-PROCESO.
		   DISPLAY REG-CLI.
		   
		   
      *		   START CLI-INDD
      *		       KEY IS = NOM-TAR
      *			   INVALID KEY DISPLAY "NO EXISTE" LINE 10 POSITION 40
      *			   MOVE 1 TO WKS-FIND.
			   
		   READ CLI-INDD
		       NEXT 
			   AT END MOVE 1 TO WKS-FIN.

      *		       IF WKS-FIN = 0 
      *			       DISPLAY REG-CLI.
	  
	  3000-FIN.
	       CLOSE CLI-INDD.
       IDENTIFICATION DIVISION.
	   PROGRAM-ID. PRAC2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
	   WORKING-STORAGE SECTION.
	   01 WKS-NOM-COM.
	       03 WKS-NOMBRE PIC X(20).
		   03 WKS-APE-PAT PIC X(20).
		   03 WKS-APE-MAT PIC X(20).
       PROCEDURE DIVISION.
	   INICIO.
	       DISPLAY "INTRODUCE NOMBRE:" LINE 10 POSITION 05.
		   ACCEPT WKS-NOMBRE LINE 10 POSITION 33.
		   DISPLAY "INTRODUCE APELLIDO PATERNO:" LINE 11 POSITION 05.
		   ACCEPT WKS-APE-PAT LINE 11 POSITION 33.
		   DISPLAY "INTRODUCE APELLIDO MATERNO:" LINE 12 POSITION 05.
		   ACCEPT WKS-APE-MAT LINE 12 POSITION 33.
		   DISPLAY "EL NOMBRE COMPLETO ES: " LINE 13 POSITION 05.
		   DISPLAY WKS-NOM-COM LINE 14 POSITION 05.
		   STOP RUN.
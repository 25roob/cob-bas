       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRO-FIN2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI-INDD ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
               RECORD KEY IS ID-CLIENTE
               ALTERNATE RECORD KEY IS NOM-TAR WITH DUPLICATES
               ALTERNATE RECORD KEY IS FEC-ING WITH DUPLICATES.
           SELECT REP-CLI ASSIGN TO PRINTER.
           SELECT REP-TAR ASSIGN TO PRINTER.
           SELECT REP-TF  ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD CLI-INDD VALUE OF TITLE WKS-ETIQ-REP.
       01 REG-CLI.
           03 ID-CLIENTE   PIC 9(03).
           03 NOM-CLI      PIC X(30).
           03 CLA-TAR      PIC 9.
           03 NOM-TAR      PIC X(20).
           03 ANUA         PIC 9(05)V99.
           03 FEC-ING      PIC 9(08).
      
       FD REP-CLI VALUE OF TITLE WKS-ETIQ-RC
           VALUE PROTECTION IS      PROTECTED
           VALUE SECURITYTYPE IS    PUBLIC
           RECORD CONTAINS          132 CHARACTERS
           BLOCK CONTAINS           1 RECORDS.
       01 LINEAC           PIC X(132).
      
       FD REP-TAR VALUE OF TITLE WKS-ETIQ-RT
       VALUE PROTECTION IS          PROTECTED
           VALUE SECURITYTYPE IS    PUBLIC
           RECORD CONTAINS          132 CHARACTERS
           BLOCK CONTAINS           1 RECORDS.
       01 LINEAT           PIC X(132).
      
       FD REP-TF VALUE OF TITLE WKS-ETIQ-RTF
       VALUE PROTECTION IS          PROTECTED
           VALUE SECURITYTYPE IS    PUBLIC
           RECORD CONTAINS          132 CHARACTERS
           BLOCK CONTAINS           1 RECORDS.
       01 LINEAF           PIC X(132).
      
      
       WORKING-STORAGE SECTION.
       01 WKS-ETIQ-REP     PIC X(24) VALUE "RFQV/S015/FILE/INDEXADO.".
       01 WKS-ETIQ-RC      PIC X(23) VALUE "RFQV/S015/LIST/REPOCLI.".
       01 WKS-ETIQ-RT      PIC X(23) VALUE "RFQV/S015/LIST/REPOTAR.".
       01 WKS-ETIQ-RTF     PIC X(24) VALUE "RFQV/S015/LIST/REPOTAFE.".
       01 WKS-TIPOT        PIC X(20).
       01 WKS-TOTAL-CLI    PIC 99.
       01 WKS-CONT-LIN     PIC 99.
       01 WKS-TOTAL-ANUA   PIC 9(05)V99.
       01 WKS-CUTTER       PIC X(20).
       01 WKS-ANUALIS      PIC 9(05)V99.
      
       77 WKS-FIN          PIC 9.
       77 WKS-DESC         PIC 9.
       77 WKS-FINR1        PIC 9.
       77 WKS-ESPERA       PIC X.
       77 WKS-FINR2        PIC 9.
       77 WKS-FINR3        PIC 9.
       77 WKS-FIN-CHE      PIC 9.
      
      
       01 WKS-HOY.
           03 WKS-ANO     PIC 99.
           03 WKS-MES     PIC 99.
           03 WKS-DIA     PIC 99.
      
      *----------VARIABLES DE REPORTE CLI-------------
       01 WKS-ENC1.
           03 FILLER      PIC X(28) VALUE SPACES.
           03 FILLER      PIC X(40)
                             VALUE "REPORTE DE CLIENTES".
      
       01 WKS-ENC2.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER      PIC X(18)
                     VALUE "FECHA DE PROCESO: ".
           03 WKS-DIA     PIC 99.
           03 FILLER      PIC X VALUE "/".
           03 WKS-MES     PIC 99.
           03 FILLER      PIC X VALUE "/".
           03 WKS-ANO     PIC 99.
           03 FILLER      PIC X(38) VALUE SPACES.
           03 FILLER      PIC X(07) VALUE "PAG. : ".
           03 WKS-NUM-PAG           PIC 9(02).
      
       01 WKS-ENC3.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(10) VALUE "ID-CLIENTE".
           03 FILLER         PIC X(02) VALUE SPACES.
           03 FILLER         PIC X(06) VALUE "NOMBRE".
           03 FILLER         PIC X(11) VALUE SPACES.
           03 FILLER         PIC X(15) VALUE "TIPO DE TARJETA".
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(14) VALUE "FEC DE INGRESO".
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(09) VALUE "ANUALIDAD".
      
       01 WKS-DET1.
           03 FILLER         PIC X(02) VALUE SPACES.
           03 WKS-ID         PIC 999.
           03 FILLER         PIC X(07) VALUE SPACES.
           03 WKS-NOMB       PIC X(21).
           03 FILLER         PIC X(02) VALUE SPACES.
           03 WKS-TIPT       PIC X(10).
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(02) VALUE "20".
           03 WKS-FECH       PIC 9(06).
           03 FILLER         PIC X(07) VALUE SPACES.
           03 WKS-ANUF       PIC $$$,$$9.99.
      
      
       01 WKS-TOT1.
           03 FILLER         PIC X(38) VALUE SPACES.
           03 FILLER         PIC X(18) VALUE "TOTAL DE CLIENTES:".
           03 FILLER         PIC X(09) VALUE SPACES.
           03 WKS-TOCLI      PIC 99.
      
      *----------VARIABLES DE REPORTE TAR------------
       01 WKS-ENCA.
           03 FILLER         PIC X(28) VALUE SPACES.
           03 FILLER         PIC X(40)
                      VALUE "REPORTE DE TARJETAS".
      
       01 WKS-ENCB.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(18)
                   VALUE "FECHA DE PROCESO: ".
           03 WKS-DIA        PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WKS-MES        PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WKS-ANO        PIC 99.
           03 FILLER         PIC X(38) VALUE SPACES.
           03 FILLER         PIC X(07) VALUE "PAG. : ".
           03 WKS-NUM-PAGA   PIC 9(02).
      
       01 WKS-ENCBB.
           03 FILLER         PIC X(20) VALUE
                "TIPO DE TARJETA:".
           03 WKS-NOM-TARB   PIC X(20).
      
       01 WKS-ENCC.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(10) VALUE "ID-CLIENTE".
           03 FILLER         PIC X(02) VALUE SPACES.
           03 FILLER         PIC X(06) VALUE "NOMBRE".
           03 FILLER         PIC X(10) VALUE SPACES.
           03 FILLER         PIC X(16) VALUE "CLAVE DE TARJETA".
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(14) VALUE "FEC DE INGRESO".
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(09) VALUE "ANUALIDAD".
      
       01 WKS-DETA.
           03 FILLER         PIC X(02) VALUE SPACES.
           03 WKS-IDT        PIC 999.
           03 FILLER         PIC X(07) VALUE SPACES.
           03 WKS-NOMT       PIC X(21).
           03 FILLER         PIC X(03) VALUE SPACES.
           03 WKS-CLAT       PIC X(10).
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(02) VALUE "20".
           03 WKS-FECT       PIC 9(06).
           03 FILLER         PIC X(06) VALUE SPACES.
           03 WKS-ANUFT      PIC $$$,$$9.99.
      
       01 WKS-TOTA.
           03 FILLER         PIC X(20) VALUE SPACES.
           03 FILLER         PIC X(16) VALUE
                    "TOTAL ANUALIDAD".
           03 WKS-FTITAR     PIC X(8).
           03 FILLER         PIC X(05) VALUE ":    ".
           03 WKS-TOANU      PIC $$$$,$$9.99.
      
       01 WKS-TOTAS.
           03 FILLER         PIC X(20) VALUE SPACES.
           03 FILLER         PIC X(19) VALUE
               "TOTAL ANUALIDADES:".
      *           03 WKS-FTITARS PIC X(8).
           03 FILLER         PIC X(05) VALUE SPACES.
           03 WKS-TOANS PIC $$$$,$$9.99.
      
      
      *---------VARIABLES REPORTE TAR-FECH-----------*
      
       01 WKS-TOTPAR.
           03 FILLER         PIC X(33) VALUE SPACES.
           03 FILLER         PIC X(24) VALUE
                   "TOTAL ANUALIDADES AÑO 20".
           03 WKS-ANOT       PIC 99.
           03 FILLER         PIC X(03) VALUE ":  ".
           03 WKS-PARTOT     PIC $$$$,$$9.99.
      
       01 WKS-FEC-SPLIT.
           03 FILLER         PIC 99.
           03 WKS-ASPLIT     PIC 99.
           03 FILLER         PIC 9(04).
      
       01 WKS-ANUASP         PIC 9(6)V99.
       01 WKS-CUTANO         PIC 99.
      
       PROCEDURE DIVISION.
      
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-FIN = 1.
           PERFORM 3000-FIN.
           STOP RUN.
      
       1000-INICIO.
           CHANGE ATTRIBUTE TITLE OF CLI-INDD TO  WKS-ETIQ-REP.
      *----ESTO ES PARA LOS REPORTES:
           SET CLI-INDD (USERBACKUPNAME) TO VALUE TRUE.
      *     OPEN INPUT CLI-INDD.
           CHANGE ATTRIBUTE TITLE OF REP-CLI TO  WKS-ETIQ-RC.
           SET REP-CLI (USERBACKUPNAME) TO VALUE TRUE.
      
           CHANGE ATTRIBUTE TITLE OF REP-TAR TO  WKS-ETIQ-RT.
           SET REP-TAR (USERBACKUPNAME) TO VALUE TRUE.
      
           CHANGE ATTRIBUTE TITLE OF REP-TF TO  WKS-ETIQ-RTF.
           SET REP-TF (USERBACKUPNAME) TO VALUE TRUE.
      
       2000-PROCESO.
      *       DISPLAY REG-CLI.
      *       READ CLI-INDD
      *       NEXT AT END MOVE 1 TO WKS-FIN.
      
           DISPLAY SPACES.
           DISPLAY "REPORTE DE TARJETAS DE CLIENTES".
           DISPLAY "REPORTE POR CLIENTES   (1)".
           DISPLAY "REPORTE POR TARJETAS   (2)".
           DISPLAY "REPORTE TARJETAS-FECHA (3)"
           DISPLAY "SALIR                  (4)".
           DISPLAY "ELEGIR OPCION:".
           ACCEPT WKS-DESC.
      
           IF WKS-DESC = 1
               PERFORM 2400-REPORTE-CLI
           ELSE IF WKS-DESC = 2
               PERFORM 2800-REPORTE-TAR
           ELSE IF WKS-DESC = 3
               PERFORM 2900-REPORTE-TAFE
           ELSE IF WKS-DESC = 4
               MOVE 1 TO WKS-FIN.
      
      
      
      
      
       2400-REPORTE-CLI.
           DISPLAY SPACE.
           DISPLAY "PROCESO DE GENERACION DE REPORTE".
           PERFORM 2410-INICIO-REP-CLI.
           PERFORM 2420-PROCESO-GEN-REP-CLI UNTIL WKS-FINR1 = 1.
           PERFORM 2430-TERMINA-GEN-REP-CLI.
      
      
       2410-INICIO-REP-CLI.
           OPEN INPUT CLI-INDD.
           OPEN OUTPUT REP-CLI.
           PERFORM 2415-LEE-ARCHIVO.
           IF WKS-FINR1 = 0
               PERFORM 2417-GENERA-ENCABEZADO.
      
       2415-LEE-ARCHIVO.
           READ CLI-INDD
           NEXT
           AT END MOVE 1 TO WKS-FINR1.
      
       2417-GENERA-ENCABEZADO.
      
           ADD 1 TO WKS-NUM-PAG.
           MOVE SPACES TO LINEAC.
           MOVE WKS-ENC1 TO LINEAC.
           WRITE LINEAC AFTER PAGE.
      
           ACCEPT WKS-HOY FROM DATE.
           MOVE CORR WKS-HOY TO WKS-ENC2.
      
           MOVE WKS-ENC2 TO LINEAC.
           WRITE LINEAC AFTER 1.
      
           MOVE WKS-ENC3 TO LINEAC.
           WRITE LINEAC AFTER 2.
           MOVE 3 TO WKS-CONT-LIN.
      
      
       2420-PROCESO-GEN-REP-CLI.
      
           MOVE ID-CLIENTE         TO WKS-ID.
           MOVE NOM-CLI            TO WKS-NOMB.
           MOVE NOM-TAR            TO WKS-TIPT.
           MOVE FEC-ING            TO WKS-FECH.
           MOVE ANUA               TO WKS-ANUF.
      
           IF WKS-CONT-LIN > 60
               PERFORM 2417-GENERA-ENCABEZADO.
      
           MOVE WKS-DET1 TO LINEAC
           WRITE LINEAC AFTER 1
           ADD 1 TO WKS-TOTAL-CLI.
      
           PERFORM 2415-LEE-ARCHIVO.
      
       2430-TERMINA-GEN-REP-CLI.
      
           MOVE WKS-TOTAL-CLI      TO WKS-TOCLI.
           MOVE WKS-TOT1           TO LINEAC.
           WRITE LINEAC AFTER 2.
           CLOSE CLI-INDD
                 REP-CLI WITH SAVE.
      
           DISPLAY "REPORTE GENERADO".
           DISPLAY "ACEPTAR".
           ACCEPT WKS-ESPERA.
      
      *------------------SEGUNDO REPORTE---------------------
      
       2800-REPORTE-TAR.
      
           DISPLAY SPACE.
           DISPLAY "PROCESO DE GENERACION DE REPORTE".
           PERFORM 2810-INICIO-REP-TAR.
      
           PERFORM 2820-PROCESO-GEN-REP-TAR UNTIL WKS-FINR2 = 1.
           PERFORM 2830-TERMINA-GEN-REP-TAR.
      
      
       2810-INICIO-REP-TAR.
           OPEN INPUT CLI-INDD.
           OPEN OUTPUT REP-TAR.
      
           MOVE SPACES TO NOM-TAR.
           START CLI-INDD KEY IS GREATER THAN NOM-TAR
           INVALID KEY MOVE 1 TO WKS-FINR2.
      
      *       MOVE SPACES TO NOM-TAR.
      *       START CLI-INDD KEY IS GREATER OR EQUAL TO NOM-TAR.
      
           IF WKS-FINR2 = 0
               PERFORM 2815-LEE-ARCHIVO.
      
           MOVE SPACES TO WKS-CUTTER.
           MOVE ZEROES TO WKS-ANUALIS.
      *     MOVE ZEROES TO WKS-TOTAS.
      *     MOVE ZEROES TO WKS-TOTA.
           MOVE ZEROES TO WKS-TOTAL-ANUA.
      
      
       2815-LEE-ARCHIVO.
           READ CLI-INDD
           NEXT
           AT END MOVE 1           TO WKS-FINR2.
      
      
       2817-GENERA-ENCABEZADO.
      
           ADD 1                   TO WKS-NUM-PAGA.
           MOVE SPACES             TO LINEAT.
           MOVE WKS-ENCA           TO LINEAT.
           WRITE LINEAT AFTER PAGE.
      
           ACCEPT WKS-HOY FROM DATE.
           MOVE CORR WKS-HOY       TO WKS-ENCB.
      
           MOVE WKS-ENCB           TO LINEAT.
           WRITE LINEAT AFTER 1.
      
           MOVE NOM-TAR            TO WKS-NOM-TARB.
           MOVE WKS-ENCBB          TO LINEAT.
           WRITE LINEAT AFTER 2.
      
           MOVE WKS-ENCC           TO LINEAT.
           WRITE LINEAT AFTER 2.
           MOVE 3                  TO WKS-CONT-LIN.
      
      
       2820-PROCESO-GEN-REP-TAR.
      
           MOVE ID-CLIENTE         TO WKS-IDT.
           MOVE NOM-CLI            TO WKS-NOMT.
           MOVE CLA-TAR            TO WKS-CLAT.
           MOVE ANUA               TO WKS-ANUFT.
           MOVE FEC-ING            TO WKS-FECT.
      
      
           IF WKS-CONT-LIN > 80
               PERFORM 2817-GENERA-ENCABEZADO.
      
           IF WKS-CUTTER = NOM-TAR
               NEXT SENTENCE
           ELSE
               PERFORM 2825-GENERA-PIE
               MOVE 0              TO WKS-TOTAL-ANUA
               PERFORM 2817-GENERA-ENCABEZADO
               MOVE NOM-TAR        TO WKS-CUTTER.
      
           MOVE WKS-DETA           TO LINEAT
           WRITE LINEAT AFTER 1
           ADD ANUA                TO WKS-TOTAL-ANUA.
           ADD ANUA                TO WKS-ANUALIS.
      
           PERFORM 2815-LEE-ARCHIVO.
      
       2825-GENERA-PIE.
           IF WKS-NUM-PAGA = 0
               NEXT SENTENCE
           ELSE
               MOVE WKS-CUTTER     TO WKS-FTITAR
               MOVE WKS-TOTAL-ANUA TO WKS-TOANU
               MOVE WKS-TOTA       TO LINEAT
      
               WRITE LINEAT AFTER 2.
      
       2830-TERMINA-GEN-REP-TAR.
      
           PERFORM 2825-GENERA-PIE.
      
           MOVE WKS-ANUALIS TO WKS-TOANS.
           MOVE WKS-TOTAS TO LINEAT.
           WRITE LINEAT AFTER 2.
      
           CLOSE CLI-INDD
                 REP-TAR WITH SAVE.
      
           DISPLAY "REPORTE GENERADO".
           DISPLAY "ACEPTAR".
           ACCEPT WKS-ESPERA.
      
      *-----------------TERCER REPORTE------------*
      
       2900-REPORTE-TAFE.
      
           DISPLAY SPACE.
           DISPLAY "PROCESO DE GENERACION DE REPORTE".
           PERFORM 2910-INICIO-REP-TF.
      
           PERFORM 2920-PROCESO-GEN-REP-TF UNTIL WKS-FINR3 = 1.
           PERFORM 2930-TERMINA-GEN-REP-TF.
      
       2910-INICIO-REP-TF.
           OPEN INPUT CLI-INDD.
           OPEN OUTPUT REP-TF.
      
           MOVE SPACES TO NOM-TAR.
           MOVE ZEROES TO FEC-ING.
           MOVE SPACES TO WKS-CUTTER.
           MOVE ZEROES TO WKS-ANUALIS.
      *     MOVE ZEROES TO WKS-TOTAS.
      *     MOVE ZEROES TO WKS-TOTA.
           MOVE ZEROES TO WKS-TOTAL-ANUA.
      
           START CLI-INDD KEY IS GREATER THAN NOM-TAR
           INVALID KEY MOVE 1 TO WKS-FINR3.
      
           IF WKS-FINR3 = 0
               READ CLI-INDD
               NEXT
               AT END MOVE 1 TO WKS-FINR3.
      
           MOVE NOM-TAR TO WKS-TIPOT.
      
           MOVE ZEROES TO FEC-ING.
           START CLI-INDD KEY IS GREATER THAN FEC-ING
               INVALID KEY MOVE 1 TO WKS-FINR3.
      
      
       2917-GENERA-ENCABEZADO.
      
           ADD 1                   TO WKS-NUM-PAGA.
           MOVE SPACES             TO LINEAF.
           MOVE WKS-ENCA           TO LINEAF.
           WRITE LINEAF AFTER PAGE.
      
           ACCEPT WKS-HOY FROM DATE.
           MOVE CORR WKS-HOY       TO WKS-ENCB.
      
           MOVE WKS-ENCB           TO LINEAF.
           WRITE LINEAF AFTER 1.
      
           MOVE NOM-TAR            TO WKS-NOM-TARB.
           MOVE WKS-ENCBB          TO LINEAF.
           WRITE LINEAF AFTER 2.
      
           MOVE WKS-ENCC           TO LINEAF.
           WRITE LINEAF AFTER 2.
           MOVE 3                  TO WKS-CONT-LIN.
      
      
       2920-PROCESO-GEN-REP-TF.
           IF FEC-ING = 0
               NEXT SENTENCE
           ELSE IF NOM-TAR = WKS-TIPOT
               PERFORM 2922-ESCRIBE-LIN.
      
      
           READ CLI-INDD
               NEXT
               AT END
               MOVE 0 TO WKS-FIN-CHE
               MOVE WKS-TIPOT TO NOM-TAR

               PERFORM 2924-CHECK-NEXT-TAR
               MOVE ZEROES TO FEC-ING
               START CLI-INDD KEY IS GREATER THAN FEC-ING
               INVALID KEY MOVE 1 TO WKS-FINR3.
      
      
       2922-ESCRIBE-LIN.
      
           MOVE ID-CLIENTE         TO WKS-IDT.
           MOVE NOM-CLI            TO WKS-NOMT.
           MOVE CLA-TAR            TO WKS-CLAT.
           MOVE ANUA               TO WKS-ANUFT.
           MOVE FEC-ING            TO WKS-FECT.
           MOVE FEC-ING            TO WKS-FEC-SPLIT.
      
           IF WKS-CONT-LIN > 60
               PERFORM 2917-GENERA-ENCABEZADO.
      
           IF WKS-CUTANO = WKS-ASPLIT
               NEXT SENTENCE
           ELSE
      *         MOVE WKS-CUTANO TO
               MOVE WKS-CUTANO TO WKS-ANOT
               MOVE WKS-ASPLIT TO WKS-CUTANO
               PERFORM 2923-ESCRIBE-DATE-ANUA.
      
           IF WKS-CUTTER = NOM-TAR
               NEXT SENTENCE
           ELSE
               PERFORM 2925-GENERA-PIE
               MOVE 0              TO WKS-TOTAL-ANUA
               PERFORM 2917-GENERA-ENCABEZADO
               MOVE NOM-TAR        TO WKS-CUTTER.
      
           MOVE WKS-DETA           TO LINEAF
           WRITE LINEAF AFTER 1
           ADD ANUA                TO WKS-TOTAL-ANUA.
           ADD ANUA                TO WKS-ANUALIS.
           ADD ANUA                TO WKS-ANUASP.
      
       2923-ESCRIBE-DATE-ANUA.
           IF WKS-NUM-PAGA = 0
               NEXT SENTENCE
           ELSE
      *     MOVE WKS-ASPLIT TO WKS-ANOT.
               MOVE WKS-ANUASP TO WKS-PARTOT
               MOVE WKS-TOTPAR TO LINEAF
      
               WRITE LINEAF AFTER 1
               MOVE ZEROES TO WKS-ANUASP.
      
      
       2924-CHECK-NEXT-TAR.
      
           START CLI-INDD KEY IS GREATER THAN NOM-TAR
           INVALID KEY MOVE 1 TO WKS-FINR3.
      
           READ CLI-INDD
           NEXT
           AT END MOVE 1 TO WKS-FINR3.
      
           MOVE NOM-TAR TO WKS-TIPOT.
      
      
       2925-GENERA-PIE.
           IF WKS-NUM-PAGA = 0
               NEXT SENTENCE
           ELSE
               MOVE WKS-CUTTER     TO WKS-FTITAR
               MOVE WKS-TOTAL-ANUA TO WKS-TOANU
               MOVE WKS-TOTA       TO LINEAF
      
               WRITE LINEAF AFTER 2.
      
      
      
       2930-TERMINA-GEN-REP-TF.
      
           MOVE WKS-CUTANO TO WKS-ANOT
           PERFORM 2923-ESCRIBE-DATE-ANUA.
      
           PERFORM 2925-GENERA-PIE.
      
           MOVE WKS-ANUALIS TO WKS-TOANS.
           MOVE WKS-TOTAS TO LINEAF.
           WRITE LINEAF AFTER 2.
      
           CLOSE CLI-INDD
                 REP-TF WITH SAVE.
      
           DISPLAY "REPORTE GENERADO".
           DISPLAY "ACEPTAR".
           ACCEPT WKS-ESPERA.
      
      
       3000-FIN.
      *     CLOSE CLI-INDD WITH SAVE.
      
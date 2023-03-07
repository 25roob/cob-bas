       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. DYN-TF.                                              
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
       FD CLI-INDD VALUE OF TITLE WKS-ETIQ-REP.                         
       01 REG-CLI.                                                      
           03 ID-CLIENTE   PIC 9(03).                                   
           03 NOM-CLI      PIC X(30).                                   
           03 CLA-TAR      PIC 9.                                       
           03 NOM-TAR      PIC X(20).                                   
           03 ANUA         PIC 9(05)V99.                                
           03 FEC-ING      PIC 9(08).                                   
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WKS-TIPOT        PIC X(20).                                   
       01 WKS-ETIQ-REP     PIC X(24) VALUE "RFQV/S015/FILE/INDEXADO.".  
       77 WKS-FIN          PIC 9.                                       
       77 WKS-DESC         PIC X.                                       
       77 WKS-FIN-CHE      PIC 9.                                       
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       INICIO.                                                          
                                                                        
           PERFORM 1000-INICIO.                                         
           PERFORM 2000-PROCESO UNTIL WKS-FIN = 1.                      
           PERFORM 3000-FIN.                                            
           STOP RUN.                                                    
                                                                        
       1000-INICIO.                                                     
      *                                                                 
           CHANGE ATTRIBUTE TITLE OF CLI-INDD TO  WKS-ETIQ-REP.         
      *----ESTO ES PARA LOS REPORTES:                                   
      *     SET CLI-INDD (USERBACKUPNAME) TO VALUE TRUE.                
           OPEN INPUT CLI-INDD.                                         
                                                                        
                                                                        
           MOVE SPACES TO NOM-TAR.                                      
           MOVE ZEROES TO FEC-ING.                                      
                                                                        
           START CLI-INDD KEY IS GREATER THAN NOM-TAR                   
           INVALID KEY MOVE 1 TO WKS-FIN.                               
                                                                        
           IF WKS-FIN = 0                                               
               READ CLI-INDD                                            
               NEXT                                                     
               AT END MOVE 1 TO WKS-FIN.                                
                                                                        
           MOVE NOM-TAR TO WKS-TIPOT.                                   
                                                                        
           MOVE ZEROES TO FEC-ING.                                      
           START CLI-INDD KEY IS GREATER THAN FEC-ING                   
               INVALID KEY MOVE 1 TO WKS-FIN.                           
                                                                        
       2000-PROCESO.                                                    
                                                                        
           IF FEC-ING = 0                                               
               NEXT SENTENCE                                            
           ELSE IF NOM-TAR = WKS-TIPOT                                  
               DISPLAY REG-CLI.                                         
                                                                        
           READ CLI-INDD                                                
               NEXT                                                     
               AT END                                                   
               MOVE 0 TO WKS-FIN-CHE                                    
               MOVE WKS-TIPOT TO NOM-TAR                                
               PERFORM 2500-CHECK-NEXT-TAR UNTIL WKS-FIN-CHE = 1        
               MOVE ZEROES TO FEC-ING                                   
               START CLI-INDD KEY IS GREATER THAN FEC-ING               
               INVALID KEY MOVE 1 TO WKS-FIN.                           
                                                                        
       2500-CHECK-NEXT-TAR.                                             
                                                                        
           START CLI-INDD KEY IS GREATER THAN NOM-TAR                   
           INVALID KEY MOVE 1 TO WKS-FIN.                               
           READ CLI-INDD                                                
           NEXT                                                         
           AT END MOVE 1 TO WKS-FIN                                     
           MOVE 1 TO WKS-FIN-CHE.                                       
                                                                        
           IF NOM-TAR = WKS-TIPOT                                       
               NEXT SENTENCE                                            
           ELSE                                                         
               MOVE NOM-TAR TO WKS-TIPOT                                
               MOVE 1 TO WKS-FIN-CHE.                                   
                                                                        
       3000-FIN.                                                        
           CLOSE CLI-INDD WITH SAVE. 
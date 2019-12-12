	   IDENTIFICATION DIVISION.
	   PROGRAM-ID. IMPOCLI.
	   AUTHOR. GERSON GUSSO.
       DATE-WRITTEN. DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
		   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   COPY "CLIENTES.SL".
	   COPY "CLICSV.SL".
	   DATA DIVISION.
	   FILE SECTION.
	   COPY "CLIENTES.FD".
	   COPY "CLICSV.FD".
	   
        
       WORKING-STORAGE SECTION.
       01  STAT-CLIENTES                  PIC XX.
       01  STAT-CLICSV                    PIC XX.	   
	   01  WS-CAMPOS-OK                   PIC X      VALUE SPACES.
	   01  WS-EOF                         PIC X      VALUE SPACES.
	   
	   01  SEPARATE-CLI.
		   05 WS-CODIGO                   PIC 9(007).
		   05 WS-CNPJ 				      PIC 9(014).
		   05 WS-RAZAO                    PIC X(040).
		   05 WS-LATITUDE                 PIC S9(003)V9(008).
		   05 WS-LONGITUDE                PIC S9(003)V9(008).
	   01  WS-AUXILIARES.
           05 WS-ROT-CNPJ                 PIC X(008) VALUE "XXRVCNPJ".
           05 WS-RC-CNPJ                  PIC X(001) VALUE ZEROS.
		   
	    
       LINKAGE SECTION.
	   01 LIN-LIGACAO.
          05 LK-RETORNO		              PIC X(030).
       PROCEDURE DIVISION USING LIN-LIGACAO.
	   1000-INICIO.
	       PERFORM 1000-OPEN-ARQUIVO
		   PERFORM 2000-PROCESSA
		   PERFORM 3000-FINALIZA.
		  
           
	   1000-OPEN-ARQUIVO.
	       OPEN I-O CLIENTES
	       IF  STAT-CLIENTES NOT EQUAL "00"
	           STRING "ERRO AO ABRIR CLIENTES ! "
                      STAT-CLIENTES
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	           GOBACK
           END-IF.
		   
		   OPEN INPUT CLISCV
		   IF  STAT-CLICSV NOT EQUAL "00"
	           STRING "ERRO AO ABRIR ARQUIVO CSV ! "
                      STAT-CLICSV
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	           GOBACK
           END-IF.
		   
	   2000-PROCESSA.
	       
	       READ  CLICSV
		   IF STAT-CLICSV NOT EQUAL "00"
			  STRING "ERRO NO PRIMEIRO READ ARQUIVO CSV ! "
                   STAT-CLICSV
                   DELIMITED BY SIZE
                   INTO LK-RETORNO
              END-STRING
	          GOBACK
		   END-IF	
		   
           PERFORM UNTIL WS-EOF EQUAL WS-EOF = "F"
		      	  
			  IF  STAT-CLICSV EQUAL "00"
			      PERFORM 2001-CARREGA-REGISTRO
				  PERFORM 2002-CONSISTE-DADOS
				 
				  PERFORM 2004-GRAVA-CLIENTE
			  ELSE
                  IF  STAT-CLICSV NOT EQUAL "10" 
				      STRING "ERRO DE LEITURA  ARQUIVO CSV ! "
                        STAT-CLICSV
                         DELIMITED BY SIZE
                        INTO LK-RETORNO
                      END-STRING
					  GOBACK
			      END-IF		  
              END-IF
			  
			  READ  CLICSV AT END 
                    MOVE "F" TO WS-EOF	
           END-PERFORM.
		   
		   CLOSE CLICSV
		   IF  STAT-CLICSV NOT EQUAL "00"
	           STRING "ERRO AO FECHAR ARQUIVO CSV ! "
                      STAT-CLICSV
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	           GOBACK
           END-IF.
		   
           CLOSE CLIENTES
		   IF  STAT-CLIENTES NOT EQUAL "00"
	           STRING "ERRO AO FECHAR CLIENTES ! "
                      STAT-CLIENTES
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	           GOBACK
           END-IF.
		   
		      
	   2001-CARREGA-REGISTRO. 
	   
	       MOVE SPACES TO SEPARATE-CLI.
           UNSTRING RG-CLICSV DELIMITED BY ","
           INTO WS-CODIGO, WS-CNPJ, WS-RAZAO, WS-LATITUDE, WS-LONGITUDE.
            	       
	   2002-CONSISTE-DADOS. 
	   
           MOVE SPACES TO WS-CAMPOS-OK.	 
		   
		   CALL WS-ROT-CNPJ USING WS-CNPJ
                                  WS-RC-CNPJ
		   IF WS-RC-CNPJ  EQUAL 1
		      MOVE "N"  TO WS-CAMPOS-OK
		   ELSE
		       IF WS-RAZAO EQUAL SPACES
			       MOVE "N"  TO WS-CAMPOS-OK
			   ELSE  
                   	IF WS-LATITUDE EQUAL ZEROS	
					   MOVE "N"  TO WS-CAMPOS-OK
                        
                    ELSE					   
		                IF WS-LONGITUDE EQUAL ZEROS	
						   MOVE "N"  TO WS-CAMPOS-OK
                            
						END-IF
					END-IF	
               END-IF	   
	       END-IF.
		   
	   2003-MOVE-CAMPOS.
	   
	       MOVE WS-CODIGO            TO CLI-CODIGO       
		   MOVE WS-CNPJ 			 TO	CLI-CNPJ      
		   MOVE WS-RAZAO             TO CLI-RAZAO       
		   MOVE WS-LATITUDE          TO CLI-LATITUDE       
		   MOVE WS-LONGITUDE         TO CLI-LONGITUDE
           MOVE SPACES               TO CLI-ON-VENDEDOR.		   
	   
	   2004-GRAVA-CLIENTE.
	   
           IF  WS-CAMPOS-OK EQUAL SPACES
		       PERFORM 2003-MOVE-CAMPOS
		       WRITE RG-CLIENTES
			   IF  STAT-CLIENTES NOT EQUAL "00"
	               STRING "ERRO AO GRAVAR CLIENTES ! "
                          STAT-CLIENTES
                     DELIMITED BY SIZE
                     INTO LK-RETORNO
                   END-STRING
	               GOBACK
			   END-IF
           END-IF.
	   
	   3000-FINALIZA.   
       		MOVE SPACES                  TO LK-RETORNO.
			GOBACK.
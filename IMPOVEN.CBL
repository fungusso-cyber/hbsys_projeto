       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPOVEN.
       AUTHOR. GERSON GUSSO.
       DATE-WRITTEN. DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
		   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "VENDEDOR.SL".
       COPY "VENCSV.SL".
       DATA DIVISION.
       FILE SECTION.
       COPY "VENDEDOR.FD".
       COPY "VENCSV.FD".
	   
        
       WORKING-STORAGE SECTION.
       01  STAT-VENDEDOR                         PIC XX.
       01  STAT-VENCSV                           PIC XX.	   
       01  WS-CAMPOS-OK                          PIC X      VALUE SPACES.
       01  WS-EOF                                PIC X      VALUE SPACES.
	   
       01  SEPARATE-VEN.
	   05 WS-CODIGO                           PIC 9(003).
	   05 WS-CPF  			          PIC 9(011).
	   05 WS-NOME                             PIC X(040).
	   05 WS-LATITUDE                         PIC S9(003)V9(008).
	   05 WS-LONGITUDE                        PIC S9(003)V9(008).
	   01  WS-AUXILIARES.
               05 WS-ROT-CPF                      PIC X(007) VALUE "XXRVCPF".
               05 WS-RC-CPF                       PIC X(001) VALUE ZEROS.   
	   
       LINKAGE SECTION.
       01 LIN-LIGACAO.
          05 LK-RETORNO		              PIC X(030).
       PROCEDURE DIVISION USING LIN-LIGACAO.
       1000-INICIO.
	   PERFORM 1000-OPEN-ARQUIVO
	   PERFORM 2000-PROCESSA
	   PERFORM 3000-FINALIZA.
		  
           
       1000-OPEN-ARQUIVO.
	   OPEN I-O VENDEDOR
	   IF  STAT-VENDEDOR NOT EQUAL "00"
	         STRING "ERRO AO ABRIR VENDEDOR ! "
                      STAT-VENDEDOR
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	       GOBACK
           END-IF.
		   
	   OPEN INPUT CLISCV
	   IF  STAT-VENCSV NOT EQUAL "00"
	       STRING "ERRO AO ABRIR ARQUIVO CSV ! "
                      STAT-VENCSV
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	       GOBACK
           END-IF.
		   
       2000-PROCESSA.
	       
	   READ  VENCSV
		   IF STAT-VENCSV NOT EQUAL "00"
		      STRING "ERRO NO PRIMEIRO READ ARQUIVO CSV ! "
                   	   STAT-VENCSV
                    	  DELIMITED BY SIZE
                      INTO LK-RETORNO
                   END-STRING
	           GOBACK
	   END-IF	
		   
           PERFORM UNTIL WS-EOF EQUAL WS-EOF = "F"
		      	  
			  IF  STAT-VENCSV EQUAL "00"
			      PERFORM 2001-CARREGA-REGISTRO
				  PERFORM 2002-CONSISTE-DADOS
				 
				  PERFORM 2004-GRAVA-VENDEDOR
			  ELSE
           		       IF  STAT-VENCSV NOT EQUAL "10" 
				    STRING "ERRO DE LEITURA  ARQUIVO CSV ! "
                        		STAT-VENCSV
                       			  DELIMITED BY SIZE
                        		INTO LK-RETORNO
                      		     END-STRING
				    GOBACK
			      END-IF		  
             		 END-IF
			  
		    READ  VENCSV AT END 
                    MOVE "F" TO WS-EOF	
           END-PERFORM.
		   
	   CLOSE VENCSV
	   IF  STAT-VENCSV NOT EQUAL "00"
	        STRING "ERRO AO FECHAR ARQUIVO CSV ! "
                      STAT-VENCSV
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	       GOBACK
           END-IF.
		   
           CLOSE VENDEDOR
	   IF  STAT-VENDEDOR NOT EQUAL "00"
	       STRING "ERRO AO FECHAR VENDEDOR ! "
                      STAT-VENDEDOR
                 DELIMITED BY SIZE
                 INTO LK-RETORNO
               END-STRING
	       GOBACK
           END-IF.
		   
		      
       2001-CARREGA-REGISTRO. 
      
	   MOVE SPACES TO SEPARATE-VEN.
           UNSTRING RG-VENCSV DELIMITED BY ","
           INTO WS-CODIGO, WS-CPF, WS-NOME, WS-LATITUDE, WS-LONGITUDE.
            	       
	2002-CONSISTE-DADOS. 
	   
            MOVE SPACES TO WS-CAMPOS-OK.	 
		   
		   CALL WS-ROT-CPF USING  WS-CPF
                                  WS-RC-CPF
		   IF WS-RC-CPF  EQUAL 1
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
	   
	   MOVE WS-CODIGO            TO VEN-CODIGO       
	   MOVE WS-CPF               TO	VEN-CNPJ      
	   MOVE WS-NOME              TO VEN-RAZAO       
	   MOVE WS-LATITUDE          TO VEN-LATITUDE       
	   MOVE WS-LONGITUDE         TO VEN-LONGITUDE
           MOVE SPACES               TO VEN-ON-VENDEDOR.		   
	   
       2004-GRAVA-VENDEDOR.
	   
           IF  WS-CAMPOS-OK EQUAL SPACES
	       PERFORM 2003-MOVE-CAMPOS
	       WRITE RG-VENDEDOR
		   IF  STAT-VENDEDOR NOT EQUAL "00"
	       	       STRING "ERRO AO GRAVAR VENDEDOR ! "
                       	  STAT-VENDEDOR
                		 DELIMITED BY SIZE
               		 INTO LK-RETORNO
               	       END-STRING
	               GOBACK
	       	   END-IF
           END-IF.
	   
      3000-FINALIZA.   
          MOVE SPACES                  TO LK-RETORNO.
	  GOBACK.

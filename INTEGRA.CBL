       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEGRA.
       AUTHOR. GERSON GUSSO.
       DATE-WRITTEN. DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
		   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CLIENTES.SL".
       COPY "VENDEDOR.SL".
       COPY "INTEGRA.SL".
	   
       DATA DIVISION.
       FILE SECTION.
       COPY "CLIENTES.FD".
       COPY "VENDEDOR.FD".
       COPY "INTEGRA.FD".
	   
       WORKING-STORAGE SECTION.
       01  STAT-CLIENTES                  PIC XX.
	   01  STAT-VENDEDOR                  PIC XX.
       01  STAT-INTEGRA                   PIC XX.
	   01  WS-IND-VENDEDOR                PIC 9(005) VALUE ZEROS.
	   01  WS-QTD-REG                     PIC 9(005) VALUE ZEROS.
	   01  WS-IND-LIMPA                   PIC 9(005) VALUE ZEROS.
	          		   
       01  TABELA-VENDEDOR OCCURS 0 TO 5000
	              DEPENDING ON  WS-QTD-REG
				  ASCENDING TAB-COD-VENDEDOR
				  INDEXED BY WS-IND-VENDEDOR.
	       05  TAB-COD-VENDEDOR           PIC 9(003).
		   05  TAB-NOM-VENDEDOR           PIC X(040).
		   05  TAB-LON-VENDEDOR           PIC S9(003)V9(008).
		   05  TAB-LAT-VENDEDOR           PIC S9(003)V9(008).
		   
       01  WS-AUX.
           05  WS-AUX-COD-CLIENTE         PIC 9(007) VALUE ZEROS.
	   05  WS-AUX-RAZAO-CLIENTE       PIC X(040) VALUE SPACES.
           05  WS-AUX-COD-VENDEDOR        PIC 9(003) VALUE ZEROS.
	   05  WS-AUX-NOM-VENDEDOR        PIC X(040) VALUE SPACES.
	   05  WS-AUX-DISTANCIA           PIC 9(005) VALUE ZEROS.
		   
       01  WS-ROT-DISTANCIA               PIC 9(005) VALUE ZEROS.   
       01  WS-EOF                         PIC X      VALUE SPACES.
       LINKAGE SECTION.
       01  LK-RETORNO-INTEGRA             PIC X(030).
       PROCEDURE DIVISION.
       1000-INICIO.
	       PERFORM 1000-OPEN-ARQUIVO
		   PERFORM 2000-PROCESSA
		   PERFORM 3000-FINALIZA.
		            
      1000-OPEN-ARQUIVO.
	       OPEN INPUT CLIENTES
	       IF  STAT-CLIENTES NOT EQUAL "00"
	           STRING "ERRO AO ABRIR CLIENTES ! "
                      STAT-CLIENTES
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
		   OPEN INPUT VENDEDOR
	       IF  STAT-VENDEDOR NOT EQUAL "00"
	           STRING "ERRO AO ABRIR ARQ. VENDEDOR ! "
                      STAT-VENDEDOR
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
		   OPEN OUTPUT INTEGRA
		   IF  STAT-INTEGRA NOT EQUAL "00"
	           STRING "ERRO AO ABRIR ARQ. CSV ! "
                      STAT-INTEGRA
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
       2000-PROCESSA.
	   
	       PERFORM 2001-INICIALIZA-TAB-COD-VENDEDOR
		   PERFORM 2002-CARREGA-TAB-VENDEDOR
		   PERFORM 2004-LE-CLIENTE.
		   
       2001-INICIALIZA-TAB-COD-VENDEDOR.
	   
	       MOVE 5000  TO WS-QTD-REG
		   PERFORM UNTIL WS-IND-LIMPA EQUAL WS-QTD-REG
		        ADD 1 TO WS-IND-LIMPA
				INITIALIZE TAB-COD-VENDEDOR(WS-IND-LIMPA)         
		        INITIALIZE TAB-NOM-VENDEDOR(WS-IND-LIMPA)           
		        INITIALIZE TAB-LON-VENDEDOR(WS-IND-LIMPA)            
		        INITIALIZE TAB-LAT-VENDEDOR(WS-IND-LIMPA)
				
		   END-PERFORM.
		   
       2002-CARREGA-TAB-VENDEDOR.
	   
	       MOVE ZEROS     TO WS-QTD-REG
		   MOVE ZEROS     TO VEN-CODIGO  
		   START VENDEDOR KEY IS GREATER THAN VEN-CODIGO
		   IF STAT-VENDEDOR NOT EQUAL "00" AND "23"
			  STRING "ERRO NO START COD VENDEDOR ! "
                   STAT-CLIENTES
                   DELIMITED BY SIZE
                   INTO LK-RETORNO-INTEGRA
              END-STRING
	          GOBACK
		   END-IF	
            
           READ  VENDEDOR
		   IF STAT-VENDEDOR NOT EQUAL "00"
			  STRING "ERRO NO PRIMEIRO READ COD VENDEDOR ! "
                   STAT-VENDEDOR
                   DELIMITED BY SIZE
                   INTO LK-RETORNO-INTEGRA
              END-STRING
	          GOBACK
		   END-IF
		   
                      	   
           PERFORM UNTIL WS-EOF EQUAL WS-EOF = "F"
		      	  
			  IF  STAT-VENDEDOR EQUAL "00"
                  PERFORM 2003-CARREGA-VENDEDOR 
              END-IF
			  
			  READ  VENDEDOR NEXT AT END 
                    MOVE "F" TO WS-EOF	
           END-PERFORM.
		   
           CLOSE VENDEDOR
		   IF  STAT-VENDEDOR NOT EQUAL "00"
	           STRING "ERRO AO FECHAR VENDEDOR ! "
                      STAT-VENDEDOR
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
       2003-CARREGA-VENDEDOR.
	   
	       ADD 1               TO  WS-QTD-REG
		   IF  WS-QTD-REG > 5000
		        STRING "ESTOURO CARREGAMENTO TABELA INTERNA ! "
                   DELIMITED BY SIZE
                   INTO LK-RETORNO-INTEGRA
              END-STRING
	          GOBACK
		   END-IF	
		   
		   MOVE VEN-CODIGO     TO  TAB-COD-VENDEDOR(WS-QTD-REG)          
		  
		   MOVE VEN-NOME       TO  TAB-NOM-VENDEDOR(WS-QTD-REG)           
		   MOVE VEN-LATITUDE   TO  TAB-LAT-VENDEDOR(WS-QTD-REG)          
		   MOVE VEN-LONGITUDE  TO  TAB-LON-VENDEDOR(WS-QTD-REG).          
	   
       2004-LE-CLIENTE.
	   
	       MOVE SPACES     TO WS-EOF
	   	   MOVE ZEROS      TO CLI-CODIGO  
		   START CLIENTES KEY IS GREATER THAN CLI-CODIGO
		   IF STAT-CLIENTES NOT EQUAL "00" AND "23"
			  STRING "ERRO NO START COD CLIENTES ! "
                   STAT-CLIENTES
                   DELIMITED BY SIZE
                   INTO LK-RETORNO-INTEGRA
              END-STRING
	          GOBACK
		   END-IF	
            
           READ  CLIENTES
		   IF STAT-CLIENTES NOT EQUAL "00"
			  STRING "ERRO NO PRIMEIRO READ COD CLIENTES ! "
                   STAT-CLIENTES
                   DELIMITED BY SIZE
                   INTO LK-RETORNO-INTEGRA
    	          END-STRING
	          GOBACK
		   END-IF
           
          	   
           PERFORM UNTIL WS-EOF EQUAL WS-EOF = "F"
		      	  
			  IF  STAT-CLIENTES EQUAL "00"
                  PERFORM 2005-PESQUISA-VENDEDOR 
              END-IF
			  
			  READ  CLIENTES NEXT AT END 
                    MOVE "F" TO WS-EOF	
           END-PERFORM.
		   
           CLOSE CLIENTES
		   IF  STAT-CLIENTES NOT EQUAL "00"
	           STRING "ERRO AO FECHAR CLIENTES ! "
                      STAT-CLIENTES
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
		   CLOSE CLIENTES
		   IF  STAT-CLIENTES NOT EQUAL "00"
	           STRING "ERRO AO FECHAR CLIENTES ! "
                      STAT-CLIENTES
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
		   CLOSE INTEGRA
		   IF  STAT-INTEGRA NOT EQUAL "00"
	           STRING "ERRO AO FECHAR INTEGRA ! "
                      STAT-INTEGRA
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
		   
		   CLOSE VENDEDOR
		   
		   IF  STAT-VENDEDOR NOT EQUAL "00"
	           STRING "ERRO AO FECHAR VENDEDOR ! "
                      STAT-VENDEDOR
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
	  	   
       2005-PESQUISA-VENDEDOR.
	   
	       MOVE CLI-CODIGO    TO WS-AUX-COD-CLIENTE
		   MOVE CLI-RAZAO     TO WS-AUX-RAZAO-CLIENTE
		   MOVE ZEROS         TO WS-AUX-DISTANCIA
	       SET 1 TO	WS-IND-VENDEDOR
           SEARCH ALL TABELA-VENDEDOR	
              AT END ADD 1 TO WS-CONT-PESQUISA
              PERFORM 2006-CALCULA-DISTANCIA
           END-SEARCH

           PERFORM 2007-GRAVA-CSV.			
	        
       2006-CALCULA-DISTANCIA. 
	   
           CALL "DISTANCIA" USING BY REFERENCE CLI-LATITUDE CLI-LONGITUDE TAB-LAT-VENDEDOR(WS-IND-VENDEDOR) 
		          TAB-LON-VENDEDOR(WS-IND-VENDEDOR) 'T'
           RETURNING WS-ROT-DISTANCIA
		   
           IF WS-ROT-DISTANCIA GREATER THAN ZEROS
			  IF  WS-ROT-DISTANCIA LESS THAN WS-AUX-DISTANCIA
				  MOVE  TAB-COD-VENDEDOR(WS-IND-VENDEDOR) TO WS-AUX-COD-VENDEDOR
				  MOVE  TAB-NOM-VENDEDOR(WS-IND-VENDEDOR) TO WS-AUX-NOM-VENDEDOR	 	
				  MOVE  WS-ROT-DISTANCIA                  TO WS-AUX-DISTANCIA
			  ELSE
				  IF  WS-AUX-DISTANCIA EQUAL ZEROS
					  MOVE  TAB-COD-VENDEDOR(WS-IND-VENDEDOR) TO WS-AUX-COD-VENDEDOR
					  MOVE  TAB-NOM-VENDEDOR(WS-IND-VENDEDOR) TO WS-AUX-NOM-VENDEDOR	 	
					  MOVE  WS-ROT-DISTANCIA                  TO WS-AUX-DISTANCIA
				  END-IF
			  END-IF
		   END-IF.	
	   			
       2007-GRAVA-CSV.
	         
	       STRING WS-AUX-COD-CLIENTE  DELIMITED BY SPACE
                  ","    DELIMITED BY SIZE
                  WS-AUX-RAZAO-CLIENTE DELIMITED BY SPACE
                  ","    DELIMITED BY SIZE
                  WS-AUX-COD-VENDEDOR DELIMITED BY SPACE
                  ","    DELIMITED BY SIZE
                  WS-AUX-NOM-VENDEDOR DELIMITED BY SPACE
				  ","    DELIMITED BY SIZE
				  WS-AUX-DISTANCIA
               INTO RG-INTEGRA
		   END-STRING
		   
		   WRITE RG-INTEGRA
		   IF  STAT-INTEGRA NOT EQUAL "00"
	           STRING "ERRO AO GRAVAR INTEGRA CSV ! "
                      STAT-INTEGRA
                 DELIMITED BY SIZE
                 INTO LK-RETORNO-INTEGRA
               END-STRING
	           GOBACK
           END-IF.
	         
       3000-FINALIZA.
           MOVE SPACES                  TO LK-RETORNO-INTEGRA. 
           GOBACK.  
       		   

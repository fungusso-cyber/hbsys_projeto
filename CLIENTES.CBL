       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCLIENTES.
       AUTHOR.     GERSON GUSSO.
       DATE-WRITTEN.     DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
	       DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CLIENTES.SL".
       DATA DIVISION.
       FILE SECTION.
       COPY "CLIENTES.FD".
       WORKING-STORAGE SECTION.
        
       77  KEYSTATUS	                          PIC 9(004) SPECIAL-NAMES CRT STATUS.
	       88 ESCAPE-KEY                      VALUE 27.
	       88 WRITE-KEY  		 	  VALUE 221.
	       88 CLEAR-KEY 			  VALUE 222.
	       88 UP-KEY                          VALUE 223.
	       88 DEL-KEY                         VALUE 224.
	       88 IMP-KEY                         VALUE 225.
       01  STAT-CLIENTES                          PIC XX    VALUE SPACES. 
       01  WS-CAMPOS-OK                           PIC X     VALUE SPACES.
       01  WS-AUXILIARES.
           05 WS-ROT-CNPJ                         PIC X(008) VALUE "XXRVCNPJ".
           05 WS-RC-CNPJ                          PIC X(001) VALUE ZEROS. 
       01  WS-MESSAGE                             PIC X(050) VALUE SPACES.
       SCREEN SECTION.
       COPY "TELACLI.SCR".
	   
       LINKAGE SECTION.
       01  LK-IMPORTA-RETORNO             PIC X(030).
	   
       PROCEDURE DIVISION USING LK-IMPORTA-RETORNO.
       1000-INICIO.
	  OPEN I-O CLIENTES
	  IF STAT-CLIENTES NOT EQUAL "00"
	      DISPLAY MESSAGE BOX
	      "ERRO AO ABRIR CLIENTES !"
              "STATUS: " STAT-CLIENTES
              EXIT PARAGRAPH
           END-IF
           DISPLAY STANDARD GRAPHICAL WINDOW LINES 23 SIZE 80
           TITLE "Manutenção de Clientes"
           DISPLAY TELA.
	    
       2000-PROCESSA.
	   
           PERFORM WITH TEST AFTER UNTIL ESCAPE-KEY
			ACCEPT TELA ON EXCEPTION
					PERFORM 4000-CONTROLE-TELA
			END-ACCEPT
           END-PERFORM.
		   
       3000-FINALIZA.
	    CLOSE CLIENTES.
	    GOBACK.
		   
       4000-CONTROLE-TELA.
	   
	   EVALUATE TRUE
               WHEN WRITE-KEY
			      
			      PERFORM 4001-VALIDA-CLIENTE
		              PERFORM 4005-VALIDA-CAMPOS
		              PERFORM 4002-GRAVAR
               WHEN UP-KEY
			      
			      PERFORM 4001-VALIDA-CLIENTE
		              PERFORM 4005-VALIDA-CAMPOS
			      PERFORM 4003-REGRAVAR
		WHEN DEL-KEY
			      
			      PERFORM 4001-VALIDA-CLIENTE
		              PERFORM 4004-DELETAR
		WHEN IMP-KEY
			      			      
		              PERFORM 4007-IMPORTA-DADOS	
				  
	         WHEN CLEAR-KEY
      
                              PERFORM 4006-LIMPA-TELA
                  				  
           END-EVALUATE.
		
       4001-VALIDA-CLIENTE.
           MOVE SPACES     TO WS-CAMPOS-OK
	   MOVE T-CODIGO   TO CLI-CODIGO
	   READ CLIENTES
	   IF STAT-CLIENTES EQUAL "00"
	      PERFORM 4001-CARREGA-TELA
	      PERFORM 4001-ON-BOTAO-UP-DEL
	      PERFORM 4001-OFF-BOTAO-INC
	   ELSE
	       IF STAT-CLIENTES NOT "23"
		  DISPLAY MESSAGE BOX
	          "ERRO NO READ CLIENTES !"
	          "STATUS: " STAT-CLIENTES	
	          MOVE "N" TO WS-CAMPOS-OK
               END-IF				  
	   END-IF.
		   
      4001-CARREGA-TELA. 
	               
          MOVE CLI-CNPJ          TO    T-CNPJ	   
	  MOVE CLI-RAZAO         TO    T-RAZAO      
	  MOVE CLI-LATITUDE      TO    T-LATITUDE      
	  MOVE CLI-LONGITUDE     TO    T-LONGITUDE.      
		          
      4001-ON-BOTAO-UP-DEL.
	   
      4001-OFF-BOTAO-INC.
	   
      4001-ON-BOTAO-INC.
	   
      4001-OFF-BOTAO-UP-DEL.
	   
      4002-GRAVAR.
          MOVE 'N'              TO CLI-ON-VENDEDOR 
          IF  WS-CAMPOS-OK = " "
	      WRITE RG-CLIENTES
	      IF STAT-CLIENTES NOT "00"
	          DISPLAY MESSAGE BOX
		 "ERRO AO GRAVAR CLIENTES !"
		 "STATUS: " STAT-CLIENTES
	      ELSE
	         PERFORM 4006-LIMPA-TELA
		 DISPLAY MESSAGE BOX
		   "INCLUSÃO EFETUADA COM SUCESSO !"
	      END-IF
	   ELSE
	       DISPLAY MESSAGE BOX
	      "INCLUSÃO NÃO EFETUADA !"
	   END-IF.
		   
      4003-REGRAVAR.
		
	 IF  WS-CAMPOS-OK = " "
	     REWRITE RG-CLIENTES
	     IF STAT-CLIENTES NOT "00"
	        DISPLAY MESSAGE BOX
		"ERRO AO REGRAVAR CLIENTES !"
		"STATUS: " STAT-CLIENTES
	     ELSE
	         PERFORM 4006-LIMPA-TELA
		 DISPLAY MESSAGE BOX
		 "ALTERAÇÃO EFETUADA COM SUCESSO !"
	     END-IF
           ELSE
	       DISPLAY MESSAGE BOX
		"ALTERAÇÃO NÃO EFETUADA !"
	   END-IF.
		   
      4004-DELETAR.
	   
	  DELETE CLIENTES
	  IF STAT-CLIENTES NOT "00"
	     DISPLAY MESSAGE BOX
	    "ERRO AO DELETAR CLIENTES !"
	    "STATUS: " STAT-CLIENTES
	  ELSE
	      PERFORM 4006-LIMPA-TELA
	      DISPLAY MESSAGE BOX
	       "EXCLUSÃO EFETUADA COM SUCESSO !"
	  END-IF.
		   
      4005-VALIDA-CAMPOS.
      
	  MOVE SPACES  TO WS-CAMPOS-OK
	  CALL WS-ROT-CNPJ USING T-CNPJ
                           WS-RC-CNPJ
	  IF WS-RC-CNPJ  EQUAL 1
	     MOVE "N"  TO WS-CAMPOS-OK
	      DISPLAY MESSAGE BOX
	      "CNPJ INVALIDO !"
	       ACCEPT T-CNPJ AT LINE 5, COL 6
			  
	   ELSE
	       IF T-RAZAO EQUAL SPACES
	          MOVE "N"  TO WS-CAMPOS-OK
	          DISPLAY MESSAGE BOX
	          "RAZÃO INVALIDA !"
	          ACCEPT T-RAZAO AT LINE 7, COL 6
	       ELSE  
                   IF T-LATITUDE EQUAL ZEROS	
		      MOVE "N"  TO WS-CAMPOS-OK
                      DISPLAY MESSAGE BOX
		      "LATITUDE INVALIDA !"
		      ACCEPT T-LATITUDE AT LINE 9, COL 6
                   ELSE					   
		       IF T-LONGITUDE EQUAL ZEROS	
		          MOVE "N"  TO WS-CAMPOS-OK
                          DISPLAY MESSAGE BOX
			  "LONGITUDE INVALIDA !"
			   ACCEPT T-LONGITUDE AT LINE 11, COL 6
		       END-IF
		   END-IF	
               END-IF	   
	     END-IF.
	     MOVE ZEROS            TO CLI-DISTANCIA-VENDEDOR
	     MOVE ZEROS            TO CLI-ON-VENDEDOR.
	   
      4006-LIMPA-TELA.
	  INITIALIZE RG-CLIENTES
	  DISPLAY TELA.
		   
      4007-IMPORTA-DADOS.
	   
	  MOVE SPACES            TO LK-IMPORTA-RETORNO
	  CALL "IMPOCLI" USING      LK-IMPORTA-RETORNO
	  IF LK-IMPORTA-RETORNO EQUAL "SPACES"
	     PERFORM 4006-LIMPA-TELA
	     DISPLAY MESSAGE BOX
	     "IMPORTAÇÃO DE CLIENTES EFETUADA COM SUCESSO !"
	  ELSE
	      STRING "ERRO ROT. IMPORTACAO CLIENTES "
	          LK-IMPORTA-RETORNO
                   DELIMITED BY SIZE
        	   INTO WS-MESSAGE
	      END-STRING
	      DISPLAY MESSAGE BOX WS-MESSAGE
	  END-IF.
		   

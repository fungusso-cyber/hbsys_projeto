       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADVENDEDOR.
       AUTHOR. GERSON GUSSO.
       DATE-WRITTEN. DEZ-2019.
       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
		   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "VENDEDOR.SL".
       DATA DIVISION.
       FILE SECTION.
       COPY "VENDEDOR.FD".
       WORKING-STORAGE SECTION.
        
       77  KEYSTATUS					  PIC 9(004) SPECIAL-NAMES CRT STATUS.
	       88 ESCAPE-KEY                  VALUE 27.
	       88 WRITE-KEY  			      VALUE 221.
	       88 CLEAR-KEY 		          VALUE 222.
		   88 UP-KEY                      VALUE 223.
		   88 DEL-KEY                     VALUE 224.
		   88 IMP-KEY                     VALUE 225.
       01  STAT-VENDEDOR                  PIC XX. 
       01  WS-CAMPOS-OK                   PIC X     VALUE SPACES.
	   
       01  WS-AUXILIARES.
           05 WS-ROT-CPF                 PIC X(007) VALUE "XXRVCPF".
           05 WS-RC-CPF                  PIC X(001) VALUE ZEROS.  
       01  WS-MESSAGE                    PIC X(050) VALUE SPACES.   
       SCREEN SECTION.
       COPY "TELAVEN.SCR".
	   
       LINKAGE SECTION.
       01  LK-IMPORTA-RETORNO           PIC X(030).
	   
       PROCEDURE DIVISION.
       1000-INICIO.
	       OPEN I-O VENDEDOR
	       IF STAT-VENDOR NOT EQUAL "00"
	          DISPLAY MESSAGE BOX
	          "ERRO AO ABRIR VENDEDOR !"
              "STATUS: " STAT-VENDOR
              EXIT PARAGRAPH
           END-IF
           DISPLAY STANDARD GRAPHICAL WINDOW LINES 23 SIZE 80
           TITLE "Manutenção de VENDEDOR"
           DISPLAY TELA.
	    
       2000-PROCESSA.
	   
           PERFORM WITH TEST AFTER UNTIL ESCAPE-KEY
				ACCEPT TELA ON EXCEPTION
					PERFORM 4000-CONTROLE-TELA
				END-ACCEPT
           END-PERFORM.
		   
       3000-FINALIZA.
		   CLOSE VENDEDOR.
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
		   READ VENDEDOR
		   IF STAT-VENDOR EQUAL "00"
		      PERFORM 4001-CARREGA-TELA
			  PERFORM 4001-ON-BOTAO-UP-DEL
			  PERFORM 4001-OFF-BOTAO-INC
		   ELSE
		       IF STAT-VENDOR NOT "23"
				  DISPLAY MESSAGE BOX
			      "ERRO NO READ VENDEDOR !"
			      "STATUS: " STAT-VENDOR
                  MOVE "N" TO WS-CAMPOS-OK				  
               END-IF				  
		   END-IF.
		   
       4001-CARREGA-TELA. 
	               
		   MOVE CLI-CNPJ 		  TO 	T-CNPJ	   
		   MOVE CLI-RAZAO         TO    T-RAZAO      
		   MOVE CLI-LATITUDE      TO    T-LATITUDE      
		   MOVE CLI-LONGITUDE     TO    T-LONGITUDE.      
		          
       4001-ON-BOTAO-UP-DEL.
	   
       4001-OFF-BOTAO-INC.
	   
       4001-ON-BOTAO-INC.
	   
       4001-OFF-BOTAO-UP-DEL.
	   
       4002-GRAVAR.
	       MOVE 'N'              TO CLI-ON-VENDEDOR 
		   IF  WS-CAMPOS-OK EQUAL SPACES
			   WRITE RG-VENDEDOR
			   IF STAT-VENDOR NOT "00"
			      DISPLAY MESSAGE BOX
			      "ERRO AO GRAVAR VENDEDOR !"
			      "STATUS: " STAT-VENDOR
		       ELSE
			      PERFORM 4006-LIMPA-TELA
			      DISPLAY MESSAGE BOX
			      "INCLUSÃO EFETUADA COM SUCESSO !"
			   END-IF
			ELSE
			     DISPLAY MESSAGE BOX
			      "INCLUSÃO NÃO EFETUADA  !"
		   END-IF.
		   
       4003-REGRAVAR.
	       IF  WS-CAMPOS-OK EQUAL SPACES
			   REWRITE RG-VENDEDOR
		       IF STAT-VENDOR NOT "00"
				  DISPLAY MESSAGE BOX
			      "ERRO AO REGRAVAR VENDEDOR !"
			     "STATUS: " STAT-VENDOR
		       ELSE
			      PERFORM 4006-LIMPA-TELA
			      DISPLAY MESSAGE BOX
			      "ALTERAÇÃO EFETUADA COM SUCESSO !"
			   END-IF
		   ELSE
		       DISPLAY MESSAGE BOX
			      "ALTERAÇÃO NÃO EFETUADA  !"
		   END-IF.
		   
       4004-DELETAR.
	   
		   DELETE VENDEDOR
		   IF STAT-VENDOR NOT "00"
			  DISPLAY MESSAGE BOX
			  "ERRO AO DELETAR VENDEDOR !"
			  "STATUS: " STAT-VENDOR
		   ELSE
			  PERFORM 4006-LIMPA-TELA
			  DISPLAY MESSAGE BOX
			  "EXCLUSÃO EFETUADA COM SUCESSO !"
		   END-IF.
		   
       4005-VALIDA-CAMPOS.   
	       MOVE SPACES           TO WS-CAMPOS-OK
		   CALL WS-ROT-CPF USING T-CPF
                                  WS-RC-CPF
		   IF WS-RC-CPF  EQUAL 1
		      MOVE "N"           TO WS-CAMPOS-OK
		      DISPLAY MESSAGE BOX
			  "CPF INVALIDO !"
			  ACCEPT T-CNPJ AT LINE 5, COL 6
			  
		   ELSE
		       IF T-NOME EQUAL SPACES
			      MOVE "N"           TO WS-CAMPOS-OK
			       DISPLAY MESSAGE BOX
			       "NOME INVALIDA !"
				   ACCEPT T-NOME AT LINE 7, COL 6
			   ELSE  
                   	IF T-LATITUDE EQUAL ZEROS	
					   MOVE "N"           TO WS-CAMPOS-OK
                       DISPLAY MESSAGE BOX
			           "LATITUDE INVALIDA !"
					   ACCEPT T-LATITUDE AT LINE 9, COL 6
                    ELSE					   
		                IF T-LONGITUDE EQUAL ZEROS	
						   MOVE "N"           TO WS-CAMPOS-OK 
                           DISPLAY MESSAGE BOX
			               "LONGITUDE INVALIDA !"
						   ACCEPT T-LONGITUDE AT LINE 11, COL 6
						END-IF
					END-IF	
               END-IF	   
	       END-IF.
	   
	   
       4006-LIMPA-TELA.
	   
		   INITIALIZE RG-VENDEDOR
		   DISPLAY TELA.
		   
       4007-IMPORTA-DADOS.
	   
		   MOVE SPACES            TO LK-IMPORTA-RETORNO
		   CALL "IMPOVEN" USING      LK-IMPORTA-RETORNO
		   IF LK-IMPORTA-RETORNO EQUAL "SPACES"
		      PERFORM 4006-LIMPA-TELA
			  DISPLAY MESSAGE BOX
			  "IMPORTAÇÃO DE VEDEDORES EFETUADA COM SUCESSO !"
		   ELSE
		      STRING "ERRO ROT. IMPORTACAO VENDEDORES "
			          LK-IMPORTA-RETORNO
                      DELIMITED BY SIZE
                 INTO WS-MESSAGE
			  END-STRING
			  DISPLAY MESSAGE BOX WS-MESSAGE
		   END-IF.   
		   
